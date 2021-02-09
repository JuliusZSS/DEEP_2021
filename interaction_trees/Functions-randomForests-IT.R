

###############################################################################
# FUNCTIONS THAT GENERATE RANDOM FORESTS AND ADD MORE; 
# THEN COMPUTE VARIABLE IMPORTANCE AND MAKE PARTIAL DEPENDENCE PLOTS
###############################################################################

# ======================================================
# BUILD RANDOM FORESTS OF INTERACTION TREES  
# ======================================================

Build.RF.IT <- function(dat, col.y, col.trt, split.var, ctg=NA, 
	N0=20, n0=5,  max.depth=10,
	ntree=500, mtry = max(floor(length(split.var)/3), 1),
	avoid.nul.tree=F)
{
	out <- as.list(NULL); 
	names(dat)[c(col.y, col.trt)] <- c("y", "trt");
	out$ID.Boots.Samples <- out$TREES <- as.list(1:ntree)
	b <- 1
	while (b <= ntree) {
		print(b)
		# TAKE BOOTSTRAP SAMPLES
		id.b <- sample(1:nrow(dat), size=nrow(dat), replace = T)
		dat.b <- dat[id.b,]
		tre.b <- grow.INT(data=dat.b, test=NULL, min.ndsz=N0, n0=n0, 
			split.var=split.var, ctg=ctg, 
			max.depth=10, mtry=mtry)
		if (avoid.nul.tree) {
			if (nrow(tre.b) > 1) {
				out$ID.Boots.Samples[[b]] <- id.b
				out$TREES[[b]] <- tre.b; 
				b <- b +1			
			}
		}
		else {
			out$ID.Boots.Samples[[b]] <- id.b
			out$TREES[[b]] <- tre.b; 
			b <- b +1		
		}
	}
	Model.Specification <- as.list(NULL)
	Model.Specification$data <- dat; Model.Specification$split.var <- split.var; Model.Specification$ctg <- ctg
	Model.Specification$col.y <- col.y; Model.Specification$col.trt <- col.trt
	out$Model.Specification <- Model.Specification
	return(out)
}


# =============================================================
# FUNCTION combine.RFIT() COMBINES TWO Build.RF.IT() OBJECTS
# =============================================================

combine.RFIT <- function (...){
	rflist <- list(...)
	n.rf <- length(rflist)
	rf1 <- rflist[[1]]
	components <- names(rf1)
	n.comp <- length(components) 
	OUT <- as.list(1:n.comp)
	for (j in 1:2) {
		comp.j <- as.list(NULL)
		for (i in 1:n.rf) {
			rf.i <- rflist[[i]]
			comp.ij <- rf.i[[j]]
			comp.j <- c(comp.j, comp.ij) 
		}
		OUT[[j]] <- comp.j
	}
	OUT[[n.comp]] <- rf1$Model.Specification			
	names(OUT) <- components 
	return(OUT)
}





# ------------------------------------------------------------------
# THIS senddown FUNCTION IS WRITTEN FOR THE VIARIABE IMPORTANCE PART
# USING RANDOM FORESTS 
# ------------------------------------------------------------------
 
send.down.VI <- function(dat.new, tre, 
	col.y, col.trt, ctg=NA, 
	n0=5, revise.tree=T)
{
	node.dat <- rep(1, nrow(dat.new))   		# COLUMNS CAN BE ADDED TO DATA
    	cut.point <- as.vector(tre$cut); split.var <- as.numeric(as.vector(tre$var)); 
	y <- dat.new[, col.y]; trt <- dat.new[, col.trt]
	tre0 <- tre # REVISED TREE
	tre0$n.test <- tre0$score.test <- rep(NA, nrow(tre)) 	# COLUMNS CAN BE ADDED TO TREE
	i <- 1
	while (i <= nrow(tre0)){
		node.i <- tre0$node[i]
        	in.node <- (node.dat== node.i);
	  	y0 <- y[in.node]; trt0 <- trt[in.node]; dat0 <- data.frame(y=y0, trt=trt0)
		n.0 <- length(y0)
		tre0$n.test[i] <- n.0
		t2 <- NA    
        	if (!is.na(split.var[i])){
			# print("################################")
            	# print(cbind(i, var=tre$var[i], cut=tre$cut[i]))
            	x.split <- dat.new[,split.var[i]]; 
            	cut <- cut.point[i]
            	if (!is.element(split.var[i], ctg)) { 
                		cut1 <- as.numeric(cut)    
                		l.nd <- node.dat[in.node & x.split <= cut1] 
                		r.nd <- node.dat[in.node & x.split > cut1]
                		z <- sign(x.split[in.node] <= cut1)
                		node.dat[in.node & x.split <= cut1] <- paste(l.nd, 1, sep="")  ############################
                		node.dat[in.node & x.split >  cut1] <- paste(r.nd, 2, sep="")  ############################ 
            	}
            	else {
                		cut1 <- unlist(strsplit(as.character(cut), split=" "))  
                		l.nd <- node.dat[in.node & is.element(x.split, cut1)] 
                		r.nd <- node.dat[in.node & !is.element(x.split, cut1)]   
                		z <- sign(is.element(x.split[in.node], cut1))  
                		node.dat[in.node & is.element(x.split, cut1)] <- paste(l.nd, 1, sep="")  	############################
                		node.dat[in.node & !is.element(x.split, cut1)] <- paste(r.nd, 2, sep="")  	############################                 
        		}
        		# print(dim(dat0)); print(length(z))   
			t2 <- ttest(dat0, z, n0=n0)
			tre0$score.test[i] <- t2
	  	}
	  	if (is.na(t2) && revise.tree) {
			node.rm <-  de(node.i, tre0)
			tre0 <- tre0[!is.element(tre0$node, node.rm), ]
			tre0[tre0$node==node.i, c("var", "vname", "cut", "score")] <- NA
		}  
		i <- i+1
	}
	return(tre0)
}




# =====================================================================
# FUNCTION Variable.Importance() COMPUTE VARIABLE IMPORTANCE MEASURES
# =====================================================================

# RF.fit = MUST BE AN OBJECT OR OUTPUT FROM FUNCTION Build.RF.IT() OR combine.RF()
# sort = OPTION TO SORT THE RESULTANT VI MEASURES
# truncate.zeros= OPTION TO TRUNCATE <= 0 VI MEAURESN TO 0. 

Variable.Importance <- function(RF.fit, n0=2, sort=T, details=F, truncate.zeros=T){
	trees <- RF.fit$TREES
	id.boots <- RF.fit$ID.Boots.Samples
	# ARGUMETNS FOR MODEL SPECIFICATION 
	Model.Specification <- RF.fit$Model.Specification
	dat0 <- Model.Specification$data; col.y <- Model.Specification$col.y; col.trt <- Model.Specification$col.trt
	split.var <- Model.Specification$split.var; ctg <- Model.Specification$ctg
	vnames <- colnames(dat0)[split.var]
	# 
	ntree <- length(trees)
	p <- length(split.var)
	VI <- rep(0, p)
	for (b in 1:ntree){
		if (details) print("######################################");  
		print(b);
		id.b <- id.boots[[b]]
		dat.oob <- dat0[-sort(unique(id.b)), ] 
		n.oob <- nrow(dat.oob)	
		tre.b <- trees[[b]]
		tre0.b <- send.down.VI(dat.new=dat.oob, tre=tre.b, 
			col.y=col.y, col.trt=col.trt, ctg=ctg, 
			n0=n0, revise.tree=T)  					########## NOTE THAT revise.tree=T HERE!
		if (details) print(tre0.b)
		if (nrow(tre0.b) > 1) {						### AVOID NULL TREES	
			Xs.b <- sort(unique(na.omit(tre0.b$var))) 
			G.oob <- sum(tre0.b$score.test, na.rm=T)
			for (j in 1:p) {
				if (details) print(j)
				G.j <- G.oob
				col.xj <- split.var[j] 
				if (is.element(col.xj, Xs.b)){			
					x.j <- dat.oob[, col.xj]
					dat.permuted <- dat.oob
					dat.permuted[ , col.xj] <- x.j[sample(1:n.oob,n.oob, replace=F)]
					tre0.bj <- send.down.VI(dat.new=dat.permuted, tre=tre0.b, 
						col.y=col.y, col.trt=col.trt, ctg=ctg, 
						n0=n0, revise.tree=F) 			########## NOTE THAT revise.tree=F HERE!
					if (details) {print("TREE WITH PERMUTED SAMPLE"); print(tre0.bj)}
					G.j <- ifelse(nrow(tre0.bj) ==1, G.oob, sum(tre0.bj$score.test, na.rm=T))
				}
				if (G.j > G.oob) G.j <- G.oob  		##################### PREVENTS NEGATIVE IMPORTANCE VALUES 
				VI[j] <- VI[j] + (G.oob - G.j)/G.oob
			}
		}	
		print(VI)	
	}
	if (truncate.zeros) VI[VI <0] <- 0  		####### IS THIS STEP NECESSARY? NOPE. 
	names(VI) <- vnames
	if (sort) VI <- sort(VI, decreasing=T) 
	return(VI)
}




# =====================================================================
# FUNCTION plot.VI() PLOTS VARIABLE IMPORTANCE MEASURES USING bar.plot
# =====================================================================


plot.VI <- function(VI, filename=NULL, horizontal=T, rain.bow=T)
{
	if (!is.null(filename)) postscript(file=filename, horizontal=horizontal)
	par(mfrow=c(1, 1), mar = c(7, 4, 7, 4));
	require(grDevices)
	p <- length(VI)
	color0 <- gray(0:(p - 1)/(p - 1))
	if (rain.bow) color0 <- rainbow(p)	
	barplot(VI, col=color0,
      	names.arg = names(VI), yaxt = "n", cex.names = 1.2, 
		las=3);  # TO HAVE VERTICAL AXIS LABELING 
	title(main = list("Variable Importance Rank with Interaction Trees", 
		font = 4, cex = 1.4));
	if (!is.null(filename)) dev.off()
}

# 





###################################### THE END ###########################################
