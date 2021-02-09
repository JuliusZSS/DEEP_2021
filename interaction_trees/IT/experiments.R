
# =============================================================
# CONSTRUCT ONE SINGLE IT VIA BOOTSTRAP METHOD (METHOD II) 
# =============================================================
IT <- function(dat, treatment, outcome, bootstrap) {
  # source("Functions-IT-Bootstrap.R")
  
  # data.file = "census-UCI_ALL_my_2"
  # dat = read.csv(paste0("C:/Users/maysy020/Desktop/personalisedDecision/fourDataSet/", 
  #                        data.file, ".csv"), stringsAsFactors = F)
  # varnames = colnames(dat)
  # treat.var = "edu.num.12"
  # treatment = which(varnames %in% treat.var)
  # outcome.var = "class"
  # outcome = which(varnames %in% outcome.var)
  
  # Shisheng
  # import data
  file_1 = '../synthetic_data/data_20_4_4/causal_trees_self_training/causal_tree_self_training_batch_1.csv'
  data_1 = read.csv(file_1)
  
  # clean data
  data_1 = data_1[, c(-1, -24)]
  colnames(data_1)[21] = 'trt'
  dat = data_1
  
  treatment = 'trt'
  outcome = 'y'
  bootstrap = 25
  
  
  
  
  
  
  reindex = which(! c(1:ncol(dat)) %in% c(treatment,outcome))
  dat = dat[,c(reindex, outcome, treatment)]
  colnames(dat)[c(ncol(dat)-1, ncol(dat))] = c("y", "trt")
  
  # GROW AND PRUNE
  split.var <- 1:(ncol(dat)-2)
  B = bootstrap
  boot.result <- boottrap.grow.prune(B, data=dat, N0=20, n0=5, 
                                     split.var=split.var, ctg=NULL, 
                                     max.depth=8, LeBlanc=T, min.boot.tree.size=1)  
  # THE INITIAL LARGE TREE CAN BE FOUND FROM TEH RESULTS
  tree0 <- boot.result$initial.tree; 
  # tree0;
  # DETERMINE THE BEST TREE SIZE AND OBTAIN THE BEST TREE MODELS
  boot.prune <- boot.result$boot.prune
  OUT <- bootstrap.size(boot.result, n=nrow(dat), 
                        plot.it=T, filename=NULL, horizontal=T);
  names(OUT)
  OUT$G.a 	# BEST G.a 
  OUT$bsize	# BEST TREE SIZES
  OUT$btree  	# BEST TREE MODELS
  
  # BEST TREE USING BIC
  btree <- OUT$btree$`G.log(n)`;  
  btree
  # plot.tree(tree=btree, textDepth=6, lines="rectangle")
  # 
  # 
  # # IF WE WANT A TREE MODEL WITH ARBITRARY NUMBER OF TERMINAL NODES 
  # # btree <- obtain.btree(tree0, bsize=4); btree
  # 
  # 
  # # =======================
  # # AMALGAMATION
  # # =======================
  # 
  # char.var <- NULL
  # n.groups <- 3
  # result.amalgamation <- amalagamation(dat, btree, char.var=char.var, n.groups=n.groups, details=F)
  # result.amalgamation$merge.info
  # dat0 <- result.amalgamation$data; dat0$node 
  # 
  # # =============================
  # # SUMMARIZE THE FINAL SUBGROUPS 
  # # =============================
  # 
  # dat$subgroup <- dat0$node
  # col.grp <- which(colnames(dat)=="subgroup")
  # summary.grp <- summary.subgroups(dat, var.equal=T, digits=4, col.grp=col.grp)
  # summary.grp
  
  
}