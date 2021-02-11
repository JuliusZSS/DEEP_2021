# Input: data frame
# Output: interaction trees result

remove(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('Functions-IT.R')

## interaction trees ####
# =============================================================
# SZ starts here... 
# =============================================================
# import data 
bootstrap = 25    # recommended min 25 max 100
# data_names = c('data_20_4_4','data_40_4_4','data_60_4_4','data_80_4_4','data_100_4_4')
data_names = "data"

n_variables = 20    # 20 40 60 80 100

for (data_name in data_names) {
  
  dir.create(paste('../synthetic_data/',data_name,'/interaction_trees_self_training', sep=""))
  
  for (n_batch in 1:10) {
    
    file_1 = paste('../synthetic_data/',data_name,'/causal_trees_self_training/causal_tree_self_training_batch_',n_batch,'.csv',sep='')
    data_1 = read.csv(file_1)
    output_file = paste('../synthetic_data/',data_name,'/interaction_trees_self_training/interaction_trees_self_training_batch_',n_batch,'.csv',sep='')
    output_png = paste('../synthetic_data/',data_name,'/interaction_trees_self_training/interaction_trees_self_training_batch_',n_batch,'.png',sep='')
    
    # clean data
    data_1 = data_1[, -1]
    data_1 = data_1[, -which(colnames(data_1) == 'CATE')]
    colnames(data_1)[which(colnames(data_1) == 'w')] = 'trt'
    dat = data_1
    
    # =============================================================
    # CONSTRUCT ONE SINGLE IT VIA TEST SAMPLE METHOD (METHOD I) 
    # =============================================================
    
    # OBTAIN A LARGE INITIAL TREE
    split.var <- 1:n_variables    # Shisheng
    tree0 <- grow.INT(data=dat, test=dat, min.ndsz=20, n0=5, 
                      split.var=split.var, ctg=NULL, max.depth=15)
    # RENAME and ROUNDING
    rownames(tree0) = 1:nrow(tree0)
    tree0$score = round(tree0$score, 6)
    tree0$score.test = round(tree0$score.test, 6)
    # REPLACE Inf
    inf_row = which(is.infinite(tree0$score))
    tree0[inf_row, 'score'] = median(tree0$score, na.rm = TRUE)
    tree0[inf_row, 'score.test'] = median(tree0$score.test, na.rm = TRUE)
    
    # DEAL WITH DUPLICATION SZ
    while (sum(duplicated(tree0$score) & !is.na(tree0$score)) > 0) {
      find_duplication = which(duplicated(tree0$score) & !is.na(tree0$score))
      tree0[find_duplication, 'score'] = tree0[find_duplication, 'score'] + 0.000001
      tree0[find_duplication, 'score.test'] = tree0[find_duplication, 'score.test'] + 0.000001
    }
    
    # PRUNE AND SELECT BEST TREE SIZE
    prn <- prune.size.testsample(tree0)
    # PRUNING INFORMATION, LOOK FOR THE MAXIMUM Ga.2, Ga.3, Ga.4, OR Ga.BIC
    prn$result 
    
    # # PLOT OF G.a VS. TREE SIZE (# TERMINALS)
    # tmp <- prn$result 
    # col.Ga <- 7:10
    # for (j in c(4, col.Ga)) tmp[,j] <- as.numeric.factor(tmp[,j])
    # y.min <- min(tmp[, col.Ga]); y.max <- max(tmp[, 7:10])
    # x.min <- min(tmp[, 4]); x.max <- max(tmp[, 4])
    # plot(c(x.min, x.max), c(y.min, y.max), type="n", xlab="tree size", ylab="G.a Values")
    # for (j in col.Ga) lines(tmp$size.tmnl, tmp[,j], col=j, lty=1)
    # legend(x.max-5, y.max, col=col.Ga, lty=1, legend=colnames(tmp)[col.Ga])    
    
    # THE BEST TREE SIZES (USING BIC)
    prn$size
    bsize <- prn$size[4]; bsize 
    # OBTAIN THE FINAL TREE STRUCTURE
    btree <- obtain.btree(tree0, bsize=bsize); 
    btree
    
    # PLOT THE TREE STRUCTURE
    jpeg(output_png)
    plot.tree(tree=btree, textDepth=4, lines="rectangle")
    title(main="The Final IT Structure")
    dev.off()
    
    # # GENERATE LaTeX CODES FOR PACKAGE {PCTricks}
    # plot.tree.latex(btree, file="tree-code.tex", digits=5)
    
    ## self training ####
    # get leaf nodes
    btree_patterns = btree[which(is.na(btree$vname)), ]
    # get test results
    test_results = send.down(data_1, btree)
    test_results = list(test_results)[[1]][['data']][['node']]
    data_2 = data_1
    data_2 = cbind(data_2, test_results)
    # match CATE
    CATE = apply(data_2, 1, function(x) {
      btree_patterns$trt.effect[which(btree_patterns$node == x['test_results'])]
    })
    data_2 = cbind(data_2, CATE)
    # change column name
    colnames(data_2)[colnames(data_2) == 'trt'] = 'w'
    data_2 = data_2[, -which(colnames(data_2) == 'test_results')]
    
    # save output
    write.csv(data_2, output_file)
    
  }
  
}




