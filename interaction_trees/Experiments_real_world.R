# Input: data frame
# Output: interaction trees result

remove(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('Functions-IT.R')
source('get_causal_effect_from_contingency_table.R')

## interaction trees ####
# =============================================================
# Shisheng starts here... 
# =============================================================
# import data 
bootstrap = 25    # recommended min 25 max 100

data_name = 'US_census'
treatment = 'educ.12'
outcome = 'income.50K'

# data_name = 'marketing_campaign'
# treatment = 'TREATMENT'
# outcome = 'PURCHASE'

# data_name = 'email_analytics'
# treatment = 'segment'
# outcome = 'visit'

# data_name = 'email_analytics_women'
# treatment = 'segment'
# outcome = 'visit'

# data_name = 'email_analytics_men'
# treatment = 'segment'
# outcome = 'visit'

# data_name = 'adult_binary'
# treatment = 'education.num.12'
# outcome = 'class'

# data_name = 'twins'
# treatment = 'T'
# outcome = 'target'

# data_name = 'criteo_uplift'
# treatment = 'treatment'
# outcome = 'visit'

################################################################################
dir.create(paste('../real_world_data/data/model_vs_contingency_table_', data_name, '_IT', sep=''))

for (n_batch in 1:10) {
  
  for (n_fold in 1:2) {
    
    file_1 = paste('../real_world_data/data/model_vs_contingency_table_',data_name,'/cross_validation_time_',n_batch,'_fold_',n_fold,'_for_model.csv',sep='')
    data_1 = read.csv(file_1)
    file_2 = paste('../real_world_data/data/model_vs_contingency_table_',data_name,'/cross_validation_time_',n_batch,'_fold_',n_fold,'_as_ground_truth.csv',sep='')
    data_2 = read.csv(file_2)
    output_png = paste('../real_world_data/data/model_vs_contingency_table_',data_name,'_IT/cross_validation_time_',n_batch,'_fold_',n_fold,'.png',sep='')
    output_file = paste('../real_world_data/data/model_vs_contingency_table_',data_name,'_IT/model_vs_contingency_table_cv_',n_batch,'_fold_',n_fold,'.csv',sep='')
    csv_for_decile = paste('../real_world_data/data/model_vs_contingency_table_',data_name,'_IT/cross_validation_batch_',n_batch,'_fold_',n_fold,'_results.csv',sep='')
    
    # clean data 1
    data_1 = data_1[, -1]
    data_1 = data_1[, -which(colnames(data_1) == 'leaf_index')]
    colnames(data_1)[which(colnames(data_1) == treatment)] = 'trt'
    colnames(data_1)[which(colnames(data_1) == outcome)] = 'y'
    dat = data_1
    n_variables = ncol(dat) - 2
    
    # clean data 2
    data_2 = data_2[, -1]
    data_2 = data_2[, -which(colnames(data_2) == 'CATE')]
    colnames(data_2)[which(colnames(data_2) == treatment)] = 'trt'
    colnames(data_2)[which(colnames(data_2) == outcome)] = 'y'
    
    
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
    # RANDOMLY ADD VERY SMALL NUMBER TO SCORE AND SCORE_TEST
    set.seed(1)
    random_double_num = runif(n=sum(!is.na(tree0$score)), min=-5, max=5)
    tree0[which(!is.na(tree0$score)), 'score'] = tree0[which(!is.na(tree0$score)), 'score'] + 0.000001 * random_double_num

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

    ## test ####
    # get leaf nodes
    if (sum(is.na(btree$vname)) > 0) {
      btree_patterns = btree[which(is.na(btree$vname)), ]
    } else {
      btree_patterns = btree
    }
    btree_patterns[, c('n11','n12','n21','n22')] = 0
    # get test results
    test_results = list(send.down(data_2, btree))[[1]][['data']]
    
    # save for decile plots
    test_results_CATE = merge(test_results, btree_patterns[,c('node','trt.effect')],
                              by.x = 'node', by.y = 'node')
    write.csv(test_results_CATE, csv_for_decile)
    
    for (i in 1:nrow(btree_patterns)) {
      row_in_test_results = which(btree_patterns[i, 'node'] == test_results$node)
      n11 = sum(test_results[row_in_test_results, 'trt'] == 1 & test_results[row_in_test_results, 'y'] == 1)
      n12 = sum(test_results[row_in_test_results, 'trt'] == 1 & test_results[row_in_test_results, 'y'] == 0)
      n21 = sum(test_results[row_in_test_results, 'trt'] == 0 & test_results[row_in_test_results, 'y'] == 1)
      n22 = sum(test_results[row_in_test_results, 'trt'] == 0 & test_results[row_in_test_results, 'y'] == 0)
      btree_patterns[i, c('n11','n12','n21','n22')] = c(n11, n12, n21, n22)
    }

    btree_patterns['CATE'] = get_causal_effect_from_contingency_table(list(btree_patterns$n11,
                                                                           btree_patterns$n12,
                                                                           btree_patterns$n21,
                                                                           btree_patterns$n22)) 
    
    # calculate bias and N
    btree_patterns['bias'] = abs(btree_patterns['CATE'] - btree_patterns['trt.effect'])
    btree_patterns['N'] = btree_patterns['n11'] + btree_patterns['n12'] + 
      btree_patterns['n21'] + btree_patterns['n22']
    
    # save output
    write.csv(btree_patterns, output_file)
    
  }
  
}



