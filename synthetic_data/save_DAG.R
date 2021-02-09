
save_DAG <- function(skel.fit, filename_to_save) {
  if (require(Rgraphviz)) {
    ## show estimated Skeleton
    png(filename_to_save, width = 788, height = 418)
    par(mfrow=c(1,2), bg='white')
    plot(skel.fit, main = "Estimated Skeleton")
    dev.off()
  }
}

