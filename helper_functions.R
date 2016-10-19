# Wrapper for simultaneously printing figures and saving them to disk
print.figure <- function(plotname, resolution = c(2160, 1280), save.figure = T, print.figure = F, parameters = '', fig.dir = 'Figs/') {
  if(save.figure == TRUE) {
    parameters <- paste(parameters, collapse = '_')
    plotname.str <- deparse(substitute(plotname))
    plot.fname <- paste(fig.dir, plotname.str, ifelse(parameters != '', '_', ''), parameters, '.png', sep = '')
    png(filename = plot.fname, width = resolution[1], height = resolution[2], units = 'px')
    print(plotname)
    dev.off()
  }
  print(plotname)
}

trim.ends <- function(values, qntl = 0.01, only.upper = F) {
  lower <- quantile(values, probs = qntl)
  upper <- quantile(values, probs = 1-qntl)
  
  retval <- if(only.upper == T) {
    values[values < upper]
  } else {
    values[values >= lower & values < upper]
  }
  return(retval)
}

cluster.func <- function(DF, cols, algo = c("kmeans", "dbscan"), other.params = c()) {
  algo <- tolower(algo) %>% 
    gsub(" +", "", .) %>% 
    gsub("\\.", "", .) %>% 
    gsub("_", "", .) %>% 
    gsub("-", "", .)
  
  if(algo == "kmeans") {
    clus <- kmeans(DF[, cols], centers = other.params$nclust)
  } else if(algo == "dbscan") {
    clus <- dbscan(x = DF[, cols], eps = other.params$eps, minPts = other.params$minPts)
  } else if(algo == "optics") {
    clus <- optics_cut(x = DF[, cols], eps_cl = other.params$eps)
  }
  
  DF$cluster <- clus$cluster
  return(DF)
}