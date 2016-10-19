# Plot pairwise variable relationships, correlation values and distribution
chart.cor <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...) {
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", method, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwiDFh(txt)
    test <- cor.test(x, y, method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  
  hist.panel = function(x, ...) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1.5)
    rug(x)
  }
  
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel, method = method, ...)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
             method = method, ...)
  axis(side = 1, at = seq(1, ncol(R), 1), labels = colnames(R))
}

# Generate correlation plot and save to disk
generate.corrplot <- function(DF, tofile = TRUE, plotname = '', parameters = '', fig.dir = 'Figs/') {
  col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", "#007FFF", "blue", "#00007F"))
  
  numeric.cols <- which(sapply(DF, class) %in% c('numeric', 'integer'))
  tmp.cor <- cor(DF[, numeric.cols], use = 'pairwise.complete.obs')
  tmp.cor <- tmp.cor[rowSums(is.na(tmp.cor)) != ncol(tmp.cor), colSums(is.na(tmp.cor)) != nrow(tmp.cor)] #Drop rows and cols that are entirely NA
  
  if(tofile) {
    params.str <- paste(parameters, collapse = '_')
    data.name <- deparse(substitute(DF))
  png(filename = paste(fig.dir, 'corrplot_', plotname, data.name, ifelse(parameters != '', '_', ''), params.str, '.png', sep = ''), 
        width = 1944, 1152, units = 'px')
    corrplot(tmp.cor, method = 'circle', col = col1(100), tl.cex = 1.7, tl.col = 'black', title = '')
    dev.off()
  }
  corrplot(tmp.cor, method = 'circle', col = col1(100), tl.cex = 1.7, tl.col = 'black', title = '')
}

# Correlation plot for contextual variables
context.corrplot <- function(DF, first.context.var = 'pop_2011', last.context.var = 'largest_firm_size_10_km', params = '', tofile = T) {
  context.vars <- get.context.vars(DF, fcv = first.context.var, lcv = last.context.var)
  DF.context <- DF[, colnames(DF) %in% context.vars]
  generate.corrplot(DF.context, tofile = tofile, plotname = 'Contextual Variables', parameters = params)
}

# Correlation plot for personal variables
personal.corrplot <- function(DF, last.context.var = 'largest_firm_size_10_km', params = '', tofile = T) {
  personal.vars <- get.personal.vars(DF, lcv = last.context.var)
  DF.personal <- DF[, colnames(DF) %in% personal.vars]
  generate.corrplot(DF.personal, tofile = tofile, plotname = 'Personal Variables', parameters = params)
}

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

get.context.vars <- function(DF, fcv = 'pop_2011', lcv = 'largest_firm_size_10_km', target.vars = c('dpd', 'profit'), drop.tgt = F) {
  target.var.cols <- which(colnames(DF) %in% target.vars)
  first.context.col <- which(colnames(DF) == fcv)
  last.context.col <- which(colnames(DF) == lcv)
  
  if(drop.tgt) {
    context.vars <- colnames(DF)[seq(first.context.col, last.context.col)]
  } else {
    context.vars <- colnames(DF)[c(seq(first.context.col, last.context.col), target.var.cols)]
  }
  return(context.vars)
}

get.personal.vars <- function(DF, target.vars = c('dpd', 'profit'), drop.tgt = F) {
  context.vars <- get.context.vars(DF)
  
  if(drop.tgt == T) {
    personal.vars <- setdiff(colnames(DF), context.vars)
  } else {
    personal.vars <- c(setdiff(colnames(DF), context.vars), target.vars)
  }
  return(personal.vars)
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

map.pca.context.ind <- function(pca.obj, orig.df, col.for.quantile = c("pop_2011", "dist_mjr_dstrct_rd", "dist_terty_rd", 
                                                                       "iwr", "migration_dec", "number_of_unique_industries")) {
  pca.ind <- pca.obj$ind$coord %>% 
    as.data.frame() %>% 
    .[.$Dim.1 < quantile(.$Dim.1, 0.999), ]
  
  pca.rownames <- rownames(pca.ind)
  
  iq <- sapply(col.for.quantile, function(x) cbind(quantile(orig.df[, x], 0.66, na.rm = T), quantile(orig.df[, x], 0.33, na.rm = T))) %>% c()
  names(iq) <- sapply(col.for.quantile, function(x) cbind(paste(x, "0.66", sep = "_"), paste(x, "0.33", sep = "_"))) %>% c()
  
  other.info <- sapply(col.for.quantile, function(col) {
    sapply(pca.rownames, function(rows) {
      q66 <- paste(col, "0.66", sep = "_")
      q33 <- paste(col, "0.33", sep = "_")
      ifelse(orig.df[rows, col] > iq[q66], "High", ifelse(orig.df[rows, col] > iq[q33], "Medium", "Low")) %>% 
        factor(., level = c("High", "Medium", "Low"), ordered = T)
    })
  }) %>% as.data.frame()
  
  pca.ind <- sapply(pca.rownames, function(x) {
    row.num <- which(rownames(orig.df) == x)
    c(district = orig.df$name_2[row.num], 
      taluk = orig.df$name_3[row.num], 
      village = orig.df$name_4[row.num],
      level = orig.df$level_2011[row.num])
  }) %>% 
    t() %>% 
    as.data.frame() %>% 
    cbind(., other.info, pca.ind)
  
  colnames(pca.ind)[seq(1, length(col.for.quantile)+4)] <- c("district", "taluk", "village", "level", col.for.quantile)
  
  return(pca.ind)
}

get.eig <- function(pca.obj) {
  pca.eig <- pca.obj$eig
  pca.eig$component <- factor(rownames(pca.eig), levels = rownames(pca.eig), ordered = T)
  rownames(pca.eig) <- c()
  colnames(pca.eig) <- gsub(" ", "_", colnames(pca.eig))
  pca.eig$percentage_of_variance <- round(pca.eig$percentage_of_variance, 2)
  pca.eig$cumulative_percentage_of_variance <- round(pca.eig$cumulative_percentage_of_variance, 2)
  
  return(pca.eig)
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