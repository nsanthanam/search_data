generic.ggplot <- function(data, 
                           x.axis = '', y.axis = '', 
                           x.label = '', y.label = '', 
                           clr = '', fill = '', add.text = '',
                           x.lim = c(), y.lim = c(),
                           x.comma = F, y.comma = F, 
                           x.scientific = F, y.scientific = F,
                           facet.x = '', facet.y = '',
                           title = '', lgnd = 'top', plot.name = '', other.params = '', 
                           sz = 2.25, txt.cex = 1, flip = F, x.lbl.size = 20, title.size = 24,
                           x.rot = 0, y.rot = 0, theme = "grey",
                           plot.type = c('density', 'histogram', 'scatter', 'boxplot', 'line', 'smooth', 'smooth.no.se', 'box.stack', 'box.dodge'), 
                           save.figure = T, print.figure = T, res = c(2160, 1280), fig.dir = "figs/") {
  
  other.params <- paste(other.params, collapse = '_')
  params <- paste(deparse(substitute(data)),
                  x.axis,
                  y.axis,
                  ifelse(clr != '', clr, fill),
                  plot.type,
                  sep =  '_')
  params <- ifelse(other.params != '', paste(params, other.params, sep = '.'), params)
  
  axis.legend.title <- theme_grey() + 
    theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = x.lbl.size, angle = 0+x.rot)) +  
    theme(axis.text.y = element_text(hjust = 1, size = txt.cex*20, angle = 0+y.rot)) + 
    theme(axis.title.x = element_text(vjust = 0, hjust = 0.5, size = txt.cex*24, face = 'bold')) + 
    theme(axis.title.y = element_text(vjust = -1, hjust = 0.5, size = txt.cex*24, face = 'bold')) + 
    theme(title = element_text(size = title.size, face = 'bold')) + 
    theme(strip.text.x = element_text(size = txt.cex*20, colour = 'black')) +
    theme(strip.text.y = element_text(size = txt.cex*24, colour = 'black', angle = 270))
  
  p <- ggplot(data) + axis.legend.title
  
  if(length(sz) != length(plot.type)) sz <- rep(sz[1], length(plot.type))
  
  for(i in 1:length(plot.type)) {
    p <- p + switch(plot.type[i],
                    density = geom_density(aes_string(x = x.axis), size = sz[i], alpha = 0.7),
                    density2d = geom_density2d(aes_string(x = x.axis, y = y.axis), size = sz[i], contour = T),
                    bin2d = geom_bin2d(aes_string(x = x.axis, y = y.axis)),
                    hex = geom_hex(aes_string(x = x.axis, y = y.axis)),
                    bar.stack = geom_bar(aes_string(x = x.axis, y = y.axis), stat = 'identity', position = 'stack'),
                    bar.dodge = geom_bar(aes_string(x = x.axis, y = y.axis), stat = 'identity', position = 'dodge'),
                    bar.count = geom_bar(aes_string(x = x.axis), position = 'dodge'),
                    histogram = geom_histogram(aes_string(x = x.axis)),
                    scatter = geom_point(aes_string(x = x.axis, y = y.axis), size = sz[i]),
                    scatter.jitter = geom_point(aes_string(x = x.axis, y = y.axis), size = sz[i], position = 'jitter'),
                    boxplot = geom_boxplot(aes_string(x = x.axis, y = y.axis)),
                    violinplot = geom_violin(aes_string(x = x.axis, y = y.axis)),
                    line = geom_line(aes_string(x = x.axis, y = y.axis, group = 1), size = sz[i]),
                    smooth = geom_smooth(aes_string(x = x.axis, y = y.axis), size = sz[i], method = 'loess'),
                    smooth.no.se = geom_smooth(aes_string(x = x.axis, y = y.axis), size = sz[i], method = 'loess', se = F))
  }
  
  if(clr != '') p <- p + aes_string(colour = clr)
  if(fill != '') p <- p + aes_string(fill = fill)
  
  if(lgnd == F) {
    p <- p + theme(legend.position = 'none')
  } else {
    p <- p + theme(legend.text = element_text(size = txt.cex*20, face = 'plain'), legend.position = "top")
  }
  if(x.comma) {
    p <- p + scale_x_continuous(limits = x.lim, labels = comma)
  } else if(x.scientific) {
    p <- p + scale_x_continuous(labels = scientific_format())
  }
  if(y.comma) {
    p <- p + scale_y_continuous(limits = y.lim, labels = comma)
  } else if(y.scientific) {
    p <- p + scale_y_continuous(labels = scientific_format())
  }
  
  if(x.label == '') {
    p <- p + xlab(x.axis)
  } else {
    p <- p + xlab(x.label)
  }
  if(y.label == '') {
    p <- p + ylab(y.axis)
  } else {
    p <- p + ylab(y.label)
  }
  
  p <- p + ggtitle(title)
  
  if(flip) p <- p + coord_flip()
  
  if(facet.x != '' & facet.y != '') {
    p <- p + facet_grid(as.formula(paste(facet.x, '~', facet.y)))
  } else if(facet.x != '') {
    p + facet_wrap(as.formula(paste('.', '~', facet.x)))
  } else if(facet.y != '') {
    p + facet_wrap(as.formula(paste(facet.y, '~',  '.')))
  }
  
  if(save.figure == T) {
    plot.fname <- paste(fig.dir, params, '.png', sep = '')
    png(filename = plot.fname, width = res[1], height = res[2], units = 'px')
    print(p)
    dev.off()
  }
  if(print.figure == T) {
    print(p)
  }
  return(p)
}

# scatter.plot <- function(DF, vars, plt.name = '') {
#   scatter.plotnames <- c()
#   for(i in 1:length(vars)) {
#     for(j in 1:length(vars)) {
#       x.var <- vars[i]
#       y.var <- vars[j]
#       var.name <- paste(plt.name, x.var, y.var, sep = '.')
#       assign(var.name, generic.ggplot(DF, 
#                                       x.axis = x.var, 
#                                       y.axis = y.var,
#                                       xlabel = x.var, 
#                                       ylabel = y.var,
#                                       clr = x.var, lgnd = F,
#                                       x.comma = T, y.comma = T,
#                                       plot.type = 'scatter', save.figure = F))
#       scatter.plotnames <- c(scatter.plotnames, as.name(var.name))
#     }
#   }
#   
#   png(filename = paste('Important Figures/', plt.name, '.scatter', '.png', sep = ''), width = 360*length(vars), height = 360*length(vars), units = 'px')
#   do.call(grid.arrange, c(scatter.plotnames, nrow = length(vars), ncol = length(vars)))
#   dev.off()
#   remove(list = ls(pattern = 'shg.group3.[a-z]+.[a-z]+'))
# }

continuous.discrete.pair.plots <- function(DF, cont.var, disc.var, plt.type = c('boxplot', 'violinplot'), plt.name = '') {
  plotnames <- c()
  for(i in 1:length(cont.var)) {
    for(j in 1:length(disc.var)) {
      y.var <- cont.var[i]
      x.var <- disc.var[j]
      var.name <- paste(plt.name, x.var, y.var, plt.type, sep = '.')
      assign(var.name, generic.ggplot(DF, 
                                      x.axis = x.var, y.axis = y.var,
                                      x.label = x.var, y.label = y.var,
                                      fill = x.var, y.comma = T, lgnd = F,
                                      plot.type = plt.type, save.figure = F))
      plotnames <- c(plotnames, as.name(var.name))
    }
  }
  png(filename = paste('Important Figures/', plt.name, '.', plt.type, '.png', sep = ''), width = 360*length(disc.var), height = 360*length(cont.var), units = 'px')
  do.call(grid.arrange, c(plotnames, nrow = length(cont.var), ncol = length(disc.var)))
  dev.off()
}

density.plots.2DF <- function(DF1, DF2 = '', match.range, fname) {
  plotnames <- c()
  matching.cols <- intersect(colnames(DF1[, match.range]), colnames(DF2))
  
  colname1 <- deparse(substitute(DF1))
  colname2 <- deparse(substitute(DF2))
  
  for(i in 1:length(matching.cols)) {
    col.name <- matching.cols[i]
    col.df <- cbind(rep(colname2, nrow(DF2)), DF2[, col.name]) %>% 
      rbind(., cbind(rep(colname1, nrow(DF1)), DF1[, col.name])) %>% 
      as.data.frame(.)
    colnames(col.df) <- c("dataset", col.name)
    col.df[, col.name] <- as.numeric(col.df[, col.name])
    # range.diff <- max(col.df[, col.name], na.rm = T) - min(col.df[, col.name], na.rm = T)
    # if(range.diff > 1e3) {
    #   col.df[, col.name] <- log10(col.df[, col.name])
    #   colnames(col.df)[2] <- paste('lg', col.name, sep = ".")
    # }
    # colnames(col.df)[2] <- ifelse(nchar(colnames(col.df)[2]) > 35, substr(colnames(col.df)[2], 1, 35), colnames(col.df)[2])
    
    plot.name <- paste("vvl", "context", col.name, "plot", sep = "_")
    assign(plot.name, generic.ggplot(data = col.df, x.axis = col.name, clr = "dataset", sz = 1.5, lgnd = T, plot.type = "density"))
    
    plotnames <- c(plotnames, as.name(plot.name))
    # warnings()
  }
  
  num.rows <- matching.cols %>% length(.) %>% sqrt(.) %>% ceiling(.)
  num.cols <- matching.cols %>% length(.) %>% sqrt(.) %>% ceiling(.)
  size.x <- 480*num.cols
  size.y <- 480*num.rows
    
  png(filename = paste('~/Dropbox/scisphere/navaneethan_scisphere_shared_folder/', fname, '.png', sep = ''), width = size.x, height = size.y, units = 'px')
  do.call(grid.arrange, c(plotnames, nrow = num.rows, ncol = num.cols))
  dev.off()
}

plot.var.and.cum.var <- function(pca.eig) {
  pca.eig.plot <- generic.ggplot(data = pca.eig, x.axis = "component", y.axis = "percentage_of_variance2", 
                                 x.label = "% variance", plot.type = "bar.stack", print.fig = F) + 
    theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 20, angle = 90)) + 
    geom_text(aes(x = component, y = percentage_of_variance2+2, label = percentage_of_variance2), vjust = 0, size = 7, angle = 0)
  print.figure(pca.eig.plot, fig.dir = shared.fig.dir)
  
  pca.eig.cum.plot <- generic.ggplot(data = pca.eig, x.axis = "component", y.axis = "cumulative_percentage_of_variance", 
                                     x.label = "% variance", plot.type = "bar.stack", print.fig = F) + 
    theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 20, angle = 90)) + 
    geom_text(aes(x = component, y = cumulative_percentage_of_variance+5, label = cumulative_percentage_of_variance), vjust = 0, size = 7, angle = 0)
  print.figure(pca.eig.cum.plot, fig.dir = shared.fig.dir)
  
  retlist <- list("pca.eig.plot" = pca.eig.plot, "pca.eig.cum.plot" = pca.eig.cum.plot)
  return(retlist)
}

generic.multiplot <- function(plotnames, fig.dir = "Figs/", fname = "", save.print.both = 'print', num.rows = NULL, num.cols = NULL) {
  plotnames2 <- sapply(plotnames, as.name)

  if(is.null(num.rows) || is.null(num.cols)) {
    num.rows <- plotnames2 %>% length() %>% sqrt() %>% ceiling()
    num.cols <- plotnames2 %>% length() %>% sqrt() %>% ceiling()
  }
  
  size.x <- 480*num.cols
  size.y <- 480*num.rows
  
  if(save.print.both == 'print') {
    do.call(grid.arrange, c(plotnames2, nrow = num.rows, ncol = num.cols))
  } else if(save.print.both == 'save') {
    png(filename = paste(fig.dir, fname, '.png', sep = ''), width = size.x, height = size.y, units = 'px')
    do.call(grid.arrange, c(plotnames2, nrow = num.rows, ncol = num.cols))
    dev.off()
  } else {
    do.call(grid.arrange, c(plotnames2, nrow = num.rows, ncol = num.cols))
    
    png(filename = paste(fig.dir, fname, '.png', sep = ''), width = size.x, height = size.y, units = 'px')
    do.call(grid.arrange, c(plotnames2, nrow = num.rows, ncol = num.cols))
    dev.off()
  }
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}