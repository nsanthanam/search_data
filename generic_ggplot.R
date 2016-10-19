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