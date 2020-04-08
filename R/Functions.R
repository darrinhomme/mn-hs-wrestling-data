SavePlot = function(p, width = 900, height = 700, name = NULL) {
  if(is.null(name)) {
    name = gsub(" ", "-", x = p$labels$title)
  }
  png(paste0("Charts/", name, ".png"), width = width, height = height)
  print(p)
  dev.off()
}


