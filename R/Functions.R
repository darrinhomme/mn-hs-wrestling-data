SavePlot = function(p, width = 900, height = 700, name = NULL) {
  if(is.null(name)) {
    name = gsub(" ", "-", x = p$labels$title)
  }
  png(paste0("Images/", name, ".png"), width = width, height = height)
  print(p)
  dev.off()
}



WEIGHTS = c("106", "113", "120", "126", "132", "138", "145", "152", "160", "170", "182", "195", "220", "285")
