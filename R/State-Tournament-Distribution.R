library(ggplot2)
library(dplyr)
library(magrittr)
library(gridExtra)
library(grid)
source("R/Functions.R")

wt = 1:42
yrs = 2016:2020
url = "http://www.tstuff.com"

dist = list()
for (y in yrs) {
  y2 = substr(y, 3, 4)
  for (w in wt) {
    page <- paste0("s", y2, "w", w, ".htm")
    full_url <- paste(url, y, page, sep="/")
    
    f <- read.csv(full_url, sep="~")[[1]] %>% paste(collapse = "~") %>% enc2native() %>% strsplit("~")
    
    dist[[paste0(y, w)]] = 7:12 %>%
      paste0("(", ., ")") %>%
      sapply(FUN = function(x) length(grep(x, x = f[[1]], fixed = T))) %>%
      (function(x) c(x, Year = y))
    
    print(w)
  }
  print(y)
}

state = dist %>% do.call(rbind, .) %>%
  as.data.frame() %>%
  dplyr::mutate(Div = rep(c(rep("A", 14), rep("AA", 14), rep("AAA", 14)), 5)) %>%
  dplyr::mutate(Weight = rep(c(rep(WEIGHTS, 3)), 5))


p = state %>%
  tidyr::gather(key = "Grade", value = "Count", 1:6) %>%
  dplyr::mutate(Grade = gsub("(", "", Grade, fixed = T)) %>%
  dplyr::mutate(Grade = gsub(")", "", Grade, fixed = T) %>% as.integer()) %>%
  dplyr::mutate(Grade = sprintf("G%02d", Grade)) %>%
  dplyr::group_by(Weight, Grade) %>%
  dplyr::summarise(Total = sum(Count)) %>%
  tidyr::spread(key = Grade, value = Total) %>%
  grid.table()


SavePlot(p, width = 600, height = 450, name = "State-Participation-By-Grade-2016-2020.png")




