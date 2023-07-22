library(ovalide)
library(tidyverse)


reactiveConsole()

r <- reactiveValues()

l <- list(a = 1, b = 2)

iwalk(l, \(x, idx) r[[idx]] <<- x)



