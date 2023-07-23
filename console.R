library(ovalide)
library(tidyverse)

(x <- list(a = 1, b = 2))
(y <- list(a = 3, b = 4, c = 5))

c <- y$c
(y <- x)
y$c <- c

y
