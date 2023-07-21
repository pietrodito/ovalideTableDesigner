library(ovalide)
library(tidyverse)

load_ovalide_tables(nature())
load_score(nature())

names(ovalide_tables(nature()))
score(nature())

stringr::str_view(names(ovalide_tables(nature())), "T1Q5DPZ_1")

(
  letters
  %>% discard(~ .x == "b")
)
