library(ovalide)
library(tidyverse)

load_ovalide_tables(nature())
load_score(nature())

names(ovalide_tables(nature()))
score(nature())

stringr::str_view(names(ovalide_tables(nature())), "T1D2RTP_1")


ovalide_tables(nature())[["T1D2RTP_1"]]

(
  letters
  %>% discard(~ .x == "b")
)

(undo_list <- list())
(undo_me <- list(x = 1, y = 1))
(undo_list <- c(undo_list, list(undo_me)))
(undo_me <- list(x = 2, y = 2))
(undo_list <- c(undo_list, list(undo_me)))

(undo_list <- undo_list[-2])
(undo_list <- undo_list[-1])


foo <- function(df, filter) {
  (
    df
    %>% dplyr::filter(.data[[filter$column]] == filter$value)
  )
}

mtcars <- as_tibble(mtcars)
n <- paste0("X_", names(mtcars))
