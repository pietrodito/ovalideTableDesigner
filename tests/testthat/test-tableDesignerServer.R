library(ovalide)
library(ovalideTableDesigner)
library(shiny)
library(tidyverse)

load_ovalide_tables(nature())
table <- ovalide_tables(nature())[["T1D2RTP_1"]]
load_score(nature())
score <- score(nature())
(finess <- score$Finess)
names(finess) <- score$Libellé

testApp <- function() {
  ui <- fluidPage(
    tableDesignerUI("designer", debug = T)
  )

  server <- function(input, output, session) {
    formating <- NULL
    if (fs::file_exists("test.rds")) {
      formating <- read_rds("test.rds")
    }

    result <- tableDesignerServer("designer", table, finess, formating)

    observe({
      req(result)
      write_rds(reactiveValuesToList(result()), "test.rds")
    })
  }

  shinyApp(ui, server)
}

sink("log.txt")

print("-------------------------------------------------------")
print(paste0("Starting app @ ", Sys.time()))
print("-------------------------------------------------------")

testApp()
