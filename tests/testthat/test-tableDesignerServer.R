if(! interactive()) {
  main <- function() {
  test_that("server works", {
    setup_ovalide_data()
    app <- shinytest::ShinyDriver$new(testApp())
  })
  }
}
setup_ovalide_data <- function() {
  library(ovalide)
  library(ovalideTableDesigner)
  library(shiny)
  library(tidyverse)

  load_ovalide_tables(nature())
  table_to_test <<- ovalide_tables(nature())[["T1D2RTP_1"]]
  load_score(nature())
  scores <<- score(nature())
  (finess <<- scores$Finess)
  names(finess) <<- scores$Libellé
}

setup_ovalide_data <- purrr::quietly(setup_ovalide_data)

testApp <- function() {
  ui <- fluidPage(
    tableDesignerUI("designer", debug = T)
  )

  server <- function(input, output, session) {
    formating <- NULL
    if (fs::file_exists("test.rds")) {
      formating <- read_rds("test.rds")
    }

    result <- tableDesignerServer("designer", table_to_test, finess, formating)

    observe({
      req(result)
      write_rds(reactiveValuesToList(result()), "test.rds")
    })
  }

  shinyApp(ui, server)
}

if(! interactive()) main()

## interactive tests ######
if (interactive()) {
  setup_ovalide_data()


  # sink("log.txt")

  print("-------------------------------------------------------")
  print(paste0("Starting app @ ", Sys.time()))
  print("-------------------------------------------------------")

  testApp()
}
