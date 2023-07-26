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

  table_name <<- "T1D2RTP_1"
  table_name <<- "T1D2DADNP_2"
}

setup_ovalide_data <- purrr::quietly(setup_ovalide_data)

testApp <- function() {
  ui <- fluidPage(
    tableDesignerUI("designer", debug = F)
  )

  server <- function(input, output, session) {

    tableDesignerServer("designer", table_name, nature())
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
