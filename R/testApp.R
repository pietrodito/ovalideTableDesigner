library(ovalide)
library(shiny)
library(tidyverse)

testApp <- function() {

 ui <- fluidPage(
   textOutput("log"),
   tableDesignerUI("designer")
 )

 server <- function(input, output, session) {
   result <- tableDesignerServer("designer",
                                 nature("mco", "dgf"),
                                 # "T1Q0QSYNTH_1")
                                 # "T1Q5DPZ_1")
                                 "T1D2RTP_1")

  output$log <- renderText({
    req(result)
    write_rds(result, "test.rds")
    (
      list(`Colonnes retenues` = result$selected_columns(),
           `Traduction`        = result$translated_columns())
      %>% map(paste, collapse = " ")
      %>% map2(names(.), ., ~ paste(.x, .y))
      %>% exec(paste, .)
    )
  })
 }

 shinyApp(ui, server)
}
