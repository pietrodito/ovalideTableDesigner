library(ovalide)
library(shiny)

testApp <- function() {

 ui <- fluidPage(
   tableDesignerUI("designer")
 )

 server <- function(input, output, session) {
   tableDesignerServer("designer",
                       nature("mco", "dgf"),
                       # "T1Q0QSYNTH_1")
                       "T1Q5DPZ_1")
 }

 shinyApp(ui, server)
}



