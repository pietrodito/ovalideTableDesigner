#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
tableDesignerUI <- function(id) {
  ns <- NS(id)
 shiny::tagList(
   shiny::selectInput (ns("finess"), label = "FINESS", choices = NULL),
   shiny::actionButton(ns("save"  ), label = "Sauvegarder"),
   shiny::actionButton(ns("rm_col"), label = "Supprimer colonne"),
   shiny::actionButton(ns("ro_row"), label = "Supprimer ligne"),
   shiny::actionButton(ns("undo"),   label = "Annuler"),
   DT::DTOutput(ns("table")),
   shiny::uiOutput(ns("translation"))
 )
}
