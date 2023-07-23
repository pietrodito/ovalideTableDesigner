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
  shiny::fluidPage(
    shiny::fluidRow(
      column(4,
             shiny::selectInput(ns("finess"),
                                label = "FINESS", choices = NULL)),
      column(4,
             shiny::actionButton(ns("save"  ),
                                 label = "Sauvegarder")),
      column(2,
             shiny::actionButton(ns("debug"  ),
                                 label = "Log")),
      column(2,
             shiny::actionButton(ns("undo_list"  ),
                                 label = "Undo list")),
    ),
    shiny::fluidRow(
      shiny::actionButton(ns("proper_left_col_start"),
                          label = "Noms en ligne"),
      shiny::actionButton(ns("proper_left_col_stop"),
                          label = "Stop noms en ligne"),
    ),
    shiny::actionButton(ns("proper_cols"), label = "Traduction"),
    shiny::actionButton(ns("rm_col"),      label = "Supprimer colonne"),
    shiny::actionButton(ns("rm_row"),      label = "Supprimer ligne"),
    shiny::actionButton(ns("undo"),        label = "Annuler"),
    DT::DTOutput(ns("table")),
    shiny::fluidRow(
      column(6, shiny::uiOutput(ns("translation_columns"))),
      column(6, shiny::uiOutput(ns("translation_rows")))
    )
  )
}
