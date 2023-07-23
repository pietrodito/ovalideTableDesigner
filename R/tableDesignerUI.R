#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
tableDesignerUI <- function(id, debug = FALSE) {

  ns <- NS(id)
  shiny::fluidPage(
    css_center(),
    shiny::fluidRow(
      shiny::column(6, finess_input(ns), class = "center"),
      shiny::column(6, save_button(ns), class = "center"),
    ),
    shiny::fluidRow(
      shiny::column(6, proper_left_col_start_button(ns), class = "center"),
      shiny::column(6, proper_left_col_stop_button(ns), class = "center")
    ),
    shiny::fluidRow(
      shiny::column(3, proper_col_button(ns), class = "center"),
      shiny::column(3, rm_col_button(ns), class = "center"),
      shiny::column(3, rm_row_button(ns), class = "center"),
      shiny::column(3, undo_button(ns), class = "center"),
    ),
    if (debug) {
      shiny::fluidRow(
        shiny::column(6, debug_button(ns), class = "center"),
        shiny::column(6, debug_undo_button(ns), class = "center"),
      )
    },
    shiny::fluidRow(
      table_output(ns)
    ),
    shiny::fluidRow(
      shiny::column(6, translation_column_inputs(ns)),
      shiny::column(6, translation_row_inputs(ns))
    )
  )
}

css_center <- function() {
  tags$style("
    .center {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100px;
      background: AliceBlue;
      border: 3px solid Coral;
    }")
}

table_output <- function(ns) {
    DT::DTOutput(ns("table"))
}

finess_input <- function(ns) {
  shiny::selectInput(ns("finess"), label = "FINESS", choices = NULL)
}

save_button <- function(ns) {
  shiny::actionButton(ns("save"), label = "Sauvegarder")
}

debug_button <- function(ns) {
  shiny::actionButton(ns("debug"  ), label = "Log")
}

debug_undo_button <- function(ns) {
  shiny::actionButton(ns("undo_list"), label = "Undo list")
}

proper_left_col_start_button <- function(ns) {
  shiny::actionButton(ns("proper_left_col_start"),
                      label = "Renommer éléments 1ère colonne")
}

proper_left_col_stop_button <- function(ns) {
  shiny::actionButton(ns("proper_left_col_stop"),
                      label = "Stop renommer 1ère colonne")
}

proper_col_button <- function(ns) {
    shiny::actionButton(ns("proper_cols"), label = "Traduction")
}

rm_col_button <- function(ns) {
    shiny::actionButton(ns("rm_col"),      label = "Supprimer colonne")
}

rm_row_button <- function(ns) {
    shiny::actionButton(ns("rm_row"),      label = "Supprimer ligne")
}

undo_button <- function(ns) {
    shiny::actionButton(ns("undo"),        label = "Annuler")
}

translation_column_inputs <- function(ns) {
  shiny::uiOutput(ns("translation_columns"))
}

translation_row_inputs <- function(ns) {
  shiny::uiOutput(ns("translation_rows"))
}
