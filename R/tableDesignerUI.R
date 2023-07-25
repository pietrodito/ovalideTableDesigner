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
    define_css(),
    shiny::fluidRow(
      shiny::column(6, finess_input(ns), class = "large"),
      shiny::column(6, save_button(ns), class = "large"),
    ),
    shiny::fluidRow(
      shiny::column(4, translate_button(ns), class = "center"),
      shiny::column(4, translate_first_col_start_button(ns), class = "center"),
      shiny::column(4, translate_first_col_stop_button(ns), class = "center")
    ),
    shiny::fluidRow(
      shiny::column(4, rm_col_button(ns), class = "center"),
      shiny::column(4, add_filter_button(ns), class = "center"),
      shiny::column(4, undo_button(ns), class = "center"),
    ),
    if (debug) {
      shiny::fluidRow(
        shiny::column(6, log_current_state_button(ns), class = "center"),
        shiny::column(6, log_undo_list_button(ns), class = "center"),
      )
    },
    shiny::fluidRow(
        shiny::column(12, description_input(ns), class = "center"),
    ),
    shiny::fluidRow(
      table_output(ns)
    ),
    shiny::fluidRow(
      shiny::column(4, translation_column_inputs(ns)),
      shiny::column(4, translation_row_inputs(ns)),
      shiny::column(4, rm_filter_button_list(ns))
    )
  )
}

define_css <- function() {
  main <- "
      display: flex;
      justify-content: center;
      align-items: center;
      background: AliceBlue;
      border: 1px solid Coral;
  "

  tags$style(str_c("
    .center {
    ",
    main,
    "
    height: 50px;",
    "}
    .large {
    ",
    main,
    "
    height: 85px;",
    "}"))
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

log_current_state_button <- function(ns) {
  shiny::actionButton(ns("log_current_state"), label = "Log")
}

log_undo_list_button <- function(ns) {
  shiny::actionButton(ns("undo_list"), label = "Undo list")
}

translate_first_col_start_button <- function(ns) {
  shiny::actionButton(ns("translate_first_col_start"),
                      label = "Renommer éléments 1ère colonne")
}

translate_first_col_stop_button <- function(ns) {
  shiny::actionButton(ns("translate_first_col_stop"),
                      label = "Stop renommer 1ère colonne")
}

translate_button <- function(ns) {
    shiny::actionButton(ns("translate"), label = "Renommer")
}

rm_col_button <- function(ns) {
    shiny::actionButton(ns("rm_col"),      label = "Supprimer colonne")
}

add_filter_button <- function(ns) {
    shiny::actionButton(ns("add_filter"), label = "Ajouter filtre ligne")
}

undo_button <- function(ns) {
    shiny::actionButton(ns("undo"),        label = "Annuler")
}

description_input <- function(ns) {
  shiny::textAreaInput(ns("description"),
                       label = "Description",
                       placeholder = "Veuillez saisir une description")
}

translation_column_inputs <- function(ns) {
  shiny::uiOutput(ns("translation_columns"))
}

translation_row_inputs <- function(ns) {
  shiny::uiOutput(ns("translation_rows"))
}

rm_filter_button_list <- function(ns) {
  shiny::uiOutput(ns("rm_filter_button_list"))
}
