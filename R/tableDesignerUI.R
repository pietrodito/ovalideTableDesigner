#' @export
tableDesignerUI <- function(id, debug = FALSE) {

  ns <- NS(id)

  shiny::fluidPage(
    define_css(),

    shiny::uiOutput(ns("table_name")),

    shiny::wellPanel(
      row_finess_and_save(ns),
      row_translate(ns),
      row_filter_undo(ns)
    ),

    if (debug) row_debug(ns),

    shiny::wellPanel(
      row_description(ns),
      row_table_output(ns)
    ),

    shiny::wellPanel(
      row_details(ns)
    ),
  )
}

row_finess_and_save <- function(ns) {
  shiny::fluidRow(
    tags$form(
      tags$div(class = "form-group row large",
               tags$label(class = "col-sm-1 col-form-label", "FINESS"),
               tags$div(class = "col-sm-4", finess_input(ns)),
               tags$div(class = "col-sm-2"),
               tags$div(class = "col-sm-3", save_button(ns))
      )
    )
  )
}

row_translate <- function(ns) {
  shiny::fluidRow(
    shiny::column(4, translate_button(ns), class = "normal"),
    shiny::column(4, translate_first_col_start_button(ns), class = "normal"),
    shiny::column(4, translate_first_col_stop_button(ns), class = "normal")
  )
}

row_filter_undo <- function(ns) {
  shiny::fluidRow(
    shiny::column(4, rm_col_button(ns), class = "normal"),
    shiny::column(4, add_filter_button(ns), class = "normal"),
    shiny::column(4, undo_button(ns), class = "normal"),
  )
}

row_debug <- function(ns) {
  shiny::fluidRow(
    shiny::column(6, log_current_state_button(ns), class = "normal"),
    shiny::column(6, log_undo_list_button(ns), class = "normal"),
  )
}

row_description <- function(ns) {
  shiny::fluidRow(
    shiny::column(12, description_input(ns), class = "verylarge") )
}

row_table_output <- function(ns) {
  shiny::fluidRow(table_output(ns))
}

row_details <- function(ns) {
  shiny::fluidRow(
    shiny::column(4, translation_column_inputs(ns)),
    shiny::column(4, translation_row_inputs(ns)),
    shiny::column(4, rm_filter_button_list(ns))
  )
}

background_color <- function() {
  ".container-fluid {
    background-color: #007BA7;
  }"
}

all_buttons_same_width <- function() {
  "button { width: 250px;}"
}

centered_buttons <- function() {
  "
  display: flex;
  justify-content: center;
  align-items: center;
  "
}

define_row_height_style <- function(name, height_in_px) {
  glue::glue("
  .<<name>> {
  <<centered_buttons()>>
  height: <<height_in_px>>px;
  }
  ", .open = "<<", .close = ">>")
}

define_css <- function() {
  tags$style(paste(
    # background_color(),
    all_buttons_same_width(),
    define_row_height_style("normal", 50),
    define_row_height_style("large", 85),
    define_row_height_style("verylarge", 100)
  ))
}

table_output <- function(ns) {
  DT::DTOutput(ns("table"))
}

finess_input <- function(ns) {
 tags$select(id = ns("finess"))
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
    shiny::actionButton(ns("undo"), label = "Annuler")
}

description_input <- function(ns) {
    shiny::textAreaInput(ns("description"),
                         label = "Description",
                         width = "100%",
                         height = "100%")
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
