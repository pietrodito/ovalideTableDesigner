#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
tableDesignerServer <- function(id,
                                table,
                                named_finess,
                                formating = NULL) {

  ## TODO ajouter une description au tableau

  random_initial_choice <- sample(named_finess, size = 1)

  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    formating <- read_or_create_formating(table, formating)
    r <- do.call(shiny::reactiveValues, formating)


    dt_table <- reactive( {
      req(input$finess)
      do.call(ovalide::format_table,
              read_formating_parameters(table, finess, r, input))
    })


    render_finess_input(session, named_finess, random_initial_choice)
    render_table(dt_table, output)
    render_translation_inputs(output, r, ns)
    render_rm_filter_list(output, input, r, ns)
    render_description_output(session, r)

    event_left_col_start(input, r)
    event_translate_first_col_stop(input, r)
    event_undo(input, r)
    event_proper_left_col(input, r, table)
    event_translate(input, r)
    event_add_filter(input, r, dt_table)
    event_rm_col(input, r)
    event_rm_filter(input, r)
    event_log_current_state(input, r, table)
    event_undo_list(input, r)
    event_description_update(input, r)

    reactive(r) %>% bindEvent(input$save)
  })
}

read_or_create_formating <- function(table, formating) {
  create_default_formating <- function() {
    original_table_names <- names(table) %>% setdiff("finess_comp")
    list(
      selected_columns   = original_table_names,
      translated_columns = original_table_names,
      filters            = list(),
      row_names          = list(),
      rows_translated    = list(),
      proper_left_col    = FALSE,
      undo_list          = list(),
      description        = list()
    )
  }
  if (is.null(formating)) {
    create_default_formating()
  } else {
    formating
  }
}

render_description_output <- function(session, r) {
  shiny::updateTextAreaInput(session, "description",
                             value = isolate(r$description))
}

render_finess_input <- function(session, choices, random_initial_choice) {
  shiny::updateSelectInput(session, "finess",
                           choices = choices,
                           selected = random_initial_choice)
}

read_formating_parameters <- function(table, finess, r, input) {
  params <- c(list(table = table,
                   finess = input$finess),
              current_state_to_parameter_list(r))
  params$description <- NULL
  params
}

render_table <- function(dt_table, output) {
  output$table <- DT::renderDT(
    dt_table(),
    rownames = FALSE,
    selection = list(mode = "single", target = "cell"),
    options   = list(dom  = "t"     , pageLength = -1))
}

render_translation_inputs <- function(output, r, ns) {
  text_input_list_from <- function(original, translated, ns) {
    purrr::map2(original, translated,
                ~ shiny::textInput(ns(.x), .x, .y))
  }
  output$translation_columns <- shiny::renderUI({
    text_input_list_from(r$selected_columns, r$translated_columns, ns)})
  output$translation_rows <- shiny::renderUI({
    req(r$proper_left_col)
    text_input_list_from(r$row_names, r$rows_translated, ns)})
}

render_rm_filter_list <- function(output, input, r, ns) {
  output$rm_filter_button_list <- shiny::renderUI({
    req(r$filters)
    choices <- purrr::map(r$filters, ~ .x$select_choice)
    names(choices) <- purrr::map_chr(r$filters, ~ .x$select_name)
    list(
      shiny::selectInput(ns("rm_filter_choice"), "Filtres", choices),
      shiny::actionButton(ns("rm_filter"), "Supprimer filtre")
    )
  })


}

current_state_to_parameter_list <- function(r) {
  parameters <- shiny::reactiveValuesToList(r)
  parameters$undo_list <- NULL
  parameters
}

create_state <- current_state_to_parameter_list

save_state_to_undo_list <- function(r) {
  last_undo <- NULL
  this_undo <- create_state(r)
  l <- length(r$undo_list)
  if (l > 0) {
    last_undo <- r$undo_list[[l]]
  }
  if(! identical(last_undo, this_undo)) {
    r$undo_list <- c(r$undo_list, list(this_undo))
  }
}

load_state_from <- function(undo, r) {
  purrr::imap(undo, \(x, idx) r[[idx]] <- x)
}

a_cell_is_selected <- function(input) {
  ncol(input$table_cells_selected) > 0
}

event_left_col_start <- function(input, r) {
  observeEvent(input$translate_first_col_start, {
    if(! r$proper_left_col) {
      save_state_to_undo_list(r)
      r$proper_left_col <- TRUE
    }
  })
}

event_translate_first_col_stop <- function(input, r) {
  observeEvent(input$translate_first_col_stop, {
    if(r$proper_left_col) {
      save_state_to_undo_list(r)
      r$proper_left_col <- FALSE
    }
  })
}

event_undo <- function(input, r) {
  observeEvent(input$undo, {
    suppress_last_element <- function(l) l[-length(l)]
    l <- length(r$undo_list)
    if (l > 0) {
      undo <- r$undo_list[[l]]
      load_state_from(undo, r)
      r$undo_list <- suppress_last_element(r$undo_list)
    }
  })
}

event_proper_left_col <- function(input, r, table) {
  observeEvent(r$proper_left_col, {
    req(input$finess)
    (
      table
      %>% dplyr::filter(finess_comp == input$finess)
      %>% dplyr::pull(r$selected_columns[1])
    ) -> x
    r$row_names <- x
    if (length(r$rows_translated) == 0) {
      r$rows_translated <- x
    }
  })
}

event_translate <- function(input, r) {
  observeEvent(input$translate, {
    save_state_to_undo_list(r)

    r$translated_columns <- purrr::map_chr(r$selected_columns, ~ input[[.x]])

    req(r$proper_left_col)
    r$rows_translated <- purrr::map_chr(r$row_names, ~ input[[.x]])
  })
}

event_add_filter <- function(input, r, dt_table) {
  observeEvent(input$add_filter, {
    if (a_cell_is_selected(input)) {
      save_state_to_undo_list(r)
      col_nb <- input$table_cells_selected[1, 2] + 1
      row_nb <- input$table_cells_selected[1, 1]
      pick_value_column <- r$translated_columns[col_nb]
      filter_column <- r$selected_columns[col_nb]
      value <- dt_table()[row_nb, pick_value_column] %>% dplyr::pull()
      r$filters <- c(r$filters, list(list(
        select_name = paste(filter_column, "<>", value),
        select_choice = paste0(filter_column, "_", value),
        column = filter_column,
        value = value
      )))
    }
  })
}

event_rm_col <- function(input, r) {
  observeEvent(input$rm_col, {
    if (a_cell_is_selected(input)) {
      save_state_to_undo_list(r)
      col_nb <- input$table_cells_selected[1, 2] + 1
      column <- r$selected_columns[col_nb]
      r$selected_columns <- r$selected_columns[-col_nb]
      r$translated_columns <- r$translated_columns[-col_nb]
    }
  })
}

event_log_current_state <- function(input, r, table) {
  observeEvent(input$log_current_state, {
    line <- function() cat(paste0(rep("-", 80), collapse = ""), "\n")
    line()
    print(Sys.time())
    line()
    print(table
          %>% dplyr::filter(finess_comp == input$finess)
          %>% dplyr::select(- finess_comp))
    print(current_state_to_parameter_list(r))
  })
}

event_undo_list <- function(input, r) {

  observeEvent(input$undo_list, {
    line <- function() cat(paste0(rep("-", 80), collapse = ""), "\n")
    line()
    print(Sys.time())
    line()
    print(r$undo_list)
  })
}

event_rm_filter <- function(input, r) {
  observeEvent(input$rm_filter, {
    req(r$filters)
    save_state_to_undo_list(r)
    r$filters <- purrr::discard(r$filters, \(f) f$select_choice == input$rm_filter_choice)
  })
}

event_description_update <- function(input, r) {
  observeEvent(input$description, {
    # if(input$description != "") {
      r$description <- input$description
    # }
  })
}
