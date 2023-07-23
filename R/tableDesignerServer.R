tableDesignerServer <- function(id,
                                table,
                                named_finess,
                                formating = NULL) {

  ## TODO mettre les filtres sur ligne apparents dans l'UI
  ## pour pouvoir les supprimer

  choices               <- named_finess
  random_initial_choice <- sample(choices, size = 1)

  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    original_table_names<- names(table) %>% setdiff("finess_comp")

    default <- list(
      selected_columns   = original_table_names,
      translated_columns = original_table_names,
      filters            = list(),
      row_names          = list(),
      rows_translated    = list(),
      proper_left_col    = FALSE,
      undo_list          = list()
    )

    if (! is.null(formating)) default <- formating

    r <- do.call(shiny::reactiveValues, default)

    output$translation_columns <- shiny::renderUI({
        text_input_list_from(r$selected_columns, r$translated_columns, ns)
    })

    output$translation_rows <- shiny::renderUI({
      req(r$proper_left_col)
      text_input_list_from(r$row_names, r$rows_translated, ns)
    })

    shiny::updateSelectInput(session,
      "finess",
      label = "FINESS",
      choices = choices,
      selected = random_initial_choice
    )

    current_state_to_parameter_list <- function() {
        parameters <- shiny::reactiveValuesToList(r)
        parameters$undo_list <- NULL
        parameters
    }

    r_table <- reactive({
      parameters <- c(list(table = table,
                           finess = input$finess),
                      current_state_to_parameter_list())
      do.call(ovalide::format_table, parameters)
    })

    output$table <- DT::renderDT(
      r_table(),
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(
        dom = "t",
        pageLength = -1
      )
    )


    create_event_observers <- function() {

      a_cell_is_selected <- function() {
        ncol(input$table_cells_selected) > 0
      }

      load_state_from <- function(undo) {
        r$selected_columns   <- undo$selected_columns
        r$translated_columns <- undo$translated_columns
        r$filters            <- undo$filters
        r$row_names          <- undo$row_names
        r$rows_translated    <- undo$rows_translated
        r$proper_left_col    <- undo$proper_left_col
      }

      create_state <- current_state_to_parameter_list

      save_state_to_undo_list <- function() {
        last_undo <- NULL
        this_undo <- create_state()
        l <- length(r$undo_list)
        if (l > 0) {
          last_undo <- r$undo_list[[l]]
        }
        if(! identical(last_undo, this_undo)) {
          r$undo_list <- c(r$undo_list, list(this_undo))
        }
      }


      observeEvent(input$proper_left_col_start, {
        if(! r$proper_left_col) {
          save_state_to_undo_list()
          r$proper_left_col <- TRUE
        }
      })
      observeEvent(input$proper_left_col_stop, {
        if(r$proper_left_col) {
          save_state_to_undo_list()
          r$proper_left_col <- FALSE
        }
      })
      observeEvent(input$undo, {
        suppress_last_element <- function(l) l[-length(l)]
        l <- length(r$undo_list)
        if (l > 0) {
          undo <- r$undo_list[[l]]
          load_state_from(undo)
          r$undo_list <- suppress_last_element(r$undo_list)
        }
        print(r$undo_list)
      })
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
      observeEvent(input$proper_cols, {
        save_state_to_undo_list()
        r$translated_columns <-
          purrr::map_chr(r$selected_columns, ~ input[[.x]])
        r$rows_translated <-
          purrr::map_chr(r$row_names, ~ input[[.x]])
      })
      observeEvent(input$rm_row, {
        if (a_cell_is_selected()) {
          save_state_to_undo_list()
          col_nb <- input$table_cells_selected[1, 2] + 1
          row_nb <- input$table_cells_selected[1, 1]
          pick_value_column <- r$translated_columns[col_nb]
          filter_column <- r$selected_columns[col_nb]
          value <- r_table()[row_nb, pick_value_column] %>% dplyr::pull()
          r$filters <- c(r$filters, list(list(
            column = filter_column,
            value = value
          )))
        }
      })
      observeEvent(input$rm_col, {
        # remove_filters_about <- function(column) {
        #   discard(r$filters, ~ .x$column == column)
        # }
        if (a_cell_is_selected()) {
          save_state_to_undo_list()
          col_nb <- input$table_cells_selected[1, 2] + 1
          column <- r$selected_columns[col_nb]
          r$selected_columns <- r$selected_columns[-col_nb]
          r$translated_columns <- r$translated_columns[-col_nb]
          # r$filters <- remove_filters_about(column)
        }
      })
      observeEvent(input$debug, {
        line <- function() cat(paste0(rep("-", 80), collapse = ""), "\n")
        line()
        print(Sys.time())
        line()
        print(table
              %>% dplyr::filter(finess_comp == input$finess)
              %>% dplyr::select(- finess_comp))
        print(current_state_to_parameter_list())
      })
    }

    create_event_observers()
    reactive(r) %>% bindEvent(input$save)
  })
}

text_input_list_from <- function(original, translated, ns) {
  purrr::map2(original, translated,
              ~ shiny::textInput(ns(.x), .x, .y))
}
