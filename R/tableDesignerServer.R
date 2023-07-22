tableDesignerServer <- function(id,
                                table,
                                named_finess,
                                formating = NULL) {

  ## TODO create config std and save to .rds file in ovalide
  ## TODO ovalide should know how to adapt original table from config
  ## TODO refactor here to use ovalide to format table with config

  choices               <- named_finess
  random_initial_choice <- sample(choices, size = 1)

  full_table <- table

  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    original_table_names<- names(full_table) %>% setdiff("finess_comp")

    default <- list(
      selected_columns   = original_table_names,
      translated_columns = original_table_names,
      filters            = list(),
      row_names          = list(),
      rows_translated    = list(),
      proper_left_col    = FALSE
    )

    if (! is.null(formating)) {
      default$selected_columns   <- formating$selected_columns
      default$translated_columns <- formating$translated_columns
      default$filters            <- formating$filters
      default$row_names          <- formating$row_names
      default$rows_translated    <- formating$rows_translated
      default$proper_left_col    <- formating$proper_left_col
    }

    r <- do.call(shiny::reactiveValues, default)

    output$translation_columns <- shiny::renderUI({
        purrr::map2(
          r$selected_columns,
          r$translated_columns,
          ~ shiny::textInput(ns(.x), .x, .y)
        )
    })

    observeEvent(input$proper_left_col_start, {
      r$proper_left_col <- TRUE
    })
    observeEvent(input$proper_left_col_stop, {
      r$proper_left_col <- FALSE
    })

    output$translation_rows <- shiny::renderUI({
      req(r$proper_left_col)
        purrr::map2(
          r$row_names,
          r$rows_translated,
          ~ shiny::textInput(ns(.x), .x, .y)
        )
    })

    shiny::updateSelectInput(session,
      "finess",
      label = "FINESS",
      choices = choices,
      selected = random_initial_choice
    )

    table <- reactive({
      (
        full_table
        %>% dplyr::filter(finess_comp == input$finess)
        %>% dplyr::select(r$selected_columns)
      ) -> result

      apply_filter <- function(df, filter) {
        if (is.na(filter$value)) {
          dplyr::filter(df, !is.na(.data[[filter$column]]))
        } else {
          dplyr::filter(df, .data[[filter$column]] != filter$value)
        }
      }

      for (f in r$filters) result <- apply_filter(result, f)

      names(result) <- r$translated_columns

      (
        result
        %>% mutate(across(
            contains("%"),
            ~ scales::percent(as.numeric(.) / 100)
          ))
      ) -> result

      if(r$proper_left_col && length(r$rows_translated) == nrow(result)) {
        result[[r$selected_columns[1]]] <- r$rows_translated
      }

      result
    })

    output$table <- DT::renderDT(
      table(),
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(
        dom = "t",
        pageLength = -1
      )
    )

    create_event_observers <- function() {
      undo_list <- reactiveVal(list())

      a_cell_is_selected <- function() {
        ncol(input$table_cells_selected) > 0
      }
      load_state_from <- function(undo) {
        r$selected_columns   <- undo$selected_columns
        r$translated_columns <- undo$translated_columns
        r$filters            <- undo$filters
        r$row_names          <- undo$row_names
        r$rows_translated    <- undo$rows_translated
      }
      create_state <- function() {
        shiny::reactiveValuesToList(r)
      }
      save_state_to_undo_list <- function() {
        undo_list(c(
          undo_list(),
          list(create_state())
        ))
      }

      observeEvent(input$undo, {
        suppress_last_element <- function(l) l[-length(l)]
        l <- length(undo_list())
        if (l > 0) {
          undo <- undo_list()[[l]]
          load_state_from(undo)
          undo_list(suppress_last_element(undo_list()))
        }
      })
      observeEvent(r$proper_left_col, {
        save_state_to_undo_list()
        req(input$finess)
        (
          full_table
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
          value <- table()[row_nb, pick_value_column] %>% dplyr::pull()
          r$filters <- c(r$filters, list(list(
            column = filter_column,
            value = value
          )))
        }
      })
      observeEvent(input$rm_col, {
        remove_filters_about <- function(column) {
          discard(r$filters, ~ .x$column == column)
        }
        if (a_cell_is_selected()) {
          save_state_to_undo_list()
          col_nb <- input$table_cells_selected[1, 2] + 1
          column <- r$selected_columns[col_nb]
          r$selected_columns <- r$selected_columns[-col_nb]
          r$translated_columns <- r$translated_columns[-col_nb]
          r$filters <- remove_filters_about(column)
        }
      })
    }

    create_event_observers()
    reactive(r) %>% bindEvent(input$save)
  })
}
