tableDesignerServer <- function(id, nature, table_name) {
  ovalide::load_ovalide_tables(nature)
  ovalide::load_score(nature)

  score_table    <- ovalide::score(nature)
  choices        <- score_table$Finess
  names(choices) <- score_table$Libellé

  moduleServer(id, function(input, output, session) {

    full_table           <- ovalide::ovalide_tables(nature)[[table_name]]
    original_table_names <- names(full_table) %>% setdiff("finess_comp")
    selected_columns     <- reactiveVal(original_table_names)
    translated_columns   <-reactiveVal(original_table_names)

    output$translation <- shiny::renderUI(
      purrr::map2(selected_columns(),
                  translated_columns(),
                  ~ shiny::textInput(ns(.x), .x, .y))
    )

    ns <- NS(id)
    one_choice <- sample(choices, size = 1)
    shiny::updateSelectInput(session,
                             "finess",
                             label = "FINESS",
                             choices = choices,
                             selected = one_choice)


    table <- reactive({
      (
        full_table
        %>% dplyr::filter(finess_comp == choices)
        %>% dplyr::select(selected_columns())
      ) -> result

      apply_filter <- function(df, filter) {
        if(is.na(filter$value)) {
          dplyr::filter(df, ! is.na(.data[[filter$column]]))
        } else {
          dplyr::filter(df, .data[[filter$column]] != filter$value)
        }
      }

      for (f in filters()) result <- apply_filter(result, f)

      names(result) <- translated_columns()

      (
        result
        %>% mutate(across(contains("%"),
                          ~ scales::percent(as.numeric(.) / 100)))
      )
    })

    output$table <- DT::renderDT(
      table(),
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(dom = 't',
                     pageLength = -1))

    undo_list <- reactiveVal(list())

    suppress_last_element <- function(l) l[-length(l)]

    remove_filters_about <- function(column) {
      (
        filters()
        %>% discard(~ .x$column == column)
      )
    }

    ### Undo event
    observeEvent(input$undo, {
      l = length(undo_list())
      if (l > 0) {
        undo <- undo_list()[[l]]
        load_state_from(undo)
        undo_list(suppress_last_element(undo_list()))
      }
    })

    filters <- reactiveVal(list())

    load_state_from <- function(undo) {
      selected_columns  (undo$selected_columns)
      translated_columns(undo$translated_columns)
      filters           (undo$filters)
    }

    create_state <- function() {
      list(
        selected_columns   = selected_columns(),
        translated_columns = translated_columns(),
        filters            = filters()
      )
    }

    save_state_to_undo_list <- function() {
      undo_list(c(
        undo_list(),
        list(create_state())))
    }

    ## Traduit colonne event
    observeEvent(input$proper_cols, {
      save_state_to_undo_list()
      translated_columns(purrr::map_chr(selected_columns(), ~ input[[.x]]))
    })

    a_cell_is_selected <- function() ncol(input$table_cells_selected) > 0


    ## Remove ligne event
    observeEvent(input$rm_row, {
      if(a_cell_is_selected()) {
        save_state_to_undo_list()
        col_nb <- input$table_cells_selected[1, 2] + 1
        row_nb <- input$table_cells_selected[1, 1]
        pick_value_column <- translated_columns()[col_nb]
        filter_column <- selected_columns()[col_nb]
        value <- table()[row_nb, pick_value_column] %>% dplyr::pull()
        filters(c(filters(), list(list(column = filter_column,
                                       value  = value))))
      }
    })

    ### Remove colonne event
    observeEvent(input$rm_col, {
      if(a_cell_is_selected()) {
        save_state_to_undo_list()
        col_nb <- input$table_cells_selected[1, 2] + 1
        column <- selected_columns()[col_nb]
        selected_columns(selected_columns()[-col_nb])
        translated_columns(translated_columns()[-col_nb])
        filters(remove_filters_about(column))
      }
    })


    list(
      selected_columns   = reactive(selected_columns()),
      translated_columns = reactive(translated_columns()),
      filters            = reactive(filters())
    ) %>% purrr::map(~ bindEvent(.x, input$save))
  })
}
