tableDesignerServer <- function(id, nature, table_name) {
  ovalide::load_ovalide_tables(nature)
  ovalide::load_score(nature)
  (
    ovalide::score(nature)
    %>% dplyr::select(Libellé, Finess)
    %>% dplyr::mutate(choices = paste(Finess, Libellé))
    %>% dplyr::pull(choices)
  ) -> choices

  moduleServer(id, function(input, output, session) {
    full_table <- ovalide::ovalide_tables(nature)[[table_name]]
    one_choice <- sample(choices, size = 1)

    shiny::updateSelectInput(session,
                             "finess",
                             label = "FINESS",
                             choices = choices,
                             selected = one_choice)

    finess_from <- function(input) stringr::str_extract(input, "\\d+")

    table <- reactive({
      (
        full_table
        %>% dplyr::filter(finess_comp == finess_from(input$finess))
        %>% dplyr::select(- dplyr::any_of("finess_comp"))
      )})

    output$table <- DT::renderDT(
      table(),
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(dom = 't',
                     pageLength = -1))


    selected_columns <- reactiveVal(names(full_table))

    output$translation <- shiny::renderUI(
      (
        selected_columns()
        %>% purrr::discard(~ .x == "finess_comp")
        %>% purrr::map(~ shiny::textInput(.x, .x, .x))
      )
    )

    ###  Return values
    # list(
    #
    # )
  })
}
