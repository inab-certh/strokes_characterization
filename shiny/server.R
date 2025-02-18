shiny::shinyServer(function(input, output,session) {
    
    current_tab <- shiny::reactive({
      selected_menu1 <- input$menu1
      if (selected_menu1 == "overall") {
        return(input$overall_results)
      } else {
        return(input$subgroup_results)
      }
    })
    
    current_analysis <- shiny::reactive({
      analysis <- analysis_ref |> 
        dplyr::filter(
          analysis == input$analysis_name,
          analysisNameShiny == current_tab()
        )
      result <- list(
        id = analysis |> dplyr::pull(analysisId),
        is_binary = analysis |> dplyr::pull(isBinary) == "Y"
      )
    })
    
    file_name_reactive <- shiny::reactive({
      file_name <- input$analysis_name
      selected_menu1 <- input$menu1
      if (selected_menu1 == "overall") {
        file_name <- paste(file_name, "overall", sep = "_")
      } else {
        file_name <- paste(
          file_name,
          "subgroup_analysis",
          input$subgroup_variable,
          sep = "_"
        )
      }
      if (stringr::str_detect(current_output(), "overview")) {
          file_name <- paste(file_name, "overview", sep = "_")
      } else {
        file_name <- paste(file_name, "analysis_id", current_analysis()$id, sep = "_")
      }
      file.path("data", paste0(file_name, ".csv"))
    })
    
    check_is_subgroup <- shiny::reactive({
      if (input$menu1 == "subgroup_analysis") TRUE
      else FALSE
    })
    
    check_is_measurement_range <- shiny::reactive({
      if (current_analysis()$id %in% multiple_concepts) TRUE else FALSE
    })
    check_is_continuous <- shiny::reactive({
      if (current_analysis()$id %in% continuous_concepts) TRUE else FALSE
    })
    
    results_file <- shiny::reactive({
      if (file.exists(file_name_reactive())) {
        result <- readr::read_csv(file.path(file_name_reactive()))
        if (check_is_continuous()) {
          return(result)
        }
        if (check_is_measurement_range()) {
          result <- result |> 
            dplyr::mutate(
              measurement_below_normal_range = measurement_below_normal_range / 100,
              measurement_above_normal_range = measurement_above_normal_range / 100,
              measurement_within_normal_range = measurement_within_normal_range / 100
            )
        } else {
          result <- result |> 
            dplyr::mutate(result = result / 100)
        }
      } else {
        NA
      }
    })
    
    current_output <- shiny::reactive({
      result <- paste(
        input$menu1,
        tolower(current_tab()),
        sep = "_"
      )
      
      stringr::str_replace_all(result, " ", "_")
    })
    
    shiny::observe({
      if (stringr::str_detect(current_output(), "overview")) {
        output[[current_output()]] <- DT::renderDataTable({
          res <- readr::read_csv( file_name_reactive())
          res
        })
      } else {
        if (dplyr::is.tbl(results_file())) {
          if (current_analysis()$is_binary) {
            output[[current_output()]] <- DT::renderDataTable({
              result <- form_table(
                results_file(), 
                check_is_subgroup(),
                check_is_measurement_range()
              ) |> 
                formattable::as.datatable()
            })
          } else {
            output[[current_output()]] <- DT::renderDataTable({
              res <- readr::read_csv(file_name_reactive()) |> 
                dplyr::select(-conceptId)
              DT::datatable(
                res,
                colnames = c(
                  "Analysis",
                  "Covariate name",
                  "1%", "5%", "10%", "25%",
                  "50%", "75%", "90%", "95%", "99%")
              ) |> 
                DT::formatRound(3:11, digits = 0)
            })
          }
        }
      }
    })
    
    # shiny::observe(print(current_analysis()))
    shiny::observe(print(paste("Current output:", current_output())))
    shiny::observe(print(paste("Analysis name:", input$analysis_name)))
    shiny::observe(print(file_name_reactive()))
    # shiny::observe(print(check_is_measurement_range()))
    # shiny::observe(print(results_file() |> data.frame() |> head()))
  })