shinyServer(function(input, output) {
  # upload-data
  data_fun <- reactive({
    cur_path <- input$data$datapath
    if(is.null(cur_path)) {
      cur_path <- "core/p2p_num_scaled.csv"
    }
    cur_data <- read.csv(cur_path)
    rownames(cur_data) <- cur_data[, 1]
    as.matrix(cur_data[, -1])
  })

  # upload-metadata
  row_groups_fun <- reactive({
    cur_path <- input$row_groups$datapath
    cur_data_path <- input$data$datapath
    if(is.null(cur_data_path) & is.null(cur_path)) {
      cur_path <- "core/p2p_gender.csv"
    }
    if(!is.null(cur_path)) {
      return(as.factor(read.csv(cur_path, header = F)[, 1]))
    }
  })

  # upload-groups
  col_groups_fun <- reactive({
    groups_from_names(colnames(data_fun()))
  })

  # build-response-var
  output$cur_response <- renderUI({
    unique_groups <- unique(col_groups_fun())
    checkboxGroupInput("tables", "Variable Groups", choices = unique_groups,
                       selected = unique_groups)
  })

  # build dropdown 1
  output$dropdown_one <- renderUI({
    choices <- colnames(data_fun())
    selectInput("dropdown_one", "Variable 1", choices = choices)
  })

  # build dropdown 2
  output$dropdown_two <- renderUI({
    choices <- colnames(data_fun())
    selectInput("dropdown_two", "Variable 2", choices = choices)
  })

  # fit cv.glmnet
  cur_glmnet <- reactive({
    fit_glmnet(data_fun(), input$tables, input$dropdown_one)
  })

  # Make a scatterplot
  reactive({
    if(!is.null(input$dropdown_one)) {
      scatterplot(data_fun(), input$dropdown_one, input$dropdown_two,
                  row_groups_fun(), input$split_groups)
    } else {
      # in case data is still loading, don't want ggvis to crash
      data.frame(id = 1, x = 0, y = 0) %>% ggvis(x = "x", y = "y") %>% layer_points() %>%
        add_tooltip(function(x) x$id)
    }
  }) %>%
    bind_shiny("scatterplot", "scatterplot_ui")


  # Make a histogram
  output$histogram <- renderPlot({
    print(histogram(data_fun(), input$dropdown_one, input$dropdown_two,
                    row_groups_fun(), input$split_groups))
  })

  output$glmnet_plot <- renderPlot({
    print(plot_lasso_coef(coef(cur_glmnet()$glmnet.fit)[-1, ]))
  })

  output$lm <- renderPrint({
    cur_vars <- get_nonzero_coef(data_fun(), input$tables, input$dropdown_one,
                                 cur_glmnet()$lambda.min)
    print(fit_lm(data_fun(), cur_vars, input$dropdown_one))
  })

  # print sorted correlations
  output$rank_cor <- renderPrint({
    print_cors(cor(data_fun()))
  })

  output$hclust <- renderPlot({
    print(plot_hclust(data_fun(), input$tables))
  })

})
