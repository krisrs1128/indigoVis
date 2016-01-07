
shinyUI(fluidPage(
  titlePanel("Indigo Vis"),
  sidebarLayout(
    sidebarPanel(
      h3("Upload Data"),
      h4("Primary Data"),
      p("Upload a csv with the following format:"),
      tags$ul(list(tags$li("The first row gives column names, and the rest have corresponding data."), tags$li("The first column gives student IDs, and the rest give responses for different questions."), tags$li("The column names are prepended with the groups from which they were obtained, followed by an underscore. For example, we can indicate that the questions came from two groups by giving column names like group1_Q1A | group1_Q1B | group2_Q2A | group2_Q2B."))),
      fileInput("data", "Upload Data"),
      h4("Supplementary Data (Optional)"),
      p("Upload a csv with the following format:"),
      tags$ul(list(tags$li("The data is a single column, with no header, and the same number of rows as the main data input."), tags$li("The 1st row of this file provides the label for the 1st sample in the main data file, the second gives the label for the second sample, etc."))),
      fileInput("row_groups", "Upload Sample Types"),
      h3("Choose Variables"),
      p("Check the box below to incorporate sample labels in analysis.  For example, checking this box might split the scatterplot into male and female points."),
      checkboxInput("split_groups", "Factor by Sample Type", value = TRUE),
      uiOutput("cur_response"),
      uiOutput("dropdown_one"),
      uiOutput("dropdown_two")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("scatterplot",
                 h3("Scatterplot"),
                 p("Displays a scatterplot of Variable 1 and Variable 2.  Make sure to select different variables in the left panel -- you can also search by typing in the box.  This allows you to see correlations and patterns of interactions between the two variables at a glance. However over a point to see which student the point corresponds to."),
                 ggvisOutput("scatterplot")),
        tabPanel("histogram",
                 h3("Histogram"),
                 p("Compares the distributions of Variable 1 and Variable 2.  The x-axis has possible values for that variable and the y-axis is the number of students who scored that value."),
                 plotOutput("histogram")),
        tabPanel("clustering",
                 h3("Clustering"),
                 p("Shows similarity of survey items at a glance.  The farther to the left that two items get merged together (or clustered), the more relatively similar they are.  Use the Variable Groups checkboxes to select the type of variables you are interested in clustering."),
                 plotOutput("hclust", height = "800px")),
        tabPanel("correlations",
                 h3("Correlations"),
                 p("Ranks the variables by pairwise correlations.  May take some time to load.  The correlation is listed on the right.  Very high correlations (>.95) might indicate problems with data scaling or formatting."),
                 verbatimTextOutput("rank_cor")),
        tabPanel("models",
                 h3("Models"),
                 p("More advanced analysis.  This tab regresses Variable 1 on the Variable Groups that are selected.  It tries to fit various models with regularization."),
                 p("Try checking one of the Variable Groups and changing Variable 1."),
                 p("The top graphic shows how the regression coefficients vary over time.  Variables with strong colors to the left are the ones that are the most predictive of Variable 1.  Blue represents variables with positive coefficients (increases in that variable predict increases in Variable 1), and red represents negative coefficients."),
                 p("The computer searches for the best model and then outputs the result below.  The bottom readout will tell you the amount of explained variance (Adjusted R-squared), which can be interpreted as telling you what percent of Variable 1 can be explained by variations in the a subset of the Variable Group selected.  You can also see which variables were selected to be predictors for Variable 1.  You can use the selection of these variables as an indicator that they are 'good' predictors of Variable 1.  However, the p-values are unreliable and should not be used."),
                 p("For more advanced users, the model was fit with the GLMNET package for elastic net regression which uses a combination of L1 and L2 regularization.  The graphic shows a lambda path.  The output shows a summary of a straight OLS fit using the nonzero features from the lasso with the best lambda (chosen via cross-validation)."),
                 plotOutput("glmnet_plot", height = "800px"),
                 verbatimTextOutput("lm"))
      )
    )
  )
))
