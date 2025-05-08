library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(tidyr)
library(plotly)
library(DT)
feature1data <- read.csv("cleaned_feature1data.csv")
feature2data <- read.csv("cleaned_feature2data.csv")
feature3data <- read.csv("cleaned_feature3data.csv")
ui <- navbarPage("College Navigator",
                 ## Feature 1
                 tabPanel("Racial Composition by Institution",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          selectInput("selected_inst", "Select Institution:",
                                                      choices = sort(unique(feature1data$INSTNM)),
                                                      selected = "Grinnell College")
                                        ),
                                        mainPanel(
                                          plotlyOutput('racePie')
                                        )
                          )
                 ),
                 ## Feature 2
                 tabPanel("College Finder",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("use_sat", "Filter by SAT Average", value = FALSE),
                              conditionalPanel(
                                condition = "input.use_sat == true",
                                sliderInput("sat_range", "SAT Average Range:", min = 400, max = 1600, value = c(1000, 1400))
                              ),
                              checkboxInput("use_adm", "Filter by Admission Rate", value = FALSE),
                              conditionalPanel(
                                condition = "input.use_adm == true",
                                sliderInput("adm_range", "Admission Rate Range:", min = 0, max = 1, value = c(0.3, 0.8), step = 0.01)
                              ),
                              checkboxGroupInput("pub_filter", "Institution Type:",
                                                 choices = list("Public" = 1, "Private" = 0)),
                              checkboxGroupInput("locale_filter", "Locale:",
                                                 choices = list("Suburb/City" = 1, "Rural/Town" = 0)),
                              actionButton("show_result", "Show Result", class = "btn-success")
                            ),
                            mainPanel(
                              plotlyOutput("college_plot"),
                              dataTableOutput("school_info"),
                              uiOutput("school_link")
                            )
                          )
                 ),
                 ## Feature 3
                 tabPanel("Find Similar Colleges",
                          sidebarLayout(
                            sidebarPanel(
                              #p("The number of colleges has been filtered to only include colleges that contain all data relevant to the clustering variables"),
                              selectInput("selected_inst_2", "Select Institution",
                                          choices = sort(unique(feature3data$INSTNM))),
                              
                              checkboxGroupInput("show_only_cluster", "Show Only Cluster of Selected Institution",
                                                 choices = list("Filter" = 1)),
                              
                              checkboxGroupInput("enrollment_filter", "Similar Enrollment Size",
                                                 choices = list("Include" = 1)),
                              checkboxGroupInput("ACT_filter", "Similar Test Score",
                                                 choices = list("Include" = 1)),
                              checkboxGroupInput("admission_filter", "Similar Admission Rate",
                                                 choices = list("Include" = 1)),
                              checkboxGroupInput("debt_filter", "Similar Graduate Debt",
                                                 choices = list("Include" = 1)),
                              
                              numericInput("n_clusters", "Number of Clusters (Specificity of Similarity)", value = 5, min = 2, max = 20),
                              
                              actionButton("show_result_3", "Show Similar Colleges")
                            ),
                            
                            mainPanel(
                              plotlyOutput("cluster_plot")
                            )
                          )
                 )
)

## Set up the server function
server <- function(input, output){
  ## Feature 1
  output$racePie <- renderPlotly({
    selected_data <- feature1data %>%
      filter(INSTNM == input$selected_inst)
    race_labels <- c("White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
                     "Two or More", "Non-Resident Alien", "Unknown")
    
    race_columns <- c("UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN",
                      "UGDS_AIAN", "UGDS_NHPI", "UGDS_2MOR", "UGDS_NRA", "UGDS_UNKN")
    
    race_values <- as.numeric(selected_data[1, race_columns])
    
    valid_indices <- !is.na(race_values) & race_values > 0
    race_labels <- race_labels[valid_indices]
    race_values <- race_values[valid_indices]
    
    plot_ly(
      labels = race_labels,
      values = race_values,
      type = "pie"
    ) %>%
      layout(title = paste("Racial Composition of", input$selected_inst))
  })
  ## Feature 2
  filtered_data <- eventReactive(input$show_result, {
    df <- feature2data
    
    if (length(input$pub_filter) > 0) {
      df <- df %>% filter(IS_PUBLIC %in% as.numeric(input$pub_filter))
    }
    
    if (length(input$locale_filter) > 0) {
      df <- df %>% filter(IS_CITY %in% as.numeric(input$locale_filter))
    }
    
    if (input$use_sat) {
      df <- df %>% filter(!is.na(SAT_AVG), SAT_AVG >= input$sat_range[1], SAT_AVG <= input$sat_range[2])
    }
    
    if (input$use_adm) {
      df <- df %>% filter(!is.na(ADM_RATE), ADM_RATE >= input$adm_range[1], ADM_RATE <= input$adm_range[2])
    }
    
    if (nrow(df) == 0) {
      showNotification("No results found based on current filters.", type = "error")
      return(data.frame())
    }
    
    return(df)
  })
  
  output$college_plot <- renderPlotly({
    df <- filtered_data()
    
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    df <- df %>%
      mutate(
        SAT_AVG = as.numeric(SAT_AVG),
        ADM_RATE = as.numeric(ADM_RATE)
      ) %>%
      filter(!is.na(SAT_AVG), !is.na(ADM_RATE))
    
    plot_ly(
      data = df,
      x = ~SAT_AVG,
      y = ~ADM_RATE,
      type = 'scatter',
      mode = 'markers',
      text = ~paste(
        "School:", INSTNM,
        "<br>SAT:", SAT_AVG,
        "<br>Adm Rate:", round(ADM_RATE, 3)
      ),
      hoverinfo = 'text',
      marker = list(color = 'rgba(0, 102, 204, 0.6)', size = 10)
    ) %>%
      layout(
        title = "Colleges: SAT vs Admission Rate",
        xaxis = list(title = "Average SAT Score"),
        yaxis = list(title = "Admission Rate"),
        annotations = list(
          x = 0.5, y = 1,  # Positioning the subtitle
          text = "Hover over and click on a point to see details about the school.",
          showarrow = FALSE,
          font = list(size = 14),
          align = 'center',
          xref = 'paper', yref = 'paper'
        )
      )
  })
  
  # Show selected school info when user clicks a point
  output$school_info <- renderDT({
    event <- event_data("plotly_click")
    df <- filtered_data()
    
    if (is.null(event) || nrow(df) == 0) return(NULL)
    
    # Find the clicked school
    clicked <- df %>%
      mutate(
        SAT_AVG = as.numeric(SAT_AVG),
        ADM_RATE = as.numeric(ADM_RATE)
      ) %>%
      filter(near(SAT_AVG, event$x), near(ADM_RATE, event$y))
    
    if (nrow(clicked) == 0) {
      showNotification("No matching school found for the selected point.", type = "error")
      return(NULL)
    }
    
    # Select the relevant columns to display in the table
    clicked %>%
      select(School = INSTNM, SAT_Avg = SAT_AVG, Admission_Rate = ADM_RATE) %>%
      datatable(
        options = list(
          pageLength = 5,      # Number of rows per page
          dom = 't',           # Only show the table (remove search and entries)
          searching = FALSE,   # Disable the search bar
          lengthChange = FALSE # Remove the "Show entries" dropdown
        )
      )
  })
  
  # Show clickable URL
  output$school_link <- renderUI({
    event <- event_data("plotly_click")
    df <- filtered_data()
    
    if (is.null(event) || nrow(df) == 0) return(NULL)
    
    clicked <- df %>%
      mutate(
        SAT_AVG = as.numeric(SAT_AVG),
        ADM_RATE = as.numeric(ADM_RATE)
      ) %>%
      filter(SAT_AVG == event$x, ADM_RATE == event$y)
    
    if (nrow(clicked) == 0) return(NULL)
    
    tags$a(
      href = ifelse(grepl("^https?://", clicked$INSTURL), clicked$INSTURL, paste0("https://", clicked$INSTURL)),
      target = "_blank",
      "Visit School Website"
    )
  })
  ## Feature 3
  filtered_data_3 <- eventReactive(input$show_result_3, {
    df_pca <- feature3data
    
    if (!1 %in% input$enrollment_filter) {
      df_pca <- select(df_pca, -UGDS)
    }
    if (!1 %in% input$ACT_filter) {
      df_pca <- select(df_pca, -ACT_MEDIAN)
    }
    if (!1 %in% input$admission_filter) {
      df_pca <- select(df_pca, -ADM_RATE)
    }
    if (!1 %in% input$debt_filter) {
      df_pca <- select(df_pca, -GRAD_DEBT_MDN)
    }
    
    return(df_pca)
  })
  
  output$cluster_plot <- renderPlotly({
    df_pca <- filtered_data_3()
    
    if (!is.data.frame(df_pca) || nrow(df_pca) == 0) {
      return(NULL)
    }
    
    if ("INSTNM" %in% colnames(df_pca)) {
      school_names <- df_pca$INSTNM
      features <- select(df_pca, -INSTNM)
    } else {
      school_names <- paste("School", 1:nrow(df_pca))
      features <- df_pca
    }
    
    features_scaled <- scale(features)
    
    # K-means clustering & PCA
    k <- input$n_clusters
    if (nrow(features_scaled) < k) {
      showNotification("Too few schools to form this many clusters. Please reduce the number of clusters.", type = "error")
      return(NULL)
    }
    kmeans_result <- tryCatch({
      kmeans(features_scaled, centers = k, nstart = 25)
    }, error = function(e) {
      showNotification("Clustering failed: Try selecting more variables or fewer clusters.", type = "error")
      return(NULL)
    })
    if (is.null(kmeans_result)) return(NULL)
    
    
    pca <- tryCatch({
      prcomp(features_scaled)
    }, error = function(e) {
      showNotification("PCA failed: Not enough variables for dimensionality reduction.", type = "error")
      return(NULL)
    })
    if (is.null(pca)) return(NULL)
    
    if (ncol(pca$x) < 2) {
      showNotification("PCA failed: not enough dimensions to display 2 components. Select more or different variables.", type = "error")
      return(NULL)
    }
    
    pca_data <- as.data.frame(pca$x[, 1:2])
    colnames(pca_data) <- c("PC1", "PC2")
    pca_data$School <- school_names
    pca_data$Cluster <- as.factor(kmeans_result$cluster)
    
    # Show only the cluster of the school you want to find similar schools for
    if ("1" %in% input$show_only_cluster) {
      selected_school <- input$selected_inst_2
      
      if (!(selected_school %in% pca_data$School)) {
        showNotification("Selected institution not found in clustered data.", type = "error")
        return(NULL)
      }
      
      selected_cluster <- pca_data$Cluster[pca_data$School == selected_school][1]
      pca_data <- pca_data[pca_data$Cluster == selected_cluster, ]
    }
    
    
    plot_ly(pca_data,
            x = ~PC1,
            y = ~PC2,
            type = 'scatter',
            mode = 'markers',
            color = ~Cluster,
            text = ~paste("School:", School,
                          "<br>Cluster:", Cluster),
            hoverinfo = 'text') %>%
      layout(
        title = "Interactive K-means Cluster Plot (PCA)",
        xaxis = list(title = "Principal Component 1"),
        yaxis = list(title = "Principal Component 2"),
        annotations = list(
          x = 0.5, y = 1,
          text = "Hover over a point to see which school it is.",
          showarrow = FALSE,
          font = list(size = 14),
          align = 'center',
          xref = 'paper', yref = 'paper'
        )
      )
  })
}


shinyApp(ui, server)