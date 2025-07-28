library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# Read data
feature1data <- read.csv("cleaned_feature1data.csv")
feature2data <- read.csv("cleaned_feature2data.csv")
feature3data <- read.csv("cleaned_feature3data.csv")
cleaned_data <- read.csv("cleaned_data.csv")

# Centralized institution list (sorted and unique)
all_institutions <- sort(unique(c(
  feature1data$INSTNM,
  feature2data$INSTNM,
  feature3data$INSTNM
)))

ui <- navbarPage("College Navigator", id = "navbar",
                 ## Feature 1
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
                              checkboxGroupInput("locale_filter", "Location:",
                                                 choices = list("Suburb/City" = 1, "Rural/Town" = 0)),
                              actionButton("show_result", "Show Result", class = "btn-success")
                            ),
                            mainPanel(
                              plotlyOutput("college_plot"),
                              dataTableOutput("school_info"),
                              uiOutput("compare_button")
                            )
                          )
                 ),
                 ## Feature 2
                 tabPanel("Compare Colleges",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Select Two Institutions to Compare"),
                              selectizeInput("college1", "First Institution:",
                                          choices = all_institutions,
                                          selected = "Grinnell College",
                                          options = list(maxItems = 1),
                                          width = "100%"),
                              selectizeInput("college2", "Second Institution:",
                                          choices = all_institutions,
                                          selected = "Carleton College",
                                          options = list(maxItems = 1),
                                          width = "100%"),
                              hr(),
                              h4("Comparison Categories"),
                              checkboxGroupInput("compare_categories", "Select categories to compare:",
                                                 choices = list(
                                                   "Academic Profile" = "academic_profile",
                                                   "Student Body" = "student_body", 
                                                   "Institution Details" = "institution_details"
                                                 ),
                                                 selected = c("academic_profile", "student_body", "institution_details")),
                            ),
                            mainPanel(
                              conditionalPanel(
                                condition = "input.compare_categories.length > 0", # Changed condition
                                h3("College Comparison Results"),
                                hr(),

                                conditionalPanel(
                                  condition = "'institution_details' in input.compare_categories",
                                  h4("Institution Details Comparison"),
                                  tableOutput("institution_info_comparison"),
                                  hr()
                                ),
                                # Summary table
                                h4("Summary Comparison Table"),
                                dataTableOutput("comparison_table")
                              )
                            )
                          )
                 ),
                 ## Feature 3
                 tabPanel("Gender and Racial Composition",
                          fluidRow(
                            column(12,
                              selectizeInput("selected_inst", "Select Institution:",
                                          choices = all_institutions,
                                          selected = "Grinnell College",
                                          options = list(maxItems = 1),
                                          width = "100%")
                            )
                          ),
                          fluidRow(
                            column(6, plotlyOutput('genderPie', height = "600px")),
                            column(6, plotlyOutput('racePie', height = "600px"))
                          )
                 ),
                 ## Feature 4
                 tabPanel("Find Similar Colleges",
                          sidebarLayout(
                            sidebarPanel(
                              #p("The number of colleges has been filtered to only include colleges that contain all data relevant to the clustering variables"),
                              selectizeInput("selected_inst_2", "Select Institution",
                                          choices = all_institutions,
                                          options = list(maxItems = 1),
                                          width = "100%"),
                              
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
                              plotlyOutput("cluster_plot"),
                              hr(),
                              uiOutput("cluster_school_header"),
                              dataTableOutput("cluster_school_info"),
                              br(),
                              uiOutput("cluster_school_link")
                            )
                          )
                 )
)

## Set up the server function
server <- function(input, output){
  selected_college <- reactiveValues(name = NULL)
  selected_schools <- reactiveValues(schools = list())
  
  # Helper function for creating pie charts
  create_pie_chart <- function(selected_data, labels, columns, title, institution_name, legend_y = -0.05) {
    if (nrow(selected_data) == 0) {
      return(plot_ly(
        labels = c("No data"),
        values = c(1),
        type = "pie",
        marker = list(colors = c("lightgrey")),
        textinfo = "label",
        textfont = list(color = 'lightgrey'),
        hoverinfo = "none"
      ) %>%
        layout(
          title = paste(title, "of", institution_name),
          showlegend = FALSE,
          margin = list(l = 50, r = 50, t = 60, b = 50),
          height = 400,
          autosize = TRUE,
          annotations = list(
            text = paste("No valid", tolower(title), "data available"),
            showarrow = FALSE,
            font = list(size = 16),
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper"
          )
        ))
    }
    
    values <- as.numeric(selected_data[1, columns])
    valid_indices <- !is.na(values) & values > 0
    labels <- labels[valid_indices]
    values <- values[valid_indices]
    
    percentages <- values * 100
    text_labels <- ifelse(percentages >= 1, labels, "")
    
    plot_ly(
      labels = labels,
      values = percentages,
      type = "pie",
      domain = list(x = c(0.10, 0.90), y = c(0.10, 0.90)),
      text = text_labels,
      textinfo = "text",
      textposition = "outside",
      hovertemplate = "%{label}<br>%{value:.1f}%<extra></extra>"
    ) %>%
      layout(
        title = paste(title, "of", institution_name),
        margin = list(l = 50, r = 50, t = 80, b = 160),
        height = 600,
        autosize = FALSE,
        title = list(
          x = 0.5,
          xanchor = "center",
          font = list(size = 14)
        ),
        legend = list(
          x = 0.5,
          y = legend_y,
          xanchor = "center",
          orientation = "h"
        )
      )
  }
  
  ## Feature: Gender Composition
  output$genderPie <- renderPlotly({
    selected_data <- feature1data %>% filter(INSTNM == input$selected_inst)
    gender_labels <- c("Men", "Women")
    gender_columns <- c("UGDS_MEN", "UGDS_WOMEN")
    create_pie_chart(selected_data, gender_labels, gender_columns, "Gender Composition", input$selected_inst)
  })
  
  ## Feature: Racial Composition
  output$racePie <- renderPlotly({
    selected_data <- feature1data %>% filter(INSTNM == input$selected_inst)
    race_labels <- c("White", "Black", "Hispanic", "Asian", "American Indian/Alaska Native", "Native Hawaiian/Pacific Islander",
                     "Two or More", "Non-Resident Alien", "Unknown")
    race_columns <- c("UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN",
                      "UGDS_AIAN", "UGDS_NHPI", "UGDS_2MOR", "UGDS_NRA", "UGDS_UNKN")
    create_pie_chart(selected_data, race_labels, race_columns, "Racial Composition", input$selected_inst, -0.15)
  })
  
  
  ## Feature: Filter College
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
          text = "Hover over and click on points to select up to 2 schools for comparison.",
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
    
    # Add school to selected schools (up to 2)
    school_name <- clicked$INSTNM[1]
    if (!(school_name %in% selected_schools$schools)) {
      if (length(selected_schools$schools) < 2) {
        selected_schools$schools <- c(selected_schools$schools, school_name)
      } else {
        # Replace the oldest selection
        selected_schools$schools <- c(selected_schools$schools[-1], school_name)
      }
    }
    
    # Get data for all selected schools
    if (length(selected_schools$schools) == 0) return(NULL)
    
    all_schools_data <- data.frame()
    for (i in seq_along(selected_schools$schools)) {
      school_data <- df %>% filter(INSTNM == selected_schools$schools[i])
      if (nrow(school_data) > 0) {
        school_data$ViewDemographics <- paste0(
          '<button class="btn btn-primary btn-sm action-button" id="view_', 
          school_data$INSTNM, 
          '" onclick="Shiny.setInputValue(\'go_to_race\', \'', 
          school_data$INSTNM, 
          '\', {priority: \'event\'})">View Gender & Racial Composition</button>'
        )
        all_schools_data <- rbind(all_schools_data, school_data)
      }
    }
    
    if (nrow(all_schools_data) == 0) return(NULL)
    
    all_schools_data <- all_schools_data %>%
      select(School = INSTNM, SAT_Avg = SAT_AVG, Admission_Rate = ADM_RATE, Demographics = ViewDemographics)
    
    datatable(all_schools_data, escape = FALSE, options = list(
      pageLength = 10,     # Increased for multiple schools
      dom = 't',           # Only show the table (remove search and entries)
      searching = FALSE,   # Disable the search bar
      lengthChange = FALSE # Remove the "Show entries" dropdown
    ))
  })
  

  

  ## Feature: Find Similar Colleges
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
            hoverinfo = 'text',
            customdata = ~School) %>%
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
  
  # Show dynamic header with school name
  output$cluster_school_header <- renderUI({
    event <- event_data("plotly_click")
    if (is.null(event)) return(NULL)
    
    clicked_school <- event$customdata
    if (is.null(clicked_school)) return(NULL)
    
    tags$h4(paste(clicked_school))
  })
  
  # Show selected school info when user clicks a point in Feature 3
  output$cluster_school_info <- renderDT({
    event <- event_data("plotly_click")
    df_pca <- filtered_data_3()
    
    # Explicitly depend on all filter inputs to ensure reactivity
    enrollment_selected <- "1" %in% input$enrollment_filter
    act_selected <- "1" %in% input$ACT_filter
    admission_selected <- "1" %in% input$admission_filter
    debt_selected <- "1" %in% input$debt_filter
    
    if (is.null(event) || !is.data.frame(df_pca) || nrow(df_pca) == 0) return(NULL)
    
    # Find the clicked school using customdata
    clicked_school <- event$customdata
    if (is.null(clicked_school)) return(NULL)
    
    clicked <- df_pca %>%
      filter(INSTNM == clicked_school)
    
    if (nrow(clicked) == 0) {
      showNotification("No matching school found for the selected point.", type = "error")
      return(NULL)
    }
    
    # Get additional info from feature2data for website and other details
    school_info <- feature2data %>% filter(INSTNM == clicked$INSTNM[1])
    
    # Create detailed info table based on selected filters
    info_data <- data.frame(
      Metric = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    # Only show enrollment if user selected "Similar Enrollment Size"
    if (enrollment_selected) {
      # Get enrollment from original feature3data since it might be removed from filtered data
      original_school_data <- feature3data %>% filter(INSTNM == clicked_school)
      if (nrow(original_school_data) > 0 && !is.na(original_school_data$UGDS[1])) {
        info_data <- rbind(info_data, data.frame(
          Metric = "Enrollment",
          Value = format(original_school_data$UGDS[1], big.mark = ","),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Only show ACT median if user selected "Similar Test Score"
    if (act_selected) {
      # Get ACT median from original feature3data since it might be removed from filtered data
      original_school_data <- feature3data %>% filter(INSTNM == clicked_school)
      if (nrow(original_school_data) > 0 && !is.na(original_school_data$ACT_MEDIAN[1])) {
        info_data <- rbind(info_data, data.frame(
          Metric = "ACT Median",
          Value = as.character(original_school_data$ACT_MEDIAN[1]),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Only show admission rate if user selected "Similar Admission Rate"
    if (admission_selected) {
      # Get admission rate from original feature3data since it might be removed from filtered data
      original_school_data <- feature3data %>% filter(INSTNM == clicked_school)
      if (nrow(original_school_data) > 0 && !is.na(original_school_data$ADM_RATE[1])) {
        info_data <- rbind(info_data, data.frame(
          Metric = "Admission Rate",
          Value = paste0(round(original_school_data$ADM_RATE[1] * 100, 1), "%"),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Only show graduate debt if user selected "Similar Graduate Debt"
    if (debt_selected) {
      # Get graduate debt from original feature3data since it might be removed from filtered data
      original_school_data <- feature3data %>% filter(INSTNM == clicked_school)
      if (nrow(original_school_data) > 0 && !is.na(original_school_data$GRAD_DEBT_MDN[1])) {
        info_data <- rbind(info_data, data.frame(
          Metric = "Graduate Debt",
          Value = paste0("$", format(original_school_data$GRAD_DEBT_MDN[1], big.mark = ",")),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # If no data to show, return message
    if (nrow(info_data) == 0) {
      return(datatable(data.frame(Message = "No data available for selected categories"), 
                      options = list(dom = 't', searching = FALSE, lengthChange = FALSE)))
    }
    
    datatable(info_data, 
              options = list(
                pageLength = 5,
                dom = 't',
                searching = FALSE,
                lengthChange = FALSE
              ),
              rownames = FALSE,
              colnames = c("Metric", "Value"))
  })
  
  # Show clickable URL for Feature 3
  output$cluster_school_link <- renderUI({
    event <- event_data("plotly_click")
    df_pca <- filtered_data_3()
    
    if (is.null(event) || !is.data.frame(df_pca) || nrow(df_pca) == 0) return(NULL)
    
    # Find the clicked school using customdata
    clicked_school <- event$customdata
    if (is.null(clicked_school)) return(NULL)
    
    clicked <- df_pca %>%
      filter(INSTNM == clicked_school)
    
    if (nrow(clicked) == 0) return(NULL)
    
    # Get website from feature2data
    school_info <- feature2data %>% filter(INSTNM == clicked$INSTNM[1])
    
    if (nrow(school_info) == 0 || is.na(school_info$INSTURL[1])) return(NULL)
    
    tags$a(
      href = ifelse(grepl("^https?://", school_info$INSTURL[1]), 
                    school_info$INSTURL[1], 
                    paste0("https://", school_info$INSTURL[1])),
      target = "_blank",
      "Visit School Website"
    )
  })
  
  observeEvent(input$go_to_race, {
    selected_college$name <- input$go_to_race
    updateSelectInput(inputId = "selected_inst", selected = selected_college$name)
    updateTabsetPanel(session = getDefaultReactiveDomain(), inputId = "navbar", selected = "Gender and Racial Composition")
  })
  
  # Handle demographics button from comparison table
  observeEvent(input$go_to_race_from_compare, {
    selected_college$name <- input$go_to_race_from_compare
    updateSelectInput(inputId = "selected_inst", selected = selected_college$name)
    updateTabsetPanel(session = getDefaultReactiveDomain(), inputId = "navbar", selected = "Gender and Racial Composition")
  })
  
  # Render compare button when 2 schools are selected
  output$compare_button <- renderUI({
    if (length(selected_schools$schools) == 2) {
      tags$div(
        style = "margin-top: 20px; text-align: center;",
        actionButton("go_to_compare", "Compare the Two Colleges", 
                   class = "btn-primary btn-sm")
      )
    }
  })
  
  # Handle compare button click
  observeEvent(input$go_to_compare, {
    if (length(selected_schools$schools) == 2) {
      # Set the two colleges in the Compare Colleges feature
      updateSelectInput(inputId = "college1", selected = selected_schools$schools[1])
      updateSelectInput(inputId = "college2", selected = selected_schools$schools[2])
      # Navigate to Compare Colleges tab
      updateTabsetPanel(session = getDefaultReactiveDomain(), inputId = "navbar", selected = "Compare Colleges")
    }
  })
  

  
  ## Feature 4: College Comparison
  # Get comparison data
  comparison_data <- reactive({
    college1_data <- list()
    college2_data <- list()
    
    # Helper function to load data for a college
    load_college_data <- function(college_name, data_source) {
      data_source %>% filter(INSTNM == college_name)
    }
    
    # Get student body data (demographics)
    if ("student_body" %in% input$compare_categories) {
      college1_demo <- load_college_data(input$college1, feature1data)
      college2_demo <- load_college_data(input$college2, feature1data)
      
      if (nrow(college1_demo) > 0) college1_data$student_body <- college1_demo
      if (nrow(college2_demo) > 0) college2_data$student_body <- college2_demo
    }
    
    # Get academic profile and institution details data
    if ("academic_profile" %in% input$compare_categories || "institution_details" %in% input$compare_categories) {
      college1_admissions <- load_college_data(input$college1, feature2data)
      college2_admissions <- load_college_data(input$college2, feature2data)
      
      if (nrow(college1_admissions) > 0) {
        college1_data$academic_profile <- college1_admissions
        college1_data$institution_details <- college1_admissions
      }
      if (nrow(college2_admissions) > 0) {
        college2_data$academic_profile <- college2_admissions
        college2_data$institution_details <- college2_admissions
      }
    }
    
    # Get additional data from cleaned_data
    college1_additional <- load_college_data(input$college1, cleaned_data)
    college2_additional <- load_college_data(input$college2, cleaned_data)
    
    if (nrow(college1_additional) > 0) {
      college1_data$ugds <- college1_additional
      college1_data$additional <- college1_additional
    }
    if (nrow(college2_additional) > 0) {
      college2_data$ugds <- college2_additional
      college2_data$additional <- college2_additional
    }
    
    list(college1 = college1_data, college2 = college2_data)
  })

  # Helper functions for comparison
  get_institution_info <- function(college_data) {
    info <- college_data$institution_info
    if (is.null(info)) {
      info <- college_data$admissions
    }
    if (is.null(info)) return(c("N/A", "N/A"))
    
    locale <- ifelse(!is.null(info$IS_CITY[1]), 
                     ifelse(info$IS_CITY[1] == 1, "City/Suburb", "Rural/Town"), "N/A")
    type <- ifelse(!is.null(info$IS_PUBLIC[1]), 
                   ifelse(info$IS_PUBLIC[1] == 1, "Public", "Private"), "N/A")
    return(c(locale, type))
  }
  
  # Helper function to add rows to summary data
  add_summary_row <- function(summary_data, metric, college1_val, college2_val) {
    rbind(summary_data, data.frame(
      Metric = metric,
      College1 = college1_val,
      College2 = college2_val,
      stringsAsFactors = FALSE
    ))
  }
  
  output$institution_info_comparison <- renderTable({
    data <- comparison_data()
    if ((is.null(data$college1$institution_info) && is.null(data$college1$admissions)) || 
        (is.null(data$college2$institution_info) && is.null(data$college2$admissions))) {
      return(data.frame(Message = "No institution info available for comparison"))
    }
    
    college1_info <- get_institution_info(data$college1)
    college2_info <- get_institution_info(data$college2)
    
    info_table <- data.frame(
      Category = c("Location", "Institution Type"),
      !!input$college1 := college1_info,
      !!input$college2 := college2_info
    )
    info_table
  })
  
  # Summary comparison table
  output$comparison_table <- renderDT({
    data <- comparison_data()
    
    if (length(data$college1) == 0 && length(data$college2) == 0) {
      return(datatable(data.frame(Message = "No data available for selected colleges")))
    }
    
    # Create summary table
    summary_data <- data.frame(
      Metric = character(),
      College1 = character(),
      College2 = character(),
      stringsAsFactors = FALSE
    )
    

    
    # Add student body data
    if ("student_body" %in% input$compare_categories && 
        !is.null(data$college1$student_body) && !is.null(data$college2$student_body)) {
      demo1 <- data$college1$student_body
      demo2 <- data$college2$student_body
      
      # Add UGDS data to Student Body section
      if (!is.null(data$college1$ugds) && !is.null(data$college2$ugds)) {
        ugds1 <- data$college1$ugds
        ugds2 <- data$college2$ugds
        
        summary_data <- add_summary_row(summary_data, "Undergraduate Enrollment",
          ifelse(is.na(ugds1$UGDS[1]), "N/A", format(as.numeric(ugds1$UGDS[1]), big.mark = ",")),
          ifelse(is.na(ugds2$UGDS[1]), "N/A", format(as.numeric(ugds2$UGDS[1]), big.mark = ","))
        )
      }
      
      # Add PCTPELL data to Student Body section
      if (!is.null(data$college1$ugds) && !is.null(data$college2$ugds)) {
        pctpell1 <- data$college1$ugds
        pctpell2 <- data$college2$ugds
        
        summary_data <- add_summary_row(summary_data, "Pell Grant Recipients",
          ifelse(is.na(pctpell1$PCTPELL[1]), "N/A", paste0(round(as.numeric(pctpell1$PCTPELL[1]) * 100, 1), "%")),
          ifelse(is.na(pctpell2$PCTPELL[1]), "N/A", paste0(round(as.numeric(pctpell2$PCTPELL[1]) * 100, 1), "%"))
        )
      }
    }
    
    # Add academic profile data
    if ("academic_profile" %in% input$compare_categories && 
        !is.null(data$college1$academic_profile) && !is.null(data$college2$academic_profile)) {
      adm1 <- data$college1$academic_profile
      adm2 <- data$college2$academic_profile
      
      summary_data <- add_summary_row(summary_data, "SAT Average",
        ifelse(is.na(adm1$SAT_AVG[1]), "N/A", as.numeric(adm1$SAT_AVG[1])),
        ifelse(is.na(adm2$SAT_AVG[1]), "N/A", as.numeric(adm2$SAT_AVG[1]))
      )
      summary_data <- add_summary_row(summary_data, "Admission Rate",
        ifelse(is.na(adm1$ADM_RATE[1]), "N/A", paste0(round(as.numeric(adm1$ADM_RATE[1]) * 100, 1), "%")),
        ifelse(is.na(adm2$ADM_RATE[1]), "N/A", paste0(round(as.numeric(adm2$ADM_RATE[1]) * 100, 1), "%"))
      )
      
      # Add graduation rate to Academic Profile section
      if (!is.null(data$college1$additional) && !is.null(data$college2$additional)) {
        add1 <- data$college1$additional
        add2 <- data$college2$additional
        
        summary_data <- add_summary_row(summary_data, "4-Year Graduation Rate",
          ifelse(is.na(add1$C200_4[1]), "N/A", paste0(round(as.numeric(add1$C200_4[1]) * 100, 1), "%")),
          ifelse(is.na(add2$C200_4[1]), "N/A", paste0(round(as.numeric(add2$C200_4[1]) * 100, 1), "%"))
        )
      }
    }

    if ("institution_details" %in% input$compare_categories && 
        ((!is.null(data$college1$institution_details) || !is.null(data$college1$academic_profile)) && 
         (!is.null(data$college2$institution_details) || !is.null(data$college2$academic_profile)))) {
      college1_info <- get_institution_info(data$college1)
      college2_info <- get_institution_info(data$college2)
      
      summary_data <- add_summary_row(summary_data, "Location", college1_info[1], college2_info[1])
      summary_data <- add_summary_row(summary_data, "Institution Type", college1_info[2], college2_info[2])
      

      
      # Add cost of attendance to Institution Details section
      if (!is.null(data$college1$additional) && !is.null(data$college2$additional)) {
        add1 <- data$college1$additional
        add2 <- data$college2$additional
        
        # Add cost of attendance
        summary_data <- add_summary_row(summary_data, "Average Cost of Attendance",
          ifelse(is.na(add1$COSTT4_A[1]), "N/A", paste0("$", format(as.numeric(add1$COSTT4_A[1]), big.mark = ","))),
          ifelse(is.na(add2$COSTT4_A[1]), "N/A", paste0("$", format(as.numeric(add2$COSTT4_A[1]), big.mark = ",")))
        )
      }
    }
    
    # Add website links at the end (always show if data is available)
    if (!is.null(data$college1$additional) && !is.null(data$college2$additional)) {
      add1 <- data$college1$additional
      add2 <- data$college2$additional
      
      # Format URLs properly (add https:// if not present)
      url1 <- ifelse(is.na(add1$INSTURL[1]), "N/A", 
                    ifelse(grepl("^https?://", add1$INSTURL[1]), add1$INSTURL[1], paste0("https://", add1$INSTURL[1])))
      url2 <- ifelse(is.na(add2$INSTURL[1]), "N/A", 
                    ifelse(grepl("^https?://", add2$INSTURL[1]), add2$INSTURL[1], paste0("https://", add2$INSTURL[1])))
      
      summary_data <- add_summary_row(summary_data, "Website",
        ifelse(url1 == "N/A", "N/A", paste0('<a href="', url1, '" target="_blank">Visit Website</a>')),
        ifelse(url2 == "N/A", "N/A", paste0('<a href="', url2, '" target="_blank">Visit Website</a>'))
      )
    }
    
    # Add View Demographics buttons (always show at the bottom)
    summary_data <- add_summary_row(summary_data, "View Demographics",
      paste0('<button class="btn btn-primary btn-sm action-button" id="view_', 
             input$college1, 
             '" onclick="Shiny.setInputValue(\'go_to_race_from_compare\', \'', 
             input$college1, 
             '\', {priority: \'event\'})">View Gender & Racial Composition</button>'),
      paste0('<button class="btn btn-primary btn-sm action-button" id="view_', 
             input$college2, 
             '" onclick="Shiny.setInputValue(\'go_to_race_from_compare\', \'', 
             input$college2, 
             '\', {priority: \'event\'})">View Gender & Racial Composition</button>')
    )
    

    

    
    # Clean up NA values
    summary_data[is.na(summary_data)] <- "N/A"
    
    datatable(summary_data, 
              options = list(
                pageLength = 50,
                dom = 't',
                searching = FALSE,
                lengthChange = FALSE
              ),
              rownames = FALSE,
              colnames = c("Metric", input$college1, input$college2),
              escape = FALSE)
  })
}

shinyApp(ui, server)