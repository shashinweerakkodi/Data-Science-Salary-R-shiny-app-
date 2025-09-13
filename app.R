#install.packages("leaflet")


# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)

# Load dataset
data <- read.csv("G:/3rd year/2nd SEM/ST 3011/projects_Rshiny/s16375/Data Science Salary 2021 to 2023.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Science Salary Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Salary Overview", tabName = "overview", icon = icon("bar-chart")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("globe")),
      menuItem("Company Insights", tabName = "company", icon = icon("building"))
    ),
    
    selectInput("experience_level", "Experience Level:",
                choices = c("All", unique(data$experience_level)), selected = "All"),
    
    selectInput("employment_type", "Employment Type:",
                choices = c("All", unique(data$employment_type)), selected = "All"),
    
    selectInput("company_size", "Company Size:",
                choices = c("All", unique(data$company_size)), selected = "All"),
    
    selectInput("company_location", "Select Country:", 
            choices = unique(data$company_location), 
            selected = unique(data$company_location)[1]), 
    
    textInput("job_title", "Job Title (leave empty for all):", ""),
    
    actionButton("apply_filters", "Apply Filters")
  ),
  
  dashboardBody(
    tabItems(
      # Salary Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                
                
                column(width = 6,
                       box(title = "About the Dataset", status = "warning", solidHeader = TRUE,
                           width = 12,  
                           uiOutput("dataset_description")),
                       
                       box(title = "Variable Levels Explained", status = "info", solidHeader = TRUE,
                           width = 12, 
                           uiOutput("variable_levels"))
                ),
                
                box(title = "Salary Distribution by Experience Level", status = "primary", solidHeader = TRUE,
                    plotOutput("salary_boxplot", height = 350)),

                box(title = "Salary Summary", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("salary_summary"))
              )),
      
      # Geographic Analysis Tab
      tabItem(tabName = "geographic",
              fluidRow(
                box(title = "Average Salary Trends by Country", status = "primary", solidHeader = TRUE,
                    plotOutput("salary_plot", height = 500)),
                box(title = "Average Salary by Company Location", status = "info", solidHeader = TRUE,
                    plotOutput("location_barplot", height = 500))
              )),
      
      # Company Insights Tab
      tabItem(tabName = "company",
              fluidRow(
                box(title = "Top Hiring Companies", status = "primary", solidHeader = TRUE,
                    dataTableOutput("top_companies")),
                box(title = "Average Salary by Company Size", status = "info", solidHeader = TRUE,
                    plotOutput("company_size_plot", height = 400))
              ))
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive dataset based on filters
  filtered_data <- reactive({
    df <- data
    if (input$experience_level != "All") {
      df <- df %>% filter(experience_level == input$experience_level)
    }
    if (input$employment_type != "All") {
      df <- df %>% filter(employment_type == input$employment_type)
    }
    if (input$company_size != "All") {
      df <- df %>% filter(company_size == input$company_size)
    }
    if (!is.null(input$company_location) && length(input$company_location) > 0) {
      df <- df %>% filter(company_location %in% input$company_location)
    }
    if (input$job_title != "") {
      df <- df %>% filter(grepl(input$job_title, job_title, ignore.case = TRUE))
    }
    return(df)
  })

  output$dataset_description <- renderUI({
    HTML("<p>This dashboard shows salary information for jobs in the Data Science field from 2020 to 2023. 
          It includes details about experience levels, company locations, employment types, 
          and salaries in USD. Our goal is to analyze salary trends and patterns.</p>")
  })
  
  # Variable Levels Explanation
  output$variable_levels <- renderUI({
    HTML("<p><b>Experience Level:</b>  
          <ul>
            <li><b>EN</b> - Entry-Level/ Junior </li>
            <li><b>MI</b> - Mid-Level</li>
            <li><b>SE</b> - Senior-Level</li>
            <li><b>EX</b> - Executive-Level</li>
          </ul>
          <b>Company Size:</b>  
          <ul>
            <li><b>S</b> - Small </li>
            <li><b>M</b> - Medium </li>
            <li><b>L</b> - Large </li>
          </ul>
          <b>Employment Type:</b>  
          <ul>
            <li><b>FT</b> - Full time</li>
            <li><b>PT</b> - Part time</li>
            <li><b>CT</b> - Contract</li>
            <li><b>FL</b> - Freelance</li>
          </ul>
          </p>")
  })
  

  # Salary Distribution Boxplot
  output$salary_boxplot <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
      geom_boxplot() +
      labs(title = "Salary Distribution by Experience Level",
           x = "Experience Level", y = "Salary in USD") +
      theme_minimal()
  })
  
  # Salary Summary Statistics
  output$salary_summary <- renderPrint({
    df <- filtered_data()
    summary(df$salary_in_usd)
  })
  
  # Geographic Analysis - Salary trend by Country
  output$salary_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(company_location == input$company_location) %>%  # Filter by selected country
      group_by(work_year) %>%
      summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) 
    
    ggplot(df, aes(x = work_year, y = avg_salary, group = 1)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = paste("Salary Trend in", input$company_location),
           x = "Year",
           y = "Average Salary (USD)") +
      theme_minimal()
  })
  
  # Salary by country
  output$location_barplot <- renderPlot({

    top_countries <- data %>%
      group_by(company_location) %>%
      summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(avg_salary)) %>%
      head(10)

    ggplot(top_countries, aes(x = reorder(company_location, avg_salary), y = avg_salary, fill = company_location)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 10 Salaries by Country", x = "Country", y = "Average Salary (USD)") +
      theme_minimal()
  })
  
  
  # Top Hiring Companies (Table)
  output$top_companies <- renderDataTable({
    df <- filtered_data() %>%
      group_by(job_title, company_location) %>%
      summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_salary)) %>%
      head(10)
    
    datatable(df, options = list(pageLength = 5))
  })
  
  # Salary by Company Size (Bar Chart)
  output$company_size_plot <- renderPlot({
    df <- filtered_data() %>%
      group_by(company_size) %>%
      summarise(avg_salary = mean(salary_in_usd, na.rm = TRUE))
    
    ggplot(df, aes(x = company_size, y = avg_salary, fill = company_size)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Salary by Company Size", x = "Company Size", y = "Average Salary (USD)") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)


