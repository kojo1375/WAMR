library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)

# Read the data
amr_data <- read_excel("amr_data.xlsx", sheet = "individual CTAs")
View(amr_data)
data_plot2 <- read_excel("amr_data.xlsx", sheet = "Time Series")
View(data_plot2)

# Convert Year to date
amr_data$Year = as.Date(paste0(amr_data$Year, "-01-01"))
data_plot2$Year = as.Date(paste0(data_plot2$Year, "-01-01"))


# UI
ui = fluidPage(
  titlePanel("AMR Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:",
                  choices = c("All", unique(amr_data$Region))),
      uiOutput("syndrome"),
      uiOutput("pathogen"),
      uiOutput("antibiotic"),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      plotlyOutput("amr_plot"),
      plotlyOutput("amr_plot2")
    )
  )
)


# Server logic
server = function(input, output, session) {
  
  
  output$syndrome = renderUI({
    choices = c("All", unique(amr_data$Infectious_Syndrome))
    selectInput("syndrome", "Select Infectious Syndrome:", choices = choices, selected = "All")
  })
  
  
  output$pathogen = renderUI({
    choices = if (input$syndrome == "All") {
      c("All", unique(amr_data$Bacterial_Pathogen))
    } else {
      c("All", unique(amr_data$Bacterial_Pathogen[amr_data$Infectious_Syndrome == input$syndrome]))
    }
    selectInput("pathogen", "Select Bacterial Pathogen:", choices = choices, selected = "All")
  })
  
  
  output$antibiotic = renderUI({
    choices = if (input$pathogen == "All") {
      c("All", unique(amr_data$Antibiotic_Type))
    } else {
      c("All", unique(amr_data$Antibiotic_Type[amr_data$Bacterial_Pathogen == input$pathogen]))
    }
    selectInput("antibiotic", "Select Antibiotic Type:", choices = choices, selected = "All")
  })
  
  
  filtered_data = reactive({
    data = amr_data
    if (input$region != "All") {
      data = filter(data, Region == input$region)
    }
    if (input$syndrome != "All") {
      data = filter(data, Infectious_Syndrome == input$syndrome)
    }
    if (input$pathogen != "All") {
      data = filter(data, Bacterial_Pathogen == input$pathogen)
    }
    if (input$antibiotic != "All") {
      data = filter(data, Antibiotic_Type == input$antibiotic)
    }
    data
  })
  
  
  filtered_data_plot2 = reactive({
    data_plot2 %>%
      filter(Infectious_Syndrome == input$syndrome,
             Bacterial_Pathogen == input$pathogen,
             Antibiotic_Type == input$antibiotic)
  })
  
  
  observeEvent(input$submit, {
    output$amr_plot = renderPlotly({
      p = ggplot(filtered_data(), aes(x = Year, y = Percent_AMR, group = Region, color = Region, text = paste(Region, ": ", round(Percent_AMR, 2), "%", sep = ""))) +
        geom_line(size = 1) +
        labs(x = "Year", y = "Percentage (%)", title = "Countries, territories and areas (CTAs)") +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0, 100)
      
      ggplotly(p, tooltip = "text")
    })
    
    output$amr_plot2 = renderPlotly({
      p = ggplot(filtered_data_plot2(), aes(x = Year, y = Median, group = 1, text = paste("Median: ", round(Median, 2), "%\nQ1: ", round(Q1, 2), "%\nQ3: ", round(Q3, 2), "%", sep = ""))) +
        geom_line(size = 1) +
        geom_ribbon(aes(ymin = Q1, ymax = Q3), alpha = 0.3) +
        labs(x = "Year", y = "Percentage (%)", title = "Median") +
        theme_minimal() +
        theme(legend.position = "none") +
        ylim(0, 100)
      
      ggplotly(p, tooltip = "text")
    })
  })
}


# Run 
shinyApp(ui = ui, server = server)
