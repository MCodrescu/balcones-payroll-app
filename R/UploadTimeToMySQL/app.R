library(shiny)
library(readxl)
library(dplyr)
library(RMySQL)
library(lubridate)
library(glue)

# Import custom functions
source("To_Knack_Transform.R")
source("Upload_To_MySQL.R")

ui <- fluidPage(fluidRow(column(
  12,
  align = "center",
  h3("Timesheets"),
  fileInput("file", "Upload Timesheet(s)", multiple = TRUE),
  uiOutput("description")
)),

fluidRow(column(
  12,
  align = "center",
  uiOutput("warning"),
  tableOutput("data1"),
  uiOutput("button"),
  uiOutput("comment")
)),)

server <- function(input, output, session) {
  # Employee List
  employee_list <- c("Marcus Codrescu",
                     "Becca Wall",
                     "Jim Glaze",
                     "John Wooley",
                     "Austin Wooley",
                     "Andrew Domke",
                     "Kerri Doughty",
                     "David Mason",
                     "Rebecca Russo",
                     "Nick Nuzzolo",
                     "Andrew Brown",
                     "Russell Sieg",
                     "Ruth Tobin",
                     "Debra Wooley")
  
  # Transforms the timesheet into the right format for knack.
  # See the documentation for function To_Knack_Transform()
  path <- reactive({
    req(input$file$datapath)
  })
  
  timesheet <- reactive({
    To_Knack_Transform(path())
  })
  
  
  # Shows a preview of the transformed timesheet with hours total
  output$data1 <- renderTable(na = " ", {
    total_hours <- sum(as.numeric(timesheet()$hours_worked))
    timesheet() %>%
      rbind(c(NA, "Total", total_hours, NA))
  })
  
  # Hides the download button until a file is submitted
  output$button <- renderUI({
    if (is.character(input$file$name)) {
      actionButton("upload", "Upload")
    }
  })
  
  #Prints text once the file is downloaded
  output$description <- renderUI({
    if (is.character(input$file$name)) {
      h4("Does the below time look correct? If so, click upload.")
    }
  })
  
  # Checks for errors
  output$warning <- renderUI({
    if (is.character(input$file$name)){
      # Check if employee is in list otherwise guess
      employee <- timesheet()$account[1]
      if(!(employee %in% employee_list)){
        tibble(employee_list = employee_list,
               closeness = t(adist(employee, employee_list))) %>% 
          arrange(closeness) %>%
          slice(1) %>%
          pull(employee_list) ->
          employee_guess
        h4(glue("'{employee}' is not correct. Did you mean '{employee_guess}'?"), style = "color: red;")
      }
    }
  })
  
  observeEvent(input$upload, {
      # Uploads timesheets
      upload_to_mysql(timesheet())
      output$comment <- renderUI(h4("Done!"))
  })
  
}

shinyApp(ui, server)