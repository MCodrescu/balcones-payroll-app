library(shiny)
library(lubridate)
library(dplyr)
library(forcats)
library(RMySQL)
library(pool)
library(glue)
library(tidyr)
library(DT)
library(readxl)


# Define UI for application that draws a histogram
ui <- navbarPage(collapsible = TRUE,
  "Balcones Application",
  #######################################
  tabPanel("Run Payroll",
           fluidPage(fluidRow(
             column(12, align = "center", h3("Run Payroll"))
           ),

           fluidRow(
             column(
               align = "center",
               12,
               actionButton("button", "Run Payroll"),
               br(),
               br(),
               tableOutput("payroll"),
               uiOutput("download"),
               br(),
               uiOutput("months")
             )
           ))),
  ############################################
  tabPanel("View Data",
           fluidPage(fluidRow(
             column(
               12,
               align = "center",
               h3("View All Data"),
               actionButton("showTable", "Show Table"),
               uiOutput("table")
             )
           ))),
  ############################################
  tabPanel("Upload Time", fluidPage(fluidRow(
    column(
      12,
      align = "center",
      h3("Timesheets"),
      fileInput("file", "Upload Timesheet(s)", multiple = TRUE),
      uiOutput("description"),
      uiOutput("button"),
      uiOutput("comment")
    )
  ),

  fluidRow(
    column(12,
           align = "center",
           uiOutput("warning"),
           dataTableOutput("data1"), )
  ), )),

  ###########################################
  tabPanel("Clean",
           fluidPage(fluidRow(
             column(
               12,
               align = "center",
               h3("Clean Data"),
               actionButton("cleanData", "Clean"),
               br(),
               uiOutput("DBmessage")
             )
           )))
)


server <- function(input, output) {
  # Connect to DB
  con <- dbPool(
    MySQL(),
    host = "166.62.27.56",
    username = "MCodrescu",
    password = "Blackcar1997!",
    dbname = "job_register",
    port = 3306
  )
  # Determine payroll start and end dates
  n_day <- mday(Sys.Date())
  n_month <- month(Sys.Date())
  n_year <- year(Sys.Date())
  n_days_last_month <- ifelse(
    n_month - 1 > 0,
    days_in_month(n_month - 1),
    days_in_month(12)
  )

  if (n_day > 15) {
    payroll_start = glue("{n_year}-{n_month}-1")
    payroll_end = glue("{n_year}-{n_month}-15")
  } else {
    payroll_start = glue("{n_year}-{n_month-1}-16")
    payroll_end = glue("{n_year}-{n_month-1}-{n_days_last_month}")
  }

  observeEvent(input$button, {

    # Pulls data from DB and totals hours
    dbGetQuery(
      con,
      glue(
        "SELECT * FROM employee_time
    WHERE work_date >= '{payroll_start}'
    AND work_date <= '{payroll_end}'"
      )
    ) %>%

      # Select necessary columns
      select(employee, job_number, hours_worked, work_date) %>%

      # Change character to factor
      mutate(job_number = factor(job_number)) %>%

      # Group into categories
      mutate(job_number = fct_collapse(
        job_number,
        VHS = c("Vacation", "Holiday", "Sick"),
        Indirect = c("Gen/Admin", "Bidding Proposals"),
        other_level = "Direct"
      )) %>%

      # Calculates total hours by category
      group_by(employee, job_number) %>%
      summarize(total = sum(as.numeric(hours_worked))) %>%

      # Pivots the table
      pivot_wider(names_from = job_number, values_from = total) %>%

      replace_na(list(
        Indirect = 0,
        Direct = 0,
        VHS = 0
      )) %>%

      # Add a total column
      mutate(Total = sum(across(.cols = !contains("employee")))) ->

      time_data

    # Display the table
    output$payroll <-
      renderTable(striped = TRUE,
                  hover = TRUE,
                  na = "0",
                  {
                    as.data.frame(time_data)
                  })

    output$months <- renderText({
      glue("Payroll from after {payroll_start} to before {payroll_end}")
    })

    output$download <- renderUI({
      # Show the download button
      downloadButton("downloadData", "Download")

      # Allows the option to download
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("Payroll", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(as.data.frame(time_data), file)
        }
      )

    })
  })

  #######################################

  # Listens to button click
  observeEvent(input$showTable, {
    output$table <- renderUI({
      # Puts the table
      dataTableOutput("timeDataTable")

      # Show a data table
      output$timeDataTable <- renderDataTable({

        # Pulls data from DB and totals hours
        dbGetQuery(
          con,
          glue(
            "SELECT * FROM employee_time
    WHERE work_date >= '{payroll_start}'
    AND work_date <= '{payroll_end}'"
          )
        ) %>%
          select(employee, job_number, work_date, hours_worked) %>%
          rename(
            Employee = employee,
            `Job Number` = job_number,
            Date = work_date,
            Hours = hours_worked
          )
      })
    })
  })

  ########################################

  # Employee List
  employee_list <- c(
    "Marcus Codrescu",
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
    "Debra Wooley"
  )

  # Transforms the timesheet into the right format for knack.
  # See the documentation for function To_Knack_Transform()
  path <- reactive({
    req(input$file$datapath)
  })

  timesheet <- reactive({
    To_Knack_Transform(path())
  })


  # Shows a preview of the transformed timesheet with hours total
  output$data1 <- renderDataTable(options = list(pageLength = 8), {
    total_hours <- sum(as.numeric(timesheet()$hours_worked))
    timesheet() %>%
      rbind(c(NA, "Total", total_hours, NA))
  })

  # Hides the download button until a file is submitted
  output$button <- renderUI({
    if (is.character(input$file$name)) {
      output$comment <- NULL
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
    if (is.character(input$file$name)) {
      # Check if employee is in list otherwise guess
      employee <- timesheet()$account[1]
      if (!(employee %in% employee_list)) {
        tibble(employee_list = employee_list,
               closeness = t(adist(employee, employee_list))) %>%
          arrange(closeness) %>%
          slice(1) %>%
          pull(employee_list) ->
          employee_guess
        h4(glue(
          "'{employee}' is not correct. Did you mean '{employee_guess}'?"
        ),
        style = "color: red;")
      }
      if (any(is.na(timesheet()$job_number))) {
        h4("Some job numbers are blank",
           style = "color: red;")
      }
    }
  })

  observeEvent(input$upload, {
    # Uploads timesheets
    upload_to_mysql(timesheet())
    output$comment <- renderUI(h4("Done!"))
  })

  ##################################################

  observeEvent(input$cleanData, {
    output$DBmessage <- renderUI({

      con <- poolCheckout(con)

      # Find duplicates
      dbSendQuery(
        con,
        glue(
          '
      CREATE OR REPLACE VIEW duplicated_ids AS (
        SELECT
            MAX(time_entry_id) AS time_entry_id
        FROM
            employee_time
        GROUP BY
            employee,
            job_number,
            work_date,
            hours_worked
        HAVING
            COUNT(DISTINCT(time_entry_id)) > 1)'
        )
      )
      # Delete duplicates
      result <- dbSendQuery(
        con,
        glue(
          '
      DELETE FROM employee_time
      WHERE time_entry_id IN (
        SELECT time_entry_id
        FROM duplicated_ids)
'
        )
      )
      poolReturn(con)
      h6(glue("{dbGetRowsAffected(result)} rows affected"))
    })
  })



}

# Run the application
#browseURL("http://localhost:3838")
#shinyApp(ui = ui, server = server, options = list(port = 3838))
