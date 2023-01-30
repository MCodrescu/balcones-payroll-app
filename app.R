# UI ---------------------------------------------------------------------------
ui <- shiny::bootstrapPage(
  theme = bslib::bs_theme(version = 5),

  # Initiate shinyjs
  shinyjs::useShinyjs(),

  ## Navbar ---------------------------------------------------------------------
  shiny::tags$div(
    class = "container-fluid pt-2",
    shiny::tags$nav(
      class = "navbar navbar-expand-sm navbar-light bg-light border rounded shadow-sm",
      shiny::div(
        class = "container-fluid",
        shiny::tags$a(
          class = "navbar-brand mb-1",
          "Balcones"
        ),
        shiny::tags$button(
          class = "navbar-toggler",
          type = "button",
          "data-bs-toggle" = "collapse",
          "data-bs-target" = "#navbarSupportedContent",
          shiny::tags$span(class = "navbar-toggler-icon")
        ),
        shiny::div(
          class = "collapse navbar-collapse",
          id = "navbarSupportedContent",
          shiny::tags$ul(
            class = "navbar-nav me-auto mb-2 mb-sm-0",
            shiny::tags$li(
              class = "nav-item",
              shiny::tags$a(
                class = "nav-link",
                id = "payrollNav",
                role = "button",
                "data-bs-toggle" = "dropdown",
                "Payroll"
              ),
            ),

            shiny::tags$li(
              class = "nav-item",
              shiny::tags$a(
                class = "nav-link",
                id = "reportsNav",
                role = "button",
                "data-bs-toggle" = "dropdown",
                "Reports"
              )
            ),

            shiny::tags$li(
              class = "nav-item",
              shiny::tags$a(
                class = "nav-link",
                shiny::tags$div(
                  class = "form-check form-switch",
                  shiny::tags$input(
                    class = "form-check-input",
                    type = "checkbox",
                    role = "switch",
                    id = "devSwitch",
                    "checked" = ""
                  ),
                  shiny::tags$label(
                    class = "form-check-label",
                    id = "devProdSwitchLabel",
                    "for" = "devSwitch",
                    "Prod"
                  )
                )
              )
            ),

          )
        )
      )
    )
  ),

  ## Main Container -------------------------------------------------------------

  shiny::div(
    class = "container py-3",
    shiny::div(
      class = "row justify-content-center",

      ### Payroll Div ------------------------------------------------------------
      shiny::div(
        class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 border rounded shadow",
        id = "payrollDiv",

        shiny::tags$h3(
          class = "text-center pb-3",
          "Run Payroll"
        ),

        shiny::tags$div(
          class = "input-group pb-3",
          shiny::tags$span(
            class = "input-group-text",
            "Start Date"
          ),
          bootstrapDateInput(
            class = "form-control",
            inputId = "payrollStartDate",
            value = Sys.Date()
          ),
          shiny::tags$button(
            id = "runPayrollButton",
            type = "button",
            class = "btn btn-outline-primary",
            "Run Payroll"
          ),

        ),

        shiny::tags$hr(),

        shiny::tags$h3(
          class = "text-center",
          "Upload File"
        ),

        # File upload to database
        shiny::fileInput(
          "newTableUpload",
          "Upload Timesheet",
          accept = c(".xlsx"),
          multiple = TRUE,
          width = "100%"
        ),

      ),

      ### Reports Div ------------------------------------------------------------
      shiny::div(
        class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 border rounded shadow",
        id = "reportsDiv",
        style = "display: none;",

        shiny::tags$h3(
          class = "text-center",
          "Reports"
        ),

        shiny::selectInput(
          "reportSelect",
          label = "Select a Report",
          width = "100%",
          choices = NULL
        ),

        shiny::hr(),

        #### Job Summary Report -------------------------------------------------
        shiny::div(
          id = "jobSummaryReportDiv",
          class = "pt-1",

          shiny::tags$h4(
            class = "text-center",
            "Job Summary Report"
          ),

          shiny::selectInput(
            "jobSummaryReportJobSelect",
            label = "Job Number",
            width = "100%",
            choices = NULL
          ),

          shiny::dateRangeInput(
            "jobSummaryReportDateRange",
            label = "Date Range",
            width = "100%"
          ),

          shiny::tags$button(
            id = "jobSummaryReportButton",
            class = "btn btn-outline-primary w-100 my-3",
            "Run Report"
          )
        ),

        #### Employee Summary Report -------------------------------------------

        shiny::div(
          id = "employeeSummaryReportDiv",
          style = "display: none;",
          class = "pt-1",

          shiny::tags$h4(
            class = "text-center",
            "Employee Summary Report"
          ),

          shiny::selectInput(
            "employeeSummaryReportEmployeeSelect",
            label = "Employee",
            width = "100%",
            choices = NULL
          ),

          shiny::dateRangeInput(
            "employeeSummaryReportDateRange",
            label = "Date Range",
            width = "100%"
          ),

          shiny::tags$button(
            id = "employeeSummaryReportButton",
            class = "btn btn-outline-primary w-100 my-3",
            "Run Report"
          )
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # Navigation -----------------------------------------------------------------
  shinyjs::onclick("payrollNav", {
    shinyjs::hideElement("reportsDiv")
    shinyjs::hideElement("uploadDiv")
    shinyjs::showElement("payrollDiv")
  })

  shinyjs::onclick("reportsNav", {
    shinyjs::hideElement("payrollDiv")
    shinyjs::hideElement("uploadDiv")
    shinyjs::showElement("reportsDiv")

    all_job_numbers <-
      DBI::dbGetQuery(
        pool,
        "SELECT DISTINCT job_number FROM employee_time"
      ) |>
      dplyr::pull(1)

    all_employees <-
      DBI::dbGetQuery(
        pool,
        "SELECT DISTINCT employee FROM employee_time"
      ) |>
      dplyr::pull(1)

    shiny::updateSelectInput(
      session,
      "jobSummaryReportJobSelect",
      choices = all_job_numbers
    )

    shiny::updateSelectInput(
      session = session,
      inputId = "employeeSummaryReportEmployeeSelect",
      choices = all_employees
    )
  })

  devSwitchClicked <- FALSE
  devSwitchLabel <- c("Dev", "Prod")
  databaseName <- c("job_register_dev", "job_register")

  shinyjs::onclick("devSwitch",{
      devSwitchClicked <- !devSwitchClicked
      shinyjs::html(
        "devProdSwitchLabel",
        devSwitchLabel[c(devSwitchClicked, !devSwitchClicked)]
      )
      con <- pool::poolCheckout(pool)
      DBI::dbSendQuery(
        con,
        glue::glue(
          "USE {databaseName[c(devSwitchClicked, !devSwitchClicked)]}"
        )
      )
      pool::poolReturn(con)
  })

  all_reports <- list(
      "Job Summary" = "jobSummaryReportDiv",
      "Employee Summary" = "employeeSummaryReportDiv"
  )

  shiny::updateSelectInput(
    session = session,
    inputId = "reportSelect",
    choices = all_reports
  )

  shinyjs::onevent("change", "reportSelect", {
    report_to_show <- all_reports[all_reports == input$reportSelect]
    reports_to_hide <- all_reports[all_reports != input$reportSelect]

    shinyjs::showElement(report_to_show[[1]])
    purrr::walk(
      reports_to_hide,
      shinyjs::hideElement
    )
  })

  # Database Connection ---------------------------------------------------

  keyset <- tryCatch({
    keyring::key_get("BalconesDBHost")
    TRUE
  }, error = function(error){
    FALSE
  })

  if (keyset){

    tryCatch({
      shiny::showNotification("Connecting ...")

      pool <-
        pool::dbPool(
          RMySQL::MySQL(),
          host = keyring::key_get("BalconesDBHost"),
          port = 3306,
          username = keyring::key_get("BalconesDBUsername"),
          password = keyring::key_get("BalconesDBPassword"),
          dbname = "job_register_dev"
        )

      shiny::showNotification("Connected")

    }, error = function(error){
      shiny::showNotification(error$message)
    })

  } else {
    shiny::showModal(
      shiny::modalDialog(
        size = "m",
        shiny::h3("Setup Database Connection"),
        shiny::p("Please enter database connection credentials."),
        shiny::textInput(
          "balconesDBHost",
          "Host",
          width = "100%"
        ),
        shiny::textInput(
          "balconesDBUsername",
          "Username",
          width = "100%"
        ),
        shiny::textInput(
          "balconesDBPassword",
          "Password",
          width = "100%"
        ),
        footer = shiny::tagList(
          shiny::tags$button(
            class = "btn btn-outline-secondary",
            "data-bs-dismiss" = "modal",
            id = "submitDBCredentials",
            "Confirm"
          )
        )
      )
    )

    shinyjs::onclick(
      "submitDBCredentials",
      {
        keyring::key_set_with_value(
          "BalconesDBHost",
          password = input$balconesDBHost
        )

        keyring::key_set_with_value(
          "BalconesDBUsername",
          password = input$balconesDBUsername
        )

        keyring::key_set_with_value(
          "BalconesDBPassword",
          password = input$balconesDBPassword
        )

        shiny::showNotification("Connecting ...")

        pool <-
          pool::dbPool(
            RMySQL::MySQL(),
            host = keyring::key_get("BalconesDBHost"),
            port = 3306,
            username = keyring::key_get("BalconesDBUsername"),
            password = keyring::key_get("BalconesDBPassword"),
            dbname = "job_register_dev"
          )

        shiny::showNotification("Connected")
      }
    )
  }



  # Payroll --------------------------------------------------------------------
  shinyjs::onclick("runPayrollButton", {

    payroll_dates <-
      get_payroll_start_end_dates(
        input$payrollStartDate
      )

    time_data <-
      get_payroll_hours(
        payroll_dates$payroll_start,
        payroll_dates$payroll_end,
        pool
      )

    shiny::showModal(
      shiny::modalDialog(
        easyClose = TRUE,
        size = "xl",
        shiny::h3("Payroll Hours"),
        shiny::p(
          glue::glue(
            "{format(as.Date(payroll_dates$payroll_start), \"%B %d, %Y\")} to {format(as.Date(payroll_dates$payroll_end), \"%B %d, %Y\")}"
          )
        ),
        shiny::div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          DT::renderDataTable(
            options = list(
              dom = "t",
              paging = FALSE,
              ordering = FALSE
            ),
            server = TRUE,
            rownames = FALSE,
            {
              time_data
            }
          )
        ),
        footer = shiny::tagList(
          shiny::tags$button(
            class = "btn btn-outline-secondary",
            id = "downloadTimeData",
            "Download"
          ),
          shiny::modalButton("Dismiss")
        )
      )
    )


  })

  # Upload Time ----------------------------------------------------------------

  shiny::observeEvent(input$newTableUpload, {

    # Read file
    timesheet_file <- input$newTableUpload
    shiny::req(timesheet_file)

    timesheet_hours <-
      do.call(
        dplyr::bind_rows,
        list(
          purrr::map(
            timesheet_file$datapath,
            get_timesheet_hours
          )
        )
    )

    total_hours <-
      sum(
        timesheet_hours$hours_worked
      )

    shiny::showModal(
      shiny::modalDialog(
        easyClose = TRUE,
        size = "xl",
        shiny::h3("Timesheet Upload"),
        shiny::p(
          glue::glue("Total Hours: {total_hours}")
        ),
        shiny::div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          DT::renderDataTable(
            options = list(
              dom = "t",
              paging = FALSE,
              ordering = FALSE
            ),
            server = TRUE,
            rownames = FALSE,
            {
              timesheet_hours
            }
          )
        ),
        footer = shiny::tagList(
          shiny::tags$button(
            class = "btn btn-outline-secondary",
            id = "uploadTimeData",
            "data-bs-dismiss" = "modal",
            "Upload"
          ),
          shiny::modalButton("Dismiss")
        )
      )
    )


    shinyjs::onclick(
      "uploadTimeData",
      {
        if(devSwitchClicked){
          shiny::showNotification("Uploading...")

          con <- pool::poolCheckout(pool)

          upload_time(
            timesheet_hours,
            con
          )

          pool::poolReturn(con)

          shiny::showNotification("Success!")

        } else {
          shiny::showNotification("Uploading...")

          upload_time(
            timesheet_hours,
            dev_mode = TRUE
          )

          shiny::showNotification("Success!")

        }
      }
    )

  })

  # Reports --------------------------------------------------------------------

  ## Job Summary Report --------------------------------------------------------
  shinyjs::onclick(
    "jobSummaryReportButton",
    {
      job_number <- input$jobSummaryReportJobSelect
      start_date <- input$jobSummaryReportDateRange[1]
      end_date <- input$jobSummaryReportDateRange[2]

      report <-
        DBI::dbGetQuery(
          pool,
          glue::glue(
            "
            SELECT DISTINCT
              employee,
              job_number,
              work_date,
              hours_worked
            FROM employee_time
            WHERE job_number = '{job_number}'
            AND work_date >= '{start_date}'
            AND work_date <= '{end_date}'
            "
          )
        )

      output$downloadJobSummaryReport <-
        shiny::downloadHandler(
          filename = function(){
            glue::glue(
              "job_summary_report_{format(Sys.Date(), \"%Y%m%d\")}.csv"
            )
          },
          content = function(file) {
            write.csv(report, file, row.names = FALSE)
          }
        )

      shiny::showModal(
        shiny::modalDialog(
          easyClose = TRUE,
          size = "xl",
          shiny::h3("Job Summary Report"),
          shiny::p(
            glue::glue(
              "{job_number} | {format(as.Date(start_date), \"%B %d, %Y\")} to {format(as.Date(end_date), \"%B %d, %Y\")}"
            )
          ),
          shiny::div(
            class = "table-responsive",
            style = "max-height: 70vh;",
            DT::renderDataTable(
              options = list(
                dom = "t",
                paging = FALSE,
                ordering = FALSE
              ),
              server = TRUE,
              rownames = FALSE,
              {
                report
              }
            )
          ),
          footer = shiny::tagList(
            shiny::downloadButton(
              "downloadJobSummaryReport",
              "Download"
            ),
            shiny::modalButton("Dismiss")
          )
        )
      )

    }
  )

  ## Employee Summary Report ----------------------------------------------
  shinyjs::onclick(
    "employeeSummaryReportButton",
    {
      employee <- input$employeeSummaryReportEmployeeSelect
      start_date <- input$employeeSummaryReportDateRange[1]
      end_date <- input$employeeSummaryReportDateRange[2]

      report <-
        DBI::dbGetQuery(
          pool,
          glue::glue(
            "
            SELECT DISTINCT
              employee,
              job_number,
              work_date,
              hours_worked
            FROM employee_time
            WHERE employee = '{employee}'
            AND work_date >= '{start_date}'
            AND work_date <= '{end_date}'
            "
          )
        )

      output$downloadEmployeeSummaryReport <-
        shiny::downloadHandler(
          filename = function(){
            glue::glue(
              "employee_summary_report_{format(Sys.Date(), \"%Y%m%d\")}.csv"
            )
          },
          content = function(file) {
            write.csv(report, file, row.names = FALSE)
          }
        )

      shiny::showModal(
        shiny::modalDialog(
          easyClose = TRUE,
          size = "xl",
          shiny::h3("Employee Summary Report"),
          shiny::p(
            glue::glue(
              "{employee} | {format(as.Date(start_date), \"%B %d, %Y\")} to {format(as.Date(end_date), \"%B %d, %Y\")}"
            )
          ),
          shiny::div(
            class = "table-responsive",
            style = "max-height: 70vh;",
            DT::renderDataTable(
              options = list(
                dom = "t",
                paging = FALSE,
                ordering = FALSE
              ),
              server = TRUE,
              rownames = FALSE,
              {
                report
              }
            )
          ),
          footer = shiny::tagList(
            shiny::downloadButton(
              "downloadEmployeeSummaryReport",
              "Download"
            ),
            shiny::modalButton("Dismiss")
          )
        )
      )

    }
  )


  # Shiny On Close -------------------------------------------------------------
  shiny::onSessionEnded(
    function(){
      try(pool::poolClose(pool))
      shiny::stopApp()
    }

  )
}

shinyApp(ui = ui, server = server)
