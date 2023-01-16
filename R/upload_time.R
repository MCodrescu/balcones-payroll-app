#' Upload Time
#' @description This function connects to the MySQL instance and uploads the
#'   timesheet data.
#'
#' @param timesheet A tidy data frame of employee time. The result of
#'   get_timesheet_hours()
#' @param dev_mode A logical. Should the upload be to a temporary table for
#'   development purposes only.
#'
#' @return Nothing is returned.
#' @export
#'
upload_time <-
  function(
    timesheet,
    dev_mode = TRUE
  ) {

    con <-
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = '166.62.27.56',
        port = 3306,
        username = 'MCodrescu',
        password = "Blackcar1997!",
        dbname = "job_register"
      )

    if (dev_mode){
      table_name <- "employee_time_dev"
      DBI::dbSendQuery(
        con,
        "
        CREATE TEMPORARY TABLE employee_time_dev (
            time_entry_id INT NOT NULL AUTO_INCREMENT,
            employee VARCHAR(255) NOT NULL,
            job_number VARCHAR(255) NOT NULL,
            work_date DATE NOT NULL,
            hours_worked FLOAT NOT NULL,
            PRIMARY KEY (time_entry_id)
        );
        "
      )

    } else {
      table_name <- "employee_time"
    }

    timesheet |>
      purrr::pwalk(
        function(
          employee,
          job_number,
          work_date,
          hours_worked
        ){
          DBI::dbSendQuery(
            con,
            glue::glue(
              "
              INSERT INTO {table_name} (employee, job_number, work_date, hours_worked)
              VALUES ('{employee}', '{job_number}', '{work_date}', {hours_worked})
              "
            )
          )
        }
      )

    if (dev_mode){
      result <- DBI::dbGetQuery(
        con,
        "
        SELECT * FROM employee_time_dev
        "
      )
      print(result)
    }

    dbDisconnect(con)

    invisible()
}
