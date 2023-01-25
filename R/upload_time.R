#' Upload Time
#' @description This function connects to the MySQL instance and uploads the
#'   timesheet data.
#'
#' @param timesheet A tidy data frame of employee time. The result of
#'   get_timesheet_hours()
#' @param con A database connection object.
#'
#' @importFrom purrr pwalk
#' @importFrom DBI dbSendQuery
#' @importFrom glue glue
#'
#' @return Nothing is returned.
#' @export
#'
upload_time <-
  function(
    timesheet,
    con
  ) {

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
              INSERT INTO employee_time (employee, job_number, work_date, hours_worked)
              VALUES ('{employee}', '{job_number}', '{work_date}', {hours_worked})
              "
            )
          )
        }
      )

}
