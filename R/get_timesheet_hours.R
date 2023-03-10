#' Get Timesheet Hours
#' @description This function converts a Balcones timesheet into a tidy data
#'   frame.
#'
#' @importFrom rio import
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr rename_with
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#'
#' @param timesheet_path A string. The path to the timesheet. See
#'   Admin > Personnel > Timesheets > Timesheet Template for an example.
#'
#' @return A data frame with employee, job number, hours worked, and dates
#'   worked.
#' @export
#'
get_timesheet_hours <-
  function(timesheet_path){

  working <-
    rio::import(
      timesheet_path
    )

  employee <- as.character(
    working[2, 4]
  )

  dates <-
    format(
      as.Date(
        as.integer(
          t(working[4, 1:7])
        ),
        origin = "1899-12-30"
      ),
      "%Y-%m-%d"
    )

  result <-
    working |>
    dplyr::select(
      c(1:7, 9)
    ) |>
    dplyr::slice(
      c(6:22, 25:29)
    ) |>
    dplyr::rename_with(
      ~ c(dates, "job_number")
    ) |>
    tidyr::pivot_longer(
      cols = 1:7,
      names_to = "work_date",
      values_to = "hours_worked",
      values_transform = as.numeric,
      names_transform = as.Date
    ) |>
    dplyr::filter(
      !is.na(hours_worked)
    ) |>
    dplyr::mutate(
      employee = employee
    ) |>
    dplyr::relocate(
      employee,
      job_number,
      work_date,
      hours_worked
    )

  return(result)
}
