#' Payroll Start and End Dates
#' @description Determine the beginning and end of a payroll period.
#'
#' @importFrom lubridate mday
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lubridate days_in_month
#' @importFrom glue glue
#'
#' @param todays_date
#'
#' @return A list of date strings.
#' @export
#'
get_payroll_start_end_dates <-
  function(
    todays_date = Sys.Date()
  ){

    n_day <-
      lubridate::mday(todays_date)

    n_month <-
      lubridate::month(todays_date)

    previous_month <-
      ifelse(
        n_month - 1 > 0,
        n_month - 1,
        12
      )

    n_year <-
      ifelse(
        n_month == 1 & n_day < 16,
        lubridate::year(todays_date) - 1,
        lubridate::year(todays_date)
      )

    n_days_last_month <-
      ifelse(
        n_month - 1 > 0,
        lubridate::days_in_month(n_month - 1),
        lubridate::days_in_month(12)
      )

    if (n_day > 15) {
      payroll_start <-
        glue::glue("{n_year}-{n_month}-1")
      payroll_end <-
        glue::glue("{n_year}-{n_month}-15")
    } else {
      payroll_start <-
        glue::glue("{n_year}-{previous_month}-16")
      payroll_end <-
        glue::glue("{n_year}-{previous_month}-{n_days_last_month}")
    }

    return (
      list(
        payroll_start = payroll_start,
        payroll_end = payroll_end
      )
    )
  }
