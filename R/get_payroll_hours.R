#' Get Payroll Hours
#' @description This function returns a summarized view of the total payroll
#'   hours given a start and end date separated by job category.
#'
#' @param payroll_start A string of the form 'YYYY-MM-DD'
#' @param payroll_end A string of the form 'YYYY-MM-DD'
#' @param con A database connection.
#'
#' @return A data frame of summarized hours.
#'
get_payroll_hours <-
  function(
    payroll_start,
    payroll_end,
    con
  ){

    employee_time <-
      DBI::dbGetQuery(
        con,
        glue::glue(
          "
          SELECT * FROM employee_time
          WHERE work_date >= '{payroll_start}'
          AND work_date <= '{payroll_end}'
          "
        )
      )

    total_hours_by_category <-
      employee_time |>
      dplyr::mutate(
        job_category = dplyr::case_when(
          job_number %in% c("Vacation", "Holiday", "Sick") ~ "VHS",
          job_number %in% c("Gen/Admin", "Bidding Proposals") ~ "Indirect",
          TRUE ~ "Direct"
        )
      ) |>
      dplyr::group_by(
        employee,
        job_category
      ) |>
      dplyr::summarize(
        total_hours = sum(
          as.numeric(hours_worked)
        )
      ) |>
      tidyr::pivot_wider(
        names_from = job_category,
        values_from = total_hours
      ) |>
      tidyr::replace_na(
        list(
          Indirect = 0,
          Direct = 0,
          VHS = 0
        )
      ) |>
      dplyr::mutate(
        Total = sum(
          dplyr::across(
            .cols = !contains("employee")
          )
        ),
      ) |>
      dplyr::rename(
        Employee = employee
      )

    return(total_hours_by_category)
  }
