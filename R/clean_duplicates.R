#' Clean Duplicate Time Entries
#'
#' @param con A database connection object.
#'
#' @return An integer number of duplicate rows found.
#'
clean_duplicates <-
  function(
    con
  ){

    result <- DBI::dbSendQuery(
      con,
      '
      DELETE FROM employee_time
      WHERE time_entry_id IN (
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
            COUNT(DISTINCT(time_entry_id)) > 1
      );
      '
    )

    return(
      DBI::dbGetRowsAffected(result)
    )

  }
