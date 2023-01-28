test_that(
  "correct dates are returned for different days of the year",
  {
    expect_equal(
      get_payroll_start_end_dates(
        as.Date("2023-01-17")
      ),
      list(
        payroll_start = "2023-1-1",
        payroll_end = "2023-1-15"
      )
    )

    expect_equal(
      get_payroll_start_end_dates(
        as.Date("2023-01-01")
      ),
      list(
        payroll_start = "2022-12-16",
        payroll_end = "2022-12-31"
      )
    )

    expect_equal(
      get_payroll_start_end_dates(
        as.Date("2022-08-15")
      ),
      list(
        payroll_start = "2022-7-16",
        payroll_end = "2022-7-31"
      )
    )
  }
)
