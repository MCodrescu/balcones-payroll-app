database_backup <-
  function(
    con,
    backup_location
    ){

    tables <-
      DBI::dbListTables(con)

    tables_df_list <-
      purrr::map(
        tables,
        \(x) DBI::dbReadTable(con, x)
      )

    purrr::map2(
      tables_df_list,
      names(tables_df_list),
      \(x, y) readr::write_csv(
        x,
        glue::glue(
          "{backup_directory}/{y}_{format(Sys.Date(), '%Y%m%d')}.csv"
        )
      )
    )

  }
