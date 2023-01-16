# Send timesheet data to MYSQL database
upload_to_mysql <- function(time_to_add) {
  # Connect to DB
  con <- dbConnect(
    MySQL(),
    host = '166.62.27.56',
    port = 3306,
    username = 'MCodrescu',
    password = "Blackcar1997!",
    dbname = "job_register"
  )
  # Number of rows
  n <- nrow(time_to_add)
  
  # Insert the data into employee time table
  for (i in 1:n) {
    query <-
      glue(
        "INSERT INTO employee_time (employee, job_number, hours_worked, work_date) VALUES ('{time_to_add[i,1]}','{time_to_add[i,2]}', '{time_to_add[i,3]}', '{time_to_add[i,4]}')"
      )
    dbSendQuery(con, query)
  }
  
  
  # Close DB connection
  dbDisconnect(con)
}