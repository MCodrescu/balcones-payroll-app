To_Knack_Transform <- function(working){
  
  # Read the excel sheet
  working <- read_excel(working)
  
  # Creates a blank data frame in the correct format.
  hours <- data.frame(matrix(nrow = 1, ncol = 4))
  colnames(hours) <- c("Employee", "Job_Number", "Hours", "Date")
  
  # Prepares the next block of the program to scan the timesheet
  # for important information.
  col_width <- 1:7
  row_len <- 6:29
  row_len <- row_len[row_len != 24]
  count = 1
  employee <- working[2,4]
  
  # Scans the timesheet to find the important information and inputs
  # it into the hours data frame.
  for (i in col_width){
    for(j in row_len){
      if (!is.na(working[j, i])){
        hours[count, "Employee"] <- employee
        hours[count, "Job_Number"] <- working[j, 9]
        hours[count, "Hours"] <- working[j, i]
        hours[count, "Date"] <- format(as.Date(as.integer(working[4, i]), origin = "1899-12-30"), "%Y-%m-%d")
        count <- count + 1
      }
    }
  }
  
  # Returns the result as a data frame
  colnames(hours) <- c("account", "job_number", "hours_worked", "date_worked")
  return (hours)
}