# previous_quarter

# Helper function which takes a date and returns date for end of previous quarter
# This is necessary as SEC organizes reports by filing dates but uses Period of Report
# ending dates in URLs.

previous_quarter <- function(x){
  # https://stackoverflow.com/questions/27015838/end-of-previous-quarter-last-day-of-previous-quarter
  "Returns end date of previous quarter for a given filing date."
  file_date        = ymd(x)
  yearsToSample    = years(year(file_date) - year(origin))
  additionalMonths = months((quarter(file_date) - 1) * 3)
  quarterEnd       = str_remove_all(ymd(origin) + yearsToSample + additionalMonths - days(1), "-")
  return(quarterEnd)
  }
