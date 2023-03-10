% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_payroll_hours.R
\name{get_payroll_hours}
\alias{get_payroll_hours}
\title{Get Payroll Hours}
\usage{
get_payroll_hours(payroll_start, payroll_end, con)
}
\arguments{
\item{payroll_start}{A string of the form 'YYYY-MM-DD'}

\item{payroll_end}{A string of the form 'YYYY-MM-DD'}

\item{con}{A database connection.}
}
\value{
A data frame of summarized hours.
}
\description{
This function returns a summarized view of the total payroll
hours given a start and end date separated by job category.
}
