% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financials_sec.R
\name{financials_sec}
\alias{financials_sec}
\title{Returns financial data from a company's SEC filing.}
\usage{
financials_sec(
  company_info,
  report_info = NULL,
  accession_num,
  report_type,
  filing_date,
  return_facts = FALSE,
  xml_link = NULL,
  warn = FALSE,
  testing_drop_incompletes = FALSE
)
}
\arguments{
\item{company_info}{A data frame containing variables as output from
\code{\link{company_info}}.}

\item{report_info}{To do.}

\item{accession_num}{Unique accession number identifying company report to
extract. Can be extracted from \code{\link{reports}}.}

\item{report_type}{The type of report to extract, one of: 10-K, 10-Q, 10.}

\item{filing_date}{String indicating report filing date.}

\item{return_facts}{True or False indicating whether to return the underling
"Facts" data frame compiled from the SEC extract in addtion to formatted
financial data.}

\item{xml_link}{String to override financial report lookup using a user-
supplied URL.}

\item{warn}{True or False on whether to return any error or warning messages.}

\item{symbol}{String of the company trading symbol in lower or upper case.}
}
\value{
Returns a data frame consisting of company financial reports within
  the supplied report. These consist of indicators from the company's Income
  Statement, Balance Sheets, and Cash Flow.
}
\description{
\code{financials_sec} Returns company finanical indicators as reported in a
user-selected report identified with by Accession Number.
}
\examples{
financials_sec(company_info  = company_info("aapl"),
               accession_num = "0000320193-21-000010",
               report_type   = "10q",
               filing_date   = "20210128")

}
