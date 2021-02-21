#' Company identifier information.
#'
#' \code{company_info} Returns company identification information used by the
#' SEC to organize company financial reports. The identifiers returned by this
#' function facilitate report extraction in \code{\link{reports}}.
#'
#' @param symbol String of the company trading symbol in lower or upper case.
#'
#' @return Returns a data frame consisting of the input company's name
#' ("Company"), stock symbol ("Symbol"), "CIK" - central index key, or the unique
#' identifier used by the SEC's EDGAR system for each company, "SIC" -
#' standard industrial classification, a code identifying the broader industry
#' within which the company operates, and the end date for the company's fiscal
#' year ("Fiscal_end").
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom magrittr "%>%"
#'
#' @examples
#' company_info("tsla")
#' company_info("AAPL")
#'
#' \dontrun{
#' company_info(aapl)
#' }
#'
#' @export

company_info <- function(symbol){
  options(stringsAsFactors = FALSE)
  target_url    = paste0("https://www.sec.gov/cgi-bin/browse-edgar?CIK=",
                         symbol, "&owner=exclude&action=getcompany&Find=Search")
  search.result = xml2::read_html(target_url)

  ExtractInfo   = function(html.node) {
    info = search.result %>% rvest::html_nodes(html.node) %>% rvest::html_text()
    return(info)
  }
  company.name.raw = ExtractInfo(".companyName") %>% strsplit(" CIK")
  if (length(company.name.raw) == 0) {
    stop(sprintf("invalid company symbol: %s", symbol))
  }
  company.name = company.name.raw[[1]][1]
  CIK.raw      = ExtractInfo(".companyName a") %>% strsplit(" ")
  CIK          = CIK.raw[[1]][1]
  SIC          = ExtractInfo(".identInfo acronym+ a")
  fiscal.year.end = gsub("^.*Fiscal Year End: ", "", ExtractInfo(".identInfo")) %>%
    substr(1, 4)
  if (fiscal.year.end == "SIC:") {
    fiscal.year.end <- NA
  }

  info = data.frame(Company    = as.character(company.name),
                    Symbol     = unlist(symbol),
                    CIK        = CIK,
                    SIC        = SIC,
                    Fiscal_end = fiscal.year.end)
  return(info)
}

# "
# Note, the SEC also has an SIC ~ Standard Insustrial Classification Code:
#
# 'The Standard Industrial Classification Codes that appear in a company's disseminated
#  EDGAR filings indicate the company's type of business. These codes are also used in
#  the Division of Corporation Finance as a basis for assigning review responsibility
#  for the company's filings. For example, a company whose business was Metal Mining
#  (SIC 1000) would have its filings reviewed by staffers in the Office of Energy &
#  Transportation.'
#
# This code would be useful to extract in order to make apples-to-apples comparisions
# of company fundamentals across a broader sector.
# "
