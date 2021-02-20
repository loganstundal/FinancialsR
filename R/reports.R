# reports

# This function returns a dataframe containing report information from the SEC
# for a given company based on user inputs.

reports <- function(company,
                    type     = "any",
                    prior_to = NULL,
                    exclude_amended = FALSE,
                    limit_results  = 20){

  if(!is.character(company) | is.null(company)){
    stop("Invalid company")
  }

  if(!type %in% c("10","10-Q","10-Q/A", "10-K", "10-K/A", "8-K", "any")){
    stop("type option invalid. Must be one of: 10, 10-Q, 10-Q/A, 10-K, 10-K/A, 8-K, or any")
  }

  if(type == "any"){type = ""}
  if(is.null(prior_to)){prior_to = ""}

  if(!limit_results %in% c(10,20,40,80,100)){
    stop(("limit_results option invalid. Must be one of: 10, 20, 40, 80, 100."))
  }

  # Target url
  target = sprintf("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=%s&dateb=%s&owner=exclude&count=%s&search_text=",
                   company,
                   type,
                   prior_to,
                   limit_results)

  # Collect filed reports
  filings     = xml2::read_html(target)
  ExtractInfo = function(html.node) {
    info = filings %>% html_nodes(html.node) %>% html_text()
    return(info)
    }

  filing.name = ExtractInfo("#seriesDiv td:nth-child(1)")
  if (length(filing.name) == 0) {
    stop(sprintf("Invalid company symbol or foreign logical: %s", company))
  }
  filing.date = ExtractInfo(".small+ td")
  accession.no.raw = ExtractInfo(".small")
  accession.no     = gsub("^.*Acc-no: ", "", accession.no.raw) %>% substr(1, 20)

  # Collect results
  info.df = data.frame(Filing.type  = filing.name,
                       Date         = filing.date,
                       Accession.no = accession.no)

  if(exclude_amended){
    info.df = info.df %>% filter(!str_detect(Filing.type, "/A"))
  }

  return(info.df)
}
