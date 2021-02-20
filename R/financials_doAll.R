#' Wrapper for financials_sec to return panel or time series financial data
#'
#'


financials_doAll <- function(symbols,
                             years,
                             # type        = "panel", # "type : panel, cs, ts" (not sure this is necessary)
                             report_type = "10"){

  cat('\14');cat(sprintf("Gathering company and financial report identifiers...\n"))
  info          = bind_rows(lapply(symbols, function(x) company_info(x)))
  years_reports = length(years)

  rpts          = bind_rows(sapply(symbols, function(x){
    reports(x,
            type            = report_type,
            limit_results   = 20,
            prior_to        = sprintf("%s1231", max(years)),
            exclude_amended = TRUE)
  },
  simplify = FALSE),
  .id = "symbol") %>%
    mutate(Filing.type = str_to_lower(str_remove_all(Filing.type, "-")),
           Filing.date = str_remove_all(Date, pattern = "-")) %>%
    group_by(symbol) %>%
    slice(1:(years_reports*4)) %>%
    ungroup() %>%
    arrange(symbol, desc(Date))

  # if(!(min(table(rpts$symbol)) >= 4)){
  #   stop("Data are missing one or more quarterly reports.")
  # }

  rpts = left_join(rpts, info, by = c("symbol" = "Symbol"))


  res = lapply(1:nrow(rpts), function(v){

    msg = paste0("Working on report %s of %s:\n",
                 "Company     \t - \t%s\n",
                 "Report date \t - \t%s\n")

    cat("\14");cat(sprintf(msg, v, nrow(rpts), rpts$symbol[v], ymd(rpts$Filing.date[v])))


    fins = tryCatch(
      expr = {financials_sec(company_info  = rpts[v, c("Company", "symbol","CIK","SIC","Fiscal_end")],
                             report_info   = NULL,
                             accession_num = rpts$Accession.no[v],
                             report_type   = rpts$Filing.type[v],
                             filing_date   = rpts$Filing.date[v],

                             return_facts = FALSE,
                             # q4fromAnnual = TRUE,
                             testing_drop_incompletes = TRUE,
                             xml_link     = NULL)},
      error = function(e){NA}
    )

    if(is.na(fins)){
      fins = data.frame(
        Company_name   = rpts$Company[v],
        Company_symbol = rpts$symbol[v],
        CIK            = rpts$CIK[v],
        Filing_Type    = str_to_upper(str_to_upper(str_replace(rpts$Filing.type[v], "10","10-"))),
        Date_Reporting = ymd(rpts$Filing.date[v])
      )
    }

    cat("Success")
    Sys.sleep(1)
    # Per sec guidelines - request rate limited to 10 per second.
    # This function should never hit that threshold, but just to
    # be save including a sleep here.
    return(fins)
  })
  res = suppressWarnings({bind_rows(res)})
  return(res)
}
