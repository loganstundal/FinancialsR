# testing 2 - moving toward building a panel, ts, or cs robust wrapper


rm(list=ls())

source("R/financials_sec.R")
source("R/company_info.R")
source("R/reports.R")

#-----------------------------------------------------------------------------#
# BLOC ~ SETUP (STATUS - WORKING)                                         ----
#-----------------------------------------------------------------------------#

# Minimal user inputs
symbols = c("tsla", "f")
years   = 2020:2019

#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# BLOC ~ FUNCTION (STATUS - WORKING)                                      ----
#-----------------------------------------------------------------------------#
financials_doAll <- function(symbols,
                             years,
                             # type        = "panel", # "type : panel, cs, ts" (not sure this is necessary)
                             report_type = "10"){

  info          = bind_rows(lapply(symbols, function(x) company_info(x)))
  years_reports = length(years)

  rpts          = bind_rows(sapply(symbols, function(x){
    reports(x,
            type            = report_type,
            limit_results   = 20,
            prior_to        = sprintf("%s0101", max(years)),
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

  if(!(min(table(rpts$symbol)) == years_reports * 4)){
    stop("Data are missing one or more quarterly reports.")
  }

  rpts = left_join(rpts, info, by = c("symbol" = "Symbol"))


  res = lapply(1:nrow(rpts), function(v){

    msg = paste0("Working on report %s of %s:\n",
                 "Company     \t - \t%s\n",
                 "Report date \t - \t%s\n")

    cat("\14");cat(sprintf(msg, v, nrow(rpts), rpts$symbol[v], ymd(rpts$Filing.date[v])))


    fins = financials_sec(company_info  = rpts[v, c("Company", "symbol","CIK","SIC","Fiscal_end")],
                          report_info   = NULL,
                          accession_num = rpts$Accession.no[v],
                          report_type   = rpts$Filing.type[v],
                          filing_date   = rpts$Filing.date[v],

                          return_facts = FALSE,
                          # q4fromAnnual = TRUE,
                          testing_drop_incompletes = TRUE,
                          xml_link     = NULL)

    cat("Success")
    Sys.sleep(1)
    # Per sec guidelines - request rate limited to 10 per second.
    # This function should never hit that threshold, but just to
    # be save including a sleep here.
    return(fins)
  })
  res = bind_rows(res)
  return(res)
}

res = financials_doAll(symbols = symbols,
                       years   = years)
View(res)

View(t(res))

#-----------------------------------------------------------------------------#
# BLOC ~ DATA EXTRACT (STATUS: WORK IN PROGRESS)                          ----
#-----------------------------------------------------------------------------#

"THIS IS A Q4 EXTRACTION - THIS CODE IS ESSENTIALLY FROM THE FINANCIALS_Q4.R SCRIPT."


# Gen Q4 data:
tstdf2 = tstdf %>%
  group_by(Company_name, Year_Fiscal) %>%
  summarize(across(Revenue:Cash_Flow_Financial, function(x){x[1] - sum(x[2:4])}),
            Company_symbol = Company_symbol[1],
            Year_Filing    = Year_Filing[1],
            .groups = "keep") %>%
  ungroup() %>%
  mutate(Quarter = "4") %>%
  bind_rows(., tstdf) %>%
  select(Company_name, Company_symbol, Year_Filing, Year_Fiscal, Quarter, everything()) %>%
  arrange(Company_name, desc(Year_Fiscal), desc(Quarter))




#-----------------------------------------------------------------------------#
# View in millions - for easy validation purposes
#-----------------------------------------------------------------------------#
      # cat("\14");apply(res, 2,
      #       function(x){tryCatch(
      #         expr = {
      #           if(str_length(x[1])>6){
      #             as.numeric(x)/1e6
      #             } else{x}
      #           },
      #         error = function(e){x},
      #         warning = function(w){x}
      #       )}) %>%
      #   as.data.frame() %>% View
#-----------------------------------------------------------------------------#









#-----------------------------------------------------------------------------#
# Plotting - delete later obviously
#-----------------------------------------------------------------------------#

d = res %>% filter(Company_symbol == "TSLA")

d = d %>%
  group_by(Company_name, Year_Fiscal) %>%
  summarize(across(Revenue:Cash_Flow_Financial, function(x){x[1] - sum(x[2:4])}),
            Company_symbol = Company_symbol[1],
            Year_Filing    = Year_Filing[1],
            .groups = "keep") %>%
  ungroup() %>%
  mutate(Quarter = "4") %>%
  bind_rows(., d) %>%
  select(Company_name, Company_symbol, Year_Filing, Year_Fiscal, Quarter, Revenue, Gross_Profit) %>%
  filter(Quarter != "Annual") %>%
  arrange(Company_name, desc(Year_Fiscal), desc(Quarter))

library(ggplot2)

d = d %>%
  tidyr::drop_na(Revenue, Gross_Profit) %>%
  mutate(x = as.numeric(paste(Year_Fiscal, Quarter, sep = ".")))

ggplot(data = d, aes(x = x)) +
  geom_line(aes(y = Revenue, color = "Revenue")) +
  geom_line(aes(y = Gross_Profit, color = "Gross Profit")) +
  scale_y_continuous(labels = function(x){x/1e6}) +
  # scale_x_continuous(breaks = 1:7,
  #                    labels = paste(sort(d$Year_Fiscal), d$Quarter, sep = " - ")) +
  theme_minimal() +
  labs(title = "Tesla - TLSA",
       subtitle = "Revenue and Gross Profits: 2018-2019")


#-----------------------------------------------------------------------------#







