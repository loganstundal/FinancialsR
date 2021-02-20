# Testing script

rm(list=ls())

source("R/company_info.R")
source("R/previous_quarter.R")
source("R/reports.R")
source("R/financials_sec.R")

library(lubridate)

#-----------------------------------------------------------------------------#

# Testing bad link logic - OR - single case code mixed in here:
symbols = c("aapl","fb","tsla")
symbols = c("tsla")

"This bit should go in a convenience wrapper that returns 'd' based on the
 input ~ symbols.

Actually... reports should just be updated to return below based on length
of input vector : symbols
"

# report = "10-Q"  # Quarterly
report = "10"  # Annual

info    = company_info("tsla") # the apply functions create some odd listing. Will need to
                               # address in the wrapper - NOT the underlying fns.
# info    = sapply(symbols, company_info, simplify = FALSE)
# info    = sapply(symbols, company_info) %>% t %>% as.data.frame
rpts    = sapply(symbols, function(x){reports(x, type = report)}, simplify = FALSE)
rpts    = bind_rows(lapply(rpts, function(x){x}),
                    .id = "symbol") %>%
  arrange(symbol, desc(ymd(Date))) %>%
  mutate(Filing.type = str_to_lower(str_remove_all(Filing.type, "-")),
         Filing.date = str_remove_all(Date, pattern = "-")
         # Accession_num_raw = Accession.no,
         # Accession_num     = str_remove_all(Accession.no, pattern = "-")
         ) %>%
  filter(!str_detect(Filing.type, "a")) %>%
  slice(1:8)

# Filing.type   = str_to_lower(str_remove_all(rpts$Filing.type, "-"))
# Filing.date   = str_remove_all(rpts$Date, pattern = "-")
# Accession_num = str_remove_all(rpts$Accession.no, pattern = "-")
# Accession_num_raw = rpts$Accession.no

# ----------------------------------- #
# Good links to input when prompted:
# This is no longer necessary as the original problem SHOULD be fixed now. 20210214
    # # Apple
    # "https://www.sec.gov/Archives/edgar/data/320193/000032019321000010/aapl-20201226_htm.xml"
    #
    # # Facebook:
    # "https://www.sec.gov/Archives/edgar/data/1326801/000132680120000084/fb-09302020x10q_htm.xml"
# ----------------------------------- #

res = lapply(1:nrow(rpts), function(v){

  msg = paste0("Working on report %s of %s:\n",
               "Company     \t - \t%s\n",
               "Report date \t - \t%s\n")

  cat("\14");cat(sprintf(msg, v, nrow(rpts), rpts$symbol[v], ymd(rpts$Filing.date[v])))

  # redundant - see updated inputs for fn

  # d = data.frame(symbol            = rpts$symbol[v],
  #                accession_num     = rpts$Accession_num[v],
  #                accession_num_raw = rpts$Accession_num_raw[v],
  #                report_type       = rpts$Filing.type[v],
  #                filing_date       = rpts$Filing.date[v])

  # fins = financials_sec(symbol            = d$symbol,
  #                       accession_num     = d$accession_num,
  #                       accession_num_raw = d$accession_num_raw,
  #                       report_type       = d$report_type,
  #                       filing_date       = d$filing_date)

  # ----------------------------------- #
  # Above are outdated as of 20210215 - delete later
  # ----------------------------------- #

  fins = financials_sec(company_info  = info,
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
View(as.data.frame(t(res)))

res <- res %>% mutate(x = yq(paste(Year_Fiscal, Quarter, sep = "-")))

ggplot(data = res) +
  geom_col(aes(x = x, y = Gross_Profit)) +
  scale_x_date(breaks = res$x,
               labels = format(zoo::as.yearqtr(res$x), "%y.%q")) +
  scale_y_continuous(labels = function(x){x/1e6}) +
  labs(title    = sprintf("%s, %s","Tesla","Gross Revenue"),
       subtitle = "USD, Millions",
       caption  = sprintf("Source: SEC.")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray80", size = 0.25),
        axis.title = element_blank())

rpts = reports(company = "tsla",
               type    = "10",
               limit_results = 100)
rpts


#-----------------------------------------------------------------------------#

# CROSS-SECTION
symbols = c("tsla","f","gm","aapl","fb","msft")
info    = sapply(symbols, company_info) %>% t %>% as.data.frame
rpts    = sapply(symbols, function(x){reports(x, type = "10-Q")}, simplify = FALSE)
rpts    = bind_rows(lapply(rpts, function(x){x[1,]}))

Filing.type   = str_to_lower(str_remove_all(rpts$Filing.type, "-"))
Filing.date   = str_remove_all(rpts$Date, pattern = "-")
Accession_num = str_remove_all(rpts$Accession.no, pattern = "-")

# "In the future I could turn this into a litle do all function wrapper"
res = lapply(1:nrow(rpts), function(v){
  Sys.sleep(1)
  # Per sec guidelines - request rate limited to 10 per second.
  # This function should never hit that threshold, but just to
  # be save including a sleep here.

  print(sprintf("Working on company: %s", symbols[v]))

  d = data.frame(symbol        = symbols[v],
                 accession_num = Accession_num[v],
                 report_type   = Filing.type[v],
                 filing_date   = Filing.date[v])

  if(symbols[v] == "aapl"){
    fins = financials_sec(symbol        = d$symbol,
                      accession_num = d$accession_num,
                      report_type   = d$report_type,
                      filing_date   = d$filing_date,
                      xml_link      = "https://www.sec.gov/Archives/edgar/data/320193/000032019321000010/aapl-20201226_htm.xml")
  } else if(symbols[v] == "fb"){
    fins = financials_sec(symbol        = d$symbol,
                      accession_num = d$accession_num,
                      report_type   = d$report_type,
                      filing_date   = d$filing_date,
                      xml_link      = "https://www.sec.gov/Archives/edgar/data/1326801/000132680120000084/fb-09302020x10q_htm.xml")
    # Honestly... that "x" looks like a fucking typo...
  } else{
    fins = financials_sec(symbol        = d$symbol,
                      accession_num = d$accession_num,
                      report_type   = d$report_type,
                      filing_date   = d$filing_date)

  }
  return(fins)
})
res2 = bind_rows(res)
View(res2)

#-----------------------------------------------------------------------------#

# TIME-SERIES
# symbols = c("msft")
# info    = sapply(symbols, company_info) %>% t %>% as.data.frame
# rpts    = reports(symbols, type = "10-Q")
#
#
# Filing.type   = str_to_lower(str_remove_all(rpts$Filing.type, "-"))
# Filing.date   = str_remove_all(rpts$Date, pattern = "-")
# Accession_num = str_remove_all(rpts$Accession.no, pattern = "-")
#
#
# res = lapply(1:nrow(rpts), function(v){
#   Sys.sleep(1)
#   # Per sec guidelines - request rate limited to 10 per second.
#   # This function should never hit that threshold, but just to
#   # be save including a sleep here.
#
#   print(sprintf("Working on report of date: %s", Filing.date[v]))
#
#   d = data.frame(symbol        = symbols,
#                  accession_num = Accession_num[v],
#                  report_type   = Filing.type[v],
#                  filing_date   = Filing.date[v])
#
#   fins = financials_sec(symbol        = d$symbol,
#                         accession_num = d$accession_num,
#                         report_type   = d$report_type,
#                         filing_date   = d$filing_date)
#
#   return(fins)
# })

#-----------------------------------------------------------------------------#

# PANEL
# TO DO
res = bind_rows(res)
View(res)
