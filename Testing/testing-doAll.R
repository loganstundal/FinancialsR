# cross-sectional model using financials_sec()


# symbols = c("TSLA","GM","NKLA","SPAQ","F","WKHS",
#             "DPHC","NIO","LI","KNDI","XPEV","TM",
#             "FUV","SOLO","AYRO","BMWYY","FCAU","HMC",
#             "SHLL","HCAC")
# cinfo = lapply(symbols, function(x){tryCatch(expr = {company_info(x)}, error = function(e){})})
# cinfo = bind_rows(cinfo)
#
# # update symbols to reflect companys with sec data
# symbols = symbols[symbols %in% cinfo$Symbol]
#
#
# # reports
# rpts = sapply(symbols, function(x){
#   tryCatch(
#     expr = {reports(x, type = "10", exclude_amended = T)},
#     error = function(e){}
#   )
# }, simplify = FALSE)
# rpts = bind_rows(rpts, .id = "symbol")
#
# # update symbols to reflect companys with reports
# symbols = symbols[symbols %in% rpts$symbol]
#

# financials


# Minimal user inputs
rm(list=ls())
symbols = c( "TSLA", "GM", "NKLA", "F", "WKHS", "KNDI", "FUV", "AYRO", "HCAC")
years   = 2020:2017

source("R/company_info.R")
source("R/reports.R")
source("R/financials_sec.R")
source("R/financials_doAll.R")
source("R/helper_selector.R")
source("R/helper_extract-vals.R")

library(stringr)
library(lubridate)
# financials
x = financials_doAll(symbols = symbols,
                     years   = years)

x2 = x %>% tidyr::drop_na(Filing_Type)

sapply(x2, function(x) prop.table(table(is.na(x))))

x3 <- x2 %>% select(1:13, Net_Income, Cash_On_Hand, Total_Assets, Total_Liabilities, Share_Holder_Equity) %>%
  tidyr::drop_na(Total_Liabilities)
View(x3)


# ----------------------------------- #
# gen q4

x4 <- x3 %>% mutate(Quarter = case_when(Quarter == 4 ~ "Annual", TRUE ~ as.character(Quarter))) %>%
  arrange(Company_name, desc(Date_Reporting), Quarter) %>%
  filter(Quarter != "Annual") %>%
  tidyr::fill(Company_symbol, Exchange, .direction = "downup")

# this will be WRONG due to Q4 missing, but whatever for now.
x5 <- x4 %>%
  group_by(Company_name) %>%
  mutate(Date_PeriodStart = lead(Date_PeriodEnd, 1)) %>%
  mutate(Date_PeriodStart = case_when(is.na(Date_PeriodStart) ~ ymd(sprintf("%s-01-01",year(Date_PeriodEnd))),
                                      TRUE ~ Date_PeriodStart))

x5[,c("Company_name","Date_PeriodEnd", "Date_PeriodStart")]

# res2 <- res2 %>%
#   group_by(Company_name, Year_Fiscal) %>%
#   # summarize(across(!(Company_name:Annual_Report), function(x){x[1] - sum(x[2:4])})) %>%
#   summarize(across(Revenue:Cash_Flow_Financial, function(x){x[1] - sum(x[2:4])}),
#             Company_symbol = Company_symbol[1],
#             Year_Filing    = Year_Filing[1],
#             .groups = "keep") %>%
#   ungroup() %>%
#   mutate(Quarter = "4") %>%
#   bind_rows(., res2) %>%
#   select(Company_name, Company_symbol, Year_Filing, Year_Fiscal, Quarter, everything()) %>%
#   arrange(Company_name, desc(Year_Fiscal), desc(Quarter))


# ----------------------------------- #
rm(list=ls())
s = data.frame()

# Because fuckery with package conflicts...
x5 = readr::read_csv("Testing/dat.csv")

# library(tidyquant)
# library(quantmod)
library(stringr)
library(dplyr)

api_key = "B8XMWGKZDF182QTK"


my_get <- function(symbol, api_key){
  x = sprintf("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=%s&apikey=%s&datatype=csv&outputsize=full",
              symbol,
              api_key)
  d = readr::read_csv(url(x))
}

symbols = sort(unique(x5$Company_symbol))

for(i in symbols){
  dat = my_get(symbol = i, api_key = api_key)

  s = bind_rows(s,dat)
}














