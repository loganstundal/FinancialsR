# financials_sec()

# Return company financial statements from SEC in tidy format


# TO DO LIST:
"
1. Still need to implement q4fromAnnual option. This option should facilitate the collection
   of Q4 data from annual reports by backing out the appropriate options using quarter reporting
   dates.
"


require(stringr)
require(XML)
require(XBRL)
require(tidyverse)

financials_sec <- function(symbol,
                           accession_num,
                           accession_num_raw,
                           report_type, filing_date,
                           return_facts = FALSE,
                           q4fromAnnual = FALSE,
                           xml_link     = NULL){

  # for testing
  d = data.frame(symbol            = symbol,
                 accession_num     = accession_num,
                 accession_num_raw = accession_num_raw,
                 report_type       = report_type,
                 filing_date       = filing_date)

  # Gather relevant data
  info       = company_info(d$symbol)
  cik        = info$CIK


  # ----------------------------------- #
  # I need some code here to quickly reach out to the
  # SEC page with the submission to grab the period of report date
  # as this is used in the URL. This would be more robust than
  # guessing as I am doing here.
  index_html = sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-index.htm",
                       cik,
                       d$accession_num,
                       d$accession_num_raw) %>%
    xml2::read_html()

  # period_end = (index_html %>% html_nodes(".info") %>% html_text())[4]


  # THIS IS WHAT I WAS DOING... TOO SIMPLE
  # period_end = previous_quarter(d$filing_date)


  "
  ... ....... I could also just grab the report link from here while alread in...

  No - I'd still be guessing the changing link structure from a list of 'a' class
  possibilities. Better to just stick with the brute-force link approach.
  It's less efficient, but will work.

  ... BUT ...

  IF (BIG IF) - if the xml doc link is ALWAYS at the end of the 'a' class, xml
  subset list, then I could grab that and use it to construct the link very easily:
  "
  suffix = index_html %>% html_nodes("a") %>% html_text() %>%
    .[str_detect(., ".xml")] %>% (function(x){tail(x,1)})
  target = sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s",
                       cik,
                       d$accession_num,
                       suffix)

  "Ugh... yeah this would be potentially much faster saving me from having to ping
   through a list of all but one dead (hopefully) link."


  "ALL LINK SETUP BELOW IS REDUNDANT AND CAN GO EXCEPT THE ERROR HANDLING.

  VERIFY THIS WITH SOME TESTING THOUGH."
  # ----------------------------------- #

  # if(is.null(xml_link)){
  #   # Construct target link - SEC does NOT have a consistent nomenclature for the last part of the
  #   # html. Will construct all, test ping response and retain working link.
  #
  #   # First 3 elements always the same : cik/accession_num/symbol
  #   # Final elements vary
  #   targets = list(
  #     # Symbol-PeriodEnd.xml
  #     sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-%s.xml",
  #             cik,
  #             d$accession_num,
  #             d$symbol,
  #             period_end),
  #
  #     # Symbol-ReportType_PeriodEnd_htm.xml
  #     sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-%s_%s_htm.xml",
  #             cik,
  #             d$accession_num,
  #             d$symbol,
  #             d$report_type,
  #             period_end),
  #
  #     # Symbol-PeriodEnd_htm.xml
  #     sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-%s_htm.xml",
  #             cik,
  #             d$accession_num,
  #             d$symbol,
  #             period_end),
  #     # These seem like just typos...
  #
  #     # Inserting an 'x' between 10 and the letter designation
  #     sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-%s%s_htm.xml",
  #             cik,
  #             d$accession_num,
  #             d$symbol,
  #             period_end,
  #             paste(str_sub(d$report_type,1,2), str_sub(d$report_type,3,3), sep = "x")),
  #
  #     # Using mmddyyyy rather than the more common ymd + inserting an 'x' between date and report type
  #     sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-%s%s_htm.xml",
  #             cik,
  #             d$accession_num,
  #             d$symbol,
  #             format(ymd(period_end), "%m%d%Y"),
  #             paste0("x", d$report_type))
  #   )
  #
  #   # Check links
  #   tst = lapply(targets, function(x){
  #     RCurl::url.exists(x)
  #   })
  #
  #   if(any(as.logical(tst))){
  #     # Create target link based on link with good return code
  #     target = unlist(targets[as.logical(tst)])
  #   } else if(!any(as.logical(tst))){
  #     msg = paste0("The function is unable to find a valid URL for:\n",
  #                  "Stock         \t- %s\n",
  #                  "Period ending \t- %s\n",
  #                  "Accession no. \t- %s\n",
  #                  "Report type   \t- %s\n",
  #                  "Would you like to attempt to find a good link externally to input here?\n",
  #                  "[enter n otherwise and the function will terminate].")
  #     msg = sprintf(msg, d$symbol, period_end, d$accession_num, d$report_type)
  #     target = readline(cat(msg))
  #
  #     if(target == "n"){
  #       stop("")
  #     } else{
  #       target = as.character(unlist(target))
  #       tst    = RCurl::url.exists(target)
  #       if(!tst){
  #         stop("The link you entered did not work.")
  #       }
  #     }
  #   }
  #
  # } else {
  #  target = xml_link
  #  if(!RCurl::url.exists(target)){
  #    stop(sprintf("No financial documents found for %s. Perhaps validate links?", d$symbol))
  #  }
  # }


#-----------------------------------------------------------------------------#

  # Download financial statement
        # destination = paste(tempdir(), sprintf("%s_%s_%s.txt", d$symbol, d$report_type, period_end), sep = "\\")
        # download.file(url      = target,
        #               destfile = destination,
        #               quiet    = TRUE)
        #
        # tst = xmlParse(destination)
        # tst = xmlToList(tst)
  # ----------------------------------- #
  fins <- XBRL()

  ##########################################################
  # DIRECTORY SETUP - ATTEMPTING WITH PRE-DL'D SCHEMAS
        # dir_xbrl <- "C:/Users/logan/Documents/R/R - My Packages/FinancialsR/Data/xbrl_schemas"
        # dir_schemas = list.files(dir_xbrl)
        #
        # proper_gaap = dir_schemas[str_detect(dir_schemas, as.character(year(ymd(d$filing_date))-1))]
        #
        # dir_xbrl = paste(dir_xbrl, proper_gaap, sep = "/")
        # dir_original_files = list.files(dir_xbrl)
  ##########################################################

  xbrl_cache = tempfile("XBRLcache_")
  dir.create(path = xbrl_cache)
  fins$setCacheDir(xbrl_cache)

  fins$openInstance(target)

  # Perform a discovery of the taxonomy:
  tryCatch(
    expr  = {suppressWarnings({fins$processSchema(fins$getSchemaName())})},
    error = function(e){})

  # Process instance file:
  fins$processContexts()
  fins$processUnits()
  fins$processFacts()
  fins$processFootnotes()
  fins$closeInstance()
  fins.vars = fins$getResults()

  ##########################################################
      # lapply(paste("C:/Users/logan/Documents/R/R - My Packages/FinancialsR/Data/xbrl_schemas",
      #              proper_gaap,
      #              setdiff(list.files(dir_xbrl), dir_original_files), sep = "/"),
      #        file.remove)
  ##########################################################


  #-----------------------------------------------------------------------------#

# ----------------------------------- #


  # Full data frame
  facts = left_join(fins.vars$fact, fins.vars$context, by = "contextId") %>%
    left_join(., fins.vars$label[,c("elementId","labelString")], by = "elementId")

  # tst = tst %>%
  #   filter(startDate == ymd(previous_quarter(period_end)) + 1,
  #          endDate   == ymd(period_end)) %>%
  #   select(elementId:measure)


  # Subset financial indicators data frame - TO FINISH
  # ----------------------------------- #
  # TO DO HERE

  # filter facts based on user input type 10-k or 10-q using my calc dates or
  # "startDate" and "endDate" from merge above.
  # function nearby -can use this to subset:

  " My logic here - there are three (2?) type of information in these reports from what I can tell
      1. quarter (if appropriate)
      2. year-to-date
      3. year-on-year

      I can use the default 'facts' created above for generic info (company names, report type, shares out, etc.)
      Then I can create a subset of the data based on input report type to easily select relevant vars (eg., revenue)
  "

  selector <- function(dat, x, second_condition = NULL){
    if(is.null(second_condition)){
      tryCatch(
        expr  = {val = dat[dat$elementId == x,]$fact[1]},
        error = {function(e) return(NA)}
      )
    } else{
      filters = list(second_condition)

      dat = dat %>% filter(elementId == x,
                           !!!filters)
      tryCatch(
        expr  = {val = dat[dat$elementId == x,]$fact[1]},
        error = {function(e) return(NA)}
      )
    }
    return(val)
  }

  # initial toy logic for revenue:
            # if(str_detect(d$report_type, "q")){
            #   facts_q = facts %>%
            #     filter(startDate == ymd(previous_quarter(period_end)) + 1,
            #            endDate   == ymd(period_end))
            # } else{
            #   # facts_k
            #   facts_k = facts %>%
            #     filter(startDate == ymd(previous_quarter(period_end)) + 1,
            #            endDate   == ymd(period_end))
            #   stop("FINISH ME!")
            # }
  # selector(dat = facts_q, x = "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax",
  #          second_condition = quote(is.na(dimension1)))


    # ----------------------------------- #




  # ----------------------------------- #
  # Searching for appropriate vars - DELETE LATER
          # vals = sort(cbind(unique(facts$elementId)))
          # # vals = vals[str_starts(vals, "us-gaap")]
          # vals = sort(cbind(facts$elementId))
          #
          # # cat("\14");vals[str_detect(str_to_lower(vals), "earn")]
          #
          # key = "revenue"
          #
          # keys = vals[str_detect(str_to_lower(vals), key)]
          # keys = keys[!str_detect(keys, pattern = "Text")]
          #
          # # tst = facts
          # # tst = facts_q
          #
          # cat("\14");cbind(
          #   Var = tst[tst$elementId %in% keys,]$elementId,
          #   Val = as.numeric(tst[tst$elementId %in% keys,]$fact) / 1e6
          # )
          #
          # # if all else fails
          # full = as.data.frame(cbind(
          #   Var = tst$elementId,
          #   Val = tst$fact
          # ))
  # ----------------------------------- #

  # this is going to be ugly as shit.
  # Revenue
  if(!is.na(selector(dat = facts, x = "us-gaap_Revenues"))){
    revenue = as.numeric(selector(dat = facts, x = "us-gaap_Revenues"))
  } else if(!is.na(selector(dat = facts, x = "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax"))){
    revenue = as.numeric(selector(dat = facts, x = "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax"))
  } else{
    revenue = NA
  }

  # Cost of Revenue
  if(!is.na(selector(dat = facts, x = "us-gaap_CostOfGoodsAndServicesSold"))){
    revenue_cost = as.numeric(selector(dat = facts, x = "us-gaap_CostOfGoodsAndServicesSold"))
  } else if(!is.na(selector(dat = facts, x = "us-gaap_CostOfRevenue"))){
    revenue_cost = as.numeric(selector(dat = facts, x = "us-gaap_CostOfRevenue"))
  } else{
    revenue_cost = NA
  }

  # Gross Profit
  if(!is.na(selector(dat = facts, x = "us-gaap_GrossProfit"))){
    gross_profit = as.numeric(selector(dat = facts, x = "us-gaap_GrossProfit"))
  } else if(!is.na(revenue)&!is.na(revenue_cost)){
    gross_profit = revenue - revenue_cost
  } else{
    gross_profit = NA
  }

  # Cash - Cash and equivalents:
  if(!is.na(selector(dat = facts, x = "us-gaap_CashCashEquivalentsAndShortTermInvestments"))){
    cash_and_equiv = as.numeric(selector(dat = facts, x = "us-gaap_CashCashEquivalentsAndShortTermInvestments"))
  } else if(!is.na(selector(dat = facts, x = "us-gaap_CashAndCashEquivalentsAtCarryingValue"))){
    cash_and_equiv = as.numeric(selector(dat = facts, x = "us-gaap_CashAndCashEquivalentsAtCarryingValue"))
  } else{
    cash_and_equiv = NA
  }

  # Cash - Short term investments:
  if(!is.na(selector(dat = facts, x = "us-gaap_AvailableForSaleSecuritiesDebtSecuritiesCurrent"))){
    cash_st = as.numeric(selector(dat = facts, x = "us-gaap_AvailableForSaleSecuritiesDebtSecuritiesCurrent"))
  } else{
    cash_st = NA
  }




  # # format:
  # if(!is.na(selector(dat = facts, x = ""))){
  #   value = value
  # } else if(!is.na(selector(dat = facts, x = ""))){
  #   value = value
  # } else{
  #   value = NA
  # }

  # Build data frame
  financial_data  = data.frame(
    # Administrative
    Company_name       = info$Company,
    Company_symbol     = selector(dat = facts, x = "dei_TradingSymbol"),
    Exchange           = selector(dat = facts, x = "dei_SecurityExchangeName"),
    CIK                = info$CIK,
    SIC                = info$SIC,
    Year_Filing        = year(ymd(d$filing_date)),
    Year_Fiscal        = selector(dat = facts, x = "dei_DocumentFiscalYearFocus"),
    Quarter            = quarter(selector(dat = facts, x = "dei_DocumentPeriodEndDate")),
    Date_PeriodEnd     = ymd(selector(dat = facts, x = "dei_DocumentPeriodEndDate")),
    Date_Reporting     = ymd(d$filing_date),
    Filing_Type        = selector(dat = facts, x = "dei_DocumentType"),
    Quarterly_Report   = ifelse(!is.na(as.logical(selector(dat = facts, x = "dei_DocumentQuarterlyReport"))),TRUE,FALSE),
    Annual_Report      = ifelse(!is.na(as.logical(selector(dat = facts, x = "dei_DocumentAnnualReport"))), TRUE, FALSE),
    Shares_Outstanding = as.numeric(selector(dat = facts, x = "dei_EntityCommonStockSharesOutstanding")),

    # Financial Indicators
    Net_Income         = as.numeric(selector(dat = facts, x = "us-gaap_NetIncomeLoss")),
    EPS_Basic          = as.numeric(selector(dat = facts, x = "us-gaap_EarningsPerShareBasic")),
    EPS_Diluted        = as.numeric(selector(dat = facts, x = "us-gaap_EarningsPerShareDiluted")),
    Gross_Profit       = gross_profit,
    Revenue            = revenue,
    Revenue_cost       = revenue_cost,

    # Balance Sheet
    Cash_Flow_OperatingActivities = as.numeric(selector(dat = facts, x = "us-gaap_NetCashProvidedByUsedInOperatingActivities")),
    Cash_and_Equivalents          = cash_and_equiv,
    Cash_ST_Investments           = cash_st,
    Cash_Total                    = sum(cash_st, cash_and_equiv, na.rm = T),

    Accounts_Receivable           = as.numeric(selector(dat = facts, x = "us-gaap_AccountsReceivableNetCurrent")),
    Inventory                     = as.numeric(selector(dat = facts, x = "us-gaap_InventoryNet")),
    # Other_Current_Assets = as.numeric(selector(dat = facts, x = "us-gaap_OtherAssetsCurrent")),
    Total_Current_Assets          = as.numeric(selector(dat = facts, x = "us-gaap_AssetsCurrent" )),
    Total_Assets                  = as.numeric(selector(dat = facts, x = "us-gaap_Assets")),

    Accounts_Payable              = as.numeric(selector(dat = facts, x = "us-gaap_AccountsPayableCurrent")),
    Total_Current_Liabilities     = as.numeric(selector(dat = facts, x = "us-gaap_LiabilitiesCurrent")),
    Other_Noncurrent_Liabilities  = as.numeric(selector(dat = facts, x = "us-gaap_OtherLiabilitiesNoncurrent")),
    Total_Liabilities             = as.numeric(selector(dat = facts, x = "us-gaap_Liabilities")),

    Total_Liabilities_Equity      = as.numeric(selector(dat = facts, x = "us-gaap_LiabilitiesAndStockholdersEquity"))
  ) %>%
    mutate(Free_Cash_Flow = Cash_Flow_OperatingActivities - Other_Noncurrent_Liabilities)
    as_tibble()

return(financial_data)
}


# Notes, Sources:

# Cash-flow definition
# https://www.investopedia.com/ask/answers/033015/what-formula-calculating-free-cash-flow.asp
#-----------------------------------------------------------------------------#




