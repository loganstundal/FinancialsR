#' Returns financial data from a company's SEC filing.
#'
#' \code{financials_sec} Returns company finanical indicators as reported in a
#' user-selected report identified with by Accession Number.
#'
#' @param company_info A data frame containing variables as output from
#'   \code{\link{company_info}}.
#' @param report_info To do.
#' @param accession_num Unique accession number identifying company report to
#'   extract. Can be extracted from \code{\link{reports}}.
#' @param report_type The type of report to extract, one of: 10-K, 10-Q, 10.
#' @param filing_date String indicating report filing date.
#' @param return_facts True or False indicating whether to return the underling
#'   "Facts" data frame compiled from the SEC extract in addtion to formatted
#'   financial data.
#' @param xml_link String to override financial report lookup using a user-
#'   supplied URL.
#' @param warn True or False on whether to return any error or warning messages.
#' @param symbol String of the company trading symbol in lower or upper case.
#'
#' @return Returns a data frame consisting of company financial reports within
#'   the supplied report. These consist of indicators from the company's Income
#'   Statement, Balance Sheets, and Cash Flow.
#'
#' @examples
#' financials_sec(company_info  = company_info("aapl"),
#'                accession_num = "0000320193-21-000010",
#'                report_type   = "10q",
#'                filing_date   = "20210128")
#'


#-----------------------------------------------------------------------------#
# Notes, Sources:
  # Cash-flow definition
  # https://www.investopedia.com/ask/answers/033015/what-formula-calculating-free-cash-flow.asp

# TO DO LIST:
# 1. Still need to implement q4fromAnnual option. This option should facilitate the collection
#    of Q4 data from annual reports by backing out the appropriate options using quarter reporting
#    dates.
#
# 2. IMPLIFYING inputs. I MUST call company_info to retrieve cik values used in report lookup
#    to extract relevant report accession codes. It slows things down (and adds to the number)
#    of SEC calls to call this function again at the beginning of this script.
#
#    - I'll reserve this by simplifying the input to as an input the output of a
#      company_info call + accession_num from a report() call + any output options.
#
#      - THIS could be simplified even further so I only need :
#       - company_info
#       - report_info
#       - ouptut_info
#-----------------------------------------------------------------------------#

# source("R/helper_selector.R")
# source("R/previous_quarter.R")
# source("R/helper_extract-vals.R")

financials_sec <- function(company_info,
                           report_info = NULL,
                           accession_num,
                           report_type,
                           filing_date,

                           return_facts = FALSE,
                           # q4fromAnnual = FALSE,
                           xml_link     = NULL,
                           warn         = FALSE,
                           testing_drop_incompletes = FALSE){


  #-----------------------------------------------------------------------------#
  # BLOC ~ ADMINISTRATIVE (STATUS: CLOSE TO DONE)                           ----
  #-----------------------------------------------------------------------------#
  # STATUS NOTE: (see note 2 in header for further ideas)
  d = cbind(company_info,
            accession_num     = str_remove_all(accession_num, "-"),
            accession_num_raw = accession_num,
            report_type       = report_type,
            filing_date       = filing_date)

  # TESTING:
  # tst = rpts[1,]
  # d   = cbind(info,
  #             accession_num     = str_remove_all(tst$Accession.no, "-"),
  #             accession_num_raw = tst$Accession.no,
  #             report_type       = tst$Filing.type,
  #             filing_date       = tst$Filing.date)
  #-----------------------------------------------------------------------------#



  #-----------------------------------------------------------------------------#
  # BLOC ~ LINK CONSTRUCTION (STATUS: GOOD)                                 ----
  #-----------------------------------------------------------------------------#
  # Gather report link from SEC
  index_html = sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s-index.htm",
                       d$CIK,
                       d$accession_num,
                       d$accession_num_raw) %>%
    xml2::read_html()

  # Extract "Period of Report" from Filing Detail page - to used for extracting
  # Q4 if q4fromAnnual is true.
  period_end = (index_html %>% html_nodes(".info") %>% html_text())[4]

  # Extract "Document" name for last xml on page - corresponds (so far?) to
  # full XBRL instance document associate with the filing.
  suffix = index_html %>%
    html_nodes("a") %>%
    html_text() %>%
    .[!str_detect(., "_cal|_def|_lab|_pre")] %>%
    .[str_detect(., ".xml")] %>%
    (function(x){tail(x,1)})

  target = sprintf("https://www.sec.gov/Archives/edgar/data/%s/%s/%s",
                       d$CIK,
                       d$accession_num,
                       suffix)

  # Error handling for potentially bad links
  if(!RCurl::url.exists(target)){
    msg = paste0("The function is unable to find a valid URL for:\n",
                 "Stock         \t- %s\n",
                 "Period ending \t- %s\n",
                 "Accession no. \t- %s\n",
                 "Report type   \t- %s\n",
                 "Would you like to attempt to find a good link externally to input here?\n",
                 "[enter n to skip].")
    msg = sprintf(msg, d$symbol, period_end, d$accession_num, d$report_type)
    target = readline(cat(msg))

    if(target %in% c("n","\"n\"","\'n\'")){
      stop("")
    } else{
      target = as.character(unlist(target))
      tst    = RCurl::url.exists(target)
      if(!tst){
        stop("The new link you entered did not work.")
      }
    }
  }
  #-----------------------------------------------------------------------------#



  #-----------------------------------------------------------------------------#
  # BLOC ~ FINANCIAL EXTRACTION (STATUS: GOOD)                              ----
  #-----------------------------------------------------------------------------#
  # Extract XBRL formatted financial data from xml sec document

  # Create directory for file collection and set schema path here
  fins            = XBRL()
  xbrl_cache      = tempfile("XBRLcache_")
  dir.create(path = xbrl_cache)
  fins$setCacheDir(xbrl_cache)
  cat("Downloading financial xml document from the SEC...\n")
  fins$openInstance(target)

  # Perform a discovery of the taxonomy:
  cat("Performing taxononmy discovery on xml file...\n")
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
  #-----------------------------------------------------------------------------#



  #-----------------------------------------------------------------------------#
  # BLOC ~ RESULTS TIDY (STATUS: GOOD)                                      ----
  #-----------------------------------------------------------------------------#


  # ----------------------------------- #
  # Extract facts from xbrl return
  # ----------------------------------- #
  # Format financial returns into tidy dataframe
  facts = left_join(fins.vars$fact, fins.vars$context, by = "contextId") %>%
    select(elementId, fact, startDate, endDate, dimension1, value1) %>%
    filter(endDate == period_end)
  # ----------------------------------- #


  # ----------------------------------- #
  # Extract a few variables characterizing the filing
  # ----------------------------------- #
  # Filing quarter
  if(!is.na(selector(data = facts, x = "dei_DocumentPeriodEndDate"))){
    filing_quarter = quarter(selector(data = facts, x = "dei_DocumentPeriodEndDate"))
  } else{
    filing_quarter = quarter(period_end)
  }

  # Modify Filing quarter to indicate annual data clearly
  filing_quarter = ifelse(filing_quarter == 4, "Annual", as.character(filing_quarter))

  # Report type codes (annual, quarterly), from filing object.
  filing_type    = selector(data = facts, x = "dei_DocumentType")
  annual_report  = ifelse(str_detect(filing_type,"K"), TRUE, FALSE)
  quarter_report = ifelse(str_detect(filing_type,"Q"), TRUE, FALSE)

  # Reporting period start and end dates.
  reporting_end_date = ymd(selector(data = facts, x = "dei_DocumentPeriodEndDate"))
  # ----------------------------------- #

  # ----------------------------------- #
  # just testing to resolve more naming fuckery here.
        # if(quarter_report){
        #   facts = facts
        # }
  # ----------------------------------- #


  # 20210217 - I've decided to wrap the Q4 extract into another function call,
  #            due to how financial data is reported to the sec, it is too
  #            fickle here. Instead I'll do a simple first difference of the
  #            annual valus and sum of Q1:Q3 values.
  #            This will be done using "financials_q4()"

                  # # ----------------------------------- #
                  # # if(q4fromAnnual) ~ Q_fiscal_final subset
                  # # ----------------------------------- #
                  #
                  # "Thoughts;
                  #
                  # 1. extract quarterly returns using a date subset.
                  # 2. bind all rows from original where elementId is NOT in the new
                  #    subset.
                  #    - this will give quarterly results in elementIds where I want
                  #      them and regualar results elsewhere - I think...
                  #
                  # "
                  #
                  # if(annual_report & q4fromAnnual){
                  #   tmp = facts %>%
                  #     filter(startDate == ymd(previous_quarter(period_end)) + 1,
                  #            # endDate   == ymd(period_end)
                  #            ) %>%
                  #     filter(str_starts(elementId, "us-gaap_"))
                  #
                  #   facts = bind_rows(
                  #     tmp,
                  #     facts[!facts$elementId %in% tmp$elementId,]
                  #   )
                  #   rm(tmp)
                  # }
                  # # ----------------------------------- #



  #-----------------------------------------------------------------------------#
  # BLOC ~ VARIALBE NAME EXTRACTION (STATUS: WORKING)                       ----
  #-----------------------------------------------------------------------------#
  # STATUS NOTE: Any modifications to the extraction of financial concepts
  #              should be done in the "helper_extract-vals.R" script.
  vals = extract_vals(data = facts)
  #-----------------------------------------------------------------------------#



  #-----------------------------------------------------------------------------#
  # BLOC ~ DATA FRAME CONSTRUCTION (STATUS: NEEDS ATTENTION)                ----
  #-----------------------------------------------------------------------------#
  # STATUS NOTE: Company / Report IDs should be integrated into extract_vals
  #              so all values are created in the same location.
  #
  #                - Presently variables that populate the "financial_data" df
  #                  originate in 3 locations: local (w/i this function call),
  #                  within "d" (data frame housing function inputs), and from
  #                  vals (the output of the extract_vals function call).
  #
  #                - This multitude of sources will eventually create oversight
  #                  problems and errors. It would be more efficient to
  #                  consolidate variable extraction to a single location.
  #
  #              Additionally, can consider more closely organizing DF to model
  #              a financial reporting statement.
  #  See example here:
  #  https://www.macrotrends.net/stocks/charts/TSLA/tesla/income-statement?freq=Q
  #-----------------------------------------------------------------------------#
  # Build data frame
  financial_data  = data.frame(


    # ----------------------------------- #
    # COMPANY / REPORT IDENTIFIERS
    # ----------------------------------- #
    Company_name       = str_trim(d$Company),
    Company_symbol     = str_trim(selector(data = facts, x = "dei_TradingSymbol")),
    Exchange           = selector(data = facts, x = "dei_SecurityExchangeName"),
    CIK                = d$CIK,
    SIC                = d$SIC,
    Year_Filing        = year(ymd(d$filing_date)),
    Year_Fiscal        = selector(data = facts, x = "dei_DocumentFiscalYearFocus"),
    Quarter            = filing_quarter,
    # Date_PeriodStart   = x,
    Date_PeriodEnd     = reporting_end_date,
    Date_Reporting     = ymd(d$filing_date),
    Filing_Type        = filing_type,
    Quarterly_Report   = quarter_report,
    Annual_Report      = annual_report,
    # Q4_from_Annual     = q4fromAnnual,
    # ----------------------------------- #


    # ----------------------------------- #
    # FINANCIALS - INCOME STATEMENT
    # ----------------------------------- #
    # Revenue
    Revenue                   = vals$revenue,
    Revenue_Cost              = vals$revenue_cost,

    # Gross Profit
    Gross_Profit              = vals$gross_profit,
    RD_Expenses               = vals$rd_expenses,
    SG_A_Expenses             = vals$sga_expenses,
    Other_Op_IncomeExpense    = vals$other_op_incexp,
    Operating_Expenses        = vals$op_expenses,

    # Operating Income
    Operating_Income          = vals$op_income,
    Total_NonOperating_IncExp = vals$total_nonoperating_incexp,

    # Pre-tax Income
    PreTax_Income             = vals$pretax_income,
    Income_Taxes              = vals$income_taxes,

    # Post-tax Income
    PostTax_Income            = vals$posttax_income,
    Other_Income              = vals$other_income,

    # Continuous/Discontinued Operations Income
    Income_Continuous_Ops     = vals$income_continuous_ops,
    Income_Discontinued_Ops   = vals$income_discontinued_ops,

    # Net Income
    Net_Income                = vals$net_income,
    EBITA                     = vals$ebita,
    EBIT                      = vals$ebit,
    Shares_Out_Basic          = vals$shares_out_basic,
    Shares_Out                = vals$shares_out,
    EPS_Basic                 = vals$eps_basic,
    EPS                       = vals$eps,
    # ----------------------------------- #


    # ----------------------------------- #
    # FINANCIALS - BALANCE SHEET
    # ----------------------------------- #
    # Assets
    Cash_On_Hand              = vals$cash_on_hand,
    Receivables               = vals$receivables,
    Inventory                 = vals$inventory,
    Pre_paid_Expenses         = vals$pre_paid_exp,
    Other_Current_Assets      = vals$other_current_assets,
    Total_Current_Assets      = vals$total_current_assets,
    Property_Plant_Equip      = vals$property_plant_equip,
    Long_Term_Investments     = vals$long_term_invest,
    Goodwill_Intangible_Assets= vals$goodwill_intangibles,
    Other_Long_Term_Assets    = vals$other_long_term_assets,
    Total_Long_Term_Assets    = vals$total_long_term_assets,
    Total_Assets              = vals$total_assets,

    # Liabilities
    Total_Current_Liabilities     = vals$total_current_liabilities,
    Long_Term_Debt                = vals$long_term_debt,
    Other_Non_Current_Liabilities = vals$other_noncurrent_liabilities,
    Total_Long_Term_Liabilities   = vals$total_long_term_liabilities,
    Total_Liabilities             = vals$total_liabilities,

    Share_Holder_Equity                       = vals$share_holder_equity,
    Total_Liabilities_And_Share_Holder_Equity = vals$total_liabilities_and_share_holder_equity,
    # ----------------------------------- #


    # ----------------------------------- #
    # FINANCIALS - CASH FLOW STATEMENT
    # ----------------------------------- #
    Cash_Flow_Operating = vals$cash_flow_operating,
    Cash_Flow_Investing = vals$cash_flow_investing,
    Cash_Flow_Financial = vals$cash_flow_financial,
    Cash_Flow_Net       = vals$cash_flow_net
    # ----------------------------------- #
  )
  #-----------------------------------------------------------------------------#



  #-----------------------------------------------------------------------------#
  # BLOC ~ ADDITIONAL VARIALBES (STATUS: NEEDS ADDITIONS)                   ----
  #-----------------------------------------------------------------------------#
  # Incomplete variables in "helper_extract-vals.R"
  incomplete = c("Other_Op_IncomeExpense", "Other_Income",
                 "Income_Discontinued_Ops", "EBITA", "EBIT")

  # financial_data[,incomplete] = "TO DO"

  if(testing_drop_incompletes){
    financial_data = financial_data %>%
      select(-one_of(incomplete))
  }

  # financial_data = financial_data %>%
  #   mutate(Free_Cash_Flow = ???) %>%
  #   as_tibble()

  # TO ADD:
  # 1. Margins (Gross margin, Operating margin, Net margin)
  #    See: https://www.macrotrends.net/stocks/charts/TSLA/tesla/profit-margins for examples.
  # ----------------------------------- #


  #-----------------------------------------------------------------------------#
  # BLOC ~ FUNCTION OPTIONAL WARNING MESSAGES ON RESULTS (STATUS: GOOD)     ----
  #-----------------------------------------------------------------------------#
  if(warn){
    warning(cat("\nfinancials_sec does not account for stock splits and only ",
                "retruns values as reported in SEC filings.\nUse your best ",
                "judgement when interpreting key financial indicators such ",
                "as EPS or calculated price ratios."))

    # warning(c(rptw, rpt2w))
  }
  #-----------------------------------------------------------------------------#



  #-----------------------------------------------------------------------------#
  # BLOC ~ RETURN STATMENT (STATUS: GOOD)                                   ----
  #-----------------------------------------------------------------------------#
  if(return_facts){
    return(list(financial_data   = financial_data,
                financial_source = facts))
  } else{
    return(financial_data)
  }
  #-----------------------------------------------------------------------------#
}






