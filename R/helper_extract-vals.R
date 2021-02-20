# extract_vals

# Helper function to extract reported values from SEC XBRL xml files.
# Do to varied naming conventions, there is trial and error here, so
# this separate script cleans up the financials_x() functions.

# Inputs  - tidied financial data frame
# Outputs - a named list to construct the df.

#-----------------------------------------------------------------------------#

# Names for some key financial reporting concepts vary across reports. To account
# for these changes, this section extracts the relevant concepts using a series
# of if statements.

# Attempt is for convenience - it applies the selector() function to the
# supplied vector of possible financial concept names and returns the
# first non-missing value.
attempt <- function(data, values, numeric){
  v = lapply(values,
             function(x){selector(data = data, x = x, numeric = numeric)})
  v = v[!is.na(v)]
  if(length(v) == 0){
    v = ifelse(numeric, NA_integer_, NA_character_)
  } else{
    v = first(v)
  }
  return(v)
}

#-----------------------------------------------------------------------------#


# ----------------------------------- #

# FOR TESTING PURPOSES - DO NOT FORGET TO FIRST RUN THE FOLLOWING!!!!!!
# data = facts

# ----------------------------------- #


extract_vals <- function(data){
  x = list(

    # ----------------------------------- #
    # FINANCIALS - INCOME STATEMENT
    # ----------------------------------- #
    # Revenue
    revenue = attempt(data = data, values = c("us-gaap_Revenues",
                                              "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax"), numeric = T),

    # Revenue cost
    revenue_cost = attempt(data = data, values = c("us-gaap_CostOfRevenue",
                                                   "us-gaap_CostOfGoodsAndServicesSold"), numeric = T),

    # Gross profit
    gross_profit = attempt(data = data, values = c("us-gaap_GrossProfit"), numeric = T),

    # Research & development expenses
    rd_expenses = attempt(data = data, values = c("us-gaap_ResearchAndDevelopmentExpense"), numeric = T),

    # SG&A expenses
    sga_expenses = attempt(data = data, values = c("us-gaap_SellingGeneralAndAdministrativeExpense"), numeric = T),

    # Other operational income or expense
    # other_op_incexp = attempt(data = data, values = c(""), numeric = T),
    other_op_incexp = "TO DO",

    # Operating income
    op_income = attempt(data = data, values = c("us-gaap_OperatingIncomeLoss"), numeric = T),

    # Total non-operational income or expense
    total_nonoperating_incexp = attempt(data = data, values = c("us-gaap_NonoperatingIncomeExpense"), numeric = T),

    # Pre-tax income
    pretax_income = attempt(data = data, values = c("us-gaap_IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest"), numeric = T),

    # Income taxes
    income_taxes = attempt(data = data, values = c("us-gaap_IncomeTaxExpenseBenefit"), numeric = T),

    # Post-tax income
    posttax_income = attempt(data = data, values = c("us-gaap_NetIncomeLoss",
                                                     "us-gaap_ProfitLoss"), numeric = T),

    # Other income
    # other_income = attempt(data = data, values = c(""), numeric = T),
    other_income = "TO DO",

    # Income - discontinued operations (pre-tax)
    # income_discontinued_ops = attempt(data = data, values = c(""), numeric = T),
    income_discontinued_ops = "TO DO",

    # Net income
    net_income = attempt(data = data, values = c("us-gaap_NetIncomeLoss",
                                                 "us-gaap_NetIncomeLossAvailableToCommonStockholdersBasic"), numeric = T),

    # Shares out - basic
    shares_out_basic = attempt(data = data, values = c("us-gaap_WeightedAverageNumberOfSharesOutstandingBasic"), numeric = T),

    # Shares out
    shares_out = attempt(data = data, values = c("us-gaap_WeightedAverageNumberOfDilutedSharesOutstanding"), numeric = T),

    # EPS - basic
    eps_basic = attempt(data = data, values = c("us-gaap_EarningsPerShareBasic"), numeric = T),

    # EPS
    eps = attempt(data = data, values = c("us-gaap_EarningsPerShareDiluted"), numeric = T),


    # ----------------------------------- #
    # FINANCIALS - BALANCE SHEET
    # ----------------------------------- #
    # Cash on hand
    cash_on_hand = attempt(data = data, values = c("us-gaap_CashAndCashEquivalentsAtCarryingValue"), numeric = T),

    # Receivables
    receivables = attempt(data = data, values = c("us-gaap_AccountsReceivableNetCurrent"), numeric = T),

    # Inventory
    inventory = attempt(data = data, values = c("us-gaap_InventoryNet"), numeric = T),

    # Pre-paid expenses
    pre_paid_exp = attempt(data = data, values = c("us-gaap_PrepaidExpenseAndOtherAssetsCurrent"), numeric = T),

    # Other current assets
    other_current_assets = attempt(data = data, values = c("us-gaap_OtherAssetsCurrent"), numeric = T),

    # Total current assets
    total_current_assets = attempt(data = data, values = c("us-gaap_AssetsCurrent"), numeric = T),

    # Property, plants, and equipment capital
    property_plant_equip = attempt(data = data, values = c("us-gaap_PropertyPlantAndEquipmentNet"), numeric = T),

    # Long-term investments
    long_term_invest = attempt(data = data, values = c("us-gaap_MarketableSecuritiesNoncurrent"), numeric = T),

    # Goodwill and intangible assets
    goodwill_intangibles = tryCatch(
      expr = {
        x = c(attempt(data = data, values = c("us-gaap_Goodwill"), numeric = T),
              attempt(data = data, values = c("us-gaap_IntangibleAssetsNetExcludingGoodwill"), numeric = T))
        if(!all(is.na(x))){
          sum(x, na.rm = T)
        } else{NA_integer_}
        },
      error = function(e){NA_integer_}
    ),

    # Other long-term assets
    other_long_term_assets = attempt(data = data, values = c("us-gaap_OtherAssetsNoncurrent"), numeric = T),

    # Total long-term assets
    total_long_term_assets = attempt(data = data, values = c("us-gaap_AssetsNoncurrent"), numeric = T),

    # Total assets
    total_assets = attempt(data = data, values = c("us-gaap_Assets"), numeric = T),

    # Total current liabilities
    total_current_liabilities = attempt(data = data, values = c("us-gaap_LiabilitiesCurrent"), numeric = T),

    # Long term debt
    long_term_debt = attempt(data = data, values = c("us-gaap_LongTermDebtNoncurrent"), numeric = T),

    # Other non-current liabilities
    other_noncurrent_liabilities = attempt(data = data, values = c("us-gaap_OtherLiabilitiesNoncurrent"), numeric = T),

    # Total long-term liabilities
    total_long_term_liabilities = attempt(data = data, values = c("us-gaap_LiabilitiesNoncurrent"), numeric = T),

    # Total liabilities
    total_liabilities = attempt(data = data, values = c("us-gaap_Liabilities"), numeric = T),

    # Share-holder equity
    share_holder_equity = attempt(data = data, values = c("us-gaap_StockholdersEquity",
                                                          "us-gaap_StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest"), numeric = T),

    # Total liabilities and share holder equity
    total_liabilities_and_share_holder_equity = attempt(data = data, values = c("us-gaap_LiabilitiesAndStockholdersEquity"), numeric = T),


    # ----------------------------------- #
    # FINANCIALS - BALANCE SHEET
    # ----------------------------------- #
    # Cash flow - operating
    cash_flow_operating = attempt(data = data, values = c("us-gaap_NetCashProvidedByUsedInOperatingActivities"), numeric = T),

    # Cash flow - investing
    cash_flow_investing = attempt(data = data, values = c("us-gaap_NetCashProvidedByUsedInInvestingActivities"), numeric = T),

    # Cash flow - financial
    cash_flow_financial = attempt(data = data, values = c("us-gaap_NetCashProvidedByUsedInFinancingActivities"), numeric = T)
  )

  # ----------------------------------- #
  # CALCULATED VARIABLES
  # ----------------------------------- #

  # Operating expenses
  x$op_expenses = tryCatch(
    expr = {sum(c(x$revenue_cost, x$rd_expenses, x$sga_expenses),na.rm = FALSE)},
    error = function(e){NA_integer_}
  )

  # Income - continuous operations (pre-tax)
  x$income_continuous_ops = tryCatch(
    expr = {x$pretax_income - x$income_taxes},
    error = function(e){NA_integer_}
  )

  # Net cash flow
  x$cash_flow_net = tryCatch(
    expr = {sum(c(x$cash_flow_operating, x$cash_flow_investing, x$cash_flow_financial), na.rm = FALSE)},
    error = function(e){NA_integer_}
  )

  # EBITA
  # x$ebita = attempt(data = data, values = c(""), numeric = T)
  x$ebita = "TO DO"

  # EBIT
  # x$ebit = attempt(data = data, values = c(""), numeric = T)
  x$ebit = "TO DO"
  # ----------------------------------- #

  return(x)
}

#-----------------------------------------------------------------------------#

