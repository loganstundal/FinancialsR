#' Return api call from Alphavantage
#'



# library(tidyquant)
# sp500 = tq_index("sp500")
#
#
# stocks.ratios <- sp500[1:4, "symbol"] %>%
#   tq_get(get  = "key.ratios",from = "2000-01-01",to = "2017-12-31") %>%
#   group_by(symbol)
#
#
#
av_api_key("B8XMWGKZDF182QTK")


# 15-min interval stock data
# tq_get("AAPL",
#        get        = "alphavantage",
#        av_fun     = "TIME_SERIES_INTRADAY",
#        interval   = "15min",
#        outputsize = "full")
#
#
# # income statment
# aapl_income  = tq_get("AAPL",
#        get        = "alphavantage",
#        av_fun     = "INCOME_STATEMENT"
#        # interval   = "15min",
#        # outputsize = "full"
#        )




x = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=IBM&interval=5min&apikey=demo&datatype=csv"


# x = sprintf("https://www.alphavantage.co/query?function=%s&symbol=%s&interval=5min&apikey=demo&datatype=csv",
#             fn,
#             symbol,
#             )


x = "https://www.alphavantage.co/query?function=XXX&symbol=IBM&apikey=demo"
x = "https://www.alphavantage.co/query?function=XXX&symbol=IBM&apikey=demo"

type = "INCOME_STATEMENT"
symbol = "TSLA"
api_key = "B8XMWGKZDF182QTK"



# ----------------------------------- #
# TIME SERIES - INTERDAY
# ----------------------------------- #

fn
symbol
interval
adjusted
outputsize
datatype
api_key

x = sprintf("https://www.alphavantage.co/query?function=%s&symbol=%s&interval=%s&apikey=%s&outputsize=compact&datatype=csv",
            "TIME_SERIES_INTRADAY",
            symbol,
            "1min",
            api_key)

d = readr::read_csv(url(x))

d %>% head

View(d)

ggplot(data = d, aes(x = timestamp, y = close)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour",
                   date_labels = "%H:%m")

# ----------------------------------- #
# FUNDAMENTAL DATA
# ----------------------------------- #
# type = options include: OVERVIEW, INCOME_STATEMENT, BALANCE_SHEET, CASH_FLOW, EARNINGS

# OUTPUT - json

x = sprintf("https://www.alphavantage.co/query?function=%s&symbol=%s&apikey=%s",
            type,
            symbol,
            api_key)
# ----------------------------------- #


# ----------------------------------- #
# LISTING STATUS
# ----------------------------------- #
# date  = date to return listing status (anything after 2010-01-01)
# state = By default, state=active and the API will return a list of actively traded stocks and ETFs.
#         Set state=delisted to query a list of delisted assets.

# OUTPUT - csv

x = sprintf("https://www.alphavantage.co/query?function=LISTING_STATUS&date=%s&state=%s&apikey=%s",
            date,
            state,
            api)
# ----------------------------------- #


# ----------------------------------- #
# EARNINGS CALENDAR
# ----------------------------------- #
# symbol = By default, no symbol will be set for this API. When no symbol is
# set, the API endpoint will return the full list of company earnings scheduled.
# If a symbol is set, the API endpoint will return the expected earnings for
# that specific symbol. For example, symbol=IBM

# horizon = By default, horizon=3month and the API will return a list of
# expected company earnings in the next 3 months. You may set horizon=6month
# or horizon=12month to query the earnings scheduled for the next 6 months or
# 12 months, respectively.

# OUTPUT - csv

horizon = "12month" # options include : 3month, 6month, 12month
symbol  = "TSLA"

if(exists("symbol")){
  x = sprintf("https://www.alphavantage.co/query?function=EARNINGS_CALENDAR&symbol=%s&horizon=%s&apikey=%s",
          symbol,
          horizon,
          api_key)
} else{
  x = sprintf("https://www.alphavantage.co/query?function=EARNINGS_CALENDAR&horizon=%s&apikey=%s",
              horizon,
              api_key)
}

d = readr::read_csv(url(x))

# ----------------------------------- #



# ----------------------------------- #
# IPO Calendar
# ----------------------------------- #
# This API returns a list of IPOs expected in the next 3 months.
#
#
# API Parameters
# Required: function
#
# The API function of your choice. In this case, function=IPO_CALENDAR
#
# Required: apikey

x = sprintf("https://www.alphavantage.co/query?function=IPO_CALENDAR&apikey=%s",
            api_key)

# ----------------------------------- #














library(jsonlite)
doc = fromJSON(x)
doc2 = doc %>% as.data.frame

json_file <- lapply(doc, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

doc2 = do.call("rbind", json_file)

