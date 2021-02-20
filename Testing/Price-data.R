

rm(list=ls())

library(ggplot2, quietly = T)
library(QuantTools)
library(lubridate, quietly = T)
library(quantmod, quietly = T, warn.conflicts = F)
library(magrittr)
library(dplyr)

# user_date = ymd("2020-12-31")
# user_52pr = user_date - 365
#
#
# symbol <- "TSLA"
# from   <- user_52pr
# # to     <- user_date
# to     <- as.character(ymd(today()))

# Via QuantTools - does not account for stock-splits!!!!
# stock  <- get_finam_data(symbol = symbol,
#                          from   = from,
#                          to     = to,
#                          period = 'day',
#                          split.adjusted = TRUE)

# Via quantmod - MUCH BETTER
stock <- getSymbols(Symbols = "tsla",
                    src = "yahoo",
                    from = (today() - 365),
                    to = today(),
                    auto.assign = F)

colnames(stock) <- gsub("^.*\\.", "", colnames(stock))
# stock2 <- stock %>%
#   as.data.frame() %>%
#   rownames_to_column("date") %>%
#   rename_all(., ~str_remove(., "TSLA.")) %>%
#   mutate(week_of  = floor_date(ymd(date), unit = 'week', week_start = getOption("lubridate.week.start", 1)),
#          volume_l = log(Volume),
#          x        = 1:n(),
#          Month    = month(date)) %>%
#   # mutate(breaks   = case_when(date == week_of ~ x, TRUE ~ NA_integer_)) %>%
#   group_by(Month) %>%
#   mutate(breaks = case_when(date == date[1] ~ x, TRUE ~ NA_integer_))
# head(stock2)


# Uese quantmod, but this wrapper what - provides tidier data?
# https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html
# library(BatchGetSymbols)
# tsla <- get.clean.data('TSLA',
#                            first.date = as.Date('2020-12-01'),
#                            last.date = as.Date('2020-12-31'))

# ----------------------------------- #
# Tidy stock data
# ----------------------------------- #
# Regular trading hours for the NYSE are:
# 9:30 a.m. to 4 p.m (16:00)

# HALF-HOURLY DATA
# stock <- stock %>%
#   mutate(time = with_tz(ymd_hms(time, tz = "UTC"), tz = "US/Eastern")) %>%
#   mutate(date = date(time),
#          volume_l = log(volume)) %>%
#   filter(hms::as_hms(time) >= hms("09:30:00") & hms::as_hms(time) <= hms("16:00:00")) %>%
#   mutate(x        = 1:n(),
#          breaks   = case_when(hour(time) == 9 ~ x, TRUE ~ NA_integer_))

# WEEKLY DATA
# stock <- stock %>%
#   mutate(week_of  = floor_date(date, unit = 'week', week_start = getOption("lubridate.week.start", 1)),
#          volume_l = log(volume),
#          x        = 1:n()) %>%
#   mutate(breaks   = case_when(date == week_of ~ x, TRUE ~ NA_integer_))


# ----------------------------------- #

#-----------------------------------------------------------------------------#
# POSSIBLE INDICATORS TO RETURN TO FINANCIALSR                            ----
#-----------------------------------------------------------------------------#

# ASSUMES - subset made on quarter
stock3 <- stock2 %>%
  mutate(date = ymd(date)) %>%
  filter(date >= ymd("2020-10-01"),
         date <= ymd("2020-12-31"))

# Going to use closing values for consistency
vals = with(stock3, list(
stock_Q_high   = max(Close),
stock_Q_low    = min(Close),
stock_Q_spread = max(Close) - min(Close),
stock_Q_sd     = sd(Close),
stock_Q_mean   = mean(Close)
))

"Q4
$stock_Q_high
[1] 705.67

$stock_Q_low
[1] 387.23

$stock_Q_spread
[1] 318.44

$stock_Q_sd
[1] 102.8641

$stock_Q_mean
[1] 513.4097
"
#-----------------------------------------------------------------------------#
chartSeries(stock, TA = c(addBBands(), addVo()))

# get options
tsla_opt = getOptionChain("TSLA", Exp = "2022-01-21")
tsla_opt$puts

tsla_opt$puts[tsla_opt$puts$Strike == 320,]
q.model = specifyModel(Next(OpCl(stock)) ~ Lag(OpHi(stock),0:3))

from
ymd(to) - 30

mod = buildModel(q.model,method='lm',training.per=c(from,
                                              ymd(to) - 30))
summary(mod)


# install.packages("smooth")
# library(smooth)
# x = sma(stock$TSLA.Close)

# ----------------------------------- #
# SIMPLE MOVING AVERAGE
# ----------------------------------- #
x7 = rollmean(stock$TSLA.Close,k = 7)
x5 = rollmean(stock$TSLA.Close,k = 5)
x3 = rollmean(stock$TSLA.Close,k = 3)

# DEATH CROSS TECHNICAL INDICATOR : 50-day SMA crosses below the 200-day SMA.
# This is considered a bearish signal, that further losses are in store.

# The golden cross occurs when a short-term SMA breaks above a long-term SMA.
# Reinforced by high trading volumes, this can signal further gains are in store.
x50 = rollmean(stock$TSLA.Close, k = 50)
x200 = rollmean(stock$TSLA.Close, k = 200)


d = cbind("x"     = (1:length(stock$Close)),
          "data"  = stock$Close,
          "pred7" = c(rep(NA,6), x7$Close),
          "pred5" = c(rep(NA,4), x5$Close),
          "pred3" = c(rep(NA,2), x3$Close),
          "pred50" = c(rep(NA, 49), x50$Close),
          "pred200" = c(rep(NA, 199), x200$Close))

ggplot(data = d, aes(x=x)) +
  geom_line(aes(y = Close, color = "Stock")) +
  # geom_line(aes(y = pred7, color = "7-MA")) +
  # geom_line(aes(y = pred5, color = "5-MA")) +
  # geom_line(aes(y = pred3, color = "3-MA"))
  geom_line(aes(y = pred200, color = "200-MA")) +
  geom_line(aes(y = pred50, color = "50-MA"))

# ----------------------------------- #

# ----------------------------------- #
# EXPONENTIAL MOVING AVERAGE
# ----------------------------------- #
# Buy signal arises when a short-run EMA crosses from below to above a long-run EMA.
#
# Sell signal arrises when a short-run EMA crosses from above to above a long-run EMA.
# https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/exponential-moving-average-ema.html


# ACTUALLY - quantmods has it - one less package now.
d = cbind("x"       = (1:length(stock$Close)),
          "data"    = stock$Close,
          "pred7"   = ema(stock$Close, n = 7),
          "pred5"   = ema(stock$Close, n = 5),
          "pred3"   = ema(stock$Close, n = 3),
          "pred50"  = ema(stock$Close, n = 50),
          "pred200" = ema(stock$Close, n = 200))

ggplot(data = d, aes(x=x)) +
  geom_line(aes(y = Close, color = "Stock")) +
  # geom_line(aes(y = pred7, color = "7-MA")) +
  # geom_line(aes(y = pred5, color = "5-MA")) +
  # geom_line(aes(y = pred3, color = "3-MA"))
  geom_line(aes(y = pred50, color = "200-MA")) +
  geom_line(aes(y = pred200, color = "50-MA"))

# ----------------------------------- #



# ----------------------------------- #
# Bollinger Bands
# ----------------------------------- #

# Typically use a 20-day sma
# https://www.investopedia.com/terms/b/bollingerbands.asp


d = cbind(x = 1:length(stock$TSLA.Close),
          stock = stock$TSLA.Close,
          TP    = with(stock, mean(TSLA.High + TSLA.Low + TSLA.Close)),
          sma20 = sma(stock$TSLA.Close, n = 20))

# roll_sd(stock$TSLA.Close, 20)

d = d %>% as.data.frame() %>%
  mutate(bblb = sma20 - (2 * roll_sd(stock$TSLA.Close, n = 20)),
         bbub = sma20 + (2 * roll_sd(stock$TSLA.Close, n = 20)))

ggplot(data = d, aes(x = x, y  = TSLA.Close)) +
  geom_ribbon(aes(ymin = bblb, ymax = bbub, fill = "Bollinger\nBand"), alpha = 0.3) +
  geom_line(aes(y = sma20, color = "20-Day MA")) +
  geom_line(aes(color = "Stock Price"))

# with quantmod
chartSeries(stock, TA = c(addBBands(), addRSI()))
# ----------------------------------- #



# ----------------------------------- #
# RELATIVE STRENGTH INDEX
# ----------------------------------- #
# Traditional interpretation and usage of the RSI are that values of 70 or
# above indicate that a security is becoming overbought or overvalued and
# may be primed for a trend reversal or corrective pullback in price. An
# RSI reading of 30 or below indicates an oversold or undervalued condition.


library(QuantTools)
x = rsi(stock$TSLA.Close, n = 20)

# With quantmod
# stock$Close = stock$TSLA.Close
chartSeries(stock, TA = c(addRSI()))
# ----------------------------------- #


# ----------------------------------- #
# MOVING AVERAGE CONVERGENCE-DIVERGENCE (MACD)
# ----------------------------------- #
# https://www.investopedia.com/terms/m/macd.asp

# Moving average convergence divergence (MACD) is a trend-following momentum
# indicator that shows the relationship between two moving averages of a
# security’s price. The MACD is calculated by subtracting the 26-period
# exponential moving average (EMA) from the 12-period EMA.

# The result of that calculation is the MACD line. A nine-day EMA of the MACD
# called the "signal line," is then plotted on top of the MACD line, which can
# function as a trigger for buy and sell signals. Traders may buy the security
# when the MACD crosses above its signal line and sell—or short—the security
# when the MACD crosses below the signal line.

d <- data.frame(
  "x"      = 1:nrow(stock),
  "stock"  = as.numeric(stock$Close),
  "EMA_12" = ema(stock$Close, n = 12),
  "EMA_26" = ema(stock$Close, n = 26),
  "RSI"    = rsi(stock$Close, n = 20)
) %>% as_tibble()

# MACD LINE
d$MACD = d$EMA_12 - d$EMA_26

# SIGNAL LINE
d$MACD_09 = c(rep(NA, 26-1),
              ema(na.omit(d$MACD), 9))


# MACD is often displayed with a histogram (see the chart below) which graphs
# the distance between the MACD and its signal line. If the MACD is above the
# signal line, the histogram will be above the MACD’s baseline. If the MACD is
# below its signal line, the histogram will be below the MACD’s baseline.
# Traders use the MACD’s histogram to identify when bullish or bearish momentum
# is high.

d$hist_vals = d$MACD - d$MACD_09
d$hist_cols = ifelse(d$hist_vals>0, "green","red")


# With quantmod
# This ignores the 12 and 26 day ema... can't do all at same time
    # chartSeries(stock$TSLA.Close)
    # chartSeries(stock$TSLA.Close, TA = c(addEMA(n = 12), addEMA(n = 26)))
    # chartSeries(stock$TSLA.Close, TA = c(addMACD()))

# using gglplot...
library(cowplot)
p1 = ggplot(data = d, aes(x = x)) +
  geom_line(aes(y = stock,  color = "Stock Price")) +
  geom_line(aes(y = EMA_12, color = "EMA12")) +
  geom_line(aes(y = EMA_26, color = "EMA26")) +
  theme_minimal_hgrid(12) +
  theme(legend.title = element_blank())

p2 = ggplot(data = d, aes(x = x)) +
  geom_col(position = "identity", aes(y = hist_vals, fill = hist_cols), alpha = 0.5) +
  geom_line(aes(y = MACD,    color = "MACD")) +
  geom_line(aes(y = MACD_09, color = "Signal Line")) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
  # scale_fill_gradient2(low = "red", high = "green", mid = "gray40", midpoint = 0) +
  # scale_fill_gradientn(colors = c("red","green")) +
  # scale_fill_manual(values = c(scales::muted("green"), scales::muted("red")), guide = FALSE) +
  scale_fill_manual(values = c(scales::muted("seagreen"), scales::muted("pink")), guide = FALSE) +
  theme_minimal_hgrid(12) +
  theme(panel.grid = element_line(linetype = "dotted", color = "gray80", size = 0.5),
        legend.title = element_blank())

p3 = ggplot(data = d, aes(x = x)) +
  geom_line(aes(y = RSI, color = "RSI")) +
  scale_color_manual(name = "", values = "black") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
  theme_minimal_hgrid(12) +
  theme(panel.grid = element_line(linetype = "dotted", color = "gray80", size = 0.5),
        legend.title = element_blank())

plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(3,1,1))


# The MACD has a positive value (orange line in the lower chart) whenever the
# 12-period EMA (orange line on the upper chart) is above the 26-period EMA
# (light green line in the upper chart) and a negative value when the 12-period
# EMA is below the 26-period EMA. The more distant the MACD is above or below
# its baseline indicates that the distance between the two EMAs is growing.
# ----------------------------------- #



# ----------------------------------- #
#
# ----------------------------------- #

# ----------------------------------- #

#-----------------------------------------------------------------------------#
# TIDYQUANT - CAN PULL THE FULL SP500...
#-----------------------------------------------------------------------------#
x = tidyquant::tq_get("TSLA")

# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ00-introduction-to-tidyquant.html#minutes-to-tidyquant

sp500 <- tidyquant::tq_index("SP500") %>%
  tidyquant::tq_get(get = "stock.prices")

sort(unique(sp500$company))

rm(list=ls())
#-----------------------------------------------------------------------------#





#-----------------------------------------------------------------------------#

# VISUALIZATION ---------------------------------------------------------------

# WEEKLY
ggplot(data = stock2, aes(x = x, y = Close)) +
  geom_line() +
  scale_x_continuous(breaks = stock2$breaks,
                     labels = format(stock2$week_of, "%b-%d")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", size = 0.5, color = "gray75"),
        axis.title = element_blank()) +
  labs(title = sprintf("%s: Closing Stock Price", symbol),
       subtitle = sprintf("%s : %s", from, to))






# HALF-HOURLY
      # ggplot(data = d %>% filter(date == ymd("2021-02-18")), aes(x = x, y = close)) +
      #   geom_line() +
      #   geom_vline(aes(xintercept = breaks), linetype = "dashed", color = "gray85", size = 0.5) +
      #   scale_x_continuous(breaks = d$breaks,
      #                      labels = d$time) +
        # theme_minimal() +
        # theme(panel.grid = element_blank(),
        #       panel.grid.major.y = element_line(linetype = "dotted", size = 0.5, color = "gray75"),
        #       axis.title = element_blank()) +
        # labs(title = sprintf("%s: Closing Stock Price", symbol),
        #      subtitle = sprintf("%s : %s", from, to))







#-----------------------------------------------------------------------------#
# FUTURE TO DOs

# WANT: TREND (REGIME) ANALYSIS [declining, stable, rising - see python pdf in docs.]
