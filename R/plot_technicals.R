#' Generates a plot of technical data from a company's stock.
#'
#' \code{plot_technicals} Returns a plot of company stock price along
#' with optional data frame of underling price data.
#'
#' @param symbol Company stock ticker as string.
#'
#' @param from,to Start and end date for stock prices as strings in format:
#' "YYYY-MM-DD". Note, difference between the start date and end date must be
#' at least 27 trading days in order to calculate the 26-day EMA. Periods
#' shorter than this will produce an error in which case use an earlier start
#' date. Default values are "2018-01-01" for "from" and today for "to.
#'
#' @param typical_price True or False. Incidator to use daily typical price
#' defined as "(High + Low + Close) /3; default is False which uses daily
#' closing price.
#'
#' @param return_stock_price True or False. Indicator to return underlying
#' stock price data as a tidied data frame.
#'
#' @param show_plot True or False. Indicator to show plot.
#'
#' @param theme Setting for plot theme. Options include "dark", the default, or "bright".
#'
#' @return Returns a ggplot object of stock price, EMA, MACD. Optionally returns
#' a data frame consisting of underlying stock price data.
#'
#' @examples
#' plot_technicals("TSLA"
#' plot_technicals("TSLA", from = as.character(today() - 150))



plot_technicals <- function(symbol,
                       from = NULL,
                       to   = NULL,
                       theme = "dark",
                       typical_price      = FALSE,
                       return_stock_price = FALSE,
                       show_plot          = TRUE){

  # ----------------------------------- #
  # Extract stock price from yahoo data using tidyquant
  # ----------------------------------- #
  if(is.null(from)){
    from = "2018-01-01"
  }
  if(is.null(to)){
    to   = as.character(today())
  }

  stock = tq_get(x    = symbol,
                 get  = "stock.prices",
                 from = from,
                 to   = to)

  if(nrow(stock) < 27){
    stop("Insufficient time difference to calculate EMA 26. Choose an earlier start date or use default.")
  }
  # ----------------------------------- #


  # ----------------------------------- #
  # Set plot theme
  # ----------------------------------- #
  if(theme == "dark"){
    my_theme = theme(plot.background   = element_rect(fill = "#252525"),
                     panel.background  = element_rect(fill = "#252525"),
                     legend.background = element_rect(fill = "#252525"),
                     legend.key        = element_rect(fill = "#252525"),
                     legend.position   = "right",
                     legend.direction  = "vertical",
                     text              = element_text(color = "#999999", size = 12),
                     axis.title        = element_blank(),
                     axis.ticks        = element_blank(),
                     legend.title      = element_blank(),
                     panel.grid        = element_blank(),
                     panel.grid.major.y= element_line(linetype = "dashed", color = "#3a4747", size = 0.5))
    rsi_color = "gray80"
    hist_alpha = 0.5
  } else if(theme == "light"){
    my_theme = theme(plot.background   = element_rect(fill = "white"),
                     panel.background  = element_rect(fill = "white"),
                     legend.background = element_rect(fill = "white"),
                     legend.key        = element_rect(fill = "white"),
                     legend.position   = "right",
                     legend.direction  = "vertical",
                     text              = element_text(color = "black", size = 12),
                     axis.title        = element_blank(),
                     axis.ticks        = element_blank(),
                     legend.title      = element_blank(),
                     panel.grid        = element_blank(),
                     panel.grid.major.y= element_line(linetype = "dashed", color = "gray70", size = 0.5))
    rsi_color = "gray40"
    hist_alpha = 0.7
  } else{
    stop("Theme choice not one of: 'dark' or 'light'.")
  }

  # ----------------------------------- #


  # ----------------------------------- #
  # Tidy data for plot
  # ----------------------------------- #
  if(typical_price){
    stock = stock %>% mutate(value = (open + high + close) / 3)
  } else{
    stock = stock %>% mutate(value = close)
  }

  stock <- stock %>%
    mutate(
      x      = 1:nrow(.),
      EMA_12 = ema(value, n = 12),
      EMA_26 = ema(value, n = 26),
      RSI    = rsi(value, n = 20),
      month_yr = paste(month(date), year(date), sep = "-")
    ) %>%
    as_tibble() %>%
    mutate(MACD    = EMA_12 - EMA_26) %>%
    mutate(MACD_09 = c(rep(NA, 26-1),
                       ema(na.omit(MACD), 9))) %>%
    mutate(hist_vals = MACD - MACD_09)
  # ----------------------------------- #


  # ----------------------------------- #
  # Format dates
  # ----------------------------------- #
  tst = length(unique(stock$month_yr))

  if(tst > 24){
    stock = stock %>%
      mutate(year = year(ymd(date))) %>%
      group_by(year) %>%
      mutate(brk = case_when(date == date[1] ~ x, TRUE ~ NA_integer_)) %>%
      mutate(label = case_when(!is.na(brk) ~ format(ymd(date), "%Y")))
  } else{
    stock = stock %>%
      group_by(month_yr) %>%
      mutate(brk = case_when(date == date[1] ~ x, TRUE ~ NA_integer_)) %>%
      mutate(label = case_when(!is.na(brk) ~ format(ymd(date), "%b")))
  }
  # ----------------------------------- #


  # ----------------------------------- #
  # Build plots
  # ----------------------------------- #
  p1 = ggplot(data = stock, aes(x = x)) +
    geom_line(aes(y = value,  color = "Stock Price")) +
    geom_line(aes(y = EMA_12, color = "EMA12")) +
    geom_line(aes(y = EMA_26, color = "EMA26")) +
    labs(title = sprintf("Stock: %s", symbol))  +
    my_theme +
    theme(axis.text.x = element_blank())

  p2 = ggplot(data = stock, aes(x = x)) +
    geom_col(position = "identity", aes(y = hist_vals, fill = hist_vals), alpha = hist_alpha) +
    geom_line(aes(y = MACD,    color = "MACD")) +
    geom_line(aes(y = MACD_09, color = "Signal Line")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
    scale_fill_gradient2(low = scales::muted("red"), high = scales::muted("green"), midpoint = 0, guide = FALSE) +
    my_theme +
    theme(axis.text.x = element_blank())

  p3 = ggplot(data = stock, aes(x = x)) +
    geom_line(aes(y = RSI, color = "RSI")) +
    scale_color_manual(name = "", values = rsi_color) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", color = "black") +
    my_theme +
    scale_x_continuous(breaks = stock$brk,
                       labels = stock$label) +
    theme(axis.text.x = element_text(angle = 45))

  plt = suppressWarnings({
    plot_grid(p1, p2, p3, align = "v", nrow = 3, rel_heights = c(3,1,1), axis = "b")
  })
  # ----------------------------------- #


  # ----------------------------------- #
  # Return statements
  # ----------------------------------- #
  if(show_plot){
    print(plt)
  }

  if(return_stock_price){
    return(list("Plot" = plt,
                "Data" = stock))
  } else{
    return(plt)
  }
  # ----------------------------------- #
}

