# financials_alpha()

# financials_alpha scrapes financial data from seekingalpha.com Use nicely.


# financials_alpha <- function(x){
#
#
#
# }





# require(rvest)
# require(tidyverse)



# symbol = "MSFT"
# time   = "quarterly"         # can also be: "annualy"
# type   = "income-statement"  # can also be: "balance-sheet","cash-flow-statement"
#
# target = sprintf("https://seekingalpha.com/symbol/%s/%s#figure_type=%s#order_type=latest_left",
#                  symbol, type, time)
#
# # "https://seekingalpha.com/symbol/MSFT/balance-sheet#figure_type=quarterly#order_type=latest_left"
# # "https://seekingalpha.com/symbol/MSFT/income-statement#figure_type=quarterly#order_type=latest_left"
#
#
# require(httr)
#
# # x <- GET(target, add_headers(c("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36",
# #                                "X-Requested-With" = "XMLHttpRequest")))
# # https://stackoverflow.com/questions/43590153/http-error-403-forbidden-when-reading-html
#
# content <- read_html(target)
# tables <- content %>% html_nodes('#financial-export-data')
# # tables <- tables %>% html_table()
#
# # //*[@id="financial-export-data"]
#
# # /html/body/div[3]/div[1]/div/div/div/div/div[2]/div/div[2]/section/div
#
#
# page <- read_html("https://en.wikipedia.org/wiki/Julienning")
#
# # point rvest html_nodes at list elements with class = references
# # parse html in r using rvest html_text
# sources <- page %>%
#   html_nodes(".references li") %>%
#   html_text()
