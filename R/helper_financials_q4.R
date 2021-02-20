# financials_q4

# Separate Q4 results are not typically compiled and presented to the
# SEC in a standalone 10-Q filing. Rather, they are supplied, sometimes
# explicitly but oftertimes not, in a company's annual 10-K report.

# Values for all recovered variables can be constructed for Q4 by
# backing out estimates as the difference between the annual reported
# values and the sum of Q1:Q3.

# This function performs that task.


#-----------------------------------------------------------------------------#
# Toy data testing
#-----------------------------------------------------------------------------#
# dat  = data.frame("group"  = rep(2020:2019, each = 4),
#                   "Quarter"= rep(c("Annual", c(3:1)), 2),
#                   "val_a"  = c(10,3,2,2,   # a "20.Q4" target = 3 (10 - 3 - 2 - 2)
#                                14,7,1,1),  # a "19.Q4" target = 5 (14 - 7 - 1 - 1)
#                   "val_b"  = c(12,2,1,1,   # b "20.Q4" target = 8 (12 - 2 - 1 - 1)
#                                07,2,2,1),  # b "19.Q4" target = 2 (07 - 2 - 2 - 1)
#                   "val_c"  = c(16,5,3,4,   # c "20.Q4" target = 4 (16 - 5 - 3 - 4)
#                                10,6,2,1)   # c "19.Q4" target = 1 (10 - 6 - 2 - 1)
#                   )
# dat
#
# "
# Q4 Targets:
# 2020 : A = 3, B = 8, C = 4
# 2019 : A = 5, B = 2, C = 1
# "
#
#
# dat %>%
#   group_by(group) %>%
#   # summarize(val_a = val_a[1] - sum(val_a[2:4]),
#   #           val_b = val_b[1] - sum(val_b[2:4])) %>%
#   # summarize(across(3:5, function(x){x[1] - sum(x[2:4])})) %>%
#   # summarize(across(starts_with("val_"), function(x){x[1] - sum(x[2:4])})) %>%
#   summarize(across(!matches(c("Quarter")), function(x){x[1] - sum(x[2:4])})) %>%
#   ungroup() %>%
#   mutate(Quarter = "4") %>%
#   bind_rows(., dat) %>%
#   select(group, Quarter, everything()) %>%
#   arrange(desc(group), desc(Quarter))
#-----------------------------------------------------------------------------#
# "
# I CAN implement this in the do all wrapper for financials_sec
#
# Error handling. Need minimum # units to be 4 (annual + 3) for all obs in group across
# all groups
# "
# dat %>% group_by(group) %>% mutate(tst = n()) %>% pull(tst) %>% (function(x){min(x)})
#
#
# if(any(is.na(dat[, c(3,4)]))){
#   stop("Missing values in quarterly variables - accurate Q4 results cannot be extracted.")
# }
#
# res2 <- res %>% mutate(Quarter = case_when(Quarter == 4 ~ "Annual", TRUE ~ as.character(Quarter)))
#
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
#
#
#
#
#
#
# financials_q4 = function(x){
#
# }





