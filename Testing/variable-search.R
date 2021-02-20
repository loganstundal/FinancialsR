

"
This script just helps me to search for financial concepts in a returned XBRL xml
financial filing pulled from the SEC.

The code below takes as its only input the 'facts' dataframe generated in the
financials_sec.R script.

"

# ----------------------------------- #
# Searching for appropriate vars - DELETE LATER
vals = sort(cbind(unique(facts$elementId)))
# vals = vals[str_starts(vals, "us-gaap")]             - IF - financials bound to company indicator (eg., tsla_xyz) can easily splice this.
vals = sort(cbind(facts$elementId))

# cat("\14");vals[str_detect(str_to_lower(vals), "earn")]

# key = "operating+expense"
key = "cash"

keys = vals[str_detect(str_to_lower(vals), key)]
keys = keys[!str_detect(keys, pattern = "Text")]

tst = facts

cat("\14");cbind(
  Var = tst[tst$elementId %in% keys,]$elementId,
  Val = as.numeric(tst[tst$elementId %in% keys,]$fact) / 1e6
)

# if all else fails
full = as.data.frame(cbind(
  Var = tst$elementId,
  Val = tst$fact
))

full <- full %>%
  filter(!str_detect(Var, "Text"),
         !str_length(Var) >= 70)

full[str_detect(full$Val, "10435"),]

# ----------------------------------- #


x[is.na(x)]
length(x[is.na(x)])
