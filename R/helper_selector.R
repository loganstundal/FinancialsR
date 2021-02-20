# selector()

# This function extracts variables from the XBRL xml financial filing
# pulled from the SEC. This function is a secondary helper function
# not intended for independent usage but is implemented in various
# locations including the financial_sec() and extract_vals() functions.

# ----------------------------------- #
# selector() function to extract facts
# ----------------------------------- #
selector <- function(data,
                     x,
                     numeric          = FALSE,
                     second_condition = NULL){
  if(is.null(second_condition)){
    tryCatch(
      expr  = {
        val = data[data$elementId == x,]
        if(nrow(val) > 1 & numeric){
          val = val %>% filter(is.na(dimension1)) %>% slice(1) %>% pull(fact)
          if(length(val) == 0 & numeric){
            val = NA_integer_
          } else if(length(val) == 0 & !numeric){
            val = NA_character_
          }
        } else if(nrow(val) > 1 & !numeric){
          val = val$fact[1]
        } else if(nrow(val) == 1){
          val = val$fact
        } else{
          val = ifelse(numeric, NA_integer_, NA_character_)
        }
      },
      error = function(e){val = ifelse(numeric, NA_integer_, NA_character_)}
    )
  }

  # else{
  #   filters = list(second_condition)
  #
  #   data = data %>% filter(elementId == x,
  #                        !!!filters)
  #   tryCatch(
  #     expr  = {val = data[data$elementId == x,]$fact[1]},
  #     error = {function(e) return(val = ifelse(numeric, NA_integer_, NA_character_))}
  #   )
  # }

  if(numeric & !is.na(val)){
    tryCatch(
      expr    = {val = as.numeric(val)},
      error   = function(e){val},
      warning = function(w){val}
    )
  }
  return(val)
}
# Note - "second_condition" input as character (ex., "is.na(variable)")
# ----------------------------------- #
