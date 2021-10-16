# Loads functions

# function to make first letter of a string uppercase
firstUp <- function(x) {
  x <- as.character(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

# define a "not in" operator
`%notin%` <- Negate(`%in%`)

# function to calculate 95% Wilson CIs on single proportions (using prop.test) then output them in a string with a point estimate as percentages
percentCI <- function(successes, total) {
  percent <- round((successes/total)*100,0)
  CI <- round(prop.test(successes,total)$conf.int*100, 0)
  CIstring <- paste0(percent,'%, CI [', CI[1], ',', CI[2], ']')
  return(CIstring)
}