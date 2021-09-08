# Loads functions

# function to make first letter of a string uppercase
firstUp <- function(x) {
  x <- as.character(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

# define a "not in" operator
`%notin%` <- Negate(`%in%`)