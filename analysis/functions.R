# Loads functions

# function to make first letter of a string uppercase
firstUp <- function(x) {
  x <- as.character(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

# define a "not in" operator
`%notin%` <- Negate(`%in%`)

# function to calculate Wilson confidence intervals on single proportions (using prop.test) then output them as a string with square brackets
propCI <- function(successes, total, percent = F, string = T, round = T) {
  CIs <- prop.test(successes,total)$conf.int # compute CIs
  if(round==T){
    CIs <- round(CIs,2)
  }
  if(percent==T){
    CIs <- CIs*100
  }
  if(string==T){
    CIs <- paste0('CI [', CIs[1], ',', CIs[2], ']')
  }
  return(CIs)
}

# function to calculate confidence intervals for multinomial proportions
# the function is copied directly from the MultinomCI from the DescTools package
# Description: Confidence intervals for multinomial proportions are often approximated by single binomial confidence intervals, which might in practice often yield satisfying results, but is properly speaking not correct. This function calculates simultaneous confidence intervals for multinomial proportions either according to the methods of Sison and Glaz, Goodman, Wald, Wald with continuity correction or Wilson.
# x is a vector of positive integers representing the number of occurrences of each class. The total number of samples equals the sum of such elements.
multiPropCI <- function (
  x, conf.level = 0.95, sides = c("two.sided", "left", 
                                          "right"), method = c("sisonglaz", "cplus1", "goodman", "wald", 
                                                               "waldcc", "wilson")){
  .moments <- function(c, lambda) {
    a <- lambda + c
    b <- lambda - c
    if (b < 0) 
      b <- 0
    if (b > 0) 
      den <- ppois(a, lambda) - ppois(b - 1, lambda)
    if (b == 0) 
      den <- ppois(a, lambda)
    mu <- mat.or.vec(4, 1)
    mom <- mat.or.vec(5, 1)
    for (r in 1:4) {
      poisA <- 0
      poisB <- 0
      if ((a - r) >= 0) {
        poisA <- ppois(a, lambda) - ppois(a - r, lambda)
      }
      if ((a - r) < 0) {
        poisA <- ppois(a, lambda)
      }
      if ((b - r - 1) >= 0) {
        poisB <- ppois(b - 1, lambda) - ppois(b - r - 
                                                1, lambda)
      }
      if ((b - r - 1) < 0 && (b - 1) >= 0) {
        poisB <- ppois(b - 1, lambda)
      }
      if ((b - r - 1) < 0 && (b - 1) < 0) {
        poisB <- 0
      }
      mu[r] <- (lambda^r) * (1 - (poisA - poisB)/den)
    }
    mom[1] <- mu[1]
    mom[2] <- mu[2] + mu[1] - mu[1]^2
    mom[3] <- mu[3] + mu[2] * (3 - 3 * mu[1]) + (mu[1] - 
                                                   3 * mu[1]^2 + 2 * mu[1]^3)
    mom[4] <- mu[4] + mu[3] * (6 - 4 * mu[1]) + mu[2] * (7 - 
                                                           12 * mu[1] + 6 * mu[1]^2) + mu[1] - 4 * mu[1]^2 + 
      6 * mu[1]^3 - 3 * mu[1]^4
    mom[5] <- den
    return(mom)
  }
  .truncpoi <- function(c, x, n, k) {
    m <- matrix(0, k, 5)
    for (i in 1L:k) {
      lambda <- x[i]
      mom <- .moments(c, lambda)
      for (j in 1L:5L) {
        m[i, j] <- mom[j]
      }
    }
    for (i in 1L:k) {
      m[i, 4] <- m[i, 4] - 3 * m[i, 2]^2
    }
    s <- colSums(m)
    s1 <- s[1]
    s2 <- s[2]
    s3 <- s[3]
    s4 <- s[4]
    probn <- 1/(ppois(n, n) - ppois(n - 1, n))
    z <- (n - s1)/sqrt(s2)
    g1 <- s3/(s2^(3/2))
    g2 <- s4/(s2^2)
    poly <- 1 + g1 * (z^3 - 3 * z)/6 + g2 * (z^4 - 6 * z^2 + 
                                               3)/24
    +g1^2 * (z^6 - 15 * z^4 + 45 * z^2 - 15)/72
    f <- poly * exp(-z^2/2)/(sqrt(2) * gamma(0.5))
    probx <- 1
    for (i in 1L:k) {
      probx <- probx * m[i, 5]
    }
    return(probn * probx * f/sqrt(s2))
  }
  n <- sum(x, na.rm = TRUE)
  k <- length(x)
  p <- x/n
  if (missing(method)) 
    method <- "sisonglaz"
  if (missing(sides)) 
    sides <- "two.sided"
  sides <- match.arg(sides, choices = c("two.sided", "left", 
                                        "right"), several.ok = FALSE)
  if (sides != "two.sided") 
    conf.level <- 1 - 2 * (1 - conf.level)
  method <- match.arg(arg = method, choices = c("sisonglaz", 
                                                "cplus1", "goodman", "wald", "waldcc", "wilson"))
  if (method == "goodman") {
    q.chi <- qchisq(conf.level, k - 1)
    lci <- (q.chi + 2 * x - sqrt(q.chi * (q.chi + 4 * x * 
                                            (n - x)/n)))/(2 * (n + q.chi))
    uci <- (q.chi + 2 * x + sqrt(q.chi * (q.chi + 4 * x * 
                                            (n - x)/n)))/(2 * (n + q.chi))
    res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                               uci))
  }
  else if (method == "wald") {
    q.chi <- qchisq(conf.level, 1)
    lci <- p - sqrt(q.chi * p * (1 - p)/n)
    uci <- p + sqrt(q.chi * p * (1 - p)/n)
    res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                               uci))
  }
  else if (method == "waldcc") {
    q.chi <- qchisq(conf.level, 1)
    lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
    uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
    res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                               uci))
  }
  else if (method == "wilson") {
    q.chi <- qchisq(conf.level, 1)
    lci <- (q.chi + 2 * x - sqrt(q.chi^2 + 4 * x * q.chi * 
                                   (1 - p)))/(2 * (q.chi + n))
    uci <- (q.chi + 2 * x + sqrt(q.chi^2 + 4 * x * q.chi * 
                                   (1 - p)))/(2 * (q.chi + n))
    res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                               uci))
  }
  else {
    const <- 0
    pold <- 0
    for (cc in 1:n) {
      poi <- .truncpoi(cc, x, n, k)
      if (poi > conf.level && pold < conf.level) {
        const <- cc
        break
      }
      pold <- poi
    }
    delta <- (conf.level - pold)/(poi - pold)
    const <- const - 1
    if (method == "sisonglaz") {
      res <- cbind(est = p, lwr.ci = pmax(0, p - const/n), 
                   upr.ci = pmin(1, p + const/n + 2 * delta/n))
    }
    else if (method == "cplus1") {
      res <- cbind(est = p, lwr.ci = pmax(0, p - const/n - 
                                            1/n), upr.ci = pmin(1, p + const/n + 1/n))
    }
  }
  if (sides == "left") 
    res[3] <- 1
  else if (sides == "right") 
    res[2] <- 0
  return(res)
}