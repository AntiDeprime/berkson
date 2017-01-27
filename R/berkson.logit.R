#' Berkson's logit chi square
#'
#' 
#' @param formula Formula
#' @param data Data
#' @keywords Berkson logit 
#' @export
#' @examples
#' berkson.logit()


berkson.logit <- function (formula, data) { 
  
  # Get original data
  tidy <- as.data.frame(model.frame(formula, data)) 
  # Get variable names
  dots <- names(tidy) 
  
  freq <- tidy %>% 
                # Group by all x's
                dplyr::group_by_(.dots = dots[2:length(dots)]) %>%
                # Summarise, calculate p(y) and count n
                dplyr::summarise_ (p=c('mean(',as.character(dots[1]), ')'),
                            n = 'n()') 
  
  
  # OLAV REIERS0L
  # Linear and non-linear multiple comparisons in logit analysis
  # fix for p = 1 and p = 0 
  freq <- freq %>% 
        # p=0 replaced by 1-(1/2*n)
        dplyr::mutate (p = if_else(p==0, 1-(1/2*n), p)) %>%
        # p=1 replaced by 1/2*n
        dplyr::mutate (p = if_else(p==1, 1/2*n, p))
  

  freq <- freq %>% 
        # calculate dependent variable
        # ln (p/q)
        dplyr::mutate (lnpq = log(p/(1-p)) ) %>%
        # calculate sigma 
        # sigma^2 = 1/n*p*q
        # sigma = sqrt (sigma^2)
        dplyr::mutate (sigma = sqrt(1/(n*p*(1-p))) ) 

  # Estimate Weighted Least Squares (WLS)
  # Dependent variable: ln (p/q)
  # Independent variables: original x's
  # Weighted by 1/sigma 
  lm (as.formula(c("lnpq ~ ", as.character(formula[3]))),
                   freq, weights = 1/sigma)
}


#   # Check for varnames 
#   ILLEGAL_VAR_NAME <- 
#     is.element("p", dots) || 
#     is.element("n", dots) ||
#     is.element("lnpq", dots) ||
#     is.element("sigma", dots) 
#   ifelse(ILLEGAL_VAR_NAME==T, stop('Illegal variable name. Cannot use "n", "p", "lnpq", "sigma"'))
