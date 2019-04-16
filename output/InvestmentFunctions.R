library(dplyr)


#' @title: future_value
#' @description: compute the future value of an investment
#' @param amount initial invested amount
#' @param rate annual rate of return as decimal percentage
#' @param years number of years
#' @return future investment amount in dollars (as a numeric value)

future_value <- function(amount = 0.00 , rate = 0.00, years = 0){
  
  time_factor <- (1 + rate) ^ years
  future_value <- amount * time_factor
  
  return(future_value)
  
}

#' @title annuity
#' @description compute the future value of annuity based on amount deposited into an account
#' @param contrib the amount deposited at the end of each year
#' @param rate the annual rate of return
#' @param years time in years
#' @return the future value of annuity you will end up with as a dollar amount

annuity <- function(contrib = 0.00, rate = 0.0, years = 0){
  
  time_factor <- (1 + rate) ^ years
  quotient <- (time_factor - 1) / rate
  future_annuity <- contrib * quotient
  
  return(future_annuity)
}

#' @title growing_annuity
#' @description compute the future value of growing annuity
#' @param contrib the amount to contribute at the end of year 1
#' @param rate annual rate of return
#' @param growth annual growth rate
#' @param years number of years
#' @return future value of growing annuity in dollars

growing_annuity <- function(contrib = 0.00, rate = 0.0, growth = 0.0, years = 0){
  
  time_factor_return <- (1 + rate) ^ years
  time_factor_growth <- (1 + growth) ^ years
  dt <- rate - growth
  quotient <- (time_factor_return - time_factor_growth) / dt
  future_value <- contrib * quotient
  
  return(future_value)
  
}

