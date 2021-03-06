#' Conversion from a numeric percentage to a French formatted label.
#'
#' @param x A numeric vector.
#' @param digits Integer indicating the number of decimal places. Set to NULL to skip round.
#' @param sign Display of "+" and "-".
#' @param suffix Suffix after the percent value.
#'
#' @return A French formatted label.
#'
#' @examples
#' caractr::str_percent_fr(0.1)
#'
#' @export
str_percent_fr <- function(x, digits = 0, sign = FALSE, suffix = TRUE) {

  percent <- x * 100

  if (!is.null(digits)) {
    percent <- round(percent, digits)
  }

  if (sign == TRUE) {
    percent <- stringr::str_c(ifelse(percent > 0, "+", ""), percent)
  }

  if (suffix == TRUE) {
    percent <- stringr::str_c(percent, "\u202F%", sep = "")
  }

  percent <- stringr::str_replace(percent, "\\.", ",")

  return(percent)
}

#' Conversion from a number to a French formatted number.
#'
#' Includes comma as decimal mark and non-breaking space between thousands.
#'
#' @param x A numeric vector.
#'
#' @return A French formatted number.
#'
#' @examples
#' caractr::str_number_fr(1854.2)
#'
#' @export
str_number_fr <- function(x) {

  `Encoding<-`(gsub("-", "\u202F", format(x, decimal.mark = ",", big.mark = "-")), "UTF-8")

}
