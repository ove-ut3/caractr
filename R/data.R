#' A conversion table between numbers and their litteral forms in French.
#'
#' @format A data frame with 1127 rows and 7 variables:
#' \describe{
#'   \item{x}{number, as integer}
#'   \item{letter}{number in its masculine litteral form}
#'   \item{letter_f}{number in its feminine litteral form}
#'   \item{ieme}{ordinal number in its masculine litteral form}
#'   \item{ieme_f}{ordinal number in its feminine litteral form}
#'   \item{ieme_number}{ordinal number in its masculine form}
#'   \item{ieme_number_f}{ordinal number in its feminine form}
#' }
"number_letter"

#' A list of stop words.
#'
#' @format A data frame with 145 rows and 2 variables:
#' \describe{
#'   \item{language}{language code (ISO 639-1)}
#'   \item{stopword}{stop word}
#' }
"stopwords"

#' A conversion table between non accentued words and accentuated words in French.
#'
#' @format A data frame with 13696 rows and 2 variables:
#' \describe{
#'   \item{word}{non accentued word}
#'   \item{word_fr_accent}{accentued word}
#' }
"word_fr_accent"
