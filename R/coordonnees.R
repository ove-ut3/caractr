#' Clean phone numbers.
#'
#' @param phone_number A vector of phone numbers.
#'
#' @return A vector of cleaned phone numbers.
#'
#' @examples
#' caractr::str_clean_phone_number("06.00.00.00.00")
#'
#' @export
str_clean_phone_number <- function(phone_number) {

  cleaned_phone_number <- stringr::str_replace_all(phone_number, "\n", " ")

  cleaned_phone_number <- stringr::str_remove_all(phone_number, "[^\\d]") %>%
    stringr::str_replace_all("^(00)?330?(\\d{9})", "0\\2") %>%
    { ifelse(nchar(.) == 9, paste0("0", .), .) } %>%
    { ifelse(nchar(.) == 10, stringr::str_replace(., "(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})", "\\1 \\2 \\3 \\4 \\5"), phone_number) }

  return(cleaned_phone_number)
}

#' Valider des emails par regex
#'
#' Valider des emails par regex
#'
#' @param email Un vecteur d'emails
#'
#' @return Un vecteur de booléens.
#'
#' @examples
#' caractr::valider_email_regex(c("test@test", "test@test.fr"))
#'
#' @export
valider_email_regex <- function(email) {

  valider_email_regex <- email %>%
    stringr::str_detect(stringr::regex("^[a-z0-9\\._%-]+@[a-z0-9\\.-]+\\.[a-z]{2,4}$", ignore_case = TRUE))

  return(valider_email_regex)
}
