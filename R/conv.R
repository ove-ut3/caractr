#' Convert a numeric vector to full letters.
#'
#' @param x A numeric vector.
#' @param type Choice between : letter|ieme|ieme_number
#' @param female If \code{TRUE}, female form.
#' @param language French language only available.
#'
#' @examples
#' caractr::str_conv_number_letter(1:10)
#' caractr::str_conv_number_letter(1, female = TRUE)
#'
#' @export
str_conv_number_letter <- function(x, type = "letter", female = FALSE, language = "fr") {

  if (female) {
    type <- glue::glue("{type}_f") %>%
      as.character()
  }

  letter <- dplyr::tibble(x) %>%
    dplyr::left_join(caractr::number_letter, by = "x") %>%
    dplyr::pull(type)

  return(letter)
}
