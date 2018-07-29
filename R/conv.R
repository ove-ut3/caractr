#' Convert a numeric vector to full letters.
#'
#' @param x A numeric vector.
#' @param type Choice between : lettre|ieme|ieme_chiffre
#' @param female If \code{TRUE}, female form.
#' @param language French language only available.
#'
#' @examples
#' caractr::str_conv_number_letter(1:10)
#' caractr::str_conv_number_letter(1, female = TRUE)
#'
#' @export
str_conv_number_letter <- function(x, type = "lettre", female = FALSE, language = "fr") {

  if (female) {
    type <- paste0(type, "_f")
  }

  letter <- dplyr::tibble(x) %>%
    dplyr::left_join(caractr::mots_lettres, by = c("x" = "nombre")) %>%
    dplyr::pull(type)

  return(letter)
}
