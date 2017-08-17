#' Nettoyer des numeros de telephone
#'
#' Nettoyer des numéros de téléphone.
#'
#' @param numero Un vecteur de numéros.
#'
#' @return Un vecteur de numéros nettoyés.
#'
#' @examples
#' caractr::nettoyer_numero_telephone("06.00.00.00.00")
#'
#' @export
nettoyer_numero_telephone <- function(numero) {

  nettoyer_numero_telephone <- tibble::tibble(numero) %>%
    dplyr::mutate(numero = stringr::str_replace_all(numero, "\n", " "),
                  numero2 = stringr::str_replace_all(numero, "[^\\d]", ""),
                  numero2 = stringr::str_replace_all(numero2, "^(00)?33(\\d{9})", "0\\2"),
                  numero2 = ifelse(nchar(numero2) == 9, paste0("0", numero2), numero2),
                  numero2 = ifelse(nchar(numero2) == 10, stringr::str_replace(numero2, "(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})", "\\1 \\2 \\3 \\4 \\5"),
                            numero))
  return(nettoyer_numero_telephone$numero2)
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
#' caractr::valider_email_regex("test@test")
#' caractr::valider_email_regex("test@test.fr")
#'
#' @export
valider_email_regex <- function(email) {

  valider_email_regex <- email %>%
    stringr::str_detect(stringr::regex("^[a-z0-9\\._%-]+@[a-z0-9\\.-]+\\.[a-z]{2,4}$", ignore_case = TRUE))

  return(valider_email_regex$numero2)
}
