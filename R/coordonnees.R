#' Nettoyer des numeros de telephone
#'
#' Nettoyer des numéros de téléphone.\cr
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
    mutate(numero = stringr::str_replace_all(numero, "\n", " "),
           numero2 = stringr::str_replace_all(numero, "[^\\d]", ""),
           numero2 = ifelse(nchar(numero2) == 10, stringr::str_replace(numero2, "(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})", "\\1 \\2 \\3 \\4 \\5"),
                            numero))
  return(nettoyer_numero_telephone$numero2)
}
