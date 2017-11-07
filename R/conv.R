#' conv_nombre_toutes_lettres
#'
#' @param nombre \dots
#' @param type au choix : lettre|lettre_f|ieme|ieme_f|ieme_chiffre|ieme_chiffre_f
#' @param langue \dots
#'
#' @export
conv_nombre_toutes_lettres <- function(nombre, type = "lettre", langue = "fr") {

  conv_nombre_toutes_lettres <- dplyr::tibble(nombre) %>%
    dplyr::left_join(caractr::mots_lettres, by = "nombre") %>%
    .[[type]]

  return(conv_nombre_toutes_lettres)
}
