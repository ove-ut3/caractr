#' conv_nombre_toutes_lettres
#'
#' @param nombre \dots
#' @param type \dots
#' @param langue \dots
#'
#' @export
conv_nombre_toutes_lettres <- function(nombre, type = "lettre", langue = "fr") {

  conv_nombre_toutes_lettres <- dplyr::tibble(nombre) %>%
    dplyr::left_join(caractr::data_mots_lettres, by = "nombre") %>%
    .[[type]]

  return(conv_nombre_toutes_lettres)
}
