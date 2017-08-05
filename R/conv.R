#' conv_nombre_toutes_lettres
#'
#' @param nombre.
#'
#' @export
conv_nombre_toutes_lettres <- function(nombre, langue = "fr") {

  conv_nombre_toutes_lettres <- dplyr::tibble(nombre) %>%
    dplyr::left_join(caractr::data_mots_lettres, by = "nombre") %>%
    dplyr::pull(lettres)

  return(conv_nombre_toutes_lettres)
}
