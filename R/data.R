#' data
#'
#' @export
#' @keywords internal
data <- function() {

  mots_vides <- importr::importer_table_access("mots_vides", paste0(racine_packages, "caractr/data/Tables_ref.accdb"))
  save("mots_vides", file = paste0(racine_packages, "caractr/data/mots_vides.RData"))

  mots_lettres <- importr::importer_table_access("mots_lettres", paste0(racine_packages, "caractr/data/Tables_ref.accdb")) %>%
    dplyr::mutate(ieme = paste0(lettres, "ième"),
                  ieme = ifelse(nombre == 0, NA_character_, ieme),
                  ieme = ifelse(nombre == 1, "premier", ieme),
                  ieme = stringr::str_replace(ieme, "eième$", "ième"),
                  ieme = stringr::str_replace(ieme, "qième$", "quième"),
                  ieme = stringr::str_replace(ieme, "fième$", "vième"),
                  ieme_f = ifelse(nombre == 1, "première", ieme))
  save("mots_lettres", file = paste0(racine_packages, "caractr/data/mots_lettres.RData"))
}
