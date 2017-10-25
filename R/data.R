#' data
#'
#' @export
#' @keywords internal
generer_data <- function() {

  mots_vides <- importr::importer_table_access("mots_vides", paste0(racine_packages, "caractr/Tables_ref.accdb"))
  save("mots_vides", file = paste0(racine_packages, "caractr/data/mots_vides.RData"))

  mots_lettres <- importr::importer_table_access("mots_lettres", paste0(racine_packages, "caractr/Tables_ref.accdb")) %>%
    dplyr::mutate(lettre_f = ifelse(lettre == "un", "une", lettre),
                  ieme = paste0(lettre, "ième"),
                  ieme = ifelse(nombre == 0, NA_character_, ieme),
                  ieme = ifelse(nombre == 1, "premier", ieme),
                  ieme = stringr::str_replace(ieme, "eième$", "ième"),
                  ieme = stringr::str_replace(ieme, "qième$", "quième"),
                  ieme = stringr::str_replace(ieme, "fième$", "vième"),
                  ieme_f = ifelse(nombre == 1, "première", ieme),
                  ieme_chiffre = ifelse(nombre == 1, "1er", paste0(nombre, "ème")),
                  ieme_chiffre = ifelse(nombre == 0, NA_character_, ieme_chiffre),
                  ieme_chiffre_f = ifelse(nombre == 1, "1ère", ieme_chiffre))
  save("mots_lettres", file = paste0(racine_packages, "caractr/data/mots_lettres.RData"))
}
