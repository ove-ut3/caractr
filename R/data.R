#' Generate data
#'
#' @export
#' @keywords internal
generate_data <- function() {

  stopwords <- impexp::access_importer("stopwords", paste0(racine_packages, "caractr/raw/Tables_ref.accdb"))
  save("stopwords", file = paste0(racine_packages, "caractr/data/stopwords.RData"))

  mots_lettres <- impexp::access_importer("mots_lettres", paste0(racine_packages, "caractr/raw/Tables_ref.accdb")) %>%
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
