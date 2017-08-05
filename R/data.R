#' data
#'
#' @export
data <- function() {

  data_mots_vides <- importr::importer_table_access("mots_vides", paste0(racine_packages, "caractr/Tables_ref.accdb"))
  save("data_mots_vides", file = paste0(racine_packages, "caractr/data/data_mots_vides.RData"))

  data_mots_lettres <- importr::importer_table_access("mots_lettres", paste0(racine_packages, "caractr/Tables_ref.accdb"))
  save("data_mots_lettres", file = paste0(racine_packages, "caractr/data/data_mots_lettres.RData"))
}
