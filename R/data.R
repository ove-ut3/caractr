#' Generate data
#'
#' @export
#' @keywords internal
generate_data <- function() {

  stopwords <- impexp::access_importer("stopwords", paste0(racine_packages, "caractr/raw/Tables_ref.accdb"))
  save("stopwords", file = paste0(racine_packages, "caractr/data/stopwords.RData"))

  number_letter <- impexp::access_importer("number_letter", paste0(racine_packages, "caractr/raw/Tables_ref.accdb")) %>%
    dplyr::mutate(letter_f = ifelse(letter == "un", "une", letter),
                  ieme = paste0(letter, "ième"),
                  ieme = ifelse(x == 0, NA_character_, ieme),
                  ieme = ifelse(x == 1, "premier", ieme),
                  ieme = stringr::str_replace(ieme, "eième$", "ième"),
                  ieme = stringr::str_replace(ieme, "qième$", "quième"),
                  ieme = stringr::str_replace(ieme, "fième$", "vième"),
                  ieme_f = ifelse(x == 1, "première", ieme),
                  ieme_number = ifelse(x == 1, "1er", paste0(x, "ème")),
                  ieme_number = ifelse(x == 0, NA_character_, ieme_number),
                  ieme_number_f = ifelse(x == 1, "1ère", ieme_number))
  save("number_letter", file = paste0(racine_packages, "caractr/data/number_letter.RData"))
}
