number_letter <- read.csv2("data-raw/number_letter.csv", na.strings = "") %>%
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


devtools::use_data(number_letter, overwrite = TRUE)
