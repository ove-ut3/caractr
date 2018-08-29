word_fr_accent <- readr::read_csv2("data-raw/word_fr_accent.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ","))

devtools::use_data(word_fr_accent, overwrite = TRUE)
