stopwords <- readr::read_csv2("data-raw/stopwords.csv", col_types = readr::cols(), locale = readr::locale(decimal_mark = ","))

usethis::use_data(stopwords, overwrite = TRUE)
