stopwords <- read.csv2("data-raw/stopwords.csv")
devtools::use_data(stopwords, overwrite = TRUE)
