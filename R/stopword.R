#' Update a character vector case according to stopwords.
#'
#' @param string Input character vector.
#' @param drop Stop words to drop.
#' @param language Stop word language.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_add_case(c("boulevard D'argonne", "1 rue descartes"))
#'
#' # An example with a dropped word
#' caractr::str_add_case(c("boulevard D'argonne", "1 rue descartes"), drop = "rue")
#'
#' # An English example
#' caractr::str_add_case(c("GENETICS AND IMMUNOLOGY OF PARASITIC DISEASES",
#'   "RESEARCH INSTITUTE  IN HEALTH"), language = "en")
#'
#' @export
str_add_case <- function(string, drop = NULL, language = "fr"){

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  prx_stopwords <- caractr::stopwords %>%
    dplyr::filter(language == !!language) %>%
    dplyr::pull(stopword) %>%
    paste0(collapse = "|") %>%
    { paste0("^(", ., ")$") }

  str_add_case <- dplyr::tibble(mot = string) %>%
    dplyr::mutate(cle = dplyr::row_number()) %>%
    tidyr::separate_rows(mot, sep = " ") %>%
    dplyr::mutate(
      # If the word is a stop word -> lower case
      mot_casse = ifelse(stringr::str_detect(stringr::str_remove_all(mot, "[[:punct:]]"), stringr::regex(prx_stopwords, ignore_case = TRUE)) & dplyr::row_number() != 1,
                                     tolower(mot), NA_character_),
      # If the word is the drop list -> lower case
      mot_casse = ifelse(is.na(mot_casse) & mot %in% drop, mot, mot_casse),
      # In other case, stringr::str_to_title
      mot_casse = ifelse(is.na(mot_casse), purrr::map_chr(mot, stringr::str_to_title), mot_casse),
      # Final update with words preceding quotes: " D'argonne " becomes " d'Argonne "
      mot_casse = sub("(D|L)'([[:alpha:]])", "\\L\\1\\E'\\U\\2\\E", mot_casse, perl = TRUE)
      ) %>%
    dplyr::group_by(cle) %>%
    dplyr::summarise(string = caractr::str_paste(mot_casse, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(string = stringr::str_replace_all(string, "\\b(d|l)\\s", "\\1'")) %>%
    dplyr::pull(string)

  return(str_add_case)
}
