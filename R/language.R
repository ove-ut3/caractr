#' Add French accents to a string.
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' Source dataset : \code{caractr::word_fr_accent}.\cr
#'
#' @examples
#' caractr::str_add_fr_accent("Ecole superieure de commerce de Troyes")
#' caractr::str_add_fr_accent("Universite de Franche-Comte")
#'
#' @export
str_add_fr_accent <- function(string) {

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  with_accent <- dplyr::tibble(string) %>%
    dplyr::mutate(cle = dplyr::row_number(),
                  word = string) %>%
    tidyr::separate_rows(.data$word, sep = "\\b") %>%
    dplyr::mutate(word_lc = tolower(.data$word)) %>%
    dplyr::left_join(caractr::word_fr_accent, by = c("word_lc" = "word")) %>%
    dplyr::mutate(word_fr_accent = ifelse(!is.na(.data$word_fr_accent), .data$word_fr_accent, .data$word_lc),
                  word_fr_accent = caractr::str_apply_case(.data$word_fr_accent, .data$word)) %>%
    dplyr::group_by(.data$cle, string) %>%
    dplyr::summarise(string_accent = caractr::str_paste(.data$word_fr_accent, collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$string_accent)

  return(with_accent)
}

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
    dplyr::pull(.data$stopword) %>%
    paste0(collapse = "|") %>%
    { paste0("^(", ., ")$") }

  str_add_case <- dplyr::tibble(mot = string) %>%
    dplyr::mutate(cle = dplyr::row_number()) %>%
    tidyr::separate_rows(.data$mot, sep = " ") %>%
    dplyr::mutate(
      # If the word is a stop word -> lower case
      mot_casse = ifelse(stringr::str_detect(stringr::str_remove_all(.data$mot, "[[:punct:]]"), stringr::regex(prx_stopwords, ignore_case = TRUE)) & dplyr::row_number() != 1,
                         tolower(.data$mot), NA_character_),
      # If the word is the drop list -> lower case
      mot_casse = ifelse(is.na(.data$mot_casse) & .data$mot %in% drop, .data$mot, .data$mot_casse),
      # In other case, stringr::str_to_title
      mot_casse = ifelse(is.na(.data$mot_casse), purrr::map_chr(.data$mot, stringr::str_to_title), .data$mot_casse),
      # Final update with words preceding quotes: " D'argonne " becomes " d'Argonne "
      mot_casse = sub("(D|L)'([[:alpha:]])", "\\L\\1\\E'\\U\\2\\E", .data$mot_casse, perl = TRUE)
    ) %>%
    dplyr::group_by(.data$cle) %>%
    dplyr::summarise(string = caractr::str_paste(.data$mot_casse, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(string = stringr::str_replace_all(string, "\\b(d|l)\\s", "\\1'")) %>%
    dplyr::pull(string)

  return(str_add_case)
}
