#' Remove accents in a string.
#'
#' Found on : \url{http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding}
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_remove_accent("Universit\u00E9 de Franche-Comt\u00E9")
#'
#' @export
str_remove_accent <- function(string) {

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  string[which(Encoding(string) == "UTF-8")] <- iconv(string[which(Encoding(string) == "UTF-8")], from = "UTF-8", to = "ASCII//TRANSLIT")

  string <- iconv(string, to = "ASCII//TRANSLIT")

  return(string)
}

#' Remove punctuation in a string.
#'
#' @param string Input character vector.
#' @param replacement A character vector of replacements.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_remove_punct("Universit\u00E9 Paris-Est Cr\u00E9teil Val-de-Marne")
#' caractr::str_remove_punct("Universit\u00E9 Paris-Est Cr\u00E9teil Val-de-Marne", replacement = "_")
#'
#' @export
str_remove_punct <- function(string, replacement = " "){

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  return(stringr::str_replace_all(string, "[[:punct:]]+", replacement))
}

#' Capitalise the first letter in a string.
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_capitalise("universit\u00E9 de Caen Normandie")
#'
#' @export
str_capitalise <- function(string){

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  return(sub("^([[:alpha:]])", "\\U\\1\\E", string, perl = TRUE))
}

#' Equivalent to base paste but with an extra na.rm parameter
#'
#' Found on : \url{http://stackoverflow.com/questions/13673894/suppress-nas-in-paste}
#'
#' @param ... one or more R objects, to be converted to character vectors.
#' @param sep a character string to separate the terms.
#' @param collapse an optional character string to separate the results.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#'
#' @return A character vector of the concatenated values.
#'
#' @examples
#' paste("chaine1", NA_character_, "chaine2")
#' caractr::str_paste("chaine1", NA_character_, "chaine2")
#'
#' @export
str_paste <- function(..., sep = " ", collapse = NULL, na.rm = TRUE) {

  if (any(purrr::map_lgl(list(...), ~ length(.) == 0))) {
    return(character(0))
  }

  if (na.rm == FALSE) {
    paste(..., sep = sep, collapse = collapse)
  } else {
    if (na.rm == TRUE) {
      paste.na <- function(x, sep) {
        ret <- paste(stats::na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = FALSE)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse)) {
        ret
      } else {
        paste.na(ret, sep = collapse)
      }
    }
  }
}

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

#' Apply case from a target character to another string.
#'
#' @param string Input character vector.
#' @param target A case target character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_apply_case(c("test1", "test2"), target = c("aaAaa", "bBBbb"))
#'
#' @export
str_apply_case <- function(string, target) {

  if (class(string) != "character" | class(target) != "character") {
    stop("Both input vectors must be character vectors", call. = FALSE)
  }

  if (length(string) != length(target)) {
    stop("Both input vectors must have same length", call. = FALSE)
  }

  if (all(nchar(string) == nchar(target)) == FALSE) {
    stop("Both input vectors must have same nchar 2 by 2", call. = FALSE)
  }

  apply_case <- dplyr::tibble(string, target) %>%
    dplyr::mutate(string = stringr::str_split(string, ""),
                  target = stringr::str_split(target, "")) %>%
    tidyr::unnest(.id = "id_string")

  names(apply_case) <- make.unique(names(apply_case))

  apply_case <- dplyr::select(apply_case, -4) %>%
    dplyr::mutate(string = ifelse(target == tolower(target),
                                   tolower(string),
                                   string),
                  string = ifelse(target == toupper(target),
                                   toupper(string),
                                   string)) %>%
    dplyr::select(-target) %>%
    dplyr::group_by(.data$id_string) %>%
    dplyr::summarise(string = paste0(string, collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(dplyr::tibble(id_string = 1:length(string)),
                     by = "id_string") %>%
    dplyr::pull(string)

  return(apply_case)
}

#' Normalise a string for a use as file name.
#'
#' @param string Input character vector.
#' @param replace_slash Replace also \code{/} character.
#'
#' @return A character vector.
#'
#' @export
str_normalise_file <- function(string, replace_slash = TRUE) {

  normalised_string <- string %>%
    stringr::str_replace_all("(\\w)([\\<\\>:|\\?\\*\\\\])", "\\1 \\2") %>%
    stringr::str_replace_all("[\\<\\>:|\\?\\*\\\\]", " - ")

  if (replace_slash == TRUE) {
    normalised_string <- stringr::str_replace_all(normalised_string, "/", "-")
  }

  normalised_string <- stringr::str_replace_all(normalised_string, " +", " ")

  return(normalised_string)
}

#' Conversion from a numeric percentage to a character label.
#'
#' @param x A numeric vector.
#' @param digits Integer indicating the number of decimal places.
#' @param suffix Suffix after the percent value.
#' @param sign Display of "+" and "-".
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_percent(0.1)
#'
#' @export
str_percent <- function(x, digits = 0, suffix = "<U+202F>%", sign = FALSE) {

  percent <- round(x * 100, digits)

  if (sign == TRUE) {
    percent <- stringr::str_c(ifelse(percent > 0, "+", ""), percent)
  }

  if (!is.null(suffix)) {
    percent <- stringr::str_c(percent, suffix, sep = "")
  }

  percent <- stringr::str_replace(percent, "\\.", ",")

  return(percent)
}

#' Conversion from a numeric to a French formed number.
#'
#' Includes comma as decimal mark and non-breaking space between thousands.
#'
#' @param x A numeric vector.
#' @param big.mark Thousands separator, non-breaking space by default.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_pretty_num(1854.2)
#'
#' @export
str_pretty_num <- function(x, big.mark = "<U+202F>") {

  `Encoding<-`(gsub("-", big.mark, format(x, decimal.mark = ",", big.mark = "-")), "UTF-8")

}

#' Cut a character string over multiple lines
#'
#' @param string Input character vector.
#' @param nchar_max The maximum number of character per line.
#' @param collapse A character string to separate the results.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_line_break(c("A very very very very very very long character string", "test"),
#'  nchar_max = 40)
#'
#' @export
str_line_break <- function(string, nchar_max, collapse = "\n") {

  str_line_break <- dplyr::tibble(string = string) %>%
    dplyr::mutate(string = stringr::str_split(string, " ")) %>%
    tidyr::unnest(.id = "id") %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(nchar = (nchar(string) + 1) %>%
                    cumsum()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ligne = cut(nchar, seq.int(0, max(dplyr::pull(., nchar)) +  nchar_max - max(dplyr::pull(., nchar)) %% nchar_max , by = nchar_max))) %>%
    dplyr::group_by(.data$id, .data$ligne) %>%
    dplyr::summarise(string = paste(string, collapse = " ")) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(string = paste(string, collapse = collapse)) %>%
    dplyr::pull(string)

  return(str_line_break)
}

#' Conversion from a camel case character to a snake case character.
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_camel_to_snake_case(c("emploiNN1Type", "rechercheEmploi"))
#'
#' @export
str_camel_to_snake_case <- function(string) {

  camel_to_snake_case <- gsub("([A-Z])", "_\\L\\1\\E", string, perl = TRUE) %>%
    tolower()

  return(camel_to_snake_case)
}

#' Replace Windows quote by the ASCII quote
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_quote("Un emploi qui n\uE28099a pas de lien avec vos \u00E9tudes")
#'
#' @export
str_quote <- function(string) {

  return(stringr::str_replace_all(string, "\uE28099", "'"))
}

#' Get today date in several formats.
#'
#' @param format Date format to use : \code{file} or \code{litteral}.
#'
#' @return Date of the day.
#'
#' @examples
#' caractr::str_today()
#' caractr::str_today("litteral")
#'
#' @export
str_today <- function(format = "file") {

  str_today <- switch(format,
                      file = Sys.Date() %>%
                        as.character() %>%
                        stringr::str_replace_all("-", "_"),
                      litteral = Sys.Date() %>%
                        format("%d %B %Y") %>%
                        stringr::str_remove("^0") %>%
                        stringr::str_replace("^1 ", "1er "))

  return(str_today)
}

#' Get age in a litteral format.
#'
#' @param age A numeric vector of age.
#'
#' @return A character vector of age in litteral format.
#'
#' @examples
#' caractr::str_age_litteral(25.64048)
#'
#' @export
str_age_litteral <- function(age) {

  mois <- age %>%
  { . - floor(.) } %>%
  { . * 12 } %>%
    round()

  mois <- dplyr::case_when(
    mois >= 1 ~ "mois",
    TRUE ~ NA_character_
  ) %>%
    caractr::str_paste(mois, .) %>%
    dplyr::na_if("0") %>%
    stringr::str_c(" et ", .)

  annee <- floor(age)

  annee <- dplyr::case_when(
    annee == 1 ~ "an",
    annee >= 2 ~ "ans",
    TRUE ~ NA_character_
  ) %>%
    caractr::str_paste(annee, .)

  str_age_litteral <- caractr::str_paste(annee, mois, sep = "")

  return(str_age_litteral)
}
