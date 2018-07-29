#' Remove accents in a string.
#'
#' Found on : \url{http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding}
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_remove_accent("Université de Franche-Comté")
#'
#' @export
str_remove_accent <- function(string) {

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  no_accents <- iconv(string, from = "UTF-8", to = "ASCII//TRANSLIT")

  # If iconv from UT8 did not work, iconv from Windows-1252
  position_na_utf8 <- which(is.na(no_accents) & !is.na(string))
  if (length(position_na_utf8) >= 1) {
    no_accents[position_na_utf8] <- iconv(string[position_na_utf8],
                                          from = "Windows-1252",
                                          to = "ASCII//TRANSLIT")
  }

  return(no_accents)
}

#' Remove punctuation in a string.
#'
#' @param string Input character vector.
#' @param replacement A character vector of replacements.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_remove_punct("Université Paris-Est Créteil Val-de-Marne")
#' caractr::str_remove_punct("Université Paris-Est Créteil Val-de-Marne", replacement = "_")
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
#' caractr::str_capitalise("université de Caen Normandie")
#'
#' @export
str_capitalise <- function(string){

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  return(sub("^([[:alpha:]])", "\\U\\1\\E", string, perl = TRUE))
}

#' Normalise a string for a use as table field name.
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' caractr::str_normalise(c("Type d'unité Sirus : entreprise profilée ou unité légale", "Nic du siège"))
#'
#' @export
str_normalise <- function(string){

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  # Lower case and conv from ISO-8859-1 if it does not work
  normalised_string <- tryCatch(
    {
      tolower(string)
    },
    error = function(cond) {
      normalised_string <- stringr::str_conv(string, "ISO-8859-1") %>%
        tolower()
      return(normalised_string)
    }
  )

  normalised_string <- normalised_string %>%
    # Replacement of punctuation and spaces by an underscore
    stringr::str_replace_all("[[:punct:]\\s]+", "_") %>%
    # A trailing undersore is removed
    stringr::str_remove_all("_$") %>%
    # All non alphanumeric strings are removed
    stringr::str_remove_all("[^\\w]") %>%
    # All accents are removed
    caractr::str_remove_accent()

  return(normalised_string)
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
        ret <- paste(na.omit(x), collapse = sep)
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
    tidyr::separate_rows(word, sep = "\\b") %>%
    dplyr::mutate(word_lc = tolower(word)) %>%
    dplyr::left_join(caractr::word_fr_accent, by = c("word_lc" = "word")) %>%
    dplyr::mutate(word_fr_accent = ifelse(!is.na(word_fr_accent), word_fr_accent, word_lc),
                  word_fr_accent = caractr::str_apply_case(word_fr_accent, word)) %>%
    dplyr::group_by(cle, string) %>%
    dplyr::summarise(string_accent = caractr::str_paste(word_fr_accent, collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::pull(string_accent)

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
    dplyr::group_by(id_string) %>%
    dplyr::summarise(string = paste0(string, collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(dplyr::tibble(id_string = 1:length(string)),
                     by = "id_string") %>%
    dplyr::pull(string)

  return(apply_case)
}

#' Normaliser des chaines de caractere en tant que nom de fichier
#'
#' Normaliser des chaines de caractère en tant que nom de fichier.
#'
#' @param char Un vecteur de type caractère.
#' @param replace_slash Remplace aussi le caractère /.
#'
#' @return Un vecteur de type caractère prêts à être utilisés en tant que nom de fichier.
#'
#' @examples
#'
#' @export
str_fichier <- function(char, replace_slash = TRUE) {

  str_fichier <- char %>%
    stringr::str_replace_all("(\\w)([\\<\\>:|\\?\\*\\\\])", "\\1 \\2") %>%
    stringr::str_replace_all("[\\<\\>:|\\?\\*\\\\]", " - ")

  if (replace_slash == TRUE) {
    str_fichier <- stringr::str_replace_all(str_fichier, "/", "-")
  }

  str_fichier <- stringr::str_replace_all(str_fichier, " +", " ")

  return(str_fichier)

}

#' Passage en libelle de pourcentage d'une valeur numerique
#'
#' Passage en libellé de pourcentage d'une valeur numérique.
#'
#' @param valeur Un vecteur de valeurs numérique.
#' @param decimales Le nombre de décimales.
#' @param symbole_pct Affichage ou non du caractère "\%".
#'
#' @return Un vecteur de libellés de pourcentages.
#'
#' @examples
#' caractr::lib_pourcentage(0.1)
#'
#' @export
lib_pourcentage <- function(valeur, decimales = 1, symbole_pct = TRUE) {

  lib_pourcentage <- round(valeur * 100, decimales) %>%
    stringr::str_replace("\\.", ",")

  if (symbole_pct == TRUE) {
    lib_pourcentage <- stringr::str_c(lib_pourcentage, "%", sep = "\U202F")

  }

  return(lib_pourcentage)
}

#' Decoupage d'une chaine de caracteres sur plusieurs lignes
#'
#' Découpage d'une chaine de caractères sur plusieurs lignes.
#'
#' @param char Un vecteur de chaines de caractères.
#' @param n_char_max Un nombre de caractères maximum par ligne.
#' @param collapse Le séparateur pour le saut de ligne.
#'
#' @return Un vecteur de chaines de caractères.
#'
#' @examples
#' caractr::str_saut_ligne(c("Une très très très très très très longue chaine de caractère", "test"), nchar = 40)
#'
#' @export
str_saut_ligne <- function(char, n_char_max, collapse = "\n") {

  str_saut_ligne <- dplyr::tibble(char = char) %>%
    dplyr::mutate(char = stringr::str_split(char, " ")) %>%
    tidyr::unnest(.id = "id") %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n_char = (nchar(char) + 1) %>%
                    cumsum()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ligne = cut(n_char, seq.int(0, max(dplyr::pull(., n_char)) +  n_char_max - max(dplyr::pull(., n_char)) %% n_char_max  , by = n_char_max))) %>%
    dplyr::group_by(id, ligne) %>%
    dplyr::summarise(char = paste(char, collapse = " ")) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(char = paste(char, collapse = collapse)) %>%
    dplyr::pull(char)

  return(str_saut_ligne)
}

#' Passage d'un nom de champ de la casse camel a snake
#'
#' Passage d'un nom de champ de la casse camel à snake.
#'
#' @param char Un vecteur de chaines de caractères à la casse camel.
#'
#' @return Un vecteur de chaines de caractères à la casse snake.
#'
#' @examples
#' caractr::camel_to_snake_case(c("emploiNN1Type", "rechercheEmploi"))
#'
#' @export
camel_to_snake_case <- function(char) {

  camel_to_snake_case <- gsub("([A-Z])", "_\\L\\1\\E", char, perl = TRUE) %>%
    tolower()

  return(camel_to_snake_case)
}

#' remplace l'apostrophe Windows par l'apostrophe ASCII
#'
#' remplace l'apostrophe Windows par l'apostrophe ASCII.
#'
#' @param char Un vecteur de chaines de caractères.
#'
#' @return Un vecteur de chaines de caractères sans apostrophe Windows.
#'
#' @examples
#' caractr::str_apostrophe("Un emploi qui n’a pas de lien avec vos études")
#'
#' @export
str_apostrophe <- function(char) {

  str_apostrophe <- stringr::str_replace_all(char, "’", "'")

  return(str_apostrophe)
}
