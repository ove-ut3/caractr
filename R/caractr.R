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

#' Mise a jour de mots non-accentues
#'
#' Mise à jour de mots non-accentués.
#'
#' @param libelle Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère dont les mots sont accentués.
#'
#' Jeu de données source : \code{caractr::mot_accent}.\cr
#' Il est créé à partir d'une table source SAS (projet "Text mining").\cr
#'
#' @examples
#' # Un exemple qui fonctionne bien
#' caractr::maj_accent("Ecole superieure de commerce de Troyes")
#'
#' # Un exemple qui fonctionne à moitié
#' caractr::maj_accent("Universite de Franche-Comte")
#'
#' @export
maj_accent <- function(libelle) {

  #libelle <- "Ecole superieure de commerce de Troyes"

  if (class(libelle) != "character") {
    stop("Le premier paramètre doit être de type character", call. = FALSE)
  }

  maj_accent <- dplyr::tibble(libelle) %>%
    dplyr::mutate(cle = dplyr::row_number(),
                  mot = libelle) %>%
    tidyr::separate_rows(mot, sep = "\\b") %>%
    dplyr::mutate(mot_lc = tolower(mot)) %>%
    dplyr::left_join(caractr::mot_accent, by = c("mot_lc" = "sans_accent")) %>%
    dplyr::mutate(mot_accent = ifelse(!is.na(avec_accent), avec_accent, mot_lc),
                  mot_accent = caractr::appliquer_casse(mot_accent, mot)) %>%
    dplyr::group_by(cle, libelle) %>%
    dplyr::summarise(libelle_accent = paste(mot_accent, collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::pull(libelle_accent)

  return(maj_accent)
}

#' Appliquer la casse d'une premiere chaine de caracteres vers une seconde chaine de caracteres
#'
#' Appliquer la casse d'une première chaine de caractères vers une seconde chaine de caractères.
#'
#' @param libelle Un vecteur de type caractère.
#' @param libelle_casse Un vecteur de type caractère servant de modèle pour la casse.
#'
#' @return Un vecteur de type caractère dont la casse est mise à jour.
#'
#' @examples
#' # Un exemple
#' caractr::appliquer_casse(c("test1", "test2"), libelle_casse = c("aaAaa", "bBBbb"))
#'
#' @export
appliquer_casse <- function(libelle, libelle_casse) {

  if (class(libelle) != "character" | class(libelle_casse) != "character") {
    stop("Les deux paramètres doivent être de type character", call. = FALSE)
  }

  if (length(libelle) != length(libelle_casse)) {
    stop("Les deux paramètres doivent avoir le même nombre d'éléments", call. = FALSE)
  }

  if (all(nchar(libelle) == nchar(libelle_casse)) == FALSE) {
    stop("Les chaines de caractères des deux paramètres doivent être toutes de même longueur deux à deux", call. = FALSE)
  }

  appliquer_casse <- dplyr::tibble(libelle, libelle_casse) %>%
    dplyr::mutate(libelle = stringr::str_split(libelle, ""),
                  libelle_casse = stringr::str_split(libelle_casse, "")) %>%
    tidyr::unnest(.id = "num_libelle")

  names(appliquer_casse) <- make.unique(names(appliquer_casse))

  appliquer_casse <- dplyr::select(appliquer_casse, -4) %>%
    dplyr::mutate(libelle = ifelse(libelle_casse == tolower(libelle_casse),
                                   tolower(libelle),
                                   libelle),
                  libelle = ifelse(libelle_casse == toupper(libelle_casse),
                                   toupper(libelle),
                                   libelle)) %>%
    dplyr::select(-libelle_casse) %>%
    dplyr::group_by(num_libelle) %>%
    dplyr::summarise(libelle = paste0(libelle, collapse = "")) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(dplyr::tibble(num_libelle = 1:length(libelle)),
                     by = "num_libelle") %>%
    dplyr::pull(libelle)

  return(appliquer_casse)
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
