#' Supprimer les accents d'une chaine de caracteres
#'
#' Supprimer les accents d'une chaine de caractères.\cr
#'
#' Trouvé sur : \url{http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding}
#'
#' @param libelle Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère sans accent.
#'
#' @examples
#' caractr::sans_accent("Université de Franche-Comté")
#'
#' @export
sans_accent <- function(libelle) {

  if (class(libelle) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  sans_accent <- iconv(libelle, from = "UTF-8", to = "ASCII//TRANSLIT")

  position_na <- which(is.na(sans_accent) & !is.na(libelle))
  if (length(position_na) >= 1) {
    sans_accent[position_na] <- iconv(libelle[position_na],
                                      from = "Windows-1252",
                                      to = "ASCII//TRANSLIT")
  }

  return(sans_accent)
}

#' Supprimer la ponctuation d'une chaine de caracteres
#'
#' Supprimer la ponctuation d'une chaine de caractères
#'
#' @param libelle Un vecteur de type caractère.
#' @param remplacement Le caractère de remplacement de la ponctuation.
#'
#' @return Un vecteur de type caractère sans ponctuation.
#'
#' @examples
#' caractr::sans_ponctuation("Université Paris-Est Créteil Val-de-Marne")
#' caractr::sans_ponctuation("Université Paris-Est Créteil Val-de-Marne", remplacement = "_")
#'
#' @export
sans_ponctuation <- function(libelle, remplacement = " "){

  if (class(libelle) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  sans_ponctuation <- stringr::str_replace_all(libelle, "[[:punct:]]+", remplacement)

  return(sans_ponctuation)
}

#' Mettre une capitale a la premiere lettre d'une chaine de caracteres
#'
#' Mettre une capitale à la première lettre d'une chaine de caractères.
#'
#' @param libelle Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère dont la première lettre de chaque élément est une majuscule.
#'
#' @examples
#' caractr::str_capitaliser("université de Caen Normandie")
#'
#' @export
str_capitaliser <- function(libelle){

  if (class(libelle) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  str_capitaliser <- sub("^([[:alpha:]])", "\\U\\1\\E", libelle, perl = TRUE)

  return(str_capitaliser)
}

#' Normaliser une chaine de caracteres (utilisation pour un nom de champ dans une table)
#'
#' Normaliser une chaine de caractères (utilisation pour un nom de champ dans une table).
#'
#' @param libelle Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère contenant les libellés normalisés.
#'
#' @examples
#' caractr::normaliser_char(c("Type d'unité Sirus : entreprise profilée ou unité légale", "Nic du siège"))
#'
#' @export
normaliser_char <- function(libelle){

  if (class(libelle) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  normaliser_char <- tryCatch(
    {
      tolower(libelle)
    },
    error = function(cond) {
      normaliser_char <- stringr::str_conv(libelle, "ISO-8859-1") %>%
        tolower()
      return(normaliser_char)
    }
  )

  normaliser_char <- normaliser_char %>%
    # Remplacement de la ponctuation et des espaces par un underscore
    stringr::str_replace_all("[[:punct:]\\s]+", "_") %>%
    # Un undersore en fin de chaine est supprimé
    stringr::str_replace_all("_$", "") %>%
    # Tous les caractères non-alphanumériques sont supprimés
    stringr::str_replace_all("[^\\w]", "") %>%
    # Conversion des accents
    caractr::sans_accent()

  return(normaliser_char)
}

#' Open data - Convertir les NA d'une chaine de caracteres vers une chaine vide
#'
#' Convertir les \code{NA} d'une chaine de caractères vers une chaine vide.\cr
#'
#' Utilisé conjointement avec la fonction \code{conv_pv_vide()} pour créer les champs multivalués vides des fichiers en open data.
#'
#' @param libelle Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère dont les \code{NA} sont transformés en \code{""}.
#'
#' @examples
#' caractr::conv_ods_na_vide(c("Université de Franche-Comté", NA_character_))
#'
#' @export
conv_ods_na_vide <- function(libelle){

  if (class(libelle) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  conv_na_vide <- ifelse(is.na(libelle), "", libelle)

  return(conv_na_vide)
}

#' Open data - Convertir les points-virgules seuls vers NA
#'
#' Convertir les points-virgules seuls vers \code{NA}.\cr
#'
#' Utilisé conjointement avec la fonction \code{conv_na_vide()} pour créer les champs multivalués vides des fichiers en open data.
#'
#' @param libelle Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère dont les points-virgule seuls sont transformés en \code{NA}.
#'
#' @examples
#' conv_ods_pv_na(c(";;;", ";CNRS;;"))
#'
#' @export
conv_ods_pv_na <- function(libelle){

  if (class(libelle) != "character") {
    stop("Le paramètre doit être de type character", call. = FALSE)
  }

  conv_pv_vide <- ifelse(stringr::str_detect(libelle, "^;+$"), NA_character_, libelle)

  return(conv_pv_vide)
}

#' Derive de la fonction paste() avec un parametre de suppression des NA
#'
#' Dérivé de la fonction \code{paste()} avec un paramètre de suppression des \code{NA}.\cr
#'
#' Trouvé sur : \url{http://stackoverflow.com/questions/13673894/suppress-nas-in-paste}
#'
#' @param ... Des chaines de caractère à concaténer.
#'
#' @return Une chaine de caractère concaténée sans \code{NA}.
#'
#' @examples
#' # Avec la fonction paste() classique
#' paste("chaine1", NA_character_, "chaine2")
#'
#' # Avec la fonction paste2()
#' caractr::paste2("chaine1", NA_character_, "chaine2")
#'
#' @export
paste2 <- function(..., sep = " ", collapse = NULL, na.rm = TRUE) {

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

#' Derive de la fonction paste() mais retourne NA si un seul des elements est a NA
#'
#' Dérivé de la fonction \code{paste()} mais retourne \code{NA} si un seul des elements est a \code{NA}\cr
#'
#' @param ... Des chaines de caractère à concaténer.
#'
#' @return Une chaine de caractère concaténée sans \code{NA}.
#'
#' @examples
#' # Avec la fonction paste() classique
#' paste("chaine1", NA_character_, "chaine2")
#'
#' # Avec la fonction paste_na()
#' caractr::paste_na("chaine1", NA_character_, "chaine2")
#'
#' @export
paste_na <- function(..., sep = " ", collapse = NULL) {

  paste_na <- data.frame(..., stringsAsFactors = FALSE)

  is_na <- is.na.data.frame(paste_na) %>%
    apply(1, function(x) any(x))

  paste_na <- ifelse(is_na, NA_character_, apply(paste_na, 1, paste, collapse = sep))

  return(paste_na)
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

  maj_accent <- tibble::tibble(libelle) %>%
    dplyr::mutate(cle = row_number(),
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

  appliquer_casse <- tibble::tibble(libelle, libelle_casse) %>%
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
    dplyr::right_join(tibble::tibble(num_libelle = 1:length(libelle)),
                     by = "num_libelle") %>%
    dplyr::mutate(libelle = caractr::conv_ods_na_vide(libelle)) %>%
    dplyr::pull(libelle)

  return(appliquer_casse)
}

#' Normaliser des chaines de caractere en tant que nom de fichier
#'
#' Normaliser des chaines de caractère en tant que nom de fichier.
#'
#' @param char Un vecteur de type caractère.
#'
#' @return Un vecteur de type caractère prêts à être utilisés en tant que nom de fichier.
#'
#' @examples
#'
#' @export
str_fichier <- function(char) {

  str_fichier <- char %>%
    stringr::str_replace_all("(\\w)([\\<\\>:|\\?\\*/\\\\])", "\\1 \\2") %>%
    stringr::str_replace_all("[\\<\\>:|\\?\\*/\\\\]", " - ") %>%
    stringr::str_replace_all(" +", " ")

  return(str_fichier)

}

#' Passage en libelle de pourcentage d'une valeur numerique
#'
#' Passage en libellé de pourcentage d'une valeur numérique.
#'
#' @param valeur Un vecteur de valeurs numérique.
#'
#' @return Un vecteur de libellés de pourcentages.
#'
#' @examples
#' caractr::lib_pourcentage(0.1)
#'
#' @export
lib_pourcentage <- function(valeur, decimales = 1) {

  lib_pourcentage <- caractr::paste_na(round(valeur * 100, decimales), "%")

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

  str_saut_ligne <- tibble::tibble(char = char) %>%
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
