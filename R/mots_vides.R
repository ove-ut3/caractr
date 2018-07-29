#' Get a list of stop words as a regular expression.
#'
#' @param language Stop word language.
#' @param drop Stop words to drop.
#' @param keep Stop words to keep.
#'
#' @return A regular expression as string character.
#'
#' @details
#' Source dataset : \code{caractr::stopwords}.
#'
#' @examples
#' # French stopwords
#' caractr::prx_stopwords()
#'
#' # French stopwords except "au" and "aux"
#' caractr::prx_stopwords(drop = c("au", "aux"))
#'
#' # French stopwords but only "au" and "aux"
#' caractr::prx_stopwords(keep = c("au", "aux"))
#'
#' # English stopwords
#' caractr::prx_stopwords(language = "en")
#'
#' @export
#' @keywords internal
prx_stopwords <- function(language = "fr", drop = NULL, keep = NULL){

  stopwords <- dplyr::filter(caractr::stopwords, language == !!language) %>%
    dplyr::pull(stopword)

  if (!is.null(drop)) {
    stopwords <- setdiff(stopwords, drop)
  }

  if (!is.null(keep)) {
    stopwords <- intersect(stopwords, keep)
  }

  stopwords <- paste(stopwords, collapse = "|") %>%
    paste0("\\b(", ., ")\\b")

  return(stopwords)
}

#' Mise a jour de la casse en prenant en compte les mots vides
#'
#' Mise en majuscule de la première lettre de chaque mot sauf les mots_vides.
#'
#' @param libelle Un vecteur de type caractère.
#' @param excepte Vecteur de mots à ne pas mettre à jour.
#' @param code_langue Code de la langue des mots vides.
#'
#' @return Un vecteur de type caractère dont tous les mots commencent par une majuscule sauf les mots vides de la langue sélectionnée.
#'
#' @examples
#' # un premier exemple
#' caractr::maj_casse(c("boulevard D'argonne", "1 rue descartes"))
#'
#' # Un exemple avec un mot non-sélectionné
#' caractr::maj_casse(c("boulevard D'argonne", "1 rue descartes"), excepte = "rue")
#'
#' # Un exemple en anglais
#' caractr::maj_casse(c("GENETICS AND IMMUNOLOGY OF PARASITIC DISEASES",
#'   "RESEARCH INSTITUTE  IN HEALTH"), code_langue = "en")
#'
#' @export
maj_casse <- function(libelle, excepte = NULL, code_langue = "fr"){

  if (class(libelle) != "character") {
    stop("Le premier paramètre doit être de type character", call. = FALSE)
  }

  prx_mots_vides <- prx_mots_vides(langue = code_langue)

  maj_casse <- dplyr::tibble(mot = libelle) %>%
    dplyr::mutate(cle = dplyr::row_number()) %>%
    tidyr::separate_rows(mot, sep = " ") %>%
    dplyr::mutate(
      # Si le mot (sans ponctuation) est un mot vide, on le passe en minuscule
      mot_casse = ifelse(stringr::str_detect(stringr::str_remove_all(mot, "[[:punct:]]"), stringr::regex(prx_mots_vides, ignore_case = TRUE)) & dplyr::row_number() != 1,
                                     tolower(mot), NA_character_),
      # Si c'est un mot de la liste excepte, on le laisse tel quel
      mot_casse = ifelse(is.na(mot_casse) & mot %in% excepte, mot, mot_casse),
      # Dans les autres cas, on applique la fonction "str_to_title" (prop_case)
      mot_casse = ifelse(is.na(mot_casse), purrr::map_chr(mot, stringr::str_to_title), mot_casse),
      # Modif de la casse finale : " D'argonne " devient " d'Argonne "
      mot_casse = sub("(D|L)'([[:alpha:]])", "\\L\\1\\E'\\U\\2\\E", mot_casse, perl = TRUE)
      ) %>%
    dplyr::group_by(cle) %>%
    dplyr::summarise(libelle = paste2(mot_casse, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(libelle = stringr::str_replace_all(libelle, "\\b(d|l)\\s", "\\1'")) %>%
    dplyr::pull(libelle)

  return(maj_casse)
}
