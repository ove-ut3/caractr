#' Obtenir la liste des mots vides (fr ou en) sous forme d'expression reguliere
#'
#' Obtenir la liste des mots vides (fr ou en) sous forme d'expression régulière.
#'
#' @param langue Code de la langue des mots vides.
#' @param excepte Vecteur de mots-vides à ne pas sélectionner.
#' @param selection Vecteur de mots-vides à sélectionner.
#'
#' @return Une expression régulière sous forme de chaine de caractères.\cr
#'
#' Jeu de données source : \code{caractr::data_mots_vides}.\cr
#' Il est créé à partir de la table "Mot_vide" de la base Access "Tables_ref.accdb" (projet "Text mining").
#'
#' @examples
#' # Les mots vides français
#' caractr::prx_mots_vides()
#'
#' # Les mots vides français sauf les mots "au" et "aux"
#' caractr::prx_mots_vides(excepte = c("au", "aux"))
#'
#' # Les mots vides français "au" et "aux"
#' caractr::prx_mots_vides(selection = c("au", "aux"))
#'
#' # Les mots vides anglais
#' caractr::prx_mots_vides(langue = "en")
#'
#' @export
prx_mots_vides <- function(langue = "fr", excepte = NULL, selection = NULL){

  prx_mots_vides <- dplyr::filter(caractr::data_mots_vides, code_langue == langue) %>%
    .[["mot_vide"]]

  if (!is.null(excepte)) {
    prx_mots_vides <- setdiff(prx_mots_vides, excepte)
  }

  if (!is.null(selection)) {
    prx_mots_vides <- intersect(prx_mots_vides, selection)
  }

  prx_mots_vides <- paste(prx_mots_vides, collapse = "|") %>%
    paste0("\\b(", ., ")\\b")

  return(prx_mots_vides)
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

  maj_casse <- dplyr::data_frame(mot = libelle) %>%
    dplyr::mutate(cle = row_number()) %>%
    tidyr::separate_rows(mot, sep = " ") %>%
    dplyr::mutate(
      # Si le mot (sans ponctuation) est un mot vide, on le passe en minuscule
      mot_casse = ifelse(stringr::str_detect(stringr::str_replace_all(mot, "[[:punct:]]", ""), stringr::regex(prx_mots_vides, ignore_case = TRUE)) & row_number() != 1,
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
    .[["libelle"]]

  return(maj_casse)
}
