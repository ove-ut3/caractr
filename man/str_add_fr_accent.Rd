% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/language.R
\name{str_add_fr_accent}
\alias{str_add_fr_accent}
\title{Add French accents to a string.}
\usage{
str_add_fr_accent(string)
}
\arguments{
\item{string}{Input character vector.}
}
\value{
A character vector.

Source dataset : \code{caractr::word_fr_accent}.\cr
}
\description{
Add French accents to a string.
}
\examples{
caractr::str_add_fr_accent("Ecole superieure de commerce de Troyes")
caractr::str_add_fr_accent("Universite de Franche-Comte")

}
