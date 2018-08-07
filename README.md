# caractr

**caractr** provides a few functions in addition of those within **stringr** package.

Its name corresponds to the simplified French translation of **character** because some of the functions deal with French characters.

## Examples

```r

# Paste with an extra na.rm parameter
paste("chaine1", NA_character_, "chaine2")
caractr::str_paste("chaine1", NA_character_, "chaine2")

# Remove (French) accents in a string
caractr::str_remove_accent("Université de Franche-Comté")

# Convert a numeric percentage to character with the non-breaking space
caractr::str_percent(0.111, digits = 1)

```
