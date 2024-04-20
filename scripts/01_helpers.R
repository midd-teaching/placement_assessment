# Helpers ---------------------------------------------------------------------


# -----------------------------------------------------------------------------




# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------


# Analytic functions ----------------------------------------------------------

# Score lextale task

# Vector of correct/incorrect SPANISH
item_cor_response <- c(
  "terzo" = 0, "pellizcar" = 1, "pulmones" = 1, "batillón" = 0, "zapato" = 1, 
  "tergiversar" = 1, "pésimo" = 1, "cadeña" = 0, "hacha" = 1, "antar" = 0, 
  "cenefa" = 1, "asesinato" = 1, "helar" = 1, "yunque" = 1, "regar" = 1, 
  "abracer" = 0, "floroso" = 0, "arsa" = 0, "brecedad" = 0, "ávido" = 1, 
  "capillo" = 0, "lacayo" = 1, "lampera" = 0, "látigo" = 1, "bisagra" = 1, 
  "secuestro" = 1, "acutación" = 0, "merodear" = 1, "decar" = 0, 
  "alardio" = 0, "pandilla" = 1, "fatacidad" = 0, "pauca" = 0, "aviso" = 1, 
  "rompido" = 0, "loro" = 1, "granuja" = 1, "estornudar" = 1, "torpe" = 1, 
  "alfombra" = 1, "rebuscar" = 1, "cadallo" = 0, "canela" = 1, "cuchara" = 1, 
  "jilguero" = 1, "martillo" = 1, "cartinar" = 0, "ladrón" = 1, "ganar" = 1, 
  "flamida" = 0, "candado" = 1, "camisa" = 1, "vegada" = 0, "fomentar" = 1, 
  "nevar" = 1, "musgo" = 1, "tacaño" = 1, "plaudir" = 0, "besar" = 1, 
  "matar" = 1, "seda" = 1, "flaco" = 1, "esposante" = 0, "orgulloso" = 1, 
  "bizcocho" = 1, "hacido" = 0, "cabello" = 1, "alegre" = 1, "engatusar" = 1, 
  "temblo" = 0, "polvoriento" = 1, "pemición" = 0, "hervidor" = 1, 
  "cintro" = 0, "yacer" = 1, "atar" = 1, "tiburón" = 1, "frondoso" = 1, 
  "tropaje" = 0, "hormiga" = 1, "pozo" = 1, "empirador" = 0, "guante" = 1, 
  "escuto" = 0, "laúd" = 1, "barato" = 1, "grodo" = 0, "acantilado" = 1, 
  "prisa" = 1, "clavel" = 1)

# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2
score_lextale <- function(
    n_real = NULL, 
    n_nonse = NULL, 
    n_real_correct = NULL, 
    n_nonse_correct = NULL, 
    n_nonse_incorrect = NULL) {
  
  if (is.null(n_nonse_incorrect)) {
    avg_real <-  (n_real_correct / n_real * 100)
    avg_nonse <- (n_nonse_correct / n_nonse * 100)
    val <- (avg_real + avg_nonse) / 2
  } else {
    val <- n_real_correct - (2 * n_nonse_incorrect)
  }
  return(val)
}

# Round and format numbers to exactly N digits
specify_decimal <- function(x, k) {
  hold <- trimws(format(round(x, k), nsmall = k))
  out <- as.numeric(hold)
  return(out)
}

scale_this <- function(x) {
  out <- (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(out)
}

# -----------------------------------------------------------------------------
