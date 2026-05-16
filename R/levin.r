# Funzione per calcolare il Levins normalizzato (Bn)
# 0 è uno specialista estremo, 1 è un generalista perfetto
calc_levins_norm <- function(counts) {
  p <- counts / sum(counts) # Calcolo proporzioni
  B <- 1 / sum(p^2) # Levins originale
  n <- length(p) # Numero di categorie (specie/risorse)
  if (n <= 1) {
    return(0)
  }
  Bn <- (B - 1) / (n - 1) # Normalizzazione (Feinsinger et al. 1981)
  return(Bn)
}
