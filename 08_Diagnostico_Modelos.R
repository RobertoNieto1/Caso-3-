# CASO 3 - RENT CAR
# PUNTO 8: diagnostico y mejora de los modelos
#   Residuos vs ajustados y vs tiempo, heterocedasticidad,
#   autocorrelacion, valores influyentes, estacionalidad residual y VIF
#   para los modelos 2 (niveles), 3 (log) y 4 (dummies), con Sales.
#   Orders da resultados equivalentes por ser proporcional a Sales.

ruta <- "tables/Base_RentCar_Final.csv"
if (!file.exists(ruta)) ruta <- "Base_RentCar_Final.csv"
stopifnot(file.exists(ruta))
base <- read.csv(ruta)
base$Week <- as.Date(base$Week)
base <- base[order(base$Week), ]   # orden cronologico explicito
base$Dic <- ifelse(format(base$Week, "%m") == "12", 1, 0)
if (!dir.exists("graphs")) dir.create("graphs")

controles <- "SP500 + WEI + UNRATE_Control + GASREGW_Control + NonBrand + Brand"
canales   <- "Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost"

m2 <- lm(as.formula(paste("Sales ~",       canales, "+", controles)), data = base)
m3 <- lm(as.formula(paste("log(Sales) ~",  canales, "+", controles)), data = base)
m4 <- lm(as.formula(paste("Sales ~",       canales, "+", controles, "+ Dic")), data = base)

modelos <- list(Modelo2 = m2, Modelo3 = m3, Modelo4 = m4)

# ---------------------------------------------------------------------
# 1. Graficos: residuos vs valores ajustados y residuos vs tiempo
# ---------------------------------------------------------------------
for (nombre in names(modelos)) {
  m <- modelos[[nombre]]
  png(paste0("graphs/08_resid_vs_ajustados_", nombre, ".png"), width = 800, height = 500)
  plot(fitted(m), resid(m), pch = 19, col = "steelblue",
       main = paste(nombre, ": residuos vs valores ajustados"),
       xlab = "Valores ajustados", ylab = "Residuos")
  abline(h = 0, col = "red", lty = 2)
  dev.off()
  
  png(paste0("graphs/08_resid_vs_tiempo_", nombre, ".png"), width = 900, height = 500)
  plot(base$Week, resid(m), type = "b", pch = 19, col = "steelblue",
       main = paste(nombre, ": residuos vs tiempo"),
       xlab = "Semana", ylab = "Residuos")
  abline(h = 0, col = "red", lty = 2)
  dev.off()
}

# ---------------------------------------------------------------------
# 2. Heterocedasticidad: Breusch-Pagan en la variante studentizada de
#    Koenker (LM = n * R2 de la regresion auxiliar del residuo al
#    cuadrado). Es una prueba asintotica chi-cuadrado valida bajo sus
#    supuestos; no es la version clasica exacta. H0: varianza constante.
# ---------------------------------------------------------------------
breusch_pagan <- function(m) {
  e2 <- resid(m)^2
  X  <- model.matrix(m)[, -1, drop = FALSE]   # regresores sin intercepto
  aux <- lm(e2 ~ X)
  LM <- nrow(X) * summary(aux)$r.squared
  p  <- pchisq(LM, df = ncol(X), lower.tail = FALSE)
  c(LM = LM, gl = ncol(X), p_valor = p)
}

# ---------------------------------------------------------------------
# 3. Autocorrelacion: estadistico de Durbin-Watson (calculado a mano,
#    con los datos ya ordenados por semana). Es DESCRIPTIVO: sin una
#    prueba formal, valores muy por debajo de 2 solo sugieren
#    autocorrelacion positiva, frecuente en series semanales.
# ---------------------------------------------------------------------
durbin_watson <- function(m) {
  e <- resid(m)
  sum(diff(e)^2) / sum(e^2)
}

# ---------------------------------------------------------------------
# 4. Valores influyentes: distancia de Cook. La referencia 4/n es solo
#    un filtro para decidir QUE observaciones revisar de cerca; no
#    prueba influencia. Un maximo menor que 1 tampoco garantiza que
#    ninguna observacion domine: hay que revisar las semanas senaladas.
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# 5. Estacionalidad residual: residuo absoluto promedio en semanas de
#    diciembre vs el resto del ano. Una diferencia grande es sugerente,
#    pero NO prueba por si sola estacionalidad residual: apoyarse
#    siempre en los graficos de la seccion 1.
# ---------------------------------------------------------------------

resultados <- data.frame()
for (nombre in names(modelos)) {
  m  <- modelos[[nombre]]
  bp <- breusch_pagan(m)
  cd <- cooks.distance(m)
  resultados <- rbind(resultados, data.frame(
    Modelo        = nombre,
    BP_LM         = round(bp["LM"], 2),
    BP_pvalor     = signif(bp["p_valor"], 3),
    Durbin_Watson = round(durbin_watson(m), 2),
    Cook_max      = round(max(cd), 3),
    Semana_Cook_max = as.character(base$Week[which.max(cd)]),
    N_Cook_mayor_4n = sum(cd > 4 / nrow(base)),
    Resid_abs_Dic = round(mean(abs(resid(m))[base$Dic == 1]), 4),
    Resid_abs_Resto = round(mean(abs(resid(m))[base$Dic == 0]), 4)
  ))
}
print(resultados)

# ---------------------------------------------------------------------
# 6. VIF calculado por separado en CADA modelo (los valores del punto 6
#    corresponden al modelo 2 y no se trasladan al modelo 4):
#    1 / (1 - R2) de regresar cada regresor sobre los demas. Por encima
#    de 5-10 suele considerarse colinealidad alta; no obliga a quitar
#    variables, pero pide cautela con los coeficientes individuales.
# ---------------------------------------------------------------------
vif_modelo <- function(m) {
  X <- model.matrix(m)[, -1, drop = FALSE]
  v <- sapply(seq_len(ncol(X)), function(j) {
    r2 <- summary(lm(X[, j] ~ X[, -j]))$r.squared
    1 / (1 - r2)
  })
  data.frame(Variable = colnames(X), VIF = round(v, 2))
}
vif_modelo(m2)
vif_modelo(m4)

# CONCLUSIONES DEL PUNTO 8
# Los graficos muestran dispersion desigual de los residuos,
# picos alrededor del cierre de ano y rachas de errores del
# mismo signo. La prueba de Breusch-Pagan aporta evidencia
# de heterocedasticidad en los tres modelos, y Durbin-Watson
# sugiere posible autocorrelacion positiva. Cook senala semanas
# que conviene revisar por su posible influencia, sin justificar
# eliminarlas automaticamente. Ademas, los VIF altos de Search
# y Sponsored requieren cautela al interpretar sus coeficientes.

# Aunque el modelo con diciembre mejora la prediccion, mantiene
# errores grandes en temporada alta y patrones en los residuos.
# Por tanto, agregar la variable de temporada no resuelve todos
# los problemas y la inferencia estadistica debe ser cautelosa.