# CASO 3 - RENT CAR
# PUNTO 7: dos caminos para examinar los picos de temporada alta
#   Camino A / Modelo (3): ln(KPI) con los mismos regresores del modelo (2)
#   Camino B / Modelo (4): KPI con dummies de temporada alta (calendario)
#   Comparacion: MAE y RMSE en la escala original del KPI,
#   con separacion temporal de entrenamiento y evaluacion

base <- read.csv("tables/Base_RentCar_Final.csv")
base$Week <- as.Date(base$Week)

# ---------------------------------------------------------------------
# 0. Verificaciones previas (el enunciado pide comprobar que el KPI
#    sea positivo antes de aplicar el logaritmo)
# ---------------------------------------------------------------------
stopifnot(
  nrow(base) > 0,
  !any(is.na(base$Week)),                 # todas las fechas parsean
  all(is.finite(base$Sales)),  all(base$Sales  > 0),   # KPI positivo y finito
  all(is.finite(base$Orders)), all(base$Orders > 0)
)

# Controles: los mismos del modelo (2) del punto 6, para que los
# resultados sean comparables. Si alguno falta en la base, el script se
# detiene aqui con un error claro.
vars_modelo <- c("Sales", "Orders",
                 "Amazon_DSP_Cost", "Amazon_Search_Cost", "Amazon_Sponsored_Cost",
                 "SP500", "WEI", "UNRATE_Control", "GASREGW_Control",
                 "NonBrand", "Brand")
stopifnot(all(vars_modelo %in% names(base)))
stopifnot(all(sapply(base[, vars_modelo], function(x) all(is.finite(x)))))

controles <- "SP500 + WEI + UNRATE_Control + GASREGW_Control + NonBrand + Brand"
canales   <- "Amazon_DSP_Cost + Amazon_Search_Cost + Amazon_Sponsored_Cost"

# ---------------------------------------------------------------------
# 1. CAMINO A / MODELO (3): ln(KPI) con los mismos regresores del modelo (2)
#    ln(KPIt) = b0 + b1 DSPt + b2 Searcht + b3 Sponsoredt + g'Xt + et
#    (ln = logaritmo natural; en R, log() ya es logaritmo natural)
# ---------------------------------------------------------------------
m3_sales  <- lm(as.formula(paste("log(Sales) ~",  canales, "+", controles)), data = base)
m3_orders <- lm(as.formula(paste("log(Orders) ~", canales, "+", controles)), data = base)

summary(m3_sales)
summary(m3_orders)

100 * coef(m3_sales)[c("Amazon_DSP_Cost", "Amazon_Search_Cost", "Amazon_Sponsored_Cost")]  # aprox
100 * (exp(coef(m3_sales)[c("Amazon_DSP_Cost", "Amazon_Search_Cost", "Amazon_Sponsored_Cost")]) - 1)  # exacto

# ---------------------------------------------------------------------
# 2. CAMINO B / MODELO (4): KPI con dummies de temporada alta
#    KPIt = b0 + b1 DSPt + b2 Searcht + b3 Sponsoredt + g'Xt + d'Dt + et
# ---------------------------------------------------------------------
# Dummy propuesta: 1 si la semana cae en diciembre, 0 en otro caso.
# Criterio puramente de calendario (mes de la fecha), verificable.
base$Dic <- ifelse(format(base$Week, "%m") == "12", 1, 0)
table(base$Dic)  # cuantas semanas quedan en cada grupo

m4_sales  <- lm(as.formula(paste("Sales ~",  canales, "+", controles, "+ Dic")), data = base)
m4_orders <- lm(as.formula(paste("Orders ~", canales, "+", controles, "+ Dic")), data = base)

summary(m4_sales)
summary(m4_orders)

# ---------------------------------------------------------------------
# 3. COMPARACION DE LOS CAMINOS A Y B (muestra comun):
#    MAE y RMSE en la ESCALA ORIGINAL del KPI, con separacion temporal
#    entrenamiento (2024) / evaluacion (2025-2026)
# ---------------------------------------------------------------------
corte <- as.Date("2025-01-01")
train <- subset(base, Week <  corte)   # entrenamiento: semanas de 2024
test  <- subset(base, Week >= corte)   # evaluacion: 2025-01-01 en adelante
stopifnot(nrow(train) > 0, nrow(test) > 0)

# Reestimar ambos modelos SOLO con el periodo de entrenamiento
m3_sales_tr <- lm(as.formula(paste("log(Sales) ~", canales, "+", controles)), data = train)
m4_sales_tr <- lm(as.formula(paste("Sales ~", canales, "+", controles, "+ Dic")), data = train)

# Predicciones en evaluacion, llevadas a la escala original del KPI.
# El modelo (4) ya predice en la escala original. Para el modelo (3),
# que predice log(Sales), se muestran DOS retransformaciones:
pred_m4 <- predict(m4_sales_tr, newdata = test)

# (a) Baseline: exp() directo de la prediccion.
pred_m3_exp <- exp(predict(m3_sales_tr, newdata = test))

# (b) Con factor de correccion ("smearing") estimado SOLO con los
#     residuos de entrenamiento: multiplicar por mean(exp(residuos)).
res_train  <- resid(m3_sales_tr)
smearing   <- mean(exp(res_train))
pred_m3_sm <- pred_m3_exp * smearing

mae  <- function(real, pred) mean(abs(real - pred))
rmse <- function(real, pred) sqrt(mean((real - pred)^2))

comparacion_sales <- data.frame(
  Modelo = c("Modelo 3 (log, exp directo)",
             "Modelo 3 (log, exp con smearing)",
             "Modelo 4 (dummies)"),
  MAE    = c(mae(test$Sales, pred_m3_exp),
             mae(test$Sales, pred_m3_sm),
             mae(test$Sales, pred_m4)),
  RMSE   = c(rmse(test$Sales, pred_m3_exp),
             rmse(test$Sales, pred_m3_sm),
             rmse(test$Sales, pred_m4))
)
comparacion_sales

# Lo mismo para Orders
m3_orders_tr <- lm(as.formula(paste("log(Orders) ~", canales, "+", controles)), data = train)
m4_orders_tr <- lm(as.formula(paste("Orders ~", canales, "+", controles, "+ Dic")), data = train)
pred_m4_o   <- predict(m4_orders_tr, newdata = test)
pred_m3_o_e <- exp(predict(m3_orders_tr, newdata = test))
pred_m3_o_s <- pred_m3_o_e * mean(exp(resid(m3_orders_tr)))
comparacion_orders <- data.frame(
  Modelo = c("Modelo 3 (log, exp directo)",
             "Modelo 3 (log, exp con smearing)",
             "Modelo 4 (dummies)"),
  MAE    = c(mae(test$Orders, pred_m3_o_e),
             mae(test$Orders, pred_m3_o_s),
             mae(test$Orders, pred_m4_o)),
  RMSE   = c(rmse(test$Orders, pred_m3_o_e),
             rmse(test$Orders, pred_m3_o_s),
             rmse(test$Orders, pred_m4_o))
)
comparacion_orders

# CONCLUSIONES DEL PUNTO 7
# El modelo con la variable de diciembre presento menores errores
# de prediccion que el modelo en logaritmos, tanto para ventas
# como para ordenes, en el periodo evaluado. Esto favorece su uso
# para representar los picos de demanda. Search y Sponsored
# mantienen asociaciones positivas y significativas en este
# modelo, mientras que DSP no resulta significativo. No se
# comparan directamente los R2 porque los modelos usan escalas
# distintas. La eleccion de diciembre se baso en los picos
# observados, por lo que el resultado debe tomarse con cautela
# y no demuestra efectos causales de la publicidad.