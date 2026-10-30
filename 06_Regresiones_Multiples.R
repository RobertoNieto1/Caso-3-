# CASO 3 - RENT CAR
# PUNTO 6: REGRESION MULTIPLE CON CONTROLES

datos <- read.csv("tables/Base_RentCar_Final.csv",
                  check.names = FALSE)

# ---------------------------------------------------------
# 1. EVALUAR LA PERTINENCIA Y REDUNDANCIA DE LOS CONTROLES
# ---------------------------------------------------------

regresores <- c(
  "Amazon_DSP_Cost", "Amazon_Search_Cost",
  "Amazon_Sponsored_Cost",
  "SP500", "WEI", "UNRATE_Control",
  "GASREGW_Control", "NonBrand", "Brand"
)

# Correlaciones entre regresores
cor_regresores <- cor(datos[, regresores])
print(round(cor_regresores, 2))

# VIF en R base: 1 / (1 - R2 de cada regresor contra los demas)
vif_manual <- sapply(regresores, function(v) {
  otros <- setdiff(regresores, v)
  formula_aux <- reformulate(otros, response = v)
  r2 <- summary(lm(formula_aux, data = datos))$r.squared
  1 / (1 - r2)
})
print(round(vif_manual, 2))

# ---------------------------------------------------------
# 2. ESPECIFICACION COMPLETA (BASE) - SALES
# ---------------------------------------------------------
# Propuesta de especificacion principal: los tres costos de
# Amazon (los exige el modelo (2) del enunciado) mas los seis
# controles documentados en el punto 3. Cada control tiene una
# hipotesis economica: SP500 y WEI (actividad economica),
# UNRATE (mercado laboral), GASREGW (costo de viajar),
# NonBrand (intencion de demanda generica) y Brand (interes de
# marca).

modelo_sales <- lm(
  Sales ~ Amazon_DSP_Cost + Amazon_Search_Cost +
    Amazon_Sponsored_Cost + SP500 + WEI +
    UNRATE_Control + GASREGW_Control +
    NonBrand + Brand,
  data = datos
)
summary(modelo_sales)
confint(modelo_sales, level = 0.95)

# ---------------------------------------------------------
# 3. ESPECIFICACION COMPLETA (BASE) - ORDERS
# ---------------------------------------------------------

modelo_orders <- lm(
  Orders ~ Amazon_DSP_Cost + Amazon_Search_Cost +
    Amazon_Sponsored_Cost + SP500 + WEI +
    UNRATE_Control + GASREGW_Control +
    NonBrand + Brand,
  data = datos
)
summary(modelo_orders)
confint(modelo_orders, level = 0.95)

# ---------------------------------------------------------
# 4. ESPECIFICACION DE SENSIBILIDAD (ROBUSTEZ)
# ---------------------------------------------------------
# SP500 y UNRATE_Control se correlacionan entre si (r = 0,77)
# y con WEI (r = 0,62 y 0,50). La sensibilidad deja a WEI como
# unico control de actividad economica y retira SP500 y
# UNRATE. Los tres costos de Amazon se mantienen en ambas:
# son regresores obligatorios del modelo (2).

modelo_sales_sens <- lm(
  Sales ~ Amazon_DSP_Cost + Amazon_Search_Cost +
    Amazon_Sponsored_Cost + WEI + GASREGW_Control +
    NonBrand + Brand,
  data = datos
)
summary(modelo_sales_sens)

modelo_orders_sens <- lm(
  Orders ~ Amazon_DSP_Cost + Amazon_Search_Cost +
    Amazon_Sponsored_Cost + WEI + GASREGW_Control +
    NonBrand + Brand,
  data = datos
)
summary(modelo_orders_sens)

# CONCLUSION DEL PUNTO 6
# Se estimaron dos regresiones multiples, una para ventas y otra
# para ordenes, incluyendo los tres canales de Amazon y controles
# de actividad economica, desempleo, gasolina e interes de busqueda.
# Ambos modelos explican cerca del 67% de la variacion observada.
# Al considerar todos los factores juntos, Search y Sponsored
# mantienen una asociacion positiva y significativa con ambos
# resultados, mientras que DSP no resulta significativo. NonBrand
# presenta una asociacion negativa y significativa; los demas
# controles no son significativos al 5%. Como Search y Sponsored
# estan muy relacionados entre si, sus resultados deben leerse
# con cautela.

# Al retirar SP500 y desempleo, se mantienen los signos
# y la significancia de los canales y el ajuste cambia poco.
# Se conserva el modelo completo como referencia y el reducido
# como comparacion. Estos resultados muestran asociaciones,
# pero no demuestran efectos causales de la publicidad.
