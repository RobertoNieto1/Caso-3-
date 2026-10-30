# CASO 3 - RENT CAR
# PUNTO 5: REGRESIONES SIMPLES - UNA PRIMERA LECTURA

datos <- read.csv("Base_RentCar_Final.csv",
                  check.names = FALSE)

# Modelo 1a: Sales ~ costo de Amazon DSP
modelo_dsp <- lm(Sales ~ Amazon_DSP_Cost, data = datos)
summary(modelo_dsp)
confint(modelo_dsp, level = 0.95)

# Modelo 1b: Sales ~ costo de Amazon Search
modelo_search <- lm(Sales ~ Amazon_Search_Cost, data = datos)
summary(modelo_search)
confint(modelo_search, level = 0.95)

# Modelo 1c: Sales ~ costo de Amazon Sponsored
modelo_sponsored <- lm(Sales ~ Amazon_Sponsored_Cost,
                       data = datos)
summary(modelo_sponsored)
confint(modelo_sponsored, level = 0.95)

# CONCLUSIONES DEL PUNTO 5
# Se estimaron tres regresiones simples para relacionar las ventas
# con el costo de cada canal de Amazon por separado. En los tres
# casos la asociacion es positiva y significativa. Search y
# Sponsored explican alrededor del 55% de la variacion de las
# ventas, frente al 29% de DSP. Esto no demuestra que un canal
# sea mas efectivo, ya que los modelos no consideran otros
# factores ni la inversion simultanea de los demas canales.
# Por tanto, los resultados muestran asociaciones y no efectos
# causales ni retornos incrementales de la publicidad.