# CASO 3 - RENT CAR
# PUNTO 3: VARIABLES DE CONTROL EXTERNAS

# Importar la base ampliada de Amazon
datos <- read.csv("Base_RentCar_Amazon.csv",
                  check.names = FALSE)

# Revisar dimensiones
dim(datos)

# Revisar las fechas disponibles
range(as.Date(datos$Week))


# ============================================================
# UNRATE: tasa de desempleo de Estados Unidos (FRED/BLS)
# ============================================================
# UNRATE - Tasa de desempleo de Estados Unidos
# Fuente original: U.S. Bureau of Labor Statistics (BLS).
# Disponible en FRED, Federal Reserve Bank of St. Louis.
# Serie: UNRATE.
# Unidades: porcentaje, ajustada por estacionalidad.
# Frecuencia original: mensual.
# Cobertura: Estados Unidos.
# Referencia: U.S. Bureau of Labor Statistics, Unemployment Rate
# [UNRATE], FRED, Federal Reserve Bank of St. Louis.
# https://fred.stlouisfed.org/series/UNRATE
# Fecha de consulta: 30 de octubre de 2026.

# Importar tasa de desempleo de Estados Unidos
UNRATE <- read.csv("UNRATE.csv")

# Convertir las fechas de UNRATE
UNRATE$observation_date <- as.Date(
  UNRATE$observation_date
)

# Seleccionar datos desde enero de 2024
UNRATE_reciente <- subset(
  UNRATE,
  observation_date >= as.Date("2024-01-01") &
    observation_date <= as.Date("2026-08-01")
)

# Crear tabla de control de publicaciones UNRATE
calendario_unrate <- UNRATE_reciente[, c(
  "observation_date",
  "UNRATE"
)]

# Crear columna para las fechas oficiales de publicacion
calendario_unrate$fecha_publicacion <- as.Date(NA)

# Fechas oficiales de publicacion UNRATE - 2024
meses_2024 <- as.Date(c(
  "2024-01-01", "2024-02-01",
  "2024-03-01", "2024-04-01",
  "2024-05-01", "2024-06-01",
  "2024-07-01", "2024-08-01",
  "2024-09-01", "2024-10-01",
  "2024-11-01", "2024-12-01"
))

publicaciones_2024 <- as.Date(c(
  "2024-02-02", "2024-03-08",
  "2024-04-05", "2024-05-03",
  "2024-06-07", "2024-07-05",
  "2024-08-02", "2024-09-06",
  "2024-10-04", "2024-11-01",
  "2024-12-06", "2025-01-10"
))

posiciones_2024 <- match(
  meses_2024,
  calendario_unrate$observation_date
)

calendario_unrate$fecha_publicacion[posiciones_2024] <-
  publicaciones_2024

# Fechas oficiales de publicacion UNRATE - 2025
# (octubre de 2025 no tuvo estimacion oficial: fecha NA)
meses_2025 <- as.Date(c(
  "2025-01-01", "2025-02-01",
  "2025-03-01", "2025-04-01",
  "2025-05-01", "2025-06-01",
  "2025-07-01", "2025-08-01",
  "2025-09-01", "2025-10-01",
  "2025-11-01", "2025-12-01"
))

publicaciones_2025 <- as.Date(c(
  "2025-02-07", "2025-03-07",
  "2025-04-04", "2025-05-02",
  "2025-06-06", "2025-07-03",
  "2025-08-01", "2025-09-05",
  "2025-11-20", NA,
  "2025-12-16", "2026-01-09"
))

posiciones_2025 <- match(
  meses_2025,
  calendario_unrate$observation_date
)

calendario_unrate$fecha_publicacion[posiciones_2025] <-
  publicaciones_2025

# Fechas oficiales de publicacion UNRATE - 2026
meses_2026 <- as.Date(c(
  "2026-01-01", "2026-02-01",
  "2026-03-01", "2026-04-01",
  "2026-05-01", "2026-06-01",
  "2026-07-01", "2026-08-01"
))

publicaciones_2026 <- as.Date(c(
  "2026-02-11", "2026-03-06",
  "2026-04-03", "2026-05-08",
  "2026-06-05", "2026-07-02",
  "2026-08-07", "2026-09-04"
))

posiciones <- match(
  meses_2026,
  calendario_unrate$observation_date
)

calendario_unrate$fecha_publicacion[posiciones] <-
  publicaciones_2026

# Integracion semanal de UNRATE
# A cada semana se le asigna la ultima tasa YA PUBLICADA,
# para no usar informacion que aun no era conocida

# Conservar registros publicados y con tasa disponible
unrate_publicado <- calendario_unrate[
  !is.na(calendario_unrate$fecha_publicacion) &
    !is.na(calendario_unrate$UNRATE),
]

# Ordenar por fecha de publicacion
unrate_publicado <- unrate_publicado[
  order(unrate_publicado$fecha_publicacion),
]

# Buscar el ultimo dato publicado en cada semana
posicion_unrate <- findInterval(
  as.numeric(as.Date(datos$Week)),
  as.numeric(unrate_publicado$fecha_publicacion)
)

# Crear el control semanal
datos$UNRATE_Control <- NA_real_

validas <- posicion_unrate > 0

datos$UNRATE_Control[validas] <-
  unrate_publicado$UNRATE[posicion_unrate[validas]]

# Completar enero de 2024 con el ultimo dato publicado
# (tasa de diciembre de 2023, publicada el 5 de enero de 2024)
semanas_enero <- as.Date(datos$Week) <
  as.Date("2024-02-02")

datos$UNRATE_Control[semanas_enero] <- 3.7

# Comprobar valores faltantes
sum(is.na(datos$UNRATE_Control))


# ============================================================
# GASREGW: precio de la gasolina regular (FRED/EIA)
# ============================================================
# GASREGW - Precio de la gasolina regular en Estados Unidos
# Fuente original: U.S. Energy Information Administration (EIA).
# Disponible en FRED, Federal Reserve Bank of St. Louis.
# Serie: GASREGW.
# Unidades: dolares estadounidenses por galon, sin ajuste estacional.
# Frecuencia original: semanal, con fecha de lunes.
# Cobertura: Estados Unidos.
# Referencia: U.S. Energy Information Administration, US Regular
# All Formulations Gas Price [GASREGW], FRED, Federal Reserve
# Bank of St. Louis.
# https://fred.stlouisfed.org/series/GASREGW
# Fecha de consulta: 30 de octubre de 2026.

# Importar precio de gasolina de Estados Unidos
GASREGW <- read.csv("GASREGW.csv")

# Convertir las fechas de gasolina
GASREGW$observation_date <- as.Date(
  GASREGW$observation_date
)

# Ordenar las observaciones por fecha
GASREGW <- GASREGW[
  order(GASREGW$observation_date),
]

# Integracion semanal de gasolina:
# se usa la ultima observacion con fecha no posterior a la semana
posicion_gasolina <- findInterval(
  as.numeric(as.Date(datos$Week)),
  as.numeric(GASREGW$observation_date)
)

# Crear columna de precio de gasolina
datos$GASREGW_Control <- NA_real_

validas_gasolina <- posicion_gasolina > 0

datos$GASREGW_Control[validas_gasolina] <-
  GASREGW$GASREGW[posicion_gasolina[validas_gasolina]]

# Comprobar valores faltantes
sum(is.na(datos$GASREGW_Control))


# ============================================================
# GOOGLE TRENDS NON-BRAND: termino "car rental"
# Tres descargas con periodos superpuestos, unidas con
# factores de escala porque los indices son relativos
# ============================================================

# Primera descarga: enero a junio de 2024
trends_nonbrand_1 <- read.csv(
  "Trends_NonBrand_EneJun2024.csv.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "NonBrand"),
  check.names = FALSE
)

trends_nonbrand_1$Week <- as.Date(
  trends_nonbrand_1$Week
)

trends_nonbrand_1$NonBrand <- as.numeric(
  trends_nonbrand_1$NonBrand
)

# Segunda descarga: junio de 2024 a agosto de 2026
trends_nonbrand_2 <- read.csv(
  "Trends_NonBrand_Jun2024_Ago2026.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "NonBrand"),
  check.names = FALSE
)

# Eliminar la fila del encabezado interno del archivo
trends_nonbrand_2 <- trends_nonbrand_2[
  trends_nonbrand_2$Week != "Semana",
]

trends_nonbrand_2$Week <- as.Date(
  trends_nonbrand_2$Week
)

trends_nonbrand_2$NonBrand <- as.numeric(
  trends_nonbrand_2$NonBrand
)

# Semana compartida entre las dos descargas: 2024-06-30
# Primera descarga: 100; segunda: 80
factor_ajuste <- 100 / 80

# Ajustar segunda descarga a la escala de la primera
trends_nonbrand_2$NonBrand_Ajustado <-
  trends_nonbrand_2$NonBrand * factor_ajuste

# Unir las dos partes de Google Trends Non-Brand
parte_1 <- trends_nonbrand_1[, c("Week", "NonBrand")]

parte_2 <- data.frame(
  Week = trends_nonbrand_2$Week,
  NonBrand = trends_nonbrand_2$NonBrand_Ajustado
)

# Eliminar fechas que ya estan en la primera descarga
parte_2 <- parte_2[
  !parte_2$Week %in% parte_1$Week,
]

trends_nonbrand_unido <- rbind(parte_1, parte_2)

trends_nonbrand_unido <- trends_nonbrand_unido[
  order(trends_nonbrand_unido$Week),
]

# Tercera descarga: enero a agosto de 2026
trends_nonbrand_3 <- read.csv(
  "Trends_NonBrand_2026.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "NonBrand"),
  check.names = FALSE
)

trends_nonbrand_3$Week <- as.Date(
  trends_nonbrand_3$Week
)

# Comparar las dos descargas en las semanas comunes
comparacion <- merge(
  trends_nonbrand_2[, c("Week", "NonBrand_Ajustado")],
  trends_nonbrand_3,
  by = "Week"
)

# Calcular factor de ajuste con las 7 semanas comunes
factor_ajuste_3 <- sum(
  comparacion$NonBrand_Ajustado * comparacion$NonBrand
) / sum(
  comparacion$NonBrand^2
)

# Ajustar la tercera descarga
trends_nonbrand_3$NonBrand_Ajustado <-
  trends_nonbrand_3$NonBrand * factor_ajuste_3

# Unir la tercera descarga con Non-Brand
parte_3 <- data.frame(
  Week = trends_nonbrand_3$Week,
  NonBrand = trends_nonbrand_3$NonBrand_Ajustado
)

parte_3 <- parte_3[
  !parte_3$Week %in% trends_nonbrand_unido$Week,
]

trends_nonbrand_final <- rbind(
  trends_nonbrand_unido,
  parte_3
)

trends_nonbrand_final <- trends_nonbrand_final[
  order(trends_nonbrand_final$Week),
]

# Comprobar que cubra todas las semanas de Rent Car
all(as.Date(datos$Week) %in% trends_nonbrand_final$Week)
sum(is.na(trends_nonbrand_final$NonBrand))

# Integrar Google Trends Non-Brand a Rent Car
n_original <- nrow(datos)

datos <- merge(
  datos,
  trends_nonbrand_final,
  by = "Week",
  all.x = TRUE,
  sort = FALSE
)

datos <- datos[order(as.Date(datos$Week)), ]

# Verificar que no perdimos semanas
nrow(datos) == n_original
sum(is.na(datos$NonBrand))


# ============================================================
# GOOGLE TRENDS BRAND: termino "enterprise rent a car"
# Tres descargas anuales, unidas con factores de escala
# ============================================================

# Brand 2024
brand_2024 <- read.csv(
  "Trends_Brand_2024.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

brand_2024$Week <- as.Date(brand_2024$Week)

# Brand 2025
brand_2025 <- read.csv(
  "Trends_Brand_2025.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

brand_2025$Week <- as.Date(brand_2025$Week)

# Semana compartida: 2024-12-29
# Brand 2024: 65; Brand 2025: 69
factor_brand_2025 <- 65 / 69

brand_2025$Brand_Ajustado <-
  brand_2025$Brand * factor_brand_2025

# Unir las series Brand de 2024 y 2025
parte_brand_2024 <- brand_2024[, c("Week", "Brand")]

parte_brand_2025 <- data.frame(
  Week = brand_2025$Week,
  Brand = brand_2025$Brand_Ajustado
)

parte_brand_2025 <- parte_brand_2025[
  !parte_brand_2025$Week %in% parte_brand_2024$Week,
]

brand_unido <- rbind(
  parte_brand_2024,
  parte_brand_2025
)

brand_unido <- brand_unido[
  order(brand_unido$Week),
]

# Brand 2026
brand_2026 <- read.csv(
  "Trends_Brand_2026.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

brand_2026$Week <- as.Date(brand_2026$Week)

# Ajustar Brand 2026 a la escala de 2024-2025
# usando la semana compartida 2025-12-28
factor_brand_2026 <-
  brand_unido$Brand[
    brand_unido$Week == as.Date("2025-12-28")
  ] /
  brand_2026$Brand[
    brand_2026$Week == as.Date("2025-12-28")
  ]

brand_2026$Brand_Ajustado <-
  brand_2026$Brand * factor_brand_2026

# Unir las tres partes de Google Trends Brand
parte_brand_2026 <- data.frame(
  Week = brand_2026$Week,
  Brand = brand_2026$Brand_Ajustado
)

parte_brand_2026 <- parte_brand_2026[
  !parte_brand_2026$Week %in% brand_unido$Week,
]

brand_final <- rbind(
  brand_unido,
  parte_brand_2026
)

brand_final <- brand_final[
  order(brand_final$Week),
]

sum(is.na(brand_final$Brand))

# Integrar Brand a la base Rent Car
n_antes <- nrow(datos)

datos <- merge(
  datos,
  brand_final,
  by = "Week",
  all.x = TRUE,
  sort = FALSE
)

datos <- datos[order(as.Date(datos$Week)), ]

# Verificar que conservamos todas las semanas
nrow(datos) == n_antes
sum(is.na(datos$Brand))


# ============================================================
# VERIFICACION FINAL Y GUARDADO
# ============================================================

# Contar valores faltantes por variable externa
colSums(is.na(datos[, c(
  "UNRATE_Control",
  "GASREGW_Control",
  "NonBrand",
  "Brand"
)]))

# Comprobar fechas duplicadas
sum(duplicated(datos$Week))

# Revisar periodo completo
range(as.Date(datos$Week))

# Guardar base final con las cuatro variables externas
write.csv(
  datos,
  "Base_RentCar_Final.csv",
  row.names = FALSE
)

# Comprobar que el archivo se creo
file.exists("Base_RentCar_Final.csv")
