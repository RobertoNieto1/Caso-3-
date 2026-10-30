
# CASO 3 - RENT CAR
# PUNTO 3: VARIABLES DE CONTROL EXTERNAS
setwd("C:/Users/USUARIO/Documents/GitHub/Caso-3-")
ruta = setwd("C:/Users/USUARIO/Documents/GitHub/Caso-3-")

# Importar la base ampliada de Amazon
datos <- read.csv(
  "Base_RentCar_Amazon.csv",
  check.names = FALSE
)

# Revisar dimensiones
dim(datos)

# Revisar las fechas disponibles
range(as.Date(datos$Week))


# Importar tasa de desempleo de Estados Unidos
UNRATE <- read.csv("UNRATE.csv")

# Revisar primeras 6 observaciones
head(UNRATE)

# Revisar dimensiones
dim(UNRATE)

# Revisar nombres de columnas
names(UNRATE)


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

# Mostrar los últimos 10 registros
tail(UNRATE_reciente, 10)

# Contar meses disponibles
nrow(UNRATE_reciente)


# Preparar UNRATE para la integracion semanal

# Crear una fecha de referencia para cada dato
# (primer dia del mes siguiente)
UNRATE_reciente$mes_siguiente <- as.Date(
  format(
    UNRATE_reciente$observation_date + 35,
    "%Y-%m-01"
  )
)

# Revisar las fechas de referencia
head(UNRATE_reciente[, c(
  "observation_date",
  "mes_siguiente",
  "UNRATE"
)])


# Comprobar semanas de la base Rent Car

fechas_rentcar <- as.Date(datos$Week)

# Revisar primeras y ultimas semanas
head(fechas_rentcar)
tail(fechas_rentcar)

# Comprobar que mantenemos 135 semanas
length(fechas_rentcar)


# Identificar el mes de cada semana de Rent Car

datos$Mes <- format(
  as.Date(datos$Week),
  "%Y-%m"
)

# Revisar las primeras 10 semanas
head(datos[, c("Week", "Mes")], 10)

# Comprobar dimensiones
dim(datos)


# Ejemplo: desempleo de enero de 2024
# Publicado el 2 de febrero de 2024

fecha_publicacion <- as.Date("2024-02-02")

# Ver semanas en las que ya se conocia el dato
datos[datos$Week >= fecha_publicacion &
        datos$Week < as.Date("2024-03-01"),
      c("Week", "Mes")]



# Crear columna semanal de desempleo
datos$UNRATE_Control <- NA_real_

# Identificar semanas posteriores a la publicacion
semanas_validas <- as.Date(datos$Week) >=
  as.Date("2024-02-02") &
  as.Date(datos$Week) < as.Date("2024-03-08")

# Incorporar tasa de enero de 2024
datos$UNRATE_Control[semanas_validas] <- 3.7

# Verificar la asignacion
datos[semanas_validas, c("Week", "UNRATE_Control")]


# Crear tabla de control de publicaciones UNRATE
calendario_unrate <- UNRATE_reciente[, c(
  "observation_date",
  "UNRATE"
)]

# Crear columna para las fechas oficiales
calendario_unrate$fecha_publicacion <- as.Date(NA)

# Incorporar la fecha ya verificada
calendario_unrate$fecha_publicacion[
  calendario_unrate$observation_date ==
    as.Date("2024-01-01")
] <- as.Date("2024-02-02")

# Revisar las primeras seis filas
head(calendario_unrate)


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

# Incorporar fechas al calendario
posiciones <- match(
  meses_2026,
  calendario_unrate$observation_date
)

calendario_unrate$fecha_publicacion[posiciones] <-
  publicaciones_2026

# Comprobar las ultimas ocho filas
tail(calendario_unrate, 8)


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

# Incorporar fechas al calendario
posiciones_2024 <- match(
  meses_2024,
  calendario_unrate$observation_date
)

calendario_unrate$fecha_publicacion[posiciones_2024] <-
  publicaciones_2024

# Comprobar las primeras 12 filas
head(calendario_unrate, 12)


# Fechas de publicacion UNRATE - 2025

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

# Incorporar fechas al calendario
posiciones_2025 <- match(
  meses_2025,
  calendario_unrate$observation_date
)

calendario_unrate$fecha_publicacion[posiciones_2025] <-
  publicaciones_2025

# Comprobar registros de 2025
subset(
  calendario_unrate,
  format(observation_date, "%Y") == "2025"
)


# PUNTO 3 - Integracion semanal de UNRATE

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

# Comprobar resultados
head(datos[, c("Week", "UNRATE_Control")], 10)

# Verificar dimensiones
dim(datos)

# Contar semanas sin dato previo disponible
sum(is.na(datos$UNRATE_Control))


# Completar enero con el ultimo dato publicado

# Identificar semanas anteriores al 2 de febrero
semanas_enero <- as.Date(datos$Week) <
  as.Date("2024-02-02")

# Asignar tasa publicada el 5 de enero
datos$UNRATE_Control[semanas_enero] <- 3.7

# Comprobar las primeras seis semanas
head(datos[, c("Week", "UNRATE_Control")])

# Comprobar valores faltantes
sum(is.na(datos$UNRATE_Control))

# Comprobar dimensiones
dim(datos)


# Importar precio de gasolina de Estados Unidos
GASREGW <- read.csv("GASREGW.csv")

# Revisar las primeras 6 observaciones
head(GASREGW)

# Revisar dimensiones
dim(GASREGW)

# Revisar nombres de columnas
names(GASREGW)

# Revisar las fechas disponibles
tail(GASREGW)


# Convertir las fechas de gasolina
GASREGW$observation_date <- as.Date(
  GASREGW$observation_date
)

# Revisar frecuencia de los datos
table(as.numeric(diff(GASREGW$observation_date)))

# Revisar primera y ultima fecha
range(GASREGW$observation_date)

# Revisar primeras seis observaciones
head(GASREGW)


# Importar nuevamente GASREGW con frecuencia semanal
GASREGW <- read.csv("GASREGW.csv")

# Convertir las fechas
GASREGW$observation_date <- as.Date(
  GASREGW$observation_date
)

# Revisar numero de observaciones
dim(GASREGW)

# Comprobar frecuencia de los datos
table(as.numeric(diff(GASREGW$observation_date)))

# Revisar primeras y ultimas fechas
range(GASREGW$observation_date)


# PUNTO 3 - Integracion semanal de gasolina

# Ordenar las observaciones por fecha
GASREGW <- GASREGW[
  order(GASREGW$observation_date),
]

# Buscar el ultimo precio disponible
posicion_gasolina <- findInterval(
  as.numeric(as.Date(datos$Week)),
  as.numeric(GASREGW$observation_date)
)

# Crear columna de precio de gasolina
datos$GASREGW_Control <- NA_real_

validas_gasolina <- posicion_gasolina > 0

datos$GASREGW_Control[validas_gasolina] <-
  GASREGW$GASREGW[posicion_gasolina[validas_gasolina]]

# Revisar primeros seis resultados
head(datos[, c("Week", "GASREGW_Control")])

# Verificar dimensiones
dim(datos)

# Comprobar valores faltantes
sum(is.na(datos$GASREGW_Control))


# Importar Google Trends Non-Brand
# Periodo enero a junio de 2024

trends_nonbrand_1 <- read.csv(
  "Trends_NonBrand_EneJun2024.csv.csv",,
  skip = 2,
  check.names = FALSE
)

# Revisar primeras observaciones
head(trends_nonbrand_1)

# Revisar cantidad de filas y columnas
dim(trends_nonbrand_1)

# Revisar nombres de columnas
names(trends_nonbrand_1)


# Corregir importacion de Google Trends Non-Brand

trends_nonbrand_1 <- read.csv(
  "Trends_NonBrand_EneJun2024.csv.csv",
  skip = 1,
  check.names = FALSE
)

# Comprobar resultados
head(trends_nonbrand_1)

# Revisar dimensiones
dim(trends_nonbrand_1)

# Revisar nombres de columnas
names(trends_nonbrand_1)


# Corregir definitivamente Google Trends Non-Brand

trends_nonbrand_1 <- read.csv(
  "Trends_NonBrand_EneJun2024.csv.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "NonBrand"),
  check.names = FALSE
)

# Convertir fechas y valores
trends_nonbrand_1$Week <- as.Date(
  trends_nonbrand_1$Week
)

trends_nonbrand_1$NonBrand <- as.numeric(
  trends_nonbrand_1$NonBrand
)

# Comprobar resultados
head(trends_nonbrand_1)
dim(trends_nonbrand_1)
tail(trends_nonbrand_1)


# Importar segunda parte de Google Trends Non-Brand

trends_nonbrand_2 <- read.csv(
  "Trends_NonBrand_Jun2024_Ago2026.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "NonBrand"),
  check.names = FALSE
)

# Convertir fechas y valores
trends_nonbrand_2$Week <- as.Date(
  trends_nonbrand_2$Week
)

trends_nonbrand_2$NonBrand <- as.numeric(
  trends_nonbrand_2$NonBrand
)

# Comprobar resultados
head(trends_nonbrand_2)
dim(trends_nonbrand_2)
tail(trends_nonbrand_2)


# Revisar el formato original de Google Trends

# Mostrar las primeras 8 fechas
head(trends_nonbrand_2$Week, 8)

# Mostrar las ultimas 6 fechas
tail(trends_nonbrand_2$Week, 6)

# Revisar nombres de columnas
names(trends_nonbrand_2)


# Corregir segunda parte de Google Trends Non-Brand

# Eliminar fila del encabezado
trends_nonbrand_2 <- trends_nonbrand_2[
  trends_nonbrand_2$Week != "Semana",
]

# Convertir las fechas
trends_nonbrand_2$Week <- as.Date(
  trends_nonbrand_2$Week
)

# Convertir los indices a numeros
trends_nonbrand_2$NonBrand <- as.numeric(
  trends_nonbrand_2$NonBrand
)

# Comprobar resultados
head(trends_nonbrand_2)
dim(trends_nonbrand_2)
tail(trends_nonbrand_2)


# Comparar semana compartida entre las dos descargas

fecha_comun <- as.Date("2024-06-30")

# Valor de la primera descarga
trends_nonbrand_1[
  trends_nonbrand_1$Week == fecha_comun,
]

# Valor de la segunda descarga
trends_nonbrand_2[
  trends_nonbrand_2$Week == fecha_comun,
]


# Calcular factor de ajuste de Google Trends

factor_ajuste <- 100 / 80

# Ajustar segunda descarga a la escala de la primera
trends_nonbrand_2$NonBrand_Ajustado <-
  trends_nonbrand_2$NonBrand * factor_ajuste

# Comprobar la semana compartida
trends_nonbrand_2[
  trends_nonbrand_2$Week == as.Date("2024-06-30"),
  c("Week", "NonBrand", "NonBrand_Ajustado")
]

# Revisar los primeros seis resultados
head(trends_nonbrand_2)


# Unir las dos partes de Google Trends Non-Brand

# Preparar primera descarga
parte_1 <- trends_nonbrand_1[, c("Week", "NonBrand")]

# Preparar segunda descarga ajustada
parte_2 <- data.frame(
  Week = trends_nonbrand_2$Week,
  NonBrand = trends_nonbrand_2$NonBrand_Ajustado
)

# Eliminar fechas que ya estan en la primera descarga
parte_2 <- parte_2[
  !parte_2$Week %in% parte_1$Week,
]

# Unir ambas partes
trends_nonbrand_unido <- rbind(parte_1, parte_2)

# Ordenar cronologicamente
trends_nonbrand_unido <- trends_nonbrand_unido[
  order(trends_nonbrand_unido$Week),
]

# Comprobar resultados
dim(trends_nonbrand_unido)
head(trends_nonbrand_unido)
tail(trends_nonbrand_unido)

# Revisar fechas duplicadas
sum(duplicated(trends_nonbrand_unido$Week))


# Importar tercera parte de Google Trends Non-Brand
# Periodo enero a agosto de 2026

trends_nonbrand_3 <- read.csv(
  "Trends_NonBrand_2026.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "NonBrand"),
  check.names = FALSE
)

# Revisar primeras observaciones
head(trends_nonbrand_3)

# Revisar dimensiones
dim(trends_nonbrand_3)

# Revisar ultimas observaciones
tail(trends_nonbrand_3)

# Revisar nombres de columnas
names(trends_nonbrand_3)


# Revisar tercera descarga de Google Trends

# Mostrar las primeras 8 filas
head(trends_nonbrand_3, 8)

# Mostrar las ultimas 8 filas
tail(trends_nonbrand_3, 8)

# Comprobar cantidad de observaciones
dim(trends_nonbrand_3)


# Revisar fechas de Google Trends 2026

# Mostrar las primeras 5 filas
head(trends_nonbrand_3, 5)

# Mostrar las ultimas 5 filas
tail(trends_nonbrand_3, 5)


# Revisar el inicio de Google Trends 2026
head(trends_nonbrand_3, 8)

# Revisar si hay encabezados dentro de los datos
sum(trends_nonbrand_3$Week == "Semana",
    na.rm = TRUE)


# Revisar las primeras semanas de Google Trends 2026
head(trends_nonbrand_3, 8)

# Revisar los tipos de datos
str(trends_nonbrand_3)


# Convertir las fechas de la tercera descarga
trends_nonbrand_3$Week <- as.Date(
  trends_nonbrand_3$Week
)

# Comparar las dos descargas en las semanas comunes
comparacion <- merge(
  trends_nonbrand_2[, c("Week", "NonBrand_Ajustado")],
  trends_nonbrand_3,
  by = "Week"
)

# Mostrar las semanas coincidentes
print(comparacion)

# Contar las semanas compartidas
nrow(comparacion)


# Calcular factor de ajuste con las 7 semanas comunes

factor_ajuste_3 <- sum(
  comparacion$NonBrand_Ajustado * comparacion$NonBrand
) / sum(
  comparacion$NonBrand^2
)

# Mostrar factor de ajuste
factor_ajuste_3

# Ajustar la tercera descarga
trends_nonbrand_3$NonBrand_Ajustado <-
  trends_nonbrand_3$NonBrand * factor_ajuste_3

# Comparar los valores ajustados
head(trends_nonbrand_3[, c(
  "Week", "NonBrand", "NonBrand_Ajustado"
)])


# Unir la tercera descarga con Non-Brand

# Preparar los datos ajustados
parte_3 <- data.frame(
  Week = trends_nonbrand_3$Week,
  NonBrand = trends_nonbrand_3$NonBrand_Ajustado
)

# Conservar solamente las semanas nuevas
parte_3 <- parte_3[
  !parte_3$Week %in% trends_nonbrand_unido$Week,
]

# Unir las tres descargas
trends_nonbrand_final <- rbind(
  trends_nonbrand_unido,
  parte_3
)

# Ordenar por fecha
trends_nonbrand_final <- trends_nonbrand_final[
  order(trends_nonbrand_final$Week),
]

# Comprobar dimensiones
dim(trends_nonbrand_final)

# Verificar fechas inicial y final
range(trends_nonbrand_final$Week)

# Revisar duplicados
sum(duplicated(trends_nonbrand_final$Week))


# Verificar cantidad total de semanas
dim(trends_nonbrand_final)

# Verificar primera y ultima fecha
range(trends_nonbrand_final$Week)

# Comprobar que coincida con Rent Car
all(as.Date(datos$Week) %in% trends_nonbrand_final$Week)

# Verificar valores faltantes
sum(is.na(trends_nonbrand_final$NonBrand))


# Integrar Google Trends Non-Brand a Rent Car

# Conservar la cantidad original de semanas
n_original <- nrow(datos)

# Unir utilizando la fecha semanal
datos <- merge(
  datos,
  trends_nonbrand_final,
  by = "Week",
  all.x = TRUE,
  sort = FALSE
)

# Ordenar cronologicamente
datos <- datos[order(as.Date(datos$Week)), ]

# Comprobar dimensiones
dim(datos)

# Verificar valores faltantes
sum(is.na(datos$NonBrand))

# Verificar que no perdimos semanas
nrow(datos) == n_original


# Verificar integracion de Non-Brand

# Comprobar dimensiones
dim(datos)

# Comprobar valores faltantes
sum(is.na(datos$NonBrand))

# Revisar primeras y ultimas observaciones
head(datos[, c("Week", "NonBrand")], 3)
tail(datos[, c("Week", "NonBrand")], 3)


# PUNTO 3 - Importar Google Trends Brand

# Importar archivo de busquedas de marca
trends_brand <- read.csv(
  "Trends_Brand.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

# Revisar primeras observaciones
head(trends_brand)

# Revisar ultimas observaciones
tail(trends_brand)

# Revisar dimensiones
dim(trends_brand)


# PUNTO 3 - Google Trends Brand 2024

# Importar datos de busquedas de marca
brand_2024 <- read.csv(
  "Trends_Brand_2024.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

# Revisar primeras observaciones
head(brand_2024)

# Revisar ultimas observaciones
tail(brand_2024)

# Revisar dimensiones
dim(brand_2024)


# Revisar las fechas de Brand 2024
head(brand_2024, 3)

tail(brand_2024, 3)

# Comprobar si hay encabezados adicionales
sum(brand_2024$Week == "Semana", na.rm = TRUE)


# Convertir las fechas de Brand 2024
brand_2024$Week <- as.Date(brand_2024$Week)

# Verificar primera y ultima fecha
range(brand_2024$Week)

# Comprobar que los registros sean semanales
table(as.numeric(diff(brand_2024$Week)))


# Verificar fechas de Brand 2024
range(brand_2024$Week)


# PUNTO 3 - Google Trends Brand 2025

# Importar busquedas de marca de 2025
brand_2025 <- read.csv(
  "Trends_Brand_2025.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

# Revisar primeras y ultimas observaciones
head(brand_2025, 3)
tail(brand_2025, 3)

# Revisar dimensiones
dim(brand_2025)


# Convertir fechas de Brand 2025
brand_2025$Week <- as.Date(brand_2025$Week)

# Comprobar primera y ultima fecha
range(brand_2025$Week)

# Comprobar frecuencia semanal
table(as.numeric(diff(brand_2025$Week)))

# Revisar fechas duplicadas
sum(duplicated(brand_2025$Week))


# Comparar semana compartida de Brand

fecha_comun_brand <- as.Date("2024-12-29")

# Valor de Brand 2024
brand_2024[
  brand_2024$Week == fecha_comun_brand,
  c("Week", "Brand")
]

# Valor de Brand 2025
brand_2025[
  brand_2025$Week == fecha_comun_brand,
  c("Week", "Brand")
]


# Valor de Brand 2024 en la semana compartida
brand_2024[
  brand_2024$Week == as.Date("2024-12-29"),
  c("Week", "Brand")
]


# Ajustar Brand 2025 a la escala de 2024

factor_brand_2025 <- 65 / 69

# Aplicar el factor de ajuste
brand_2025$Brand_Ajustado <-
  brand_2025$Brand * factor_brand_2025

# Comprobar la semana compartida
brand_2025[
  brand_2025$Week == as.Date("2024-12-29"),
  c("Week", "Brand", "Brand_Ajustado")
]


# Unir las series Brand de 2024 y 2025

# Preparar datos de 2024
parte_brand_2024 <- brand_2024[, c("Week", "Brand")]

# Preparar datos ajustados de 2025
parte_brand_2025 <- data.frame(
  Week = brand_2025$Week,
  Brand = brand_2025$Brand_Ajustado
)

# Eliminar semanas repetidas
parte_brand_2025 <- parte_brand_2025[
  !parte_brand_2025$Week %in% parte_brand_2024$Week,
]

# Unir las dos series
brand_unido <- rbind(
  parte_brand_2024,
  parte_brand_2025
)

# Ordenar por fecha
brand_unido <- brand_unido[
  order(brand_unido$Week),
]

# Comprobar resultados
dim(brand_unido)
range(brand_unido$Week)
sum(duplicated(brand_unido$Week))


# PUNTO 3 - Google Trends Brand 2026

# Importar busquedas de marca
brand_2026 <- read.csv(
  "Trends_Brand_2026.csv",
  skip = 1,
  header = FALSE,
  col.names = c("Week", "Brand"),
  check.names = FALSE
)

# Revisar primeras y ultimas observaciones
head(brand_2026, 3)
tail(brand_2026, 3)

# Revisar dimensiones
dim(brand_2026)


# Convertir fechas de Brand 2026
brand_2026$Week <- as.Date(brand_2026$Week)

# Comprobar primera y ultima fecha
range(brand_2026$Week)

# Comprobar frecuencia semanal
table(as.numeric(diff(brand_2026$Week)))

# Revisar fechas duplicadas
sum(duplicated(brand_2026$Week))


# Comparar Brand 2025 y 2026

fecha_comun_2026 <- as.Date("2025-12-28")

# Valor ajustado de Brand 2025
brand_unido[
  brand_unido$Week == fecha_comun_2026,
  c("Week", "Brand")
]

# Valor original de Brand 2026
brand_2026[
  brand_2026$Week == fecha_comun_2026,
  c("Week", "Brand")
]


# Comparar los valores de la semana compartida

brand_unido[
  brand_unido$Week == as.Date("2025-12-28"),
  c("Week", "Brand")
]

brand_2026[
  brand_2026$Week == as.Date("2025-12-28"),
  c("Week", "Brand")
]


# Mostrar ambos valores en una sola tabla

data.frame(
  Serie = c("Brand 2024-2025", "Brand 2026"),
  Valor = c(
    brand_unido$Brand[
      brand_unido$Week == as.Date("2025-12-28")
    ],
    brand_2026$Brand[
      brand_2026$Week == as.Date("2025-12-28")
    ]
  )
)


# Ajustar Brand 2026 a la escala de 2024-2025

# Calcular el factor usando la semana compartida
factor_brand_2026 <-
  brand_unido$Brand[
    brand_unido$Week == as.Date("2025-12-28")
  ] /
  brand_2026$Brand[
    brand_2026$Week == as.Date("2025-12-28")
  ]

# Ajustar los valores de 2026
brand_2026$Brand_Ajustado <-
  brand_2026$Brand * factor_brand_2026

# Comprobar la semana compartida
brand_2026[
  brand_2026$Week == as.Date("2025-12-28"),
  c("Week", "Brand", "Brand_Ajustado")
]

# Mostrar el factor
factor_brand_2026


# Unir las tres partes de Google Trends Brand

# Preparar los valores ajustados de 2026
parte_brand_2026 <- data.frame(
  Week = brand_2026$Week,
  Brand = brand_2026$Brand_Ajustado
)

# Eliminar semanas que ya tenemos
parte_brand_2026 <- parte_brand_2026[
  !parte_brand_2026$Week %in% brand_unido$Week,
]

# Unir las tres descargas
brand_final <- rbind(
  brand_unido,
  parte_brand_2026
)

# Ordenar cronologicamente
brand_final <- brand_final[
  order(brand_final$Week),
]

# Comprobar resultados
dim(brand_final)

range(brand_final$Week)

sum(duplicated(brand_final$Week))

sum(is.na(brand_final$Brand))


# Integrar Brand a la base Rent Car

# Guardar cantidad original de observaciones
n_antes <- nrow(datos)

# Unir Brand por fecha semanal
datos <- merge(
  datos,
  brand_final,
  by = "Week",
  all.x = TRUE,
  sort = FALSE
)

# Ordenar cronologicamente
datos <- datos[order(as.Date(datos$Week)), ]

# Comprobar dimensiones
dim(datos)

# Comprobar valores faltantes en Brand
sum(is.na(datos$Brand))

# Verificar que conservamos todas las semanas
nrow(datos) == n_antes

# Revisar las cuatro variables externas
head(datos[, c(
  "Week",
  "UNRATE_Control",
  "GASREGW_Control",
  "NonBrand",
  "Brand"
)])


# Verificacion final de las variables externas

# Comprobar dimensiones
dim(datos)

# Contar valores faltantes por variable
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

# Verificar dimensiones
dim(datos)


# Guardar base final en el repositorio compartido

write.csv(
  datos,
  file.path(ruta_repositorio, "Base_RentCar_Final.csv"),
  row.names = FALSE
)

# Verificar que se guardo en GitHub
file.exists(
  file.path(ruta_repositorio, "Base_RentCar_Final.csv")
)


# Comprobar archivos externos necesarios

archivos <- c(
  "UNRATE.csv",
  "GASREGW.csv",
  "Trends_NonBrand_EneJun2024.csv.csv",
  "Trends_NonBrand_Jun2024_Ago2026.csv",
  "Trends_NonBrand_2026.csv",
  "Trends_Brand_2024.csv",
  "Trends_Brand_2025.csv",
  "Trends_Brand_2026.csv"
)

data.frame(
  Archivo = archivos,
  Existe = file.exists(archivos)
)


# Copiar los 8 archivos externos al repositorio

# Obtener la carpeta del repositorio
ruta_repositorio <- dirname(carpeta_github)

# Copiar archivos conservando los originales
resultado_copia <- file.copy(
  from = archivos,
  to = ruta_repositorio,
  overwrite = FALSE
)

# Comprobar si todos se copiaron
data.frame(
  Archivo = archivos,
  Copiado = resultado_copia
)


# Verificar archivos necesarios en GitHub

archivos_necesarios <- c(
  "Base_RentCar_Amazon.csv",
  "UNRATE.csv",
  "GASREGW.csv",
  "Trends_NonBrand_EneJun2024.csv.csv",
  "Trends_NonBrand_Jun2024_Ago2026.csv",
  "Trends_NonBrand_2026.csv",
  "Trends_Brand_2024.csv",
  "Trends_Brand_2025.csv",
  "Trends_Brand_2026.csv",
  "Fuentes_y_Metodologia.txt"
)

data.frame(
  Archivo = archivos_necesarios,
  En_GitHub = file.exists(
    file.path(ruta_repositorio, archivos_necesarios)
  )
)


# Copiar documento de metodologia a GitHub

file.copy(
  "Fuentes_y_Metodologia.txt",
  file.path(ruta_repositorio, "Fuentes_y_Metodologia.txt"),
  overwrite = FALSE
)

# Comprobar que quedo en el repositorio
file.exists(
  file.path(ruta_repositorio, "Fuentes_y_Metodologia.txt")
)
