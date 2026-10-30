# CASO 3 - RENT CAR
# PUNTO 1: RECONOCIMIENTO DE LA BASE
#
# Entrada: Base_datos_caso_3.csv (en la carpeta del proyecto)
# Salida: ninguna; este script solo explora y deja el registro de ajustes
#
# Antes de correr: abrir R o RStudio con la carpeta del repositorio como
# directorio de trabajo (Session > Set Working Directory, o un .Rproj).
# No usar setwd() con rutas fijas: esas rutas solo existen en un equipo.

# Importar la base de datos original
datos <- read.csv("Base_datos_caso_3.csv",
                  check.names = FALSE)

# Revisar dimensiones
dim(datos)

# Revisar nombres de las variables
names(datos)

# Revisar las primeras 6 filas
head(datos)

# Identificar los tipos de datos
str(datos)

# Revisar datos faltantes
sum(is.na(datos))

# Revisar filas duplicadas
sum(duplicated(datos))

# Revisar el tipo de dato de cada columna
sapply(datos, class)

# Revisar las fechas de la base
head(datos$Week)

# Revisar fecha inicial y final
range(as.Date(datos$Week))

# Revisar si existen semanas repetidas
sum(duplicated(datos$Week))

# Revisar estadísticas básicas de ventas
summary(datos$Sales)

# Revisar estadísticas básicas de órdenes
summary(datos$Orders)

# Revisar si existen valores negativos
sum(datos$Sales < 0, na.rm = TRUE)
sum(datos$Orders < 0, na.rm = TRUE)

# Identificar columnas de costos publicitarios
columnas_costos <- grep("\\.Cost$", names(datos),
                        value = TRUE)

# Revisar estadísticas de los costos
summary(datos[columnas_costos])

# Contar valores negativos en los costos
sum(datos[columnas_costos] < 0, na.rm = TRUE)

# Comprobar que las fechas estén cada 7 días
fechas <- sort(as.Date(datos$Week))
table(as.numeric(diff(fechas)))


# AJUSTES - PUNTO 1
# - Dimensiones: 135 filas x 33 columnas.
# - Una fila representa una semana (Week), del 2024-01-07 al 2026-08-02.
# - Variables de respuesta: Sales y Orders.
# - Valores faltantes: 0. Filas duplicadas: 0. Semanas repetidas: 0.
# - Separacion entre semanas consecutivas: 7 dias en todos los casos.
# - Valores negativos en Sales, Orders y costos: 0.
# - Ajustes aplicados sobre la muestra: ninguno.
#   La base original se conserva sin alterar.