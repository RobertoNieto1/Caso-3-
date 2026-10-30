
# CASO 3 - RENT CAR
# PUNTO 1: RECONOCIMIENTO DE LA BASE
setwd("C:/Users/USUARIO/Documents/GitHub/Caso-3-")

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


# Comprobar valores faltantes
sum(is.na(datos))

# Comprobar filas duplicadas
sum(duplicated(datos))

# Comprobar que las fechas estén cada 7 días
fechas <- sort(as.Date(datos$Week))
table(as.numeric(diff(fechas)))
