# CASO 3 - RENT CAR
# PUNTO 2: CONSTRUCCION DE CANALES AMAZON

# Importar la base original
datos <- read.csv("Base_datos_caso_3.csv",
                  check.names = FALSE)

# Identificar las campañas de Amazon por prefijo
names(datos)[grepl("^(DSP|Search|Sponsored)", names(datos))]

# Los agregados se construyen por prefijo y sufijo, como pide el
# enunciado: campañas que comienzan por DSP, Search o Sponsored,
# terminadas en .Cost o .Impression. Las campañas Google quedan por
# fuera de los agregados y se conservan en la base.

# Construir costos totales de Amazon DSP
columnas <- grep("^DSP.*\\.Cost$", names(datos), value = TRUE)
datos$Amazon_DSP_Cost <- rowSums(datos[, columnas, drop = FALSE])

# Construir impresiones totales de Amazon DSP
columnas <- grep("^DSP.*\\.Impression$", names(datos), value = TRUE)
datos$Amazon_DSP_Impressions <- rowSums(datos[, columnas, drop = FALSE])

# Construir costos totales de Amazon Search
columnas <- grep("^Search.*\\.Cost$", names(datos), value = TRUE)
datos$Amazon_Search_Cost <- rowSums(datos[, columnas, drop = FALSE])

# Construir impresiones totales de Amazon Search
columnas <- grep("^Search.*\\.Impression$", names(datos), value = TRUE)
datos$Amazon_Search_Impressions <- rowSums(datos[, columnas, drop = FALSE])

# Comprobar si hay semanas con impresiones
sum(datos$Amazon_Search_Impressions > 0)

# Construir costos totales de Amazon Sponsored
columnas <- grep("^Sponsored.*\\.Cost$", names(datos), value = TRUE)
datos$Amazon_Sponsored_Cost <- rowSums(datos[, columnas, drop = FALSE])

# Construir impresiones totales de Amazon Sponsored
columnas <- grep("^Sponsored.*\\.Impression$", names(datos), value = TRUE)
datos$Amazon_Sponsored_Impressions <- rowSums(datos[, columnas, drop = FALSE])

# Comprobar dimensiones de la base
dim(datos)

# Comprobacion manual de las sumas para dos semanas elegidas
columnas_amazon <- c(
  "Week",
  "Amazon_DSP_Cost",
  "Amazon_DSP_Impressions",
  "Amazon_Search_Cost",
  "Amazon_Search_Impressions",
  "Amazon_Sponsored_Cost",
  "Amazon_Sponsored_Impressions"
)

# Mostrar resultados de las dos primeras semanas
print(datos[1:2, columnas_amazon])

# Seleccionar las dos primeras semanas
semanas <- datos[1:2, ]

# Revisar que DSP sea la suma de sus campañas
print(semanas[, c(
  "DSP - Evergreen.Cost",
  "DSP - Seasonal.Cost",
  "Amazon_DSP_Cost"
)])

# Comprobar las seis variables agregadas
for (canal in c("DSP", "Search", "Sponsored")) {
  for (tipo in c("Cost", "Impressions")) {
    
    sufijo <- if (tipo == "Cost") {
      ".Cost"
    } else {
      ".Impression"
    }
    
    originales <- grep(
      paste0("^", canal, ".*\\", sufijo, "$"),
      names(datos),
      value = TRUE
    )
    
    agregado <- paste0("Amazon_", canal, "_", tipo)
    
    comprobacion <- rowSums(
      semanas[, originales, drop = FALSE]
    )
    
    cat(agregado, ": ",
        isTRUE(all.equal(
          as.numeric(comprobacion),
          semanas[[agregado]],
          tolerance = 1e-8
        )), "\n")
  }
}

# Guardar base con los canales de Amazon (unico bloque de guardado)
write.csv(
  datos,
  "Base_RentCar_Amazon.csv",
  row.names = FALSE
)

# Comprobar que el archivo se guardo
file.exists("Base_RentCar_Amazon.csv")
