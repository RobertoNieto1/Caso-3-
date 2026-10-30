
# CASO 3 - RENT CAR
# PUNTO 2: CONSTRUCCION DE CANALES AMAZON

# Importar la base original
datos <- read.csv(
  "Base_datos_caso_3 (1) (1).csv",
  check.names = FALSE
)

# Identificar las campañas de Amazon
names(datos)[grepl("^(DSP|Search|Sponsored)", names(datos))]


# Construir costo total de Amazon DSP
datos$Amazon_DSP_Cost <-
  datos[["DSP - Evergreen.Cost"]] +
  datos[["DSP - Seasonal.Cost"]]

# Revisar los primeros 6 resultados
head(datos$Amazon_DSP_Cost)


# Construir impresiones totales de Amazon DSP
datos$Amazon_DSP_Impressions <-
  datos[["DSP - Evergreen.Impression"]] +
  datos[["DSP - Seasonal.Impression"]]

# Revisar los primeros 6 resultados
head(datos$Amazon_DSP_Impressions)


# Construir costo total de Amazon Search
datos$Amazon_Search_Cost <-
  datos[["Search - Men - Brand.Cost"]] +
  datos[["Search - Men - Non-Brand.Cost"]] +
  datos[["Search - Women - Brand.Cost"]] +
  datos[["Search - Women - Non-Brand.Cost"]]

# Revisar los primeros 6 resultados
head(datos$Amazon_Search_Cost)


# Construir impresiones totales de Amazon Search
datos$Amazon_Search_Impressions <-
  datos[["Search - Men - Brand.Impression"]] +
  datos[["Search - Men - Non-Brand.Impression"]] +
  datos[["Search - Women - Brand.Impression"]] +
  datos[["Search - Women - Non-Brand.Impression"]]

# Revisar los primeros 6 resultados
head(datos$Amazon_Search_Impressions)

# Comprobar si hay semanas con impresiones
sum(datos$Amazon_Search_Impressions > 0)


# Construir costo total de Amazon Sponsored
datos$Amazon_Sponsored_Cost <-
  datos[["Sponsored Brands-Paid Search.Cost"]] +
  datos[["Sponsored Display-Non-Brand.Cost"]] +
  datos[["Sponsored Products-Brand Search.Cost"]] +
  datos[["Sponsored Products-NonBrand Search.Cost"]]

# Revisar los primeros 6 resultados
head(datos$Amazon_Sponsored_Cost)


# Construir impresiones totales de Amazon Sponsored
datos$Amazon_Sponsored_Impressions <-
  datos[["Sponsored Brands-Paid Search.Impression"]] +
  datos[["Sponsored Display-Non-Brand.Impression"]] +
  datos[["Sponsored Products-Brand Search.Impression"]] +
  datos[["Sponsored Products-NonBrand Search.Impression"]]

# Revisar los primeros 6 resultados
head(datos$Amazon_Sponsored_Impressions)

# Comprobar dimensiones de la base
dim(datos)


# Comprobacion de los agregados de Amazon
# Seleccionar las primeras dos semanas

columnas_amazon <- c(
  "Week",
  "Amazon_DSP_Cost",
  "Amazon_DSP_Impressions",
  "Amazon_Search_Cost",
  "Amazon_Search_Impressions",
  "Amazon_Sponsored_Cost",
  "Amazon_Sponsored_Impressions"
)

# Mostrar resultados de ambas semanas
print(datos[1:2, columnas_amazon])



# Comprobacion de sumas para dos semanas

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


# Guardar base con los canales de Amazon
write.csv(
  datos,
  "Base_RentCar_Amazon.csv",
  row.names = FALSE
)

# Comprobar que el archivo se guardo
file.exists("Base_RentCar_Amazon.csv")


# Guardar la base ampliada en el repositorio de GitHub
write.csv(
  datos,
  "~/Downloads/Caso-3-/Base_RentCar_Amazon.csv",
  row.names = FALSE
)

# Verificar que se guardo
file.exists("~/Downloads/Caso-3-/Base_RentCar_Amazon.csv")



# Seleccionar carpeta del repositorio GitHub
carpeta_github <- file.choose()

# Mostrar la ruta seleccionada
carpeta_github


# Obtener la carpeta del repositorio
ruta_repositorio <- dirname(carpeta_github)

# Guardar la base ampliada en GitHub
write.csv(
  datos,
  file.path(ruta_repositorio, "Base_RentCar_Amazon.csv"),
  row.names = FALSE
)

# Verificar que se guardo correctamente
file.exists(
  file.path(ruta_repositorio, "Base_RentCar_Amazon.csv")
)


