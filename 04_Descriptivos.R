# CASO 3 - RENT CAR
# PUNTO 4: EXPLORACION Y ESTADISTICAS DESCRIPTIVAS

datos <- read.csv("tables/Base_RentCar_Final.csv",
                  check.names = FALSE)
datos$Week <- as.Date(datos$Week)

# Variables del analisis descriptivo
variables <- c(
  "Sales", "Orders",
  "Amazon_DSP_Cost", "Amazon_Search_Cost",
  "Amazon_Sponsored_Cost",
  "Amazon_DSP_Impressions", "Amazon_Search_Impressions",
  "Amazon_Sponsored_Impressions",
  "SP500", "WEI",
  "UNRATE_Control", "GASREGW_Control",
  "NonBrand", "Brand"
)

# Tabla con N, media, desviacion estandar, minimo y maximo
tabla <- data.frame(
  Variable = variables,
  N = sapply(datos[, variables],
             function(x) sum(!is.na(x))),
  Media = sapply(datos[, variables],
                 function(x) mean(x, na.rm = TRUE)),
  Desv_Est = sapply(datos[, variables],
                    function(x) sd(x, na.rm = TRUE)),
  Min = sapply(datos[, variables],
               function(x) min(x, na.rm = TRUE)),
  Max = sapply(datos[, variables],
               function(x) max(x, na.rm = TRUE))
)

print(tabla)

write.csv(tabla, "tables/p4_estadisticas_descriptivas.csv",
          row.names = FALSE)

# Serie temporal de cada KPI
png("tables/p4_serie_sales.png", width = 900, height = 500)
plot(datos$Week, datos$Sales, type = "l",
     main = "Ventas semanales",
     xlab = "Semana", ylab = "Sales")
dev.off()

png("tables/p4_serie_orders.png", width = 900, height = 500)
plot(datos$Week, datos$Orders, type = "l",
     main = "Ordenes semanales",
     xlab = "Semana", ylab = "Orders")
dev.off()

# Costos por canal de Amazon en un solo grafico
costos <- c("Amazon_DSP_Cost",
            "Amazon_Search_Cost",
            "Amazon_Sponsored_Cost")

png("graphs/p4_costos_canales.png", width = 900, height = 500)
matplot(datos$Week, datos[, costos], type = "l",
        lty = 1, col = c("steelblue", "darkgreen", "tomato"),
        main = "Costos semanales por canal de Amazon",
        xlab = "Semana", ylab = "Costo (USD)")
legend("topleft", legend = costos,
       col = c("steelblue", "darkgreen", "tomato"),
       lty = 1, cex = 0.8)
dev.off()

# Dispersion de Sales contra el costo de cada canal
for (canal in costos) {
  archivo <- paste0("graphs/p4_dispersion_",
                    tolower(canal), ".png")
  png(archivo, width = 700, height = 500)
  plot(datos[[canal]], datos$Sales,
       main = paste("Sales vs", canal),
       xlab = canal, ylab = "Sales")
  dev.off()
}

# Matriz de covarianzas de Sales con las demas numericas
covarianzas <- cov(datos[, variables])
print(covarianzas["Sales", ])
write.csv(covarianzas, "tables/p4_covarianzas.csv")

# Matriz de correlaciones (compara entre escalas distintas)
correlaciones <- cor(datos[, variables])
print(round(correlaciones["Sales", ], 3))
write.csv(round(correlaciones, 3), "tables/p4_correlaciones.csv")

# CONCLUSIONES DEL PUNTO 4
# Las 135 semanas muestran una demanda muy variable, con picos
# de ventas y ordenes en diciembre de 2024 y 2025, coincidiendo
# con aumentos de la inversion publicitaria. Las ventas se
# relacionan positivamente con los costos de los tres canales,
# especialmente Search y Sponsored, mientras que los controles
# externos presentan asociaciones mas debiles o negativas.
# Sales y Orders tienen un comportamiento casi identico.
# Tambien debe considerarse que Search tuvo un periodo inicial
# sin inversion en 2024. Estas relaciones describen la muestra,
# pero no demuestran efectos causales de la publicidad.