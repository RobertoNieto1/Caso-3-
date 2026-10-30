
CASO 3 - RENT CAR
PUNTO 3: FUENTES Y METODOLOGIA

Pontificia Universidad Javeriana
Asignatura: Business Analytics
Fecha de consulta: 30 de octubre de 2026

1. OBJETIVO

Ampliar la base de datos de alquiler de vehiculos en Estados Unidos mediante cuatro variables externas que permitan analizar factores economicos y de demanda relacionados con Sales y Orders.

Periodo analizado: 7 de enero de 2024 al 2 de agosto de 2026.
Frecuencia de la base: semanal.
Observaciones finales: 135.

2. VARIABLES ECONOMICAS

UNRATE_Control
Fuente: Federal Reserve Economic Data (FRED).
Entidad original: Bureau of Labor Statistics (BLS).
URL: https://fred.stlouisfed.org/series/UNRATE
Concepto: tasa de desempleo de Estados Unidos.
Unidad: porcentaje.
Frecuencia original: mensual.
Cobertura: Estados Unidos.

Integracion: se asigno a cada semana la ultima tasa publicada disponible, utilizando el calendario de publicaciones del BLS. Se conservo el ultimo dato conocido hasta la siguiente publicacion.

Durante octubre de 2025 no existio estimacion oficial de desempleo por la interrupcion de la encuesta de hogares.

GASREGW_Control
Fuente: FRED.
Entidad original: Energy Information Administration (EIA).
URL: https://fred.stlouisfed.org/series/GASREGW
Concepto: precio promedio de gasolina regular.
Unidad: dolares estadounidenses por galon.
Frecuencia descargada: semanal.
Cobertura: Estados Unidos.

Integracion: se utilizo la ultima observacion de precio con fecha no posterior a la semana correspondiente. Este procedimiento debe interpretarse con la precaucion de verificar la disponibilidad efectiva del dato en su fecha de referencia.

3. GOOGLE TRENDS

Configuracion comun:
  Fuente: https://trends.google.com/trends/
  Entidad: Google.
Pais: Estados Unidos.
Categoria: Todas las categorias.
Tipo: Busqueda web.
Frecuencia final: semanal.

NonBrand
Termino exacto: car rental.
Concepto: interes de busqueda generico sobre alquiler de vehiculos.

Se realizaron tres descargas con periodos superpuestos, unidas por fecha. Para comparar sus escalas se utilizo un factor de 1,25 entre las dos primeras descargas y un segundo factor estimado mediante siete semanas comunes.

Brand
Termino exacto: enterprise rent a car.
Concepto: interes de busqueda de una marca reconocida de alquiler de vehiculos.

Se realizaron tres descargas semanales, correspondientes a 2024, 2025 y 2026, con semanas de coincidencia.

Factores de ajuste:
  2025: 65 / 69 = 0,942029.
2026: 0,8722491, aplicado sobre la escala previamente ajustada.

La variable Brand representa el interes por Enterprise Rent-A-Car, no necesariamente por la empresa del caso.

4. CRITERIOS DE INTEGRACION

Las series se incorporaron mediante la fecha Week.
Se conservaron las 135 observaciones originales.
No se eliminaron semanas durante las uniones.
No quedaron valores faltantes en las cuatro variables finales.

Las variables SP500 y WEI ya estaban incluidas en la base original y no se contabilizaron como nuevas variables externas.

5. LIMITACIONES METODOLOGICAS

Los indices de Google Trends son relativos, no representan cantidades absolutas de busquedas.

La combinacion de descargas mediante factores de escala es aproximada. Los factores calculados a partir de una sola semana compartida pueden ser sensibles a variaciones de muestreo.

Las series de Google Trends descargadas retrospectivamente pueden contener informacion revisada respecto de la disponible historicamente.

UNRATE requiere considerar las fechas efectivas de publicacion y sus revisiones posteriores.

En GASREGW debe distinguirse la fecha de observacion de la fecha efectiva de disponibilidad.

La inclusion de estos controles permite estudiar asociaciones estadisticas, pero no demuestra causalidad ni incrementabilidad de la publicidad.

6. RESULTADO FINAL

Archivo: Base_RentCar_Final.csv
Observaciones: 135.
Variables: 44.
Periodo: enero de 2024 a agosto de 2026.

Los controles incorporados son:
  UNRATE_Control
GASREGW_Control
NonBrand
Brand

La base se entrega para el desarrollo de estadisticas descriptivas, regresiones y diagnosticos posteriores.
