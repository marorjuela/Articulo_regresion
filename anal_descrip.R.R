# Analisis descriptivo de los datos del articulo sobre CBR

library(readxl)

url <- "https://raw.githubusercontent.com/marorjuela/Articulo_regresion/main/datos.xlsx"
archivo <- tempfile(fileext = ".xlsx")
download.file(url, destfile = archivo, mode = "wb")
datos <- read_excel(archivo, sheet = "datos", range = "B1:I39")

# Revision general
cat("\nDimension de la base:\n")
print(dim(datos))

cat("\nPrimeras observaciones:\n")
print(head(datos))

cat("\nDatos faltantes por variable:\n")
print(colSums(is.na(datos)))

numericas <- datos[, c("cbr_nat", "cbr_sat", "ll", "ip", "qu")]

cat("\nResumen estadistico:\n")
print(summary(numericas))

cat("\nDesviacion estandar:\n")
print(round(sapply(numericas, sd), 3))

cat("\nVarianza:\n")
print(round(sapply(numericas, var), 3))

# Frecuencias por tipo de suelo
frecuencias <- table(datos$sucs)

cat("\nFrecuencias por tipo de suelo (SUCS):\n")
print(frecuencias)

cat("\nPorcentajes por tipo de suelo (SUCS):\n")
print(round(100 * prop.table(frecuencias), 2))

cat("\nMatriz de correlaciones de Pearson:\n")
print(round(cor(numericas), 3))

hist(datos$cbr_nat, main = "CBR natural", xlab = "CBR natural (%)")
hist(datos$cbr_sat, main = "CBR saturado", xlab = "CBR saturado (%)")
hist(datos$ll, main = "Limite liquido", xlab = "Limite liquido (%)")
hist(datos$ip, main = "Indice de plasticidad", xlab = "Indice de plasticidad (%)")
hist(datos$qu, main = "Compresion inconfinada", xlab = "qu (kg/cm2)")

boxplot(datos$cbr_nat, datos$cbr_sat,
        names = c("Natural", "Saturado"),
        main = "CBR natural y saturado", ylab = "CBR (%)")

barplot(frecuencias, main = "Clasificacion SUCS",
        xlab = "Tipo de suelo", ylab = "Frecuencia")

plot(datos$qu, datos$cbr_nat,
     main = "CBR natural vs qu",
     xlab = "qu (kg/cm2)", ylab = "CBR natural (%)")

plot(datos$qu, datos$cbr_sat,
     main = "CBR saturado vs qu",
     xlab = "qu (kg/cm2)", ylab = "CBR saturado (%)")
