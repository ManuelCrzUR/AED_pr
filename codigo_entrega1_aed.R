# Cargar librerías necesarias
library(readr)  # Para leer archivos CSV
library(dplyr)  # Para manipulación de datos
install.packages("rstatix")  # Solo la primera vez
library(rstatix)
install.packages("ggpubr")
library(ggpubr)
install.packages("GGally")
library(GGally)
install.packages("randomForest")
install.packages("pROC")
install.packages("gbm")
install.packages("mgcv")
library(randomForest)
library(pROC)

# Cargar la base de datos y dar una pequeña vista
forestfires <- read_csv("C:/Users/famil/Downloads/forestfires.csv")

library(skimr)
skim(forestfires)

# Normalidad
library(MVN)
hist_mardia=mvn(data=forestfires,
                mvnTest="mardia",
                univariatePlot = "histogram")

#Gráfico qqplot
qq_plot=mvn(data=forestfires,
            mvnTest="mardia",
            univariatePlot = "qqplot")

#Pruebas de normalidad univariadas
# Cargar la librería necesaria
library(MVN)
# Cargar la librería
library(MVN)




# Normalidad



# Cargar librería necesaria
library(MVN)

# Seleccionar solo las 11 variables de interés
variables <- c("X", "Y", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area")

# Iterar sobre cada variable y aplicar las pruebas de normalidad
for (var in variables) {
  cat("\nResultados para la variable:", var, "\n")
  
  # Extraer solo la columna de interés y asegurar que sea un data frame con nombre
  data_var <- forestfires[, var, drop = FALSE]  # Mantiene el formato data.frame
  
  # Prueba de Shapiro-Wilk
  sw_result <- mvn(data = data_var, univariateTest = "SW", desc = TRUE)
  print(sw_result$univariateNormality) 
  
  # Prueba de Anderson-Darling
  ad_result <- mvn(data = data_var, univariateTest = "AD", desc = TRUE)
  print(ad_result$univariateNormality)
}

pnm=mvn(data=forestfires_numeric,
        mvnTest = "mardia")

pnm$multivariateNormality



# Convertir todas las columnas numéricas
forestfires_numeric <- forestfires[, sapply(forestfires, is.numeric)]

# Ejecutar la prueba de Shapiro-Wilk (SW)
pn_SW <- mvn(data = forestfires_numeric, univariateTest = "SW", desc = TRUE)
print(pn_SW)

# Ejecutar la prueba de Anderson-Darling (AD)
pn_AD <- mvn(data = forestfires_numeric, univariateTest = "AD", desc = TRUE)
print(pn_AD)


# Configuración de la disposición de gráficos (2 columnas)
par(mfrow = c(1,3))

# Histogramas de todas las variables con títulos más grandes
hist(forestfires$FFMC, main="Hist. de FFMC", xlab="FFMC", col="lightblue", border="black", cex.main=1.8)
hist(forestfires$DMC, main="Hist. de DMC", xlab="DMC", col="lightblue", border="black", cex.main=1.8)
hist(forestfires$DC, main="Hist. de DC", xlab="DC", col="lightblue", border="black", cex.main=1.8)

par(mfrow = c(1,3))
hist(forestfires$ISI, main="Hist. de ISI", xlab="ISI", col="lightblue", border="black", cex.main=1.8)
hist(forestfires$temp, main="Hist. de Temperatura", xlab="Temperatura (°C)", col="lightblue", border="black", cex.main=1.8)
hist(forestfires$RH, main="Hist. de RH", xlab="Humedad Relativa (%)", col="lightblue", border="black", cex.main=1.8)

par(mfrow = c(1,3))
hist(forestfires$wind, main="Hist. de Viento", xlab="Velocidad del Viento (km/h)", col="lightblue", border="black", cex.main=1.8)
hist(forestfires$rain, main="Hist. de Lluvia", xlab="Precipitación (mm)", col="lightblue", border="black", cex.main=1.8)
hist(forestfires$area, main="Hist. de Área Quemada", xlab="Área Quemada (ha)", col="lightblue", border="black", cex.main=1.8)

# matriz de correlaciones
library(dplyr)
library(skimr)
library(corrplot)
library(GGally)

library(dplyr)
library(skimr)
library(corrplot)
library(GGally)

# Calcular la matriz de correlación
corr_matrix = round(cor(x=forestfires_numeric[,-1], method = "pearson"), 3);corr_matrix


# Ajustar los márgenes para maximizar el espacio
par(mar = c(0, 0, 0, 0))  # Márgenes mínimos
par(pty = "s")  # Mantiene la relación de aspecto cuadrada

par(mfrow = c(1,1))
# Dibujar la matriz de correlación ocupando todo el plot
round(cor(x=forestfires_numeric,method = "pearson"),3)

corrplot(corr=cor(x=forestfires_numeric,method = "pearson"),
         method = "number",
         tl.cex = 0.7,
         number.cex = 0.8,
         cl.pos="n")



ggpairs(forestfires_numeric[,-1],
        lower = list(continuous="smooth"),
        diag=list(continuous="barDiag"),
        axisLabels = "none")


library(dplyr)
library(ggplot2)

numeric_vars <- forestfires %>% select(FFMC, DMC, DC, temp, RH, wind, rain, area)

# Función para detectar valores atípicos con IQR
detect_outliers_IQR <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  df %>% filter(df[[column]] < lower_bound | df[[column]] > upper_bound) %>% select(column)
}

# Función para detectar valores atípicos con Z-score
detect_outliers_Z <- function(df, column, threshold = 3) {
  mean_val <- mean(df[[column]], na.rm = TRUE)
  sd_val <- sd(df[[column]], na.rm = TRUE)
  z_scores <- (df[[column]] - mean_val) / sd_val
  
  df %>% filter(abs(z_scores) > threshold) %>% select(column)
}

# Aplicar la detección de outliers con IQR y Z-score
outliers_IQR <- lapply(names(numeric_vars), function(col) {
  list(variable = col, outliers = detect_outliers_IQR(numeric_vars, col))
})

outliers_Z <- lapply(names(numeric_vars), function(col) {
  list(variable = col, outliers = detect_outliers_Z(numeric_vars, col))
})

# Visualización con boxplots para ver la distribución y valores atípicos
par(mfrow = c(1, 2))
boxplot(numeric_vars$FFMC, main = "FFMC", col = "lightblue", horizontal = TRUE)
boxplot(numeric_vars$DMC, main = "DMC", col = "lightblue", horizontal = TRUE)

par(mfrow = c(1, 2))
boxplot(numeric_vars$DC, main = "DC", col = "lightblue", horizontal = TRUE)
boxplot(numeric_vars$temp, main = "Temp", col = "lightblue", horizontal = TRUE)

par(mfrow = c(1, 2))
boxplot(numeric_vars$RH, main = "RH", col = "lightblue", horizontal = TRUE)
boxplot(numeric_vars$wind, main = "Wind", col = "lightblue", horizontal = TRUE)

par(mfrow = c(1, 2))
boxplot(numeric_vars$rain, main = "Rain", col = "lightblue", horizontal = TRUE)
boxplot(numeric_vars$area, main = "Area", col = "lightblue", horizontal = TRUE)

# Mostrar los valores atípicos detectados
list(IQR = outliers_IQR, Zscore = outliers_Z)


# Cargar librerías necesarias
library(readr)
library(agricolae)
library(car)
library(stats)
library(lmtest)

# Cargar datos
forestfires <- read_csv("C:/Users/angel/Downloads/forest+fires/forestfires.csv")

# Ver las primeras filas del dataframe
head(forestfires)

# Supongamos que quieres analizar la "Área Quemada" según el mes
forestfires$month <- as.factor(forestfires$month) # Convertir mes en factor

# ANOVA
modelo <- aov(area ~ month, data = forestfires)

ANOVA <- anova(modelo)
print(ANOVA)

# Comparaciones múltiples con Tukey
interTukey <- TukeyHSD(modelo, conf.level = 0.95)
print(interTukey)
plot(interTukey)

# LSD
interLSD <- LSD.test(forestfires$area, forestfires$month, 
                     DFerror = ANOVA$Df[2], 
                     MSerror = ANOVA$Mean Sq[2], 
                     console = TRUE)
plot(interLSD)

# Prueba de normalidad con Shapiro-Wilk
error <- modelo$residuals
shapiro.test(error)

# Prueba de homogeneidad de varianzas (Bartlett)
bartlett.test(error ~ forestfires$month)

# Prueba de independencia de errores (Durbin-Watson)
dwtest(modelo)