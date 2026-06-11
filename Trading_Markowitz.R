#Session → Restart R
 #.rs.restartR()

install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("dplyr")
install.packages("caret")
install.packages("tidyverse")
install.packages(ggplot2)
install.packages(tidyr)
install.packages("corrplot")
?quantmod # ? para obtener la información

library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(caret)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(corrplot)
library(ggplot2)
# Descarga de los dataset

fecha_inicio <- "2025-06-01"

# AAPL
AAPL <- getSymbols("AAPL",
                   src = "yahoo",
                   from = fecha_inicio,
                   auto.assign = FALSE)

AAPL_df <- data.frame(
  date = index(AAPL),
  coredata(AAPL)
)

write.csv(
  AAPL_df,
  "C:/Users/metis/OneDrive/Escritorio/Curso de R/portafolio_financiero/AAPL.csv",
  row.names = FALSE
)

# NVDA
NVDA <- getSymbols("NVDA",
                   src = "yahoo",
                   from = fecha_inicio,
                   auto.assign = FALSE)

NVDA_df <- data.frame(
  date = index(NVDA),
  coredata(NVDA)
)

write.csv(
  NVDA_df,
  "C:/Users/metis/OneDrive/Escritorio/Curso de R/portafolio_financiero/NVDA.csv",
  row.names = FALSE
)

# AMZN
AMZN <- getSymbols("AMZN",
                   src = "yahoo",
                   from = fecha_inicio,
                   auto.assign = FALSE)

AMZN_df <- data.frame(
  date = index(AMZN),
  coredata(AMZN)
)

write.csv(
  AMZN_df,
  "C:/Users/metis/OneDrive/Escritorio/Curso de R/portafolio_financiero/AMZN.csv",
  row.names = FALSE
)

# Verificar valores faltantes
colSums(is.na(AAPL_df))
colSums(is.na(NVDA_df))
colSums(is.na(AMZN_df))

# Eliminar filas con NA
AAPL_df <- na.omit(AAPL_df)
NVDA_df <- na.omit(NVDA_df)
AMZN_df <- na.omit(AMZN_df)

# Verificar duplicados


# Eliminar duplicados
AAPL_df <- unique(AAPL_df)
NVDA_df <- unique(NVDA_df)
AMZN_df <- unique(AMZN_df)

# Comprobar que no quedaron NA
colSums(is.na(AAPL_df))
colSums(is.na(NVDA_df))
colSums(is.na(AMZN_df))


#Ver el precio ajustado

date <- "2025-06-01"
NVDA <- getSymbols(
  "NVDA",
  src = "yahoo",
  from = date,
  auto.assign = FALSE
)
NVDAclose <- NVDA[,6]
head(NVDAclose)


date <- "2025-06-01"
AAPL <- getSymbols(
  "AAPL",
  src = "yahoo",
  from = date,
  auto.assign = FALSE
)
AAPLclose <- AAPL[,6]
head(AAPLclose)


date <- "2025-06-01"
AMZN <- getSymbols(
  "AMZN",
  src = "yahoo",
  from = date,
  auto.assign = FALSE
)
AMZNclose <- AMZN[,6]
head(AMZNclose)

#grafica de retorno 

NVDARets <- na.omit(dailyReturn(NVDAclose,type = "log")) #para mostrar una grafica sin errores de datos y un retorno logaritmico
chartSeries(NVDARets)

AAPLRets <- na.omit(dailyReturn(AAPLclose,type = "log")) #para mostrar una grafica sin errores de datos y un retorno logaritmico
chartSeries(AAPLRets)

AMZNRets <- na.omit(dailyReturn(AMZNclose,type = "log")) #para mostrar una grafica sin errores de datos y un retorno logaritmico
chartSeries(AMZNRets)


#dataset de NVIDIA AGREGAMOS COLUMNAS Company y Ticker

NVDA_df <- NVDA_df %>% mutate(Company = "NVIDIA",Ticker = "NVDA")
View(NVDA_df)
AAPL_df <- AAPL_df %>% mutate(Company = "APPLE",Ticker = "AAPL")
View(AAPL_df)
AMZN_df <- AMZN_df %>% mutate(Company = "AMAZON",Ticker = "AMZN")
View(AMZN_df)


#verificaciones de columnas para poder unir los dataset
names(NVDA_df)
names(AAPL_df)
names(AMZN_df)
#renombrar las columnas
colnames(NVDA_df) <- c(
  "date","Open","High","Low",
  "Close","Volume","Adjusted",
  "Company","Ticker")
colnames(AAPL_df) <- c(
  "date","Open","High","Low",
  "Close","Volume","Adjusted",
  "Company","Ticker")
colnames(AMZN_df) <- c(
  "date","Open","High","Low",
  "Close","Volume","Adjusted",
  "Company","Ticker")

#unir los dataset NVDA,AAPL Y AMZN

LATAMIndex <- rbind(NVDA_df,AAPL_df,AMZN_df)

view(LATAMIndex)

#exploracion y limpieza de los datos

str(LATAMIndex)

#observaciones tiene cada empresa
table(LATAMIndex$Ticker)

#resumen de los tres dataset
summary(LATAMIndex)

#Qué empresa tuvo mayor volumen promedio
aggregate(
  Volume ~ Ticker,
  data = LATAMIndex,mean)

#Qué empresa tuvo mayor precio promedio
aggregate(
  Adjusted ~ Ticker,
  data = LATAMIndex,mean)


#Cuál fue la evolución de precios
ggplot(LATAMIndex,
       aes(x = date,
           y = Adjusted,
           color = Ticker)) +
  geom_line(size = 1) +
  labs(
    title = "Comparación de precios ajustados",
    x = "Fecha",
    y = "Precio")

#desviacion standar por empresa NVDA AAPL AMZN

aggregate(
  Adjusted ~ Ticker,
  data = LATAMIndex,
  sd
)

#Grafico de barras de la desviaciones standar
riesgo <- aggregate(
  Adjusted ~ Ticker,
  data = LATAMIndex,
  sd
)

ggplot(riesgo,
       aes(x = Ticker,
           y = Adjusted,
           fill = Ticker)) +
  geom_col() +
  labs(
    title = "Volatilidad por empresa",
    x = "Empresa",
    y = "Desviación estándar"
  ) +
  theme_minimal()


#matriz de correlación entre AAPL, NVDA y AMZN

precios <- LATAMIndex %>%
  select(date, Ticker, Adjusted) %>%
  pivot_wider(
    names_from = Ticker,
    values_from = Adjusted)
head(precios)

correlacion <- cor(
  precios[, c("AAPL","AMZN","NVDA")],
  use = "complete.obs")
correlacion

corrplot(
  correlacion,
  method = "color",
  type = "upper",
  addCoef.col = "black")


#Precio promedio (mean)
aggregate(
  Adjusted ~ Ticker,
  data = LATAMIndex,
  mean)

#Coeficiente de variación (CV)
cv <- aggregate(
  Adjusted ~ Ticker,
  data = LATAMIndex,
  function(x) 100 * sd(x) / mean(x))
cv

#===================================
# TEORÍA DE MARKOWITZ
#===================================

cat("
    Se calculan los retornos promedio y la matriz de
    covarianzas para estimar el comportamiento esperado
    de cada activo y su relación conjunta.
    
    Estos parámetros son la base para construir
    portafolios eficientes que maximicen retorno
    y minimicen riesgo.
    ")
mean_returns <- colMeans(returns)
mean_returns

#Matriz de covarianzas
cov_matrix <- cov(returns)
cov_matrix

#Fijas una semilla
set.seed(123)

#Definir cuántos portafolios vas a probar
#"Genera 5000 combinaciones distintas entre AAPL, AMZN y NVDA."
n_portfolios <- 5000

#Contar cuántos activos tienes
n_assets <- ncol(returns)

#muestra la cantidad de activos AAPL, AMZN y NVDA
n_assets

#Crear la tabla donde guardarás resultados
results <- data.frame(
  Return = numeric(n_portfolios),
  Risk = numeric(n_portfolios))

#Crear la matriz de pesos
weights_list <- matrix(
  0,
  nrow = n_portfolios,
  ncol = n_assets)

#calculado los 5000 portafolios
n_portfolios <- 5000
n_assets <- 3
results <- data.frame(
  Return = numeric(n_portfolios),
  Risk = numeric(n_portfolios))
weights_list <- matrix(
  0,
  nrow = n_portfolios,
  ncol = n_assets)

#------------------------------

for(i in 1:n_portfolios){
  
  # generar pesos aleatorios
  weights <- runif(n_assets)
  
  # normalizar para que sumen 1
  weights <- weights / sum(weights)
  
  # guardar pesos
  weights_list[i, ] <- weights
  
  # retorno esperado
  results$Return[i] <- sum(weights * mean_returns)
  
  # riesgo esperado
  results$Risk[i] <- sqrt(
    t(weights) %*% cov_matrix %*% weights
  )
}

head(results)

#===================================
# Ratio de Sharpe
#===================================

results$Sharpe <- results$Return / results$Risk

head(results)

#mejor portafolio
best_index <- which.max(results$Sharpe)
best_index

#mejor resultado
results[best_index, ]

#Ver cuánto invertir en cada acción
weights_best <- weights_list[best_index, ]
weights_best

#Gráfico riesgo-retorno (Markowitz)


ggplot(results,
       aes(x = Risk,
           y = Return,
           color = Sharpe)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Frontera de portafolios aleatorios",
    x = "Riesgo",
    y = "Retorno"
  )

results$Sharpe <- results$Return / results$Risk

best_index <- which.max(results$Sharpe)

results[best_index, ]

weights_best <- weights_list[best_index, ]

weights_best
