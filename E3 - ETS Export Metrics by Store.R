library(readr)
library(dplyr)
library(fpp2)
library(lubridate)
library(tibble)

# Leer datos
datos <- read_csv("C:/Users/dguev/Downloads/Walmart Sales Dataset of 45stores/walmart-sales-dataset-of-45stores.csv")  # Cambia esta ruta

# Convertir la columna Date al formato correcto
datos$Date <- as.Date(datos$Date, format = "%d-%m-%Y")

head(datos)

# Inicializar lista para guardar métricas
resultados <- list()

# Iterar por cada tienda
for (store_id in unique(datos$Store)) {
  
  # Filtrar por tienda
  tienda_df <- datos %>%
    filter(Store == store_id) %>%
    group_by(Date) %>%
    summarise(Ventas = sum(Weekly_Sales)) %>%
    arrange(Date)
  
  # Asegurar que hay suficientes datos
  if (nrow(tienda_df) < 100) next  # puedes ajustar este umbral
  
  # Crear serie de tiempo (semanal, frecuencia = 52)
  serie_ts <- ts(tienda_df$Ventas, frequency = 52, start = c(2010, 6))
  
  # Dividir en train (todo menos 12 semanas) y test (últimas 12)
  train_length <- length(serie_ts) - 12
  train_ts <- window(serie_ts, end = c(2012, (train_length %% 52)))
  test_ts <- window(serie_ts, start = c(2012, (train_length %% 52) + 1))
  
  # Modelo ETS + STLF
  modelo <- stlf(train_ts, h = 12, method = "ets", s.window = "periodic")
  
  # Predicciones
  predicciones <- modelo$mean
  
  # Calcular errores
  errores <- accuracy(predicciones, test_ts)
  
  
  
  # Guardar en lista
  resultados[[as.character(store_id)]] <- tibble(
    Store = store_id,
    ME = errores[1, "ME"],
    RMSE = errores[1, "RMSE"],
    MAE  = errores[1, "MAE"],
    MPE  = errores[1, "MPE"],
    MAPE = errores[1, "MAPE"]
  )
}

# Unir todos los resultados y exportar a CSV
df_resultados <- bind_rows(resultados)
write_csv(df_resultados, "errores_por_tienda.csv")