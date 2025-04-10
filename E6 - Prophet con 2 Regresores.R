library(prophet)
library(dplyr)
library(ggplot2)


data <- read.csv("C:/Users/dguev/Downloads/Walmart Sales Dataset of 45stores/walmart-sales-dataset-of-45stores.csv")  # Ajusta la ruta
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")


data$EsDiciembre <- ifelse(format(data$Date, "%m") == "12", 1, 0)


data_prophet <- data %>%
  group_by(Date) %>%
  summarise(y = sum(Weekly_Sales), 
            CPI = mean(CPI), 
            EsDiciembre = max(EsDiciembre)) %>%
  rename(ds = Date)

# Definir y entrenar el modelo Prophet con regresores
modelo <- prophet()
modelo <- add_regressor(modelo, 'CPI')
modelo <- add_regressor(modelo, 'EsDiciembre')
modelo <- fit.prophet(modelo, data_prophet)

Summary(modelo)
######
# Obtener los valores ajustados
fitted_val <- fitted(modelo)

# Crear dataframe para gráfico
comparaciones <- data.frame(
  Fecha = data_prophet$Date,
  Real = data_prophet$y,
  Pronosticado = as.numeric(fitted_val)
)

# Gráfico de comparación
ggplot(comparaciones, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 0.7) +
  geom_line(aes(y = Pronosticado, color = "Pronosticado"), linetype = "dashed", size = 0.7) +
  scale_color_manual(values = c("Real" = "#1c3875", "Pronosticado" = "#18c42f")) +
  labs(title = "Ventas Reales vs Pronosticadas por ETS SLTF",
       y = "Ventas Semanales",
       x = "Fecha",
       color = "Serie") +
  theme_minimal()

##########


# Crear dataframe futuro (12 semanas sin regresores futuros)
future <- make_future_dataframe(modelo, periods = 12, freq = "week")

# Pegar los regresores disponibles SOLO para las fechas históricas
library(lubridate)  # para usar month()

# Unir regresores históricos
future <- left_join(future, data_prophet[, c("ds", "CPI", "EsDiciembre")], by = "ds")

# Identificar las filas futuras (donde CPI es NA)
idx_futuro <- which(is.na(future$CPI))

# Obtener último CPI real
ultimo_cpi <- tail(na.omit(data_prophet$CPI), 1)

# Generar 12 valores de CPI con incremento de 0.10% semanal
future_cpi <- ultimo_cpi * (1 + 0.001)^(1:length(idx_futuro))

# Asignar los valores simulados
future$CPI[idx_futuro] <- future_cpi

# Asignar EsDiciembre = 1 si el mes es diciembre
future$EsDiciembre[idx_futuro] <- ifelse(month(future$ds[idx_futuro]) == 12, 1, 0)
# Hacer el pronóstico
forecast <- predict(modelo, future)


# Agregar etiqueta para distinguir histórico vs pronóstico
forecast$tipo <- ifelse(forecast$ds <= max(data_prophet$ds), "Histórico", "Pronóstico")

# Visualización con ggplot2
ggplot() +
  geom_line(data = forecast, aes(x = ds, y = yhat, color = tipo), size = 0.7) +
  scale_color_manual(values = c("Histórico" = "#1c3875", "Pronóstico" = "#18c42f")) +
  labs(title = "Forecast Prophet con Regresores Simulados",
       x = "Fecha", y = "Ventas Semanales") +
  theme_minimal()


#METRICASSS

library(prophet)
library(dplyr)
library(Metrics)   # para las métricas MAE, RMSE, etc.

# 1. Partición: usar solo hasta octubre 2012 para entrenamiento
cutoff_date <- as.Date("2012-08-10")  # última semana antes del forecast real
train <- data_prophet %>% filter(ds <= cutoff_date)
test  <- data_prophet %>% filter(ds > cutoff_date)

# 2. Entrenar modelo Prophet con los regresores
modelo_eval <- prophet()
modelo_eval <- add_regressor(modelo_eval, "CPI")
modelo_eval <- add_regressor(modelo_eval, "EsDiciembre")
modelo_eval <- fit.prophet(modelo_eval, train)

# 3. Preparar las fechas futuras (12 semanas reales que ya conocemos)
future_eval <- test[, c("ds", "CPI", "EsDiciembre")]

# 4. Predecir
forecast_eval <- predict(modelo_eval, future_eval)

# 5. Comparar
y_true <- test$y
y_pred <- forecast_eval$yhat

# 6. Métricas de error
mae_val <- mae(y_true, y_pred)
rmse_val <- rmse(y_true, y_pred)
mape_val <- mape(y_true, y_pred)

# 7. Mostrar resultados
cat("Evaluación del modelo Prophet con CPI y EsDiciembre:\n")
cat("MAE: ", round(mae_val, 2), "\n")
cat("RMSE:", round(rmse_val, 2), "\n")
cat("MAPE:", round(mape_val * 100, 2), "%\n")