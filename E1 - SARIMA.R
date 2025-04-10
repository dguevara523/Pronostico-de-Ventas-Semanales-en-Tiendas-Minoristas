# Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Leemos los datos y los preparamos
datos <- read.csv("C:/Users/dguev/Downloads/Walmart Sales Dataset of 45stores/walmart-sales-dataset-of-45stores.csv") # Ajusta el nombre del archivo

# Convertir la fecha al formato correcto
datos$Date <- as.Date(datos$Date, format = "%d-%m-%Y")

# Sumar las ventas de todas las tiendas por semana
ventas_semanales <- datos %>%
  group_by(Date) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales)) %>%
  arrange(Date)



# Convertir a serie temporal
# Creamos un objeto ts con frecuencia 52 (semanas por año)
start_year <- year(min(ventas_semanales$Date))
start_week <- week(min(ventas_semanales$Date))

ts_ventas <- ts(ventas_semanales$Weekly_Sales, 
                frequency = 52,
                start = c(start_year, start_week))

# Visualizar la serie temporal
autoplot(ts_ventas) + 
  ggtitle("Ventas Semanales Totales") +
  ylab("Ventas") + xlab("Tiempo")



# Descomponer la serie para ver tendencia y estacionalidad
decomposed <- stl(ts_ventas, s.window = "periodic")
autoplot(decomposed)

# Test de estacionariedad (Augmented Dickey-Fuller)
adf.test(ts_ventas, alternative = "stationary")

# Si la serie no es estacionaria, necesitaremos diferenciación

# Encontrar los parámetros óptimos usando auto.arima
modelo_sarima <- auto.arima(ts_ventas, 
                            seasonal = TRUE,
                            stepwise = FALSE,
                            approximation = FALSE,
                            trace = TRUE)

# Resumen del modelo
summary(modelo_sarima)

# Verificar residuos
checkresiduals(modelo_sarima)


# Obtener los valores ajustados
fitted_values <- fitted(modelo_sarima)

# Crear dataframe para gráfico
comparacion <- data.frame(
  Fecha = ventas_semanales$Date,
  Real = ventas_semanales$Weekly_Sales,
  Pronosticado = as.numeric(fitted_values)
)

# Gráfico de comparación
ggplot(comparacion, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 0.7) +
  geom_line(aes(y = Pronosticado, color = "Pronosticado"), linetype = "dashed", size = 0.7) +
  scale_color_manual(values = c("Real" = "#1c3875", "Pronosticado" = "#18c42f")) +
  labs(title = "Ventas Reales vs Pronosticadas por SARIMA",
       y = "Ventas Semanales",
       x = "Fecha",
       color = "Serie") +
  theme_minimal()



# Generar pronóstico de 12 semanas
pronostico <- forecast(modelo_sarima, h = 12)

# Crear fechas futuras
ultima_fecha <- max(ventas_semanales$Date)
fechas_futuras <- seq(ultima_fecha + weeks(1), by = "week", length.out = 12)

# Preparar datos para el gráfico
historico <- data.frame(
  Fecha = ventas_semanales$Date,
  Ventas = ventas_semanales$Weekly_Sales,
  Tipo = "Histórico"
)

futuro <- data.frame(
  Fecha = fechas_futuras,
  Ventas = as.numeric(pronostico$mean),
  Tipo = "Pronóstico"
)

# Intervalos de confianza
futuro$Lo80 <- as.numeric(pronostico$lower[,1])
futuro$Hi80 <- as.numeric(pronostico$upper[,1])
futuro$Lo95 <- as.numeric(pronostico$lower[,2])
futuro$Hi95 <- as.numeric(pronostico$upper[,2])

# Combinar datos
datos_grafico <- bind_rows(historico, futuro)

# Gráfico completo
ggplot(datos_grafico, aes(x = Fecha, y = Ventas)) +
  geom_line(aes(color = Tipo), size = 0.8) +
  geom_ribbon(data = futuro, aes(ymin = Lo95, ymax = Hi95), fill = "#18c42f", alpha = 0.2) +
  geom_ribbon(data = futuro, aes(ymin = Lo80, ymax = Hi80), fill = "#18c42f", alpha = 0.3) +
  geom_vline(xintercept = ultima_fecha, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Histórico" = "#1c3875", "Pronóstico" = "#18c42f")) +
  labs(title = "Pronóstico de Ventas Semanales para las próximas 12 semanas",
       subtitle = "Modelo SARIMA - Todas las Tiendas",
       y = "Ventas Semanales",
       x = "Fecha") +
  theme_minimal()
