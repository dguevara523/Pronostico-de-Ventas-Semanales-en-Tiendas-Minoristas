library(fpp2)        # contiene stlf() y autoplot()
library(lubridate)   
library(ggplot2)     
library(readr)
library(dplyr)

# Leer el dataset
datos <- read.csv("C:/Users/dguev/Downloads/Walmart Sales Dataset of 45stores/walmart-sales-dataset-of-45stores.csv") 

datos$Date <- as.Date(datos$Date, format = "%d-%m-%Y")

# Agrupar por semana 
ventas_agregadas <- datos %>%
  group_by(Date) %>%
  summarise(Ventas = sum(Weekly_Sales))

# Ordenar por fecha
ventas_agregadas <- ventas_agregadas %>% arrange(Date)

# Crear la serie de tiempo (frecuencia = 52 porque es semanal)
serie_ts <- ts(ventas_agregadas$Ventas, start = c(2010, 6), frequency = 52)


# Pronóstico de 12 semanas
forecast_ets <- stlf(serie_ts, h = 12, method = "ets", s.window = "periodic")


# Obtener los valores ajustados
fitted_values <- fitted(forecast_ets)

# Crear dataframe para gráfico
comparacion <- data.frame(
  Fecha = ventas_agregadas$Date,
  Real = ventas_agregadas$Ventas,
  Pronosticado = as.numeric(fitted_values)
)

# Gráfico de comparación
ggplot(comparacion, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 0.7) +
  geom_line(aes(y = Pronosticado, color = "Pronosticado"), linetype = "dashed", size = 0.7) +
  scale_color_manual(values = c("Real" = "#1c3875", "Pronosticado" = "#18c42f")) +
  labs(title = "Ventas Reales vs Pronosticadas por ETS SLTF",
       y = "Ventas Semanales",
       x = "Fecha",
       color = "Serie") +
  theme_minimal()


##################

# Generar pronóstico de 12 semanas
pronostico <- forecast(forecast_ets, h = 12)

# Crear fechas futuras
ultima_fecha <- max(ventas_agregadas$Date)
fechas_futuras <- seq(ultima_fecha + weeks(1), by = "week", length.out = 12)

# Preparar datos para el gráfico
historico <- data.frame(
  Fecha = ventas_agregadas$Date,
  Ventas = ventas_agregadas$Ventas,
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
       subtitle = "Modelo ETS SLTF - Todas las Tiendas",
       y = "Ventas Semanales",
       x = "Fecha") +
  theme_minimal()

#######################################


# Crear fechas reales (históricas + futuras)
fechas <- seq(from = min(ventas_agregadas$Date),
              by = "week", length.out = length(serie_ts) + 12)

# Armar dataframe para graficar
df_plot <- data.frame(
  Fecha = fechas,
  Valor = c(as.numeric(serie_ts), as.numeric(forecast_ets$mean)),
  Tipo = c(rep("Histórico", length(serie_ts)), rep("Pronóstico", 12))
)

# Graficar el pronóstico
autoplot(forecast_ets) +
  ggtitle("Prueba Todas las Tiendas - Pronóstico de Ventas - 12 Semanas (STLF)") +
  xlab("Año") + ylab("Ventas")

# Graficar con ggplot
ggplot(df_plot, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Histórico" = "black", "Pronóstico" = "blue")) +
  labs(title = "Pronóstico de Ventas Semanales (ETS + STLF)",
       x = "Fecha", y = "Ventas") +
  theme_minimal()

summary(forecast_ets)

