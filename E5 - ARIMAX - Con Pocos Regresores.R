library(readr)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)


# Leer el CSV
datos <- read_csv("C:/Users/dguev/Downloads/Walmart Sales Dataset of 45stores/walmart-sales-dataset-of-45stores.csv")  # Cambia a la ruta real

# Convertir fecha al formato correcto
datos$Date <- as.Date(datos$Date, format = "%d-%m-%Y")

datos <- datos %>%
  mutate(EsDiciembre = ifelse(format(Date, "%m") == "12", 1, 0))

ventas_agregadas <- datos %>%
  group_by(Date) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales),
            CPI = mean(CPI),
            EsDiciembre = first(EsDiciembre)) %>%
  arrange(Date)

library(forecast)


# Serie completa
y <- ts(ventas_agregadas$Weekly_Sales, frequency = 52)

# Regresores históricos
xreg <- as.matrix(ventas_agregadas[, c("CPI", "EsDiciembre")])

# Crear 12 fechas futuras a partir de la última
fecha_inicio <- max(ventas_agregadas$Date)
fechas_futuras <- seq(from = fecha_inicio + 7, by = "week", length.out = 12)

# Calcular EsDiciembre para esas fechas
es_diciembre_futuro <- ifelse(format(fechas_futuras, "%m") == "12", 1, 0)

# Asumimos valores constantes para otras variables (promedio o último valor)
últimos_valores <- ventas_agregadas %>% tail(1)

xreg_futuro <- data.frame(
  #Holiday_Flag = rep(últimos_valores$Holiday_Flag, 12),
  #Temperature = rep(últimos_valores$Temperature, 12),
  #Fuel_Price = rep(últimos_valores$Fuel_Price, 12),
  CPI = rep(últimos_valores$CPI, 12),
 # Unemployment = rep(últimos_valores$Unemployment, 12),
  EsDiciembre = es_diciembre_futuro
)

xreg_futuro <- as.matrix(xreg_futuro)



modelo <- auto.arima(y, xreg = xreg, seasonal = TRUE)

summary (modelo)

pronostico <- forecast(modelo, xreg = xreg_futuro, h = 12)

# Graficar el pronóstico
autoplot(pronostico) +
  ggtitle("Pronóstico de Ventas - 12 Semanas (ARIMA)") +
  xlab("Año") + ylab("Ventas")



library(ggplot2)

df_plot <- data.frame(
  Fecha = c(ventas_agregadas$Date, fechas_futuras),
  Weekly_Sales = c(as.numeric(y), as.numeric(pronostico$mean)),
  Tipo = c(rep("Histórico", length(y)), rep("Pronóstico", 12))
)

ggplot(df_plot, aes(x = Fecha, y = Weekly_Sales, color = Tipo)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("Histórico" = "black", "Pronóstico" = "blue")) +
  labs(title = "Forecast SARIMAX con EsDiciembre y regresores",
       x = "Fecha", y = "Ventas Semanales") +
  theme_minimal()
