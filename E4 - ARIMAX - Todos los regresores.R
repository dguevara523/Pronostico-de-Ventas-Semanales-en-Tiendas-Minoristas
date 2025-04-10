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
            Holiday_Flag = first(Holiday_Flag),
            Temperature = mean(Temperature),
            Fuel_Price = mean(Fuel_Price),
            CPI = mean(CPI),
            Unemployment = mean(Unemployment),
            EsDiciembre = first(EsDiciembre)) %>%
  arrange(Date)

library(forecast)

# Separar en train y test (últimas 12 semanas para test)
n <- nrow(ventas_agregadas)
h <- 12  # número de semanas a predecir

train <- ventas_agregadas[1:(n - h), ]
test  <- ventas_agregadas[(n - h + 1):n, ]

# Series
y_train <- ts(train$Weekly_Sales, frequency = 52)
xreg_train <- as.matrix(train[, c("Holiday_Flag", "Temperature", "Fuel_Price", "CPI", "Unemployment", "EsDiciembre")])
xreg_test  <- as.matrix(test[,  c("Holiday_Flag", "Temperature", "Fuel_Price", "CPI", "Unemployment", "EsDiciembre")])


modelo <- auto.arima(y_train, xreg = xreg_train, seasonal = TRUE)
summary(modelo)

# Forecast
pronostico <- forecast(modelo, xreg = xreg_test, h = h)


########
# Obtener los valores ajustados
fitted_val <- fitted(modelo)

# Crear dataframe para gráfico
comparaciones <- data.frame(
  Fecha = train$Date,
  Real = train$Weekly_Sales,
  Pronosticado = as.numeric(fitted_val)
)

# Gráfico de comparación
ggplot(comparaciones, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 0.7) +
  geom_line(aes(y = Pronosticado, color = "Pronosticado"), linetype = "dashed", size = 0.7) +
  scale_color_manual(values = c("Real" = "#1c3875", "Pronosticado" = "#18c42f")) +
  labs(title = "Ventas Reales vs Pronosticadas - ARIMAX",
       y = "Ventas Semanales",
       x = "Fecha",
       color = "Serie") +
  theme_minimal()

######

# Crear vector de fechas para eje x
fechas <- ventas_agregadas$Date
fechas_forecast <- test$Date

# Combinar resultados
library(ggplot2)

df_plot <- data.frame(
  Fecha = c(fechas[1:(n - h)], fechas_forecast),
  Weekly_Sales = c(as.numeric(y_train), as.numeric(pronostico$mean)),
  Tipo = c(rep("Histórico", length(y_train)), rep("Pronóstico", h))
)

ggplot(df_plot, aes(x = Fecha, y = Weekly_Sales, color = Tipo)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Histórico" = "black", "Pronóstico" = "blue")) +
  labs(title = "Forecast con SARIMAX + EsDiciembre",
       x = "Fecha", y = "Ventas Semanales") +
  theme_minimal()
