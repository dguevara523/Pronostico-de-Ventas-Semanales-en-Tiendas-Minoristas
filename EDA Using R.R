library(forecast)
# Librerías principales
library(tidyverse)   # Manipulación y visualización de datos
library(lubridate)   # Manejo de fechas
library(ggplot2)     # Gráficos avanzados
library(corrplot)    # Matriz de correlación
library(skimr)       # Resumen estadístico rápido
library(ggpubr)      # Combinación de gráficos


# Cargar datos (suponiendo que tienes un archivo CSV)
walmart  <- read.csv("C:/Users/dguev/Downloads/Walmart Sales Dataset of 45stores/walmart-sales-dataset-of-45stores.csv")

# Convertir fecha a formato Date
walmart$Date <- as.Date(walmart$Date)

# Ver estructura
glimpse(walmart)
skim(walmart)

# Función para gráficos de distribución
plot_hist_box <- function(data, variable, name) {
  p1 <- ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(fill = "skyblue", color = "black", bins = 30) +
    labs(title = paste("Distribución de", name), x = name, y = "Frecuencia")
  
  p2 <- ggplot(data, aes(y = .data[[variable]])) +
    geom_boxplot(fill = "orange", alpha = 0.7) +
    labs(title = paste("Boxplot de", name), y = name)
  
  ggarrange(p1, p2, ncol = 2)
}

# Aplicar a variables numéricas
plot_hist_box(walmart, "Weekly_Sales", "Ventas Semanales")
plot_hist_box(walmart, "Temperature", "Temperatura (F°)")
plot_hist_box(walmart, "Fuel_Price", "Precio Combustible (USD)")
plot_hist_box(walmart, "CPI", "Índice de Precios (CPI)")
plot_hist_box(walmart, "Unemployment", "Tasa de Desempleo (%)")

# Holiday_Flag (0 = No festivo, 1 = Festivo)
ggplot(walmart, aes(x = factor(Holiday_Flag))) +
  geom_bar(fill = c("gray", "red")) +
  labs(title = "Semanas Festivas vs No Festivas", x = "Festivo", y = "Conteo")

# Conteo de datos por tienda (Store)
ggplot(walmart, aes(x = factor(Store))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución de Datos por Tienda", x = "ID Tienda", y = "N° de Registros") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Función para scatterplots con línea de tendencia
plot_scatter <- function(data, x_var, y_var = "Weekly_Sales", x_lab) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste("Ventas Semanales vs", x_lab), x = x_lab, y = "Ventas Semanales (USD)")
}

# Aplicar a predictores numéricos
plot_scatter(walmart, "Temperature", "Weekly_Sales", "Temperatura (F°)")
plot_scatter(walmart, "Fuel_Price", "Weekly_Sales", "Precio Combustible (USD)")
plot_scatter(walmart, "CPI", "Weekly_Sales", "Índice de Precios (CPI)")
plot_scatter(walmart, "Unemployment", "Weekly_Sales", "Desempleo")
plot_scatter(walmart, "Holiday_Flag", "Weekly_Sales", "Holiday Flag")

ggplot(walmart, aes(x = factor(Holiday_Flag), y = Weekly_Sales, fill = factor(Holiday_Flag))) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red"), labels = c("No Festivo", "Festivo")) +
  labs(title = "Ventas Semanales: Festivos vs No Festivos", x = "", y = "Ventas (USD)") +
  theme(legend.title = element_blank())



# Seleccionar solo variables numéricas
numeric_data <- walmart %>% select(Weekly_Sales, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment)

# Calcular correlación
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Visualizar
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.7)

# Agregar año y mes para análisis
walmart <- walmart %>%
  mutate(Year = year(Date),
         Month = month(Date, label = TRUE))

# Ventas promedio por mes (evolución anual)
walmart %>%
  group_by(Year, Month) %>%
  summarise(Avg_Sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = Month, y = Avg_Sales, group = Year, color = factor(Year))) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Tendencia Anual de Ventas", x = "Mes", y = "Ventas Promedio (USD)", color = "Año") +
  theme_minimal()


ggplot(walmart, aes(x = factor(Store), y = Weekly_Sales)) +
  geom_boxplot(fill = "steelblue", outlier.color = "red") +
  labs(title = "Distribución de Ventas por Tienda", 
       x = "ID de Tienda", 
       y = "Ventas Semanales (USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
  scale_y_continuous(labels = scales::dollar)


# Seleccionar 5 tiendas aleatorias para visualización
set.seed(123)
sample_stores <- sample(unique(walmart$Store), 5)

walmart %>%
  filter(Store %in% sample_stores) %>%
  ggplot(aes(x = Date, y = Weekly_Sales, color = factor(Store))) +
  geom_line(linewidth = 0.8) +
  labs(title = "Evolución Temporal de Ventas por Tienda (Muestra Aleatoria)", 
       x = "Fecha", 
       y = "Ventas Semanales (USD)",
       color = "ID Tienda") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()


library(heatmaply)

# Preparar datos: ventas promedio por tienda y mes
heatmap_data <- walmart %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  group_by(Store, Month) %>%
  summarise(Avg_Sales = mean(Weekly_Sales)) %>%
  pivot_wider(names_from = Month, values_from = Avg_Sales) %>%
  column_to_rownames("Store")

# Heatmap interactivo
heatmaply(heatmap_data, 
          main = "Ventas Promedio por Tienda y Mes (USD)",
          xlab = "Mes", 
          ylab = "ID Tienda",
          scale = "column",  # Normalizar por columna (mes)
          colors = viridis::viridis(100),
          k_col = 4,  # Número de clusters para meses
          k_row = 4)  # Número de clusters para tiendas

walmart %>%
  group_by(Store, Holiday_Flag) %>%
  summarise(Median_Sales = median(Weekly_Sales)) %>%
  mutate(Holiday_Flag = ifelse(Holiday_Flag == 1, "Festivo", "No Festivo")) %>%
  ggplot(aes(x = factor(Store), y = Median_Sales, fill = Holiday_Flag)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ventas Medianas en Festivos vs No Festivos por Tienda",
       x = "ID de Tienda",
       y = "Ventas Medianas (USD)",
       fill = "") +
  scale_fill_manual(values = c("Festivo" = "Blue", "No Festivo" = "gray")) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))


walmart %>%
  ggplot(aes(x = Unemployment, y = Weekly_Sales)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ Store, scales = "free_y", ncol = 9) +  # 9 columnas x 5 filas = 45 tiendas
  labs(title = "Relación entre Tasa de Desempleo y Ventas por Tienda",
       x = "Tasa de Desempleo (%)",
       y = "Ventas Semanales (USD)") +
  theme_minimal() +
  theme(axis.text = element_text(size = 6))


# Función para calcular outliers por tienda
find_outliers <- function(data, store_id) {
  store_data <- data %>% filter(Store == store_id)
  z_scores <- scale(store_data$Weekly_Sales)
  outliers <- which(abs(z_scores) > 3)
  return(store_data[outliers, ])
}

# Ejemplo para la tienda 1
outliers_tienda1 <- find_outliers(walmart, 1)

# Gráfico de outliers para todas las tiendas
walmart %>%
  group_by(Store) %>%
  mutate(Z_Score = scale(Weekly_Sales)) %>%
  filter(abs(Z_Score) > 3) %>%
  ggplot(aes(x = factor(Store), y = Weekly_Sales)) +
  geom_point(color = "red", size = 2) +
  labs(title = "Outliers de Ventas por Tienda (Z-Score > 3)",
       x = "ID de Tienda",
       y = "Ventas Semanales (USD)") +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))