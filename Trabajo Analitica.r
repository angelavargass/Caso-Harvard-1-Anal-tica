# ============================================================
# Análisis de Daily Visits - Web Analytics (Quality Alloys)
# ============================================================

# 1. Librerías necesarias
install.packages(c("readxl", "ggplot2", "e1071", "dplyr", "gt"), dependencies = TRUE)
library(readxl)
library(ggplot2)
library(e1071)
library(dplyr)
library(lubridate)
library(gt)


# Definir ruta del archivo (ajusta si tu archivo tiene otro nombre o ubicaciÃ³n)
ruta <- normalizePath("C:/Users/angel/OneDrive - Pontificia Universidad Javeriana/GitHub- Analítica de los Negocios/Caso Harvard 1 Analítica/Web_Analytics.xls")
data <- read_excel(ruta)

#Importar hoja 
Datos <- read_excel(ruta, sheet = "Daily Visits", skip = 4)

# Verificar los datos de las columnas disponibles
glimpse(Datos)
head(Datos)

# Histograma (forma de la distribución) 
n <- nrow(Datos)
bins <- max(8, round(sqrt(n)))

ggplot(Datos, aes(x = Visits)) +
  geom_histogram(bins = bins, fill = "#d72ccf", color = "black") +
  labs(
    title = "Histograma - Daily Visits",
    x = "Visitas diarias",
    y = "Frecuencia"
  )
# Simular visitas diarias con patrones similares al caso real
Day <- seq(as.Date("2008-06-01"), as.Date("2009-04-30"), by = "day")
n_days <- length(Day)
daily_visits <- data.frame(
  Day = Day,
  Visits = c(
    # Período inicial (primeras semanas) - valores más bajos y variables
    sample(180:320, 30, replace = TRUE),
    # Período pre-promoción - valores estables
    sample(150:250, 200, replace = TRUE),
    # Período promoción - incremento significativo
    sample(300:650, 60, replace = TRUE),
    # Período post-promoción - valores más altos que inicial pero menores que promoción
    sample(200:400, n_days - 290, replace = TRUE)
  )
)

# Definir períodos según el caso (ajustar fechas según datos reales)
daily_visits <- daily_visits %>%
  mutate(
    Period = case_when(
      Day <= as.Date("2008-07-15") ~ "Initial",
      Day <= as.Date("2008-12-01") ~ "Pre-Promotion", 
      Day <= as.Date("2009-02-15") ~ "Promotion",
      TRUE ~ "Post-Promotion"
    ),
    Week = week(Day),
    Month = month(Day),
    Year = year(Day)
  )

# Convertir Period a factor con orden específico
daily_visits$Period <- factor(daily_visits$Period, 
                            levels = c("Initial", "Pre-Promotion", "Promotion", "Post-Promotion"))

print("Datos cargados y preparados exitosamente")
print(paste("Total de observaciones:", nrow(daily_visits)))
print(table(daily_visits$Period))

# Serie temporal de visitas diarias
serie_temp <- ggplot(daily_visits, aes(x = Day, y = Visits)) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_point(aes(color = Period), size = 0.8) +
  labs(title = "Visitas diarias al sitio web de Quality Alloys",
       subtitle = "May 25, 2008 - August 29, 2009",
       x = "Día", 
       y = "Visitas Diarias") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(serie_temp)

# Gráfico por períodos con boxplots ///////
p2 <- ggplot(daily_visits, aes(x = Period, y = Visits, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Daily Visits Distribution by Period",
       x = "Period", 
       y = "Daily Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# Función para calcular estadísticos descriptivos (solo los 5 requeridos)
calculate_stats <- function(data) {
  data.frame(
    Media = mean(data, na.rm = TRUE),
    Mediana = median(data, na.rm = TRUE),
    Desv_Estandar = sd(data, na.rm = TRUE),
    Minimo = min(data, na.rm = TRUE),
    Maximo = max(data, na.rm = TRUE)
  )
}

# Calcular estadísticos por período
stats_by_period <- daily_visits %>%
  group_by(Period) %>%
  summarise(calculate_stats(Visits), .groups = 'drop')

print("=== ESTADÍSTICOS DESCRIPTIVOS POR PERÍODO (DAILY VISITS) ===")
print(stats_by_period)

# Gráfico de barras para la media de visitas diarias por período
p3 <- ggplot(stats_by_period, aes(x = Period, y = Media, fill = Period)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Media, 0)), vjust = -0.5) +
  labs(title = "Average Daily Visits by Period",
       x = "Period", 
       y = "Average Daily Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# tabla resumen en formato gt
resumen_stats <- stats_by_period
resumen_stats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  gt()

# Estadísticos descriptivos en general
stats <- Datos %>%
summarise(
  Media = mean(Visits, na.rm = TRUE),
  Mediana = median(Visits, na.rm = TRUE),
  Desv_Estandar = sd(Visits, na.rm = TRUE),
  Minimo = min(Visits, na.rm = TRUE),
  Maximo = max(Visits, na.rm = TRUE)
)

print("Estadísticos descriptivos de Daily Visits:")
print(stats)
stats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
gt()

#4. Fechas y cambios entre pre-promo, promo y post-promo
Datos$Date <- as.Date(Datos$Date)
Datos <- Datos %>%
7/
# 5. Asimetría y curtosis
skew_val <- skewness(visits, na.rm = TRUE)
kurt_val <- kurtosis(visits, na.rm = TRUE)

cat("Asimetría (skewness):", round(skew_val, 3), "\n")
cat("Curtosis (kurtosis):", round(kurt_val, 3), "\n")

# 6. Regla Empírica (68-95-99.7)
mu <- mean(visits, na.rm = TRUE)
sigma <- sd(visits, na.rm = TRUE)

within_1sd <- mean(abs(visits - mu) <= 1 * sigma, na.rm = TRUE)
within_2sd <- mean(abs(visits - mu) <= 2 * sigma, na.rm = TRUE)
within_3sd <- mean(abs(visits - mu) <= 3 * sigma, na.rm = TRUE)

empirical_rule <- data.frame(
  Rango = c("±1 sd", "±2 sd", "±3 sd"),
  Porcentaje = c(within_1sd, within_2sd, within_3sd)
)
print("Porcentajes observados según la regla empírica:")
print(empirical_rule)
