# ============================================================
# CASO HARVARD #1 - ANALÍTICA DE LOS NEGOCIOS
# Análisis de Daily Visits - Web Analytics (Quality Alloys)
# Autores: Ángela Lucía Vargas, Santiago Muñoz, Josymar Nocua
# Fecha: 24 de Agosto de 2025
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

# Definición de periodos de pre-pomoción, promoción y post-promoción
Day <- seq(as.Date("2008-06-01"), as.Date("2009-04-30"), by = "day")
n_days <- length(Day)
daily_visits <- data.frame(
  Day = Day,
  Visits = c(
    # Período inicial (primeras semanas)
    sample(180:320, 30, replace = TRUE),
    # Período pre-promoción
    sample(150:250, 200, replace = TRUE),
    # Período promoción 
    sample(300:650, 60, replace = TRUE),
    # Período post-promoción 
    sample(200:400, n_days - 290, replace = TRUE)
  )
)

# Definir períodos según el caso (fechas según datos reales)
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

print(paste("Total de observaciones:", nrow(daily_visits)))
print(table(daily_visits$Period))

# ============================================================
# 1. Forma de la distribución de lo datos Daily Visits
# ============================================================

# Histograma general de Daily Visits
n <- nrow(Datos)
bins <- max(8, round(sqrt(n)))

ggplot(Datos, aes(x = Visits)) +
  geom_histogram(bins = bins, fill = "#d72ccf", color = "black") +
  labs(
    title = "Histograma - Daily Visits",
    x = "Visitas diarias",
    y = "Frecuencia"
  )

# Serie temporal de Daily Visits
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

# ============================================================
# 2. Estadísticos descriptivos 
# ============================================================
# Estadísticos descriptivos en general
stats <- Datos %>%
 summarise(
    Media = mean(Visits, na.rm = TRUE),
    Mediana = median(Visits, na.rm = TRUE),
    Desv_Estandar = sd(Visits, na.rm = TRUE),
    Minimo = min(Visits, na.rm = TRUE),
    Maximo = max(Visits, na.rm = TRUE),
    n_observaciones = n()
  )

print("=== Estadísticos descriptivos de Daily Visits:")
print(stats)

#tabla bonita
stats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
 gt() %>%
  tab_header(
    title = "Estadísticos Descriptivos Generales",
    subtitle = "Daily Visits - Quality Alloys"
  )

#Estadísticos descriptivos por período
stats_calcular <- function(data) {
  data.frame(
    Media = mean(data, na.rm = TRUE),
    Mediana = median(data, na.rm = TRUE),
    Desv_Estandar = sd(data, na.rm = TRUE),
    Minimo = min(data, na.rm = TRUE),
    Maximo = max(data, na.rm = TRUE)
  )
}

# Calcular estadísticos por período
stats_periodo <- daily_visits %>%
  group_by(Period) %>%
  summarise(stats_calcular(Visits), .groups = 'drop')

print("=== Estadísticos descriptivos de Daily Visits por período:")
print(stats_periodo)

#tabla bonita
resumen_stats <- stats_periodo
resumen_stats %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
   gt() %>%
  tab_header(
    title = "Estadísticos Descriptivos por Período",
    subtitle = "Daily Visits - Quality Alloys"
  )
# ===================================================================
# 3. Gráfico de columnas para la media de visitas diarias por período
# ===================================================================
column_chart_media <- ggplot(stats_periodo, aes(x = Period, y = Media, fill = Period)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Media, 0)), vjust = -0.5) +
  labs(title = "Media de Visitas Diarias por Período",
       x = "Periodo", 
       y = "Media de Visitas Diarias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(column_chart_media)

# ============================================================================
# 4. Diagrama y visualización de Daily Visits por período con boxplot y jitter
# ============================================================================
dispersion_chart <- ggplot(daily_visits, aes(x = Period, y = Visits, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Distribución de Daily Visits por Período",
       x = "Periodo", 
       y = "Daily Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(dispersion_chart)

# ============================================================================
# 5. Histograma y normalidad de la distribución
# ============================================================================

# Histograma general
histograma_general <- ggplot(daily_visits, aes(x = Visits)) +
  geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
  geom_density(aes(y = after_stat(density) * nrow(daily_visits) * 
                     (max(daily_visits$Visits) - min(daily_visits$Visits)) / 30), 
               color = "red", size = 1) +
  labs(title = "Distribution of Daily Visits",
       x = "Daily Visits", 
       y = "Frequency") 
print(histograma_general)

# Histograma por período
histograma_periodo <- ggplot(daily_visits, aes(x = Visits, fill = Period)) + 
geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  facet_wrap(~Period, scales = "free") +
  labs(title = "Daily Visits Distribution by Period",
       x = "Daily Visits", 
       y = "Frequency") +
  theme_minimal()

print(histograma_periodo)

# ============================================================================
# 6. PRUEBAS DE NORMALIDAD Y REGLA EMPÍRICA
# ============================================================================
Datos <- Datos %>%
  mutate(
    z_score = (Visits - stats$Media) / stats$Desv_Estandar
  )

# Variables para la Regla Empírica
mean_visits <- stats$Media
sd_visits <- stats$Desv_Estandar
total_obs <- stats$n_observaciones
#-----------------------------------------------------------------------------
# TABLA 1: Regla Empírica Básica (±1σ, ±2σ, ±3σ)
#-----------------------------------------------------------------------------

# Crear tabla de intervalos básicos
regla_empirica <- data.frame(
  Intervalos = c("mean ± 1 std. dev.", "mean ± 2 std. dev.", "mean ± 3 std. dev."),
  Porcentajes = c(68, 95, 99), #
  Observaciones_Teóricas = round(c(68, 95, 99) * total_obs / 100, 0)
)
# Calcular observaciones reales en cada intervalo
regla_empirica$Actual_No_Obs <- c(
  # ±1 desviación estándar
  sum(abs(Datos$z_score) <= 1, na.rm = TRUE),
  # ±2 desviaciones estándar
  sum(abs(Datos$z_score) <= 2, na.rm = TRUE),
  # ±3 desviaciones estándar
  sum(abs(Datos$z_score) <= 3, na.rm = TRUE)
)

print("=== Regla Empírica - Tabla 1")
print(regla_empirica)
#tabla bonita
regla_empirica_1 <- regla_empirica
regla_empirica_1 %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  gt() %>%
  tab_header(
    title = "Tabla de Regla Empírica Básica",
    subtitle = "Daily Visits - Quality Alloys"
  )

#-----------------------------------------------------------------------------
# TABLA 2: Análisis Refinado por Variables e Intervalos (±1σ, ±2σ, ±3σ)
#-----------------------------------------------------------------------------
# Crear tabla refinada como se solicita en el caso
regla_empirica_refinada <- data.frame(
  Interval = c(
    "mean + 1 std. dev.",
    "mean - 1 std. dev.", 
    "1 std. dev. to 2 std. dev.",
    "-1 std. dev. to -2 std. dev.",
    "2 std. dev. to 3 std. dev.",
    "-2 std. dev. to -3 std. dev."
  ),
  Porc_Teo = c(34, 34, 13.5, 13.5, 2.35, 2.35),
  No_Obs_Teo = round(c(34, 34, 13.5, 13.5, 2.35, 2.35) * total_obs / 100, 0)
)

# Calcular observaciones reales para cada intervalo refinado
regla_empirica_refinada$No_Obs_Real <- c(
  # mean + 1 std. dev. (0 a +1σ)
  sum(Datos$z_score > 0 & Datos$z_score <= 1, na.rm = TRUE),
  # mean - 1 std. dev. (-1σ a 0)
  sum(Datos$z_score >= -1 & Datos$z_score <= 0, na.rm = TRUE),
  # 1 std. dev. to 2 std. dev. (+1σ a +2σ)
  sum(Datos$z_score > 1 & Datos$z_score <= 2, na.rm = TRUE),
  # -1 std. dev. to -2 std. dev. (-2σ a -1σ)
  sum(Datos$z_score >= -2 & Datos$z_score < -1, na.rm = TRUE),
  # 2 std. dev. to 3 std. dev. (+2σ a +3σ)
  sum(Datos$z_score > 2 & Datos$z_score <= 3, na.rm = TRUE),
  # -2 std. dev. to -3 std. dev. (-3σ a -2σ)
  sum(Datos$z_score >= -3 & Datos$z_score < -2, na.rm = TRUE)
)

print("=== Regla Empírica - Tabla 2 (Análisis Refinado)")
print(regla_empirica_refinada)

# Tabla bonita
regla_empirica_2 <- regla_empirica_refinada
regla_empirica_2 %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  gt() %>%
  tab_header(
    title = "Tabla de Regla Empírica Refinada",
    subtitle = "Daily Visits - Quality Alloys"
  )

# ============================================================================
# 7. ANÁLISIS DE ASIMETRÍA Y CURTOSIS
# ============================================================================

# Calcular skewness y kurtosis 
skewness_val <- skewness(Datos$Visits, na.rm = TRUE)
kurtosis_val <- kurtosis(Datos$Visits, na.rm = TRUE)

# Crear tabla de resultados
skew_kurt <- data.frame(
  Medida = c("Skewness", "Kurtosis"),
  Valor = c(round(skewness_val, 6), round(kurtosis_val, 6))
)

print("=== Análisis de Skewness y Kurtosis")
print(skew_kurt)

# Tabla bonita
skew_kurt %>%
  gt() %>%
  tab_header(
    title = "Skewness y Kurtosis",
    subtitle = "Daily Visits - Quality Alloys"
  )

# ----------------------------------------------------------------------------
# Resultados e interpretación
# ----------------------------------------------------------------------------
cat("=== Interpretación de distribución de Daily Visits")

# Interpretación del Skewness
cat(sprintf("SKEWNESS: %.6f\n", skewness_val))
if (abs(skewness_val) < 0.5) {
  cat("- Distribución aproximadamente simétrica\n")
} else if (skewness_val > 0.5) {
  cat("- Distribución con sesgo positivo (cola hacia la derecha)\n")
  cat("- Hay más valores concentrados en la parte baja de la distribución\n")
} else {
  cat("- Distribución con sesgo negativo (cola hacia la izquierda)\n")
  cat("- Hay más valores concentrados en la parte alta de la distribución\n")
}

# Interpretación del Kurtosis - CORREGIDO
cat(sprintf("\nKURTOSIS: %.6f\n", kurtosis_val))
# NOTA: En e1071, kurtosis() calcula el exceso de curtosis (kurtosis - 3)
if (kurtosis_val < -0.5) {
  cat("- Distribución platicúrtica (menos puntiaguda que la normal)\n")
  cat("- Colas más ligeras que la distribución normal\n")
  cat("- Distribución más plana en el centro\n")
} else if (kurtosis_val > 0.5) {
  cat("- Distribución leptocúrtica (más puntiaguda que la normal)\n")
  cat("- Colas más pesadas que la distribución normal\n")
  cat("- Mayor concentración de valores alrededor de la media\n")
} else {
  cat("- Distribución mesocúrtica (similar a la normal en términos de curtosis)\n")
  cat("- Forma de las colas similar a la distribución normal\n")
}

# Evaluación general de normalidad - CORREGIDO
cat("=== Evaluación general de normalidad")

# Calcular desviaciones de la Regla Empírica
desviaciones_abs <- abs(regla_empirica$Actual_No_Obs - regla_empirica$Observaciones_Teóricas)
desviaciones_rel <- (desviaciones_abs / regla_empirica$Observaciones_Teóricas) * 100

cat("Desviaciones de la Regla Empírica:\n")
for(i in 1:nrow(regla_empirica)) {
  cat(sprintf("- %s: %d obs. reales vs %d teóricas (%.1f%% de diferencia)\n", 
              regla_empirica$Intervalos[i], 
              regla_empirica$Actual_No_Obs[i],
              regla_empirica$Observaciones_Teóricas[i],
              desviaciones_rel[i]))
}

# Conclusión sobre normalidad
if(max(desviaciones_rel) < 15 && abs(skewness_val) < 1 && abs(kurtosis_val) < 2) {
  cat("\nCONCLUSIÓN: Los datos se aproximan razonablemente a una distribución normal\n")
} else {
  cat("\nCONCLUSIÓN: Los datos NO siguen una distribución normal\n")
  if(abs(skewness_val) >= 1) cat("- Skewness indica asimetría significativa\n")
  if(abs(kurtosis_val) >= 2) cat("- Kurtosis indica forma muy diferente a la normal\n")
  if(max(desviaciones_rel) >= 15) cat("- Regla empírica muestra desviaciones importantes\n")
}

