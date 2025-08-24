# 📊 Caso Harvard 1 - Web Analytics (Quality Alloys)

Este repositorio contiene el análisis de **Daily Visits** al sitio web de *Quality Alloys*, siguiendo el caso de Harvard sobre *Web Analytics*.  
El trabajo se centra únicamente en los **visitas diarias** (Daily Visits), dejando de lado los registros semanales.

## 🎯 Objetivo

Analizar el comportamiento de las visitas diarias al sitio web en diferentes períodos del caso (Initial, Pre-Promotion, Promotion, Post-Promotion) para:

- Visualizar tendencias y distribuciones.
- Calcular estadísticos descriptivos.
- Evaluar normalidad con la **Regla Empírica**.
- Analizar **asimetría (skewness)** y **curtosis (kurtosis)**.
- Generar gráficas que respalden la interpretación.

📂 Caso-Harvard-1-Analitica Contiene las siguientes carpetas
 ┣ 📁 graficas → Contiene todas las gráficas generadas con el código (histogramas, series temporales, boxplots, etc.).  
 ┣ 📁 codigo   → Incluye el script en R (`Trabajo Analitica.r`) con todo el análisis.  
 ┣ 📁 datos    → Contiene un documento Word con la explicación de los resultados del análisis y las gráficas, además del archivo original `Web_Analytics.xls`.  


## 🛠️ Librerías utilizadas

El script en R requiere las siguientes librerías:

r
install.packages(c("readxl", "ggplot2", "e1071", "dplyr", "lubridate", "gt"))
library(readxl)
library(ggplot2)
library(e1071)
library(dplyr)
library(lubridate)
library(gt)

