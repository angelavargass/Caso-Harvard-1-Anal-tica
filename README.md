# 📊 Caso Harvard 1 - Web Analytics (Quality Alloys)  (Ángela Lucía Vargas, Josymar Yised Nocua, Santiago Muñoz Moreno)

Este repositorio contiene el análisis de **Daily Visits** al sitio web de *Quality Alloys*, siguiendo el caso de Harvard sobre *Web Analytics*.  
El trabajo se centra únicamente en los **visitas diarias** (Daily Visits), dejando de lado los registros semanales.

## 🎯 Objetivo

Analizar el comportamiento de las visitas diarias al sitio web en diferentes períodos del caso (Initial, Pre-Promotion, Promotion, Post-Promotion) para:

- Visualizar tendencias y distribuciones.
- Calcular estadísticos descriptivos.
- Evaluar normalidad con la **Regla Empírica**.
- Analizar **asimetría (skewness)** y **curtosis (kurtosis)**.
- Generar gráficas que respalden la interpretación.

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
