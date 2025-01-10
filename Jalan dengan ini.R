# Install Package (JIKA BELUM)

install.packages(c("ggplot2", "forecast", "openxlsx", "tibble", "BiocManager", "devtools"))

library(BiocManager)
BiocManager::install("mixOmics")

library("devtools")
devtools::install_github("dulmaj12id/ForecastPDRB64")


# Library Loading

library(ForecastPDRB64)
library(openxlsx)
library(forecast)
library(mixOmics)
library(tibble)
library(ggplot2)


# Database Loading

pdrb_df <- read.xlsx(file.choose())
data.pdrb <- pdrb_df[, 3:ncol(pdrb_df)]


# Forecasting dengan ARIMA
# Simpan gambar

mypath <- file.path(choose.dir(caption = "Pilih Lokasi Simpan Model ARIMA"), "ModelARIMA.txt")
sink(mypath)
arima <- pdrb.forecast.arima(data.pdrb)
sink()

cat("=============================================\n")
cat("Model ARIMA berhasil disimpan di", mypath, "\n")
cat("=============================================\n")


# Forecasting dengan Exponential Smoothing
# Simpan gambar

mypath <- file.path(choose.dir(caption = "Pilih Lokasi Simpan Model Exp Smoothing"), "ModelExpSmoothing.txt")
sink(mypath)
es <- pdrb.forecast.es(data.pdrb)
sink()

cat("=============================================\n")
cat("Model Exp Smoothing berhasil disimpan di", mypath, "\n")
cat("=============================================\n")


# Export Hasil Forecasting ke Excel

export.hasil(arima$forecastedval, arima$fittedval,
             es$forecastedval, es$fittedval)
