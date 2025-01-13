# ==========================================================================
# Install Package Forecasting PDRB (HANYA UNTUK PERTAMA)
install.packages("devtools")
devtools::install_github("dulmaj12id/ForecastPDRB64")
# ==========================================================================



# Library Loading
# --------------------------------------------------------------------------
library(ForecastPDRB64)


# Check & Load Pakcage yang dibutuhkan
# --------------------------------------------------------------------------
ForecastPDRB64::cek.package.nya()


# Data Loading
# --------------------------------------------------------------------------
pdrb_df <- read.xlsx(file.choose())
data.pdrb <- pdrb_df[, 3:ncol(pdrb_df)]


# Forecasting dengan ARIMA dan Simpan Model
# --------------------------------------------------------------------------
mypath <- file.path(choose.dir(caption = "Pilih Lokasi Simpan Model ARIMA"),
                    "ModelARIMA.txt")
sink(mypath)
arima <- pdrb.forecast.arima(data.pdrb)
sink()
cat("Model ARIMA berhasil disimpan di", mypath, "\n")


# Forecasting dengan Exponential Smoothing dan Simpan Model
# --------------------------------------------------------------------------
mypath <- file.path(choose.dir(caption = "Pilih Lokasi Simpan Model Exponential Smoothing"),
                    "ModelExponentialSmoothing.txt")
sink(mypath)
es <- pdrb.forecast.es(data.pdrb)
sink()
cat("Model Exp Smoothing berhasil disimpan di", mypath, "\n")


# Export Hasil Forecasting ke Excel
# --------------------------------------------------------------------------
export.hasil(arima$forecastedval, arima$fittedval,
            es$forecastedval, es$fittedval)
