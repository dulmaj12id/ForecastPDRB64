# Install Package Forecasting PDRB
install.packages("devtools")
devtools::install_github("dulmaj12id/ForecastPDRB64")

# Check Pakcage yang dibutuhkan
cek.package.nya()
# Install Package yang belum tersedia dengan fungsi instal.package("nama package")


# Library Loading

library(ForecastPDRB64)
ForecastPDRB64::load.package()


# Data Loading

pdrb_df <- read.xlsx(file.choose())
data.pdrb <- pdrb_df[, 3:ncol(pdrb_df)]


# Forecasting dengan ARIMA
# Simpan gambar

mypath <- file.path(choose.dir(caption = "Pilih Lokasi Simpan Model ARIMA"), "ModelARIMA.txt")
sink(mypath)
arima <- pdrb.forecast.arima(data.pdrb)
sink()

cat("Model ARIMA berhasil disimpan di", mypath, "\n")



# Forecasting dengan Exponential Smoothing
# Simpan gambar

mypath <- file.path(choose.dir(caption = "Pilih Lokasi Simpan Model Exponential Smoothing"), "ModelExponentialSmoothing.txt")
sink(mypath)
es <- pdrb.forecast.es(data.pdrb)
sink()


cat("Model Exp Smoothing berhasil disimpan di", mypath, "\n")



# Export Hasil Forecasting ke Excel

export.hasil(arima$forecastedval, arima$fittedval,
             es$forecastedval, es$fittedval)
