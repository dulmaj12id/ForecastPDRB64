\name{export.hasil}
\alias{export.hasil}
\title{
Export Kompilasi Forcasted Value dan Fitted Value dari Hasil Pemodelan ARIMA danExponential Smoothing
}
\description{
Melakukan penyimpanan Forcasted Value dan Fitted Value dari Hasil Pemodelan ARIMA
dan Exponential Smoothing dalam file berformat *.xlsx pada folder nomor 4.
}
\usage{
export.hasil(arima.forecastedval,
              arima.fittedval,
              es.forecastedval,
              es.fittedval)
}
\arguments{
  \item{arima.forecastedval}{
data frame forecasting value model ARIMA
}
  \item{arima.fittedval}{
data frame fitted value model ARIMA
}
  \item{es.forecastedvall}{
data frame forecasting value model Exponential Smoothing
}
  \item{es.fittedval}{
data frame fitted value model Exponential Smoothing
}
}
\author{
Muhammad Abdul Majid
}
\note{
Fungsi ini hanya bisa berjalan jika fungsi pdrb.forecast.arima(data_df) dan
pdrb.forecast.es(data_df) sudah menyimpan nilai forecasted value dan fitted value
}
