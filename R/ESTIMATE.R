load.package <- function() {
  library(ForecastPDRB64)
  library(openxlsx)
  library(forecast)
  library(mixOmics)
  library(tibble)
  library(ggplot2)
  library(ggcats)
}

cek.package.nya <- function() {
  packages <- c("openxlsx", "forecast", "tibble", "ggplot2")
  packages1 <- c("mixOmics")
  packages2 <- c("ggcats")
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  missing_packages1 <- packages1[!(packages1 %in% installed.packages()[,"Package"])]
  missing_packages2 <- packages2[!(packages2 %in% installed.packages()[,"Package"])]

  if (length(missing_packages1) > 0) {
    cat("Package mixOmics belum terinstall. Package akan diinstall. \n")
    install.packages("BiocManager")
    library(BiocManager)
    BiocManager::install("mixOmics")
    cat("Package mixOmics telah terinstall")
  } else if (length(missing_packages2) > 0) {
    cat("Package ggcats belum terinstall. Package akan diinstall. \n")
    devtools::install_github("R-CoderDotCom/ggcats@main")
    cat("Package ggcats telah terinstall")
  } else if (length(missing_packages) > 0) {
    cat("Package ini belum terinstall:", paste(missing_packages, collapse = ", "), ". Package akan diinstall. \n")
    install.packages(paste0(missing_packages))
    cat("Package ", paste(missing_packages, collapse = ", "), " telah diinstall. \n")
  } else {
    cat("Seluruh package telah diinstall.\n")
  }
  load.package()
  cat("Seluruh package telah di-Load.\n")
}

pdrb.forecast.arima <- function(data_df) {
  # INISIASI
  forecasted_df <- data.frame()
  fitted_df <- data.frame()
  fitted_val <- data.frame()
  plot_list <- list()

  # Melakukan forecasting untuk setiap variabel
  for (i in 1:ncol(data_df)) {
    # Melakukan arima dengan parameter yang ditentukan
    if (i != 3) {
      prediksi <- auto.arima(data_df[, i])
    } else {
      ts_data <- ts(data_df[, i], start = 1, frequency = 4)
      prediksi <- auto.arima(ts_data)
    }

    # Melakukan forecasting
    forecasted_values <- forecast(prediksi, h = 1)

    # Menambahkan hasil forecasting ke dalam dataframe
    forecasted_df[1, i] <- forecasted_values$mean
    forecasted_df[2, i] <- forecasted_values$upper
    forecasted_df[3, i] <- forecasted_values$lower

    # Menambahkan hasil fitted ke dalam dataframe
    fitted_val <- ts(forecasted_values$fitted, frequency = 1)
    for (j in 1:nrow(data_df)) {
      fitted_df[j, i] <- fitted_val[j]
    }

    # Menyimpan hasil forecast dalam plot
    mypath <- file.path("2. ARIMA Plot dan Model/", paste0("ARIMA - ", i, ". ", colnames(data_df[i]), ".png"))
    png(mypath)
    plot_list[i] <- plot(forecasted_values, col = "red", main = names(fitted_val[i + 2]), ylab = "PDRB", xlab = "Triwulan")
    plot_list[i] <- lines(forecasted_values$fitted, pch = 20, col = "green")
    plot_list[i] <- legend("topleft", c("Actual data", "Smoothed data"), lty = 8, col = c("green", "red"), cex = 0.8)
    dev.off()

    # Mencetak hasil forecast dalam list
    cat("-----------------------------------------------------------------------", "\n")
    cat(colnames(data_df)[i], "\n")
    cat("-----------------------------------------------------------------------", "\n")
    print(summary(prediksi))
    cat("\n", "\n")

  }

  # Save forecast
  names(forecasted_df) <- names(data_df)
  # Transpose
  forecasted_df <- t(forecasted_df)
  colnames(forecasted_df) <- c("Mean", "Upper", "Lower")
  forecasted_df <- as.data.frame(forecasted_df)
  # Tambah Kolom
  forecasted_df <- tibble::rownames_to_column(forecasted_df, "Kategori_Subkategori")

  # Save fitted
  names(fitted_df) <- names(data_df)
  fitted_df <- data.frame(pdrb_df[, 1:2], fitted_df)

  # Save Output
  return(list(forecastedval = forecasted_df, fittedval = fitted_df))
}

pdrb.forecast.es <- function(data_df) {
  # INISIASI
  forecasted_df <- data.frame()
  fitted_df <- data.frame()
  fitted_val <- data.frame()
  plot_list <- list()
  folder_image <- choose.dir(default = "", caption = "Pilih folder untuk menyimpan Exponential Smoothing Plot file")

  # Melakukan forecasting untuk setiap variabel
  for (i in 1:ncol(data_df)) {
    # Mengambil data time series
    ts_data <- ts(data_df[, i], start = 1, frequency = 4)

    # Melakukan exponential smoothing dengan parameter yang ditentukan
    if (i != 3) {
      prediksi <- holt(ts_data, damped = TRUE, alpha = NULL, beta = NULL)
    } else {
      prediksi <- ets(ts_data)
    }

    # Melakukan forecasting
    forecasted_values <- forecast(prediksi, h = 1)

    # Menambahkan hasil forecasting ke dalam dataframe
    forecasted_df[1, i] <- forecasted_values$mean
    forecasted_df[2, i] <- forecasted_values$upper
    forecasted_df[3, i] <- forecasted_values$lower

    # Menambahkan hasil fitted ke dalam dataframe
    fitted_val <- ts(forecasted_values$fitted, frequency = 1)
    for (j in 1:nrow(data_df)) {
      fitted_df[j, i] <- fitted_val[j]
    }

    # Menyimpan hasil forecast dalam plot
    mypath <- file.path(folder_image, paste0("Exp Smoothing - ", i, ". ", colnames(data_df[i]), ".png"))
    png(mypath)
    plot_list[i] <- plot(forecasted_values, col = "red", main = names(fitted_val[i + 2]), ylab = "PDRB", xlab = "Triwulan")
    plot_list[i] <- lines(forecasted_values$fitted, pch = 20, col = "green")
    plot_list[i] <- legend("topleft", c("Actual data", "Smoothed data"), lty = 8, col = c("green", "red"), cex = 0.8)
    dev.off()

    # Mencetak hasil forecast dalam list
    cat("-----------------------------------------------------------------------", "\n")
    cat(colnames(data_df)[i], "\n")
    cat("-----------------------------------------------------------------------", "\n")
    print(summary(prediksi))
    cat("\n", "\n")
  }

  # Save forecast
  names(forecasted_df) <- names(data_df)
  # Transpose
  forecasted_df <- t(forecasted_df)
  colnames(forecasted_df) <- c("Mean", "Upper", "Lower")
  forecasted_df <- as.data.frame(forecasted_df)
  # Tambah Kolom
  forecasted_df <- tibble::rownames_to_column(forecasted_df, "Kategori_Subkategori")

  # Save fitted
  names(fitted_df) <- names(data_df)
  fitted_df <- data.frame(pdrb_df[, 1:2], fitted_df)

  # Save Output
  return(list(forecastedval = forecasted_df, fittedval = fitted_df))
}

cat.final <- function(){
  grid <- expand.grid(3, 2)
  df <- data.frame(x = grid[, 1],
                 y = grid[, 2],
                 image = "maru")
  ggplot(df) +
  geom_cat(aes(x, y, cat = image), size = 15) +
    xlim(c(0.25, 5.5)) + 
    ylim(c(0.25, 3.5)) +
      annotate("text", x = 1, y = 1, label = "Terima Kasih")
}

export.hasil <- function(arima.forecastedval, arima.fittedval, es.forecastedval, es.fittedval){
  savetoexcel <- list("Forecast ARIMA" = arima.forecastedval, "Fitted ARIMA" = arima.fittedval,
                      "Forecast Exp Smoothing" = es.forecastedval, "Fitted Exp Smoothing" = es.fittedval)

  # Open a window to choose the folder to save the file
  folder_path <- choose.dir(default = "", caption = "Pilih folder untuk menyimpan Excel file hasil forecasting")

  file_path <- file.path(folder_path, "Hasil Forecasting ARIMA dan EXPONENTIAL SMOOTHING.xlsx")
  write.xlsx(savetoexcel, file = file_path)
  cat("\n =================================================================================================================",
      "\n File Excel Forcasted Value dan Fitted Value telah disimpan di ", folder_path,
      "\n =================================================================================================================")
  cat.final()
}
