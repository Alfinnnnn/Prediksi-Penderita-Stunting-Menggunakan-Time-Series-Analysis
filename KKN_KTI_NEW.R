# Mengimpor library yang diperlukan
install.packages("forecast")
library(ggplot2)
library(forecast)

# Membaca dataset dari file CSV
data <- read.csv("stuntingdatabaregbeg_test.csv")

# Menampilkan Data
print(data) # Melihat data

# Mengubah kolom Tanggal menjadi tipe data Date
data$Tanggal <- as.Date(data$Tanggal, format = "%m/%d/%Y")

# Membuat time series dari dataset
ts_data <- ts(data$Jumlah_Stunting, frequency = 2 , start = c(2016, 2)) # Dataset mulai dari bulan agustus 2018

# Menampilkan plot time series
plot(ts_data, main = "Time Series Jumlah Penderita Stunting", xlab = "Tanggal", ylab = "Jumlah Penderita Stunting")

# -------------------------------------------------------------------------------------------- #

# Melakukan peramalan menggunakan metode Holt-Winters
holt_Winters <- hw(ts_data, h = 10)  # Peramalan langsung saat model dibuat, horizon 10 periode ke depan

# Menampilkan plot peramalan menggunakan Holt-Winters
plot(holt_Winters, main = "Prediksi Jumlah Penderita Stunting - Holt-Winters", 
     xlab = "Tanggal", ylab = "Jumlah Penderita Stunting")

# Menampilkan akurasi peramalan menggunakan metode Holt-Winters
akurasi_holt_winters <- accuracy(holt_Winters)
print(akurasi_holt_winters)

# -------------------------------------------------------------------------------------------- #

# Melakukan peramalan menggunakan neural network
neural_model <- nnetar(ts_data)

# Proses peramalan neural network
forecast_neural <- forecast(neural_model, h = 10)  # Meramalkan 10 periode ke depan

# Menampilkan plot peramalan neural network
plot(forecast_neural, main = "Prediksi Jumlah Penderita Stunting - Neural Network", 
     xlab = "Tanggal", ylab = "Jumlah Penderita Stunting")

# Menampilkan akurasi peramalan neural network
akurasi_neural <- accuracy(forecast_neural)
print(akurasi_neural)

# -------------------------------------------------------------------------------------------- #

# Peramalan menggunakan ARIMA
arima_model <- auto.arima(ts_data)

# Proses peramalan ARIMA
forecast_arima <- forecast(arima_model, h = 10)  # Meramalkan 10 periode ke depan

# Menampilkan plot peramalan ARIMA
plot(forecast_arima, main = "Prediksi Jumlah Penderita Stunting - ARIMA", 
     xlab = "Tanggal", ylab = "Jumlah Penderita Stunting")

# Menampilkan akurasi ARIMA
akurasi_arima <- accuracy(forecast_arima)
print(akurasi_arima)

# -------------------------------------------------------------------------------------------- #

# Peramalan menggunakan metode State Space
state_space_model <- StructTS(ts_data, type = "level")

# Proses peramalan State Space
forecast_state_space <- forecast(state_space_model, h = 10)  # Meramalkan 10 periode ke depan

# Menampilkan plot peramalan State Space
plot(forecast_state_space, main = "Prediksi Jumlah Penderita Stunting - State Space", 
     xlab = "Tanggal", ylab = "Jumlah Penderita Stunting")

# Menampilkan akurasi State Space
akurasi_state_space <- accuracy(forecast_state_space)
print(akurasi_state_space)

# -------------------------------------------------------------------------------------------- #

# Menampilkan ringkasan model dan prediksi
summary(forecast_neural)
summary(holt_Winters)
summary(forecast_arima)
summary(forecast_state_space)

