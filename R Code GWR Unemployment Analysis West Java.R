# Code R Tugas 3 Spasial - Kelompok I

library(sf)
library(sp)
library(spdep)
library(lmtest)
library(GWmodel)
library(car)
library(ggplot2)
library(ggpubr)
library(readxl)
library(MASS) 
library(tidyr)
library(dplyr)
        
data <- read_excel("C:/Users/LENOVO/Downloads/Data Provinsi Jawa Barat 2024.xlsx")
names(data)


# Model full tanpa IPM (karena multikolinear dengan lama sekolah)
model_full1 <- lm(`Tingkat Pengangguran Terbuka (TPT)` ~
                    `Tingkat Partisipasi Angkatan Kerja (TPAK)` +
                    `Jumlah Perusahaan Industri Mikro & Kecil` +
                    `Jumlah Tenaga Kerja` +
                    `Persentase Pertumbuhan PDRB per Kapita` +
                    `Rata-rata Lama Sekolah (Tahun)` +
                    `Jarak ke Ibukota (km)`,
                  data = data)

# Model full tanpa Rata-rata Lama Sekolah
model_full2 <- lm(`Tingkat Pengangguran Terbuka (TPT)` ~
                    `Tingkat Partisipasi Angkatan Kerja (TPAK)` +
                    `Jumlah Perusahaan Industri Mikro & Kecil` +
                    `Jumlah Tenaga Kerja` +
                    `Persentase Pertumbuhan PDRB per Kapita` +
                    `Indeks Pembangunan Manusia (IPM)` +
                    `Jarak ke Ibukota (km)`,
                  data = data)

model_full3 <- lm(`Tingkat Pengangguran Terbuka (TPT)` ~
                    `Tingkat Partisipasi Angkatan Kerja (TPAK)` +
                    `Jumlah Perusahaan Industri Mikro & Kecil` +
                    `Jumlah Tenaga Kerja` +
                    `Persentase Pertumbuhan PDRB per Kapita` +
                    `Rata-rata Lama Sekolah (Tahun)` + 
                    `Jarak ke Ibukota (km)` +
                    `Persentase Penduduk Miskin` +
                    `Jumlah Perguruan Tinggi` +
                    `Jumlah Rumah Sakit Umum` +
                    `Tingkat Kegemaran Membaca` +
                    `Persentase Keluhan Kesehatan & Rawat Jalan`,
                  data = data)

# Stepwise
step_model1 <- stepAIC(model_full1, direction = "both")
summary(step_model1)

step_model2 <- stepAIC(model_full2, direction = "both")
summary(step_model2)

step_model3 <- stepAIC(model_full3, direction = "both")
summary(step_model3)

summary(step_model1)
summary(step_model2)
summary(step_model3)
################################################################################################################
#---------------------------------------------------------------------------------------------------------------
# Ambil variabel dari model terbaik
data_gwr <- data[, c(
  "Tingkat Pengangguran Terbuka (TPT)",
  "Tingkat Partisipasi Angkatan Kerja (TPAK)",
  "Jumlah Perusahaan Industri Mikro & Kecil",
  "Jarak ke Ibukota (km)",
  "Jumlah Rumah Sakit Umum",
  "Longitude", "Latitude"
)]

# Ubah Nama Variabel
names(data_gwr) <- c("TPT", "TPAK", "UMKM", "Jarak", "RSU", "long", "lat")
names(data_gwr)

# Ubah latitude dan longitude jadi numeric
data_gwr$long <- as.numeric(data_gwr$long)
data_gwr$lat <- as.numeric(data_gwr$lat)

summary(data_gwr$long)
summary(data_gwr$lat)

# Cek data
head(data_gwr)
summary(data_gwr)

###################### ------- STAT DESKRIPTIF ------- ######################
shape <- st_read("C:/Users/LENOVO/Downloads/Jawa_Barat_ADMIN_BPS.shp")
shape <- shape %>% filter(Kabupaten != "Waduk Cirata")

shape$TPT <- data$`Tingkat Pengangguran Terbuka (TPT)`
ggplot(shape) +
  geom_sf(aes(fill = TPT), color = "black", size = 0.3) +
  scale_fill_gradient(low = "#a5d8ff", high = "#ffffcc") +
  labs(title = "Peta Sebaran Tingkat Pengangguran Terbuka (TPT)", fill = "%") +
  theme_minimal()
ggplot(data, aes(y = `Tingkat Pengangguran Terbuka (TPT)`)) +
  geom_boxplot(fill = "#ffffcc", color = "black") +
  labs(title = "Boxplot Tingkat Pengangguran Terbuka (TPT)", y = "%") +
  theme_minimal()
ggplot(data, aes(x = `Tingkat Pengangguran Terbuka (TPT)`)) +
  geom_histogram(bins = 10, fill = "#cce5ff", color = "black") +
  labs(title = "Histogram Tingkat Pengangguran Terbuka (TPT)", x = "%", y = "Frekuensi") +
  theme_minimal()

shape$TPAK <- data$`Tingkat Partisipasi Angkatan Kerja (TPAK)`
shape$UMKM <- data$`Jumlah Perusahaan Industri Mikro & Kecil`
shape$Jarak <- data$`Jarak ke Ibukota (km)`
shape$RSU <- data$`Jumlah Rumah Sakit Umum`
shape$LamaSekolah <- data$`Rata-rata Lama Sekolah (Tahun)`

# 1. PETA SEBARAN
make_choropleth <- function(var, judul, label) {
  ggplot(shape) +
    geom_sf(aes(fill = .data[[var]]), color = "black", size = 0.3) +
    scale_fill_gradient(low = "#a5d8ff", high = "#ffffcc") +
    labs(title = paste("Peta Sebaran", judul), fill = label) +
    theme_minimal()
}

# 2. BOXPLOT 
make_boxplot <- function(df, var, judul) {
  ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#ffffcc", color = "black") +
    labs(title = paste("Boxplot", judul), y = judul) +
    theme_minimal()
}

# 3. HISTOGRAM
make_histogram <- function(df, var, judul) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 10, fill = "#cce5ff", color = "black") +
    labs(title = paste("Histogram", judul), x = judul, y = "Frekuensi") +
    theme_minimal()
}

# --- Visualisasi: TPAK ---
make_choropleth("TPAK", "Tingkat Partisipasi Angkatan Kerja", "%")
make_boxplot(data, "Tingkat Partisipasi Angkatan Kerja (TPAK)", "Tingkat Partisipasi Angkatan Kerja")
make_histogram(data, "Tingkat Partisipasi Angkatan Kerja (TPAK)", "Tingkat Partisipasi Angkatan Kerja")

# --- Visualisasi: UMKM ---
make_choropleth("UMKM", "Jumlah Perusahaan UMKM", "Jumlah")
make_boxplot(data, "Jumlah Perusahaan Industri Mikro & Kecil", "Jumlah Perusahaan UMKM")
make_histogram(data, "Jumlah Perusahaan Industri Mikro & Kecil", "Jumlah Perusahaan UMKM")

# --- Visualisasi: Jarak ---
make_choropleth("Jarak", "Jarak ke Ibukota", "Km")
make_boxplot(data, "Jarak ke Ibukota (km)", "Jarak ke Ibukota")
make_histogram(data, "Jarak ke Ibukota (km)", "Jarak ke Ibukota")

# --- Visualisasi: RSU ---
make_choropleth("RSU", "Jumlah Rumah Sakit Umum", "Jumlah")
make_boxplot(data, "Jumlah Rumah Sakit Umum", "Jumlah Rumah Sakit Umum")
make_histogram(data, "Jumlah Rumah Sakit Umum", "Jumlah Rumah Sakit Umum")



###################### ------- ANALISIS REGRESI GLOBAL ------- ######################
# Model OLS
model_ols <- lm(`Tingkat Pengangguran Terbuka (TPT)` ~
                  `Tingkat Partisipasi Angkatan Kerja (TPAK)` +
                  `Jumlah Perusahaan Industri Mikro & Kecil` +
                  `Jarak ke Ibukota (km)` +
                  `Jumlah Rumah Sakit Umum`,
                data = data)

# Ringkasan hasil model
summary(model_ols)

### CEK ASUMSI
# [1] UJI F dan RINGKASAN MODEL OLS
summary(model_ols)

# [2] MULTIKOLINEARITAS – VIF
vif(model_ols)

# [3] UJI NORMALITAS RESIDUAL
# (a) Shapiro-Wilk Test
shapiro.test(residuals(model_ols))

# (b) Q-Q Plot
qqnorm(residuals(model_ols))
qqline(residuals(model_ols), col = "red")

# [4] AUTOKORELASI – Durbin-Watson
dwtest(model_ols)

# [5] HETEROSKEDASTISITAS – Breusch-Pagan Test
bptest(model_ols)

###################### ------- ANALISIS GEOGRAPHICALLY WEIGHTED REGRESSION (GWR) ------- ######################
data_sf <- st_as_sf(data_gwr, coords = c("long", "lat"), crs = 4326)
data_sp <- as(data_sf, "Spatial")

# Membuat matriks jarak
m_jarak <- gw.dist(dp.locat = coordinates(data_sp))

names(data_sp) <- c("TPT", "TPAK", "UMKM", "Jarak", "RSU")

# Menyiapkan function untuk plot pemilihan model GWR
generate_gwr_selection_plot <- function(dep_var, indep_vars, kernel_type, title_prefix, bw_val = 1) {
  message("Running GWR pseudo-stepwise for kernel: ", kernel_type)
  
  # Step 1: Model selection
  model_sel <- model.selection.gwr(
    dep_var,
    indep_vars,
    data = data_sp,
    kernel = kernel_type,
    bw = bw_val,
    adaptive = FALSE,
    approach = "CV",
    dMat = m_jarak
  )
  
  # Step 2: Sort models
  sorted_models <- model.sort.gwr(
    model_sel,
    numVars = length(indep_vars),
    ruler.vector = model_sel[[2]][, 2]
  )
  
  model_list <- sorted_models[[1]]
  
  # Step 3A: Circular view
  model.view.gwr(
    dep_var,
    indep_vars,
    model.list = model_list
  )
  
  # Step 3B: CV plot
  plot(
    sorted_models[[2]][, 2],
    type = "b", col = "black", pch = 20, lty = 5,
    main = paste("CV Score - Kernel", title_prefix),
    ylab = "CV Score", xlab = "Model number"
  )
}

InDeVars_clean <- c("TPAK", "UMKM", "Jarak", "RSU")

# Kernel Gaussian
generate_gwr_selection_plot("TPT", InDeVars_clean, "gaussian", "Gaussian")

# Kernel Bisquare 
generate_gwr_selection_plot("TPT", InDeVars_clean, "bisquare", "Bisquare", bw_val = 6)

# Kernel Tricube 
generate_gwr_selection_plot("TPT", InDeVars_clean, "tricube", "Tricube", bw_val = 6)

### Penentuan Bandwith Optimum
# Kernel Gaussian
bw_gauss <- bw.gwr(
  formula = TPT ~ TPAK + UMKM + Jarak + RSU,  
  data = data_sp,
  approach = "CV",  # Cross-validation
  kernel = "gaussian",
  dMat = m_jarak,
  adaptive = FALSE
)
cat("Bandwidth Optimum (Gaussian):", bw_gauss, "\n\n")

# Kernel Bisquare 
bw_bisq <- bw.gwr(
  formula = TPT ~ TPAK + UMKM + Jarak + RSU,  
  data = data_sp,
  approach = "CV",
  kernel = "bisquare",
  dMat = m_jarak,
  adaptive = FALSE
)
cat("Bandwidth Optimum (Bisquare):", bw_bisq, "\n\n")

# Kernel Tricube 
bw_tricube <- bw.gwr(
  formula = TPT ~ TPAK + UMKM + Jarak + RSU,  
  data = data_sp,
  approach = "CV",
  kernel = "tricube",
  dMat = m_jarak,
  adaptive = FALSE
)
cat("Bandwidth Optimum (Tricube):", bw_tricube, "\n\n")

### Estimasi model GWR untuk tiap kernel 
gwr_gauss <- gwr.basic(TPT ~ TPAK + UMKM + Jarak + RSU,
                       data = data_sp,
                       bw = bw_gauss,
                       kernel = "gaussian",
                       adaptive = FALSE)

gwr_bisq <- gwr.basic(TPT ~ TPAK + UMKM + Jarak + RSU,
                      data = data_sp,
                      bw = bw_bisq,
                      kernel = "bisquare",
                      adaptive = FALSE)

gwr_tri <- gwr.basic(TPT ~ TPAK + UMKM + Jarak + RSU,
                     data = data_sp,
                     bw = bw_tricube,
                     kernel = "tricube",
                     adaptive = FALSE)


# Perbandingan AIC dan Mean Local R²
model_metrics <- data.frame(
  Kernel = c("Gaussian", "Bisquare", "Tricube"),
  AIC = c(gwr_gauss$GW.diagnostic$AIC,
          gwr_bisq$GW.diagnostic$AIC,
          gwr_tri$GW.diagnostic$AIC),
  Mean_Local_R2 = c(mean(gwr_gauss$SDF$Local_R2),
                    mean(gwr_bisq$SDF$Local_R2),
                    mean(gwr_tri$SDF$Local_R2))
)

print(model_metrics)

# Model terbaik (AIC terkecil dan/atau R² tertinggi)
best_model_aic <- model_metrics[which.min(model_metrics$AIC), ]
best_model_r2 <- model_metrics[which.max(model_metrics$Mean_Local_R2), ]

cat("\nModel dengan AIC terkecil:\n")
print(best_model_aic)

cat("\nModel dengan R² lokal rata-rata tertinggi:\n")
print(best_model_r2)

# AIC dan R-squared untuk OLS
aic_ols <- AIC(model_ols)
r2_ols <- summary(model_ols)$r.squared

cat("AIC OLS:", aic_ols, "\n")
cat("R-squared OLS:", r2_ols, "\n")

###################### -------  HASIL ------- ######################
shape <- st_read("C:/Users/LENOVO/Downloads/Jawa_Barat_ADMIN_BPS.shp")
shape <- shape %>% filter(Kabupaten != "Waduk Cirata")

kabupaten_names <- shape$Kabupaten
data_sp$Kabupaten <- kabupaten_names

head(data_sp$Kabupaten)
head(shape$Kabupaten)

# Hitung nilai p-value dari koefisien masing-masing di GWR
pvalue_df <- data.frame(
  Kabupaten = kabupaten_names,
  TPAK_p = 2 * (1 - pnorm(abs(gwr_gauss$SDF$TPAK / gwr_gauss$SDF$TPAK_SE))),
  UMKM_p = 2 * (1 - pnorm(abs(gwr_gauss$SDF$UMKM / gwr_gauss$SDF$UMKM_SE))),
  Jarak_p = 2 * (1 - pnorm(abs(gwr_gauss$SDF$Jarak / gwr_gauss$SDF$Jarak_SE))),
  RSU_p = 2 * (1 - pnorm(abs(gwr_gauss$SDF$RSU / gwr_gauss$SDF$RSU_SE)))
)

shape_merged <- merge(shape, pvalue_df, by = "Kabupaten", all.x = TRUE)
head(shape_merged)

# Tentukan variabel yg signifikan di setiap wilayah
pvalue_df$Kelompok <- apply(pvalue_df[, -1], 1, function(row) {
  signif_vars <- names(row)[which(row < 0.05)]  # 0.05 adalah threshold untuk signifikan
  if (length(signif_vars) == 0) {
    return("None")
  } else {
    return(paste(signif_vars, collapse = ", "))
  }
})

shape$Kabupaten <- data_sp$Kabupaten  # Menambahkan kolom Kabupaten di shapefile
shape_merged <- merge(shape, pvalue_df, by = "Kabupaten", all.x = TRUE)

# Visualisasi Peta Sebaran Variabel Signifikan
ggplot(shape_merged) +
  geom_sf(aes(fill = Kelompok), color = "white") +
  scale_fill_brewer(palette = "Set3", na.value = "gray") +  # Ubah palette jika perlu
  labs(title = "Peta Sebaran Variabel Signifikan Berdasarkan GWR",
       subtitle = "Menampilkan variabel signifikan per kabupaten/kota",
       fill = "Variabel Signifikan") +
  theme_minimal()

table(shape_merged$Kelompok) 

# Tentukan kelompok berdasarkan p-value signifikan
pvalue_df$Kelompok <- apply(pvalue_df[, -1], 1, function(row) {
  signif_vars <- names(row)[which(row < 0.05)]  # 0.05 adalah threshold untuk signifikan
  if (length(signif_vars) == 0) {
    return("None")
  } else {
    return(paste(signif_vars, collapse = ", "))
  }
})

# Tabel jumlah kabupaten/kota berdasarkan kombinasi variabel yang signifikan
table_by_kelompok <- table(pvalue_df$Kelompok)

# Menampilkan jumlah kabupaten/kota untuk masing-masing kelompok
print(table_by_kelompok)

kabupaten_per_kelompok <- lapply(unique(pvalue_df$Kelompok), function(kelompok) {
  kabupaten_list <- pvalue_df$Kabupaten[pvalue_df$Kelompok == kelompok]
  return(data.frame(Kelompok = kelompok, Kabupaten = paste(kabupaten_list, collapse = ", "), Jumlah = length(kabupaten_list)))
})

final_table <- do.call(rbind, kabupaten_per_kelompok)
print(final_table)

###################### ------- VISUALISASI PER VARIABEL ------- ######################
# --- Tambah Koefisien dan P-value untuk TPAK ---
shape$Beta_TPAK <- gwr_gauss$SDF$TPAK
shape$Pval_TPAK <- 2 * (1 - pnorm(abs(gwr_gauss$SDF$TPAK / gwr_gauss$SDF$TPAK_SE)))

# --- UMKM ---
shape$Beta_UMKM <- gwr_gauss$SDF$UMKM
shape$Pval_UMKM <- 2 * (1 - pnorm(abs(gwr_gauss$SDF$UMKM / gwr_gauss$SDF$UMKM_SE)))

# --- Jarak ---
shape$Beta_Jarak <- gwr_gauss$SDF$Jarak
shape$Pval_Jarak <- 2 * (1 - pnorm(abs(gwr_gauss$SDF$Jarak / gwr_gauss$SDF$Jarak_SE)))

# --- RSU ---
shape$Beta_RSU <- gwr_gauss$SDF$RSU
shape$Pval_RSU <- 2 * (1 - pnorm(abs(gwr_gauss$SDF$RSU / gwr_gauss$SDF$RSU_SE)))

# --- TPAK ---
# Peta P-value
ggplot(shape) +
  geom_sf(aes(fill = Pval_TPAK), color = "white") +
  scale_fill_gradientn(
    colors = c("#fff0f6", "#ffb3c6", "#ff4d6d", "#a10036"),
    limits = c(0, max(shape$Pval_TPAK, na.rm = TRUE))
  ) +
  labs(title = "Peta Keragaman Spasial P-value TPAK", fill = "P-value") +
  theme_minimal()

# Peta Koefisien
ggplot(shape) +
  geom_sf(aes(fill = Beta_TPAK), color = "white") +
  scale_fill_gradientn(colors = c("#f7fcf5", "#bae4b3", "#74c476", "#238b45"),
                       limits = c(min(shape$Beta_TPAK, na.rm = TRUE), 
                                  max(shape$Beta_TPAK, na.rm = TRUE))) +
  labs(title = "Peta Keragaman Spasial Koefisien TPAK", fill = "Koefisien") +
  theme_minimal()


# --- UMKM ---
ggplot(shape) +
  geom_sf(aes(fill = Pval_UMKM), color = "white") +
  scale_fill_gradientn(
    colors = c("#fff0f6", "#ffb3c6", "#ff4d6d", "#a10036"),
    limits = c(0, max(shape$Pval_UMKM, na.rm = TRUE))
  ) +
  labs(title = "Peta Keragaman Spasial P-value UMKM", fill = "P-value") +
  theme_minimal()

ggplot(shape) +
  geom_sf(aes(fill = Beta_UMKM), color = "white") +
  scale_fill_gradientn(colors = c("#f7fcf5", "#bae4b3", "#74c476", "#238b45"),
                       limits = c(min(shape$Beta_UMKM, na.rm = TRUE), 
                                  max(shape$Beta_UMKM, na.rm = TRUE))) +
  labs(title = "Peta Keragaman Spasial Koefisien UMKM", fill = "Koefisien") +
  theme_minimal()


# --- Jarak ---
ggplot(shape) +
  geom_sf(aes(fill = Pval_Jarak), color = "white") +
  scale_fill_gradientn(
    colors = c("#fff0f6", "#ffb3c6", "#ff4d6d", "#a10036"),
    limits = c(0, max(shape$Pval_Jarak, na.rm = TRUE))
  ) +
  labs(title = "Peta Keragaman Spasial P-value Jarak", fill = "P-value") +
  theme_minimal()

ggplot(shape) +
  geom_sf(aes(fill = Beta_Jarak), color = "white") +
  scale_fill_gradientn(colors = c("#f7fcf5", "#bae4b3", "#74c476", "#238b45"),
                       limits = c(min(shape$Beta_Jarak, na.rm = TRUE), 
                                  max(shape$Beta_Jarak, na.rm = TRUE))) +
  labs(title = "Peta Keragaman Spasial Koefisien Jarak", fill = "Koefisien") +
  theme_minimal()


# --- RSU ---
ggplot(shape) +
  geom_sf(aes(fill = Pval_RSU), color = "white") +
  scale_fill_gradientn(
    colors = c("#fff0f6", "#ffb3c6", "#ff4d6d", "#a10036"),
    limits = c(0, max(shape$Pval_RSU, na.rm = TRUE))
  ) +
  labs(title = "Peta Keragaman Spasial P-value RSU", fill = "P-value") +
  theme_minimal()

ggplot(shape) +
  geom_sf(aes(fill = Beta_RSU), color = "white") +
  scale_fill_gradientn(colors = c("#f7fcf5", "#bae4b3", "#74c476", "#238b45"),
                       limits = c(min(shape$Beta_RSU, na.rm = TRUE), 
                                  max(shape$Beta_RSU, na.rm = TRUE))) +
  labs(title = "Peta Keragaman Spasial Koefisien RSU", fill = "Koefisien") +
  theme_minimal()

###################### ------- PETA RESIDUAL ------- ######################
shape$Residual_Gaussian <- gwr_gauss$SDF$residual
shape$Residual_Bisquare <- gwr_bisq$SDF$residual
shape$Residual_Tricube  <- gwr_tri$SDF$residual
shape$Residual_OLS <- residuals(model_ols)

residual_summary <- data.frame(
  Model = c("Regresi Linear Global", "GWR Gaussian", "GWR Bisquare", "GWR Tricube"),
  Min = c(
    min(abs(shape$Residual_OLS), na.rm = TRUE),
    min(abs(shape$Residual_Gaussian), na.rm = TRUE),
    min(abs(shape$Residual_Bisquare), na.rm = TRUE),
    min(abs(shape$Residual_Tricube), na.rm = TRUE)
  ),
  Max = c(
    max(abs(shape$Residual_OLS), na.rm = TRUE),
    max(abs(shape$Residual_Gaussian), na.rm = TRUE),
    max(abs(shape$Residual_Bisquare), na.rm = TRUE),
    max(abs(shape$Residual_Tricube), na.rm = TRUE)
  )
)

print(residual_summary)

# --- Peta Residual OLS ---
ggplot(shape) +
  geom_sf(aes(fill = Residual_OLS), color = "white", size = 0.4) +
  scale_fill_gradient2(
    low = "#fdbb84", mid = "white", high = "#807dba", midpoint = 0
  ) +
  labs(title = "Peta Persebaran Residual Regresi Linear Global", fill = "Residual") +
  theme_minimal()

# --- Peta Residual GWR Gaussian ---
ggplot(shape) +
  geom_sf(aes(fill = Residual_Gaussian), color = "white", size = 0.4) +
  scale_fill_gradient2(
    low = "#fdbb84", mid = "white", high = "#807dba", midpoint = 0
  ) +
  labs(title = "Peta Persebaran Residual GWR Gaussian", fill = "Residual") +
  theme_minimal()

# --- Peta Residual GWR Bisquare ---
ggplot(shape) +
  geom_sf(aes(fill = Residual_Bisquare), color = "white", size = 0.4) +
  scale_fill_gradient2(
    low = "#fdbb84", mid = "white", high = "#807dba", midpoint = 0
  ) +
  labs(title = "Peta Persebaran Residual GWR Bisquare", fill = "Residual") +
  theme_minimal()

# --- Peta Residual GWR Tricube ---
ggplot(shape) +
  geom_sf(aes(fill = Residual_Tricube), color = "white", size = 0.4) +
  scale_fill_gradient2(
    low = "#fdbb84", mid = "white", high = "#807dba", midpoint = 0
  ) +
  labs(title = "Peta Persebaran Residual GWR Tricube", fill = "Residual") +
  theme_minimal()

#---------------------------------------------------------------------------------------------------------------
