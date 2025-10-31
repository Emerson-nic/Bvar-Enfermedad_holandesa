# instalar
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  urca, tseries, dplyr, ggplot2, patchwork, vars, BVAR, 
  parallel, magrittr, coda, forecast, gridExtra,
  MASS, grid, lubridate, reshape2, FinTS, gt, scales, ggforce,
  nortest, rugarch,DescTools, ggrepel, ggthemes, bsvars, tidyr,
  zoo,xtable,readxl,openxlsx
)

#exportar

data <- read_excel(choose.files())

# preparacion de datos 

series <- c("itcer","ipi_eeuu", "iti", "transable", "remesas", 
            "no_transable")

endogenas_originales <- data[, series]
verificar_estacionariedad <- function(data, nombre_col, nombre_etiq, diff = 1) {
  #diferenciar
  if (diff == 1) {
    serie_diff <- na.omit(diff(data[[nombre_col]]))
  } else if (diff == 2) {
    serie_diff <- na.omit(diff(diff(data[[nombre_col]])))
  } else {
    stop("Solo se admiten diff=1 o diff=2")
  }
  
  cat("\n----------------------------------------\n")
  cat("Pruebas de estacionariedad para:", nombre_etiq, "\n")
  cat("----------------------------------------\n\n")
  
  #test ADF
  adf <- tseries::adf.test(serie_diff)
  cat("ADF Test:\n")
  cat("- p-valor =", round(adf$p.value, 4), "\n")
  
  #test PP
  pp <- urca::ur.pp(serie_diff, type = "Z-tau", model = "trend", lags = "long")
  cat("\nPP Test:\n")
  cat("- Estadístico =", round(pp@teststat, 4), "\n")
  cat("- Valor crítico 5% =", pp@cval[2], "\n")
  
  #test ZA
  za <- urca::ur.za(serie_diff, model = "both")
  cat("\nZA Test (con quiebre estructural):\n")
  cat("- Estadístico =", round(za@teststat, 4), "\n")
  cat("- Valor crítico 5% =", za@cval[2], "\n")
  cat("- Punto de quiebre =", za@bpoint, "\n")
  
  #test KPSS
  kpss <- urca::ur.kpss(serie_diff, type = "tau", lags = "long")
  cat("\nKPSS Test:\n")
  cat("- Estadístico =", round(kpss@teststat, 4), "\n")
  cat("- Valor crítico 5% =", kpss@cval[2], "\n")
  
  #conclision final
  cat("\nCONCLUSIÓN FINAL: ")
  if (adf$p.value < 0.05 && pp@teststat < pp@cval[2] &&
      za@teststat < za@cval[2] && kpss@teststat < kpss@cval[2]) {
    cat("SERIE ESTACIONARIA\n")
  } else {
    cat("PROBLEMAS DE ESTACIONARIEDAD\n")
  }
  
  #retorna los resultados
  return(list(
    adf = adf$p.value,
    pp = c(stat = pp@teststat, critical = pp@cval[2]),
    za = c(stat = za@teststat, critical = za@cval[2], breakpoint = za@bpoint),
    kpss = c(stat = kpss@teststat, critical = kpss@cval[2])
  ))
}

#lista de resultados
resultados <- list()

#aplicar 1ra diferencias a algunas series 
resultados$d2_itcer <- verificar_estacionariedad(
  data = endogenas_originales, 
  nombre_col = "itcer", 
  nombre_etiq = "Primera diferencia de ITCER",
  diff = 1
)

resultados$d2_iti <- verificar_estacionariedad(
  data = endogenas_originales, 
  nombre_col = "iti", 
  nombre_etiq = "Primera diferencia de ITI",
  diff = 1
)

resultados$d2_no_transable <- verificar_estacionariedad(
  data = endogenas_originales, 
  nombre_col = "no_transable", 
  nombre_etiq = "Primera diferencia de Sector No Transable",
  diff = 1
)

resultados$d2_transable <- verificar_estacionariedad(
  data = endogenas_originales,
  nombre_col = "transable",
  nombre_etiq = "Primera diferencia de Sector Transable",
  diff = 1
)

resultados$d1_ipi_eeuu <- verificar_estacionariedad(
  data = endogenas_originales,
  nombre_col = "ipi_eeuu",
  nombre_etiq = "Primera diferencia de IPI EEUU",
  diff = 1
)

resultados$d1_remesas <- verificar_estacionariedad(
  data = endogenas_originales,
  nombre_col = "remesas",
  nombre_etiq = "Primera diferencia de Remesas",
  diff = 1
)


#dataset
endogenas_transformadas <- data.frame(
  d1_itcer = c(NA, diff(data$itcer)),
  d1_iti = c(NA, diff(data$iti)),
  d1_no_transable = c(NA, diff(data$no_transable)),
  d1_transable = c(NA,diff(data$transable)),
  
  d1_ipi_eeuu = c(NA, diff(data$ipi_eeuu)),
  d1_remesas = c(NA, diff(data$remesas))
)

#dummys
data <- data %>% 
  mutate(
    dummy_2008 = ifelse(año == 2008 & mes %in% c("III", "IV"), 1, 0),
    dummy_2018 = ifelse(año == 2018 & mes %in% c("II", "III", "IV"), 1, 0),
    dummy_2020 = ifelse(año == 2020, 1, 0)
  )

# combinar al dataset
data_final <- data %>%
  dplyr::select(
    año, 
    mes,
    dummy_2008,
    dummy_2018,
    dummy_2020
  ) %>%
  bind_cols(endogenas_transformadas) %>%
  na.omit() 

# Crear fecha trimestral
data_final <- data_final %>%
  mutate(
    quarter_start = case_when(
      mes == "I" ~ "01-01",
      mes == "II" ~ "04-01",
      mes == "III" ~ "07-01",
      mes == "IV" ~ "10-01"
    ),
    date = as.Date(paste(año, quarter_start, sep = "-"))
  ) %>%
  dplyr::select(-quarter_start)

# Verificar estructura
cat("\n\n========================================")
cat("\nESTRUCTURA DEL DATASET FINAL")
cat("\n========================================\n")
str(data_final)
head(data_final)

#graficos
plot_series_professional <- function(data, var_name, title) {
  if (!"date" %in% names(data)) {
    stop("El dataframe no contiene la columna 'date'")
  }
  
  #limites del eje Y
  serie_data <- data[[var_name]]
  y_min <- min(serie_data, na.rm = TRUE)
  y_max <- max(serie_data, na.rm = TRUE)
  
  ggplot(data, aes(x = date, y = .data[[var_name]])) +
    geom_line(color = "#3498DB", linewidth = 1.2, alpha = 0.9) +
    
    #eventos  dummys
    geom_vline(xintercept = min(data$date[data$dummy_2008 == 1], na.rm = TRUE), 
               color = "#F1C40F", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    geom_vline(xintercept = min(data$date[data$dummy_2018 == 1], na.rm = TRUE), 
               color = "#27AE60", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    geom_vline(xintercept = min(data$date[data$dummy_2020 == 1], na.rm = TRUE), 
               color = "#8E44AD", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    
    #anotaciones dummys
    annotate("text", x = min(data$date[data$dummy_2008 == 1], na.rm = TRUE), 
             y = y_min + 0.05*(y_max - y_min),
             label = "Crisis Financiera (2008)", color = "#F1C40F",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    annotate("text", x = min(data$date[data$dummy_2018 == 1], na.rm = TRUE), 
             y = y_min + 0.05*(y_max - y_min),
             label = "Crisis Política (2018)", color = "#27AE60",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    annotate("text", x = min(data$date[data$dummy_2020 == 1], na.rm = TRUE), 
             y = y_min + 0.05*(y_max - y_min),
             label = "Pandemia COVID-19", color = "#8E44AD",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.03)) +
    
    labs(
      title    = title,
      subtitle = "Serie transformada con eventos destacados",
      x        = NULL,
      y        = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle    = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption     = element_text(face = "italic", size = 7, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line.x      = element_line(color = "black"),
      axis.text        = element_text(size = 10),
      axis.title.y     = element_text(margin = margin(r = 8))
    )
}

# graficos aplicados
p1 <- plot_series_professional(data_final, "d1_itcer", "Primera diferencia de ITCER")
p2 <- plot_series_professional(data_final, "d1_iti", "Primera diferencia de Términos de Intercambio")
p3 <- plot_series_professional(data_final, "d1_no_transable", "Primera diferencia de Sector No Transable")
p4 <- plot_series_professional(data_final, "d1_transable", "Primera diferencia del Sector Transable")
p5 <- plot_series_professional(data_final, "d1_ipi_eeuu", "Primera diferencia de IPI EEUU")
p6 <- plot_series_professional(data_final, "d1_remesas", "Primera diferencia de Remesas")

#combinar graficos
combined_plot_1 <- (p1 | p2) +
  plot_annotation(
    title    = "Series Transformadas para Modelo BVAR",
    subtitle = "Diferenciadas según requerimiento de estacionariedad",
    caption  = "Nota: Series estacionarizadas mediante diferencias\nFuente: Elaboración propia con datos del BCN, SEMCA & FRED",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      plot.caption  = element_text(face = "italic", size = 12, hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

combined_plot_2 <-(p4 | p5 | p6)  +
  plot_annotation(
    title    = "Series Transformadas para Modelo BVAR",
    subtitle = "Diferenciadas según requerimiento de estacionariedad",
    caption  = "Nota: Series estacionarizadas mediante diferencias\nFuente: Elaboración propia con datos del BCN, SEMCA & FRED",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      plot.caption  = element_text(face = "italic", size = 12, hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

print(combined_plot_1)
print(combined_plot_2)

#na omit
endogenas_transformadas <- na.omit(endogenas_transformadas)

data_final <- data.frame(
  d1_itcer = c(NA,(diff(data$itcer))),
  d1_iti = c(NA,(diff(data$iti))),
  d1_no_transable = c(NA,(diff(data$no_transable))),
  d1_transable = c(NA, (diff(data$transable))),
  d1_ipi_eeuu = c(NA, diff(data$ipi_eeuu)),
  d1_remesas = c(NA, diff(data$remesas)),
  dummy_2008 = data$dummy_2008,
  dummy_2018 = data$dummy_2018,
  dummy_2020 = data$dummy_2020
)

data_final <- na.omit(data_final)

# orden de lags
endogenas <- data_final[, c(
  "d1_itcer", "d1_iti", "d1_transable", "d1_ipi_eeuu", "d1_remesas"   
)]

exogenas <- data_final[, c("dummy_2018", "dummy_2020", "dummy_2008")]

# seleccion de rezago
lag_selection <- vars::VARselect( 
  y = endogenas,
  exogen = exogenas,
  lag.max = 7,
  type = "const"
)

cat("Resultados de selección de rezagos:\n")
print(lag_selection$selection)
cat("\nMatriz de criterios:\n")
print(lag_selection$criteria)

# Prior

#orden de la enfermedad

endogenas <- as.matrix(data_final[, c(
  "d1_ipi_eeuu",
  "d1_remesas",
  "d1_iti",
  "d1_itcer",  
  "d1_transable"
)])

exogenas <- as.matrix(data_final[, c(
  "dummy_2018", "dummy_2020", "dummy_2008"
)])

# configuracion de priors 
priors_auto <- BVAR::bv_priors(
  hyper = "auto"
)

# modelo BVAR 
set.seed(123)
model_bvar_final <- BVAR::bvar(
  data = endogenas,
  lags = 7,
  exogen = exogenas,
  priors = priors_auto,
  n_draw = 300000,
  n_burn = 50000,
  n_thin = 100, 
  mh = BVAR::bv_mh(scale_hess = 1, adjust_acc = TRUE)
)

summary(model_bvar_final)

# nuevos hiperparametros 
lambda_hat <- mean(model_bvar_final$hyper[, "lambda"], na.rm = TRUE)

alpha_hat <- model_bvar_final$priors$alpha$mode

psi_hat <- if (!is.null(model_bvar_final$priors$psi$mode)) {
  model_bvar_final$priors$psi$mode
} else if (!is.null(model_bvar_final$optim$psi)) {
  model_bvar_final$optim$psi
} else {
  NA
}

hyper_opt <- list(
  lambda = lambda_hat,
  alpha = alpha_hat,
  psi = psi_hat
)

cat("\n=== Hiperparametros ===\n")
cat("- Lambda       :", round(hyper_opt$lambda, 4), "\n")
cat("- Alpha        :", round(hyper_opt$alpha, 4), "\n")
cat("- Psi          :", 
    ifelse(is.na(hyper_opt$psi), "No disponible", 
           round(hyper_opt$psi, 4)), "\n")

#Orden psi
var_names <- c(
  "d1_ipi_eeuu",
  "d1_remesas",
  "d1_iti",
  "d1_itcer",
  "d1_transable"
)

psi_modes <- hyper_opt$psi

names(psi_modes) <- var_names
print(psi_modes)

cat("Estructura del objeto BVAR:\n")
print(names(model_bvar_final))

# diagnostico de convergencia 
if (requireNamespace("coda", quietly = TRUE)) {
  cat("\n=== DIAGNÓSTICO DE CONVERGENCIA ===\n")
}

if (is.matrix(model_bvar_final$hyper)) {
  lambda_chain <- model_bvar_final$hyper[, "lambda"]
} else {
  lambda_chain <- model_bvar_final$hyper$lambda
}

# Convertir a vector numérico simple
lambda_chain <- as.numeric(lambda_chain)

cat("Lambda:\n")
cat("- Iteraciones totales :", length(lambda_chain), "\n")

#tamaño efectivo
ess_lambda <- coda::effectiveSize(lambda_chain)
cat("- Tamaño efectivo     :", round(ess_lambda, 1), "\n")

# autocorrelación 
if (length(lambda_chain) > 1) {
  acf_result <- acf(lambda_chain, lag.max = 1, plot = FALSE)
  ac1_lambda <- acf_result$acf[2]
  cat("- Autocorrelación lag1:", round(ac1_lambda, 3), "\n")
} else {
  ac1_lambda <- NA
  cat("- Autocorrelación lag1: No calculable (cadena muy corta)\n")
}

# test de Geweke para convergencia 
if (length(lambda_chain) > 100) {
  geweke_test <- coda::geweke.diag(lambda_chain)
  geweke_z <- round(geweke_test$z, 3)
  p_value <- round(2 * pnorm(-abs(geweke_z)), 4)
  cat("- Test de Geweke (z)  :", geweke_z, "\n")
  cat("- p-value Geweke      :", p_value, "\n")
} else {
  cat("- Test de Geweke: No aplicable (muestras insuficientes)\n")
}

# calcular ACF 
acf_all <- acf(lambda_chain, lag.max = 18, plot = FALSE)$acf

# extraer los valores
acf1 <- round(acf_all[2], 3)  
acf2 <- round(acf_all[3], 3)
acf3 <- round(acf_all[4], 3)  
acf4 <- round(acf_all[5], 3)
acf5 <- round(acf_all[6], 3)
acf6 <- round(acf_all[7], 3)
acf7 <- round(acf_all[8], 3)

cat("ACF lag1:", acf1, "\n")
cat("ACF lag2:", acf2, "\n")
cat("ACF lag3:", acf3, "\n")
cat("ACF lag4:", acf4, "\n")
cat("ACF lag5:", acf5, "\n")
cat("ACF lag6:", acf6, "\n")
cat("ACF lag7:", acf7, "\n")

# tamaño efectivo de la cadena
ess_lambda <- as.numeric(effectiveSize(lambda_chain))

calc_z_p <- function(rho, ess) {
  z  <- rho * sqrt(ess)
  p  <- 2 * (1 - pnorm(abs(z)))
  list(z = z, p = p)
}

res2 <- calc_z_p(acf2, ess_lambda)
res3 <- calc_z_p(acf3, ess_lambda)
res4 <- calc_z_p(acf4, ess_lambda)
res6 <- calc_z_p(acf6, ess_lambda)
res7 <- calc_z_p(acf7, ess_lambda)

cat("ESS chain λ:", round(ess_lambda, 1),   "\n\n")

cat("Lag 2:\n")
cat("  ACF       =", acf2,                  "\n")
cat("  z         =", round(res2$z,  3),     "\n")
cat("  p-value   =", round(res2$p,  4),     "\n\n")

cat("Lag 3:\n")
cat("  ACF       =", acf3,                  "\n")
cat("  z         =", round(res2$z,  3),     "\n")
cat("  p-value   =", round(res2$p,  4),     "\n\n")

cat("Lag 4:\n")
cat("  ACF       =", acf4,                  "\n")
cat("  z         =", round(res4$z,  3),     "\n")
cat("  p-value   =", round(res4$p,  4),     "\n\n")

cat("Lag 6:\n")
cat("  ACF       =", acf6,                  "\n")
cat("  z         =", round(res4$z,  3),     "\n")
cat("  p-value   =", round(res4$p,  4),     "\n\n")

cat("Lag 7:\n")
cat("  ACF       =", acf7,                  "\n")
cat("  z         =", round(res7$z,  3),     "\n")
cat("  p-value   =", round(res7$p,  4),     "\n")

C <- companion(model_bvar_final, type = "mean")
ev <- eigen(C)$values
df <- data.frame(
  Re = Re(ev),
  Im = Im(ev),
  Sign = ifelse(Re(ev) < 0, "Negativa", "No negativa")
)

# grafica circular de raices
grafico_raices <- ggplot(df, aes(x = Re, y = Im, color = Sign)) +
  # Círculo unidad punteado (cambio size → linewidth)
  geom_circle(aes(x0 = 0, y0 = 0, r = 1),
              color     = "grey70",
              linetype  = "dashed",
              linewidth  = 0.7) +
  # Puntos de eigenvalores
  geom_point(size = 6, alpha = 0.9) +
  # Paleta sobria: rojo para negativas, azul para positivas/ceros
  scale_color_manual(values = c(
    "Negativa" = "#E74C3C",
    "No negativa" = "#3498DB"   
  )) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(
    title    = "Raíces del Polinomio Característico (BVAR)",
    subtitle = "Eigenvalores de la matriz companion",
    caption  = "Fuente: Elaboración propia con resultados del modelo BVAR",
    x        = "Parte Real",
    y        = "Parte Imaginaria",
    color    = "Signo de la Parte Real"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title       = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle    = element_text(size = 16, color = "grey40", hjust = 0.5),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

grafico_raices

# grafico de convergencia

plot_convergencia_profesional <- function(chain, nombre, window = 0.1) {
  n_iter    <- length(chain)
  mean_val  <- mean(chain)
  ess       <- coda::effectiveSize(chain)
  acf1      <- acf(chain, plot = FALSE)$acf[2]
  
  df <- data.frame(
    iter = 1:n_iter,
    value = chain,
    rolling_mean = zoo::rollmean(chain, 
                                 k = max(2, floor(n_iter * window)), 
                                 fill = NA)
  )
  
  p_trace <- ggplot(df, aes(x = iter, y = value)) +
    geom_line(color = "#3498DB", alpha = 0.7) +
    geom_line(
      aes(y = rolling_mean),
      color     = "#E74C3C",
      linewidth = 1.2,
      na.rm     = TRUE     
    ) +
    geom_hline(yintercept = mean_val, 
               color = "#2C3E50", 
               linetype = "dashed", 
               linewidth = 1) +
    labs(
      title = paste("Trazado de la Cadena MCMC:", nombre),
      subtitle = paste("ESS =", round(ess, 1), "| ACF(lag7) =", round(acf7, 3)),
      x = "Iteración",
      y = "Valor"
    ) +
    scale_x_continuous(labels = scales::comma) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#2C3E50"),
      plot.subtitle = element_text(size = 11, color = "#7F8C8D"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA)
    )
  
  p_hist <- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)), 
                   fill = "#3498DB", alpha = 0.7, bins = 30) +
    geom_density(color = "#E74C3C", linewidth = 1.2) +
    geom_vline(xintercept = mean_val, 
               color = "#2C3E50", 
               linetype = "dashed", 
               linewidth = 1) +
    labs(title = "Distribución Posterior", x = "Valor", y = "Densidad") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#2C3E50"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    )
  
  lag_max <- min(8, n_iter - 1)
  acf_res <- acf(chain, plot = FALSE, lag.max = lag_max)$acf
  acf_data <- data.frame(lag = 0:lag_max, acf = as.numeric(acf_res))
  
  p_acf <- ggplot(acf_data, aes(x = lag, y = acf)) +
    geom_segment(aes(xend = lag, yend = 0), color = "#3498DB", linewidth = 1.2) +
    geom_hline(yintercept = 0, color = "#2C3E50") +
    geom_hline(yintercept = c(-1, 1) * 1.96 / sqrt(n_iter), 
               color = "#E74C3C", linetype = "dashed") +
    labs(title = "Función de Autocorrelación", x = "Rezago", y = "ACF") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#2C3E50"),
      panel.grid.minor = element_blank()
    )
  
  # Combinar graficos
  combined <- (p_trace) / (p_hist | p_acf) +
    plot_annotation(
      title    = paste("Diagnóstico de Convergencia:", nombre),
      subtitle = "Análisis de cadena MCMC",
      caption  = "Fuente: BCN, SECMCA & FRED | Elaboración propia estimada con modelo BVAR",
      theme    = theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2C3E50"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#7F8C8D"),
        plot.caption = element_text(face = "italic", size = 10, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )
  
  return(combined)
}

if (exists("lambda_chain")) {
  convergence_plot <- plot_convergencia_profesional(lambda_chain, "Lambda")
} else {
  message("La cadena 'lambda_chain' no existe. Generando ejemplo.")
  set.seed(123)
  ejemplo_chain <- arima.sim(model = list(ar = 0.8), n = 1000) + 0.5
  convergence_plot <- plot_convergencia_profesional(ejemplo_chain, "Ejemplo")
}

print(convergence_plot)


# impulsos respuesta

# configuracion IRF
n_horizon  <- 12
conf_bands <- c(0.025, 0.5, 0.975)


# calcular irfs
irf_all <- BVAR::irf(
  x = model_bvar_final,
  horizon = n_horizon,
  conf_bands = conf_bands
)

# extraer irfs en contra enfermedad holandesa
get_irf_data <- function(irf_obj, impulse, response) {
  vars    <- irf_obj$variables
  i_imp  <- match(impulse, vars)
  i_resp <- match(response, vars)
  q      <- irf_obj$quants
  
  data.frame(
    periodo    = 0:(dim(q)[3] - 1),
    inferior  = q[1, i_imp, , i_resp],
    mediana   = q[2, i_imp, , i_resp],
    superior  = q[3, i_imp, , i_resp],
    impulso   = impulse,
    respuesta = response,
    row.names = NULL
  )
}

plot_irf <- function(df) {
  
  labels <- c(
    d1_remesas = "Remesas",
    d1_itcer = "Tipo de Cambio Real",
    d1_ipi_eeuu = "IPI Estados Unidos",
    d1_iti = "ITI",
    d1_transable = "Sector Transable"
  )
  
  color_palette <- list(
    "d1_remesas" = "#3498DB",
    "d1_itcer" = "#2ECC71",
    "d1_transable" = "#FF5733", 
    "d1_ipi_eeuu" = "#9B59B6",
    "d1_iti" = "#F39C12"
  )
  
  color_fill <- color_palette[[df$impulso[1]]]
  pt <- df[which.max(abs(df$mediana)), ]
  
  ggplot(df, aes(x = periodo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_ribbon(aes(ymin = inferior, ymax = superior),
                fill = color_fill, alpha = 0.15) +
    geom_line(aes(y = mediana), color = color_fill, linewidth = 1.2) +
    geom_point(data = pt, aes(y = mediana), color = "#E74C3C", size = 3) +
    geom_text(data = pt,
              aes(x = periodo, y = mediana * 1.1,
                  label = paste0(periodo, "T: ", round(mediana, 3))),
              color = "#E74C3C", fontface = "bold", size = 3.5,
              vjust = ifelse(pt$mediana > 0, -0.5, 1.2)) +
    labs(
      title = paste("Shock en", labels[df$impulso[1]]),
      subtitle = paste("Respuesta de", labels[df$respuesta[1]]),
      x = "Trimestre después del shock",
      y = "Mediana IRF",
      caption = paste0("Intervalo de credibilidad al ", 95, "%")
    ) +
    scale_x_continuous(breaks = seq(0, n_horizon, by = 1)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray90", fill = NA, linewidth = 0.5),
      plot.caption =  element_text(size = 14, hjust = 0.5)
    )
}

# 1er plot

irf_list_11 <- list(
  get_irf_data(irf_all, "d1_remesas", "d1_itcer"),
  get_irf_data(irf_all, "d1_itcer", "d1_transable")
)

plots_11 <- lapply(irf_list_11, plot_irf)

combined_11 <- (plots_11[[1]] | plots_11[[2]] ) +
  plot_annotation(
    title  = "Efecto de las Variables",
    subtitle = "Análisis de Impulso-Respuesta",
    caption  = paste0("Horizonte: ", n_horizon, " Trimestres | Periodo: 2006-2023\n",
                      "Fuente: BCN, SEMCA & FRED| Elaboración propia estimada con modelo BVAR"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )


# 2do plot

irf_list_22 <- list(
  get_irf_data(irf_all, "d1_ipi_eeuu", "d1_itcer"),
  get_irf_data(irf_all, "d1_iti", "d1_itcer")
)

plots_22 <- lapply(irf_list_22, plot_irf)

combined_22 <- (plots_22[[1]] | plots_22[[2]]) +
  
  plot_annotation(
    title    = "Efecto de las Variables",
    subtitle = "Análisis de Impulso-Respuesta",
    caption  = paste0("Horizonte: ", n_horizon, " Trimestres | Periodo: 2006-2023\n",
                      "Fuente: BCN, SEMCA & FRED| Elaboración propia estimada con modelo BVAR"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

# 3er plot

irf_list_33 <- list(
  get_irf_data(irf_all, "d1_remesas", "d1_transable")
)

plots_33 <- lapply(irf_list_33, plot_irf)

combined_33 <- (plots_33[[1]]) +
  
  plot_annotation(
    title    = "Efecto de las Variables",
    subtitle = "Análisis de Impulso-Respuesta",
    caption  = paste0("Horizonte: ", n_horizon, " Trimestres | Periodo: 2006-2023\n",
                      "Fuente: BCN, SEMCA & FRED| Elaboración propia estimada con modelo BVAR"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )


print(combined_11)
print(combined_22)
print(combined_33)

#exportar a excel para mejor visivilidad

#lista usada 
irf_combinados_transable <- list(
  get_irf_data(irf_all, "d1_remesas", "d1_itcer"),
  get_irf_data(irf_all, "d1_itcer", "d1_transable"),
  get_irf_data(irf_all, "d1_ipi_eeuu", "d1_itcer"),
  get_irf_data(irf_all, "d1_iti", "d1_itcer"),
  get_irf_data(irf_all, "d1_remesas", "d1_transable")
)

#lista combinada
todos_los_irfs_transable <- do.call(rbind, irf_combinados_transable)

#excel
hoja_irf <- list(
  "Irfs_Total" = todos_los_irfs_transable,           #todos los irfs
  "Remesas_ITCER" = todos_los_irfs_transable[1],  #separados
  "ITCER_Transable" = todos_los_irfs_transable[5]
)

#exportarlo
write.xlsx(hoja_irf, file = "irf_total(modelo transable).xlsx")


#para obtener el horizonte de los irf quite el # del siguiente scritp

irf_total_list <- c(irf_list_11, irf_list_22, irf_list_33)
irf_horizonte_df <- bind_rows(irf_total_list)
print(irf_horizonte_df)
#View(irf_horizonte_df)
#print(irf_horizonte_df[167:252, ])

# irfs acumulados

# orden en contra de la enfermedad holandesas
get_irf_acumulada <- function(irf_obj, impulse, response) {
  vars <- irf_obj$variables
  i_imp <- match(impulse, vars)
  i_resp <- match(response, vars)
  q <- irf_obj$quants
  
  acumulado <- array(NA, dim = dim(q))
  for(i in 1:dim(q)[1]) {
    for(j in 1:dim(q)[2]) {
      acumulado[i,j,,] <- apply(q[i,j,,], 2, cumsum)
    }
  }
  
  data.frame(
    periodo = 0:(dim(acumulado)[3]-1),
    inferior = acumulado[1, i_imp, , i_resp],
    mediana = acumulado[2, i_imp, , i_resp],
    superior = acumulado[3, i_imp, , i_resp],
    impulso = impulse,
    respuesta = response,
    row.names = NULL
  )
}

irf_list_acum <- list(
  get_irf_acumulada(irf_all, "d1_remesas", "d1_itcer"),
  get_irf_acumulada(irf_all, "d1_ipi_eeuu", "d1_itcer"),
  get_irf_acumulada(irf_all, "d1_itcer", "d1_transable"),
  get_irf_acumulada(irf_all, "d1_iti", "d1_itcer")
)

plot_irf_acum11 <- function(df) {
  labels <- c(
    d1_remesas = "Remesas",
    d1_itcer = "Tipo de Cambio Real",
    d1_ipi_eeuu = "IPI Estados Unidos",
    d1_iti = "ITI",
    d1_transable = "Sector Transable"
  )
  
  
  color_palette <- list(
    "d1_remesas" = "#3498DB",
    "d1_itcer" = "#2ECC71",
    "d1_transable" = "#FF5733", 
    "d1_ipi_eeuu" = "#9B59B6",
    "d1_iti" = "#F39C12"
  )
  
  color_fill <- color_palette[[df$impulso[1]]]
  
  pt <- df[which.max(abs(df$mediana)), ]
  
  y_range <- range(df$inferior, df$superior, na.rm = TRUE)
  y_span <- diff(y_range)
  label_offset <- ifelse(
    pt$mediana > 0,
    min(pt$mediana * 1.05, max(y_range) - 0.05 * y_span),
    max(pt$mediana * 1.05, min(y_range) + 0.05 * y_span)
  )
  
  label_x <- pt$periodo
  label_hjust <- 0.5
  if(pt$periodo > (n_horizon * 0.8)) {
    label_x <- pt$periodo - 4
    label_hjust <- 1
  }
  
  color_fill <- color_palette[[df$impulso[1]]]
  pt <- df[which.max(abs(df$mediana)), ]
  
  color_fill <- color_palette[[df$impulso[1]]]
  pt <- df[which.max(abs(df$mediana)), ]
  
  color_fill <- color_palette[[df$impulso[1]]]
  pt <- df[which.max(abs(df$mediana)), ]
  
  ggplot(df, aes(x = periodo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_ribbon(aes(ymin = inferior, ymax = superior),
                fill = color_fill, alpha = 0.15) +
    geom_line(aes(y = mediana), color = color_fill, linewidth = 1.2) +
    geom_point(data = pt, aes(y = mediana), color = "#E74C3C", size = 3) +
    geom_text(data = pt,
              aes(x = periodo, y = mediana * 1.1,
                  label = paste0(periodo, "T: ", round(mediana, 3))),
              color = "#E74C3C", fontface = "bold", size = 3.5,
              vjust = ifelse(pt$mediana > 0, -0.5, 1.2)) +
    labs(
      title = paste("Shock Acumunlado en", labels[df$impulso[1]]),
      subtitle = paste("Respuesta de", labels[df$respuesta[1]]),
      x = "Trimestre después del shock",
      y = "Mediana IRF",
      caption = paste0("Intervalo de credibilidad al ", 95, "%")
    ) +
    scale_x_continuous(breaks = seq(0, n_horizon, by = 1)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray90", fill = NA, linewidth = 0.5),
      plot.caption =  element_text(size = 14, hjust = 0.5)
    ) + 
    coord_cartesian(clip = "off")
}

# 1er plot
plots_acum11 <- lapply(irf_list_acum, function(df) {
  tryCatch(plot_irf_acum11(df), error = function(e) ggplot() + labs(title = "Error en gráfico"))
})

combined_acum_11 <- (plots_acum11[[1]] | plots_acum11[[2]] ) +
  plot_annotation(
    title = "Análisis de Impulso-Respuesta Acumulado (Parte 1)",
    subtitle = "Efectos Acumulados de los Shocks de Bonanza y Producción",
    caption = paste0("Horizonte: ", n_horizon, " Trimestres | Periodo: 2006-2023\n",
                     "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

# 2do plot
combined_acum_22 <- (plots_acum11[[3]] | plots_acum11[[4]]) +
  plot_annotation(
    title = "Análisis de Impulso-Respuesta Acumulado (Parte 2)",
    subtitle = "Efectos Acumulados de la Enfermedad Holandesa y Controles Externos",
    caption = paste0("Horizonte: ", n_horizon, " Trimestres | Periodo: 2006-2023\n",
                     "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

print(combined_acum_11)
print(combined_acum_22)

#lista usada 
irf_list_acum_transable <- list(
  get_irf_acumulada(irf_all, "d1_remesas", "d1_itcer"),
  get_irf_acumulada(irf_all, "d1_ipi_eeuu", "d1_itcer"),
  get_irf_acumulada(irf_all, "d1_itcer", "d1_transable"),
  get_irf_acumulada(irf_all, "d1_iti", "d1_itcer")
)

#lista combinada
todos_los_irfs_acum_transable <- do.call(rbind, irf_list_acum_transable)

#excel
hoja_irf_acum_transable <- list(
  "Irfs_Total_acum_transable" = todos_los_irfs_acum_transable  #todos los irfs
)

#exportarlo
write.xlsx(hoja_irf_acum_transable, file = "irf_total_acum_transable.xlsx")

# diagnostico de resuduos bvar 

# extraer los residuos
resids_mat <- residuals(model_bvar_final)  
resids_df  <- as.data.frame(resids_mat)

make_acf <- function(vec, varname) {
  forecast::ggAcf(vec, lag.max = 36) +
    labs(
      title = paste("ACF de residuos de", varname),
      x = "Rezago (Trimestre)",
      y = "ACF"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 8)
    )
}

make_pacf <- function(vec, varname) {
  forecast::ggPacf(vec, lag.max = 36) +
    labs(
      title = paste("PACF de residuos de", varname),
      x     = "Rezago (Trimestre)",
      y     = "PACF"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 8)
    )
}

acf_plots  <- lapply(names(resids_df), function(v) make_acf(resids_df[[v]], v))
pacf_plots <- lapply(names(resids_df), function(v) make_pacf(resids_df[[v]], v))

grid_acf  <- wrap_plots(acf_plots) + plot_annotation(title = "ACF de residuos del BVAR")
grid_pacf <- wrap_plots(pacf_plots) + plot_annotation(title = "PACF de residuos del BVAR")

grid_acf
grid_pacf

cool_blue <- "#1F77B4"
warm_orange <- "#FF7F0E"

#profesional

plot_acf_pacf <- function(vec, varname, lag.max = 12) {
  # extraer acf
  acf_data <- acf(vec, lag.max = lag.max, plot = FALSE)
  acf_df <- with(acf_data, data.frame(lag, acf))
  
  # extraer pacf 
  pacf_data <- pacf(vec, lag.max = lag.max, plot = FALSE)
  pacf_df <- data.frame(lag = pacf_data$lag, pacf = pacf_data$acf)
  
  # bandas de confianza
  conf_level <- qnorm((1 + 0.95)/2)/sqrt(acf_data$n.used)
  
  # grafico acf
  p_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_segment(aes(xend = lag, yend = 0), 
                 color = cool_blue, linewidth = 0.8) +
    geom_hline(yintercept = c(-1, 1) * conf_level, 
               linetype = "dashed", color = warm_orange) +
    labs(title = paste("ACF:", varname),
         x = NULL, y = "Autocorrelación") +
    scale_x_continuous(breaks = seq(0, lag.max, by = 6)) +
    coord_cartesian(ylim = c(-0.4, 0.4)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5), # Título del gráfico de ACF con tamaño 10
      axis.line.x = element_line(color = "black")
    )
  
  # grafico pacf
  p_pacf <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_segment(aes(xend = lag, yend = 0), 
                 color = cool_blue, linewidth = 0.8) +
    geom_hline(yintercept = c(-1, 1) * conf_level, 
               linetype = "dashed", color = warm_orange) +
    labs(title = paste("PACF:", varname),
         x = "Rezago (Trimestre)", y = "Autocorr. Parcial") +
    scale_x_continuous(breaks = seq(0, lag.max, by = 6)) +
    coord_cartesian(ylim = c(-0.4, 0.4)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5), # Título del gráfico de PACF con tamaño 10
      axis.line.x = element_line(color = "black")
    )
  
  p_combined <- p_acf / p_pacf
  
  return(p_combined)
}

residual_plots <- lapply(names(resids_df), function(v) {
  plot_acf_pacf(resids_df[[v]], v)
})

# Dividir la lista de gráficos 
half_length <- ceiling(length(residual_plots) / 2)
residual_plots_1 <- residual_plots[1:half_length]
residual_plots_2 <- residual_plots[(half_length + 1):length(residual_plots)]

combined_residuals_1 <- wrap_plots(residual_plots_1, ncol = 2) +
  plot_annotation(
    title = "Diagnóstico de Autocorrelación - Modelo BVAR",
    subtitle = "Análisis de residuos para Nicaragua (2006-2023) - Parte 1",
    caption = "Fuente: Elaboración propia con datos del BCN, SEMCA & FRED",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(face = "italic", size = 12, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Combinar los graficos
combined_residuals_2 <- wrap_plots(residual_plots_2, ncol = 2) +
  plot_annotation(
    title = "Diagnóstico de Autocorrelación - Modelo BVAR",
    subtitle = "Análisis de residuos para Nicaragua (2006-2023) - Parte 2",
    caption = "Fuente: Elaboración propia con datos del BCN, SEMCA & FRED",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(face = "italic", size = 12, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(combined_residuals_1)
print(combined_residuals_2)

# suspuestos bvar

check_residuals <- function(model_bvar) {
  resids     <- residuals(model_bvar)
  resids_df  <- as.data.frame(resids)
  results    <- list()
  
  for (v in names(resids_df)) {
    series <- na.omit(resids_df[[v]])
    
    cat("\n----------------------------------------\n")
    cat("RESIDUALES DE:", v, "\n")
    cat("----------------------------------------\n")
    
    # jarque-bera (normalidad)
    jb <- tseries::jarque.bera.test(series)
    print(jb)
    
    # ljung–box (autocorrelacion)
    lb <- Box.test(series, lag = 7, type = "Ljung-Box")
    print(lb)
    
    # arch lm (heterocedasticidad)
    arch <- FinTS::ArchTest(series, lags = 7)
    print(arch)
    
    # Graficos con tamanos y etiquetas ajustados
    p_qq    <- ggplot(data.frame(x = series), aes(sample = x)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste("QQ-Plot:", v)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    p_ts    <- ggplot(data.frame(idx = seq_along(series), y = series),
                      aes(x = idx, y = y)) +
      geom_line(color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Serie de residuos:", v),
           x = "Índice", y = "Residual") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    p_acf  <- forecast::ggAcf(series, lag.max = 12) +
      labs(title = paste("ACF residuos:", v), x = "Lag\n") + # <<-- Aquí se agrega la etiqueta con el salto de línea
      scale_x_continuous(breaks = seq(0, 36, by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    
    p_pacf <- forecast::ggPacf(series, lag.max = 12 ) +
      labs(title = paste("PACF residuos:", v), x = "Lag\n") + # <<-- Aquí se agrega la etiqueta con el salto de línea
      scale_x_continuous(breaks = seq(0, 36, by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    
    grid.arrange(p_qq, p_ts, p_acf, p_pacf, ncol = 2)
    
    grid.text("Fuente: BCN, SECMCA & FRED | Elaboración propia estimada con modelo BVAR",
              x = unit(0.98, "npc"), 
              y = unit(0.02, "npc"), 
              just = "right",
              gp = gpar(fontsize = 12, fontface = "italic", col = "gray40"))
    
    results[[v]] <- list(jb = jb, lb = lb, arch = arch)
  }
  
  return(results)
}
resids_report <- check_residuals(model_bvar_final)

#fevd

#===================== Importante ===================================

# De manera teoria un grafico y tabla fevd debe ser igual al 100%
# Ya sea por la media o acumulado el fevd no llega al 100% 
# revisar el codigo a este punto 

# fevd 12 trimestres
fevd_result <- BVAR::fevd(
  model_bvar_final,
  h = 12,
  conf_bands = c(0.025, 0.975)
)

#recoger indice de la variable remesas
variables <- fevd_result$variables
shock_index <- which(variables == "d1_remesas")

#medianas y cuantiles en la estructura
fevd_median <- apply(fevd_result$fevd[, , , shock_index], 
                     MARGIN = c(2, 3), 
                     FUN = median)
fevd_lower <- apply(fevd_result$fevd[, , , shock_index], 
                    MARGIN = c(2, 3), 
                    FUN = quantile, probs = 0.025)
fevd_upper <- apply(fevd_result$fevd[, , , shock_index], 
                    MARGIN = c(2, 3), 
                    FUN = quantile, probs = 0.975)

#asignar nombres
rownames(fevd_median) <- variables
colnames(fevd_median) <- 1:12
rownames(fevd_lower) <- variables
colnames(fevd_lower) <- 1:12
rownames(fevd_upper) <- variables
colnames(fevd_upper) <- 1:12

#convertir a data frames
df_median <- as.data.frame(fevd_median)
df_median$Variable <- rownames(df_median)
df_median_long <- reshape2::melt(df_median, id.vars = "Variable",
                                 variable.name = "Horizonte", value.name = "Mediana")

df_lower <- as.data.frame(fevd_lower)
df_lower$Variable <- rownames(df_lower)
df_lower_long <- reshape2::melt(df_lower, id.vars = "Variable",
                                variable.name = "Horizonte", value.name = "Inferior")

df_upper <- as.data.frame(fevd_upper)
df_upper$Variable <- rownames(df_upper)
df_upper_long <- reshape2::melt(df_upper, id.vars = "Variable",
                                variable.name = "Horizonte", value.name = "Superior")

#combinar todos los datos
fevd_long <- Reduce(function(x, y) merge(x, y, by = c("Variable", "Horizonte")),
                    list(df_median_long, df_lower_long, df_upper_long))

#convertir horizonte a trimestres
fevd_long$Horizonte <- as.numeric(as.character(fevd_long$Horizonte))

#nombres
variable_labels <- c(
  "d1_iti" = "Términos de Intercambio",
  "d1_ipi_eeuu" = "IPI EEUU",
  "d1_remesas" = "Remesas",
  "d1_itcer" = "Tipo de Cambio Real",
  "d1_transable" = "Sector Transable"
)

#colores
line_colors <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", 
  "#9467BD")

#grafico con bandas de confianza
fevd_plot <- ggplot(fevd_long, aes(x = Horizonte, group = Variable, color = Variable)) +
  geom_ribbon(aes(ymin = Inferior * 100, ymax = Superior * 100, fill = Variable), 
              alpha = 0.2, color = NA) +
  geom_line(aes(y = Mediana * 100), linewidth = 1) +  
  geom_point(aes(y = Mediana * 100), size = 2) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_color_manual(values = line_colors, labels = variable_labels) +
  scale_fill_manual(values = line_colors, labels = variable_labels) +
  labs(
    title = "Impacto de las Remesas en la Economía Nicaragüense",
    subtitle = "Descomposición de Varianza (FEVD) - Porcentaje explicado por shocks en Remesas",
    x = "Horizonte Temporal (Trimestres)",
    y = "Porcentaje de Varianza Explicada",
    color = "Variable Afectada",
    fill = "Variable Afectada",
    caption = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR\nBandas de confianza al 95%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(face = "italic", size = 16, hjust = 1),
    axis.title = element_text(face = "bold", size = 15),
    legend.title = element_text(face = "bold", size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white")
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      override.aes = list(size = 3)
    ),
    fill = "none"
  )

print(fevd_plot)

# fevd sin bandas de confianza

#remesa

#horizonte de medias 12
medianas_h12 <- fevd_long %>%
  filter(Horizonte == 12) %>%
  arrange(desc(Mediana))
orden_variables <- medianas_h12$Variable

#grafico
fevd_plot_remesa_nocum <- ggplot(fevd_long) +
  geom_area(
    aes(
      x = Horizonte,
      y = Mediana * 100,  #convertir a porcentaje
      fill = Variable
    ),
    position = "stack",
    alpha = 0.85,
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = line_colors,
    labels = variable_labels
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(1, 12, by = 1),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  labs(
    title = "Impacto de las Remesas en la Economía Nicaragüense",
    subtitle = "Contribución Mediana a la Varianza del error Explicada por Shocks en Remesas",
    x = "Horizonte Temporal (Trimestres)",
    y = "Porcentaje de Varianza Explicada",
    fill = "Variable Afectada",
    caption = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(face = "italic", size = 16, hjust = 1),
    axis.title = element_text(face = "bold", size = 15),
    legend.title = element_text(face = "bold", size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2
    )
  )

print(fevd_plot_remesa_nocum)

# #fevd itcer
# 
# shock_index_itcer <- which(variables == "d2_itcer")
# 
# #fevd con medias
# fevd_mean_itcer <- apply(fevd_result$fevd[, , , shock_index_itcer], 
#                          MARGIN = c(2, 3), 
#                          FUN = mean)  
# 
# #nombres
# rownames(fevd_mean_itcer) <- variables
# colnames(fevd_mean_itcer) <- 1:12
# 
# #convertir a dataframe
# df_mean_itcer <- as.data.frame(fevd_mean_itcer)
# df_mean_itcer$Variable <- rownames(df_mean_itcer)
# df_mean_long_itcer <- reshape2::melt(df_mean_itcer, id.vars = "Variable",
#                                      variable.name = "Horizonte", value.name = "Media")
# 
# #horizonte numerico
# df_mean_long_itcer$Horizonte <- as.numeric(as.character(df_mean_long_itcer$Horizonte))
# 
# medias_h12_itcer <- df_mean_long_itcer %>%
#   filter(Horizonte == 12) %>%
#   arrange(desc(Media))
# orden_variables_itcer <- medias_h12_itcer$Variable
# 
# # grafico con media
# fevd_plot_itcer_nocum <- ggplot(df_mean_long_itcer) +
#   geom_area(
#     aes(
#       x = Horizonte,
#       y = Media * 100,  #convertir a %
#       fill = Variable
#     ),
#     position = "stack",
#     alpha = 0.85,
#     color = "white",
#     linewidth = 0.3
#   ) +
#   scale_fill_manual(
#     values = line_colors,
#     labels = variable_labels
#   ) +
#   scale_y_continuous(
#     limits = c(0, 100),
#     breaks = seq(0, 100, by = 10),
#     labels = scales::percent_format(scale = 1)
#   ) +
#   scale_x_continuous(
#     breaks = seq(1, 12, by = 1),
#     expand = expansion(add = c(0.5, 0.5))
#   ) +
#   labs(
#     title = "Impacto del Tipo de Cambio Real en la Economía Nicaragüense",
#     subtitle = "Contribución Media a la Varianza Explicada por Shocks en el TCR",
#     x = "Horizonte Temporal (Trimestres)",
#     y = "Porcentaje de Varianza Explicada",
#     fill = "Variable Afectada",
#     caption = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
#     plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
#     plot.caption = element_text(face = "italic", size = 16, hjust = 1),
#     axis.title = element_text(face = "bold", size = 15),
#     legend.title = element_text(face = "bold", size = 15),
#     legend.position = "bottom",
#     legend.text = element_text(size = 15),
#     panel.grid.major = element_line(color = "grey90"),
#     panel.grid.minor = element_blank(),
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white")
#   ) +
#   guides(
#     fill = guide_legend(
#       title.position = "top",
#       title.hjust = 0.5,
#       nrow = 2
#     )
#   )
# 
# print(fevd_plot_itcer_nocum)

#itcer

shock_index_itcer <- which(variables == "d1_itcer")

#calcular media del la variable
fevd_median_itcer <- apply(fevd_result$fevd[, , , shock_index_itcer], 
                               MARGIN = c(2, 3), 
                               FUN = median)

#nombres
rownames(fevd_median_itcer) <- variables
colnames(fevd_median_itcer) <- 1:12

#dataframe
df_median_itcer <- as.data.frame(fevd_median_itcer)
df_median_itcer$Variable <- rownames(df_median_itcer)
df_median_long_itcer <- reshape2::melt(df_median_itcer, id.vars = "Variable",
                                           variable.name = "Horizonte", value.name = "Mediana")

#horizonte numerico
df_median_long_itcer$Horizonte <- as.numeric(as.character(df_median_long_itcer$Horizonte))

#media con horizonte 12
medianas_h12_itcer <- df_median_long_itcer %>%
  filter(Horizonte == 12) %>%
  arrange(desc(Mediana))
orden_variables_itcer <- medianas_h12_itcer$Variable

#grafico
fevd_plot_itcer_nocum <- ggplot(df_median_long_itcer) +
  geom_area(
    aes(
      x = Horizonte,
      y = Mediana * 100,  #convertir a porcentaje
      fill = Variable
    ),
    position = "stack",
    alpha = 0.85,
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = line_colors,
    labels = variable_labels
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(1, 12, by = 1),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  labs(
    title = "Impacto del Sector No Transable en la Economía Nicaragüense",
    subtitle = "Contribución Mediana a la Varianza del error Explicada por Shocks en el Itcer",
    x = "Horizonte Temporal (Trimestres)",
    y = "Porcentaje de Varianza Explicada",
    fill = "Variable Afectada",
    caption = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(face = "italic", size = 16, hjust = 1),
    axis.title = element_text(face = "bold", size = 15),
    legend.title = element_text(face = "bold", size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2
    )
  )

print(fevd_plot_itcer_nocum)

#fevd transable

shock_index_transable <- which(variables == "d1_transable")

#calcular media del la variable
fevd_median_transable <- apply(fevd_result$fevd[, , , shock_index_transable], 
                                  MARGIN = c(2, 3), 
                                  FUN = median)

#nombres
rownames(fevd_median_transable) <- variables
colnames(fevd_median_transable) <- 1:12

#dataframe
df_median_transable <- as.data.frame(fevd_median_transable)
df_median_transable$Variable <- rownames(df_median_transable)
df_median_long_transable <- reshape2::melt(df_median_transable, id.vars = "Variable",
                                              variable.name = "Horizonte", value.name = "Mediana")

#horizonte numerico
df_median_long_transable$Horizonte <- as.numeric(as.character(df_median_long_transable$Horizonte))

#media con horizonte 12
medianas_h12_transable <- df_median_long_transable %>%
  filter(Horizonte == 12) %>%
  arrange(desc(Mediana))
orden_variables_transable <- medianas_h12_transable$Variable

#grafico
fevd_plot_transable_nocum <- ggplot(df_median_long_transable) +
  geom_area(
    aes(
      x = Horizonte,
      y = Mediana * 100,  #convertir a porcentaje
      fill = Variable
    ),
    position = "stack",
    alpha = 0.85,
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = line_colors,
    labels = variable_labels
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(1, 12, by = 1),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  labs(
    title = "Impacto del Sector Transable en la Economía Nicaragüense",
    subtitle = "Contribución Mediana a la Varianza del error Explicada por Shocks en el Sector Transable",
    x = "Horizonte Temporal (Trimestres)",
    y = "Porcentaje de Varianza Explicada",
    fill = "Variable Afectada",
    caption = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(face = "italic", size = 16, hjust = 1),
    axis.title = element_text(face = "bold", size = 15),
    legend.title = element_text(face = "bold", size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2
    )
  )

print(fevd_plot_transable_nocum)

#para ver el horizonte fevd, quitar #

#cat("Primeras filas del data.frame con los resultados de la FEVD:\n")
print(head(fevd_long))
#cat("\n")

#cat("Imprimiendo todo el data.frame de FEVD:\n")
print(fevd_long)
#este es el fev de todas la variables juntas
#print(fevd_long, n = Inf)


#exportar a excel
shocks <- c("d1_remesas", "d1_itcer", "d1_transable")
sheets <- list()

#Calcular medianas y cuantiles (2.5% y 97.5%) sobre las draws
fevd_median_all <- apply(fevd_result$fevd, MARGIN = c(2,3,4), FUN = median)
fevd_lower_all  <- apply(fevd_result$fevd, MARGIN = c(2,3,4), FUN = function(x) quantile(x, probs = 0.025))
fevd_upper_all  <- apply(fevd_result$fevd, MARGIN = c(2,3,4), FUN = function(x) quantile(x, probs = 0.975))

#sheet de resumen a h = 12 (matriz variables x shocks) en %
sheets[["FEVD_h12_median_pct"]] <- cbind(Variable = rownames(as.data.frame(fevd_median_all[,12,])),
                                         as.data.frame(fevd_median_all[,12,] * 100))
sheets[["FEVD_h12_lower_pct"]]  <- cbind(Variable = rownames(as.data.frame(fevd_lower_all[,12,])),
                                         as.data.frame(fevd_lower_all[,12,] * 100))
sheets[["FEVD_h12_upper_pct"]]  <- cbind(Variable = rownames(as.data.frame(fevd_upper_all[,12,])),
                                         as.data.frame(fevd_upper_all[,12,] * 100))

#sheet por shock: wide (filas = variable, cols = 1:12) en % para median/lower/upper
for(sh in shocks){
  i <- which(variables == sh)
  if(length(i) == 0) stop(paste0("No existe la variable: ", sh))
  
  mat_med  <- fevd_median_all[ , , i] * 100   #[variable, horizonte]
  mat_low  <- fevd_lower_all[ , , i] * 100
  mat_high <- fevd_upper_all[ , , i] * 100
  
  df_med  <- as.data.frame(mat_med);  df_med  <- cbind(Variable = rownames(df_med),  df_med)
  df_low  <- as.data.frame(mat_low);  df_low  <- cbind(Variable = rownames(df_low),  df_low)
  df_high <- as.data.frame(mat_high); df_high <- cbind(Variable = rownames(df_high), df_high)
  
  sheets[[paste0(sh, "_median_pct")]] <- df_med
  sheets[[paste0(sh, "_lower_pct")]]  <- df_low
  sheets[[paste0(sh, "_upper_pct")]]  <- df_high
}

#el excel
out <- "fevd_modelo_transable.xlsx"
writexl::write_xlsx(sheets, path = out)
message("Exportado: ", normalizePath(out))


# Agregar nota al pie manualmente
cat("Los valores representan el porcentaje de la varianza de cada variable explicada por shocks la variables. Los intervalos de confianza al 95% se muestran entre corchetes.\n")
##################################################################################################################################################################################
