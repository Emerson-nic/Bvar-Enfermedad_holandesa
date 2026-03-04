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

#veficar cointegracion
data_cointegracion <- data %>%
  dplyr::select(itcer, ipi_eeuu, iti, transable, remesas) %>%
  na.omit()

#rezagos
lag_select <- VARselect(data_cointegracion, lag.max = 8, type = "const")
p_lag <- lag_select$selection["AIC(n)"]

#test de Johansen
#'ecdet = const' asume una constante en la relación de cointegracion
johansen_test <- ca.jo(data_cointegracion, 
                       type = "trace", 
                       ecdet = "const", 
                       K = p_lag)
#resultados
summary(johansen_test)

#existe prueba cointegracion al menos 3 vectores de cointegracion

#datos en log-niveles
data_niveles <- data %>%
  dplyr::select(itcer, ipi_eeuu, iti, transable, remesas) %>%
  mutate(across(everything(), log)) %>% # Log-niveles
  na.omit()

#dummys
data <- data %>% 
  mutate(
    dummy_2008 = ifelse(año == 2008 & mes %in% c("III", "IV"), 1, 0),
    dummy_2018 = ifelse(año == 2018 & mes %in% c("II", "III", "IV"), 1, 0),
    dummy_2020 = ifelse(año == 2020, 1, 0)
  )

#combinar al dataset
data_final <- data %>%
  dplyr::select(
    año, 
    mes,
    dummy_2008,
    dummy_2018,
    dummy_2020
  ) %>%
  bind_cols(data_niveles) %>%
  na.omit() 

#Crear fecha trimestral
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

#graficos aplicados
p1 <- plot_series_professional(data_final, "itcer", "ITCER En Niveles")
p2 <- plot_series_professional(data_final, "iti", "Términos de Intercambio En Niveles")
#p3 <- plot_series_professional(data_final, "no_transable", "Sector No Transable En Niveles")
p4 <- plot_series_professional(data_final, "transable", "Sector Transable En Niveles")
p5 <- plot_series_professional(data_final, "ipi_eeuu", "IPI EEUU En Niveles")
p6 <- plot_series_professional(data_final, "remesas", "Remesas En Niveles")

#combinar graficos
combined_plot_1 <- (p1 | p2) +
  plot_annotation(
    title    = "Series en Log-Niveles para Modelo BVAR",
    subtitle = "Representación estructural de largo plazo (Test de Johansen: r = 3)",
    caption  = "Nota: Variables expresadas en logaritmos naturales. Fuente: BCN, SECMCA y FRED"
  ) & 
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
    plot.caption  = element_text(face = "italic", size = 12, hjust = 1),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

combined_plot_2 <- (p4 | p5 | p6) +
  plot_annotation(
    title    = "Series en Log-Niveles para Modelo BVAR",
    subtitle = "Representación estructural de largo plazo (Test de Johansen: r = 3)",
    caption  = "Nota: Variables expresadas en logaritmos naturales. Fuente: BCN, SECMCA y FRED"
  ) & 
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
    plot.caption  = element_text(face = "italic", size = 12, hjust = 1),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(combined_plot_1)
print(combined_plot_2)


# orden de lags
endogenas <- data_final[, c(
  "ipi_eeuu",
  "remesas",
  "iti",
  "itcer",  
  "transable"
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
  "ipi_eeuu",
  "remesas",
  "iti",
  "itcer",  
  "transable"
)])

exogenas <- as.matrix(data_final[, c(
  "dummy_2018", "dummy_2020", "dummy_2008"
)])

# configuracion de priors 
priors_auto <- BVAR::bv_priors(
  hyper = "auto"
)

# modelo BVAR 

#b = 1 indica que el prior para el primer rezago de cada variable es 1 (Raíz Unitaria)
#'soc' ayuda a que el modelo no explote por las raices unitarias
#'sur': Single-Unit-Root (estabiliza las relaciones de cointegracion)
priors_final <- BVAR::bv_priors(
  hyper = "auto", 
  mn    = BVAR::bv_minnesota(
    lambda = BVAR::bv_lambda(mode = 0.0005, sd = 0.000001, min = 0.0001, max = 0.0006),
    alpha  = BVAR::bv_alpha(mode = 2, sd = 0.0001, min = 1.99, max = 2.01),
    b      = 1
  ),
  soc   = BVAR::bv_soc(mode = 5), #fuerza de la Suma de Coeficientes
  sur   = BVAR::bv_sur(mode = 20)  #fuerza de la Raíz Unitaria Única
)

set.seed(888)
model_bvar_final <- BVAR::bvar(
  data   = endogenas,
  lags   = 2,           
  exogen = exogenas,
  priors = priors_final,
  n_draw = 600000, #aumentamos sorteos para mayor precisión decimal
  n_burn = 150000,
  n_thin = 200
)

#verificar modulo máximo de la media posterior
C <- companion(model_bvar_final)
max_mod <- max(Mod(eigen(C)$values))
cat("modulo maximo:", round(max_mod, 5), "\n")

cat('El modelo no cede, es mejor otro modelo las reices explotan')

summary(model_bvar_final)

#nuevos hiperparametros 
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
  "ipi_eeuu",
  "remesas",
  "iti",
  "itcer",
  "transable"
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
  geom_circle(aes(x0 = 0, y0 = 0, r = 1),
              color     = "grey70",
              linetype  = "dashed",
              linewidth  = 0.7) +
  geom_point(size = 6, alpha = 0.9) +
  
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

#grafico de convergencia

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
  
  #combinar graficos
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
