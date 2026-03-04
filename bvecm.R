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

#seleccion de rezago
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

#Modelo BVECM

#orden cholesky
variables_johansen <- c("ipi_eeuu",  
                        "iti",       
                        "remesas",   
                        "itcer",     
                        "transable")
endogenas_check <- as.matrix(data_final[, variables_johansen])

#extraer los vectores de cointegracion (beta)
#johansen_test@V tiene (k+1) filas por la constante (ecdet="const")
beta_hat <- johansen_test@V[, 1:3] 

#implementar el termino de correccion de error (ECT)
#añadimos una columna de 1s a endogenas para multiplicar por la constante de beta
endogenas_con_constante <- cbind(endogenas_check, 1)
ect <- endogenas_con_constante %*% beta_hat

#crear el rezago del ECT (t-1)
ect_lagged <- ect[1:(nrow(ect)-1), ]

#ajustar el modelo el vecm
dy <- diff(endogenas_check)
exog_vecm <- cbind(diff(exogenas), ect_lagged)

#estimación con bsvars (p=1 en diferencias equivale a p=2 en niveles)
especificacion <- bsvars::specify_bsvar$new(
  data = dy, 
  p    = 6, 
  exog = exog_vecm
)

set.seed(123)
posterior_vecm <- bsvars::estimate(especificacion, S = 80000)

cat('Se determinó un orden de rezago $p=7$ para el sistema en niveles 
    mediante el criterio de AIC. En consecuencia, el modelo BVECM se 
    estimó utilizando $p-1=6$ rezagos en diferencias, permitiendo
    capturar la dinámica de corto plazo sin perder la estructura de 
    la relación de largo plazo')

#velocidad de ajuste alfa
summary(posterior_vecm)

#irf general

#calcular irf
irfs_finales <- bsvars::compute_impulse_responses(posterior_vecm, horizon = 24)

#print
plot(irfs_finales)

#resumen numerico
summary(irfs_finales)

#guardar resumen
sum_vecm <- summary(posterior_vecm)

#irfs

plot_irf <- function(df) {
  labels <- c(
    remesas   = "Remesas",
    itcer     = "Tipo de Cambio Real",
    ipi_eeuu  = "IPI Estados Unidos",
    iti       = "ITI",
    transable = "Sector Transable"
  )
  
  color_palette <- list(
    "remesas"   = "#3498DB",
    "itcer"     = "#2ECC71",
    "transable" = "#FF5733", 
    "ipi_eeuu"  = "#9B59B6",
    "iti"       = "#F39C12"
  )
  
  color_fill <- color_palette[[df$impulso[1]]]
  if(is.null(color_fill)) color_fill <- "#3498DB"
  
  pt <- df[which.max(abs(df$mediana)), ]
  
  ggplot(df, aes(x = periodo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_ribbon(aes(ymin = inferior, ymax = superior), fill = color_fill, alpha = 0.15) +
    geom_line(aes(y = mediana), color = color_fill, linewidth = 1.2) +
    geom_point(data = pt, aes(y = mediana), color = "#E74C3C", size = 3) +
    geom_text(data = pt, aes(x = periodo, y = mediana * 1.1, 
                             label = paste0(periodo, "T: ", round(mediana, 4))),
              color = "#E74C3C", fontface = "bold", size = 3.5,
              vjust = ifelse(pt$mediana > 0, -0.5, 1.2)) +
    labs(x = "Trimestre después del shock", y = "Respuesta (Mediana)", 
         caption = "Intervalo de credibilidad al 95% (Bayesiano)") +
    scale_x_continuous(breaks = seq(0, max(df$periodo), by = 2)) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "gray90", fill = NA, linewidth = 0.5))
}

#resumen de hiperparametros
resumen <- summary(posterior_vecm)
a_global <- resumen$hyper$A["A_global_scale", "mean"]
b_global <- resumen$hyper$B["B_global_scale", "mean"]

cat("\n=== RIGOR METODOLÓGICO: HIPERPARÁMETROS ESTIMADOS ===\n")
cat("- Shrinkage Global A (Lags):    ", round(a_global, 6), "\n")
cat("- Shrinkage Global B (Estructural):", round(b_global, 6), "\n")

#generar los impulsos 
irfs_raw <- bsvars::compute_impulse_responses(posterior_vecm, horizon = 20)

#Shock remesas (3) -> respuestas: itcer (4) y transable (5)
irf_itcer     <- irfs_raw[4, 3, , ] 
irf_transable <- irfs_raw[5, 3, , ]

preparar_df <- function(matriz, var_name, impulso_name) {
  data.frame(
    periodo   = 0:(dim(matriz)[1] - 1),
    mediana   = apply(matriz, 1, median),
    inferior  = apply(matriz, 1, quantile, probs = 0.05),
    superior  = apply(matriz, 1, quantile, probs = 0.95),
    impulso   = impulso_name,
    respuesta = var_name
  )
}

df_itcer     <- preparar_df(irf_itcer, "itcer", "remesas")
df_transable <- preparar_df(irf_transable, "transable", "remesas")

#grafico
p_itcer <- plot_irf(df_itcer) + 
  labs(title = "Evidencia EH: Apreciación del Tipo de Cambio Real",
       subtitle = "Shock de Remesas -> Respuesta ITCER (BVECM)")

p_transable <- plot_irf(df_transable) + 
  labs(title = "Evidencia EH: Impacto en el Sector Transable",
       subtitle = "Shock de Remesas -> Respuesta Producción Transable")

print(p_itcer)
print(p_transable)

#residuos

#bsvars devuelve valores ajistados. 
#ll residuo es: aatos - ajustados
fitted_draws <- bsvars::compute_fitted_values(posterior_vecm)
#obtener la mediana de los residuos para las pruebas clásicas
#dy es la matriz de datos en diferencias usada en el modelo
residuos_mediana <- t(dy[7:nrow(dy),]) - apply(fitted_draws, c(1, 2), median)
residuos_mediana <- t(residuos_mediana)
colnames(residuos_mediana) <- colnames(dy)

#test de residuos
diag_results <- data.frame(
  Variable = colnames(residuos_mediana),
  Jarque_Bera_p = NA,
  Ljung_Box_p = NA,
  ARCH_LM_p = NA
)

for(i in 1:ncol(residuos_mediana)) {
  res <- residuos_mediana[,i]
  
  # jarque-bera (normalidad)
  diag_results$Jarque_Bera_p[i] <- tseries::jarque.bera.test(res)$p.value
  
  # ljung-box (autocorrelacion - lag 4)
  diag_results$Ljung_Box_p[i] <- Box.test(res, lag = 4, type = "Ljung-Box")$p.value
  
  # ARCH-LM (heterocedasticidad - lag 2)
  diag_results$ARCH_LM_p[i] <- FinTS::ArchTest(res, lags = 2)$p.value
}

cat("\n=== Residuos p-vlue ===\n")
print(round(diag_results[,2:4], 4))
#Nota: H0: hay normalidad, no hay autocor, no hay ARCH

#test de convergencia cmcm

#obtener los drawm para el geweke
draws_A <- posterior_vecm$posterior$A[1, 1:5, ]
mcmc_A  <- coda::mcmc(t(draws_A))
geweke_test <- coda::geweke.diag(mcmc_A)
geweke_z <- geweke_test$z
geweke_p <- 2 * (1 - pnorm(abs(geweke_z)))

cat("\n=== test de convergencia GEWEKE (z-scores) ===\n")
print(geweke_test)
cat("Nota: valores absolutos < 1.96 indican convergencia exitosa.\n")

#tabla de resumen
tabla_geweke <- data.frame(
  Variable = names(geweke_z), 
  Z_score  = as.numeric(geweke_z),
  p_value  = as.numeric(geweke_p)
)
tabla_geweke_format <- tabla_geweke
tabla_geweke_format[, 2:3] <- round(tabla_geweke[, 2:3], 4)

cat("\n=== Test GEWEKE p-valores ===\n")
print(tabla_geweke_format)

#tabla de resumen print
tabla_diag <- diag_results %>%
  gt() %>%
  tab_header(title = "Pruebas de Diagnóstico de Residuales (BVECM)") %>%
  fmt_number(columns = 2:4, decimals = 4) %>%
  cols_label(
    Jarque_Bera_p = "Jarque-Bera (p)",
    Ljung_Box_p = "Box-Ljung (p)",
    ARCH_LM_p = "ARCH-LM (p)"
  )

print(tabla_diag)


#raices del bvecm

get_bvecm_roots <- function(model_obj) {
  #obtener la mediana posterior de los coeficientes autorregresivos (A)
  #bvars almacena A como [n_vars, n_vars * p, draws]
  A_median <- apply(model_obj$posterior$A, c(1, 2), median)
  
  #dimensiones
  k <- nrow(A_median)
  p <- ncol(A_median) / k
  
  #matriz companion

  #funcion para visualización de estabilidad
  companion_mat <- matrix(0, k * p, k * p)
  companion_mat[1:k, 1:(k * p)] <- A_median
  if (p > 1) {
    companion_mat[(k + 1):(k * p), 1:(k * (p - 1))] <- diag(k * (p - 1))
  }
  
  #calculo de eigenvalores
  ev <- eigen(companion_mat)$values
  return(data.frame(Re = Re(ev), Im = Im(ev)))
}

#obtener las raices
df_roots <- get_bvecm_roots(posterior_vecm)
df_roots$Sign <- ifelse(df_roots$Re < 0, "Negativa", "No negativa")

#grafico
grafico_raices_bvecm <- ggplot(df_roots, aes(x = Re, y = Im, color = Sign)) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), 
                       color = "grey70", linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_point(size = 5, alpha = 0.8) +
  scale_color_manual(values = c("Negativa" = "#E74C3C", "No negativa" = "#3498DB")) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  labs(
    title = "Raíces del Polinomio Característico (BVECM)",
    subtitle = "Estabilidad del modelo en niveles (Mediana Posterior)",
    x = "Parte Real", y = "Parte Imaginaria",
    color = "Signo de la Parte Real",
    caption = "Fuente: Elaboración propia con resultados del modelo BVECM\nNota: Las raíces sobre el círculo unitario representan los vectores de cointegración."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "grey40", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  )

#print grafico
print(grafico_raices_bvecm)

diag_plot_coef <- plot_diagnostico_bvecm(cadena_impacto, "Impacto Remesas -> ITCER")

diag_plot_final <- diag_plot_coef & 
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(face = "italic", size = 10, hjust = 0.5, color = "gray30")
  )

#grafico centrado
print(diag_plot_final)

#resumen de trazas cmcm, distribucion y acf

diag_plot_coef <- plot_diagnostico_bvecm(cadena_impacto, "Impacto Remesas -> ITCER")

#grafico
plot_diagnostico_bvecm <- function(chain, nombre) {
  chain <- as.numeric(na.omit(chain))
  n_iter <- length(chain)
  mean_val <- mean(chain)
  ess <- tryCatch(round(coda::effectiveSize(coda::mcmc(chain)), 0), error = function(e) n_iter)
  acf_res <- acf(chain, plot = FALSE, lag.max = 8)
  acf_val <- round(acf_res$acf[8], 3) 
  
  df <- data.frame(iter = 1:n_iter, value = chain)
  df$rolling_mean <- zoo::rollmean(df$value, k = max(2, floor(n_iter * 0.1)), fill = NA)
  
  p1 <- ggplot(df, aes(x = iter, y = value)) +
    geom_line(color = "#3498DB", alpha = 0.5) +
    geom_line(aes(y = rolling_mean), color = "#E74C3C", linewidth = 1, na.rm = TRUE) +
    geom_hline(yintercept = mean_val, linetype = "dashed", color = "#2C3E50") +
    labs(title = paste("Trazado de la Cadena MCMC:", nombre),
         subtitle = paste("ESS =", ess, "| ACF(lag7) =", acf_val),
         x = "Iteración", y = "Valor") + theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  p2 <- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)), fill = "#3498DB", alpha = 0.6, bins = 30) +
    geom_density(color = "#E74C3C", linewidth = 1.2) +
    geom_vline(xintercept = mean_val, linetype = "dashed") +
    labs(title = "Distribución Posterior", x = "Valor", y = "Densidad") + theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  df_acf <- data.frame(lag = 0:(length(acf_res$acf)-1), acf = as.numeric(acf_res$acf))
  p3 <- ggplot(df_acf, aes(x = lag, y = acf)) +
    geom_segment(aes(xend = lag, yend = 0), color = "#3498DB", linewidth = 1.5) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-1, 1) * 1.96 / sqrt(n_iter), linetype = "dashed", color = "#E74C3C") +
    labs(title = "Autocorrelación (ACF)", x = "Rezago", y = "ACF") + theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  return((p1) / (p2 | p3))
}
#trazas mcmc 
cadena_impacto <- as.numeric(posterior_vecm$posterior$B[4, 3, ])
diag_plot_coef <- plot_diagnostico_bvecm(cadena_impacto, "Impacto Remesas -> ITCER")

#grafica global
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(face = "italic", size = 10, hjust = 0.5, color = "gray30") # CENTRADO
  )

#print grafico
print(diag_plot_final)

#la cadena de trazado
#B es [n, n, draws] 
#remesas es 3, itcer es 4.
cadena_impacto <- as.numeric(posterior_vecm$posterior$B[4, 3, ])

#print grafico
diag_plot_coef <- plot_diagnostico_bvecm(cadena_impacto, "Impacto Remesas -> ITCER")
print(diag_plot_coef)

#fevd del itcer

fevd_obj <- bsvars::compute_variance_decompositions(posterior_vecm, horizon = 12)

#obterner mediana para el ITCER
#orden es: ipi_eeuu(1), iti(2), remesas(3), itcer(4), transable(5)
fevd_itcer_mediana <- apply(fevd_obj[4, , , ], c(1, 2), median)

#nombre extraido
rownames(fevd_itcer_mediana) <- variables_johansen

#grafico
df_raw <- as.data.frame(t(fevd_itcer_mediana))
df_norm <- as.data.frame(t(apply(df_raw, 1, function(x) (x / sum(x)) * 100)))
df_norm$Horizonte <- 1:nrow(df_norm)
df_long <- tidyr::pivot_longer(df_norm, -Horizonte, names_to = "Variable", values_to = "Porcentaje")

df_long$Variable <- factor(df_long$Variable, levels = variables_johansen)

ggplot(df_long, aes(x = Horizonte, y = Porcentaje, fill = Variable)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("#3498DB", "#2ECC71", "#E74C3C", "#F1C40F", "#9B59B6"),
    labels = c("IPI EEUU", "ITI", "Remesas", "ITCER", "Transable")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1), 
    limits = c(0, 100), 
    oob = scales::squish, 
    expand = c(0, 0)
  ) +
  scale_x_continuous(breaks = seq(1, max(df_norm$Horizonte), by = 2), expand = c(0, 0)) +
  labs(
    title = "Descomposición de Varianza del ITCER (BVECM)",
    subtitle = "Contribución porcentual de los shocks al error de pronóstico",
    x = "Horizonte Temporal (Trimestres)",
    y = "Porcentaje de Varianza Explicada",
    caption = "Fuente: Elaboración propia con resultados del modelo BVECM.\nNota: La dominancia del propio shock del ITCER confirma su persistencia estructural."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0.5),
    plot.caption  = element_text(face = "italic", size = 11, hjust = 0.5, color = "gray30"), # CENTRADO
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#tabla fevd

#extreaer fevd
fevd_obj <- bsvars::compute_variance_decompositions(posterior_vecm, horizon = 12)

#intervalos de confianza
obtener_stats_fevd <- function(target_var_idx, h_idx) {
  #se extrajo todos los draws del horizonte
  draws_h <- fevd_obj[target_var_idx, , h_idx, ] #eesultado [n_vars, draws]
  
  #calcular mediana, lim inferior (2.5%) lim superior (97.5%)
  stats <- apply(draws_h, 1, function(x) {
    q <- quantile(x, probs = c(0.025, 0.5, 0.975))
    return(q)
  })
  
  return(as.data.frame(t(stats)))
}

#los horizontas del fevd h1 princio h12 final
h1_stats  <- obtener_stats_fevd(4, 1)
h12_stats <- obtener_stats_fevd(4, 12)

#tabla formada
tabla_final <- data.frame(
  Fuente_Shock = variables_johansen,
  Corto_Plazo_H1 = paste0(round(h1_stats[,2], 2), "% [", round(h1_stats[,1], 2), ", ", round(h1_stats[,3], 2), "]"),
  Largo_Plazo_H12 = paste0(round(h12_stats[,2], 2), "% [", round(h12_stats[,1], 2), ", ", round(h12_stats[,3], 2), "]")
)

print(tabla_final)

cat('Nota: Debido a que se reportan las medianas de la distribución posterior para cada componente, la suma de las contribuciones puede diferir ligeramente del 100%')

