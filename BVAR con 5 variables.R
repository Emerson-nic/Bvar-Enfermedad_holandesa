
# instalar

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  urca, tseries, dplyr, ggplot2, patchwork, vars, BVAR, 
  parallel, magrittr, coda, forecast, gridExtra,
  MASS, grid, lubridate, reshape2, FinTS, gt, scales, ggforce,
  nortest, rugarch,DescTools, ggrepel, ggthemes, bsvars
)

# preparacion de datos 

series <- c("itcer","ipi_eeuu", "iti", "exportaciones_manofactura_reales", "remesas_reales")
endogenas_originales <- data[, series]

# pre-blanqueo ARIMA automatico
pre_blanqueo_arima <- function(serie) {
  modelo <- auto.arima(
    serie,
    seasonal = TRUE,          
    stepwise = FALSE,        
    approximation = FALSE,    
    trace = TRUE,             
    allowdrift = TRUE,        
    allowmean = TRUE          
  )
  
  residuos <- residuals(modelo)
  
  test <- Box.test(residuos, lag = 24, type = "Ljung-Box")
  cat("Resultado Box-Ljung para", deparse(substitute(serie)), ":\n")
  cat("- p-valor =", round(test$p.value, 4), "\n\n")
  
  return(residuos)
}

# aplicar pre-blanqueo 
endogenas_blanqueadas <- data.frame(
  ipi_eeuu_blanq = pre_blanqueo_arima(endogenas_originales$ipi_eeuu),
  itcer_blanq = pre_blanqueo_arima(endogenas_originales$itcer),
  iti_blanq = pre_blanqueo_arima(endogenas_originales$iti),
  exp_blanq = pre_blanqueo_arima(endogenas_originales$exportaciones_manofactura_reales),
  log_remesas_blanq = pre_blanqueo_arima(log(endogenas_originales$remesas_reales))
  
)
verificar_estacionariedad <- function(serie, nombre) {

  serie <- na.omit(serie)
  
  cat("\n----------------------------------------")
  cat("\nPruebas de estacionariedad para:", nombre)
  cat("\n----------------------------------------\n")
  
  # 1. Prueba adf
  adf <- adf.test(serie)
  cat("ADF Test:\n")
  cat("- p-valor =", round(adf$p.value, 4), "\n")
  cat("- Conclusión:", ifelse(adf$p.value < 0.05, 
                              "Rechaza raíz unitaria (estacionaria)", 
                              "No rechaza raíz unitaria"), "\n\n")
  
  # 2. Prueba pp 
  pp <- ur.pp(serie, type = "Z-tau", model = "trend", lags = "long")
  cat("PP Test:\n")
  cat("- Estadístico =", round(pp@teststat, 4), "\n")
  cat("- Valor crítico 5% =", pp@cval[2], "\n")
  cat("- Conclusión:", ifelse(pp@teststat < pp@cval[2], 
                              "Rechaza raíz unitaria (estacionaria)", 
                              "No rechaza raíz unitaria"), "\n\n")
  
  # 3. Prueba za
  za <- ur.za(serie, model = "both")
  cat("ZA Test (con quiebre estructural):\n")
  cat("- Estadístico =", round(za@teststat, 4), "\n")
  cat("- Valor crítico 5% =", za@cval[2], "\n")
  cat("- Punto de quiebre =", za@bpoint, "\n")
  cat("- Conclusión:", ifelse(za@teststat < za@cval[2], 
                              "Rechaza raíz unitaria (estacionaria)", 
                              "No rechaza raíz unitaria"), "\n\n")
  
  # 4. Prueba kpss
  kpss <- ur.kpss(serie, type = "tau", lags = "long")
  cat("KPSS Test:\n")
  cat("- Estadístico =", round(kpss@teststat, 4), "\n")
  cat("- Valor crítico 5% =", kpss@cval[2], "\n")
  cat("- Conclusión:", ifelse(kpss@teststat < kpss@cval[2], 
                              "No rechaza estacionariedad", 
                              "Rechaza estacionariedad"), "\n")
  
  # Resumen 
  cat("\nCONCLUSIÓN FINAL: ")
  if(adf$p.value < 0.05 & pp@teststat < pp@cval[2] & 
     za@teststat < za@cval[2] & kpss@teststat < kpss@cval[2]) {
    cat("SERIE ESTACIONARIA\n")
  } else {
    cat("PROBLEMAS DE ESTACIONARIEDAD\n")
  }
  
  return(list(
    adf = adf$p.value,
    pp = c(stat = pp@teststat, critical = pp@cval[2]),
    za = c(stat = za@teststat, critical = za@cval[2], breakpoint = za@bpoint),
    kpss = c(stat = kpss@teststat, critical = kpss@cval[2])
  ))
}

# resulatdo de las pruebas

resultados_estacionariedad <- list()

cat("\n\n========================================")
cat("\nVERIFICAR ESTACIONARIEDAD")
cat("\n========================================\n")

resultados_estacionariedad$itcer <- verificar_estacionariedad(
  endogenas_blanqueadas$itcer_blanq, 
  "TCR Blanqueado"
)

resultados_estacionariedad$iti <- verificar_estacionariedad(
  endogenas_blanqueadas$iti_blanq, 
  "Términos de Intercambio Blanqueado"
)

resultados_estacionariedad$export <- verificar_estacionariedad(
  endogenas_blanqueadas$exp_blanq, 
  "Exportaciones Blanqueadas"
)

resultados_estacionariedad$remesas <- verificar_estacionariedad(
  endogenas_blanqueadas$log_remesas_blanq, 
  "Remesas Blanqueadas"
)

resultados_estacionariedad$ipi_eeuu <- verificar_estacionariedad(
  endogenas_blanqueadas$ipi_eeuu_blanq,  # Nombre corregido
  "IPI EEUU Blanqueado"
)

# ---------------------------------------------------------
# resumen de todo
# ---------------------------------------------------------
cat("\n\n========================================")
cat("\nRESUMEN ESTACIONARIEDAD")
cat("\n========================================\n")

for(serie in names(resultados_estacionariedad)) {
  res <- resultados_estacionariedad[[serie]]
  estacionaria <- ifelse(
    res$adf < 0.05 & 
      res$pp["stat"] < res$pp["critical"] & 
      res$za["stat"] < res$za["critical"] & 
      res$kpss["stat"] < res$kpss["critical"],
    "ESTACIONARIA", "NO ESTACIONARIA"
  )
  
  cat("\n-", serie, ":", estacionaria)
  cat(sprintf(
    "\n  (ADF: p=%.3f, PP: stat=%.2f, ZA: stat=%.2f, KPSS: stat=%.2f)",
    res$adf, res$pp["stat"], res$za["stat"], res$kpss["stat"]
  ))
}

#  dummies 
data <- data %>%
  mutate(
    # Dummy para protestas del Canal (Julio-Diciembre 2014)
    dummy_2014 = ifelse(año == 2014 & mes >= 7, 1, 0),
    
    # Dummy para crisis política (Abril-Diciembre 2018)
    dummy_2018 = ifelse(año == 2018 & mes >= 4, 1, 0),
    
    # Dummy para pandemia COVID-19 (Marzo 2020 - Diciembre 2021)
    dummy_covid = ifelse(año >= 2020 & (año < 2022 | (año == 2022 & mes <= 6)), 1, 0),
    
    # Dummy para crisis financiera global (Sept 2008 - Junio 2009)
    dummy_2008 = ifelse((año == 2008 & mes >= 9) | (año == 2009 & mes <= 6), 1, 0)
  )

#  dataset con series blanqueadas y dummies
data_final <- data %>%
  dplyr::select(
    año, 
    mes, 
    dummy_2014,
    dummy_2018,
    dummy_covid,
    dummy_2008
  ) %>%
  bind_cols(endogenas_blanqueadas) %>%
  na.omit() 

cat("\n\n========================================")
cat("\nESTRUCTURA DEL DATASET")
cat("\n========================================\n")
str(data_final)
head(data_final)

data_final$date <- lubridate::ymd(paste0(data_final$año, "-", data_final$mes, "-01"))

# graficos

plot_series_professional <- function(data, var_name, title) {
  if (!"date" %in% names(data)) {
    stop("El dataframe no contiene la columna 'date'")
  }
  y_min <- min(data[[var_name]], na.rm = TRUE)
  y_max <- max(data[[var_name]], na.rm = TRUE)
  
  ggplot(data, aes(x = date, y = .data[[var_name]])) +
    geom_line(color = "#3498DB", linewidth = 1.2, alpha = 0.9) +
    
    # eventos historicos
    geom_vline(xintercept = min(data$date[data$dummy_2008 == 1], na.rm = TRUE), 
               color = "#F1C40F", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    geom_vline(xintercept = min(data$date[data$dummy_2014 == 1], na.rm = TRUE), 
               color = "#E67E22", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    geom_vline(xintercept = min(data$date[data$dummy_2018 == 1], na.rm = TRUE), 
               color = "#27AE60", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    geom_vline(xintercept = min(data$date[data$dummy_covid == 1], na.rm = TRUE), 
               color = "#8E44AD", alpha = 0.6, linewidth = 1.2, linetype = "dotdash") +
    
    # anotaciones
    annotate("text", x = min(data$date[data$dummy_2008 == 1], na.rm = TRUE), y = y_min * 0.95,
             label = "Crisis Financiera (2008)", color = "#F1C40F",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    annotate("text", x = min(data$date[data$dummy_2014 == 1], na.rm = TRUE), y = y_max * 1.05,
             label = "Protestas Canal (2014)", color = "#E67E22",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    annotate("text", x = min(data$date[data$dummy_2018 == 1], na.rm = TRUE), y = y_max * 1.05,
             label = "Crisis Política (2018)", color = "#27AE60",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    annotate("text", x = min(data$date[data$dummy_covid == 1], na.rm = TRUE), y = y_min * 0.95,
             label = "Pandemia COVID-19", color = "#8E44AD",
             angle = 90, vjust = 1.2, size = 3.5, fontface = "bold") +
    
    # eje de tiempo
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.03)) +
    
    # Etiquetas y tema
    labs(
      title    = title,
      subtitle = "Serie histórica blanqueada con eventos destacados",
      x        = NULL,
      y        = NULL,
      caption  = "Fuente: Elaboración propia con datos del BCN, SEMCA & FRED"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption     = element_text(face = "italic", size = 9, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.line.x      = element_line(color = "black"),
      axis.text        = element_text(size = 10),
      axis.title.y     = element_text(margin = margin(r = 10))
    )
}

# graficos individuales
p1 <- plot_series_professional(data_final, "itcer_blanq", "Tipo de Cambio Real Blanqueado")
p2 <- plot_series_professional(data_final, "iti_blanq", "Términos de Intercambio Blanqueados")
p3 <- plot_series_professional(data_final, "exp_blanq", "Exportaciones Manufactura Blanqueadas")
p4 <- plot_series_professional(data_final, "log_remesas_blanq", "Log Remesas Blanqueadas")
p5 <- plot_series_professional(data_final, "ipi_eeuu_blanq",     "IPI Estados Unidos Blanqueado")

# combinar graficos
combined_plot <- (p1 | p2 | p5) / (p3 | p4) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title    = "Series Blanqueadas para la Economía Nicaragüense",
    subtitle = "Período 2006-2023 con eventos históricos destacados",
    caption  = "Nota: Todas las series estacionarizadas mediante pre-blanqueo ARIMA",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      plot.caption  = element_text(face = "italic", size = 10, hjust = 1),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave(
  "series_blanqueadas_simples.png",
  plot = combined_plot,
  width = 16, height = 10, dpi = 400, bg = "white"
)

remesas_export <- p3 / p4 +
  plot_annotation(
    title    = "Exportaciones y Remesas Blanqueadas",
    subtitle = "Nicaragua 2006-2023",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

print(combined_plot)
ggsave(
  "series_blanqueadas_profesional.png",
  plot = combined_plot,
  width = 16, height = 10, dpi = 400, bg = "white"
)

# grafico remesas y exportaciones solamente

remesas_export <- p3 / p4 +
  plot_annotation(
    title    = "Exportaciones y Remesas Blanqueadas",
    subtitle = "Nicaragua 2006-2023",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

ggsave(
  "export_remesas_blanqueadas.png",
  plot = remesas_export,
  width = 10, height = 8, dpi = 400, bg = "white"
)

# garch (ignorar)

# Para ITCER: sGARCH con media ARMA
spec_itcer <- ugarchspec(
  mean.model = list(armaOrder = c(1,0)), 
  variance.model = list(model = "sGARCH", garchOrder = c(0,1)),
  distribution.model = "std"
)
# Para remesas: eGARCH con media ARMA
acf(endogenas_blanqueadas$log_remesas_blanq)  
pacf(endogenas_blanqueadas$log_remesas_blanq)

spec_cgarch <- ugarchspec(
  mean.model = list(armaOrder = c(1,1)),
  variance.model = list(model = "csGARCH", garchOrder = c(1,1)),
  distribution.model = "std"
)

# A. garch aplicada en las series
fit_itcer_garch <- ugarchfit(
  spec = spec_itcer,  
  data = endogenas_blanqueadas$itcer_blanq
)

fit_remesas_garch <- ugarchfit(
  spec = spec_cgarch,  
  data = endogenas_blanqueadas$log_remesas_blanq
)


# B. obtener volatilidades
vol_itcer <- sigma(fit_itcer_garch)
vol_remesas <- sigma(fit_remesas_garch)

# C. convertir a vectores numéricos
vol_itcer_vec <- as.numeric(vol_itcer)
vol_remesas_vec <- as.numeric(vol_remesas)

# D. se agrega al dataframe
data_final$vol_itcer <- vol_itcer_vec
data_final$vol_remesas <- vol_remesas_vec

cat("\n\n========================================")
cat("\nResultados GARCH para ITCER/ ignorar pero se puede usar")
cat("\n========================================\n")
show(fit_itcer_garch)

cat("\n\n========================================")
cat("\nResultados eGARCH para Remesas/ NO PASO LAS PRUEBRAS NO USAR")
cat("\n========================================\n")
show(fit_remesas_garch)

dplyr::select(data_final, date, vol_itcer, vol_remesas) %>% head()

# volatilidad garh como dummy
data_final <- data_final %>%
  mutate(
    crisis_tcr = ifelse(vol_itcer > quantile(vol_itcer, 0.90, na.rm = TRUE), 1, 0),
    crisis_remesas = ifelse(
      vol_remesas > quantile(vol_remesas, 0.90, na.rm = TRUE) & 
        log_remesas_blanq < 0, 
      1, 0
    )
  )


# agragar votalidad en dataframe exogenas
exogenas <- data_final %>%
  dplyr::select(
    crisis_tcr,
    crisis_remesas,
    dummy_2014,
    dummy_2018,
    dummy_covid,
    dummy_2008
  )

eventos_historicos <- data.frame(
  date = as.Date(c(
    "2008-09-01",  # Inicio crisis financiera global
    "2009-06-01",  # Fin crisis financiera
    "2014-07-01",  # Inicio protestas Canal
    "2014-12-01",  # Fin protestas Canal
    "2018-04-01",  # Inicio crisis política
    "2018-12-01",  # Fin crisis política
    "2020-03-01",  # Inicio COVID-19
    "2021-12-01"   # Fin periodo COVID agudo
  )),
  evento = c(
    "Crisis Financiera Inicio",
    "Crisis Financiera Fin",
    "Protestas Canal Inicio",
    "Protestas Canal Fin",
    "Crisis Política Inicio",
    "Crisis Política Fin",
    "COVID-19 Inicio",
    "COVID-19 Fin"
  )
)

data_final <- data_final %>%
  left_join(eventos_historicos, by = "date") %>%
  mutate(
    evento = ifelse(is.na(evento), "Sin crisis", evento),
    en_crisis = ifelse(evento != "Sin crisis", 1, 0)
  )

# grafico garch 

data_final$evento <- "Sin crisis"

for(i in 1:nrow(eventos_historicos)) {
  fecha_evento <- eventos_historicos$date[i]
  evento <- eventos_historicos$evento[i]
  
  idx <- which(data_final$date == fecha_evento)
  
  if(length(idx) > 0) {
    data_final$evento[idx] <- evento
  }
}

data_final$en_crisis <- as.integer(data_final$evento != "Sin crisis")

cat("Eventos asignados:\n")
print(unique(data_final$evento))

cat("\nFechas con eventos:\n")
print(data_final %>% 
        filter(evento != "Sin crisis") %>% 
        distinct(date, evento))

cat("\nEstructura de data_final:\n")
str(data_final[, c("date", "evento", "en_crisis")])


# grafico itcer
y_min <- min(data_final$itcer_blanq - data_final$vol_itcer, na.rm = TRUE) * 1.1
y_max <- max(data_final$itcer_blanq + data_final$vol_itcer, na.rm = TRUE) * 1.25

ggplot(data_final, aes(x = date)) +
  geom_ribbon(aes(ymin = itcer_blanq - vol_itcer, 
                  ymax = itcer_blanq + vol_itcer,
                  fill = "Bandas de Volatilidad (±1σ)"),
              alpha = 0.3) +
  
  geom_line(aes(y = itcer_blanq, color = "TCR Cíclico"), 
            linewidth = 1.2) +
  
  geom_point(data = filter(data_final, crisis_tcr == 1),
             aes(y = itcer_blanq, shape = "Crisis de Volatilidad"),
             color = "#D55E00", size = 3.5) +
  

  geom_vline(data = eventos_historicos,
             aes(xintercept = date),
             linetype = "dotted", color = "grey40", alpha = 0.8) +
  
  geom_label(data = eventos_historicos,
             aes(x = date, y = y_max * 0.95, label = evento),
             angle = 90, hjust = 1, vjust = 0.5, size = 3.2,
             fill = "white", color = "black", alpha = 0.8,
             label.size = 0.2, label.padding = unit(0.3, "lines")) +

  geom_hline(yintercept = 0, linetype = "solid", color = "grey60", alpha = 0.7) +
  geom_hline(yintercept = quantile(data_final$itcer_blanq, 0.1, na.rm = TRUE),
             linetype = "dashed", color = "#CC79A7", alpha = 0.9, linewidth = 0.8) +
  
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(add = c(60, 60))  # Más espacio para etiquetas
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = pretty_breaks(n = 8)
  ) +
  scale_color_manual(values = c("TCR Cíclico" = "#0072B2")) +
  scale_fill_manual(values = c("Bandas de Volatilidad (±1σ)" = "#56B4E9")) +
  scale_shape_manual(values = c("Crisis de Volatilidad" = 17)) +
  
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, 
                              margin = margin(b = 15), color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#34495e", 
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 10, color = "#7f8c8d", hjust = 0, 
                                margin = margin(t = 15)),
    axis.title.y = element_text(size = 13, margin = margin(r = 15), color = "#2c3e50"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 11, color = "#2c3e50"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "#2c3e50"),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) +
  
  labs(
    title = "VOLATILIDAD DEL TIPO DE CAMBIO REAL EN NICARAGUA (2006-2023)",
    subtitle = "Tendencia ciclo con GARCH",
    y = "Componente Cíclico del TCR (blanqueado)",
    caption = "Fuente: Elaboración propia con datos de la Secretaría Ejecutiva del Consejo Monetario Centroamericano (SECMCA)\nNotas: Bandas = ±1 desviación estándar | Triángulos = Crisis de volatilidad (percentil 90)\nLínea morada = Umbral del 10% inferior (sobrevaluación crítica)"
  ) +
  
  guides(
    color = guide_legend(order = 1, keywidth = unit(2, "cm")),
    fill = guide_legend(order = 2),
    shape = guide_legend(order = 3)
  )

ggsave("volatilidad_tcr_nicaragua.png", 
       width = 14, height = 9, dpi = 300, bg = "white")


y_min <- min(data_final$log_remesas_blanq - data_final$vol_remesas, na.rm = TRUE) * 1.1
y_max <- max(data_final$log_remesas_blanq + data_final$vol_remesas, na.rm = TRUE) * 1.25

# grafico de las remesas
ggplot(data_final, aes(x = date)) +
  geom_ribbon(aes(ymin = log_remesas_blanq - vol_remesas, 
                  ymax = log_remesas_blanq + vol_remesas,
                  fill = "Bandas de Volatilidad (±1σ)"),
              alpha = 0.3) +
 
  geom_line(aes(y = log_remesas_blanq, color = "Remesas Cíclicas"), 
            linewidth = 1.2) +

  geom_point(data = filter(data_final, crisis_remesas == 1),
             aes(y = log_remesas_blanq, shape = "Crisis de Volatilidad"),
             color = "#D55E00", size = 3.5) +
  
 
  geom_vline(data = eventos_historicos,
             aes(xintercept = date),
             linetype = "dotted", color = "grey40", alpha = 0.8) +
  
  geom_label(data = eventos_historicos,
             aes(x = date, y = y_max * 0.95, label = evento),
             angle = 90, hjust = 1, vjust = 0.5, size = 3.2,
             fill = "white", color = "black", alpha = 0.8,
             label.size = 0.2, label.padding = unit(0.3, "lines")) +
  

  geom_hline(yintercept = 0, linetype = "solid", color = "grey60", alpha = 0.7) +
  geom_hline(yintercept = quantile(data_final$log_remesas_blanq, 0.1, na.rm = TRUE),
             linetype = "dashed", color = "#CC79A7", alpha = 0.9, linewidth = 0.8) +
  

  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(add = c(60, 60))  
  ) +
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = pretty_breaks(n = 8)
  ) +
  scale_color_manual(values = c("Remesas Cíclicas" = "#0072B2")) +
  scale_fill_manual(values = c("Bandas de Volatilidad (±1σ)" = "#56B4E9")) +
  scale_shape_manual(values = c("Crisis de Volatilidad" = 17)) +
  

  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, 
                              margin = margin(b = 15), color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#34495e", 
                                 margin = margin(b = 20)),
    plot.caption = element_text(size = 10, color = "#7f8c8d", hjust = 0, 
                                margin = margin(t = 15)),
    axis.title.y = element_text(size = 13, margin = margin(r = 15), color = "#2c3e50"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 11, color = "#2c3e50"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, color = "#2c3e50"),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
  ) +
  

  labs(
    title = "VOLATILIDAD DE LAS REMESAS EN NICARAGUA (2006-2023)",
    subtitle = "Evolución del componente cíclico y bandas de volatilidad estimada con GARCH",
    y = "Componente Cíclico de las Remesas (log, blanqueado)",
    caption = "Fuente: Elaboración propia con datos del Banco Central de Nicaragua (BCN)\nNotas: Bandas = ±1 desviación estándar | Triángulos = Crisis de volatilidad (percentil 90)\nLínea morada = Umbral del 10% inferior (caídas significativas)"
  ) +

  guides(
    color = guide_legend(order = 1, keywidth = unit(2, "cm")),
    fill = guide_legend(order = 2),
    shape = guide_legend(order = 3)
  )

ggsave("volatilidad_remesas_nicaragua.pdf", 
       width = 14, height = 9, dpi = 300, bg = "white")

# orden de lags

endogenas <- data_final[, c(
  "ipi_eeuu_blanq",          
  "log_remesas_blanq",  
  "iti_blanq",
  "itcer_blanq",          
  "exp_blanq"     
)]

exogenas <- data_final[, c("dummy_2014", "dummy_2018", "dummy_covid", "dummy_2008")]

# seleccion de rezago
lag_selection <- vars::VARselect( 
  y = endogenas,
  exogen = exogenas,
  lag.max = 18,
  type = "const"
)

cat("Resultados de selección de rezagos:\n")
print(lag_selection$selection)
cat("\nMatriz de criterios:\n")
print(lag_selection$criteria)

# verosimilitud marginal para el orden de rezagos 

p_max <- 18  
p_min <- 1   

endogenas <- data_final[, c( "iti_blanq", "log_remesas_blanq", 
                             "itcer_blanq", "exp_blanq")]
exogenas <- data_final[, c("dummy_2014", "dummy_2018", "dummy_covid", "dummy_2008")]

# calcular el test LR 
lr_test_var <- function(p_u, p_r) {

  var_u <- VAR(endogenas, p = p_u, type = "const", exogen = exogenas)
  var_r <- VAR(endogenas, p = p_r, type = "const", exogen = exogenas)
  
  # Obtener log-verosimilitudes
  loglik_u <- logLik(var_u)
  loglik_r <- logLik(var_r)
  
  # calcular estadistico LR
  LR_stat <- 2 * (loglik_u - loglik_r)
  
  # grados de libertad
  k <- ncol(endogenas)
  df <- k^2 * (p_u - p_r)  
  
  # Valor p
  p_value <- pchisq(LR_stat, df = df, lower.tail = FALSE)
  
  return(data.frame(
    Rezago_NoRestringido = p_u,
    Rezago_Restringido = p_r,
    LR_Statistic = LR_stat,
    Df = df,
    P_Value = p_value
  ))
}

resultados_lr <- data.frame()

for (p_actual in seq(p_max, p_min + 1, -1)) {
  test_result <- lr_test_var(p_actual, p_actual - 1)
  resultados_lr <- rbind(resultados_lr, test_result)
}
print("Resultados de las pruebas de razón de verosimilitud:")
print(resultados_lr)

# identificar el mejor rezago
mejor_rezago <- resultados_lr %>% 
  filter(P_Value < 0.05) %>% 
  slice(1) %>% 
  pull(Rezago_Restringido)

cat("\n=============================================")
cat("\nRecomendación de rezago óptimo basado en LR test:")
cat("\nEl mejor rezago:", mejor_rezago)
cat("\n=============================================\n")

# grafico LR
ggplot(resultados_lr, aes(x = Rezago_Restringido, y = -log10(P_Value))) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_hline(yintercept = -log10(0.05), color = "red", linetype = "dashed") +
  labs(title = "Prueba de Razón de Verosimilitud para Selección de Rezagos",
       subtitle = "Comparación: Rezago p vs p-1",
       x = "Rezago Restringido (p-1)",
       y = "-log10(p-value)") +
  annotate("text", x = mejor_rezago, y = 2, 
           label = paste("Rezago óptimo:", mejor_rezago), color = "red") +
  theme_minimal()

# Prior

  endogenas <- as.matrix(data_final[, c(
    "ipi_eeuu_blanq",
    "log_remesas_blanq",
    "iti_blanq",
    "itcer_blanq",
    "exp_blanq"
  )])
  
  exogenas <- as.matrix(data_final[, c(
    "dummy_2014", "dummy_2018", "dummy_covid", "dummy_2008"
  )])
  
  # configuracion de priors 
  priors_auto <- BVAR::bv_priors(
    hyper = "auto",
    mn = BVAR::bv_mn(
      lambda = BVAR::bv_lambda(mode = 0.3, min = 0.01, max = 0.6),
      alpha = BVAR::bv_alpha(mode = 1.8, min = 1, max = 3.6)
    )
  )
  
  # modelo BVAR 
  set.seed(123)
  model_bvar_final <- BVAR::bvar(
    data = endogenas,
    lags = 14,
    exogen = exogenas,
    priors = priors_auto,
    n_draw = 750000,
    n_burn = 350000,
    n_thin = 150, 
    mh = BVAR::bv_mh(scale_hess = 0.25, adjust_acc = TRUE)
  )


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

var_names <- c(
  "ipi_eeuu_blanq",
  "log_remesas_blanq",
  "iti_blanq",
  "itcer_blanq",
  "exp_blanq"
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
acf6 <- round(acf_all[7], 3)  
acf12 <- round(acf_all[13], 3)
acf13 <- round(acf_all[14], 3)
acf14 <- round(acf_all[15], 3)

cat("ACF lag1:", acf1, "\n")
cat("ACF lag6:", acf6, "\n")
cat("ACF lag12:", acf12, "\n")
cat("ACF lag13:", acf13, "\n")
cat("ACF lag14:", acf14, "\n")

# tamaño efectivo de la cadena
ess_lambda <- as.numeric(effectiveSize(lambda_chain))

calc_z_p <- function(rho, ess) {
  z  <- rho * sqrt(ess)
  p  <- 2 * (1 - pnorm(abs(z)))
  list(z = z, p = p)
}

res13 <- calc_z_p(acf13, ess_lambda)
res14 <- calc_z_p(acf14, ess_lambda)

cat("ESS chain λ:", round(ess_lambda, 1),   "\n\n")

cat("Lag 13:\n")
cat("  ACF       =", acf13,                  "\n")
cat("  z         =", round(res13$z,  3),     "\n")
cat("  p-value   =", round(res13$p,  4),     "\n\n")

cat("Lag 14:\n")
cat("  ACF       =", acf14,                  "\n")
cat("  z         =", round(res14$z,  3),     "\n")
cat("  p-value   =", round(res14$p,  4),     "\n")

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

ggsave(
  filename = "raices_polinomio_caracteristico.pdf",
  plot = grafico_raices,
  width = 8,           
  height = 7,           
  bg = "transparent" 
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
      subtitle = paste("ESS =", round(ess, 1), "| ACF(lag14) =", round(acf14, 3)),
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
  
  lag_max <- min(30, n_iter - 1)
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
      caption  = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR",
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

ggsave(
  "convergencia_lambda_profesional.png", 
  plot = convergence_plot,
  width = 12, height = 10, dpi = 400, bg = "white"
)

# impulsos-respuesta 

# configuracion irf
n_horizon  <- 36                    
conf_bands <- c(0.025, 0.5, 0.975)   


# calcular todas las irfs

irf_all <- BVAR::irf(
  x = model_bvar_final,
  horizon = n_horizon,
  conf_bands = conf_bands
)

# extraer irfs

get_irf_data <- function(irf_obj, impulse, response) {
  vars   <- irf_obj$variables
  i_imp  <- match(impulse, vars)
  i_resp <- match(response, vars)
  q      <- irf_obj$quants  
  
  data.frame(
    periodo   = 0:(dim(q)[3] - 1),
    inferior  = q[1, i_imp, , i_resp],
    mediana   = q[2, i_imp, , i_resp],
    superior  = q[3, i_imp, , i_resp],
    impulso   = impulse,
    respuesta = response,
    row.names = NULL
  )
}

irf_list <- list(
  get_irf_data(irf_all, "log_remesas_blanq", "itcer_blanq"),  
  get_irf_data(irf_all, "itcer_blanq", "exp_blanq"),         
  get_irf_data(irf_all, "log_remesas_blanq", "exp_blanq"),   
  get_irf_data(irf_all, "ipi_eeuu_blanq", "log_remesas_blanq") 
)

plot_irf <- function(df) {
 
  labels <- c(
    log_remesas_blanq = "Log Remesas Blanqueadas",
    itcer_blanq = "Tipo de Cambio Real Blanqueado",
    exp_blanq = "Exportaciones Manufactura Blanqueadas",
    ipi_eeuu_blanq = "IPI Estados Unidos Blanqueado)"
  )
  

  color_palette <- list(
    "log_remesas_blanq" = "#3498DB",  
    "itcer_blanq" = "#2ECC71",        
    "ipi_eeuu_blanq" = "#9B59B6"      
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
                  label = paste0(periodo, "m: ", round(mediana, 3))),
              color = "#E74C3C", fontface = "bold", size = 3.5,
              vjust = ifelse(pt$mediana > 0, -0.5, 1.2)) +
    labs(
      title = paste("Shock en", labels[df$impulso[1]]),
      subtitle = paste("Respuesta de", labels[df$respuesta[1]]),
      x = "Meses después del shock",
      y = "Mediana IRF",
      caption = paste0("Intervalo de credibilidad al ", 95, "%")
    ) +
    scale_x_continuous(breaks = seq(0, n_horizon, by = 6)) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 15, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray90", fill = NA, linewidth = 0.5),
      plot.caption =  element_text(size = 15, hjust = 0.5)
    )
}

plots <- lapply(irf_list, plot_irf)

combined <- (plots[[1]] + plots[[2]]) / 
  (plots[[3]] + plots[[4]]) +
  plot_annotation(
    title    = "Mecanismo de Enfermedad Holandesa en Nicaragua",
    subtitle = "Análisis de Impulso-Respuesta",
    caption  = paste0("Horizonte: ", n_horizon, " meses | Periodo: 2006-2023\n",
                      "Fuente: BCN, SEMCA & FRED| Elaboración propia estimada con modelo BVAR"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  )

print(combined)
ggsave(
  "irf_enfermedad_holandesa_4paneles.png",
  plot   = combined,
  width  = 14, 
  height = 10,  
  dpi    = 400,
  bg     = "white"
)


# irfs acumulados

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
  get_irf_acumulada(irf_all, "log_remesas_blanq", "itcer_blanq"),
  get_irf_acumulada(irf_all, "itcer_blanq", "exp_blanq"),
  get_irf_acumulada(irf_all, "log_remesas_blanq", "exp_blanq"),
  get_irf_acumulada(irf_all, "ipi_eeuu_blanq", "log_remesas_blanq")  # Nuevo
)

plot_irf_acum <- function(df) {
  labels <- c(
    log_remesas_blanq = "Log Remesas Blanqueadas",
    itcer_blanq = "Tipo de Cambio Real Blanqueado",
    exp_blanq = "Exportaciones Manufactura Blanqueadas",
    ipi_eeuu_blanq = "IPI Estados Unidos Blanqueado"
  )
  
  color_palette <- list(
    "log_remesas_blanq" = "#3498DB",  
    "itcer_blanq" = "#2ECC71",        
    "ipi_eeuu_blanq" = "#9B59B6"     
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
  
  ggplot(df, aes(x = periodo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    geom_ribbon(aes(ymin = inferior, ymax = superior),
                fill = color_fill, alpha = 0.2) +
    geom_line(aes(y = mediana), color = color_fill, linewidth = 2.0) +
    
    geom_point(data = pt, aes(y = mediana), color = "#E74C3C", size = 3, stroke = 1) + 
    
    geom_label(
      data = pt,
      aes(x = label_x, y = label_offset, 
          label = paste0(periodo, "m: ", round(mediana, 3))),
      color = "#E74C3C", 
      fill = alpha("white", 0.85),
      size = 4,  
      label.size = 0.3,
      label.padding = unit(0.15, "lines"),
      fontface = "bold",
      vjust = ifelse(pt$mediana > 0, -0.2, 1.2),
      hjust = label_hjust
    ) +
    
    labs(
      title = paste("Shock en", labels[df$impulso[1]]), 
      subtitle = paste("Respuesta acumulada de", labels[df$respuesta[1]]),
      x = "Meses después del shock",
      y = "Efecto acumulado",
      caption = paste0("Intervalo de credibilidad al 95%")
    ) +
    scale_x_continuous(
      breaks = seq(0, n_horizon, by = 6),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    theme_minimal(base_size = 13) + 
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Mismo tamaño
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#34495E"),  # Mismo tamaño
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3),
      panel.border = element_rect(color = "gray90", fill = NA, linewidth = 0.5),  # Mismo borde
      plot.margin = unit(c(10, 15, 10, 10), "mm"),  # Márgenes similares
      plot.caption = element_text(size = 15, margin = margin(t = 5))
    ) +
    coord_cartesian(clip = "off")
}

# combinar graficos
plots_acum <- lapply(irf_list_acum, function(df) {
  tryCatch(plot_irf_acum(df), error = function(e) ggplot() + labs(title = "Error en gráfico"))
})

combined_acum <- (plots_acum[[1]] + plots_acum[[2]]) / 
  (plots_acum[[3]] + plots_acum[[4]]) +
  plot_annotation(
    title    = "Mecanismo de Enfermedad Holandesa en Nicaragua",
    subtitle = "Análisis de Impulso-Respuesta Acumulado",
    caption  = paste0("Horizonte: 35 meses | Periodo: 2006-2023\n",
                      "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR"),
    theme    = theme(
      plot.title        = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle     = element_text(size = 18, hjust = 0.5, color = "#34495E"),
      plot.caption      = element_text(size = 16, color = "#7F8C8D", hjust = 0.5),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA)
    )
  )

print(combined_acum)
ggsave(
  "irf_acumulados_enfermedad_holandesa.png",
  plot  = combined_acum,
  width = 16,   
  height = 12,   
  dpi = 400,
  bg = "white"
)
# diagnostico de resuduos bvar 

# extraer los residuos
resids_mat <- residuals(model_bvar_final)  
resids_df  <- as.data.frame(resids_mat)

make_acf <- function(vec, varname) {
  forecast::ggAcf(vec, lag.max = 36) +
    labs(
      title = paste("ACF de residuos de", varname),
      x = "Rezago (meses)",
      y = "ACF"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12)
    )
}

make_pacf <- function(vec, varname) {
  forecast::ggPacf(vec, lag.max = 36) +
    labs(
      title = paste("PACF de residuos de", varname),
      x     = "Rezago (meses)",
      y     = "PACF"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12)
    )
}

acf_plots  <- lapply(names(resids_df), function(v) make_acf(resids_df[[v]], v))
pacf_plots <- lapply(names(resids_df), function(v) make_pacf(resids_df[[v]], v))

grid_acf  <- wrap_plots(acf_plots,  ncol = 2, nrow = 3) + plot_annotation(title = "ACF de residuos del BVAR")
grid_pacf <- wrap_plots(pacf_plots, ncol = 2, nrow = 3) + plot_annotation(title = "PACF de residuos del BVAR")

grid_acf
grid_pacf

cool_blue <- "#1F77B4"
warm_orange <- "#FF7F0E"


plot_acf_pacf <- function(vec, varname, lag.max = 36) {
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
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
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
         x = "Rezago (meses)", y = "Autocorr. Parcial") +
    scale_x_continuous(breaks = seq(0, lag.max, by = 6)) +
    coord_cartesian(ylim = c(-0.4, 0.4)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.line.x = element_line(color = "black")
    )
  p_combined <- p_acf / p_pacf + 
    plot_annotation(
      title = paste("Análisis de Autocorrelación:", varname),
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#333333")
      )
    )
  
  return(p_combined)
}

residual_plots <- lapply(names(resids_df), function(v) {
  plot_acf_pacf(resids_df[[v]], v)
})

combined_residuals <- wrap_plots(residual_plots, ncol = 2) +
  plot_annotation(
    title = "Diagnóstico de Autocorrelación - Modelo BVAR",
    subtitle = "Análisis de residuos para Nicaragua (2006-2023)",
    caption = "Fuente: Elaboración propia con datos del BCN, SEMCA & FRED",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(face = "italic", size = 16, hjust = 1),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(combined_residuals)

ggsave(
  "diagnostico_autocorrelacion_bvar.png",
  plot = combined_residuals,
  width = 14, 
  height = 10,
  dpi = 400,
  bg = "white"
)

for (v in names(resids_df)) {
  p <- plot_acf_pacf(resids_df[[v]], v) +
    plot_annotation(
      theme = theme(
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.background = element_rect(fill = "#f8f9fa", color = NA)
      )
    )
  
  ggsave(
    paste0("acf_pacf_", v, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
}

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
    lb <- Box.test(series, lag = 14, type = "Ljung-Box")
    print(lb)
    
    # arch lm (heterocedasticidad)
    arch <- FinTS::ArchTest(series, lags = 14)
    print(arch)
    
    # Graficos
    p_qq   <- ggplot(data.frame(x = series), aes(sample = x)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste("QQ-Plot:", v)) +
      theme_minimal()
    
    p_ts   <- ggplot(data.frame(idx = seq_along(series), y = series),
                     aes(x = idx, y = y)) +
      geom_line(color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Serie de residuos:", v),
           x = "Índice", y = "Residual") +
      theme_minimal()
    
    p_acf  <- forecast::ggAcf(series, lag.max = 36) +
      labs(title = paste("ACF residuos:", v)) +
      theme_minimal()
    
    p_pacf <- forecast::ggPacf(series, lag.max = 36) +
      labs(title = paste("PACF residuos:", v)) +
      theme_minimal()
    
    # Necesitamos explícitamente imprimir el arreglo de gráficos
    grid.arrange(p_qq, p_ts, p_acf, p_pacf, ncol = 2)
    
    results[[v]] <- list(jb = jb, lb = lb, arch = arch)
  }
  
  return(results)
}
resids_report <- check_residuals(model_bvar_final)

# fevd


# fevd 36 meses
fevd_result <- BVAR::fevd(
  model_bvar_final,
  h = 36, 
  conf_bands = c(0.025, 0.975)
)


cat("Horizonte calculado:", dim(fevd_result$fevd)[2], "meses\n")


cat("Cuantiles disponibles:", dimnames(fevd_result$quants)[[1]], "\n")

# metodo alternativo si hay errores
if (!("2.5%" %in% dimnames(fevd_result$quants)[[1]])) {
  # calcular manualmente los cuantiles
  fevd_samples <- fevd_result$fevd
  
  # calcular percentiles manualmente
  fevd_quants <- apply(
    fevd_samples,
    MARGIN = c(2, 3, 4),
    FUN = quantile,
    probs = c(0.025, 0.16, 0.50, 0.84, 0.975)
  )
  
  # reestructurar en formato compatible
  dimnames(fevd_quants)[[1]] <- c("2.5%", "16%", "50%", "84%", "97.5%")
  fevd_result$quants <- fevd_quants
}

cat("Cuantiles actualizados:", dimnames(fevd_result$quants)[[1]], "\n")

variables <- fevd_result$variables
shock_index <- which(variables == "log_remesas_blanq")

fevd_mat <- fevd_result$quants["50%", , , shock_index]
rownames(fevd_mat) <- variables

# bandas de confianza 95% 
if ("2.5%" %in% dimnames(fevd_result$quants)[[1]]) {
  fevd_lower <- fevd_result$quants["2.5%", , , shock_index]  
  fevd_upper <- fevd_result$quants["97.5%", , , shock_index]  
} else {
  # usar los disponibles si no estan los de 95%
  fevd_lower <- fevd_result$quants["16%", , , shock_index]
  fevd_upper <- fevd_result$quants["84%", , , shock_index]
}
rownames(fevd_lower) <- variables
rownames(fevd_upper) <- variables

df_median <- as.data.frame(fevd_mat)
df_median$Variable <- rownames(df_median)
df_median_long <- melt(df_median, id.vars = "Variable",
                       variable.name = "Horizonte", value.name = "Mediana")

df_lower <- as.data.frame(fevd_lower)
df_lower$Variable <- rownames(df_lower)
df_lower_long <- melt(df_lower, id.vars = "Variable",
                      variable.name = "Horizonte", value.name = "Inferior")

df_upper <- as.data.frame(fevd_upper)
df_upper$Variable <- rownames(df_upper)
df_upper_long <- melt(df_upper, id.vars = "Variable",
                      variable.name = "Horizonte", value.name = "Superior")

fevd_long <- Reduce(function(x, y) merge(x, y, by = c("Variable", "Horizonte")),
                    list(df_median_long, df_lower_long, df_upper_long))

fevd_long$Horizonte <- as.numeric(gsub("V", "", fevd_long$Horizonte))

variable_labels <- c(
  "iti_blanq" = "Términos de Intercambio",
  "log_remesas_blanq" = "Log Remesas",
  "itcer_blanq" = "Tipo de Cambio Real",
  "exp_blanq" = "Exportaciones Manufactureras"
)

line_colors <- c(
  "#1F77B4",
  "#FF7F0E",  
  "#2CA02C",
  "#D62728",  
  "#9467BD",
  "#8C564B"   
)


ggplot(fevd_long, aes(x = Horizonte, y = Mediana * 100,
                      color = Variable, fill = Variable, group = Variable)) +
  
  geom_ribbon(aes(ymin = Inferior * 100, ymax = Superior * 100),
              alpha = 0.15, color = NA) +
  
  geom_line(linewidth = 0.5, alpha = 0.9) +
  geom_point(size = 3, alpha = 0.8) +
  
  scale_color_manual(values = line_colors, labels = variable_labels) +
  scale_fill_manual(values = line_colors, labels = variable_labels, guide = "none") +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = percent_format(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 36, by = 6),
    expand = expansion(add = c(0.5, 0.5))
  ) +
  
  labs(
    title = "Impacto de las Remesas en la Economía Nicaragüense",
    subtitle = "Descomposición de Varianza (FEVD) - Porcentaje explicado por shocks en Remesas",
    x = "Horizonte Temporal (Meses)",
    y = "Porcentaje de Varianza Explicada",
    color = "Variable Afectada",
    caption = "Fuente: BCN, SEMCA & FRED | Elaboración propia estimada con modelo BVAR\nBandas de confianza al 95%"
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
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 2,
      override.aes = list(size = 3)
    )
  ) +
  
  geom_hline(yintercept = 50, linetype = "dashed", 
             color = "#E63946", alpha = 0.6) +
  annotate(
    "text", 
    x = 36, y = 53, 
    label = "Límite del 50%", 
    color = "#E63946", 
    size = 3.5,
    hjust = 1,
    fontface = "italic"
  )

ggsave(
  "fevd_remesas_bandas_95.png", 
  width = 10, 
  height = 7, 
  dpi = 400, 
  bg = "white"
)

# Matriz de correlación de residuos
res_cor <- cor(residuals(model_bvar_final))

# Gráfico de calor mejorado
ggplot(reshape2::melt(res_cor), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#E74C3C", high = "#3498DB", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  labs(title = "Correlación Contemporánea entre Residuos",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# reconstruir matriz 

cor_matrix <- matrix(
  c(
    1.00,  0.29, -0.18,  0.10,  0.30,  
    0.29,  1.00,  0.03, -0.14,  0.06,  
    -0.18,  0.03,  1.00, -0.07, -0.10,  
    0.10, -0.14, -0.07,  1.00,  0.30,  
    0.30,  0.06, -0.10,  0.30,  1.00   
  ),
  nrow  = 5,
  byrow = TRUE
)

var_names <- c(
  "ipi_eeuu_blanq",
  "log_remesas_blanq",
  "iti_blanq",
  "itcer_blanq",
  "exp_blanq"
)
rownames(cor_matrix) <- colnames(cor_matrix) <- var_names

cor_df <- melt(
  cor_matrix,
  varnames   = c("Variable1", "Variable2"),
  value.name = "Correlacion"
)

cor_df$Variable1 <- factor(cor_df$Variable1, levels = var_names)
cor_df$Variable2 <- factor(cor_df$Variable2, levels = rev(var_names))

p <- ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlacion)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Correlacion),
                color = ifelse(abs(Correlacion) > 0.3, "white", "black")),
            size = 4, fontface = "bold", show.legend = FALSE) +
  scale_fill_gradient2(
    low      = "#D55E00",  
    mid      = "white",
    high     = "#0072B2",  
    midpoint = 0,
    limits   = c(-1, 1),
    name     = "Correlación"
  ) +
  scale_color_identity() +
  labs(
    title = "Correlación Contemporánea entre Residuos",
    subtitle = "Nicaragua 2006–2023",
    caption = "Fuente: Elaboracón propia con datos del BCN, SEMCA & FRED"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(face = "italic", size = 16, hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 15)
  ) +
  # Resaltar diagonal con un marco verde
  geom_tile(
    data = subset(cor_df, Variable1 == Variable2),
    fill = NA,
    color = "#009E73",
    linewidth = 1.2
  )

print(p)

ggsave(
  filename = "correlacion_residuos_5vars.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 400,
  bg = "white"
)
