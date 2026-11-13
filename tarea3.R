# =============================================================================
# ICS2563 - Tarea 3 | Integrantes: Paula Castro, Fransisca Torres

# Para correr:
#   1. Dejar base_tarea3.csv en la MISMA carpeta que este script.
#   2. Correr todo con source("tarea3.R") o Ctrl/Cmd + Shift + Enter.
#   3. Si no tiene los paquetes, instalarlos una vez en la consola con:
#      install.packages(c("data.table", "ggplot2", "patchwork", "fixest"))
#
#
# Paquetes: data.table, ggplot2, patchwork, fixest
# Las figuras se guardan en la carpeta ./figuras/
# =============================================================================

# ---- 0. Preliminares --------------------------------------------------------
library(data.table)
library(ggplot2)
library(patchwork)
library(fixest)

datos <- fread("base_tarea3.csv")
dir.create("figuras", showWarnings = FALSE)

datos[, zona := factor(zona, levels = c("norte", "centro", "sur"),
                       labels = c("Norte", "Centro", "Sur"))]
datos[, norte := as.integer(zona == "Norte")]
datos[, periodo := factor(fifelse(anio <= 2022, "Pre (2019-2022)", "Post (2023-2025)"),
                          levels = c("Pre (2019-2022)", "Post (2023-2025)"))]

colores <- c(Norte = "#C0392B", Centro = "#2471A3", Sur = "#229954")
theme_set(theme_minimal(base_size = 11) + theme(legend.position = "bottom"))

# =============================================================================
# PREGUNTA 1
# =============================================================================
pre <- datos[anio <= 2022]    # período previo al brote

# ---- P1 a) Densidades kernel ------------------------------------------------
densidad <- function(var, titulo, eje_x, ajuste = 1) {
  ggplot(pre, aes(x = .data[[var]], color = zona, fill = zona)) +
    geom_density(alpha = 0.15, adjust = ajuste) +
    scale_color_manual(values = colores) + scale_fill_manual(values = colores) +
    labs(title = titulo, x = eje_x, y = "Densidad", color = "Zona", fill = "Zona")
}

g1 <- densidad("asistencia", "Asistencia", "Asistencia (% de días)") +
  coord_cartesian(xlim = c(70, 100))
g2 <- densidad("puntaje_mat", "Puntaje matemáticas", "Puntaje estandarizado (DE)")
g3 <- densidad("puntaje_leng", "Puntaje lenguaje", "Puntaje estandarizado (DE)")
g4 <- densidad("ingreso_hogar", "Ingreso del hogar", "Ingreso mensual (euros, escala log)") +
  scale_x_log10()
g5 <- densidad("educ_madre", "Educación de la madre", "Años de escolaridad", ajuste = 3)

fig_p1a <- (g1 + g2 + g3 + g4 + g5 + guide_area()) + plot_layout(ncol = 3, guides = "collect")
fig_p1a
ggsave("figuras/p1a_densidades.pdf", fig_p1a, width = 11, height = 6.5)

# ---- P1 b) Tabla de balance -------------------------------------------------
vars <- c("asistencia", "puntaje_mat", "puntaje_leng", "ingreso_hogar", "educ_madre",
          "personas_hogar", "mujer", "edad", "tamano_curso", "repite")

# Panel A.1: medias por zona
medias <- pre[, lapply(.SD, mean), by = zona, .SDcols = vars]
medias_tabla <- dcast(melt(medias, id.vars = "zona"), variable ~ zona)
print(medias_tabla, digits = 3)

# Panel A.2: diferencias Norte - grupo (EE agrupados por colegio) y dif. normalizada
grupos <- list(Centro = "Centro", Sur = "Sur", Resto = c("Centro", "Sur"))

balance <- rbindlist(lapply(vars, function(v) {
  rbindlist(lapply(names(grupos), function(g) {
    sub <- pre[zona %in% c("Norte", grupos[[g]])]
    reg <- feols(as.formula(paste(v, "~ norte")), data = sub, cluster = ~id_colegio)
    x1 <- sub[norte == 1, get(v)]
    x0 <- sub[norte == 0, get(v)]
    data.table(variable = v, grupo = g,
               diferencia = coef(reg)["norte"], ee = se(reg)["norte"],
               dif_normalizada = (mean(x1) - mean(x0)) / sqrt((var(x1) + var(x0)) / 2))
  }))
}))
print(dcast(balance, variable ~ grupo, value.var = c("diferencia", "dif_normalizada")), digits = 2)

# Panel B: ¿tendencias paralelas antes del brote?
# y_it = a_i + l_t + g * (Norte_i x (anio - 2019)) + e_it
pre[, tendencia := anio - 2019]

tendencias <- rbindlist(lapply(c("asistencia", "puntaje_mat", "puntaje_leng"), function(v) {
  rbindlist(lapply(names(grupos), function(g) {
    reg <- feols(as.formula(paste(v, "~ norte:tendencia | id_estudiante + anio")),
                 data = pre[zona %in% c("Norte", grupos[[g]])], cluster = ~id_colegio)
    data.table(variable = v, grupo = g, pendiente_relativa = coef(reg)[1],
               ee = se(reg)[1], p_valor = pvalue(reg)[1])
  }))
}))
print(tendencias, digits = 2)

# Tabla de balance final: medias + diferencias normalizadas, en el orden de 'vars'
dn <- dcast(balance, variable ~ grupo, value.var = "dif_normalizada")
tabla_p1b <- merge(medias_tabla, dn, by = "variable")
setnames(tabla_p1b, c("Centro.x", "Sur.x", "Centro.y", "Sur.y", "Resto"),
         c("Centro", "Sur", "d_Centro", "d_Sur", "d_Resto"))
tabla_p1b <- tabla_p1b[match(vars, variable)]
print(tabla_p1b, digits = 2)

# ---- P1 c) Binscatter asistencia vs puntajes --------------------------------
# 20 bins de igual tamaño de asistencia, dentro de cada zona y período
datos[, bin := ceiling(20 * frank(asistencia, ties.method = "first") / .N),
      by = .(zona, periodo)]

bins <- datos[, .(asistencia   = mean(asistencia),
                  puntaje_mat  = mean(puntaje_mat),
                  puntaje_leng = mean(puntaje_leng)),
              by = .(zona, periodo, bin)]

binscatter <- function(y, titulo) {
  ggplot(bins, aes(x = asistencia, y = .data[[y]], color = periodo)) +
    geom_point(size = 1.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6) +
    facet_wrap(~zona) +
    scale_color_manual(values = c("grey50", "#E67E22")) +
    labs(title = titulo, x = "Asistencia (%)", y = "Puntaje (estandarizado)", color = "Período")
}

fig_mat  <- binscatter("puntaje_mat",  "Asistencia y puntaje en matemáticas")
fig_leng <- binscatter("puntaje_leng", "Asistencia y puntaje en lenguaje")
fig_mat; fig_leng
ggsave("figuras/p1c_binscatter_mat.pdf",  fig_mat,  width = 10, height = 4)
ggsave("figuras/p1c_binscatter_leng.pdf", fig_leng, width = 10, height = 4)

# Pendientes por MCO con microdatos, por zona x período (EE agrupados por colegio)
datos[, zona_periodo := paste(zona, periodo, sep = " | ")]
pendientes <- feols(c(puntaje_mat, puntaje_leng) ~ asistencia, data = datos,
                    split = ~zona_periodo, cluster = ~id_colegio)
tabla_pend <- as.data.table(coeftable(pendientes))[coefficient == "asistencia"]
print(tabla_pend[, .(sample, lhs, Estimate, `Std. Error`)], digits = 3)

# =============================================================================
# PREGUNTA 2: Diferencias en diferencias 2022-2023
# =============================================================================

# Solo 2022 (pre) y 2023 (brote)
d2 <- datos[anio %in% c(2022, 2023)]
d2[, post := as.integer(anio == 2023)]

resultados <- c("asistencia", "puntaje_mat", "puntaje_leng")

# ---- P2 b) Estimación: 3 resultados x 3 grupos de comparación ---------------
# Modelo: y = a + b*norte + h*post + d*(norte x post) + e, EE agrupados por colegio
# También se estima la versión con efectos fijos de estudiante y año (robustez)
modelos  <- list()
tabla_p2 <- list()

for (y in resultados) {
  for (g in names(grupos)) {
    sub <- d2[zona %in% c("Norte", grupos[[g]])]
    
    m    <- feols(as.formula(paste(y, "~ norte * post")),
                  data = sub, cluster = ~id_colegio)
    m_ef <- feols(as.formula(paste(y, "~ norte:post | id_estudiante + anio")),
                  data = sub, cluster = ~id_colegio)
    
    modelos[[paste(y, g)]] <- m
    tabla_p2[[paste(y, g)]] <- data.table(
      resultado = y, grupo = g,
      delta    = coef(m)["norte:post"],
      ee       = se(m)["norte:post"],
      p_valor  = pvalue(m)["norte:post"],
      delta_ef = coef(m_ef)[1],
      ee_ef    = se(m_ef)[1],
      n_obs    = nobs(m))
  }
}

tabla_p2 <- rbindlist(tabla_p2)
num <- c("delta", "ee", "p_valor", "delta_ef", "ee_ef")
tabla_p2[, (num) := lapply(.SD, round, 4), .SDcols = num]
print(tabla_p2)

# Regresión completa (todos los coeficientes) para asistencia, las 3 comparaciones
etable(modelos[c("asistencia Centro", "asistencia Sur", "asistencia Resto")])

# ---- P2 b) DD a partir de las medias (tabla 2x2) ----------------------------
medias_2x2 <- function(y, g) {
  sub <- d2[zona %in% c("Norte", grupos[[g]])]
  m <- sub[, .(media = mean(get(y))),
           by = .(grupo = fifelse(norte == 1, "Norte", g), anio)]
  t <- dcast(m, grupo ~ anio, value.var = "media")
  setnames(t, c("2022", "2023"), c("y2022", "y2023"))
  t[, cambio := y2023 - y2022]
  dd <- t[grupo == "Norte", cambio] - t[grupo != "Norte", cambio]
  print(t, digits = 4)
  cat("DD calculado con medias =", round(dd, 4), "\n\n")
}

medias_2x2("asistencia", "Centro")
medias_2x2("puntaje_mat", "Sur")


# ---- P2 c) Magnitudes para interpretar --------------------------------------
# Media del norte en 2022 (línea base) para cada resultado
media_norte_2022 <- unlist(d2[zona == "Norte" & anio == 2022,
                              lapply(.SD, mean), .SDcols = resultados])
tabla_p2[, media_norte_2022 := media_norte_2022[resultado]]

# Brecha previa (beta: coeficiente de 'norte') y efecto como múltiplo de esa brecha
tabla_p2[, brecha_previa := sapply(modelos, function(m) coef(m)["norte"])]
tabla_p2[, efecto_vs_brecha := delta / brecha_previa]

# Asistencia: efecto en % de la línea base y en días (año escolar de 180 días)
tabla_p2[resultado == "asistencia",
         `:=`(efecto_pct    = 100 * delta / media_norte_2022,
              dias_perdidos = -delta / 100 * 180)]

print(tabla_p2[, .(resultado, grupo, delta, ee, media_norte_2022,
                   brecha_previa, efecto_vs_brecha, efecto_pct, dias_perdidos)],
      digits = 3)

# =============================================================================
# PREGUNTA 3: Event study 2019-2025
# =============================================================================

# ---- P3 a) Evolución de los promedios por zona y año -------------------------
medias_anio <- datos[, lapply(.SD, mean), by = .(zona, anio), .SDcols = resultados]

# Pasar a formato "largo" para graficar las 3 variables en paneles
medias_largo <- melt(medias_anio, id.vars = c("zona", "anio"),
                     variable.name = "resultado", value.name = "media")
medias_largo[, resultado := factor(resultado, levels = resultados,
                                   labels = c("Asistencia (%)",
                                              "Puntaje matemáticas (DE)",
                                              "Puntaje lenguaje (DE)"))]

fig_p3a <- ggplot(medias_largo, aes(x = anio, y = media, color = zona)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~resultado, scales = "free_y", nrow = 1) +
  scale_color_manual(values = colores) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(x = "Año", y = "Promedio", color = "Zona") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_p3a
ggsave("figuras/p3a_evolucion.pdf", fig_p3a, width = 12, height = 4.5)

# Tabla con los promedios (para citar números en el informe)
print(dcast(medias_largo, resultado + zona ~ anio, value.var = "media"), digits = 3)

# =============================================================================
# PREGUNTA 4
# =============================================================================

# ---- P4 a) Modelo DiD con heterogeneidad por sexo ---------------------------

# Restringir la muestra a 2022 y 2023
p4 <- datos[anio %in% c(2022, 2023)]

# Indicador del período posterior al brote
p4[, post := as.integer(anio == 2023)]

# Modelo:
# y_it = alpha_i + lambda_t + beta*(Norte x Post)
#        + delta*(Post x Mujer) + gamma*(Norte x Post x Mujer) + e_it
#
# Efecto en hombres: beta
# Efecto en mujeres: beta + gamma
# Diferencia mujeres - hombres: gamma

# ---- P4 b) Estimaciones para cada variable de resultado ---------------------

variables_p4 <- c("asistencia", "puntaje_mat", "puntaje_leng")

resultados_p4 <- rbindlist(lapply(variables_p4, function(v) {
  
  # Estimar modelo con efectos fijos de estudiante y año
  reg <- feols(as.formula(paste(
    v, "~ norte * post * mujer | id_estudiante + anio"
  )), data = p4, cluster = ~id_colegio)
  
  # Extraer coeficientes y matriz de varianzas-covarianzas
  b <- coef(reg)
  V <- vcov(reg)
  
  # Identificar los coeficientes de interés
  beta <- "norte:post"
  gamma <- "norte:post:mujer"
  
  # Efecto estimado para hombres
  efecto_hombres <- b[beta]
  ee_hombres <- sqrt(V[beta, beta])
  
  # Efecto estimado para mujeres
  efecto_mujeres <- b[beta] + b[gamma]
  ee_mujeres <- sqrt(
    V[beta, beta] + V[gamma, gamma] + 2 * V[beta, gamma]
  )
  
  # Diferencia entre ambos efectos
  diferencia <- b[gamma]
  ee_diferencia <- sqrt(V[gamma, gamma])
  
  # Test de igualdad de efectos entre mujeres y hombres
  ct <- coeftable(reg)
  p_valor <- ct[gamma, "Pr(>|t|)"]
  
  data.table(
    variable = v,
    efecto_hombres = unname(efecto_hombres),
    ee_hombres = unname(ee_hombres),
    efecto_mujeres = unname(efecto_mujeres),
    ee_mujeres = unname(ee_mujeres),
    diferencia = unname(diferencia),
    ee_diferencia = unname(ee_diferencia),
    p_valor = unname(p_valor)
  )
}))

# Presentar resultados
print(resultados_p4, digits = 4)


# =============================================================================
# PREGUNTA 5
# =============================================================================

# ---- P5 a) Estimador de Wald y comparación con MCO --------------------------

# Utilizar exclusivamente las observaciones del año 2023
p5 <- datos[anio == 2023]

# Definir ausentismo como porcentaje de días sin asistir a clases
p5[, ausentismo := 100 - asistencia]

# Estimar Wald y MCO para matemáticas y lenguaje
variables_p5 <- c("puntaje_mat", "puntaje_leng")

resultados_p5 <- rbindlist(lapply(variables_p5, function(v) {
  
  # Mantener observaciones con información completa
  sub <- p5[!is.na(ausentismo) & !is.na(get(v))]
  
  # Medias de ausentismo por grupo
  aus_norte <- sub[norte == 1, mean(ausentismo)]
  aus_resto <- sub[norte == 0, mean(ausentismo)]
  
  # Medias del puntaje por grupo
  punt_norte <- sub[norte == 1, mean(get(v))]
  punt_resto <- sub[norte == 0, mean(get(v))]
  
  # Primer componente: primera etapa (diferencia en ausentismo)
  primera_etapa <- aus_norte - aus_resto
  
  # Segundo componente: forma reducida (diferencia en puntajes)
  forma_reducida <- punt_norte - punt_resto
  
  # Estimador de Wald
  wald <- forma_reducida / primera_etapa
  
  # Estimación por MCO: puntaje sobre ausentismo
  reg_mco <- feols(
    as.formula(paste(v, "~ ausentismo")),
    data = sub,
    cluster = ~id_colegio
  )
  
  data.table(
    variable = v,
    media_aus_norte = aus_norte,
    media_aus_resto = aus_resto,
    primera_etapa = primera_etapa,
    media_punt_norte = punt_norte,
    media_punt_resto = punt_resto,
    forma_reducida = forma_reducida,
    wald = wald,
    mco = coef(reg_mco)["ausentismo"],
    ee_mco = se(reg_mco)["ausentismo"],
    observaciones = nrow(sub)
  )
}))

# Presentar componentes y estimaciones
print(resultados_p5, digits = 4)





