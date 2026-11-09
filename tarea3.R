# =============================================================================
# ICS2563 - Tarea 3 | Integrantes: [Nombre 1], [Nombre 2]
#
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