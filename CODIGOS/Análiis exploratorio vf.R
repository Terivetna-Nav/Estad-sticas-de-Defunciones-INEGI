# ==============================================================================
# ANALISIS EXPLORATORIO DE HOMICIDIOS 2015–2023 (AÑO DE OCURRENCIA)
# ==============================================================================

rm(list = ls())
gc()

# ==============================================================================
# 0. PAQUETES
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, scales, forcats, stringr, glue
)

theme_set(theme_minimal(base_size = 12))

# ==============================================================================
# 1. RUTAS
# ==============================================================================

ruta_base   <- "C:/MIS DOCUMENTOS/DATA CIVICA/Estad-sticas-de-Defunciones-INEGI"
ruta_salida <- file.path(ruta_base, "Salida")
if (!dir.exists(ruta_salida)) dir.create(ruta_salida, recursive = TRUE)

# ==============================================================================
# 2. BASE
# ==============================================================================

base <- readRDS(
  file.path(ruta_salida, "base_homicidios_2015_2023_ANALISIS.rds")
)

# ==============================================================================
# 3. CATÁLOGOS Y VARIABLES ANALÍTICAS
# ==============================================================================

base <- base |>
  filter(sexo %in% c("Hombre", "Mujer")) |>
  mutate(
    # Mecanismo de agresión
    mecanismo_agresion = case_when(
      str_detect(causa_def, "^X9[3-5]|^Y2[0-4]") ~ "Arma de fuego",
      str_detect(causa_def, "^X9[6-7]") ~ "Arma blanca / punzocortante",
      str_detect(causa_def, "^X8[5-9]|^Y0[0-9]") ~ "Otros medios especificados",
      TRUE ~ "No identificado"
    ),
    mecanismo_agresion = factor(
      mecanismo_agresion,
      levels = c(
        "Arma de fuego",
        "Arma blanca / punzocortante",
        "Otros medios especificados",
        "No identificado"
      )
    ),
    
    # Variables defensivas (evita errores)
    conindig    = if ("conindig" %in% names(base)) conindig else NA,
    afromex    = if ("afromex" %in% names(base)) afromex else NA,
    nacionalid = if ("nacionalid" %in% names(base)) nacionalid else NA
  )

# ==============================================================================
# 4. METADATOS
# ==============================================================================

rango_homicidios <- paste0(
  min(base$anio_ocur, na.rm = TRUE),
  "–",
  max(base$anio_ocur, na.rm = TRUE)
)

pie_de_foto <- paste0(
  "Fuente: Elaboración propia con datos de INEGI. Defunciones por homicidio (",
  rango_homicidios, ")."
)

colores_sexo <- c(
  "Total"   = "black",
  "Hombres" = "#08306B",
  "Mujeres" = "#6BAED6"
)

colores_mecanismo <- c(
  "Arma de fuego" = "#6A1B9A",
  "Arma blanca / punzocortante" = "#8E24AA",
  "Otros medios especificados" = "#B39DDB",
  "No identificado" = "#BDBDBD"
)

# ==============================================================================
# GRÁFICA 1
# ==============================================================================

g1_data <- base |>
  count(anio_ocur, sexo) |>
  mutate(sexo_plot = recode(sexo, Hombre = "Hombres", Mujer = "Mujeres")) |>
  bind_rows(
    base |> count(anio_ocur) |> mutate(sexo_plot = "Total")
  )

g1 <- ggplot(g1_data,
             aes(anio_ocur, n, color = sexo_plot, group = sexo_plot)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = colores_sexo) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Gráfica 1. Homicidios en México por sexo",
    x = "Año de ocurrencia",
    y = "Número de homicidios",
    caption = pie_de_foto
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(ruta_salida, "g1_homicidios_totales_sexo.png"),
       g1, width = 9, height = 5, dpi = 300)


# ==============================================================================
# GRÁFICA 2: HOMICIDIOS DE MUJERES + TENDENCIA POR MECANISMO
# ==============================================================================

# --------------------------------------------------
# 1. Total de homicidios de mujeres por año
# --------------------------------------------------
g2_total <- base |>
  filter(sexo == "Mujer") |>
  count(anio_ocur, name = "total") |>
  mutate(serie = "Total mujeres")

# --------------------------------------------------
# 2. Homicidios de mujeres por mecanismo y año
# --------------------------------------------------
g2_mecanismo <- base |>
  filter(
    sexo == "Mujer",
    !is.na(mecanismo_agresion)
  ) |>
  count(anio_ocur, mecanismo_agresion, name = "total") |>
  mutate(serie = mecanismo_agresion)

# --------------------------------------------------
# 3. Unir datos
# --------------------------------------------------
g2_data <- bind_rows(g2_total, g2_mecanismo)

# --------------------------------------------------
# 4. Paleta de colores específica
# --------------------------------------------------
colores_g2 <- c(
  "Total mujeres" = "#1F78B4",
  "Arma de fuego" = "#7B1FA2",
  "Arma blanca / punzocortante" = "#BA68C8",
  "Otros medios especificados" = "#CE93D8",
  "No identificado" = "#9E9E9E"
)

# --------------------------------------------------
# 5. Gráfica
# --------------------------------------------------
g2 <- ggplot(g2_data, aes(anio_ocur, total, color = serie)) +
  
  # Líneas
  geom_line(
    aes(linewidth = serie),
    alpha = 0.95
  ) +
  
  # Puntos SOLO para el total
  geom_point(
    data = g2_total,
    size = 2.6
  ) +
  
  # Etiquetas SOLO para el total
  geom_text(
    data = g2_total,
    aes(label = comma(total)),
    vjust = -0.9,
    size = 3,
    color = "#1F78B4"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  scale_color_manual(
    values = colores_g2,
    breaks = c(
      "Total mujeres",
      "Arma de fuego",
      "Arma blanca / punzocortante",
      "Otros medios especificados",
      "No identificado"
    )
  ) +
  
  scale_linewidth_manual(
    values = c(
      "Total mujeres" = 1.6,
      "Arma de fuego" = 0.9,
      "Arma blanca / punzocortante" = 0.9,
      "Otros medios especificados" = 0.9,
      "No identificado" = 0.9
    ),
    guide = "none"
  ) +
  
  labs(
    title = "Gráfica 2. Homicidios de mujeres en México",
    subtitle = "Tendencia total y por mecanismo de agresión, 2015–2023",
    x = "Año de ocurrencia",
    y = "Número de homicidios",
    color = "Mecanismo de agresión",
    caption = pie_de_foto
  ) +
  
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    legend.text = element_text(size = 9)
  )

# --------------------------------------------------
# 6. Imprimir y guardar
# --------------------------------------------------
print(g2)

ggsave(
  file.path(ruta_salida, "g2_homicidios_mujeres_tendencia_mecanismo.png"),
  g2,
  width = 10,
  height = 5.5,
  dpi = 300
)


# ==============================================================================
# GRÁFICA 3
# ==============================================================================

g3 <- base |>
  count(anio_ocur, sexo, mecanismo_agresion) |>
  group_by(anio_ocur, sexo) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(factor(anio_ocur), prop, fill = mecanismo_agresion)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ sexo) +
  scale_fill_manual(values = colores_mecanismo) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Gráfica 3. Mecanismo de agresión por sexo",
    x = "Año de ocurrencia",
    y = "Proporción",
    fill = "Mecanismo",
    caption = pie_de_foto
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(ruta_salida, "g3_mecanismo_sexo.png"),
       g3, width = 11, height = 6.5, dpi = 300)

# ==============================================================================
# GRÁFICA 4: HEATMAP — MECANISMO DE AGRESIÓN POR ENTIDAD Y SEXO
# ==============================================================================

# Asegurar que tenemos la base correcta
# (usa el objeto `base` que ya cargaste)
stopifnot(exists("base"))

# ------------------------------------------------------------------------------
# CATÁLOGO DE ENTIDADES FEDERATIVAS (INEGI)
# ------------------------------------------------------------------------------
cat_entidades <- c(
  "01"="Aguascalientes","02"="Baja California","03"="Baja California Sur",
  "04"="Campeche","05"="Coahuila","06"="Colima","07"="Chiapas",
  "08"="Chihuahua","09"="Ciudad de México","10"="Durango",
  "11"="Guanajuato","12"="Guerrero","13"="Hidalgo","14"="Jalisco",
  "15"="Estado de México","16"="Michoacán","17"="Morelos",
  "18"="Nayarit","19"="Nuevo León","20"="Oaxaca","21"="Puebla",
  "22"="Querétaro","23"="Quintana Roo","24"="San Luis Potosí",
  "25"="Sinaloa","26"="Sonora","27"="Tabasco","28"="Tamaulipas",
  "29"="Tlaxcala","30"="Veracruz","31"="Yucatán","32"="Zacatecas"
)

# ------------------------------------------------------------------------------
# PREPARACIÓN DE DATOS (FORMATO LARGO)
# ------------------------------------------------------------------------------
g4_data <- base |>
  filter(
    sexo %in% c("Mujer", "Hombre"),
    !is.na(ent_ocurr),
    !is.na(mecanismo_agresion)
  ) |>
  mutate(
    ent_ocurr = stringr::str_pad(ent_ocurr, 2, pad = "0"),
    entidad   = cat_entidades[ent_ocurr]
  ) |>
  filter(!is.na(entidad)) |>
  count(entidad, sexo, mecanismo_agresion, name = "total") |>
  group_by(entidad, sexo) |>
  mutate(proporcion = total / sum(total)) |>
  ungroup()

# ------------------------------------------------------------------------------
# ORDEN CORRECTO DE ENTIDADES (SEGÚN ARMA DE FUEGO)
# ------------------------------------------------------------------------------
orden_entidades <- g4_data |>
  filter(mecanismo_agresion == "Arma de fuego") |>
  group_by(entidad) |>
  summarise(prop_af = sum(proporcion), .groups = "drop") |>
  arrange(prop_af)

g4_data_final <- g4_data |>
  mutate(
    entidad_plot = factor(entidad, levels = orden_entidades$entidad)
  )

# ------------------------------------------------------------------------------
# HEATMAP
# ------------------------------------------------------------------------------
g4_heatmap <- ggplot(
  g4_data_final,
  aes(
    x = sexo,
    y = entidad_plot,
    fill = proporcion
  )
) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ mecanismo_agresion, ncol = 2) +
  scale_fill_gradient(
    low  = "#F3E5F5",   # violeta claro (legible)
    high = "#4A148C",   # violeta oscuro (contraste fuerte)
    labels = scales::percent,
    name = "% del total"
  ) +
  labs(
    title = "Gráfica 4. Distribución territorial del mecanismo de agresión",
    subtitle = "Proporción de homicidios por entidad federativa y sexo",
    x = "Sexo",
    y = "Entidad federativa",
    caption = pie_de_foto
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

# ------------------------------------------------------------------------------
# IMPRIMIR Y GUARDAR
# ------------------------------------------------------------------------------
print(g4_heatmap)

ggsave(
  filename = file.path(ruta_salida, "g4_heatmap_mecanismo_entidad_sexo.png"),
  plot = g4_heatmap,
  width = 12,
  height = 10,
  dpi = 300
)

# ==============================================================================
# GRÁFICA 5 – ESPECIFICIDADES HOMICIDIOS MUJERES 2015-2023
# ==============================================================================


# ------------------------------------------------------------------
# 1. BASE SOLO MUJERES (2019–2023)
# ------------------------------------------------------------------

base_mujeres <- base |>
  dplyr::filter(
    sexo == "Mujer",
    anio_ocur >= 2019,
    anio_ocur <= 2023
  )

# ------------------------------------------------------------------
# 2. CREAR CATEGORÍAS ANALÍTICAS
# ------------------------------------------------------------------

base_mujeres <- base_mujeres |>
  dplyr::mutate(
    
    # --------------------------------------------------
    # LUGAR DE OCURRENCIA
    # --------------------------------------------------
    lugar_categoria = dplyr::case_when(
      contexto_violencia == "Vía pública"     ~ "Vía pública",
      contexto_violencia == "Espacio privado" ~ "Espacio privado",
      TRUE                                    ~ "Otro / No especificado"
    ),
    
    # --------------------------------------------------
    # RELACIÓN CON EL AGRESOR
    # --------------------------------------------------
    agresor_categoria = dplyr::case_when(
      par_agre %in% c(11,12,21,22,23,31,41,45,66,72) ~ "Familiar / Pareja",
      par_agre %in% c(51,52,53,54,55)               ~ "No familiar",
      TRUE                                          ~ "No especificado"
    )
  )

# ------------------------------------------------------------------
# 3. DATASET LARGO PARA MINI-GRÁFICAS
# ------------------------------------------------------------------

g5_data <- dplyr::bind_rows(
  
  base_mujeres |>
    dplyr::count(anio_ocur, categoria = lugar_categoria) |>
    dplyr::mutate(panel = "Lugar de ocurrencia"),
  
  base_mujeres |>
    dplyr::count(anio_ocur, categoria = agresor_categoria) |>
    dplyr::mutate(panel = "Relación con el agresor")
)

# ------------------------------------------------------------------
# 4. GRÁFICA
# ------------------------------------------------------------------

g5 <- ggplot(
  g5_data,
  aes(x = anio_ocur, y = n, color = categoria)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.3) +
  facet_wrap(~ panel, scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    
    # LUGAR
    "Vía pública"            = "#5E35B1",
    "Espacio privado"        = "#00897B",
    "Otro / No especificado" = "#9E9E9E",
    
    # AGRESOR
    "Familiar / Pareja" = "#C62828",  # rojo fuerte
    "No familiar"       = "#455A64",
    "No especificado"   = "#BDBDBD"
  )) +
  labs(
    title = "Gráfica 5. Violencia letal contra mujeres en México",
    subtitle = "Tendencias 2019–2023 por lugar de ocurrencia y relación con el agresor",
    x = "Año de ocurrencia",
    y = "Número de homicidios",
    color = "",
    caption = pie_de_foto
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.text = element_text(size = 9)
  )

# ------------------------------------------------------------------
# 5. IMPRIMIR Y GUARDAR
# ------------------------------------------------------------------

print(g5)

ggsave(
  filename = file.path(
    ruta_salida,
    "g5_violencia_mujeres_lugar_agresor_2019_2023.png"
  ),
  plot = g5,
  width = 11,
  height = 8,
  dpi = 300
)

message("✅ GRÁFICA 5 GENERADA Y GUARDADA CORRECTAMENTE")