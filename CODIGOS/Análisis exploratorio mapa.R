# ==============================================================================
# MAPAS — HOMICIDIOS DE MUJERES POR ENTIDAD (2020–2023)
# ==============================================================================

library(sf)
library(tidyverse)
library(scales)

# ------------------------------------------------------------------
# 1. AÑOS A MAPEAR
# ------------------------------------------------------------------

anios_mapa <- c(2020, 2021, 2022, 2023)

# ------------------------------------------------------------------
# 2. SHAPEFILE — MARCO GEOESTADÍSTICO INEGI
# ------------------------------------------------------------------

ruta_shp <- "C:/MIS DOCUMENTOS/DATA CIVICA/Estad-sticas-de-Defunciones-INEGI/MarcoGeo/conjunto_de_datos"

entidades_sf <- st_read(
  file.path(ruta_shp, "00ent.shp"),
  quiet = TRUE
) |>
  mutate(
    ent_ocurr = stringr::str_pad(CVE_ENT, 2, pad = "0")
  )

# ------------------------------------------------------------------
# 3. LOOP PARA GENERAR UN MAPA POR AÑO
# ------------------------------------------------------------------

for (anio in anios_mapa) {
  
  # --------------------------------------------------
  # BASE: MUJERES POR AÑO
  # --------------------------------------------------
  
  base_mujeres_anio <- base |>
    filter(
      sexo == "Mujer",
      anio_ocur == anio,
      !is.na(ent_ocurr)
    ) |>
    mutate(
      ent_ocurr = stringr::str_pad(ent_ocurr, 2, pad = "0")
    ) |>
    count(ent_ocurr, name = "homicidios")
  
  # --------------------------------------------------
  # UNIR CON SHAPE
  # --------------------------------------------------
  
  mapa_data <- entidades_sf |>
    left_join(base_mujeres_anio, by = "ent_ocurr") |>
    mutate(
      homicidios = replace_na(homicidios, 0)
    )
  
  # --------------------------------------------------
  # MAPA
  # --------------------------------------------------
  
  mapa <- ggplot(mapa_data) +
    geom_sf(
      aes(fill = homicidios),
      color = "white",
      linewidth = 0.25
    ) +
    scale_fill_gradient(
      low = "#F3E5F5",
      high = "#4A148C",
      labels = comma,
      name = "Homicidios\nde mujeres"
    ) +
    labs(
      title = glue::glue("Homicidios de mujeres en México, {anio}"),
      subtitle = "Defunciones por homicidio según entidad federativa",
      caption = glue::glue(
        "Fuente: Elaboración propia con datos de INEGI. Año de ocurrencia: {anio}."
      )
    ) +
    theme_void() +
    theme(
      plot.title.position = "plot",
      legend.position = "right"
    )
  
  # --------------------------------------------------
  # IMPRIMIR Y GUARDAR
  # --------------------------------------------------
  
  print(mapa)
  
  ggsave(
    filename = file.path(
      ruta_salida,
      glue::glue("mapa_homicidios_mujeres_{anio}.png")
    ),
    plot = mapa,
    width = 9,
    height = 7,
    dpi = 300
  )
}

message("✅ Mapas 2020–2023 generados y guardados correctamente")

# ==============================================================================
# TABLA — HOMICIDIOS DE MUJERES POR ENTIDAD FEDERATIVA (2020–2023)
# ==============================================================================

# Años de interés
anios_tabla <- c(2020, 2021, 2022, 2023)

# ------------------------------------------------------------------
# 1. BASE: SOLO MUJERES Y AÑOS DE INTERÉS
# ------------------------------------------------------------------

tabla_base <- base |>
  filter(
    sexo == "Mujer",
    anio_ocur %in% anios_tabla,
    !is.na(ent_ocurr)
  ) |>
  mutate(
    ent_ocurr = stringr::str_pad(ent_ocurr, 2, pad = "0"),
    entidad = cat_entidades[ent_ocurr]
  ) |>
  filter(!is.na(entidad))

# ------------------------------------------------------------------
# 2. CONTEO POR ENTIDAD Y AÑO
# ------------------------------------------------------------------

tabla_conteo <- tabla_base |>
  count(entidad, anio_ocur, name = "homicidios")

# ------------------------------------------------------------------
# 3. PASAR A FORMATO ANCHO (UNA COLUMNA POR AÑO)
# ------------------------------------------------------------------

tabla_final <- tabla_conteo |>
  tidyr::pivot_wider(
    names_from = anio_ocur,
    values_from = homicidios,
    values_fill = 0
  ) |>
  arrange(desc(2023))

# ------------------------------------------------------------------
# 4. IMPRIMIR TABLA
# ------------------------------------------------------------------

print(tabla_final)

# ------------------------------------------------------------------
# 5. GUARDAR TABLA
# ------------------------------------------------------------------

write_csv(
  tabla_final,
  file.path(
    ruta_salida,
    "tabla_homicidios_mujeres_entidad_2020_2023.csv"
  )
)

message("✅ Tabla de homicidios de mujeres 2020–2023 generada y guardada")




