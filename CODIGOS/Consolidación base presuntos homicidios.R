# ==============================================================================
# CONSOLIDACIÓN Y LIMPIEZA DE HOMICIDIOS (2015–2023)
# ==============================================================================

# 1. PAQUETES
# =========================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, foreign, janitor, stringr, purrr, rlang, readr
)
# =========================

# 2. RUTAS ABSOLUTAS
# =========================
ruta_base <- "C:/MIS DOCUMENTOS/DATA CIVICA/Estad-sticas-de-Defunciones-INEGI"
ruta_origen  <- file.path(ruta_base, "Data")
ruta_destino <- file.path(ruta_base, "Salida")
dir.create(ruta_destino, showWarnings = FALSE, recursive = TRUE)
# =========================

# 3. CIE-10 — HOMICIDIOS
# =========================
cie10_homicidios_inicio <- "X85"
cie10_homicidios_fin    <- "Y09"

vars_interes <- c(
  "ent_ocurr", "mun_ocurr",
  "anio_ocur", "anio_ocurr", "anio_regis",
  "causa_def", "lista_mex",
  "tipo_defun", "sitio_ocur", "vio_fami", "par_agre",
  "sexo", "edad", "conindig", "afromex", "nacionalid"
)
# =========================

# 4. FUNCIÓN DE LECTURA Y LIMPIEZA
# =========================
leer_defunciones <- function(archivo_dbf) {
  
  message("Procesando: ", basename(archivo_dbf))
  
  df <- foreign::read.dbf(archivo_dbf, as.is = TRUE) |>
    as_tibble() |>
    janitor::clean_names()
  
  # --- NORMALIZACIÓN AÑO DE OCURRENCIA ---
  if ("anio_ocurr" %in% names(df) & !"anio_ocur" %in% names(df)) {
    df <- df |> rename(anio_ocur = anio_ocurr)
  }
  
  # --- FIX ROBUSTO PARA SEXO ---
  posibles_sexo <- names(df)[str_detect(names(df), "sex|sx")]
  if (!"sexo" %in% names(df)) {
    if (length(posibles_sexo) == 1) {
      df <- df |> rename(sexo = all_of(posibles_sexo))
    } else {
      df <- df |> mutate(sexo = NA)
    }
  }
  
  # --- CREAR VARIABLES FALTANTES ---
  faltantes <- setdiff(vars_interes, names(df))
  if (length(faltantes) > 0) {
    df <- df |> mutate(
      !!!setNames(rep(list(NA), length(faltantes)), faltantes)
    )
  }
  
  # --- TIPOS DE DATOS ---
  df <- df |>
    select(all_of(vars_interes)) |>
    mutate(
      anio_ocur   = suppressWarnings(as.integer(anio_ocur)),
      anio_regis  = suppressWarnings(as.integer(anio_regis)),
      sexo        = suppressWarnings(as.numeric(sexo)),
      edad        = suppressWarnings(as.numeric(edad)),
      sitio_ocur  = suppressWarnings(as.numeric(sitio_ocur)),
      vio_fami    = suppressWarnings(as.numeric(vio_fami)),
      par_agre    = suppressWarnings(as.numeric(par_agre)),
      conindig    = suppressWarnings(as.numeric(conindig)),
      afromex    = suppressWarnings(as.numeric(afromex)),
      nacionalid = suppressWarnings(as.numeric(nacionalid)),
      causa_def  = as.character(causa_def),
      ent_ocurr  = as.character(ent_ocurr),
      mun_ocurr  = as.character(mun_ocurr)
    )
  
  # --- FILTRO: HOMICIDIOS Y PERIODO ANALÍTICO ---
  df |>
    filter(
      !is.na(causa_def),
      causa_def >= cie10_homicidios_inicio,
      causa_def <= cie10_homicidios_fin,
      anio_ocur >= 2015,
      anio_ocur <= 2023
    )
}
# =========================

# 5. ARCHIVOS DBF
# =========================
archivos_dbf <- list.files(
  ruta_origen,
  pattern = "^defun.*\\.dbf$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

stopifnot(length(archivos_dbf) > 0)
message("Archivos encontrados: ", length(archivos_dbf))
# =========================

# 6. BASE MAESTRA
# =========================
base_homicidios <- purrr::map_dfr(archivos_dbf, leer_defunciones)
# =========================

# 7. VARIABLES ANALÍTICAS
# =========================
base_homicidios <- base_homicidios |>
  mutate(
    # Sexo
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ "Otro / No especificado"
    ),
    
    # Edad en años
    edad_anos = case_when(
      edad >= 4001 & edad <= 4120 ~ edad - 4000,
      TRUE ~ NA_real_
    ),
    
    # Mecanismo de agresión
    mecanismo_agresion = case_when(
      str_detect(causa_def, "^X9[3-5]|^Y2[0-4]") ~ "Arma de fuego",
      str_detect(causa_def, "^X9[6-7]") ~ "Arma blanca",
      TRUE ~ "Otro mecanismo"
    ),
    
    # Poblaciones de interés
    poblacion_vulnerable = case_when(
      conindig == 1 ~ "Persona indígena",
      afromex == 1 ~ "Persona afromexicana",
      nacionalid != 1 ~ "Persona migrante",
      TRUE ~ "No especificado"
    ),
    
    # Sitio de ocurrencia (incluye NK)
    contexto_violencia = case_when(
      sitio_ocur %in% c(11, 12, 13) ~ "Espacio privado",
      sitio_ocur == 10 ~ "Vía pública",
      sitio_ocur %in% c(98, 99) ~ "No especificado (NK)",
      is.na(sitio_ocur) ~ "No registrado",
      TRUE ~ "Otro sitio"
    ),
    
    # Violencia familiar
    violencia_familiar = case_when(
      vio_fami == 1 ~ "Sí",
      vio_fami == 2 ~ "No",
      TRUE ~ "No especificado"
    ),
    
    # Clave geoestadística
    cve_geo = paste0(
      str_pad(ent_ocurr, 2, pad = "0"),
      str_pad(mun_ocurr, 3, pad = "0")
    )
  ) |>
  select(
    anio_ocur, anio_regis,
    cve_geo, ent_ocurr, mun_ocurr,
    sexo, edad_anos,
    mecanismo_agresion, par_agre,
    poblacion_vulnerable,
    contexto_violencia,
    violencia_familiar,
    causa_def
  )
# =========================

# 8. VALIDACIONES
# =========================
message("Total de homicidios:", nrow(base_homicidios))
print(table(base_homicidios$sexo, useNA = "ifany"))
print(range(base_homicidios$anio_ocur, na.rm = TRUE))
# =========================

# 9. GUARDADO
# =========================
write_csv(
  base_homicidios,
  file.path(ruta_destino, "homicidios_consolidados_2015_2023_ANALISIS.csv")
)

saveRDS(
  base_homicidios,
  file.path(ruta_destino, "base_homicidios_2015_2023_ANALISIS.rds")
)

message("Proceso terminado correctamente.")
