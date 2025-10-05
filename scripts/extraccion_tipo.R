p_load(dplyr, stringr, stringi, stringdist)


# --- Normalizar texto ---
norm_txt <- function(x) x |>
  str_to_lower() |>
  stringi::stri_trans_general("Latin-ASCII") |>
  str_squish()

# --- Léxico ---
lexicon <- list(
  "Apartamento/Apartaestudio" = c("apartamento","apto","apartment","depto","apartamentos",
                                  "apartaestudio","apartestudio","apart a estudio","studio","estudio"),
  "Casa"         = c("casa","townhouse","casa campestre","casa lote","finca"),
  "Dúplex"       = c("duplex","dúplex"),
  "Tríplex"      = c("triplex","tripex","tríplex"),
  "Penthouse"    = c("penthouse","pent house","ph"),
  "Habitación"   = c("habitacion","cuarto"),
  "Oficina"      = c("oficina","office"),
  "Local"        = c("local","local comercial","tienda"),
  "Bodega"       = c("bodega","warehouse"),
  "Lote/Terreno" = c("lote","terreno","parcela"),
  "Edificio"     = c("edificio","building"),
  "Consultorio"  = c("consultorio"),
  "Parqueadero"  = c("parqueadero")
)
lexicon_norm <- lapply(lexicon, norm_txt)

# Orden deseado (vivienda primero, luego otros)
ordered_cats <- c("Apartamento/Apartaestudio","Casa","Dúplex","Tríplex","Penthouse","Habitación",
                  "Oficina","Local","Bodega","Lote/Terreno","Edificio","Consultorio","Parqueadero")

# --- Fuzzy helper ---
fuzzy_hit <- function(text, keywords, max_dist = 2) {
  tokens <- unlist(str_split(text, "\\W+"))
  any(vapply(keywords, function(k) {
    any(stringdist(tokens, k, method = "lv") <= max_dist) ||
      stringdist(text, k, method = "lv") <= max_dist + 1
  }, logical(1)))
}

# --- Clasificador (regex en orden; si no, fuzzy en el mismo orden) ---
clasifica_tipo <- function(title_raw) {
  t <- norm_txt(title_raw)
  # 1) Regex
  exact <- dplyr::case_when(
    str_detect(t, "\\b(apartamento|apto|apartment|depto|apartamentos?)\\b") |
      str_detect(t, "\\b(aparta\\s*estudio|apart(a)?estudio|studio|estudio)\\b") ~ "Apartamento/Apartaestudio",
    str_detect(t, "\\b(casa(\\s+campestre)?|townhouse|casa\\s+lote|finca)\\b") ~ "Casa",
    str_detect(t, "\\b(duplex|d[uú]plex)\\b") ~ "Dúplex",
    str_detect(t, "\\b(triplex|tr[ií]plex|tripex)\\b") ~ "Tríplex",
    str_detect(t, "\\b(pent\\s*house|penthouse|\\bph\\b)\\b") ~ "Penthouse",
    str_detect(t, "\\b(habitacion(es)?|cuarto(s)?)\\b") ~ "Habitación",
    str_detect(t, "\\b(oficina|office)\\b") ~ "Oficina",
    str_detect(t, "\\b(local(es)?|local\\b|tienda\\b)\\b") ~ "Local",
    str_detect(t, "\\b(bodega(s)?|warehouse)\\b") ~ "Bodega",
    str_detect(t, "\\b(lote|terreno|parcela)\\b") ~ "Lote/Terreno",
    str_detect(t, "\\b(edificio|building)\\b") ~ "Edificio",
    str_detect(t, "\\b(consultorio)\\b") ~ "Consultorio",
    str_detect(t, "\\b(parqueadero)\\b") ~ "Parqueadero",
    TRUE ~ NA_character_
  )
  if (!is.na(exact)) return(exact)
  # 2) Fuzzy
  for (tipo in ordered_cats) {
    if (fuzzy_hit(t, lexicon_norm[[tipo]], max_dist = 2)) return(tipo)
  }
  "Otro/Indeterminado"
}

# --- Aplicación y agrupación "Comercial" ---

comercial_cats <- c("Oficina","Local","Bodega","Edificio","Consultorio","Lote/Terreno","Habitación","Parqueadero")

housing_data <- housing_data %>%
  mutate(
    tipo_detalle = vapply(title, clasifica_tipo, character(1)),
    tipo = if_else(tipo_detalle %in% comercial_cats, "Comercial", tipo_detalle),
    tipo = factor(tipo,
                  levels = c("Apartamento/Apartaestudio","Casa","Dúplex","Tríplex","Penthouse",
                             "Comercial","Otro/Indeterminado"))
  )

# --- Chequeo rápido ---

dplyr::count(housing_data, tipo, sort = TRUE)
