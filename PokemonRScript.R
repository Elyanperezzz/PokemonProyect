library(dplyr)
library(stringr)
library(readr)

# 1. Carga y estandarización
pokemon_raw <- read_csv("C:/Users/perez.elyanramon/Downloads/Pokemon.csv", show_col_types = FALSE)

pokemon <- pokemon_raw %>%
  rename(
    pokedex_id = `#`,
    name       = Name,
    type_1     = `Type 1`,
    type_2     = `Type 2`,
    total      = Total,
    hp         = HP,
    attack     = Attack,
    defense    = Defense,
    sp_atk     = `Sp. Atk`,
    sp_def     = `Sp. Def`,
    speed      = Speed,
    generation = Generation,
    legendary  = Legendary
  ) %>%
  mutate(
    type_2       = if_else(is.na(type_2), "None", type_2),
    base_name    = str_extract(name, "^[A-Z][a-zA-Z0-9.'-]+"),
    form         = str_trim(str_remove(name, fixed(base_name))),
    form         = if_else(form == "", NA_character_, form),
    is_main_form = !duplicated(pokedex_id)
  )

# 2. Detección de outliers (1.5 * IQR)
cols_stats <- c("hp", "attack", "defense", "sp_atk", "sp_def", "speed")

es_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

pokemon <- pokemon %>%
  mutate(
    across(all_of(cols_stats), es_outlier, .names = "out_{.col}"),
    n_outlier_stats = rowSums(across(starts_with("out_"))),
    has_outlier = n_outlier_stats > 0
  ) %>%
  select(-starts_with("out_"))

tipos <- sort(unique(pokemon$type_1))

# 3. Variables derivadas
pokemon_final <- pokemon %>%
  mutate(
    dual_type = type_2 != "None",
    form_category = case_when(
      is.na(form)                 ~ "Base",
      str_detect(form, "^Mega")   ~ "Mega",
      str_detect(form, "^Primal") ~ "Primal",
      TRUE                        ~ "Alternate form"
    ),
    offense = attack + sp_atk,
    bulk    = hp + defense + sp_def,
    attacker_style = case_when(
      attack > sp_atk ~ "Physical",
      attack < sp_atk ~ "Special",
      TRUE            ~ "Balanced"
    ),
    power_tier = case_when(
      total < 300 ~ "Very low",
      total < 400 ~ "Low",
      total < 500 ~ "Medium",
      total < 600 ~ "High",
      TRUE        ~ "Elite"
    )
  )

# 4. Codificación de factores
pokemon_final <- pokemon_final %>%
  mutate(
    type_1         = factor(type_1, levels = tipos),
    type_2         = factor(type_2, levels = c(tipos, "None")),
    generation     = factor(generation, levels = 1:6, ordered = TRUE),
    form_category  = factor(form_category,
                            levels = c("Base", "Mega", "Primal", "Alternate form")),
    attacker_style = factor(attacker_style,
                            levels = c("Physical", "Balanced", "Special")),
    power_tier     = factor(power_tier,
                            levels = c("Very low", "Low", "Medium", "High", "Elite"),
                            ordered = TRUE)
  )

# 5. Selección y ordenación final
pokemon_final <- pokemon_final %>%
  select(
    pokedex_id, name, base_name, form, form_category, is_main_form,
    type_1, type_2, dual_type, generation, legendary,
    all_of(cols_stats), total,
    offense, bulk, attacker_style, power_tier,
    n_outlier_stats, has_outlier
  ) %>%
  arrange(pokedex_id)

# 6. Verificaciones y resumen
cat("Dimensiones:", nrow(pokemon_final), "filas x", ncol(pokemon_final), "columnas\n")

stopifnot(all(colSums(is.na(select(pokemon_final, -form))) == 0))
cat("Sin NA en ninguna columna salvo 'form' (NA = sin forma alternativa): OK\n\n")

glimpse(pokemon_final)

cat("\nPokémon por categoría de forma:\n")
print(count(pokemon_final, form_category))

cat("\nPokémon por nivel de poder:\n")
print(count(pokemon_final, power_tier))

resumen_tipo <- pokemon_final %>%
  filter(is_main_form) %>%
  group_by(type_1) %>%
  summarise(
    n = n(),
    across(all_of(c(cols_stats, "total")), ~ round(mean(.x), 1), .names = "mean_{.col}"),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_total))

cat("\nMedia de stats por tipo principal (una fila por especie):\n")
print(resumen_tipo, n = Inf)

# 7. Exportación
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(pokemon_final, "data/processed/03_pokemon_final.rds")
write_csv(pokemon_final, "data/processed/03_pokemon_final.csv")

cat("\nGuardado: data/processed/03_pokemon_final.rds y .csv\n")