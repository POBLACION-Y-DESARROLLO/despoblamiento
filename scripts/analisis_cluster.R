# 1. CARGA DE LIBRERIAS ---------------------------------------------------
library(tidyverse)
library(openxlsx)
library(tidymodels)
library(broom)
library(deckgl)
library(factoextra)
library(funModeling)
library(GGally)
library(DataExplorer)
library(ggcorrheatmap)

# 2. CARGA DE DATOS -------------------------------------------------------
despoblamiento <- read.xlsx("data/despoblacion.xlsx", sheet = 2) %>% 
  mutate(SUP_KM2 = ifelse(is.na(SUPERFICIE_KM2), 0, SUPERFICIE_KM2), 
         CAT_PCM = case_when(
             POB_TOT_2025 >= 51     & POB_TOT_2025 <= 500       ~ "Caserío Menor",
             POB_TOT_2025 >= 501    & POB_TOT_2025 <= 1000      ~ "Caserío Mayor",
             POB_TOT_2025 >= 1001   & POB_TOT_2025 <= 2000      ~ "Pueblo",
             POB_TOT_2025 >= 2001   & POB_TOT_2025 <= 5000      ~ "Villa",
             POB_TOT_2025 >= 5001   & POB_TOT_2025 <= 20000     ~ "Ciudad - Menor",
             POB_TOT_2025 >= 20001  & POB_TOT_2025 <= 100000    ~ "Ciudad - Intermedia",
             POB_TOT_2025 >= 100001 & POB_TOT_2025 <= 500000    ~ "Ciudad - Mayor",
             POB_TOT_2025 > 500000                         ~ "Metrópoli Regional",
             .default = NA)
  )

despoblamiento |> write.xlsx("salidas/despoblamienot_abs.xlsx")

despoblamiento |> df_status()


# 3. EXPLORACIÓN DE DATOS -------------------------------------------------


despoblamiento |> 
  group_by(DEPARTAMENTO, PROVINCIA) |>  
  count() |>  arrange((n))

# Normalizado de datos
despoblamiento_tidy <- despoblamiento |>
  select(
    UBIGEO,
    DEPARTAMENTO,
    PROVINCIA,
    DISTRITO,
    CAT_SDOT,
    CAT_PCM,
    CAT_VIDAL,
    VAR_ABS,
    VAR_PER,
    TASA_MIGRACION_NETA,
    TGF,
    TD_0_14,
    TD_60_MAS,
    TD_15_59,
    IVIA,
    POBREZA_2018,
    DENSIDAD,
    ALTITUD,
    PER_AGUA,
    PER_DESAGUE,
    PER_ELECTRICIDAD,
    PER_RURAL,
    SUP_KM2,
    IPRES,
    EDU_URB,
    EDU_RUR,
    EDU_TOT,
    DEMAGEGON
  ) |> 
  mutate(
    across(where(is.numeric), ~ as.numeric(scale(.)))
  )

despoblamiento_tidy |> write.xlsx("salidas/despoblamienot_std.xlsx")


despoblamiento_tidy |> view()
despoblamiento_tidy |> plot_correlation()
despoblamiento_tidy |> ggcorr(
                              geom = "tile",
                              nbreaks = 5,
                              label = TRUE,
                              label_size = 3,
                              color = "black",
                              label_color = 1,
                              hjust = 1,
                              palette = "PuOr"
                              )


despoblamiento_tidy %>%
  select(where(is.numeric)) %>%
  ggcorrhm(
    layout = "bottomright",
    cell_labels = TRUE,
    cell_label_col = "black",
    cell_label_size = 3,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    include_diag = FALSE,
    show_names_diag = TRUE,
    show_names_x = TRUE,
    show_names_y = FALSE   # 🔹 Oculta los nombres en el eje Y
  ) +
  scale_fill_viridis_c(option = "A", direction = -1) +
  ggtitle("Matriz de Correlaciones") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) # 🔹 Rotar etiquetas para mejor lectura
  )

# VARIBLES PREDICTORAS ----------------------------------------------------

despoblamiento_tidy_obj <- despoblamiento_tidy |> 
  transmute(
    UBIGEO = UBIGEO,
    TMN       = TASA_MIGRACION_NETA,
    TGF       = TGF,
    D_014     = TD_0_14,
    D_60      = TD_60_MAS,
    TC_1725   = TCAC_2017_2024,
    POBRE18   = POBREZA_2018,
    ALTI      = ALTITUD,
    DESAGÜE   = PER_DESAGUE,
    ELECTRI   = PER_ELECTRICIDAD,
    RURAL     = PER_RURAL,
    IPRES     = IPRES,
    EDU_TOT   = EDU_TOT,
    DEMAGE    = DEMAGEGON
       
  ) 

despoblamiento_tidy_obj |> select(where(is.numeric)) |> 
  ggcorrhm(
    layout = "topright",
    cell_labels = TRUE,
    cell_label_col = "black",
    cell_label_size = 3,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    include_diag = FALSE,
    show_names_diag = TRUE,
    show_names_x = T,
    show_names_y = F   # 🔹 Oculta los nombres en el eje Y
  ) +
  scale_fill_viridis_b(option = "E", direction = -1) +
  ggtitle("Matriz de Correlaciones") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) # 🔹 Rotar etiquetas para mejor lectura
  )

  

# 4. ELEGIR EL NÚMERO DE CLUSTERS -----------------------------------------

fviz_nbclust(despoblamiento_tidy_obj |> select(-UBIGEO), kmeans, method = "wss")
fviz_nbclust(despoblamiento_tidy_obj |> select(-UBIGEO), kmeans, method = "gap_stat")
# k = 6


# 4.1 IMPLEMENTAR LA AGRUPACIÓN EN CLUSTERS --------------------------------

set.seed(123)
k_means <- kmeans(despoblamiento_tidy_obj |> select(-UBIGEO),
                  centers = 3,  
                  nstart  = 20
)


fviz_cluster(k_means, data = despoblamiento_tidy_obj |> select(-UBIGEO))

# 4.2 AGREGAR LOS CLUSTER AL DATASET --------------------------------------
dependencia_cluster_fe <- dependencia_tidy %>%
  select(UBIGEO) %>%
  mutate(CLUSTER = k_means$cluster)

# Asignar nombres a los clusters
dependencia_cluster_fe <- dependencia_cluster_fe %>%
  mutate(CLUSTER_NAME = case_when(
    CLUSTER == 1 ~ "Moderada carga por vejez",
    CLUSTER == 2 ~ "Moderada carga por juventud, con poblacion activa",
    CLUSTER == 3 ~ "Dependencia moderada, equilibrado",
    CLUSTER == 4 ~ "Muy alta carga por juventud, fuerte poblacion activa",
    CLUSTER == 5 ~ "Muy alta vejez con débil soporte"
  ))

# Agregar al dataset original

dependencia_tidy_clust_fe <- dependencia%>% left_join(
  dependencia_cluster_fe, by = "UBIGEO"
) 

dependencia_tidy_clust_fe %>% 
  select(UBIGEO, PROVINCIA, DISTRITO, TDD_JOVE, TDD_VEJEZ, RA_POTENCIAL, RA_PADRES, CLUSTER_NAME) |> 
  group_by(CLUSTER_NAME) %>% summarise(
    CANTID = n_distinct(UBIGEO),
    TD_JOV = mean(TDD_JOVE),
    TD_VEJ = mean(TDD_VEJEZ),
    TD_POT = mean(RA_POTENCIAL),
    TD_PAD = mean(RA_PADRES),
  ) 



dependencia_tidy_clust_fe%>% write.xlsx("salidas/depedencia_cluster_fe.xlsx")