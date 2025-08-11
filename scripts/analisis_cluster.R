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

# 2. CARGA DE DATOS -------------------------------------------------------
despoblamiento <- read.xlsx("data/despoblacion.xlsx")
despoblamiento |> df_status()


# 3. EXPLORACIÓN DE DATOS -------------------------------------------------


despoblamiento |> 
  group_by(DEPARTAMENTO, PROVINCIA) |>  
  count() |>  arrange((n))

# Normalizado de datos
despoblamiento_tidy <- despoblamiento |>
  select(
    UBIGEO,
    #DEPARTAMENTO,
    #PROVINCIA,
    #DISTRITO,
    TASA_MIGRACION_NETA,
    TGF,
    TD_0_14,
    TD_60_MAS,
    TD_15_59,
    TCAC_2017_2024,
    IVIA,
    POBREZA_2018,
    #DENSIDAD,
    ALTITUD,
    PER_AGUA,
    PER_DESAGUE,
    PER_ELECTRICIDAD
  ) |> 
  mutate(
    across(where(is.numeric), ~ as.numeric(scale(.)))
  )

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
                              palette = "PuOr" )



# 4. ELEGIR EL NÚMERO DE CLUSTERS -----------------------------------------

fviz_nbclust(despoblamiento_tidy |> select(-UBIGEO), kmeans, method = "wss")
fviz_nbclust(despoblamiento_tidy |> select(-UBIGEO), kmeans, method = "gap_stat")
# k = 6


# 4.1 IMPLEMENTAR LA AGRUPACIÓN EN CLUSTERS --------------------------------

set.seed(123)
k_means <- kmeans(despoblamiento_tidy |> select(-UBIGEO),
                  centers = 6,  
                  nstart  = 20
)


fviz_cluster(k_means, data = despoblamiento_tidy |> select(-UBIGEO))

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