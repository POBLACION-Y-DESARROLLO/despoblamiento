# 1. CARGA DE LIBRERIAS ---------------------------------------------------
library(tidyverse)
library(openxlsx)
library(broom)
library(factoextra)
library(funModeling)
library(GGally)
library(DataExplorer)
library(ggcorrheatmap)
library(rstatix)
library(scales)
library(psych) " KMO"

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

# INDIENTIFICAR VALORES EXTREMOS PARA LA VARIACIÓN PORCENTUAL DE LA POBLACIÓN.
despoblamiento_outliers <- despoblamiento |> 
  identify_outliers(VAR_PER) |>
  select(UBIGEO,is.extreme)

# UBIR A LA BASE DE DATOS PRINCIPAL

despoblamiento <- despoblamiento |> 
  left_join(despoblamiento_outliers, by = c("UBIGEO" = "UBIGEO")) |> 
  mutate(
    is.extreme  = replace_na(is.extreme, FALSE)
  )

despoblamiento |> write.xlsx("salidas/despoblamiento_abs.xlsx")
despoblamiento |> df_status()



# 3. EXPLORACIÓN DE DATOS -------------------------------------------------


despoblamiento |> 
  filter(is.extreme != "TRUE") |> 
  group_by(DEPARTAMENTO, PROVINCIA) |>  
  count() |>  arrange((n))

# Normalizado de datos
despoblamiento_tidy <- despoblamiento |>
  filter(is.extreme!="TRUE") |> 
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
    across(where(is.numeric), ~ rescale(., to = c(0, 1)))
  )

#despoblamiento_tidy |> write.xlsx("salidas/despoblamienot_std.xlsx")


# CORRELACIONES ENTRE VARIBABLES
despoblamiento_tidy |> view()
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

# VARIBLES OBEJTIVO A USAR

# Se usa en el modelo 19 variables 

despoblamiento_tidy_acp <- despoblamiento_tidy |> 
  transmute(
    UBIGEO    = UBIGEO,
    VAR_PER   = VAR_PER,
    TMN       = TASA_MIGRACION_NETA,
    TGF       = TGF,
    D_014     = TD_0_14,
    D_60      = TD_60_MAS,
    #TD_15_59  = TD_15_59,
    #IVIA      = IVIA,
    POBRE18   = POBREZA_2018,
    ALTI      = ALTITUD,
    AGUA      = PER_AGUA,
    DESAGÜE   = PER_DESAGUE,
    ELECTRI   = PER_ELECTRICIDAD,
    RURAL     = PER_RURAL,
    #SUP_KM2   = SUP_KM2,
    #IPRES     = IPRES,
    #EDU_RUR   = EDU_URB,
    #EDU_RUR   = EDU_RUR,
    #EDU_TOT   = EDU_TOT,
    #DEMAGE    = DEMAGEGON
  ) 

# 4. ANÁLISIS DE COMPONENTES PRINCIPALES ----------------------------------

# Prueba de KMO
corr_matrix <- cor(despoblamiento_tidy_acp |> select(-UBIGEO), use = "pairwise.complete.obs")
# Determinante de la matriz
det(corr_matrix)

# Prueba de esfericidad de Barlet
kmo <- KMO(corr_matrix)

# Prueba de Bartlett
cortest.bartlett(corr_matrix, n = nrow(despoblamiento_tidy_acp))

# Matriz antimagen y diagonal
kmo$MSAi # Me quedo con las mayores de 60
kmo$Image |> view() # Hay buenas correlacioens paraciales cercacnas a cero.



# Calculo del ACP
acp_result <- principal(despoblamiento_tidy_acp |> select(-UBIGEO),
                        nfactors = ncol(despoblamiento_tidy_acp |> select(-UBIGEO)), # Extraigo 11 factores igual que el numero de varibales
                        rotate = "varimax") 

acp_result

# Gráfico de sedimentación
scree(acp_result$Vaccounted, factors = 11, pc = T)


acp_result <- principal(despoblamiento_tidy_acp |> select(-UBIGEO),
                        nfactors = 3, # me quedo con 3
                        rotate = "varimax") 

acp_result

# Extraer los puntajes factoriales
scores <- acp_result$scores 


# VARIBLES OBEJETIVO -------------------------------------------
despoblamiento_tidy_obj <- despoblamiento |>
  filter(is.extreme!="TRUE") |> 
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
  )


despoblamiento_tidy_obj_scores <- despoblamiento_tidy_obj |> 
  bind_cols(as.data.frame(scores))
























