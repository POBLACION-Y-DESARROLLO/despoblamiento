
# 1. ELEGIR EL NÚMERO DE CLUSTERS OPTIMO --------------------------------------

fviz_nbclust(despoblamiento_tidy_obj_scores |> select(RC1, RC2, RC3), kmeans, method = "wss")
fviz_nbclust(despoblamiento_tidy_obj_scores |> select(RC1, RC2, RC3), kmeans, method = "gap_stat")
# k = 4


# 4.1 IMPLEMENTAR LA AGRUPACIÓN EN CLUSTERS --------------------------------

set.seed(2025)
k_means <- kmeans(despoblamiento_tidy_obj_scores |> select(RC1, RC2, RC3),
                  centers = 4,  
                  nstart  = 20
)


fviz_cluster(k_means, data = despoblamiento_tidy_obj_scores |> select(RC1, RC2, RC3))

# 4.2 AGREGAR LOS CLUSTER AL DATASET --------------------------------------
despoblamiento_cluster_fe <- despoblamiento_tidy_obj_scores %>%
  mutate(CLUSTER = k_means$cluster)


# Asignar nombres a los clusters
despoblamiento_cluster_fe <- despoblamiento_cluster_fe %>%
  mutate(CLUSTER_NAME = case_when(
    CLUSTER == 1 ~ "Distritos intermedios, ni muy urbanos ni muy rurales, algo de infraestructura",
    CLUSTER == 2 ~ "Distritos rurales y despoblados, poca infraestructur",
    CLUSTER == 3 ~ "Distritos con fuerte despoblamiento, baja urbanización, población joven",
    CLUSTER == 4 ~ "Distritos urbanos con buena infraestructura y población estable o creciente",
  ))


despoblamiento_cluster_fe %>% 
  group_by(CLUSTER_NAME) %>% summarise(
    CANTID  = n_distinct(UBIGEO),
    AGUA    = mean(PER_AGUA),
    DESAGÜE = mean(PER_DESAGUE),
    ELECTRI = mean(PER_ELECTRICIDAD),
    RURAL   = mean(PER_RURAL),
    VAR_PER = mean(VAR_PER),
    POBRE18 = mean(POBREZA_2018),
    TMN     = mean(TASA_MIGRACION_NETA), 
    ALTI    = mean(ALTITUD),
    TGF     = mean(TGF),
    D_014   = mean(TD_0_14),
    D_60   = mean(TD_60_MAS)
  ) |> view()



despoblamiento_cluster_fe%>% write.xlsx("salidas/despoblamiento_cluster_fe.xlsx")


# GRAFICACO EL ACP Y LSO CLUSTERS -----------------------------------------

library(plotly)
scores <- despoblamiento_cluster_fe |> 
  select(CLUSTER,RC1, RC2, RC3) |> 
  mutate(cluster = as.factor(CLUSTER))

# Gráfico 3D interactivo
fig <- plot_ly(
  data = scores,
  x = ~RC1,
  y = ~RC2,
  z = ~RC3,
  color = ~cluster,
  colors = c("red", "blue", "green", "purple"),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
) |> layout(
  scene = list(
    xaxis = list(title = "RC1 (Infraestructura/Urbanización)"),
    yaxis = list(title = "RC2 (Despoblamiento/Pobreza)"),
    zaxis = list(title = "RC3 (Estructura Demográfica)")
  ),
  legend = list(title = list(text='Cluster'))
)

fig
