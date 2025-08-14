library(tidyverse)
library(spa)

# 1. CARGA DE DATOS -------------------------------------------------------

ipres <- read_csv2("data/RENIPRESS_2025_v4.csv", locale = locale(encoding = "Latin1"))

ipres |> 
  filter(ESTADO == "ACTIVO" & CATEGORIA %in% c("I-1", "I-2"))|> 
  group_by(UBIGEO,
           DEPARTAMENTO,
           PROVINCIA,
           DISTRITO
           ) |> 
  summarise(
    IPRESIII = n_distinct(COD_IPRESS)
  ) |>  write.xlsx("salidas/ipres.xlsx")
  

# Cantidad de instituciones educativas 
educacion <- read.xlsx("data/Padron_web.xlsx")


#Urbana,

educacion |> 
  filter(
    D_ESTADO == "Activa" & DAREACENSO == "Urbana") |> 
  group_by(
    CODGEO,
    D_DPTO,
    D_PROV,
    D_DIST
  ) |> 
  mutate(id = paste(CODLOCAL,CEN_EDU)) |> 
  summarise(
    CANT_EDU = n_distinct(id)
  ) |> write.xlsx("salidas/cant_educactivos_urbana.xlsx")


#Rural,

educacion |> 
  filter(
    D_ESTADO == "Activa" & DAREACENSO == "Rural") |> 
  group_by(
    CODGEO,
    D_DPTO,
    D_PROV,
    D_DIST
  ) |> 
  mutate(id = paste(CODLOCAL,CEN_EDU)) |> 
  summarise(
    CANT_EDU = n_distinct(id)
  ) |> write.xlsx("salidas/cant_educactivos_rural.xlsx")
