library(tidyverse)
library(raster)
library(sf)
library(sp)
library(ggspatial)
library(ggplot2)
library(tidyr)
library(viridisLite)

setwd("~/Desktop/Curso de Mapas/Corregimientos")
shp <- st_read("Corregimientos_of_Panama.shp")

library(readxl)
Tabla <- read_excel("~/Desktop/Curso de Mapas/Data/CursoDeMapasExcel.xlsx")
View(Tabla)

shp %>%
  ggplot() +
  geom_sf()

aggregated_tbl <- Tabla %>%
  count(Corregimie)

pan_map_infl <- left_join(shp, aggregated_tbl, by = c("CORREGIMIE" = "Corregimie"))

pan_map_infl %>%
  ggplot() +
  geom_sf(aes(fill = n), color = "black") +
  scale_fill_viridis_c(alpha = 0.75) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title = "Distribucion de Casos de Tuberculosis",
       subtitle = "2022",
       fill = "Numbers of Cases")