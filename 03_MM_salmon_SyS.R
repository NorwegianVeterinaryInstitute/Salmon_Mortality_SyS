### 03_MM_salmon_SyS ###
# Tables and Figures used in the Material and Methods section of the manuscript.
# Contributors: Victor H. S. Oliveira, Fernanda C. Dórea, Katharine R. Dean, Britt Bang Jensen

library(here)
#source(here("02_performance_salmonSyS.R"))

# Packages ----

library(tmap)
library(sf)

# Table 1 ----
# Title: Overview of data used for developing and evaluating performance of a syndromic surveillance system in aquaculture.
# Please, see manuscript. 

# Figure 1 ----
# Title: Location of the 13 production areas and farms licensed to produce Atlantic salmon for food consumption between 2014 and 2021 in Norway. This study considered the production areas 3−6, highlighted in blue, where PD is endemic. 

df_sys_no_exclusions <- readRDS(here("data", "df_sys_no_exclusions.rds"))

df_map <- st_as_sf(df_sys_no_exclusions %>%
                     group_by(location) %>%
                     slice(1) %>%
                     ungroup() %>%
                     filter(!is.na(n_geowgs84)) %>%
                     mutate(dot = "Salmon farm") %>%
                     dplyr::select(location, dot, e_geowgs84, n_geowgs84),
                   coords=c("e_geowgs84", "n_geowgs84"), crs=4326)

Coastline <- read_sf(dsn= here("data", "map_data"),
                     layer="Hav_norge")

Norway <- read_sf(dsn= here("data", "map_data"),
                  layer="N500_NorgeLand")

Production_areas <- read_sf(dsn=here("data", "map_data"),
                            layer=paste("Produksjon_omraader")) %>%
  mutate(color = NA) %>%
  sf::st_as_sf()

Production_areas_3_6 <- read_sf(dsn=here("data", "map_data"),
                                layer=paste("Produksjon_omraader")) %>%
  dplyr::mutate(color = case_when(id %in% c(3,4,5,6) ~ "#6fb2d3",
                                  TRUE ~ as.character(NA))) %>%
  sf::st_as_sf()

map_PA_3_6 <- tm_shape(Coastline, bbox = st_bbox(Production_areas_3_6)) +
  tm_fill() +
  tm_shape(Norway) +
  tm_borders(lwd=0.5) +
  tm_shape(df_map) +
  tm_dots(title = "",
          shape = 21,
          size = 0.15,
          col = "dot",
          palette= "black") +
  tm_legend(text.size=1.2) +
  tm_shape(Production_areas_3_6) +
  tm_borders(lty = 2) +
  tm_fill(col = "color", alpha = 0.4) +
  tm_shape(Production_areas_3_6 %>% dplyr::filter(id == 1)) +
  tm_text("id",
          ymod = -1.75,
          xmod = -1,
          size = 1.2,
          fontface = "bold", 
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 2)) +
  tm_text("id",
          ymod = -0.25,
          xmod = -1.5,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% dplyr::filter(id == 3)) +
  tm_text("id",
          ymod = -0.2,
          xmod = -1.2,
          size = 1.2,
          fontface = "bold", 
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 4)) +
  tm_text("id",
          ymod = .75,
          xmod = -1.5,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 5)) +
  tm_text("id",
          ymod = .8,
          xmod = -.8,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 6)) +
  tm_text("id",
          ymod = 1,
          xmod = -.8,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 7)) +
  tm_text("id",
          ymod = .8,
          xmod = -1.2,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 8)) +
  tm_text("id",
          ymod = 0,
          xmod = -1.15,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 9)) +
  tm_text("id",
          ymod = 1.,
          xmod = -.6,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 10)) +
  tm_text("id",
          ymod = 1.2,
          xmod = -.5,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 11)) +
  tm_text("id",
          ymod = 1.,
          xmod = -.7,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 12)) +
  tm_text("id",
          ymod = 1.,
          xmod = -.7,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_shape(Production_areas_3_6 %>% filter(id == 13)) +
  tm_text("id",
          ymod = 1.1,
          xmod = 0,
          size = 1.2,
          fontface = "bold",
          col = "#000000") +
  tm_scale_bar(position = c("right", "bottom"), width = 0.3,breaks = c(0, 100, 200, 300, 400), text.size = 1) +
  tm_add_legend(title = "",
                type = "fill",
                label = "Production areas 3-6",
                col = "#6fb2d3",
                alpha = 0.4) +
  tm_layout(legend.position = c("right", "center"),
            frame = TRUE) +
  tm_compass(position = c("left", "top"), size = 3) 

tmap_save(map_PA_3_6,
          dpi = 600,
          height = 9,
          width = 7.25,
          filename = here("figures","PA_3_6.jpg"))

# Figure 2 ----
# Title: Workflow for the syndromic salmon mortality surveillance system.  
# Please, see manuscript. 

# Table 2 ----
# Fitted models used for parametrization of the proposed syndromic surveillance system. 
# See "02_performance_salmon_SyS" with further information for building this table.
print(training.model_vector)

# Figure 3 ----
# Schematic representation of production months and fish cohorts considered positive or negative for pancreas disease (PD) using the proposed syndromic surveillance system for outbreak alarms. 
# Please, see manuscript. 

