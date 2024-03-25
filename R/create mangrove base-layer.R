
library(dggridR)
library(sf)
library(tidyverse)

theme_set(theme_bw())

sf_lc2021 <- st_read("data-raw/confidential/Landcover/PL_2021_JUNI2022_am.shp")
fra_mg    <- read_csv("data-raw/CEO_projectsDBs_csv_04102022_ALLprojects_.csv", col_types = "c")

sf_islands_simp <- st_read("data-raw/sf_island_simp.gpkg")

## Done one time to create easier to handle file
# load("data-raw/sf_islands.Rdata")
# names(sf_islands)
# names(sf_islands_simp) <- names(sf_islands)
# save(sf_islands_simp, file = "data-raw/sf_islands_simp.Rdata")
# load("data-raw/sf_islands_simp.Rdata")
# sf_strata <- data.table::rbindlist(sf_islands_simp) |> st_as_sf()
# sf_strata
# st_write(sf_strata, "data-raw/sf_island_simp.gpkg")

table(sf_lc2021$lc2021)

sf_mg      <- sf_lc2021 |> filter(lc2021 %in% c("primary mangrove forest", "secondary mangrove forest"))
sf_mg_b    <-sf_mg |> st_transform("ESRI:54017")
sf_mg_buff <- 

sf_lc2021_mg2 <- sf_lc2021_mg |> st_transform(crs = 4326)

sf_mg <- st_intersection(dg15_centroid, sf_lc2021_mg2)

nrow(sf_mg)

table(sf_mg$lc2021)

# ggplot() +
#   #geom_sf(data = sf_islands_simp) +
#   geom_sf(data = sf_mg, fill = "red")

names(fra_mg) <- str_replace_all(names(fra_mg), " ", "_")
names(fra_mg) <- str_replace_all(names(fra_mg), "%", "perc_")

#### 
sf_fra <- fra_mg |>
  mutate(lon_copy = lon, lat_copy = lat) |>
  st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326) |>
  st_join(sf_islands_simp)
####


## Extract the value hex from grid 17 that is at the center of grid 13
list_island <- names(sf_islands_simp)
x <- "Kalimantan"

#walk(list_island, function(x){
  
  dggrid15 <- st_read(paste0("data-raw/DGGRID-raw/", x, "-ISEA3H-res15-upland.gpkg"))
  dggrid17 <- st_read(paste0("data-raw/DGGRID-raw/", x, "-ISEA3H-res17-upland.gpkg"))
  
  dg15_centroid <- st_centroid(dggrid15) |> rename(seqnum15 = seqnum)
  dg17_15center <- dggrid17 |> st_join(dg15_centroid) |> filter(!is.na(seqnum15))
  
  # ggplot() +>
  #   geom_sf(data = dggrid15) +
  #   geom_sf(data = dg17_centroid, fill = NA, color = "grey60") +
  #   geom_sf(data = sf_fra) +
  #   coord_sf(xlim = c(109.2, 109.5), ylim = c(-0.8, -0.5))
  
  dg17_15center_fra <- dg17_15center |> st_join(sf_fra)
  
  dg17_15center_fra |> filter(!is.na(plotid)) |> nrow()
  
  sf_fra2 <- sf_fra |> filter(plotid %in% dg17_15center_fra$plotid)
  
  ggplot() +
    geom_sf(data = dggrid15) +
    geom_sf(data = dg17_15center, fill = NA, color = "grey60") +
    geom_sf(data = sf_fra) +
    geom_sf(data = sf_fra2, col = "red") +
    coord_sf(xlim = c(109.2, 109.5), ylim = c(-0.8, -0.5))
  
#})











# ggplot() +
#   geom_sf(data = sf_lc2021_mg)



ggplot() +
  geom_sf(data = sf_lc2021_mg2, fill = "lightblue", color = NA) +
  geom_sf(data = dggrid15, fill = NA) +
  geom_sf(data = sf_mg, fill = NA, color = "blue") +
  geom_sf(data = sf_ph2, color = "red") +
  coord_sf(xlim = c(109, 110), ylim = c(-1, 0))



