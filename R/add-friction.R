

library(data.table)
library(terra)
library(sf)
library(tidyterra)

## Data from data-source are downloaded from internet: 
## https://malariaatlas.org/ > Data > friction layers clipped for Indonesia


rs_friction_health <- rast("data-source/202001_Global_Motorized_Travel_Time_to_Healthcare_IDN.tiff")
rs_friction_health

plot(rs_friction_health)

rs_friction_city <- rast("data-source/201501_Global_Travel_Time_to_Cities_IDN.tiff")
rs_friction_city

plot(rs_friction_city)


ceo    <- read_csv("data/ceo.csv") 
sf_ceo <- ceo |> 
  mutate(lon_copy = lon, lat_copy = lat) |>
  st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326)

sf_dggrid <- st_read("data/sf_ddggrid.gpkg")


## Check
# ggplot() +
#  geom_spatraster(data = rs_friction) +
#  geom_sf(data = sf_ceo, size = 0.1, col = "red")

friction_health_values <- terra::extract(rs_friction_health, vect(sf_ceo))[,2]
friction_city_values <- terra::extract(rs_friction_city, vect(sf_ceo))[,2]

sf_ceo2 <- sf_ceo |>
  mutate(
    friction_health = friction_health_values,
    friction_city = friction_city_values
    
    # lu_access_corr = case_when(
    #   lu_cat2 == "Forest" & friction_values <= 180 ~ "Forest-access",
    #   lu_cat2 == "Forest" & friction_values >  180 ~ "Forest-no access",
    #   lu_cat2 == "Forest" & is.na(friction_values) ~ "Forest-no access",
    #   lu_cat2 == "TOF"    & friction_values <= 180 ~ "TOF-access",
    #   lu_cat2 == "TOF"    & friction_values >  180 ~ "TOF-no access",
    #   lu_cat2 == "TOF"    & is.na(friction_values) ~ "TOF-no access",
    #   TRUE ~  "Other land"
    # )
)


## Checks
summary(sf_ceo2$friction_health)
summary(sf_ceo2$friction_city)

sf_ceo2 |>
  filter(friction_health <= 180 | friction_city <= 180) |>
  ggplot() +
  geom_point(aes(x = friction_city, y = friction_health))


## Combine with hex
sf_dggrid2 <- sf_dggrid |>
  st_join(sf_ceo2)


sf_dggrid2 |>
  mutate(friction_health2 = if_else(friction_health / 60 > 3, 3, friction_health / 60)) |>
  ggplot() +
  geom_sf(aes(fill = friction_health2), color = NA) +
  scale_fill_viridis_c(direction = -1) +
  theme_bw()

# table(sf_ceo2$pl_island, sf_ceo2$lu_access_corr)
# table(sf_ceo2$pl_island, sf_ceo2$lu_access2)
# 

# 
# sf_ceo2 |>
#   filter(friction_min <= 180) |>
#   ggplot() +
#   geom_sf(aes(color = friction_min), size = 0.2) +
#   geom_sf(data = sf_country, fill = NA)
# 
# sf_ceo2 |>
#   ggplot() +
#   geom_sf(aes(color = lu_access2), size = 0.4) +
#   geom_sf(data = sf_country, fill = NA) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(
#     subtitle = "Plot accessibility from CEO Phase 1 data",
#     color = ""
#     ) +
#   scale_color_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink"))
# 
# sf_ceo2 |>
#   ggplot() +
#   geom_sf(aes(color = lu_access_corr), size = 0.4) +
#   geom_sf(data = sf_country, fill = NA) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(
#     subtitle = "Plot accessibility from Malariaatlas v202001",
#     color = ""
#   ) +
#   scale_color_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink"))


### ADD hilbert's distance
sf_hilb <- st_read("data/sf_hilbert.gpkg")

hilb <- sf_hilb |> as_tibble() |> select(FRA_HEXID, hilbert_dist = dist)

sf_ceo3 <- sf_ceo2 |>
  left_join(hilb, by = c("pl_fra_hexid" = "FRA_HEXID"))
  
sf_ceo3 |>
  as_tibble() |>
  select(-geometry) |>
  write_csv("data/ceo_withfriction_hilbert.csv")
