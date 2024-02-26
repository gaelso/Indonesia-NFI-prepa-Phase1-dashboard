

rs_friction <- rast("data-source/202001_Global_Motorized_Travel_Time_to_Healthcare_IDN.tiff")
rs_friction
plot(rs_friction)

sf_ceo <- ceo |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

# ggplot() +
#   geom_spatraster(data = rs_friction) +
#   geom_sf(data = sf_ceo, size = 0.5, col = "red")

friction_values <- terra::extract(rs_friction, vect(sf_ceo))[,2]

sf_ceo2 <- sf_ceo |>
  mutate(
    friction_min = friction_values,
    lu_access_corr = case_when(
      lu_cat2 == "Forest" & friction_values <= 180 ~ "Forest-access",
      lu_cat2 == "Forest" & friction_values >  180 ~ "Forest-no access",
      lu_cat2 == "Forest" & is.na(friction_values) ~ "Forest-no access",
      lu_cat2 == "TOF"    & friction_values <= 180 ~ "TOF-access",
      lu_cat2 == "TOF"    & friction_values >  180 ~ "TOF-no access",
      lu_cat2 == "TOF"    & is.na(friction_values) ~ "TOF-no access",
      TRUE ~  "Other land"
    ))

summary(sf_ceo2$friction_min)
table(sf_ceo2$pl_island, sf_ceo2$lu_access_corr)
table(sf_ceo2$pl_island, sf_ceo2$lu_access2)

sf_ceo2 |>
  filter(friction_min <= 180) |>
  ggplot() +
  geom_sf(aes(color = friction_min), size = 0.2) +
  geom_sf(data = sf_country, fill = NA)

sf_ceo2 |>
  ggplot() +
  geom_sf(aes(color = lu_access2), size = 0.4) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    subtitle = "Plot accessibility from CEO Phase 1 data",
    color = ""
    ) +
  scale_color_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink"))

sf_ceo2 |>
  ggplot() +
  geom_sf(aes(color = lu_access_corr), size = 0.4) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    subtitle = "Plot accessibility from Malariaatlas v202001",
    color = ""
  ) +
  scale_color_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink"))

ceo2 <- sf_ceo2 |> as_tibble() |> select(-geometry)

write_csv(ceo2, "data/ceo_withfriction.csv")
