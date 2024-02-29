
library(ggpubr)
library(sf)
library(tidyverse)


ceo <- read_csv("data/ceo_withfriction_hilbert.csv")
sf_ceo <- ceo |> 
  mutate(lon_copy = lon, lat_copy = lat) |>
  st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326)

disa_lucat_stat <- read_csv("data/disa_lucat_stat.csv") 

sf_dggrid <- st_read("data/sf_ddggrid.gpkg")

load("data-raw/sf_islands.Rdata")


sfc_factor <- tibble(
  island_name  = c("BaliNusa", "Jawa", "Kalimantan", "Maluku", "Papua", "Sulawesi", "Sumatera"),
  island_code  = c("BL", "JV", "KL", "ML", "PP", "SL", "SM"),
  phase1_error = head(round(disa_lucat_stat$disagree / disa_lucat_stat$total, 2), -1),
  n_plot       = c(119, 148, 413, 219, 257, 486, 386),
  n_plot_ph2   = ceiling(n_plot * (1 + phase1_error)),
  sfc_adjust   = c(2.5, 5, 1.7, 0.7, 1, 1.1, 3)
)


## Start iteration
table(ceo$lu_cat2)


ceo_access <- ceo |>
  mutate(
    lu_access1 = case_when(
        lu_cat2 == "Forest" & access_by_road == "Yes"  ~ "Forest-access",
        lu_cat2 == "Forest" & access_by_river == "Yes" ~ "Forest-access",
        lu_cat2 == "Forest" & access_by_coast == "Yes" ~ "Forest-access",
        lu_cat2 == "Forest" ~ "Forest-no access",
        lu_cat2 == "TOF" & access_by_road == "Yes"  ~ "TOF-access",
        lu_cat2 == "TOF" & access_by_river == "Yes" ~ "TOF-access",
        lu_cat2 == "TOF" & access_by_coast == "Yes" ~ "TOF-access",
        lu_cat2 == "TOF" ~ "TOF-no access",
        TRUE ~ "Other land"
    ),
    lu_access2 = case_when(
      pl_island %in% c("Papua", "Kalimantan") & lu_cat2 == "Forest" & access_by_road == "Yes"  ~ "Forest-access",
      pl_island %in% c("Papua", "Kalimantan") & lu_cat2 == "Forest" ~ "Forest-no access",
      pl_island %in% c("Papua", "Kalimantan") & lu_cat2 == "TOF" & access_by_road == "Yes"  ~ "TOF-access",
      pl_island %in% c("Papua", "Kalimantan") & lu_cat2 == "TOF" ~ "TOF-no access",
      TRUE ~ lu_access1
    ),
    lu_access3 = case_when(
      lu_cat2 == "Forest" & friction_health <= 180  ~ "Forest-access",
      lu_cat2 == "Forest" ~ "Forest-no access",
      lu_cat2 == "TOF" & friction_health <= 180 ~ "TOF-access",
      lu_cat2 == "TOF" ~ "TOF-no access",
      TRUE ~ "Other land"
    )
  )

table(ceo_access$lu_access1, ceo_access$lu_access2)

sf_ceo_access <- ceo_access |>
  mutate(lon_copy = lon, lat_copy = lat) |>
  st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326)

sf_dggrid_access <- sf_dggrid |> st_join(sf_ceo_access)

gr_access1 <- ggplot(sf_dggrid_access) +
  geom_sf(aes(fill = lu_access1), color = NA) +
  scale_fill_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink")) +
  labs(fill = "", subtitle = "Accessiblity option 1") +
  theme_void()

gr_access2 <- ggplot(sf_dggrid_access) +
  geom_sf(aes(fill = lu_access2), color = NA) +
  scale_fill_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink")) +
  labs(fill = "", subtitle = "Accessiblity option 2") +
  theme_void()

gr_access3 <- ggplot(sf_dggrid_access) +
  geom_sf(aes(fill = lu_access3), color = NA) +
  scale_fill_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink")) +
  labs(fill = "", subtitle = "Accessiblity option 3") +
  theme_void()

ggpubr::ggarrange(gr_access1, gr_access2, gr_access3, nrow = 3, align = "v", common.legend = T, legend = "right")


npool_access1 <- ceo_access |>
  filter(lu_access1 == "Forest-access") |>
  group_by(pl_island) |>
  summarise(n_pool_access1 = n())

npool_access2 <- ceo_access |>
  filter(lu_access2 == "Forest-access") |>
  group_by(pl_island) |>
  summarise(n_pool_access2 = n())

npool_access3 <- ceo_access |>
  filter(lu_access3 == "Forest-access") |>
  group_by(pl_island) |>
  summarise(n_pool_access3 = n())

sfc_factor2 <- sfc_factor |>
  left_join(npool_access1, by = c("island_name" = "pl_island")) |>
  left_join(npool_access2, by = c("island_name" = "pl_island")) |>
  left_join(npool_access3, by = c("island_name" = "pl_island"))
sfc_factor2
  


## RUN ALGO FOR EACH ISLAND

x = sfc_factor$island_name[3]


island_code <- sfc_factor |> filter(island_name == x) |> pull(island_code)
sf_strata   <- sf_islands[[x]]
ceo_sub     <- ceo_access |> filter(pl_island == x) |> mutate(lu_access = lu_access2)

sf_ceo_sub <- ceo_sub |>
  mutate(lon_copy = lon, lat_copy = lat) |>
  st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326)

sf_dggrid_sub <- sf_dggrid |> 
  st_join(sf_ceo_sub) |> 
  filter(!is.na(plotid)) |>
  arrange(hilbert_dist)

## Show sfc and access
gr <- sf_ceo_sub |>
  arrange(hilbert_dist) |>
  ggplot() +
  geom_sf(aes(color = hilbert_dist), size = 0.6) +
  geom_path(aes(x = lon, y = lat, color = hilbert_dist)) +
  scale_color_viridis_c() +
  theme_void() +
  theme(legend.position = "none")
print(gr)

gr <- ggplot() +
  geom_sf(data = sf_dggrid_sub, aes(fill = lu_access), size = 0.6) +
  scale_fill_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink")) +
  labs(fill = "") +
  theme_void()
print(gr)


n_pool <- ceo_sub |> filter(lu_access1 == "Forest-access") |> nrow()

n_plot <- sfc_factor |> filter(island_name == target_island) |> pull(n_plot_ph2)
sfc_adjust <- sfc_factor |> filter(island_name == target_island) |> pull(sfc_adjust)
n_access_forest <- sf_point |> as_tibble() |> filter(lu_access == "Forest-access") |> nrow()
sfc_corr <- round(n_access_forest / n_plot, 2)
n_plot2 <- ceiling(n_plot * sfc_corr * sfc_adjust)

samprop <- n_plot / n_access_forest
ceiling(1/samprop)

ran_start_range <- 1:ceiling(n_pool / n_plot2)
ran_start_range

## !!! Temporary setting a seed to make results stable
set.seed(10)
start_point <- sample(ran_start_range, 1)

## Get sampling proportion of available plots
sampling_proportion <- n_plot2 / n_pool
print(sampling_proportion)

## Initialize for loop
pool_seqnum <- sf_point$FRA_HEXID
plot_seqnum <- vector(length = n_pool)
sfc_cluster <- vector(length = n_pool) 


j <- start_point
for (i in 1:n_pool) {
  
  if (i >= start_point & ceiling(i * sampling_proportion) == j) {
    plot_seqnum[i] <- pool_seqnum[i]
    j <- j + 1
  } else {
    plot_seqnum[i] <- 0
    sfc_cluster[i] <- j
  }
} ## End for


# plot_seqnum_selected <- plot_seqnum[plot_seqnum != 0]

sf_point_phase2 <- sf_point %>% 
  mutate(
    field_plot_seqnum = plot_seqnum,
    sfc_cluster_no    = sfc_cluster
  ) |>
  filter(field_plot_seqnum != 0) |>
  mutate(lu_cat3 = if_else(lu_cat2 %in% c("Forest", "TOF"), lu_cat2, "Other"))

#table(sf_point_phase2$lu_cat2)
#table(sf_point_phase2$lu_access)
table(sf_point_phase2$lu_access2, sf_point_phase2$lu_cat3)

##
## Forest access vs target
##
n_plot
tt <- sf_point_phase2 |> filter(lu_access2 == "Forest-access") |> nrow()
tt
tt > n_plot


