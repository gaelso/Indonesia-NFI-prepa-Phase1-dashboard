
library(ggpubr)
library(sf)
library(tidyverse)

theme_set(theme_bw())

dir.create("results/option2", showWarnings = F)


ceo <- read_csv("data/ceo_withfriction_hilbert.csv")
sf_ceo <- ceo |> 
  mutate(lon_copy = lon, lat_copy = lat) |>
  st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326)

disa_lucat_stat <- read_csv("data/disa_lucat_stat.csv") 

sf_dggrid <- st_read("data/sf_ddggrid.gpkg")

load("data-raw/sf_islands.Rdata")


sfc_factor <- tibble(
  island_name   = c("BaliNusa", "Jawa", "Kalimantan", "Maluku", "Papua", "Sulawesi", "Sumatera"),
  island_code   = c("BL", "JV", "KL", "ML", "PP", "SL", "SM"),
  phase1_error  = head(round(disa_lucat_stat$disagree / disa_lucat_stat$total, 2), -1),
  sfc_adjust    = c(1, 1, 1, 1, 1, 1, 1),
  n_plot_init   = c(119, 148, 413, 219, 257, 486, 386),
  n_plot_target = ceiling(n_plot_init * (1 + phase1_error) * sfc_adjust),
  seed          = c(67, 93, 76, 16, 59, 89, 19)
  #n_plot_ph2   = ceiling(n_plot * (1 + phase1_error)),
  #sfc_adjust   = c(2.5, 5, 1.7, 0.7, 1, 1.1, 3)
)
sfc_factor

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
    ),
    friction_access = case_when(
      friction_health <= 180 ~ "1 day measurement",
      friction_health <= 600 ~ "3 days measurement",
      TRUE ~ "5 days or more"
    )
  )

table(ceo_access$lu_access1, ceo_access$lu_access2, useNA = "ifany")

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
  left_join(npool_access3, by = c("island_name" = "pl_island")) |>
  mutate(
    samprop_access2 = round(n_plot_target / n_pool_access2, 2),
    samprop_access3 = round(n_plot_target / n_pool_access3, 2)
  )
sfc_factor2





##
## RUN ALGO FOR ONE ISLAND
##


walk(sfc_factor$island_name, function(x){
  
  island_code <- sfc_factor |> filter(island_name == x) |> pull(island_code)
  seed_num    <- sfc_factor |> filter(island_name == x) |> pull(seed)
  sf_strata   <- sf_islands[[x]]
  ceo_sub     <- ceo_access |> filter(pl_island == x) |> mutate(lu_access = lu_access2) |> arrange(hilbert_dist)
  
  sf_ceo_sub <- ceo_sub |>
    mutate(lon_copy = lon, lat_copy = lat) |>
    st_as_sf(coords = c("lon_copy", "lat_copy"), crs = 4326)
  
  sf_dggrid_sub <- sf_dggrid |> 
    st_join(sf_ceo_sub) |> 
    filter(!is.na(plotid)) |>
    arrange(hilbert_dist)
  
  ## Show sfc and access
  # gr <- sf_ceo_sub |>
  #   ggplot() +
  #   geom_sf(aes(color = hilbert_dist), size = 0.6) +
  #   geom_path(aes(x = lon, y = lat, color = hilbert_dist)) +
  #   scale_color_viridis_c() +
  #   theme_void() +
  #   theme(legend.position = "none")
  # print(gr)
  
  # gr <- ggplot() +
  #   geom_sf(data = sf_dggrid_sub, aes(fill = lu_access), size = 0.6) +
  #   scale_fill_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink")) +
  #   labs(fill = "") +
  #   theme_void()
  # #print(gr)
  
  ## SFC inputs
  n_pool      <- nrow(ceo_sub)
  n_pool_sub  <- ceo_sub |> filter(lu_access %in% c("Forest-access")) |> nrow()
  #n_pool_sub  <- ceo_sub |> filter(lu_access %in% c("Forest-access")) |> nrow()
  n_plot_init <- sfc_factor |> filter(island_name == x) |> pull(n_plot_init)
  n_plot_U    <- sfc_factor |> filter(island_name == x) |> pull(phase1_error)
  sfc_adjust  <- sfc_factor |> filter(island_name == x) |> pull(sfc_adjust)
  
  n_plot <- ceiling(n_plot_init * (1 + n_plot_U) * sfc_adjust)
  n_plot
  n_pool_sub
  
  samprop <-  round(n_plot / n_pool_sub, 2)
  samprop
  
  ceiling(1/samprop)
  
  ## Chose seed at random
  set.seed(seed_num)
  if (length(ceiling(1/samprop) < 5)){
    start_point <- sample(2:5, 1)
  } else {
    start_point <- sample(2:ceiling(1/samprop), 1)
  }
  
  ## Recap SFC params
  samprop
  start_point
  
  ## Initialize for loop
  pool_dist <- sort(ceo_sub$hilbert_dist)
  
  ph2_dist <- vector(length = n_pool) ## Hilbert dist of the chosen plots
  ph2_no   <- vector(length = n_pool) ## Rank of the chosen plots, 1, 2, 3, ...
  ph2_pos  <- vector(length = n_pool) ## Position of the chosen plot, ie value in 1:n_pool
  
  j <- 1
  for (i in start_point:n_pool) {
    
    if (ceiling((i - start_point + 1) * samprop) == j) {
      ph2_dist[i] <- pool_dist[i]
      ph2_pos[i]  <- i
      ph2_no[i]   <- j
      j <- j + 1
      # } else {
      #   ph2_dist[i] <- 0
      #   ph2_pos[i]  <- 0
      #   ph2_no[i]   <- 0
    }
  } ## End for
  
  ## group SFC results
  ph2_sfc <- tibble(hilbert_dist = ph2_dist, ph2_pos = ph2_pos, ph2_no = ph2_no) |>
    filter(hilbert_dist > 0)
  
  sf_ceo_ph2 <- sf_ceo_sub |> 
    left_join(ph2_sfc, by = "hilbert_dist")
  
  head(sf_ceo_ph2$ph2_pos, 50)
  
  summary(sf_ceo_ph2$ph2_no)
  
  # sf_ceo_ph2 |>
  #   filter(!is.na(ph2_no)) |>
  #   ggplot() +
  #   geom_sf(aes(color = ph2_no)) +
  #   scale_color_viridis_c()
  
  # sf_ceo_ph2 |>
  #   filter(!is.na(ph2_no), lu_cat2 == "Forest") |>
  #   arrange(hilbert_dist) |>
  #   ggplot() +
  #   geom_sf(data = sf_strata, fill = "grey90", color = NA) + 
  #   geom_path(data = sf_ceo_ph2, aes(x = lon, y = lat), color = "grey80") +
  #   geom_sf(data = sf_ceo_ph2, aes(color = lu_access)) +
  #   geom_sf(color = "black", shape = 8) +
  #   scale_color_manual(values = c("green", "red", "grey80", "yellowgreen", "lightpink"))
  
  sf_ph2 <- sf_ceo_ph2 |> 
    filter(!is.na(ph2_no))
  
  ## random selection of TOF
  ceo_tof_id <- sf_ph2 |>
    filter(lu_access == "TOF-access") |>
    pull(plotid)
  
  if (length(ceo_tof_id) <= 70){
    tof_sample <- ceo_tof_id
  } else {
    set.seed(seed_num)
    tof_sample <- sample(ceo_tof_id, 70)
  }
  
  gr <- sf_ph2 |>
    filter(lu_cat2 == "Forest" | plotid %in% tof_sample) |>
    arrange(hilbert_dist) |>
    ggplot() +
    geom_sf(data = sf_strata, fill = "grey90", color = NA) + 
    geom_path(data = sf_ceo_ph2, aes(x = lon, y = lat), color = "grey80") +
    geom_sf(aes(color = lu_access), size = 0.8) +
    scale_color_manual(values = c("darkgreen", "darkred", "yellowgreen")) +
    labs(color = "") +
    theme_void()
  #print(gr)
  ggsave(gr, filename = paste0("results/option2/", x, "-NFI-Phase2-all samples.jpeg"), width = 24, height = 18, dpi = 300, units = "cm")
  
  tt <- sf_ph2 |>
    filter(lu_cat2 == "Forest" | plotid %in% tof_sample)
  
  table(tt$lu_access)
  samprop
  start_point
  n_plot
  n_pool_sub
  x
  
  sf_ceo_ph2 |>
    as_tibble() |>
    mutate(ph2_selected = case_when(
      plotid %in% tof_sample ~ "TOF-ph2",
      !is.na(ph2_no) & lu_access == "Forest-access" ~ "Forest-access-ph2",
      !is.na(ph2_no) & lu_access == "Forest-no access" ~ "Forest-no-access-ph2",
      TRUE ~ "Not-selected-ph2"
    )) |>
    write_csv(paste0("results/option2/", x, "-CEO-results_with_ph2.csv"))
  
  sf_ph2 |>
    as_tibble() |>
    filter(lu_access == "Forest-access") |>
    write_csv(paste0("results/option2/", x, "-NFI-Phase2-Forest.csv"))
  
  sf_ph2 |>
    as_tibble() |>
    filter(lu_access == "Forest-no access") |>
    write_csv(paste0("results/option2/", x, "-NFI-Phase2-Forest-non accessible.csv"))
  
  sf_ph2 |>
    as_tibble() |>
    filter(plotid %in% tof_sample) |>
    write_csv(paste0("results/option2/", x, "-NFI-Phase2-TOF.csv"))
  
  sf_ph2 |>
    as_tibble() |>
    filter(lu_access == "Forest-access") |>
    summarise(count = n(), .by = lu_sub) |>
    write_csv(paste0("results/option2/", x, "-NFI-Phase2-Forest-subcat.csv"))
  
  sf_ph2 |>
    as_tibble() |>
    filter(lu_access == "Forest-access") |>
    summarise(count = n(), .by = friction_access) |>
    write_csv(paste0("results/option2/", x, "-NFI-Phase2-friction.csv"))
  
})



##
## Summary stats
##

path_out1 <- list.files("results/option2", pattern =  "subcat", full.names = T)
out1 <- map(path_out1, function(x){
  out  <- read_csv(x, show_col_types = F)
  name <- x |> str_remove(".*/") |> str_remove("-.*")
  out  <- out |>
    mutate(island = name)
}) |> 
  list_rbind() |>
  pivot_wider(names_from = "island", values_from = "count", values_fill = 0)
out1  
write_csv(out1, "results/option2/zz-stat-land use.csv")


path_out2 <- list.files("results/option2", pattern =  "friction", full.names = T)
out2 <- map(path_out2, function(x){
  
  out <- read_csv(x, show_col_types = F)
  name <- x |> str_remove(".*/") |> str_remove("-.*")
  
  out <- out |>
    mutate(island = name)
}) |> 
  list_rbind() |>
  arrange(friction_access) |>
  pivot_wider(names_from = "friction_access", values_from = "count", values_fill = 0)
out2  
write_csv(out2, "results/option2/zz-stat-time access.csv")


path_out3 <- list.files("results/option2", pattern =  "with_ph2", full.names = T)
out3 <- map(path_out3, read_csv, show_col_types = FALSE) |> 
  list_rbind() |>
  filter(ph2_selected != "Not-selected-ph2") |>
  filter(lu_access2 %in% c("Forest-access", "Forest-no access", "TOF-access")) |>
  group_by(pl_island, lu_access2) |>
  summarise(count = n()) |>
  pivot_wider(names_from = "lu_access2", values_from = "count", values_fill = 0)
out3  
write_csv(out3, "results/option2/zz-stat-nplot.csv")
