

path_res <- "results"
dir.create(path_res, showWarnings = F)

sfc_factor <- tibble(
  island_name  = c("BaliNusa", "Jawa", "Kalimantan", "Maluku", "Papua", "Sulawesi", "Sumatera"),
  phase1_error = head(round(disa_lucat_stat$disagree / disa_lucat_stat$total, 2), -1),
  n_plot       = c(119, 148, 413, 219, 257, 486, 386),
  n_plot_ph2   = ceiling(n_plot * (1 + phase1_error)),
  sfc_adjust   = c(2.5, 5, 2.2, 0.7, 1, 1.1, 3) ## To be updated for friction
)

# ceo2 <- ceo %>%
#   mutate(lu_access2 = case_when(
#       pl_island == "Papua" & lu_cat2 == "Forest" & access_by_road == "No" & access_by_river == "Yes" ~ "Forest-no access",
#       pl_island == "Papua" & lu_cat2 == "Forest" & access_by_road == "No" & access_by_coast == "Yes" ~ "Forest-no access",
#       pl_island == "Papua" & lu_cat2 == "TOF" & access_by_road == "No" & access_by_river == "Yes" ~ "TOF-no access",
#       pl_island == "Papua" & lu_cat2 == "TOF" & access_by_road == "No" & access_by_coast == "Yes" ~ "TOF-no access",
#       TRUE ~ lu_access
#     )
#   )

## CEO number of total accessible plots in forest and TOF
table(ceo2$lu_access2, ceo2$pl_island)

path_dat <- list.files("data-raw/Ph1-centroid-withSFC", pattern = "withSFC", full.names = T)

x = path_dat[3]

walk(path_dat, function(x){
  
  target_island <- x |> str_remove(".*/") |> str_remove("-.*")
  target_island
  
  sf_strata <- sf_country |> filter(island == target_island)
  
  sf_sfc <- st_read(x, quiet = T) |>
    mutate(
      plotid = PLOTID, 
      pl_fra_hexid = as.character(FRA_HEXID)
    )
  
  sf_point <- sf_sfc |>
    left_join(ceo2, by = c("plotid", "pl_fra_hexid")) |>
    mutate(
      x = st_coordinates(sf_sfc)[,1],
      y = st_coordinates(sf_sfc)[,2]
    )
  
  gr <- ggplot() +
    geom_sf(data = sf_point, aes(color = dist)) +
    geom_path(data = sf_point, aes(x = x, y = y)) +
    scale_color_viridis_c() +
    theme_void() +
    theme(legend.position = "none")
  print(gr)
  
  n_pool <- nrow(sf_point)
  n_plot <- sfc_factor |> filter(island_name == target_island) |> pull(n_plot_ph2)
  sfc_adjust <- sfc_factor |> filter(island_name == target_island) |> pull(sfc_adjust)
  n_access_forest <- sf_point |> as_tibble() |> filter(lu_access == "Forest-access") |> nrow()
  sfc_corr <- round(n_access_forest / n_plot, 2)
  n_plot2 <- ceiling(n_plot * sfc_corr * sfc_adjust)
  
  ran_start_range <- 1:ceiling(n_pool / n_plot2)
  ran_start_range
  
  ## !!! Temporary setting a seed to make results stable
  set.seed(10)
  start_point <- sample(ran_start_range, 1)
  
  ## Get sampling proportion of available plots
  sampling_proportion <- n_plot2 / n_pool
  sampling_proportion
  
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
  tt <- sf_point_phase2 |> filter(lu_access_corr == "Forest-access") |> nrow()
  tt
  tt > n_plot
  
  sf_ph2 <- sf_point_phase2 |> filter(lu_access_corr == "Forest-access") 
  print(table(sf_ph2$lu_sub, sf_ph2$lu_cat))
  
  sf_tof <- sf_point_phase2 |> filter(lu_access_corr == "TOF-access")
  table(sf_tof$lu_sub, sf_tof$lu_cat)
  
  sf_ph2_all <- bind_rows(sf_ph2, sf_tof) |> distinct()
  
  gr <- ggplot() +
    geom_path(data = sf_point, aes(x = x, y = y), color = "grey90") +
    geom_sf(data = sf_ph2, aes(color = lu_sub)) +
    scale_color_viridis_d() +
    theme_void()
  print(gr)
  
  gr <- ggplot() +
    #geom_path(data = sf_point, aes(x = x, y = y), color = "grey90") +
    geom_sf(data = filter(sf_ceo2, pl_island == target_island), aes(color = lu_access2)) +
    geom_sf(data = sf_ph2_all, fill = "white", size = 0.6, pch = 21) +
    labs(
      color = ""
    ) +
    scale_color_manual(values = c("darkgreen", "darkred", "grey80", "yellowgreen", "lightpink")) +
    theme_void()
  print(gr)
  
  gr <- ggplot() +
    geom_sf(data = sf_strata, fill = "grey95", color = NA) +
    geom_path(data = sf_point, aes(x = x, y = y), color = "grey80") +
    geom_sf(data = sf_ph2_all, aes(color = lu_cat2)) +
    scale_color_viridis_d() +
    theme_void() +
    labs(color = "")
  print(gr)
  ggsave(gr, filename = paste0(path_res, "/", target_island, "_accessible_forest_TOF.jpeg"), width = 12, height = 9, dpi = 300, units = "cm")
  
  sf_point_phase2 |>
    as_tibble() |>
    select(-email) |>
    write_csv(file.path(path_res, paste0(target_island, "_all_samples.csv")))
  
  sf_ph2_all |>
    as_tibble() |>
    select(-email) |>
    write_csv(file.path(path_res, paste0(target_island, "_PH2_samples.csv")))
  
})

