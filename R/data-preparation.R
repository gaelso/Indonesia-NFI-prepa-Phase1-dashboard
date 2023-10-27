
## Create tables for analysis and display

## TO RESET ANALYSIS REMOVE CONTENT FROM "./data"

## Check if data ready
 if(!("ceo.csv" %in% list.files("data"))){
   
   
   ## User's info
   usr_info <- read_csv("data-raw/list of participants NFI-Phase1.csv")
   
   ## CEO raw data
   ceosession <- "ceo-10-27-am"
   ceopath    <- list.files(file.path("data-raw", ceosession), pattern = "csv", full.names = T)
   
   ## Uncomment to reduce analysis to the inital survey
   ## The initial survey was discontinued to remove the user assignments
   # ceopath    <- str_subset(ceopath, pattern = "Updated", negate = T)
   
   ceoraw <- map(ceopath, read_csv, col_types = cols(.default = "c")) |> list_rbind()
   ceoraw
   
   names(ceoraw)
   
   sf_ceoid <- ceoraw |>
     select(plotid, lon, lat) |>
     distinct() |>
     st_as_sf(coords = c("lon", "lat"), crs = 4326)
   
   ## Spatial data
   gridpath <- list.files("data-raw/DGGRID", pattern = "gpkg", full.names = T)
   sf_grid <- map(gridpath, function(x){
     name <- x |> str_remove(".*/") |> str_remove("-.*")
     sf_out <- st_read(x, quiet = TRUE) |>
       mutate(island = name)
     sf_out
   }) |> data.table::rbindlist() |> st_as_sf()
   
   #load("data-raw/sf_islands_simp.Rdata")
   #sf_country <- sf_islands_simp |> data.table::rbindlist() |> st_as_sf()
   
   # ggplot() +
   #   geom_sf(data = sf_country, aes(fill = island)) +
   #   geom_sf(data = sf_grid, fill = NA)
   
   
   
   ##
   ## USER INFO ###################################################################
   ##
   
   set.seed(10)
   usr_ano <- usr_info |>
     mutate(
       num  = 1:nrow(usr_info),
       ran = sample(num),
       user_code = case_when(
         ran < 10 ~ paste0("user0", ran),
         TRUE     ~ paste0("user", ran)
       )
     ) |>
     select(-ran)
   usr_ano 
   
   usr_island_planned <- usr_ano |>
     summarise(planned = n(), .by = island)
   
   usr_ano2 <- usr_ano |> select(email, user_code)
   
   ##
   ## Survey data #################################################################
   ##
   
   new_names <- names(ceoraw) |> 
     str_replace_all("%", "perc")|>
     str_replace_all(" of ", "_") |>
     str_to_lower() |>
     str_replace_all(" ", "_") |>
     str_remove_all("\\?") |>
     str_remove_all("-_") |>
     str_replace_all("-", "_")
   new_names
   
   ## !!! REMOVING NOT MEASURED AND FLAGGED
   ceocomp <- ceoraw |> filter(!is.na(email) & flagged == "false")
   ## !!!
   
   ## Remove duplicates
   ceodup <- ceocomp |>
     summarise(count = n(), .by = c(pl_island, plotid)) |>
     filter(count > 1) |>
     mutate(
       n_dup = count - 1
     )
   
   n_dup <- sum(ceodup$n_dup)
   n_dup
   
   max_dup <- max(ceodup$n_dup)
   max_dup
   
   names(ceocomp) <- new_names
   
   ceo <- ceocomp |>
     arrange(pl_island, collection_time) |>
     distinct(plotid, .keep_all = T) |>
     left_join(usr_ano2, by = "email") |>
     select(-sample_geom) |>
     mutate(
       user_group = paste0(pl_island, "-", user_code)
     ) |>
     # mutate(
     #   collection_time = as.POSIXct(collection_time, tz = "GMT"),
     #   collection_time = with_tz(collection_time, tz = "Asia/Makassar"),
     #   date     = date(collection_time),
     #   year     = year(collection_time),
     #   month    = month(collection_time),
     #   day      = day(collection_time),
     #   day_txt  = paste0(month, "-", day(collection_time)),
     #   day_half = if_else(am(collection_time), paste0(month, "-", day, " AM"), paste0(month, "-", day, " PM")),
     #   duration = abs(round(as.numeric(str_remove(analysis_duration, " secs")) / 60, 2))
     # ) |>
     mutate(
       lu_cat = land_use_2023_centroid,
       lu_sub = paste0(forest_sub_categories, owl_sub_categories, ol_sub_categories, water_sub_categories),
       lu_sub = str_remove_all(lu_sub, "NA")
     ) |>
     mutate(
       plotid = as.numeric(plotid),
       pl_fra_hexid = as.numeric(pl_fra_hexid)
     ) |>
     mutate(
       lu_access = case_when(
         lu_cat == "Forest" & access_by_road == "Yes"  ~ "Forest-access",
         lu_cat == "Forest" & access_by_river == "Yes" ~ "Forest-access",
         lu_cat == "Forest" & access_by_coast == "Yes" ~ "Forest-access",
         lu_cat == "Forest" ~ "Forest-no access",
         TRUE ~ "Non-forest"
       )
     )
   
   names(ceo)
   
   ## Valid data
   message("Total number of measurements: ", nrow(ceoraw))
   message("Total number of unique plots measurements: ", nrow(ceo))
   message("Number of revisited plots: ", n_dup)
   message("Max number of duplicates for a plot: ", max_dup)
   message("Number of plots visited 1 time + revisited match total: ", nrow(ceocomp) == nrow(ceo) + n_dup)
   message("Percentage of revisited plots: ", round(n_dup / nrow(ceocomp) * 100), "%")
   
   
   
   ##
   ## Spatial joins ###############################################################
   ##
   
   sf_ceo <- sf_grid |>
     rename(pl_island = island) |>
     st_join(sf_ceoid) |>
     mutate(
       plotid = as.numeric(plotid),
     ) |>
     left_join(ceo, by = c("plotid", "pl_island")) |>
     filter(!is.na(plotid))
   
   # ggplot() +
   #   geom_sf(data = sf_progress, aes(fill = lu_cat), color = NA) +
   #   scale_fill_manual(values = pal_lucat, na.value = "grey95") +
   #   theme_bw()
   
   
   
   ##
   ## QAQC ########################################################################
   ##
   
   ceodup_plotid <- ceodup |> pull(plotid) |> unique()
   
   ceoqaqc <- ceocomp |>
     filter(plotid %in% ceodup_plotid) |>
     mutate(
       collection_time = as.POSIXct(collection_time, tz = "GMT"),
       collection_time = with_tz(collection_time, tz = "Asia/Makassar"),
       date     = date(collection_time),
       year     = year(collection_time),
       month    = month(collection_time),
       day      = day(collection_time),
       day_txt  = paste0(month, "-", day(collection_time)),
       day_half = if_else(am(collection_time), paste0(month, "-", day, " AM"), paste0(month, "-", day, " PM")),
       duration = abs(round(as.numeric(str_remove(analysis_duration, " secs")) / 60, 2))
     ) |>
     mutate(
       lu_cat = land_use_2023_centroid,
       lu_sub = paste0(forest_sub_categories, owl_sub_categories, ol_sub_categories, water_sub_categories),
       lu_sub = str_remove_all(lu_sub, "NA")
     ) |>
     mutate(
       plotid = as.numeric(plotid),
       pl_fra_hexid = as.numeric(pl_fra_hexid)
     ) |>
     mutate(
       lu_access = case_when(
         lu_cat == "Forest" & access_by_road == "Yes"  ~ "Forest-access",
         lu_cat == "Forest" & access_by_river == "Yes" ~ "Forest-access",
         lu_cat == "Forest" & access_by_coast == "Yes" ~ "Forest-access",
         lu_cat == "Forest" ~ "Forest-no access",
         TRUE ~ "Non-forest"
       )
     )
   
   
   ##
   ## LIST FLAGGED
   ##
   
   ceoflag <- ceoraw |> filter(flagged == "true")
   ceoflag
   
   table(ceoflag$pl_island)
   
   ##
   ## Write object for data analysis
   ##
   
   st_write(sf_ceo, "data/sf_ceo.gpkg", delete_dsn = TRUE)
   write_csv(ceo, "data/ceo.csv")
   write_csv(ceoqaqc, "data/ceo_QAQC.csv")
   
   rm(list = ls())
   
   
 }








