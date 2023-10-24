
## Create tables for analysis and display

##
## Load data
##

## User's info
usr_info <- read_csv("data-raw/list of participants NFI-Phase1.csv")

## CEO raw data
ceosession <- "ceo-10-24-pm"
ceopath    <- list.files(file.path("data-raw", ceosession), pattern = "csv", full.names = T)

ceoraw <- map(ceopath, read_csv, col_types = cols(.default = "c")) |> list_rbind()
ceoraw

names(ceoraw)

sf_ceo <- ceoraw |>
  select(plotid, pl_fra_hexid, pl_island, lon, lat) |>
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

load("data-raw/sf_islands_simp.Rdata")

sf_country <- sf_islands_simp |> data.table::rbindlist() |> st_as_sf()

ggplot() +
  geom_sf(data = sf_country, aes(fill = island)) +
  geom_sf(data = sf_grid, fill = NA)
  
## List of workshop collection days
list_day     <- c("10-23", "10-24", "10-25", "10-26", "10-27")
list_halfday <- expand.grid(list_day, am_pm = c("AM", "PM")) |>
  mutate(day_half = paste(list_day, am_pm)) |>
  filter(day_half != "10-23 AM") |>
  pull(day_half) |>
  sort()



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

ceocomp <- ceoraw |> 
  filter(!is.na(email) & flagged == "false")

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
  filter(flagged == "false") |>
  arrange(pl_island, collection_time) |>
  distinct(plotid, .keep_all = T) |>
  left_join(usr_ano, by = "email") |>
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
  )



##
## Spatial joins ###############################################################
##

sf_grid2 <- sf_grid |>
  st_join(sf_ceo) |>
  mutate(
    plotid = as.numeric(plotid),
    pl_fra_hexid = as.numeric(pl_fra_hexid)
  )

# ggplot() +
#   geom_sf(data = sf_grid2, aes(fill = plotid)) +
#   scale_fill_viridis_c() +
#   theme_bw()

sf_progress <- sf_grid2 |>
  left_join(ceo, by = "plotid") 

ggplot() +
  geom_sf(data = sf_progress, aes(fill = lu_cat), color = NA)

session_done <- ceo %>% pull(day_half) |> unique()

sf_session <- map(session_done, function(x){
  
  session_last     <- which(session_done == x)
  progress_session <- session_done[1:session_last]
  
  gr_out <- sf_progress |>
    mutate(
      lu_session = if_else(day_half %in% progress_session, lu_cat, NA_character_)
    ) |>
    ggplot() +
    geom_sf(aes(fill = lu_session), color = NA) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      title = paste0("Progress until: ",  x),
      fill = ""
    ) 
  
  print(gr_out)
  gr_out
  
})




##
## CHECKS ######################################################################
##

nrow(ceocomp) == nrow(ceo) + n_dup
print(paste0("Percentage of revisted plots: ", round(n_dup / nrow(ceocomp) * 100), "%"))

## Check completion ------------------------------------------------------------
target_island <- ceoraw |> 
  distinct(plotid, .keep_all = T) |>
  summarise(target = n(), .by = pl_island)

target_session <- expand.grid(
  day_half = list_halfday,
  pl_island = target_island$pl_island
  ) |> 
  left_join(target_island, by = "pl_island") |>
  mutate(target = if_else(day_half == "10-23 PM", 0, ceiling(target / 8)))
target_session

progress_island <- ceo |>
  summarise(count = n(), .by = pl_island) |>
  left_join(target_island, by = "pl_island")

progress_all <- progress_island |>
  bind_rows(list(
    pl_island = "Total", 
    count = sum(progress_island$count), 
    target = sum(progress_island$target)
    )) |>
  mutate(perc = paste0(round(count / target * 100), "%"))
print(progress_all)

progress_session <- ceo |>
  summarise(count = n(), .by = c(day_half, pl_island)) |>
  full_join(target_session, by = c("day_half", "pl_island"))

target_session |>
  rename(count = target) |>
  ggplot(aes(x = day_half, y = count)) +
  geom_col(fill = "grey50") +
  geom_col(data = progress_session, aes(fill = pl_island), alpha = 0.9) +
  facet_wrap(~pl_island, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none") +
  guides(x = guide_axis(angle = 45)) +
  labs(
    title = "Number of analyzed plots per session and island group",
    x = "",
    y = "CEO plot count"
  )


## Check group
usr_island <- ceo |>
  select(island = pl_island, email) |>
  distinct() |>
  summarise(count = n(), .by = island) |>
  left_join(usr_island_planned, by = "island") |>
  mutate(
    perc = paste0(round(count / planned * 100), "%")
  )
print(usr_island)

## Check measurement time
# ceo |>
#   filter(duration <= 20) |>
#   ggplot(aes(x = land_use_2023_centroid, y = duration)) +
#   geom_boxplot(aes(fill = land_use_2023_centroid), outlier.shape = NA) +
#   geom_jitter(alpha = 0.3, size = 1) +
#   scale_fill_manual(values = c("#34675c", "#b7b8b6", "#b3c100", "#4cb5f5")) +
#   facet_wrap(~pl_island, nrow = 2) +
#   guides(x = guide_axis(angle = 45)) +
#   theme_bw() +
#   theme(legend.position = "none")

ceo |>
  filter(duration <= 20) |>
  ggplot(aes(x = land_use_2023_centroid, y = duration)) + 
  geom_violin(aes(fill = land_use_2023_centroid), trim = FALSE) +
  scale_fill_manual(values = c("#34675c", "#b7b8b6", "#b3c100", "#4cb5f5")) +
  facet_wrap(~pl_island, nrow = 2) +
  guides(x = guide_axis(angle = 45)) +
  theme_bw() +
  theme(legend.position = "none")

avg_duration <- ceo |>
  summarise(
    n_plot = n(),
    avg = mean(duration),
    std = sd(duration),
    min = min(duration),
    max = max(duration),
    .by = c(day_half, pl_island)
  ) |>
  mutate(
    ci      = std / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / avg * 100, 0)
  ) |>
  arrange(day_half)
print(avg_duration)

avg_duration2 |>
  ggplot(aes(x = day_half, y = avg)) +
  geom_line(aes(group = pl_island, color = pl_island)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = avg - ci, ymax = avg + ci), width = 0.2) +
  theme_bw() +
  facet_wrap(~pl_island)

ceo |>
  mutate(user_group = paste0(pl_island, "-", user_code)) |>
  arrange(user_group) |>
  filter(duration <= 20, date >= "2023-10-24") |>
  ggplot(aes(x = collection_time, y = duration, group = pl_island, color = pl_island)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~user_group, nrow = 5)



##
## Areas 
## 

# new_names |> str_subset("sub")


# basic treemap
p <- treemap(
  dtf = cbind(ceo, value = 1), 
  index = c("lu_cat","lu_sub"),
  vSize = "value",
  type = "index",
  palette = "Set2",
  bg.labels=c("white"),
  align.labels=list(
    c("center", "center"), 
    c("right", "bottom")
  )
)


# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2( p ,  rootname = "General" )
inter



##
## Spatial 
##




