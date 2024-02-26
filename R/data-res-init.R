

ceo_init <- read_csv("data/ceo.csv", col_types = cols(.default = "c"))
ceoqaqc  <- read_csv("data/ceo_QAQC.csv", show_col_types = F)
sf_ceo   <- st_read("data/sf_ceo.gpkg", quiet = T)

load("data-raw/sf_islands_simp.Rdata")
sf_country <- sf_islands_simp |> data.table::rbindlist() |> st_as_sf()

anci <- read_csv("data-raw/sample_statistics-PabloMartin.csv")

## Sort data types for ceo 
ceo <- ceo_init |>
  mutate(
    plotid = as.numeric(plotid),
    collection_time = as.POSIXct(collection_time, tz = "GMT"),
    collection_time = with_tz(collection_time, tz = "Asia/Makassar"),
    date     = date(collection_time),
    year     = year(collection_time),
    month    = month(collection_time),
    day      = day(collection_time),
    day_txt  = paste0(month, "-", day(collection_time)),
    day_half = if_else(am(collection_time), paste0(month, "-", day, " AM"), paste0(month, "-", day, " PM")),
    duration = abs(round(as.numeric(str_remove(analysis_duration, " secs")) / 60, 2))
  )


## Palettes
pal_greens   <- c("#265c00", "#68a225", "#b3be81")
pal_lucat    <- c("#258039", "#f5be41", "#b3c100", "#31a9b8")
pal_luaccess <- c("#258039", "#cf3721", "#f5be41")
pal_bsmint   <- c("#78c2ad", "#ff7851")

## List of workshop collection days
list_day     <- c("10-23", "10-24", "10-25", "10-26", "10-27")
list_halfday <- expand.grid(list_day, am_pm = c("AM", "PM")) |>
  mutate(day_half = paste(list_day, am_pm)) |>
  filter(day_half != "10-23 AM") |>
  pull(day_half) |>
  sort()

## Targets
target_island <- sf_ceo |>
  as_tibble() |>
  summarise(target = n(), .by = pl_island)

target_session <- expand.grid(
  day_half = list_halfday,
  pl_island = target_island$pl_island
) |> 
  left_join(target_island, by = "pl_island") |>
  mutate(target = if_else(day_half == "10-23 PM", 0, ceiling(target / 8)))

target_phase2 <- tibble(
  pl_island = target_island$pl_island,
  target_ph2 = c(476, 148, 413, 219, 257, 486, 386)
)
target_phase2


target_phase2v2 <- tibble(
  pl_island = target_island$pl_island,
  target_ph2 = c(119, 148, 413, 219, 257, 486, 386)
)
target_phase2v2


tt <- sf_ceo |>
  as_tibble() |>
  filter(is.na(email), pl_island %in% c("Papua", "Sulawesi"))


