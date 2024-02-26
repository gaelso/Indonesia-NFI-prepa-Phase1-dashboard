

## 
## CHECK ONE PERSON
##
# 
# unique(ceoqaqc$email)
# 
# plot_check <- ceoqaqc |> filter(email == "ganis.ratna@gmail.com") |> pull(plotid)
# ceoqaqc2 <- ceoqaqc |> filter(plotid %in% plot_check, lu_cat == "Forest")

##
## check repetitions
##

tt <- ceoqaqc |> summarise(count = n(), .by = plotid)
table(tt$count)

plot_dup <- tt |> filter(count == 2) |> pull(plotid)

##
## Checks QAQC
##

ceodup <- ceoqaqc |> filter(plotid %in% plot_dup)

## Check global disagreements
names(ceodup)

tt <- ceodup |>
  select(
    -email, -collection_time, -analysis_duration, 
    -date, -month, -day, -day_txt, -day_half, -duration
    ) |>
  distinct()

tt2 <- tt |> summarise(count = n(), .by = c(pl_island, plotid))
table(tt2$pl_island, tt2$count)

## Check disa LVL1
tt <- ceodup |>
  select(plotid, pl_island, lu_cat) |>
  distinct() 

tt2 <- tt |> summarise(count = n(), .by = c(pl_island, plotid))
table(tt2$pl_island, tt2$count)

## Check disa LVL2
tt <- ceodup |>
  select(plotid, pl_island, lu_sub) |>
  distinct() 

tt2 <- tt |> summarise(count = n(), .by = c(pl_island, plotid))
table(tt2$pl_island, tt2$count)

## Check change
tt <- ceodup |>
  select(plotid, pl_island, lu_change) |>
  distinct() 

tt2 <- tt |> summarise(count = n(), .by = c(pl_island, plotid))
table(tt2$pl_island, tt2$count)



##
## Check cause of disa
##

disa_lucat_where <- ceodup |>
  select(plotid, pl_island, lu_cat) |>
  arrange(pl_island, plotid) |>
  mutate(obs = paste0("obs", rep(1:2, length(plot_dup)))) |>
  pivot_wider(values_from = lu_cat, names_from = obs)
disa_lucat_where
write_csv(disa_lucat_where, "data/disa_lucat_where.csv")

table(disa_lucat_where$obs1, disa_lucat_where$obs2)


## Change subcat
disa_lusub_where <- ceodup |>
  select(plotid, pl_island, lu_sub) |>
  arrange(pl_island, plotid) |>
  mutate(obs = paste0("obs", rep(1:2, length(plot_dup)))) |>
  pivot_wider(values_from = lu_sub, names_from = obs)
disa_lusub_where
write_csv(disa_lusub_where, "data/disa_lusub_where.csv")

table(disa_lusub_where$obs1, disa_lusub_where$obs2)


# 
tt <- ceodup |>
  select(plotid, pl_island, lu_cat)

disa_lucat <- ceoqaqc |>
  filter(plotid %in% plot_dup) |>
  select(pl_island, plotid, lu_cat) |>
  distinct() |>
  summarise(count = n(), .by = c(pl_island, plotid)) |>
  arrange(pl_island, plotid) |>
  mutate(disa = if_else(count == 1, "agree", "disagree")) |>
  distinct(plotid, .keep_all = T)

gr_disa_lucat <- disa_lucat |>
  summarise(count = n(), .by = c(pl_island, disa)) |>
  ggplot(aes(x = pl_island, y = count)) +
  geom_bar(aes(fill = disa), position = "fill" , stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = c("#78c2ad", "#ff7851")) +
  labs(
    title = "Percentage disagreement on Land use 2023 centroid"
  )
print(gr_disa_lucat)

disa_lucat_stat <- disa_lucat |>
  summarise(count = n(), .by = c(pl_island, disa)) |>
  pivot_wider(names_from = disa, values_from = count) |>
  mutate(total = agree + disagree) %>%
  add_row(pl_island = "Total", summarise(., across(where(is.numeric), sum))) |>
  mutate(perc  = paste(round(disagree / total * 100), "%")
  )
disa_lucat_stat
# 
# disa_lusub <- ceoqaqc |>
#   filter(plotid %in% plot_dup) |>
#   select(pl_island, plotid, lu_sub) |>
#   distinct() |>
#   summarise(count = n(), .by = c(pl_island, plotid)) |>
#   arrange(pl_island, plotid) |>
#   mutate(disa = if_else(count == 1, "agree", "disagree")) |>
#   distinct(plotid, .keep_all = T)
# 
# gr_disa_lusub <- disa_lusub |>
#   summarise(count = n(), .by = c(pl_island, disa)) |>
#   ggplot(aes(x = pl_island, y = count)) +
#   geom_bar(aes(fill = disa), position = "fill" , stat = "identity") +
#   theme_bw() +
#   scale_fill_manual(values = c("#78c2ad", "#ff7851")) +
#   labs(title = "Disagreement on land use sub-categories")
# print(gr_disa_lusub)
# 
# disa_lusub_stat <- disa_lusub |>
#   summarise(count = n(), .by = c(pl_island, disa)) |>
#   pivot_wider(names_from = disa, values_from = count) |>
#   mutate(total = agree + disagree) %>%
#   add_row(pl_island = "Total", summarise(., across(where(is.numeric), sum))) |>
#   mutate(perc  = paste(round(disagree / total * 100), "%")
#   )
# disa_lusub_stat
# 
# disa_lucat_id <- disa_lucat |> filter(disa == "disagree") |> pull(plotid) 
# disa_lusub_id <- disa_lusub |> filter(disa == "disagree") |>pull(plotid) 
# 
# disa_lusub2 <- ceoqaqc |>
#   filter(lu_cat == "Forest") |>
#   select(pl_island, plotid, lu_sub) |>
#   distinct() |>
#   summarise(count = n(), .by = c(pl_island, plotid)) |>
#   arrange(pl_island, plotid) |>
#   mutate(disa = if_else(count == 1, "agree", "disagree")) |>
#   distinct(plotid, .keep_all = T)
# 
# disa_lusub2_id <- disa_lusub2 |> filter(disa == "disagree") |>pull(plotid) 
# 
# disa_lucat_where <- ceoqaqc |>
#   filter(plotid %in% disa_lucat_id) |>
#   select(plotid, pl_island, lu_cat) |>
#   arrange(pl_island, plotid) |>
#   mutate(obs = paste0("obs", rep(1:2, length(disa_lucat_id)))) |>
#   pivot_wider(values_from = lu_cat, names_from = obs)
# disa_lucat_where
# write_csv(disa_lucat_where, "data/disa_lucat_where.csv")
# 
# table(disa_lucat_where$obs1, disa_lucat_where$obs2)
# 
# gr_disa_lucat_where <- disa_lucat_where |>
#   summarise(count = n(), .by = c(obs1, obs2)) |>
#   ggplot(aes(x = obs1, y = obs2, fill = count)) +
#   geom_tile() +
#   scale_fill_viridis_c(direction = -1) +
#   theme_bw()
# gr_disa_lucat_where
# 
# 
# disa_lusub_where <- ceoqaqc |>
#   filter(plotid %in% disa_lusub2_id) |>
#   select(plotid, pl_island, lu_sub) |>
#   arrange(pl_island, plotid)
# 
# disa_lusub_where <- disa_lusub_where |>
#   mutate(obs = paste0("obs", rep(1:2, nrow(disa_lusub_where)/2))) |>
#   pivot_wider(values_from = lu_sub, names_from = obs)
# disa_lusub_where
# 
# gr_disa_lusub_where <- disa_lusub_where |>
#   summarise(count = n(), .by = c(obs1, obs2)) |>
#   ggplot(aes(x = obs1, y = obs2, fill = count)) +
#   geom_tile() +
#   scale_fill_viridis_c(direction = -1) + 
#   theme_bw() +
#   guides(x = guide_axis(angle = 45))
# gr_disa_lusub_where

