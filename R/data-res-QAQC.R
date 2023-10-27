

## 
## CHECK ONE PERSON
##
unique(ceoqaqc$email)

plot_check <- ceoqaqc |> filter(email == "ganis.ratna@gmail.com") |> pull(plotid)
ceoqaqc2 <- ceoqaqc |> filter(plotid %in% plot_check, lu_cat == "Forest")


##
## Checks QAQC
##

disa_lucat <- ceoqaqc |>
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

disa_lusub <- ceoqaqc |>
  select(pl_island, plotid, lu_sub) |>
  distinct() |>
  summarise(count = n(), .by = c(pl_island, plotid)) |>
  arrange(pl_island, plotid) |>
  mutate(disa = if_else(count == 1, "agree", "disagree")) |>
  distinct(plotid, .keep_all = T)

gr_disa_lusub <- disa_lusub |>
  summarise(count = n(), .by = c(pl_island, disa)) |>
  ggplot(aes(x = pl_island, y = count)) +
  geom_bar(aes(fill = disa), position = "fill" , stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = c("#78c2ad", "#ff7851")) +
  labs(title = "Disagreement on land use sub-categories")
print(gr_disa_lusub)

disa_lusub_stat <- disa_lusub |>
  summarise(count = n(), .by = c(pl_island, disa)) |>
  pivot_wider(names_from = disa, values_from = count) |>
  mutate(total = agree + disagree) %>%
  add_row(pl_island = "Total", summarise(., across(where(is.numeric), sum))) |>
  mutate(perc  = paste(round(disagree / total * 100), "%")
  )
disa_lusub_stat

disa_lucat_id <- disa_lucat |> filter(disa == "disagree") |> pull(plotid) 
disa_lusub_id <- disa_lusub |> filter(disa == "disagree") |>pull(plotid) 

disa_lusub2 <- ceoqaqc |>
  filter(lu_cat == "Forest") |>
  select(pl_island, plotid, lu_sub) |>
  distinct() |>
  summarise(count = n(), .by = c(pl_island, plotid)) |>
  arrange(pl_island, plotid) |>
  mutate(disa = if_else(count == 1, "agree", "disagree")) |>
  distinct(plotid, .keep_all = T)

disa_lusub2_id <- disa_lusub2 |> filter(disa == "disagree") |>pull(plotid) 

disa_lucat_where <- ceoqaqc |>
  filter(plotid %in% disa_lucat_id) |>
  select(plotid, pl_island, lu_cat) |>
  arrange(pl_island, plotid) %>%
  mutate(obs = paste0("obs", rep(1:2, nrow(.)/2))) |>
  pivot_wider(values_from = lu_cat, names_from = obs)
disa_lucat_where

gr_disa_lucat_where <- disa_lucat_where |>
  summarise(count = n(), .by = c(obs1, obs2)) |>
  ggplot(aes(x = obs1, y = obs2, fill = count)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) + 
  theme_bw()
gr_disa_lucat_where


disa_lusub_where <- ceoqaqc |>
  filter(plotid %in% disa_lusub2_id) |>
  select(plotid, pl_island, lu_sub) |>
  arrange(pl_island, plotid) %>%
  mutate(obs = paste0("obs", rep(1:2, nrow(.)/2))) |>
  pivot_wider(values_from = lu_sub, names_from = obs)
disa_lusub_where

gr_disa_lusub_where <- disa_lusub_where |>
  summarise(count = n(), .by = c(obs1, obs2)) |>
  ggplot(aes(x = obs1, y = obs2, fill = count)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) + 
  theme_bw() +
  guides(x = guide_axis(angle = 45))
gr_disa_lusub_where
