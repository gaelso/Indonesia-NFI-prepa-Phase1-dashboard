


##
## CHECK PROGRESS ##############################################################
##

progress_island <- ceo |>
  summarise(count = n(), .by = pl_island) |>
  left_join(target_island, by = "pl_island")

progress_all <- progress_island |>
  bind_rows(list(
    pl_island = "Total", 
    count = sum(progress_island$count), 
    target = sum(progress_island$target)
  )) |>
  mutate(
    perc = paste0(round(count / target * 100), "%"),
    remaining = target - count
  )
print(progress_all)

gr_progress_all <- progress_all |>
  filter(pl_island != "Total") |>
  ggplot(aes(y = pl_island)) +
  geom_col(aes(x = target), fill = "#ff7851") +
  geom_col(aes(x = count), fill = "#78c2ad") +
  theme_void() +
  theme(axis.text.y = element_text())
print(gr_progress_all)

gr_progress_all2 <- progress_all |>
  pivot_longer(cols = c(count, remaining), names_to = "progress", values_to = "count") |>
  mutate(progress = factor(progress, levels = c("remaining", "count"))) |>
  ggplot(aes(x = count, y = pl_island)) +
  geom_bar(aes(fill = progress), position = "fill" , stat = "identity") +
  scale_fill_manual(values = c("#ff7851", "#78c2ad")) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(0, 1, 0.1))
print(gr_progress_all2)

progress_session <- ceo |>
  summarise(count = n(), .by = c(day_half, pl_island)) |>
  full_join(target_session, by = c("day_half", "pl_island")) |>
  arrange(pl_island, day_half)

# gr_target_session <- target_session |>
#   rename(count = target) |>
#   ggplot(aes(x = day_half, y = count)) +
#   geom_col(fill = "grey50") +
#   geom_col(data = progress_session, aes(fill = pl_island), alpha = 0.9) +
#   facet_wrap(~pl_island, nrow = 2) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   scale_fill_viridis_d() +
#   guides(x = guide_axis(angle = 45)) +
#   labs(
#     title = "Number of analyzed plots per session and island group",
#     x = "",
#     y = "CEO plot count"
#   )
# print(gr_target_session)


## Accessible plots
table(ceo$lu_access)

progress_access <- ceo |>
  filter(lu_access == "Forest-access") |>
  summarise(count = n(), .by = "pl_island") |>
  left_join(target_phase2, by = "pl_island") |>
  mutate(
    target_ph2x2 = target_ph2 * 2,
    target_ph2x3 = target_ph2 * 3,
    target_ph2x4 = target_ph2 * 4,
    target_color = case_when(
      count >= target_ph2x2 ~ "#78c2ad",
      count >= target_ph2 ~ "#f3969a",
      TRUE ~ "#ff7851",
    )
  )
progress_access  

gr_progress_access <- progress_access |>
  ggplot(aes(y = pl_island)) +
  geom_col(aes(x = count, fill = target_color)) +
  geom_boxplot(aes(x = target_ph2), color = "darkred") +
  geom_boxplot(aes(x = target_ph2x2), color = "darkgreen") +
  scale_fill_manual(values = c("#78c2ad", "#f3969a", "#ff7851")) +
  theme_void() +
  theme(
    axis.text.y = element_text(), 
    legend.position = "none", 
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
    ) +
  labs(
    title = "Number of forest accessible plots vs NFI Phase 2 target",
    caption = "red marker: NFI Phase 2 target \ngreen marker: 2 x NFI Phase 2 target"
  )
print(gr_progress_access)

table(ceo$pl_island, ceo$lu_access)

progress_access2 <- ceo |>
  filter(lu_access == "Forest-access") |>
  summarise(count = n(), .by = "pl_island") |>
  left_join(target_phase2v2, by = "pl_island") |>
  mutate(
    target_ph2x2 = target_ph2 * 2,
    target_ph2x3 = target_ph2 * 3,
    target_ph2x4 = target_ph2 * 4,
    target_color = case_when(
      count >= target_ph2x2 ~ "#78c2ad",
      count >= target_ph2 ~ "#f3969a",
      TRUE ~ "#ff7851",
    )
  )
progress_access2

gr_progress_access2 <- progress_access2 |>
  ggplot(aes(y = pl_island)) +
  geom_col(aes(x = count, fill = target_color)) +
  geom_boxplot(aes(x = target_ph2), color = "darkred") +
  geom_boxplot(aes(x = target_ph2x2), color = "darkgreen") +
  scale_fill_manual(values = c("#78c2ad", "#f3969a", "#ff7851")) +
  theme_void() +
  theme(
    axis.text.y = element_text(), 
    legend.position = "none", 
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    title = "Number of forest accessible plots vs NFI Phase 2 target",
    caption = "red marker: NFI Phase 2 target \ngreen marker: 2 x NFI Phase 2 target"
  )
print(gr_progress_access2)

# GIFs
tt <- ceo |>
  #filter(date == "2023-10-24")  |>
  mutate(
    collection_hour = round_date(collection_time, "0.25 hours")
  ) |>
  summarise(count = n(), .by = c(pl_island, collection_hour)) |>
  left_join(target_island, by = "pl_island") |>
  group_by(pl_island) |>
  mutate(
    cumsum = cumsum(count),
    cumsum_perc = cumsum/target,
    target_time = if_else(cumsum == target, target, NA_real_),
    ) |>
  ungroup()

# tt$collection_hour[1:10]

tt |>
  ggplot(aes(x = collection_hour, y = cumsum, color = pl_island)) +
  geom_point() +
  geom_line(aes(group = pl_island)) +
  geom_point(aes(y = target_time), size = 6) +
  scale_x_datetime(
    labels = scales::date_format("%Y-%m-%d %H"),
    date_breaks = "12 hours",
    limits = as_datetime(c("2023-10-23 12:00:00", "2023-10-27 18:00:00"))
  ) +
  guides(x = guide_axis(angle = 45)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    x = "", y= "", color = ""
  )

# 
# gr_tt <- tt |> 
#   ggplot() +
#   geom_col(aes(y = pl_island, x = cumsum_perc, fill = pl_island)) +
#   labs(title = "Progress until: {current_frame}") +
#   transition_manual(collection_hour, cumulative = TRUE) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# animate(gr_tt, end_pause = 30, width = 800, height = 600)
# anim_save(filename = "progress-island-1027am.gif",  path = "data-raw")


