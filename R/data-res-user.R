

##
## CHECK USERS #################################################################
##

usr_island <- ceo |>
  select(island = pl_island, email) |>
  distinct() |>
  summarise(count = n(), .by = island) |>
  left_join(usr_island_planned, by = "island") |>
  mutate(
    perc = paste0(round(count / planned * 100), "%")
  )
#print(usr_island)


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

# avg_duration <- ceo |>
#   summarise(
#     n_plot = n(),
#     avg = mean(duration),
#     std = sd(duration),
#     min = min(duration),
#     max = max(duration),
#     .by = c(day_half, pl_island)
#   ) |>
#   mutate(
#     ci      = std / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
#     ci_perc = round(ci / avg * 100, 0)
#   ) |>
#   arrange(day_half)
# print(avg_duration)

# gr_avg_duration <- avg_duration |>
#   ggplot(aes(x = day_half, y = avg)) +
#   geom_line(aes(group = pl_island, color = pl_island)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avg - ci, ymax = avg + ci), width = 0.2) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   guides(x = guide_axis(n.dodge = 2)) +
#   facet_wrap(~pl_island) +
#   labs(
#     title = "Average time for plot completion",
#     x = "Session",
#     y = "Completion time (min)"
#   )
# print(gr_avg_duration)

# avg_user_duration <- ceo |>
#   summarise(
#     n_plot = n(),
#     avg = mean(duration),
#     std = sd(duration),
#     min = min(duration),
#     max = max(duration),
#     .by = c(date, pl_island, user_code)
#   ) |>
#   mutate(
#     ci      = std / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
#     ci_perc = round(ci / avg * 100, 0)
#   ) |>
#   arrange(date)
# avg_user_duration
# gr_user_duration <- avg_user_duration |>
#   mutate(user_group = paste0(pl_island, "-", user_code)) |>
#   #filter(day_half != "10-23 PM") |>
#   filter(date != "2023-10-23") |>
#   ggplot(aes(x = date, y = n_plot)) +
#   geom_col(aes(group = user_code, fill = user_code), position = position_dodge(0.9)) +
#   scale_fill_viridis_d() +
#   # geom_line(aes(group = user_group, color = user_group)) +
#   # geom_point() +
#   #geom_errorbar(aes(ymin = avg - ci, ymax = avg + ci), width = 0.2) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   guides(x = guide_axis(n.dodge = 2)) +
#   facet_wrap(~pl_island) +
#   #facet_wrap(~user_group) +
#   labs(
#     title = "Number of plots collected per user and per day",
#     x = "Session",
#     y = "Number of plots"
#   )
# print(gr_user_duration)

gr_user_behavior <- ceo |>
  arrange(user_group) |>
  filter(duration <= 20, date >= "2023-10-24") |>
  ggplot(aes(x = collection_time, y = duration, group = pl_island, color = pl_island)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~user_group, nrow = 5)
#print(gr_user_behavior)


avg_plot_user <- ceo |>
  summarise(
    n_plot = n(),
    .by = c(day_half, pl_island, user_code, user_group)
  )

avg_plot <- avg_plot_user |>
  summarise(
    n = n(),
    avg = mean(n_plot),
    std = sd(n_plot),
    min = min(n_plot),
    max = max(n_plot),
    .by = c(day_half, pl_island)
  ) |>
  mutate(
    ci      = std / sqrt(n) * round(qt(0.975, n-1), 2),
    ci_perc = round(ci / avg * 100, 0)
  ) |>
  arrange(day_half)


# gr_avg_user <- avg_plot_user |>
#   ggplot(aes(x = day_half, y = avg)) +
#   geom_line(aes(group = user_group, color = user_group)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = avg - ci, ymax = avg + ci), width = 0.2) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   guides(x = guide_axis(n.dodge = 2)) +
#   facet_wrap(~pl_island) +
#   labs(
#     title = "Average number of plot per session",
#     x = "Session",
#     y = "Number of plots"
#   )
# print(gr_avg_user)

gr_avg_plot <- avg_plot |>
  #filter(day_half != "10-25 PM") |>
  ggplot(aes(x = day_half, y = avg)) +
  geom_line(aes(group = pl_island, color = pl_island)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg - ci, ymax = avg + ci), width = 0.2) +
  theme_bw() +
  theme(legend.position = "none") +
  guides(x = guide_axis(n.dodge = 2)) +
  facet_wrap(~pl_island) +
  labs(
    title = "Average number of plot per session",
    x = "Session",
    y = "Number of plots"
  )
print(gr_avg_plot)




##
## CHECKS LAND USE #############################################################
##

# new_names |> str_subset("sub")


# basic treemap
tm_lucat <- treemap(
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
d3tm_lucat <- d3tree2( tm_lucat ,  rootname = "General")
d3tm_lucat


# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Compute the position of labels
data <- data %>%
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = group), color = "white", size=6)

Prop <- c(3,7,9)
pie(Prop , labels = c("Gr-A","Gr-B","Gr-C"), border="white", col=pal_luaccess)



##
## Spatial
##

session_done <- ceo %>% pull(day_half) |> unique()


gr_session_lu <- map(session_done, function(x){
  
  session_last     <- which(session_done == x)
  progress_session <- session_done[1:session_last]
  
  gr_out <- sf_ceo |>
    mutate(
      lu_session = if_else(day_half %in% progress_session, lu_cat, NA_character_)
    ) |>
    ggplot() +
    geom_sf(aes(fill = lu_session), color = NA) +
    # scale_fill_manual(values = c("#34675c", "#b7b8b6", "#b3c100", "#4cb5f5"), na.value = "grey20") +
    scale_fill_manual(values = c("#258039", "#f5be41", "#b3c100", "#31a9b8"), na.value = "grey95") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      title = paste0("Progress until: ",  x),
      fill = ""
    )
  
  #print(gr_out)
  gr_out
  
})



gr_session_access <- map(session_done, function(x){
  
  session_last     <- which(session_done == x)
  progress_session <- session_done[1:session_last]
  
  gr_out <- sf_ceo |>
    mutate(
      lu_session = if_else(day_half %in% progress_session, lu_access, NA_character_)
    ) |>
    ggplot() +
    geom_sf(aes(fill = lu_session), color = NA) +
    scale_fill_manual(values = pal_luaccess, na.value = "grey95") +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      title = paste0("Progress until: ",  x),
      fill = ""
    )
  
  #print(gr_out)
  gr_out
  
})


## Animate completion
min(sf_ceo$collection_time, na.rm = T)
max(sf_ceo$collection_time, na.rm = T)

# gr_lu_anim <- sf_ceo |>
#   mutate(
#     collection_hour = round_date(collection_time, "hours")
#   ) |>
#   ggplot(aes(fill = lu_access)) +
#   geom_sf(data = sf_country, fill = "grey95", color = NA) +
#   geom_sf(color = NA) +
#   scale_fill_manual(values = pal_luaccess, na.value = "grey95") +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Progress until: {current_frame}",
#     fill = ""
#     ) +
#   transition_manual(collection_hour, cumulative = TRUE)
#
# animate(gr_lu_anim, end_pause = 10, width = 800, height = 600)
# anim_save(filename = "progress-lu-access-1025pm.gif",  path = "data-raw")

