library(ggplot2)

# two group
t <- weibo_week |>
  mutate(SH=as.factor(IP_May == "Shanghai")) |>
  group_by(week, SH )|>
  summarize(
  proportion = mean(proportion, na.rm = TRUE),
  .groups = 'drop')

legend_labels <- c("Shanghai", "Other Provinces (Control)")

# Plotting the trend in proportion for each region
ggplot(t, aes(x = week, y = proportion, color = SH)) +
  geom_line() +
  labs(title = "Parallel Trends in Proportion Across Regions",
       x = "Week",
       y = "Proportion of Nationalistic Posts") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(weibo_week$week), max(weibo_week$week), by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("black", "dark grey"), labels = legend_labels)
