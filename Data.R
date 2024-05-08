library(tidyverse)

data <- read_csv("Weibo_1215.csv")

weibo <- data %>%
  filter(IP_May %in% c("Shanghai","Anhui","Chongqing","Guizhou","Hainan","Henan","Hunan","Jiangxi", "Shanxi","Sichuan","Hubei","Ningxia","Qinghai","Tibet","Xinjiang","Yunnan")) |>
  select(date, weibo_id, Nationalistic, user_id, female, verified, user_fans_count, IP_May, SH) %>%
  distinct() %>%
  mutate(
    Nationalistic_binary = (Nationalistic > 0.3),
    week = as.numeric(format(date - 1, "%U"))
  )

weibo_week <- weibo |>
  group_by(week, user_id, IP_May) |>
  summarize(
    female = mean(female, na.rm = TRUE),
    verified = mean(verified, na.rm = TRUE),
    user_fans_count = mean(user_fans_count, na.rm = TRUE),
    count = sum(Nationalistic_binary, na.rm = TRUE),
    proportion = mean(Nationalistic_binary, na.rm = TRUE),
    .groups = 'drop') |>
  group_by(week,IP_May) |>
  summarize(
    female = mean(female, na.rm = TRUE),
    verified = mean(verified, na.rm = TRUE),
    user_fans_count = mean(user_fans_count, na.rm = TRUE),
    count = mean(count, na.rm = TRUE),
    proportion = mean(proportion, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  select(IP_May, week, proportion,female, verified,user_fans_count ) |>
  mutate(lockdown_week=as.numeric(week >= 13 & (IP_May == "Shanghai")),
         first.treat=as.numeric(IP_May == "Shanghai")*13,
         IP_May_numeric = as.numeric(as.factor(IP_May)))
