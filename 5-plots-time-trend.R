################################################################################
### Time Trends 
################################################################################


a <- pas |>
  filter(gender == 1) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
    summarise(y = mean(trust_lat, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(-.5,.2))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("White Men") +
  theme_bw()

b <- pas |>
  filter(gender == 2) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_lat, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(-.5,.2))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("White Women") +
  theme_bw()

c <- pas |>
  filter(gender == 1) |>
  filter(ethnicity > 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_lat, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(-.5,.2))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Men") +
  theme_bw()

d <- pas |>
  filter(gender == 2) |>
  filter(ethnicity > 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_lat, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(-.5,.2))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Women") +
  theme_bw()


p <- cowplot::plot_grid(
  a,
  b,
  c,
  d,
  ncol = 2, 
  labels = c("A", "B", "C", "D"))

p







pas |>
  filter(gender == 2) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "year")) |>
  group_by(time) %>%
  summarise(y = mean(trust_lat, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_line() +
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Women") +
  theme_bw()


pas |>
  filter(gender == 2) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "quarter")) |>
  group_by(time) %>%
  summarise(y = mean(trust_lat, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_line() +
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Women") +
  theme_bw()



################################################################################
### Trust - Single Survey Iten
################################################################################

a <- pas |>
  filter(gender == 1) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(3,4.5))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("White Men") +
  theme_bw()

b <- pas |>
  filter(gender == 2) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(3,4.5))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("White Women") +
  theme_bw()

c <- pas |>
  filter(gender == 1) |>
  filter(ethnicity > 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(3,4.5))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Men") +
  theme_bw()

d <- pas |>
  filter(gender == 2) |>
  filter(ethnicity > 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(3,4.5))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Women") +
  theme_bw()


p <- cowplot::plot_grid(
  a,
  b,
  c,
  d,
  ncol = 2, 
  labels = c("A", "B", "C", "D"))

p


################################################################################
### Binary Trust
################################################################################


pas <- pas |>
  mutate(
    trust_bin = if_else(trust > 3, 1, 0)
  )


a <- pas |>
  filter(gender == 1) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_bin, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(.5,1))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("White Men") +
  theme_bw()

b <- pas |>
  filter(gender == 2) |>
  filter(ethnicity <= 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_bin, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(.5,1))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("White Women") +
  theme_bw()

c <- pas |>
  filter(gender == 1) |>
  filter(ethnicity > 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_bin, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(.5,1))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Men") +
  theme_bw()

d <- pas |>
  filter(gender == 2) |>
  filter(ethnicity > 2) |> 
  mutate(time = ceiling_date(interview_date, "month")) |>
  group_by(time) %>%
  summarise(y = mean(trust_bin, na.rm = TRUE)) %>%
  ungroup() |>
  ggplot(aes(x = time, y = y)) +
  geom_point() +
  ylim(c(.5,1))+
  geom_vline(data = data.frame(date = as.Date("2021-03-10")), aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "darkgrey") +
  ylab("") +
  xlab("Time (Months)") +
  ggtitle("Non-White Women") +
  theme_bw()


p <- cowplot::plot_grid(
  a,
  b,
  c,
  d,
  ncol = 2, 
  labels = c("A", "B", "C", "D"))

p
