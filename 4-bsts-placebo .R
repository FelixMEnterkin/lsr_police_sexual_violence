

library(CausalImpact)


df <- pas  |> filter(ethnicity <= 2)  |> filter(gender == 1)

df <- df |>
  dplyr::select(
    trust_lat, 
    crime, 
    interview_date, 
  ) |> na.omit()

df <- df |>
  group_by(date = floor_date(interview_date, unit = "month")) |>
  summarise(
    y = mean(trust_lat, na.rm = TRUE),
    xc= mean(crime, na.rm = TRUE),
    #xe = mean(effective, na.rm = TRUE),
    .groups = "drop")

Y <- df$y
xc <- df$xc
X <- df$date |> as.Date()

# craete tiome series object
data <- zoo(cbind(Y,xc), X)

# null effect - halfway thriough treatment perion
pre.period <- as.Date(c("2015-01-01", "2018-03-14"))
post.period <- as.Date(c("2018-03-15", "2021-03-10"))

# estimate causal forcast 
impact <- CausalImpact(data, pre.period, post.period)

summary(impact)

a <- plot(impact, "original") + 
  ggtitle("White Men") +
  ylab("Trustworthiness") +
  scale_y_continuous(limits = c(-0.3,0.25)) +
  geom_vline(data = data.frame(date = as.Date("2021-03-1")), aes(xintercept = date), linetyple = "solid") +
  theme_bw()
a 


df <- pas  |> filter(ethnicity <= 2)  |> filter(gender == 2)

df <- df |>
  dplyr::select(
    trust_lat, 
    crime, 
    interview_date, 
  ) |> na.omit()

df <- df |>
  group_by(date = floor_date(interview_date, unit = "month")) |>
  summarise(
    y = mean(trust_lat, na.rm = TRUE),
    xc= mean(crime, na.rm = TRUE),
    #xe = mean(effective, na.rm = TRUE),
    .groups = "drop")

Y <- df$y
xc <- df$xc
X <- df$date |> as.Date()

# craete tiome series object
data <- zoo(cbind(Y,xc), X)

# null effect - halfway thriough treatment perion
pre.period <- as.Date(c("2015-01-01", "2018-03-14"))
post.period <- as.Date(c("2018-03-15", "2021-03-10"))

# estimate causal forcast 
impact <- CausalImpact(data, pre.period, post.period)

summary(impact)

b <- plot(impact, "original") + 
  ggtitle("White Women") +
  ylab("Trustworthiness") +
  scale_y_continuous(limits = c(-0.3,0.25)) +
  geom_vline(data = data.frame(date = as.Date("2021-03-1")), aes(xintercept = date), linetyple = "solid") +
  theme_bw()
b 


df <- pas  |> filter(ethnicity > 2)  |> filter(gender == 1)

df <- df |>
  dplyr::select(
    trust_lat, 
    crime, 
    interview_date, 
  ) |> na.omit()

df <- df |>
  group_by(date = floor_date(interview_date, unit = "month")) |>
  summarise(
    y = mean(trust_lat, na.rm = TRUE),
    xc= mean(crime, na.rm = TRUE),
    #xe = mean(effective, na.rm = TRUE),
    .groups = "drop")

Y <- df$y
xc <- df$xc
X <- df$date |> as.Date()

# craete tiome series object
data <- zoo(cbind(Y,xc), X)

# null effect - halfway thriough treatment perion
pre.period <- as.Date(c("2015-01-01", "2018-03-14"))
post.period <- as.Date(c("2018-03-15", "2021-03-10"))

# estimate causal forcast 
impact <- CausalImpact(data, pre.period, post.period)

summary(impact)

c <- plot(impact, "original") + 
  ggtitle("Non-White Men") +
  ylab("Trustworthiness") +
  scale_y_continuous(limits = c(-0.3,0.25)) +
  geom_vline(data = data.frame(date = as.Date("2021-03-1")), aes(xintercept = date), linetyple = "solid") +
  theme_bw()
c 


df <- pas  |> filter(ethnicity > 2)  |> filter(gender == 1)

df <- df |>
  dplyr::select(
    trust_lat, 
    crime, 
    interview_date, 
  ) |> na.omit()

df <- df |>
  group_by(date = floor_date(interview_date, unit = "month")) |>
  summarise(
    y = mean(trust_lat, na.rm = TRUE),
    xc= mean(crime, na.rm = TRUE),
    #xe = mean(effective, na.rm = TRUE),
    .groups = "drop")

Y <- df$y
xc <- df$xc
X <- df$date |> as.Date()

# craete tiome series object
data <- zoo(cbind(Y,xc), X)

# specify periods 
# null effect - halfway thriough treatment perion
pre.period <- as.Date(c("2015-01-01", "2018-03-14"))
post.period <- as.Date(c("2018-03-15", "2021-03-10"))

# estimate causal forcast 
impact <- CausalImpact(data, pre.period, post.period)

summary(impact)

d <- plot(impact, "original") + 
  ggtitle("Non-White Women") +
  ylab("Trustworthiness") +
  scale_y_continuous(limits = c(-0.3,0.25)) +
  geom_vline(data = data.frame(date = as.Date("2021-03-1")), aes(xintercept = date), linetyple = "solid") +
  theme_bw() 
d 






p <- cowplot::plot_grid(
  a,
  b,
  c,
  d,
  ncol = 2, 
  labels = c("A", "B", "C", "D"))

p

