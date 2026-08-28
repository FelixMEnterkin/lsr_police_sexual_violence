


## Constricting Dichotomous Age Variable 
pas <- pas |>
  mutate(
    age_1 = if_else(age == 1, 1, 0),
    age_2 = if_else(age == 2, 1, 0),
    age_3 = if_else(age == 3, 1, 0),
    age_4 = if_else(age == 4, 1, 0),
    age_5 = if_else(age == 5, 1, 0),
    age_6 = if_else(age == 6, 1, 0)
  )

df <- pas |> filter(ethnicity > 2) |> filter(gender == 2)

attach(df)
rda1 <-
  rdrobust(
    y = age_1,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd")

rd_1 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_1) <- "Age 18-24"

rda1 <-
  rdrobust(
    y = age_2,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_12 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_12) <- "Age 25-34"


rda1 <-
  rdrobust(
    y = age_3,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_13 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_13) <- "Age 35-44"

rda1 <-
  rdrobust(
    y = age_4,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_14 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_14) <- "Age 45-54"


rda1 <-
  rdrobust(
    y = age_1,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_15 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_15) <- "Age 55-64"


rda1 <-
  rdrobust(
    y = age_6,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_16 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_16) <- "Age 65+"


rda2 <-
  rdrobust(
    y = employed,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_2 <- c(
  Estimate = round(rda2$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda2$ci[3, 1], 2), ", ", round(rda2$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda2$se[3, 1], 2),
  `p value` = round(rda2$pv[3, 1], 3),
  `N` = sum(rda2$N),
  `Effective N` = sum(rda2$N_h),
  `Bandwidth` = round(c(rda2$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_2) <- "Employed"



rda3 <-
  rdrobust(
    y = unemployed,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_3 <- c(
  Estimate = round(rda3$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda3$ci[3, 1], 2), ", ", round(rda3$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda3$se[3, 1], 2),
  `p value` = round(rda3$pv[3, 1], 3),
  `N` = sum(rda3$N),
  `Effective N` = sum(rda3$N_h),
  `Bandwidth` = round(c(rda3$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_3) <- "Unemployed"




rda1 <-
  rdrobust(
    y = parttime,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_4 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_4) <- "Partime"


rda1 <-
  rdrobust(
    y = student,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_5 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_5) <- "Student"

rda4 <-
  rdrobust(
    y = retired,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_6 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_6) <- "Retired"

rda4 <-
  rdrobust(
    y = victim,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_7 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_7) <- "Victim"

rda4 <-
  rdrobust(
    y = stopped,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_8 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 3),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_8) <- "Stopped"


tab_cov <- rbind(rd_1, rd_12, rd_13, rd_14, rd_15, rd_16, rd_2, rd_3, rd_4, rd_5, rd_6, rd_7, rd_8)
tab_cov

