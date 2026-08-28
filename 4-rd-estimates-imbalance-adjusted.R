###############################################################################
####### RD estimates Table Cooperation and Trust - Covariate Adjusted
################################################################################



pas <- pas |>
  mutate(
    age_1 = if_else(age == 1, 1, 0),
    age_2 = if_else(age == 2, 1, 0),
    age_3 = if_else(age == 3, 1, 0),
    age_4 = if_else(age == 4, 1, 0),
    age_5 = if_else(age == 5, 1, 0),
    age_6 = if_else(age == 6, 1, 0)
  )


### coop


df <- pas |> filter(ethnicity <= 2) |> filter(gender == 1)

attach(df)

rda1 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    covs = age_1 + age_3 + age_4 + age_6,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_1 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 4),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_1) <- "White Men"



df <- pas |> filter(ethnicity <= 2)  |> filter(gender == 2)

attach(df)

rda2 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    covs = age_3 + employed,
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
  `p value` = round(rda2$pv[3, 1], 4),
  `N` = sum(rda2$N),
  `Effective N` = sum(rda2$N_h),
  `Bandwidth` = round(c(rda2$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_2) <- "White Women"



df <- pas |> filter(ethnicity > 2)  |> filter(gender == 1)

attach(df)

rda3 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    covs = age_6 + employed,
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
  `p value` = round(rda3$pv[3, 1], 4),
  `N` = sum(rda3$N),
  `Effective N` = sum(rda3$N_h),
  `Bandwidth` = round(c(rda3$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_3) <- "Non-White Men"


df <- pas |> filter(ethnicity > 2) |> filter(gender == 2)

attach(df)

rda4 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    covs = age_1,
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
  `p value` = round(rda1$pv[3, 1], 4),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_4) <- "Non-White Women"

tab_coop <- rbind(rd_1, rd_2, rd_3, rd_4)
tab_coop



### trust 


df <- pas |> filter(ethnicity <= 2) |> filter(gender == 1)

attach(df)

rda1 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    covs = age_1 + age_3 + age_4 + age_6,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_1 <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 4),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_1) <- "White Men"



df <- pas |> filter(ethnicity <= 2)  |> filter(gender == 2)

attach(df)

rda2 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    covs = age_3 + employed,
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
  `p value` = round(rda2$pv[3, 1], 4),
  `N` = sum(rda2$N),
  `Effective N` = sum(rda2$N_h),
  `Bandwidth` = round(c(rda2$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_2) <- "White Women"



df <- pas |> filter(ethnicity > 2)  |> filter(gender == 1)

attach(df)

rda3 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    covs = age_6 + employed,
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
  `p value` = round(rda3$pv[3, 1], 4),
  `N` = sum(rda3$N),
  `Effective N` = sum(rda3$N_h),
  `Bandwidth` = round(c(rda3$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_3) <- "Non-White Men"


df <- pas |> filter(ethnicity > 2) |> filter(gender == 2)

attach(df)

rda4 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    covs = age_1,
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
  `p value` = round(rda1$pv[3, 1], 4),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_4) <- "Non-White Women"

tab_trust  <- rbind(rd_1, rd_2, rd_3, rd_4)
tab_trust


tab <- rbind(tab_coop, tab_trust)
tab

ft <- tab |> flextable::flextable()



















