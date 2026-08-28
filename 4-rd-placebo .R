#############################################################################
#### RD Estimates Table - Placebo
#############################################################################


### cooperation


df <- pas |> filter(ethnicity <= 2) |> filter(gender == 2)


attach(df)

rda1 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    kernel = "triangular",
    c = -30,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

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

rownames(rd_1) <- "30 Days"



rda2 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    kernel = "triangular",
    c = -60,
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

rownames(rd_2) <- "60 Days"


tab <- rbind(rd_1, rd_2)

placebo_coop <- tab
placebo_coop


ft <- tab |> flextable::flextable()
flextable::save_as_docx(ft, path = "./code/replication ASR/table_placebo_rd_coop.docx")




#########



rda1 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    kernel = "triangular",
    c = -30,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

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

rownames(rd_1) <- "30 Days"



rda2 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    kernel = "triangular",
    c = -60,
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

rownames(rd_2) <- "60 Days"


tab <- rbind(rd_1, rd_2)

placebo_trust <- tab
placebo_trust



ft <- tab |> flextable::flextable()
flextable::save_as_docx(ft, path = "./code/replication ASR//table_placebo_rd_trust.docx")








rdrobust(
  y = trust_lat,
  x = runner,
  kernel = "triangular",
  c = -60,
  vce = "hc2", masspoints = 3,
  cluster = Borough,
  bwselect = "mserd", all = T
) |> summary()

