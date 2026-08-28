############################################################################
###  RD Estimates Table - Full Sample
############################################################################

df <- pas

attach(df)

rda1 <-
  rdrobust(
    y = coop_lat,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_coop <- c(
  Estimate = round(rda1$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda1$ci[3, 1], 2), ", ", round(rda1$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda1$se[3, 1], 2),
  `p value` = round(rda1$pv[3, 1], 4),
  `N` = sum(rda1$N),
  `Effective N` = sum(rda1$N_h),
  `Bandwidth` = round(c(rda1$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_coop) <- "Cooperation"

rda2 <-
  rdrobust(
    y = trust_lat,
    x = runner,
    kernel = "triangular",
    c = 0,
    vce = "hc2",
    cluster = Borough,
    bwselect = "mserd"
  )

rd_trust <- c(
  Estimate = round(rda2$coef[3, 1], 2),
  `Robust CI` = paste0("(", round(rda2$ci[3, 1], 2), ", ", round(rda2$ci[3, 2], 2), ")"),
  `Robust SE` = round(rda2$se[3, 1], 2),
  `p value` = round(rda2$pv[3, 1], 4),
  `N` = sum(rda2$N),
  `Effective N` = sum(rda2$N_h),
  `Bandwidth` = round(c(rda2$bws[1, 1]), 2)) |>
  t() |>
  as.data.frame()

rownames(rd_trust) <- "Trust"


tab <- rbind(rd_coop, rd_trust)

tab

ft <- tab |> flextable::flextable()


