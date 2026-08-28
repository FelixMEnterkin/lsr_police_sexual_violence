################################################################################
### Estimates as a function of Bandwith Selection
################################################################################


df <- pas |> filter(ethnicity <= 2) |> filter(gender == 2)

attach(df)


### cooperation


opt_mser <- rdrobust(
  y = coop_lat,
  x = runner,
  kernel = "triangular",
  c = 0, bwselect = "mserd",
  vce = "hc2"
)
opt_mser$bws[1,2]




coef_data <- data.frame(
  Bandwidth = numeric(),
  BW = numeric(),
  Estimate = numeric(),
  `Robust SE` = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  `p value` = numeric(),
  N = integer(),
  `Effective N` = integer()
)


for (i in 20:500) {
  mod <- rdrobust(
    y = coop_lat,
    x = runner,
    kernel = "triangular",
    c = 0, h = i,
    vce = "hc2"
  )
  
  out <- data.frame(
    Bandwidth = i,
    BW = mod$bws[1, 1],
    Estimate = round(mod$coef[3, 1], 2),
    `Robust SE` = round(mod$se[3, 1], 2),
    CI_low = mod$ci[3, 1],
    CI_high = mod$ci[3, 2],
    `p value` = mod$pv[3, 1],
    N = sum(mod$N),
    `Effective N` = sum(mod$N_h))
  
  coef_data[i - 29,] <- out
}

head(coef_data)

ggplot(coef_data, aes(x = Bandwidth, y = Estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(title = "RD Estimate as a Function of Bandwidth - Cooperation",
       x = "Bandwidth",
       y = "Estimate") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = opt_mser$bws[1,2]) +
  geom_text(aes(x = opt_mser$bws[1,2] + 5, y = 0.02), label = "MSE Optimal Bandwith",angle=90) + 
  theme_minimal()




#### trust 

opt_mser <- rdrobust(
  y = trust_lat,
  x = runner,
  kernel = "triangular",
  c = 0, bwselect = "mserd",
  vce = "hc2"
)
opt_mser$bws[1,2]


coef_data <- data.frame(
  Bandwidth = numeric(),
  BW = numeric(),
  Estimate = numeric(),
  `Robust SE` = numeric(),
  CI_low = numeric(),
  CI_high = numeric(),
  `p value` = numeric(),
  N = integer(),
  `Effective N` = integer()
)

for (i in 20:500) {
  mod <- rdrobust(
    y = trust_lat,
    x = runner,
    kernel = "triangular",
    c = 0, h = i,
    vce = "hc2"
  )
  
  out <- data.frame(
    Bandwidth = i,
    BW = mod$bws[1, 1],
    Estimate = round(mod$coef[3, 1], 2),
    `Robust SE` = round(mod$se[3, 1], 2),
    CI_low = mod$ci[3, 1],
    CI_high = mod$ci[3, 2],
    `p value` = mod$pv[3, 1],
    N = sum(mod$N),
    `Effective N` = sum(mod$N_h))

  coef_data[i - 29,] <- out
}

head(coef_data)

ggplot(coef_data, aes(x = Bandwidth, y = Estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  labs(title = "RD Estimate as a Function of Bandwidth - Trust",
       x = "Bandwidth",
       y = "Estimate") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = opt_mser$bws[1,2])+
  geom_text(aes(x = opt_mser$bws[1,2] +10, y = -0.02), label = "MSE Optimal Bandwith",angle=90) + 
  theme_minimal()



