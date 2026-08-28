################################################################################
### Table 1 descrptives of dependant variables 
###############################################################################

library(modelsummary)
library(flextable)

v <- c("Full Sample", "", "", "", "", "", "")

tab <- pas %>% 
  filter(runner <= 365) |> 
  dplyr::filter(abs(runner) < 365) |>
  dplyr::select(
    'Cooperation (Raw)' = cooperation,
    'Cooperation (Latent)' = coop_lat,
    'Trust (Raw)' = trust,
    'Trust (Latent)' = trust_lat,
    treatment = treatment
  ) %>% 
  datasummary_balance(~treatment, data = .,
                      dinm_statistic = "p.value",
                      fmt = fmt_sprintf("%.3f"),
                      output = "data.frame")


v1 <- c("White Men", "", "", "", "", "", "")

tab1 <- pas %>% 
  filter(runner <= 365) |> 
  filter(gender == 1) |> filter(ethnicity <= 2) |> 
  dplyr::filter(abs(runner) < 365) |>
  dplyr::select(
    'Cooperation (Raw)' = cooperation,
    'Cooperation (Latent)' = coop_lat,
    'Trust (Raw)' = trust,
    'Trust (Latent)' = trust_lat,
    treatment = treatment
  ) %>% 
  datasummary_balance(~treatment, data = .,
                      dinm_statistic = "p.value",
                      fmt = fmt_sprintf("%.3f"),
                      output = "data.frame")


v2 <- c("White Women", "", "", "", "", "", "")

tab2 <- pas %>% 
  filter(runner <= 365) |> 
  filter(gender == 2) |> filter(ethnicity <= 2) |> 
  dplyr::filter(abs(runner) < 365) |>
  dplyr::select(
    'Cooperation (Raw)' = cooperation,
    'Cooperation (Latent)' = coop_lat,
    'Trust (Raw)' = trust,
    'Trust (Latent)' = trust_lat,
    treatment = treatment
  ) %>% 
  datasummary_balance(~treatment, data = .,
                      dinm_statistic = "p.value",
                      fmt = fmt_sprintf("%.3f"),
                      output = "data.frame")

v3 <- c("Non-White Men", "", "", "", "", "", "")

tab3 <- pas %>% 
  filter(runner <= 365) |> 
  filter(gender == 1) |> filter(ethnicity > 2) |> 
  dplyr::filter(abs(runner) < 365) |>
  dplyr::select(
    'Cooperation (Raw)' = cooperation,
    'Cooperation (Latent)' = coop_lat,
    'Trust (Raw)' = trust,
    'Trust (Latent)' = trust_lat,
    treatment = treatment
  ) %>% 
  datasummary_balance(~treatment, data = .,
                      dinm_statistic = "p.value",
                      fmt = fmt_sprintf("%.3f"),
                      output = "data.frame")


v4 <- c("Non-White Women", "", "", "", "", "", "")

tab4 <- pas %>% 
  filter(runner <= 365) |> 
  filter(gender == 2) |> filter(ethnicity > 2) |> 
  dplyr::filter(abs(runner) < 365) |>
  dplyr::select(
    'Cooperation (Raw)' = cooperation,
    'Cooperation (Latent)' = coop_lat,
    'Trust (Raw)' = trust,
    'Trust (Latent)' = trust_lat,
    treatment = treatment
  ) %>% 
  datasummary_balance(~treatment, data = .,
                      dinm_statistic = "p.value",
                      fmt = fmt_sprintf("%.3f"),
                      output = "data.frame")

tab <- rbind(v, tab, v1, tab1, v2, tab2, v3, tab3, v4, tab4)
tab 

tab |> flextable()

ft <- tab |> flextable::flextable()


