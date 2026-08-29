################################################################################
### Measurement and scaling Properties 
################################################################################

library(lavaan)
library(semTools)



#---------------------------------------------------------------
## seperate confirmatory factor analysis: one-factor model 
#---------------------------------------------------------------

#' models are run separately as trust appears in more years that cooperation. 
#' Running a signal two-factor model with maximim likihood means extrapolating 
#' cooperation score well beyond where they are supported in the data


### Trust in the Police 


cfam_trust <- 
  'f =~ trust_1 + trust_2 + trust_3 + trust_4' |>
  cfa(pas, missing = "ML")


summary(cfam_trust, fit.measures=TRUE, standardize = TRUE)


pas$trust_lat1 <- lavPredict(cfam_trust) 

# correlation with the direct variable 
cor(pas$trust_lat1, pas$trust, use = "complete.obs")  # diff





### Cooperation with Police 

cfam_coop <- 
  'f =~ cooperation_1 + cooperation_2 + cooperation_3 + cooperation_4' |>
  cfa(pas, missing = "ML")

summary(cfam_coop, fit.measures=TRUE, standardize = TRUE)

pas$coop_lat1 <- lavPredict(cfam_coop) 

# correlation with the direct survey item
cor(pas$coop_lat1, pas$cooperation, use = "complete.obs")  # 



#-----------------------------------
## Construct Latent Variables
#-----------------------------------

pas$coop_lat  <- lavPredict(cfam_coop)
pas$trust_lat <- lavPredict(cfam_trust)




#--------------------------------------
## Measurement Invariance 
#--------------------------------------


#------------------
## Trust 
#-----------------

## base model 


cfam_trust <- 
  'f =~ trust_1 + trust_2 + trust_3 + trust_4
  '

# configuration - fully flexable
fit_config <- cfa(cfam_trust, data = pas, group = "round", 
                  missing = "ML")
# metric - fixed loadings
fit_metric <- cfa(cfam_trust, data = pas, group = "round",
                  group.equal = "loadings", missing = "ML")

# scaler - fixed intecepts + loadings
fit_scalar <- cfa(cfam_trust, data = pas, group = "round",
                  group.equal = c("loadings", "intercepts"), missing = "ML")

fit <- compareFit(fit_config, fit_metric, fit_scalar)
summary(fit)



## refined model 

cfam_trust <- 
  'f =~ trust_1 + trust_2 + trust_3 + trust_4
  trust_1 ~~ trust_2
  '


# configuration
fit_config <- cfa(cfam_trust, data = pas, group = "round", 
                  missing = "ML")
# weak invariance
fit_metric <- cfa(cfam_trust, data = pas, group = "round",
                  group.equal = "loadings", missing = "ML")

# stong invariance
fit_scalar <- cfa(cfam_trust, data = pas, group = "round",
                  group.equal = c("loadings", "intercepts"), missing = "ML")

# partial invariance
fit_partial <- cfa(cfam_trust, data = pas,group = "round",
  group.equal = c("loadings", "intercepts"),
  group.partial = c("trust_1 ~ 1", "trust_2 ~ 1"), missing = "ML")

fit <- compareFit(fit_config, fit_metric, fit_scalar, fit_partial)
summary(fit)


### Survey from 2020 - 2021  --- Full Scaler Invariance 

pas_sub <- pas |> filter(round > 2019)

cfam_trust <- 
  'f =~ trust_1 + trust_2 + trust_3 + trust_4
  '

# configuration - fully flexable
fit_config <- cfa(cfam_trust, data = pas_sub, group = "round", 
                  missing = "ML")
# metric - fixed loadings
fit_metric <- cfa(cfam_trust, data = pas_sub, group = "round",
                  group.equal = "loadings", missing = "ML")

# scaler - fixed intecepts + loadings
fit_scalar <- cfa(cfam_trust, data = pas_sub, group = "round",
                  group.equal = c("loadings", "intercepts"), missing = "ML")

fit <- compareFit(fit_config, fit_metric, fit_scalar)
summary(fit)





#------------------------
### Cooperation
#-------------------------

pas_sub <- pas |> filter(round > 2019 & round < 2022)


cfam_coop <- 
  'f =~ cooperation_1 + cooperation_2 + cooperation_3 + cooperation_4' 

fit_config <- cfa(cfam_coop, data = pas_sub, group = "round", 
                  missing = "ML")

fit_metric <- cfa(cfam_coop, data = pas_sub, group = "round",
                  group.equal = "loadings", missing = "ML")

fit_scalar <- cfa(cfam_coop, data = pas_sub, group = "round",
                  group.equal = c("loadings", "intercepts"),
                  missing = "ML")

fit <- compareFit(fit_config, fit_metric, fit_scalar)
summary(fit)


 
modindices(fit_config, sort.=TRUE)

cfam_coop <- '
  f =~ cooperation_1 + cooperation_2 + cooperation_3 + cooperation_4
  cooperation_1 ~~ cooperation_2
  cooperation_3 ~~ cooperation_4
'  |>
  cfa(pas, missing = "ML")


summary(cfam_coop, fit.measures=TRUE, standardize = TRUE)

fit_config <- cfa(cfam_coop, data = pas_sub, group = "round", 
                  missing = "ML")
fit_metric <- cfa(cfam_coop, data = pas_sub, group = "round",
                  group.equal = "loadings", missing = "ML")

fit_scalar <- cfa(cfam_coop, data = pas_sub, group = "round",
                  group.equal = c("loadings", "intercepts"),
                  missing = "ML")

fit <- compareFit(fit_config, fit_metric, fit_scalar)
summary(fit)

#---------------------------------------------------------------
## combined confirmatory factor analysis: one teo-factor model 
#---------------------------------------------------------------

#' a combined model can be run on the years wher both variables are in the data
#' this can be used as a robustness test. This can alos allow trust and coopertaion 
#' to correlate in the model.



pas_sub <- subset(pas, round > 2018 & round < 2022)

cfam <-
  'Trust =~ trust_1 + trust_2 + trust_3 + trust_4
   Cooperation =~ cooperation_1 + cooperation_2 + cooperation_3 + cooperation_4
   Trust ~~ Cooperation' |>
  cfa(pas_sub, missing = "ML")

summary(cfam, fit.measures=TRUE, standardize = TRUE,)

# average
(0.668^2 + 0.722^2 + 0.789^2)/3
(0.625^2 + 0.45^2 + 0.583^2)/3


# correlations with the single item versions
cor(pas_sub$cooperation_1, lavPredict(cfam)[, "Cooperation"], use = "complete.obs")
cor(pas_sub$trust_1, lavPredict(cfam)[, "Trust"], use = "complete.obs")


# extract latent factor
pas_sub$coop_lat2  <- lavPredict(cfam, ) [, "Cooperation"]
pas_sub$trust_lat2 <- lavPredict(cfam, ) [, "Trust"]


# pas_sub$coop_lat2  <- scale(pas_sub$coop_lat2)
# pas_sub$trust_lat2 <- scale(pas_sub$trust_lat2)


#------------------------------
## Comparison between verions
#-----------------------------


# comparison - single vs joined factor model 

cor(pas$coop_lat1, pas$coop_lat2, use = "complete.obs")
cor(pas$trust_lat1, pas$trust_lat2, use = "complete.obs")  # same 

# comparison to raw survey item 
cor(pas$cooperation_1, pas$coop_lat2, use = "complete.obs")
cor(pas$trust_1, pas$trust_lat2, use = "complete.obs")  # same 




#------------------------------------
## Three measure cooperation variable - robustness 
#-----------------------------------



cfam_coop <- 
  'f =~ cooperation_1 + cooperation_2 + cooperation_3' |>
  cfa(pas, missing = "ML")

summary(cfam_coop, fit.measures=TRUE, standardize = TRUE)

pas$coop_lat1 <- lavPredict(cfam_coop) 

cor(pas$coop_lat1, pas$cooperation, use = "complete.obs")  # 

#------------------------------------
## Three measure Trust variable - robustness 
#-----------------------------------


cfam_trust <- 
  'f =~ trust_1 + trust_2' |>
  cfa(pas, missing = "ML")


summary(cfam_trust, fit.measures=TRUE, standardize = TRUE)

cor(pas$trust, lavPredict(cfam_trust), use = "complete.obs")

# This is effectvly just the original trust measure. 







cfam_trust <- 
  'f =~ trust_1 + trust_2 + trust_5 + trust_6 + trust_8' |>
  cfa(pas, missing = "ML")

summary(cfam_trust, fit.measures=TRUE, standardize = TRUE)



cfam_trust <- 
  'f =~ trust_1 + trust_2  + trust_4 + trust_5 + trust_6 + trust_8' |>
  cfa(pas, missing = "ML")

summary(cfam_trust, fit.measures=TRUE, standardize = TRUE)
