################################################################################
### DATA CLEAN OF PUBLIC ATTITUDES SURVEY 
################################################################################

#' this script cleans the public attitudes survey and constructs the variables on interests.
#' data for the public attitudes survey is not kept in a single dataset, but in a dataset for each 
#' year. As a result these set must be cleaned separately and joined together. Crucially, not 
#' all variables are run in each year of the survey and even when variables are they are sometimes 
#' renamed across years in the data. Accordingly, most of the cleaning done on individual survey years 
#' is to create equivelence across survey rounds prior to joining together.
#' Confirmatory factor analyse I conducted ay the end of the data clean to construct latent variables 
#' for the analysis. Measurement and scaling properties are assessed in a later document.



## packages

library(tidyverse)
library(haven)
library(ggplot2)
library(lubridate)
library(lavaan)


################################################################################
#### 2016 Survey Round
################################################################################

# select relevent variables 
pas_2016 <- pas_2016 |>
  dplyr::select(
    interview_date,
    C2,         
    ward,       # ward
    XQ135r,     # Gender 
    Q136r,      # Age
    NQ147r,     # Ethnicicty
    BQ90A,      # Have you been the victim of a crime in the last 12 months
    
    ## Trust 
    NQ135BD,   # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Miscellaneous
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    Q60,      # how good a job 
    NQ62B,    # Duty to obey the law
    Q13,      # Worry Crime in your area
    Q3C,      # -22 People in this neighborhood can be trusted
    Q3I,      # -22 If I sensed trouble whilst in this area, I could get help from people who live here
    Q3J       # -22 The people who live here can be relied upon to call the police if someone is acting suspiciously
    
    
    
  ) 


# Correct columnn names to uniform
colnames(pas_2016)[colnames(pas_2016) == "C2"] <- "Borough"
colnames(pas_2016)[colnames(pas_2016) == "XQ135r"] <- "ReXQ135"
colnames(pas_2016)[colnames(pas_2016) == "Q136r"] <- "ReQ136"
colnames(pas_2016)[colnames(pas_2016) == "NQ147r"] <- "ReNNQ147"

# construct missing columns 
pas_2016$IMDQuartile <- NA
pas_2016$XQ128A <- NA
pas_2016$ReQ139 <- NA

pas_2016$ XQ128A2B <- NA # Would you call the police 
pas_2016$ XQ128A2C <- NA
pas_2016$ XQ128A2D <- NA
pas_2016$ XQ128A2E <- NA

pas_2016$WT_Discrete <- NA  # survey weights
pas_2016$WT_Discrete_rebased <- NA


pas_2016$round <- 2015

# make all columns numeric
pas_2016 <- pas_2016 %>%  mutate(across(where(is.labelled), as.numeric))

################################################################################
#### 2017 Survey Round
################################################################################


# select relevant variables 
pas_2017 <- pas_2017 |>
  dplyr::select(
    interview_date,
    C2,
    ward,
    XQ135r,     # Gender 
    Q136r,      # Age
    NQ147r,     # Ethnicicty
    BQ90A,      # Have you been the victim of a crime in the last 12 months
    
    ## Trust 
    NQ135BD,  # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Misellanious
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    NQ62B,    # Duty to obey the law
    Q13,      # Worry Crime in your area
    Q60,
    Q3C,      # -22 People in this neighborhood can be trusted
    Q3I,      # -22 If I sensed trouble whilst in this area, I could get help from people who live here
    Q3J      # -22 The people who live here can be relied upon to call the police if someone is acting suspiciously
    
    
  ) 

# Correct col names to uniform
colnames(pas_2017)[colnames(pas_2017) == "C2"] <- "Borough"
colnames(pas_2017)[colnames(pas_2017) == "XQ135r"] <- "ReXQ135"
colnames(pas_2017)[colnames(pas_2017) == "Q136r"] <- "ReQ136"
colnames(pas_2017)[colnames(pas_2017) == "NQ147r"] <- "ReNNQ147"


# construct missing columns 
pas_2017$IMDQuartile <- NA
pas_2017$XQ128A <- NA
pas_2017$ReQ139 <- NA

pas_2017$ XQ128A2B <- NA # Would you call the police 
pas_2017$ XQ128A2C <- NA
pas_2017$ XQ128A2D <- NA
pas_2017$ XQ128A2E <- NA

pas_2017$WT_Discrete <- NA  # survey weights 
pas_2017$WT_Discrete_rebased <- NA

pas_2017$round <- 2017

# make all columns numeric 
pas_2017 <- pas_2017 %>%  mutate(across(where(is.labelled), as.numeric))



################################################################################
#### 2018 Survey Round
################################################################################

pas_2018 <- pas_2018 |>
  dplyr::select(
    interview_date,
    C2,
    ward,
    XQ135r,     # Gender 
    Q136r,      # Age
    NQ147r,     # Ethnicicty
    Q139r, # employ 
    BQ90A,       # Have you been the victim of a crime in the last 12 months
    
    ## Trust 
    NQ135BD,  # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Misellanious
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    NQ62B,    # Duty to obey the law
    Q13,      # Worry Crime in your area
    Q60, 
    Q3C,      # -22 People in this neighborhood can be trusted
    Q3I,      # -22 If I sensed trouble whilst in this area, I could get help from people who live here
    Q3J,      # -22 The people who live here can be relied upon to call the police if someone is acting suspiciously
    
    WT_Discrete,
    WT_Discrete_rebased
  ) 

# Correct col names to uniform
colnames(pas_2018)[colnames(pas_2018) == "C2"] <- "Borough"
colnames(pas_2018)[colnames(pas_2018) == "XQ135r"] <- "ReXQ135"
colnames(pas_2018)[colnames(pas_2018) == "Q136r"] <- "ReQ136"
colnames(pas_2018)[colnames(pas_2018) == "NQ147r"] <- "ReNNQ147"
colnames(pas_2018)[colnames(pas_2018) == "Q139r"] <- "ReQ139"

pas_2018$IMDQuartile <- NA
pas_2018$XQ128A <- NA

pas_2018$ XQ128A2B <- NA # Would you call the police 
pas_2018$ XQ128A2C <- NA
pas_2018$ XQ128A2D <- NA
pas_2018$ XQ128A2E <- NA

pas_2018$round <- 2018

pas_2018 <- pas_2018 %>%  mutate(across(where(is.labelled), as.numeric))



################################################################################
#### 2019 Survey Round
################################################################################


### Select Variable 

pas_2019 <- pas_2019 |>
  dplyr::select(
    interview_date,
    BOROUGHNEIGHBOURHOODCODED,
    ward,
    XQ135r,     # Gender 
    Q136r,      # Age
    NQ147r,     # Ethnicity
    XQ128A,     # Have you personally ever been stopped and searched in London?
    ## have you been the victim of a crime 
    Q139r, # employ 
    
    ## Trust 
    NQ135BD,  # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Misellanious
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    
    NQ62B,    
    Q13,      # Worry Crime in your area
    
    Q60, 
    Q3C,      # -22 People in this neighborhood can be trusted
    Q3I,      # -22 If I sensed trouble whilst in this area, I could get help from people who live here
    Q3J,      # -22 The people who live here can be relied upon to call the police if someone is acting suspiciously
    
    WT_Discrete,
    WT_Discrete_rebased
  )


# Correct col names to make them uniform with 2021
colnames(pas_2019)[colnames(pas_2019) == "XQ135r"] <- "ReXQ135"
colnames(pas_2019)[colnames(pas_2019) == "NQ147r"] <- "ReNNQ147"
colnames(pas_2019)[colnames(pas_2019) == "Q136r"] <- "ReQ136"
colnames(pas_2019)[colnames(pas_2019) == "BOROUGHNEIGHBOURHOODCODED"] <- "Borough"
colnames(pas_2019)[colnames(pas_2019) == "Q139r"] <- "ReQ139"


## Add Missing columns 
pas_2019$IMDQuartile <- NA
pas_2019$BQ90A  <- NA     # Have you been the victim of a crime in the last 12 months

pas_2019$ XQ128A2B <- NA # Would you call the police 
pas_2019$ XQ128A2C <- NA
pas_2019$ XQ128A2D <- NA
pas_2019$ XQ128A2E <- NA


colnames(pas_2019)[colnames(pas_2019) == "NNQ135A_newA"] <- "NNQ135A"
colnames(pas_2019)[colnames(pas_2019) == "NNQ135A_newB"] <- "NNQ135B"
colnames(pas_2019)[colnames(pas_2019) == "NNQ135A_newC"] <- "NNQ135C"

pas_2019$round <- 2019

# Make all values numberical 
pas_2019 <- pas_2019 %>%  mutate(across(where(is.labelled), as.numeric))



################################################################################
#### 2020 Survey Round
################################################################################

pas_2020 <- pas_2020 |>
  dplyr::select(
    interview_date,
    Borough,
    ward,
    ReXQ135,     # Gender 
    ReQ136,      # Age
    ReNQ147,     # Ethnicicty
    
    ReQ139,      # employed 
    
    IMDQuartile, # Indicies of Relative Deprevation
    XQ128A,      # Have you personally ever been stopped and searched in London?
    BQ90A,       # Have you been the victim of a crime in the last 12 months
    
    ## Trust 
    NQ135BD,  # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Misellanious
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    
    NQ62B,    # Duty to obey the law
    Q13,      # Worry Crime in your area
    
    ## Willingess to Contact Police 
    XQ128A2B, # call if victim
    XQ128A2C, # call if graphiti 
    XQ128A2D, # call if robb
    XQ128A2E, # call if firearm
    
    WT_Discrete,
    WT_Discrete_rebased
  )

# Correct col names to uniform
colnames(pas_2020)[colnames(pas_2020) == "ReNQ147"] <- "ReNNQ147"

pas_2020$round <- 2020

pas_2020 <- pas_2020 %>%  mutate(across(where(is.labelled), as.numeric))



################################################################################
#### 2021
###############################################################################
pas_2021 <- pas_2021 |>
  dplyr::select(
    interview_date,
    Borough,
    ward,
    ReXQ135,     # Gender 
    ReQ136,      # Age
    ReNNQ147,    # Ethnicicty
    ReQ139, # employed
    
    IMDQuartile, # Indicies of Relative Deprevation
    
    XQ128A,      # Have you personally ever been stopped and searched in London?
    BQ90A,       # Have you been the victim of a crime in the last 12 months 
    # Contacted in the last 12 months - dont have 2020-21
    
    
    ## Trust 
    NQ135BD,  # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Misellanious
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    NQ62B,    # Duty to obey the law
    Q13,      # Worry Crime in your area
    Q60,
    
    ## Willingess to Contact Police 
    XQ128A2B, # call if victim
    XQ128A2C, # call if graphiti 
    XQ128A2D, # call if robb
    XQ128A2E, # call if firearm
    
    Q3C,      # -22 People in this neighborhood can be trusted
    Q3I,      # -22 If I sensed trouble whilst in this area, I could get help from people who live here
    Q3J,       # -22 The people who live here can be relied upon to call the police if someone is acting suspiciously
    
    WT_Discrete,
    WT_Discrete_rebased
  )

pas_2021$round <- 2021

pas_2021 <- pas_2021 %>%  mutate(across(where(is.labelled), as.numeric))

################################################################################
#### 2022
################################################################################

pas_2022 <- pas_2022 |>
  dplyr::select(
    interview_date,
    Borough,
    ward,
    
    NXQ135, # Gender 
    ReQ136,  # Age
    ReNNQ147, # Ethnicicty
    ReQ139,
    
    IMDQuartile, # Indicies of Relative Deprevation
    XQ128A,  # Have you personally ever been stopped and searched in London?
    
    ## Trust 
    NQ135BD,  # The Metropolitan Police Service is an organisation that I can trust
    
    ## Effectiveness 
    Q60,      # Police Effectiveness Local
    Q61,      # Police Effectiveness London
    
    # Misellanious
    Q62A,     # They can be relied upon when you need them
    Q62TG,    # The police in your area listen to the concerns of local people
    
    Q62B,     # contact any reason
    Q62C,     # treat everyone fair 
    Q62F,     # matters to the community
    Q62TG,    # Listen to concerns
    
    NQ62B,    # Duty to obey the law
    Q13,      # Worry Crime in your area
    Q60,
    
    WT_Discrete,
    WT_Discrete_rebased
    
  )


# Correct col names to uniform
colnames(pas_2022)[colnames(pas_2022) == "NXQ135"] <- "ReXQ135"

## Collecteive Efficay for Falsification
pas_2022$Q3C <- NA
pas_2022$Q3I <- NA
pas_2022$Q3J <- NA 

## Cooperation with police 
pas_2022$XQ128A2B <- NA
pas_2022$XQ128A2C <- NA
pas_2022$XQ128A2D <- NA
pas_2022$XQ128A2E <- NA

pas_2022$BQ90A  <- NA     # Have you been the victim of a crime in the last 12 months

pas_2022 <- pas_2022 %>%  mutate(across(where(is.labelled), as.numeric))

pas_2022$round <- 2022

# Joining dataframes
pas <- rbind(pas_2016, pas_2017, pas_2018, pas_2019, pas_2020, pas_2021, pas_2022)



################################################################################
### Variable Construction 
################################################################################


# Variables are inverted to make positive values high and negative low

pas <- pas |>
  mutate(
    #------------------------------------
    #### Dependent Variable 
    #------------------------------------
    
    ## Trust 
    police_trust = if_else(NQ135BD <= 5, (6 - (NQ135BD)), NA_real_), 
    
    ## Contacting Police - victim of a crime 
    cooperation = ifelse(XQ128A2B <= 4, (5 - XQ128A2B), NA_real_),
    
    ## Latent Contacting Police items 
    cooperation_1 =  (5 - pas$XQ128A2B), 
    cooperation_2 =  (5 - pas$XQ128A2C), 
    cooperation_3 =  (5 - pas$XQ128A2D),
    
    ## Latent Trust items 
    trust_1 = if_else(NQ135BD <= 5, (6 - (NQ135BD)), NA_real_), 
    trust_2 = if_else(Q62A <= 5, (6 - Q62A), NA_real_),   
    trust_3 = if_else(Q62B <= 5, (6 - Q62B), NA_real_),   
    trust_4 = if_else(Q62C <= 5, (6 - Q62C), NA_real_),  
    
    
    #----------------------------------------------
    ## Additional variables
    #----------------------------------------------
    
    ## Trust Falsification 
    trust_gov = if_else(NQ135BDB <= 5, (6 - NQ135BDB), NA_real_),
    trust_nhs = if_else(NQ135BDD <= 5, (6 - NQ135BDD), NA_real_),
    trust_media = if_else(NQ135BDE <= 5, (6 - NQ135BDE), NA_real_),
    trust_people = if_else(Q3C <= 5, (6 - Q3C), NA_real_),
    
    ## Crime Fear 
    crime = if_else(Q13 <= 4, (5 - Q13), NA_real_),

    
    ## Date of Interview
    interview_date = as_date(interview_date),    
    
    ## Controls 
    stopped = if_else(XQ128A == 1, 1, 0),
    Stopped = case_when(stopped == 0 ~ "No",
                        stopped ==1 ~ "Yes"),
    
    victim = if_else(BQ90A == 1, 1, 0),
    Victim = case_when(victim == 0 ~ "No",
                       victim == 1 ~ "Yes"),
    
    gender = ifelse(ReXQ135 <= 3, ReXQ135, NA_real_),
    
    female = ifelse(gender == 2, 1, 0),
    female_fc = case_when(female == 0 ~ "Male",
                          female == 1 ~ "Female"),
    
    age = ifelse(ReQ136 <= 6, ReQ136, NA_real_),
    age_fc = case_when(  
      age == 1 ~ "18-24",  
      age == 2 ~ "25-34",  
      age == 3 ~ "35-44",  
      age == 4 ~ "45-54",  
      age == 5 ~ "55-64", 
      age == 6 ~ "65 and Over"),
    
    ethnicity = ifelse(ReNNQ147 <= 6, ReNNQ147, NA_real_),
    ethnicity_fc = case_when(
      ethnicity <= 2 ~ "White",
      ethnicity <= 2 ~ "Mixed",
      ethnicity <= 2 ~ "Asian",
      ethnicity <= 2 ~ "Black",
      ethnicity <= 2 ~ "Other"),
    
    employed = if_else(ReQ139 <= 2, 1, 0),
    unemployed = if_else(ReQ139 == 6 | ReQ139 == 3, 1, 0),
    parttime = if_else(ReQ139 == 2, 1, 0),
    student = if_else(ReQ139 == 7, 1, 0),
    retired = if_else(ReQ139 == 5, 1, 0)
  )



################################################################################
### TREATMENT ASSIGNMENT
################################################################################

## Date of event (Treatment)
event <- as_date('2021-03-10')

# Treatment Assignment and Runner 
pas <- pas |>
  mutate(
    treatment = if_else(interview_date >= event, 1, 0),  # treatment indicator
    runner = as.integer((interview_date - event)),
    weeks = as.integer(floor(runner / 7)),
    months = as.integer(floor(interval(event, interview_date) / months(1)))
  )


################################################################################
### LATENT MEASURMENT MODELS 
################################################################################

library(lavaan)
library(semTools)

#---------------------------------------------------------------
## seperate confirmatory factor analysis: one-factor model 
#---------------------------------------------------------------


### Trust in the Police 

cfam_trust <- 
  'f =~ trust_1 + trust_2 + trust_3 + trust_4' |>
  cfa(pas, missing = "ML")

summary(cfam_trust, fit.measures=TRUE, standardize = TRUE)


### Cooperation with Police 

cfam_coop <- 
  'f =~ cooperation_1 + cooperation_2 + cooperation_3' |>
  cfa(pas, missing = "ML")

summary(cfam_coop, fit.measures=TRUE, standardize = TRUE)


#-----------------------------------
## Construct Latent Variables
#-----------------------------------

pas$coop_lat  <- lavPredict(cfam_coop)
pas$trust_lat <- lavPredict(cfam_trust)
