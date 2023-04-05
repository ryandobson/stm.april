### Running ANOVAs for STM Racism in Policing for NCUR 2023 

library(psych)
library(tidyverse)
library(car)
library(sjstats)
library(effectsize)


#loading in the data set 
data <- read_csv("data/lucid_race_police_merged.csv")

#Renaming all of the variables to lowercase for consistency 
data <- data %>% rename_all(tolower)

source("factor_script.R")

glimpse(data)

#Removing a few unecessary columns to remove clutter 
data <- data %>% select(-luciddatacollectionsession,
                        -startdate,
                        -enddate,
                        -progress,
                        -durationseconds,
)


### Running the first anova model 
or_model <- aov(overall.receptivity ~ iv1_speaker_race * iv2_speaker_platform * participant_race, data = data)

or_model <- Anova(or_model, 
      type = "III")

#Getting the effect size for the model ()
eta_squared(or_model, 
            partial = TRUE,
            ci = 0.95
            )

cen_model <- aov(desire.to.censor ~ iv1_speaker_race * iv2_speaker_platform * participant_race, data = data)

cen_model <- Anova(cen_model, 
                  type = "III")

cen_model

eta_squared(cen_model, 
            partial = TRUE,
            ci = 0.95)



### Running some simple t-tests 


t.test(lwa.composite ~ participant_race,
       data = data, 
       paired = FALSE)

t.test(politicallean ~ participant_race,
       data = data,
       paired = FALSE)

### you need the non-factorized variable to calculate cohen's d! 
cohen.d(lwa.composite ~ participantrace, data = data)

cohen.d(politicallean ~ participantrace, data = data)

?cohen.d
?t.test


data %>% group_by(participant_race) %>% 
  summarize(mean = mean(politicallean, na.rm = TRUE))


data %>% group_by(participant_race) %>% 
  summarize(mean = mean(lwa.composite, na.rm = TRUE))

