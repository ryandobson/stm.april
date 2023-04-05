
library(tidyverse)

#loading in the data set 
data <- read_csv("data/lucid_race_police_merged.csv")

#Renaming all of the variables to lowercase for consistency 
data <- data %>% rename_all(tolower)

#Getting a mean summary of LWA to determine if it is setup good 
data %>% summarize(mean = mean(lwa.composite))

#Removing a few unecessary columns to remove clutter 
data <- data %>% select(-luciddatacollectionsession,
                -startdate,
                -enddate,
                -progress,
                -durationseconds,
                )


data %>% glimpse()


#Box Plots! 

data %>% ggplot(mapping = aes(x = iv1_speaker_race, 
                                   y = overall.receptivity, 
                                   color = participant_race
)
) +
  geom_boxplot(outlier.colour = "blue",
               
  ) + 
  coord_cartesian(                  ylim = c(1, 7)
  )  +
  theme_classic() 


### Creating Pirate Plots! 

library(ggpirate)

data %>% ggplot(mapping = aes(x = iv1_speaker_race, 
                              y = overall.receptivity, 
                              color = participant_race)
) + 
  geom_pirate(bars = FALSE
  ) + theme_classic() +
  theme(legend.position = "bottom")




###Attempting to run an ANOVA 

library(psych)

?psych

model <- aov(overall.receptivity ~ iv1_speaker_race * iv2_speaker_platform * participant_race, data = data)

#display the results of the 2x2x2 anova 
model

#displays the results of the 2x2x2 anova in a more readable fashion.  
summary(model)


lm.model <- lm(overall.receptivity ~ iv1_speaker_race * iv2_speaker_platform * participant_race, data = data)

lm.model

summary(lm.model)


?aov

##install.packages("car")

#Package to get the type 3 sum of squares 
library(car)

?Anova
###Geting the results for a type 3 sum of squares 
Anova(model, 
      type = "III")



### Creating a scatter plot 


data %>% ggplot(mapping = aes(x = lwa.composite, 
                              y = desire.to.censor, 
                             )) +  #color = participant_race
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Left Wing Authoritanism",
       y = "Desire to Censor", 
       title = "In both groups, stronger left-wing authoritariansim was associated with more censoriousness",
       subtitle = "(Black r = .27, White r = .41)") + 
  coord_cartesian(xlim = c(1, 7),
                  ylim = c(1, 7) ) +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 7) +
  theme(legend.title = element_blank()) + 
  facet_wrap(~participant_race) 
  
      
?scale_y_continuous
?element_text
?labs
?guides
?scale_fill_discrete

glimpse(data)
  
?legend.text
?geom_smooth
?coord_cartesian
?theme

?geom_point

#filters to use to generate correlations split by group 
black.part <- data %>% filter(participant_race == "Black")

white.part <- data %>% filter(participant_race == "White")

#grabbing correlations for white and black participants to double check things 
black.part %>% select(desire.to.censor, 
                      lwa.composite) %>% 
  cor() 

white.part %>% select(desire.to.censor, 
                      lwa.composite) %>% 
  cor()

data %>%  select(desire.to.censor, 
                 lwa.composite) %>% 
                 cor()



?cor



### Creating some bar graphs with error bars 


data %>% ggplot(mapping = aes(x = participant_race,
                              y = overall.receptivity,
                              fill = participant_race)) +
  theme_classic() + 
  stat_summary(fun = "mean", geom = "bar", show.legend = FALSE) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.25, show.legend = FALSE) +
  scale_y_continuous(n.breaks = 7) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(y = "Overall Receptivity",
       x = "Participant Race") +
  facet_wrap(~iv1_speaker_race)


?stat_summary

data %>% ggplot(mapping = aes(x = participant_race,
                              y = overall.receptivity,
                              fill = participant_race)) +
                geom_bar(stat = "identity", color = "black",
                         position = position_dodge()) +
                geom_errorbar(aes(ymin = ))


?geom_errorbar
?geom_bar

data %>% ggplot(mapping = aes(x = lwa.composite, 
                              y = desire.to.censor,
                              color = participant_race
)) +  
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_classic() +
  labs(x = "Left Wing Authoritanism",
       y = "Desire to Censor", 
       title = "In both groups, stronger left-wing authoritariansim was associated with more censoriousness",
       subtitle = "(Black r = .27, White r = .41)") + 
  coord_cartesian(xlim = c(1, 7),
                  ylim = c(1, 7) ) +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 7) +
  theme(legend.title = element_blank())

