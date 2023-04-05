

#Coding the factors for variables! 


#getting the sample size of participant race so I know what to correctly 
#label the specific participant races 
data %>% group_by(participantrace) %>% 
  summarize(n = n())


data <- data %>% 
  mutate(participant_race = factor(participantrace) %>%  
           recode_factor("1" = "Black", 
                         "2" = "White"))

#The names of these factor levels might be mismatched! 
data <- data %>% 
  mutate(iv1_speaker_race = factor(iv1.speakerrace) %>%  
           recode_factor("1" = "Black", 
                         "2" = "White"))

data <- data %>% 
  mutate(iv2_speaker_platform = factor(iv2.speakerplatform) %>% 
           recode_factor("1" = "Fox", 
                         "2" = "MSNBC"))

