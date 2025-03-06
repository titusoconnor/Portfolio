library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)


data <- read_excel("C:/Users/CSLO/Downloads/MBA Projects/Yara/Yara.xlsx")
df<-data
str(df)

#Data Cleaning
df <- df %>%
  mutate(gender = case_when(gender == "Female" ~ "1",
                            gender == "Male" ~ "2",
                            TRUE ~ 'unknown'))
df <- df %>%
  mutate(bank_structure_success = case_when(bank_structure_success == "Strongly disagree" ~ "1",
                                            bank_structure_success == "Disagree" ~ "2",
                                            bank_structure_success == "Neutral" ~ "3",
                                            bank_structure_success == "Agree" ~ "4",
                                            bank_structure_success == "Strongly agree" ~ "5",
                                            TRUE ~ 'unknown'))

df <- df %>%
  mutate(transitional_support_perception = case_when(transitional_support_perception == "Strongly disagree" ~ "1",
                                                     transitional_support_perception == "Disagree" ~ "2",
                                                     transitional_support_perception == "Neutral" ~ "3",
                                                     transitional_support_perception == "Agree" ~ "4",
                                                     transitional_support_perception == "Strongly agree" ~ "5",
                                                     TRUE ~ 'unknown'))

df <- df %>%
  mutate(negotiation_and_collaboration = case_when(negotiation_and_collaboration == "Strongly disagree" ~ "1",
                                                   negotiation_and_collaboration == "Disagree" ~ "2",
                                                   negotiation_and_collaboration == "Neutral" ~ "3",
                                                   negotiation_and_collaboration == "Agree" ~ "4",
                                                   negotiation_and_collaboration == "Strongly agree" ~ "5",
                                                   TRUE ~ 'unknown'))

df <- df %>%
  mutate(feel_heard = case_when(feel_heard == "Strongly disagree" ~ "1",
                                feel_heard == "Disagree" ~ "2",
                                feel_heard == "Neutral" ~ "3",
                                feel_heard == "Agree" ~ "4",
                                feel_heard == "Strongly agree" ~ "5",
                                TRUE ~ 'unknown'))

df <- df %>%
  rename(impact_wellbeing = `impact_on_well-being`) %>%
  mutate(impact_wellbeing = case_when(
    impact_wellbeing == "Strongly disagree" ~ "1",
    impact_wellbeing == "Disagree" ~ "2",
    impact_wellbeing == "Neutral" ~ "3",
    impact_wellbeing == "Agree" ~ "4",
    impact_wellbeing == "Strongly agree" ~ "5",
    TRUE ~ 'unknown'
  ))

df <- df %>%
  mutate(acceptance = case_when(acceptance == "Strongly disagree" ~ "1",
                                acceptance == "Disagree" ~ "2",
                                acceptance == "Neutral" ~ "3",
                                acceptance == "Agree" ~ "4",
                                acceptance == "Strongly agree" ~ "5",
                                TRUE ~ 'unknown'))

df <- df %>%
  mutate(belief = case_when(belief == "Strongly disagree" ~ "1",
                            belief == "Disagree" ~ "2",
                            belief == "Neutral" ~ "3",
                            belief == "Agree" ~ "4",
                            belief == "Strongly agree" ~ "5",
                            TRUE ~ 'unknown'))

df <- df %>%
  mutate(motivation = case_when(motivation == "Strongly disagree" ~ "1",
                                motivation == "Disagree" ~ "2",
                                motivation == "Neutral" ~ "3",
                                motivation == "Agree" ~ "4",
                                motivation == "Strongly agree" ~ "5",
                                TRUE ~ 'unknown'))

df <- df %>%
  mutate(positive_outcomes = case_when(positive_outcomes == "Strongly disagree" ~ "1",
                                       positive_outcomes == "Disagree" ~ "2",
                                       positive_outcomes == "Neutral" ~ "3",
                                       positive_outcomes == "Agree" ~ "4",
                                       positive_outcomes == "Strongly agree" ~ "5",
                                       TRUE ~ 'unknown'))

df <- df %>%
  mutate(change_sustainability = case_when(change_sustainability  == "Strongly disagree" ~ "1",
                                           change_sustainability  == "Disagree" ~ "2",
                                           change_sustainability  == "Neutral" ~ "3",
                                           change_sustainability  == "Agree" ~ "4",
                                           change_sustainability  == "Strongly agree" ~ "5",
                                           TRUE ~ 'unknown'))

df <- df %>%
  mutate(willingness_participate = case_when(willingness_participate   == "Strongly disagree" ~ "1",
                                             willingness_participate   == "Disagree" ~ "2",
                                             willingness_participate   == "Neutral" ~ "3",
                                             willingness_participate   == "Agree" ~ "4",
                                             willingness_participate   == "Strongly agree" ~ "5",
                                             TRUE ~ 'unknown'))

df <- df %>%
  mutate(additional_resources  = case_when(additional_resources == "Strongly disagree" ~ "1",
                                           additional_resources == "Disagree" ~ "2",
                                           additional_resources == "Neutral" ~ "3",
                                           additional_resources == "Agree" ~ "4",
                                           additional_resources == "Strongly agree" ~ "5",
                                           TRUE ~ 'unknown'))

df <- df %>%
  mutate(challenges_perception  = case_when(challenges_perception  == "Extremely challenging" ~ "1",
                                            challenges_perception  == "Very challenging" ~ "2",
                                            challenges_perception  == "Neutral" ~ "3",
                                            challenges_perception  == "Slightly challenging" ~ "4",
                                            challenges_perception  == "Not challenging at all" ~ "5",
                                            TRUE ~ 'unknown'))

df <- df %>%
  mutate(understand_potential_impact  = case_when(understand_potential_impact  == "Do not understand" ~ "1",
                                                  understand_potential_impact  == "Slightly understand" ~ "2",
                                                  understand_potential_impact  == "Neutral" ~ "3",
                                                  understand_potential_impact  == "Mostly understand" ~ "4",
                                                  understand_potential_impact  == "Completely understand" ~ "5",
                                                  TRUE ~ 'unknown'))

df <- df %>%
  mutate(adaptation  = case_when(adaptation  == "Not confident at all" ~ "1",
                                 adaptation  == "Slightly confident" ~ "2",
                                 adaptation  == "Neutral" ~ "3",
                                 adaptation  == "Confident" ~ "4",
                                 adaptation  == "Very confident" ~ "5",
                                 TRUE ~ 'unknown'))


df <- df %>%
  mutate(age  = case_when(age  == "Between 20 to 30" ~ "1",
                          age  == "Between 30 to 40" ~ "2",
                          age  == "Between 40 to 50" ~ "3",
                          TRUE ~ 'unknown'))

df<-as.data.frame(df)

df$gender<-as.numeric(df$gender)
df$age<-as.numeric(df$age)
df$bank_structure_success<-as.numeric(df$bank_structure_success)
df$understand_potential_impact<-as.numeric(df$understand_potential_impact)
df$challenges_perception<-as.numeric(df$challenges_perception)
df$transitional_support_perception<-as.numeric(df$transitional_support_perception)
df$negotiation_and_collaboration<-as.numeric(df$negotiation_and_collaboration)
df$feel_heard<-as.numeric(df$feel_heard)
df$impact_wellbeing<-as.numeric(df$impact_wellbeing)
df$acceptance<-as.numeric(df$acceptance)
df$belief<-as.numeric(df$belief)
df$adaptation<-as.numeric(df$adaptation)
df$motivation<-as.numeric(df$motivation)
df$positive_outcomes<-as.numeric(df$positive_outcomes)
df$change_sustainability<-as.numeric(df$change_sustainability)
df$willingness_participate<-as.numeric(df$willingness_participate)
df$additional_resources<-as.numeric(df$additional_resources)


#1. Communication Gaps: Ineffective communication of change initiatives and their benefits.
df%>%ggplot(aes(x=df$understand_potential_impact))+
  geom_bar(fill="blue", color="white")

y<-df$motivation+df$willingness_participate+df$negotiation_and_collaboration

comms<-lm(y~df$understand_potential_impact+df$bank_structure_success)
summary(comms)

#2. Psychological Resistance: Fear, uncertainty, and other emotional factors that hinder acceptance of change.
psycho<-lm(y~df$challenges_perception+df$belief+df$impact_wellbeing)
summary(psycho)

df%>%ggplot(aes(x=df$challenges_perception))+
  geom_bar(fill="blue", color="white")

#3. Leadership and Trust: The role of leadership in fostering trust and guiding employees through transitions.
lead<-lm(y~df$feel_heard+df$transitional_support_perception+df$additional_resources)
summary(lead)
df%>%ggplot(aes(x=feel_heard))+
  geom_bar(fill="blue", color="white")
#4. Cultural Alignment: Misalignment between organizational culture and the proposed reforms.
culture<-lm(y~df$change_sustainability)
summary(culture)

#5. Impact on Performance: The effect of resistance on organizational goals, employee productivity, and morale.
df%>%ggplot(aes(x=motivation))+
  geom_bar(fill="blue", color="white")

df%>%ggplot(aes(x=adaptation))+
  geom_bar(fill="blue", color="white")
t.test(df$motivation ~ df$gender, var.equal = TRUE)

# Performing ANOVA
anova_result <- aov(adaptation ~ age, data = df)
summary(anova_result)

perf<-lm(y~df$adaptation)
summary(perf)
#6. Grievance: When they feel the new transformation is not fair and doesn’t reflect their actual work
grievance<-lm(y~df$bank_structure_success+df$positive_outcomes+df$belief)
summary(grievance)

df%>%ggplot(aes(x=bank_structure_success))+
  geom_bar(fill="blue", color="white")