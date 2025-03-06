library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(nnet)

data <- read_excel("C:/Users/CSLO/Downloads/MBA Projects/Lamees/Lamees.xlsx")
df<-data
str(df)

#Cleaning
df <- df %>%
  mutate(gender = case_when(gender == "Female" ~ "1",
                            gender == "Male" ~ "2",
                            TRUE ~ 'unknown'))

df <- df %>%
  mutate(educ = case_when(educ == "Bachelor’s Degree" ~ "1",
                            educ == "Master’s Degree" ~ "2",
                            TRUE ~ 'unknown'))
df <- df %>%
  mutate(rank = case_when(rank == "Entry-level" ~ "1",
                          rank == "Mid-level" ~ "2",
                          rank == "Senior-level" ~ "3",
                          rank == "Executive-level" ~ "4",
                            TRUE ~ 'unknown'))

df <- df %>%
  mutate(income = case_when(income == "Under 10,000 SAR" ~ "1",
                            income == "10,000 SAR - 25,000 SAR" ~ "2",
                            income == "25,000 SAR - 40.000 SAR" ~ "3",
                            income == "40.000 SAR – 55.000 SAR" ~ "4",
                            income == "Above 55,000 SAR" ~ "5",
                            TRUE ~ 'unknown'))


df <- df %>%
  mutate(support = case_when(support == "Very Dissatisfied" ~ "1",
                             support == "Dissatisfied" ~ "2",
                             support == "Neutral" ~ "3",
                             support == "Satisfied" ~ "4",
                             support == "Very Satisfied" ~ "5",
                              TRUE ~ 'unknown'))

df <- df %>%
  mutate(manageable_workload = case_when(manageable_workload == "Never" ~ "1",
                                         manageable_workload == "Rarely" ~ "2",
                                         manageable_workload == "Sometimes" ~ "3",
                                         manageable_workload == "Often" ~ "4",
                                         manageable_workload == "Always" ~ "5",
                                         TRUE ~ 'unknown'))


df <- df %>%
  mutate(resources = case_when(resources == "Never" ~ "1",
                               resources == "Rarely" ~ "2",
                               resources == "Sometimes" ~ "3",
                               resources == "Often" ~ "4",
                               resources == "Always" ~ "5",
                               TRUE ~ 'unknown'))

df <- df %>%
  mutate(compensation_reflects = case_when(compensation_reflects == "Does Not Reflect" ~ "1",
                                           compensation_reflects == "Slightly Reflects" ~ "2",
                                           compensation_reflects ==  "Somewhat Reflects" ~ "3",
                                           compensation_reflects == "Mostly Reflects" ~ "4",
                                           compensation_reflects == "Completely Reflects" ~ "5",
                                           TRUE ~ "unknown"))
df <- df %>%
  mutate(valued = case_when(valued == "Never" ~ "1",
                            valued == "Rarely" ~ "2",
                            valued ==  "Sometimes" ~ "3",
                            valued == "Often" ~ "4",
                            valued == "Always" ~ "5",
                            TRUE ~ "unknown"))

df <- df %>%
  mutate(career_growth = case_when(career_growth == "Never" ~ "1",
                                   career_growth == "Rarely" ~ "2",
                                   career_growth ==  "Sometimes" ~ "3",
                                   career_growth == "Often" ~ "4",
                                   career_growth == "Always" ~ "5",
                                   TRUE ~ "unknown"))
  

df <- df %>%
  mutate(stress_support = case_when(stress_support == "Not Sure" ~ "0",
                                    stress_support == "No" ~ "2",
                                    stress_support ==  "Yes" ~ "3",
                                    TRUE ~ "unknown"))

df <- df %>%
  mutate(worklife_balance = case_when(worklife_balance == "Very Dissatisfied" ~ "1",
                                      worklife_balance == "Dissatisfied" ~ "2",
                                      worklife_balance ==  "Neutral" ~ "3",
                                      worklife_balance == "Satisfied" ~ "4",
                                      worklife_balance == "Very Satisfied" ~ "5",
                                      TRUE ~ "unknown"))

df <- df %>%
  mutate(understand_goals = case_when(understand_goals == "Not Clear at All" ~ "1",
                                      understand_goals == "Slightly Clear" ~ "2",
                                      understand_goals ==  "Somewhat Clear" ~ "3",
                                      understand_goals == "Mostly Clear" ~ "4",
                                      understand_goals == "Completely Clear" ~ "5",
                                      TRUE ~ "unknown"))
df <- df %>%
  mutate(job_security = case_when(job_security  == "Very Insecure" ~ "1",
                                  job_security  == "Insecure" ~ "2",
                                  job_security  ==  "Neutral" ~ "3",
                                  job_security  == "Secure" ~ "4",
                                  job_security  == "Very Secure" ~ "5",
                                  TRUE ~ "unknown"))

df <- df %>%
  mutate(transparent_comms = case_when(transparent_comms  == "Very Ineffective" ~ "1",
                                       transparent_comms  == "Ineffective" ~ "2",
                                       transparent_comms  ==  "Neutral" ~ "3",
                                       transparent_comms  == "Effective" ~ "4",
                                       transparent_comms  == "Very Effective" ~ "5",
                                       TRUE ~ "unknown"))

df <- df %>%
  mutate(career_development_needs = case_when(career_development_needs  == "Not Sure" ~ "1",
                                              career_development_needs  == "No" ~ "2",
                                              career_development_needs  ==  "Yes" ~ "3",
                                              TRUE ~ "unknown"))
  
df <- df %>%
  mutate(conflicts = case_when(conflicts == "Not Sure" ~ "0",
                               conflicts == "Yes" ~ "1",
                               conflicts == "No" ~ "2",
                               TRUE ~ "unknown"))

df <- df %>%
  mutate(considered_leaving = case_when(considered_leaving  == "Not Sure" ~ "1",
                                        considered_leaving  == "Yes" ~ "2",
                                        considered_leaving  ==  "No" ~ "3",
                                        TRUE ~ "unknown"))
  
df <- df %>%
  mutate(retention_employee_engagement = case_when(retention_employee_engagement  == "Not Sure" ~ "1",
                                                   retention_employee_engagement  == "No" ~ "1",
                                                   retention_employee_engagement  ==  "Yes" ~ "2",
                                                   TRUE ~ "unknown"))
  
df <- df %>%
  mutate(motivation = case_when(motivation  == "Very Unmotivated" ~ "1",
                                motivation  == "Unmotivated" ~ "2",
                                motivation  ==  "Neutral" ~ "3",
                                motivation  ==  "Motivated" ~ "4",
                                motivation  ==  "Very Motivated" ~ "5",
                                TRUE ~ "unknown"))
  
df <- df %>%
  mutate(technology_productivity = case_when(technology_productivity  == "Does Not Enhance" ~ "1",
                                             technology_productivity  == "Minimally Enhances" ~ "2",
                                             technology_productivity  ==  "Neutral" ~ "3",
                                             technology_productivity  ==  "Enhances" ~ "4",
                                             technology_productivity  ==  "Greatly Enhances" ~ "5",
                                             TRUE ~ "unknown")) 
  
df <- df %>%
  mutate(analytics_retention = case_when(analytics_retention  == "Strongly Disbelieve" ~ "1",
                                         analytics_retention  == "Disbelieve" ~ "2",
                                         analytics_retention  ==  "Neutral" ~ "3",
                                         analytics_retention  ==  "Believe" ~ "4",
                                         analytics_retention  ==  "Strongly Believe" ~ "5",
                                         TRUE ~ "unknown")) 
  
df <- df %>%
  mutate(AI_satisfaction = case_when(AI_satisfaction   == "Not Sure" ~ "0",
                                     AI_satisfaction   == "No" ~ "1",
                                     AI_satisfaction   ==  "Yes" ~ "2",
                                     TRUE ~ "unknown"))  
  
df <- df %>%
  mutate(tech_training_freq = case_when(tech_training_freq  == "Never" ~ "1",
                                        tech_training_freq  == "Rarely" ~ "2",
                                        tech_training_freq  ==  "Sometimes" ~ "3",
                                        tech_training_freq  ==  "Often" ~ "3",
                                        tech_training_freq  ==  "Always" ~ "3",
                                        TRUE ~ "unknown"))

df <- df %>%
  mutate(colleague_rlshp = case_when(colleague_rlshp  == "Very Poor" ~ "1",
                                     colleague_rlshp  == "Poor" ~ "2",
                                     colleague_rlshp  ==  "Neutral" ~ "3",
                                     colleague_rlshp  ==  "Good" ~ "4",
                                     colleague_rlshp  ==  "Very Good" ~ "5",
                                     TRUE ~ "unknown"))

df <- df %>%
  mutate(benefits = case_when(benefits  == "Very Dissatisfied" ~ "1",
                              benefits  == "Dissatisfied" ~ "2",
                              benefits  ==  "Neutral" ~ "3",
                              benefits  ==  "Satisfied" ~ "4",
                              benefits  ==  "Very Satisfied" ~ "5",
                              TRUE ~ "unknown"))

df <- df %>%
  mutate(work_personal_alignment = case_when(work_personal_alignment  == "Very Poorly" ~ "1",
                                             work_personal_alignment  == "Poorly" ~ "2",
                                             work_personal_alignment  ==  "Neutral" ~ "3",
                                             work_personal_alignment  ==  "Well" ~ "4",
                                             work_personal_alignment  ==  "Very Well" ~ "5",
                                             TRUE ~ "unknown"))

df <- df %>%
  mutate(leadership_trust = case_when(leadership_trust  == "Fully Distrust" ~ "1",
                                      leadership_trust  == "Distrust" ~ "2",
                                      leadership_trust  ==  "Neutral" ~ "3",
                                      leadership_trust  ==  "Trust" ~ "4",
                                      leadership_trust  ==  "Fully Trust" ~ "5",
                                      TRUE ~ "unknown"))

str(df)
df$gender<-as.numeric(df$gender)
df$educ<-as.numeric(df$educ)
df$rank<-as.numeric(df$rank)
df$income<-as.numeric(df$income)
df$support<-as.numeric(df$support)
df$manageable_workload<-as.numeric(df$manageable_workload)
df$resources<-as.numeric(df$resources)
df$compensation_reflects<-as.numeric(df$compensation_reflects)
df$valued<-as.numeric(df$valued)
df$career_growth<-as.numeric(df$career_growth)
df$stress_support<-as.numeric(df$stress_support)
df$worklife_balance<-as.numeric(df$worklife_balance)
df$understand_goals<-as.numeric(df$understand_goals)
df$job_security <-as.numeric(df$job_security)
df$transparent_comms<-as.numeric(df$transparent_comms)
df$career_development_needs <-as.numeric(df$career_development_needs)
df$conflicts<-as.numeric(df$conflicts)
df$considered_leaving<-as.numeric(df$considered_leaving)
df$retention_employee_engagement<-as.numeric(df$retention_employee_engagement)
df$motivation<-as.numeric(df$motivation)
df$technology_productivity<-as.numeric(df$technology_productivity)
df$analytics_retention <-as.numeric(df$analytics_retention)
df$AI_satisfaction<-as.numeric(df$AI_satisfaction)
df$tech_training_freq<-as.numeric(df$tech_training_freq)
df$colleague_rlshp <-as.numeric(df$colleague_rlshp)
df$benefits<-as.numeric(df$benefits)
df$work_personal_alignment <-as.numeric(df$work_personal_alignment)
df$leadership_trust <-as.numeric(df$leadership_trust)
df$conflicts<-as.numeric(df$conflicts)

#Demographics
boxplot(df$age)
df%>%ggplot(aes(x=rank))+
  geom_bar(fill="blue", color="white")

df%>%ggplot(aes(x=income))+
  geom_bar(fill="blue", color="white")


#1.	Job design and role clarity
#This is related to how well tasks and responsibilities have been articulated for employees. This is important as it reflects how employees perceive the use of their potential and opportunities to advance their careers within or outside the corporations. Well-defined tasks and responsibilities per employee capabilities and sufficient growth opportunities will likely reduce employee turnover rates. 
job_design_vars <- c("manageable_workload", "resources", "understand_goals", "job_security", "compensation_reflects")

# Summary statistics
summary(df[job_design_vars])

describe(df[job_design_vars])
df %>%
  gather(key = "Variable", value = "Value", job_design_vars) %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18, 
    size = 3, 
    color = "red", 
    aes(label = round(..y.., 2))
  ) +
  geom_text(
    data = . %>% 
      group_by(Variable) %>%
      summarize(
        Median = median(Value),
        Q1 = quantile(Value, 0.25),
        Q3 = quantile(Value, 0.75)
      ),
    aes(
      x = Variable, 
      y = Median, 
      label = paste0("Median: ", round(Median, 2), "\nQ1: ", round(Q1, 2), "\nQ3: ", round(Q3, 2))
    ),
    vjust = -1.5, 
    hjust = 0.5, 
    color = "blue"
  ) +
  theme_minimal() +
  labs(title = "Job Design & Role Clarity Measures", x = "Variable", y = "Score")
#2.	Employee engagement and satisfaction
#This phenomenon gauges how employees are engaged in the industry's communication process and how satisfied they are within their sphere of control or role. It is a huge determiner of whether they will stay or leave the organization. 
engagement_vars <- c("motivation", "valued", "career_growth", "worklife_balance", 
                     "retention_employee_engagement", "colleague_rlshp", 
                     "leadership_trust", "benefits")

summary(df[engagement_vars])
describe(df[engagement_vars])
# Histogram of Motivation
ggplot(df, aes(x = motivation)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Motivation", x = "Motivation Score", y = "Count")

# Boxplot of Motivation by Gender
ggplot(df, aes(x = gender, y = motivation, fill = gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Motivation by Gender", x = "Gender", y = "Motivation Score")

# Boxplot of Motivation by Salary

ggplot(df, aes(x = as.factor(income), y = motivation, fill = as.factor(income))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Motivation by Salary (Income Level)", x = "Income Level", y = "Motivation Score") +
  scale_fill_brewer(palette = "Set3")  # Uses a color palette for better visibility

anova_motivation_income <- aov(motivation ~ as.factor(income), data = df)
summary(anova_motivation_income)

ggplot(df, aes(x = as.factor(valued), y = motivation, fill = as.factor(valued))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Motivation by Feeling Valued", x = "Feeling Valued", y = "Motivation Score") +
  scale_fill_brewer(palette = "Set2")
anova_motivation_valued <- aov(motivation ~ as.factor(valued), data = df)
summary(anova_motivation_valued)

#3.	Organizational change management 
#This examines how the different structural changes in the banking and investment industry impact employees’ feelings and likelihood of rebelling or quitting their jobs. 

change_mgmt_vars <- c("transparent_comms", "career_development_needs", "conflicts", "considered_leaving")

summary(df[change_mgmt_vars])
describe(df[change_mgmt_vars])

df$considered_leaving_binary <- ifelse(df$considered_leaving == "2", 1, 0)
df$considered_leaving <- factor(df$considered_leaving, levels = c(1, 2, 3)
model <- multinom(considered_leaving ~ transparent_comms + career_development_needs + conflicts, data = df)
odds_ratios <- exp(coef(model))  # Compute ORs
print(odds_ratios)
ci <- exp(confint(model))

print(ci)


logit_model <- glm(considered_leaving_binary ~ transparent_comms + career_development_needs + conflicts,
                   data = df, 
                   family = binomial)
summary(logit_model)  # View model results

summary(model)


#Visuals
# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Convert variables to factors
# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Convert variables to factors
df$considered_leaving <- as.factor(df$considered_leaving)
df$conflicts <- as.factor(df$conflicts)
df$career_development_needs <- as.factor(df$career_development_needs)
df$income <- as.factor(df$income)

# Create age groups
df$age_group <- cut(df$age, breaks = c(20, 30, 40, 50, 60), 
                    labels = c("20-30", "31-40", "41-50", "51-60"), 
                    include.lowest = TRUE)

# Function to create bar charts with improved readability
create_bar_chart <- function(data, x_var, title, x_label) {
  ggplot(data, aes_string(x = x_var, fill = "considered_leaving")) +
    geom_bar(position = "dodge") +
    theme_minimal() +
    labs(title = title, x = x_label, y = "Count", fill = "Considered Leaving") +
    scale_fill_brewer(palette = "Set2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotate x-axis labels
          axis.text.y = element_text(size = 12),                         # Increase y-axis text size
          plot.title = element_text(size = 14, face = "bold"),           # Title formatting
          legend.position = "bottom")                                    # Move legend to bottom
}

# Create improved plots
p1 <- create_bar_chart(df, "conflicts", "Considered Leaving by Workplace Conflicts", "Conflicts Level")
p2 <- create_bar_chart(df, "career_development_needs", "Considered Leaving by Career Development Needs", "Career Development Needs Level")
p3 <- create_bar_chart(df, "income", "Considered Leaving by Salary (Income Level)", "Income Level")
p4 <- create_bar_chart(df, "age_group", "Considered Leaving by Age Group", "Age Group")

# Arrange all plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)

#4.	Predictive analytics
#This involves studying how AI algorithms can determine attrition patterns, i.e., how employees leave based on various factors such as performance metrics, engagement scores, and demographic data. 

# Convert considered_leaving to numeric (if categorical)
df$considered_leaving <- as.numeric(as.character(df$considered_leaving))

# Check if all variables are numeric
numeric_vars <- sapply(df[predictive_vars], is.numeric)

# Keep only numeric columns
df_numeric <- df[predictive_vars][, numeric_vars]

# Compute correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Print correlation matrix
print(cor_matrix)
library(ggcorrplot)


# Heatmap of Correlation Matrix
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE)

# Load required libraries
library(ggcorrplot)
library(ggplot2)

# Compute correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Reorder the correlation matrix for better visualization
cor_ordered <- ggcorrplot::ggcorrplot(
  cor_matrix, 
  method = "circle",        # Choose visualization method: "circle", "square", or "ellipse"
  type = "full",            # Show full correlation matrix instead of lower half
  hc.order = TRUE,          # Reorder based on hierarchical clustering
  lab = TRUE,               # Show correlation values
  lab_size = 5,             # Adjust label size for readability
  colors = c("red", "yellow", "green") # Color gradient: blue (-1), white (0), red (+1)
)

# Display the plot
print(cor_ordered)

considered_leaving_binary <- ifelse(df$considered_leaving == "2", 1, ifelse(df$considered_leaving == "3", 0, NA))
considered_leaving_binary[is.na(considered_leaving_binary)] <- 0

logistic_model <- glm(considered_leaving_binary ~ motivation + career_growth + job_security + leadership_trust + benefits+age + gender+ rank + income + worklife_balance+ stress_support+ manageable_workload+valued+colleague_rlshp+conflicts+resources + compensation_reflects,
                      data = df, family = binomial)

summary(logistic_model)

#5.	Sentiment analysis
#Analyze employee feedback and communication using natural language processing (NLP) to gauge overall satisfaction and identify common concerns.

sentiment_vars <- c("valued", "stress_support", "leadership_trust", "work_personal_alignment", "AI_satisfaction", "transparent_comms")

summary(df[sentiment_vars])
describe(df[sentiment_vars])
# Boxplot of Leadership Trust by Gender
df %>%
  gather(key = "Variable", value = "Value", sentiment_vars) %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18, 
    size = 3, 
    color = "red", 
    aes(label = round(..y.., 2))
  ) +
  geom_text(
    data = . %>% 
      group_by(Variable) %>%
      summarize(
        Median = median(Value),
        Q1 = quantile(Value, 0.25),
        Q3 = quantile(Value, 0.75)
      ),
    aes(
      x = Variable, 
      y = Median, 
      label = paste0("Median: ", round(Median, 2), "\nQ1: ", round(Q1, 2), "\nQ3: ", round(Q3, 2))
    ),
    vjust = -1.5, 
    hjust = 0.5, 
    color = "blue"
  ) +
  theme_minimal() +
  labs(title = "Sentiment Measures", x = "Variable", y = "Score")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1) # Rotate x-axis text by 45 degrees
  )


#6.	Personalized employee retention strategies
#This phenomenon will investigate whether different people require special interventions to influence them to stay within their organizations and how AI can be integrated to develop personalized solutions. 
# Mean Comparison by Gender
df %>%
  group_by(gender) %>%
  summarise(Average_Loyalty_Score = mean(considered_leaving, na.rm = TRUE))

df %>%
  group_by(income)%>%
  summarise(Average_Loyalty_Score = mean(considered_leaving, na.rm = TRUE))
