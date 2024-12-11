setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)


# SESSION 1
# Download datasets
responses_exp4_session1 <- read.csv("./Data/Experiment4/Experiment4_session1_responses.csv") 
condition_exp4_session1 <- read.csv("./Data/Experiment4/Experiment4_session1_condition.csv")
subInfo_exp4_session1 <- read.csv("./Data/Experiment4/Experiment4_session1_subinfo.csv") %>% filter(id %in% responses_exp4_session1$id)

# Clean dataset
duplicates <- find_duplicates(responses_exp4_session1) # One participant completed the experiment twice, so only included first 24 responses
refreshers <- find_refreshers(condition_exp4_session1) %>% filter(id %in% subInfo_exp4_session1$id)
cleaned_responses_prep <- prep_data(responses_exp4_session1) %>% filter(!(id %in% refreshers$id)) %>% filter(!(id %in% duplicates$id)) %>% filter(id %in% subInfo_exp4_session1$id) 
first_entry <- prep_data(responses_exp4_session1) %>% filter(id %in% duplicates$id) %>% group_by(id) %>% mutate(row = row_number()) %>% filter(row <= 24) %>% select(-row)
cleaned_responses_session1 <- rbind(cleaned_responses_prep,first_entry)
count_groups(subInfo_exp4_session1, refreshers) 

# Plot results 
plot_graph(cleaned_responses_session1)
plot_by_fairness(cleaned_responses_session1)

# Run regression
logit_data_session1 <- prep_me_data_exp2(cleaned_responses_session1) 
run_two_mixed_effects_models(logit_data_session1)

# ANOVA and t-tests
call_anova_to_jasp(cleaned_responses_session1, "Experiment4")
ttest_six_pairwise(cleaned_responses_session1)

# SESSION 2
# Download datasets
responses_exp4_session2 <- read.csv("./Data/Experiment4/Experiment4_session2_responses.csv") 
condition_exp4_session2 <- read.csv("./Data/Experiment4/Experiment4_session2_condition.csv")
subInfo_exp4_session2 <- read.csv("./Data/Experiment4/Experiment4_session2_subinfo.csv") %>% filter(id %in% responses_exp4_session2$id)

# Clean dataset
cleaned_responses_session2 <- prep_data(responses_exp4_session2) %>% filter(!id %in% refreshers$id) %>% filter(id %in% subInfo_exp4_session2$id) 
count_groups(subInfo_exp4_session2, refreshers) 

# Plot results 
plot_graph(cleaned_responses_session2)
plot_by_fairness(cleaned_responses_session2)

# Run regression
logit_data_session2 <- prep_me_data_exp2(cleaned_responses_session2) 
run_two_mixed_effects_models(logit_data_session2)

logit_data_session2_unfair <- prep_me_data_exp2(cleaned_responses_session2 %>% filter(receiver_offer <= 3)) 
run_two_mixed_effects_models(logit_data_session2_unfair) # In helperFunction, need to change optimizer to bobyqa

logit_data_session2_unfair_AI <- logit_data_session2_unfair %>% filter(opponent_AI == 1)
summary(glmer(accept ~ receiver_offer2*training_condition + (1|id), data = logit_data_session2_unfair_AI,family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))
logit_data_session2_unfair_human <- logit_data_session2_unfair %>% filter(opponent_AI == -1)
summary(glmer(accept ~ receiver_offer2*training_condition + (1|id), data = logit_data_session2_unfair_human,family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))