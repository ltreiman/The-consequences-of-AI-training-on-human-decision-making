setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)


# Download datasets
responses_exp5 <- read.csv("./Data/Experiment5/Experiment5_responses.csv") 
condition_exp5 <- read.csv("./Data/Experiment5/Experiment5_condition.csv")
subInfo_exp5 <- read.csv("./Data/Experiment5/Experiment5_subinfo.csv") %>% filter(id != "id0165") # Participant had no data for responses 

# Clean dataset
refreshers <- find_refreshers(condition_exp5) %>% filter(id %in% subInfo_exp5$id)
cleaned_responses <- prep_data(responses_exp5) %>% filter(!id %in% refreshers$id)  %>% filter(id %in% subInfo_exp5$id) 
count_groups(subInfo_exp5, refreshers) 

# Plot results 
plot_graph(cleaned_responses)
plot_by_fairness(cleaned_responses)

# Run mixed effects models
logit_data <- prep_me_data_exp3(cleaned_responses) # Prep data for logit model
run_mixed_effects_model(logit_data)

# Run conditional effects models
ai_training_responses <- logit_data %>% filter(ai_train_proposer == 1)
summary(glmer(accept ~  receiver_offer * opponent_AI + (1|id), data = ai_training_responses, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5))))


control_responses <- logit_data %>% filter(ai_train_proposer == -1)
summary(glmer(accept ~  receiver_offer * opponent_AI + (1|id), data = control_responses, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5))))


# ANOVA 
call_anova_to_jasp(cleaned_responses, "Experiment5") # This will export a csv you need to plug into Jasp (https://jasp-stats.org/) to find ANOVA results 
