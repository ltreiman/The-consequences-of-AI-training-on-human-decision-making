setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)


# SESSION 1
# Download datasets
responses_exp1_session1 <- read.csv("./Data/Experiment1/Experiment1_session1_responses.csv") 
condition_exp1_session1 <- read.csv("./Data/Experiment1/Experiment1_session1_condition.csv")
subInfo_exp1_session1 <- read.csv("./Data/Experiment1/Experiment1_session1_subinfo.csv") %>% filter(id %in% responses_exp1_session1$id) # Cross-reference to make sure include participants with only saved data

# Clean dataset
refreshers <- find_refreshers(condition_exp1_session1) %>% filter(id %in% subInfo_exp1_session1$id)# Note there is one duplicate, but this duplicate also refreshed the page so was removed from the analysis. 
cleaned_responses_session1 <- prep_data(responses_exp1_session1) %>% filter(!id %in% refreshers$id) %>% filter(id %in% subInfo_exp1_session1$id) 
count_groups(subInfo_exp1_session1, refreshers) 

# Plot results 
plot_graph(cleaned_responses_session1)
plot_by_fairness(cleaned_responses_session1)

# Run mixed effects models
logit_data_session1 <- prep_me_data_exp1(cleaned_responses_session1) # Prep data for logit model
run_mixed_effects_model(logit_data_session1)

# ANOVA and pairwise t-tests
call_anova_to_jasp(cleaned_responses_session1, "Experiment1") # This will export a csv you need to plug into Jasp (https://jasp-stats.org/) to find ANOVA results 
ttest_fair_ai_interaction(cleaned_responses_session1)

# SESSION 2
responses_exp1_session2 <- read.csv("./Data/Experiment1/Experiment1_session2_responses.csv") %>% select(-training_condition) # training condition is none for all participants since no one saw AI training
condition_exp1_session2 <- read.csv("./Data/Experiment1/Experiment1_session2_condition.csv") 
subInfo_exp1_session2 <- read.csv("./Data/Experiment1/Experiment1_session2_subinfo.csv") %>% filter(id %in% responses_exp1_session2$id)

# Clean data
cleaned_responses_session2 <- prep_data_for_part2(responses_exp1_session2, subInfo_exp1_session2) %>% filter(!id %in% refreshers$id) 
count_groups(subInfo_exp1_session2, refreshers)

# Plot results 
plot_graph(cleaned_responses_session2)
plot_by_fairness(cleaned_responses_session2)

# Run mixed effect model
logit_data_session2 <- prep_me_data_exp1(cleaned_responses_session2) # Prep data for logit model
run_mixed_effects_model(logit_data_session2)