setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)


# SESSION 1
# Download datasets
responses_exp2_session1 <- read.csv("./Data/Experiment2/Experiment2_session1_responses.csv") 
condition_exp2_session1 <- read.csv("./Data/Experiment2/Experiment2_session1_condition.csv")
subInfo_exp2_session1 <- read.csv("./Data/Experiment2/Experiment2_session1_subinfo.csv")  %>% filter(id %in% responses_exp2_session1$id) # Cross-reference to make sure include participants with only saved data

# Clean dataset
refreshers <- find_refreshers(condition_exp2_session1) %>% filter(id %in% subInfo_exp2_session1$id)
cleaned_responses_session1 <- prep_data(responses_exp2_session1) %>% filter(!id %in% refreshers$id) %>% filter(id %in% subInfo_exp2_session1$id) 
count_groups(subInfo_exp2_session1, refreshers) 

# Plot results 
plot_graph(cleaned_responses_session1)
plot_by_fairness(cleaned_responses_session1)

# Run regression
logit_data_session1 <- prep_me_data_exp2(cleaned_responses_session1) 
run_two_mixed_effects_models(logit_data_session1)

# ANOVA and t-tests
call_anova_to_jasp(cleaned_responses_session1, "Experiment2")
ttest_six_pairwise(cleaned_responses_session1)

# SESSION 2
# Download datasets
responses_exp2_session2 <- read.csv("./Data/Experiment2/Experiment2_session2_responses.csv") 
condition_exp2_session2 <- read.csv("./Data/Experiment2/Experiment2_session2_condition.csv")
subInfo_exp2_session2 <- read.csv("./Data/Experiment2/Experiment2_session2_subinfo.csv")  %>% filter(id %in% responses_exp2_session2$id) # One person completed subinfo information twice

# Clean data set
duplicates<- find_duplicates(responses_exp2_session2) #Three participants completed the second session twice
cleaned_responses_prep <- prep_data(responses_exp2_session2) %>% filter(!(id %in% refreshers$id)) %>% filter(!(id %in% duplicates$id)) %>% filter(id %in% subInfo_exp2_session2$id) 
first_entry <- prep_data(responses_exp2_session2) %>% filter(id %in% duplicates$id) %>% group_by(id) %>% mutate(row = row_number()) %>% filter(row <= 24) %>% select(-row)
cleaned_responses_session2 <- rbind(cleaned_responses_prep,first_entry)
count_groups(subInfo_exp2_session2, refreshers) 


# Plot results 
plot_graph(cleaned_responses_session2)
plot_by_fairness(cleaned_responses_session2)

# Run regression
logit_data_session2 <- prep_me_data_exp2(cleaned_responses_session2) 
run_two_mixed_effects_models(logit_data_session2)
