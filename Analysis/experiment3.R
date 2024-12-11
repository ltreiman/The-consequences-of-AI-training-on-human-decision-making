setwd("./OSF")
source("./Analysis/helperFunctions.R")

# Need to download packages before running
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmerTest)


# Download datasets
responses_exp3 <- read.csv("./Data/Experiment3/Experiment3_responses.csv") 
condition_exp3 <- read.csv("./Data/Experiment3/Experiment3_condition.csv")
subInfo_exp3 <- read.csv("./Data/Experiment3/Experiment3_subinfo.csv") %>% filter(id %in% responses_exp3$id)

# Clean dataset
refreshers <- find_refreshers(condition_exp3) %>% filter(id %in% subInfo_exp3$id)
cleaned_responses <- prep_data(responses_exp3) %>% filter(!id %in% refreshers$id) %>% filter(id %in% subInfo_exp3$id) 
count_groups(subInfo_exp3, refreshers) 

# Plot results 
plot_graph(cleaned_responses)
plot_by_fairness(cleaned_responses)

# Run mixed effects models
logit_data <- prep_me_data_exp3(cleaned_responses) # Prep data for logit model
run_mixed_effects_model(logit_data)

# ANOVA 
call_anova_to_jasp(cleaned_responses, "Experiment3") # This will export a csv you need to plug into Jasp (https://jasp-stats.org/) to find ANOVA results 
# Exploratory ANOVA
call_anova_to_jasp_by_opponent(cleaned_responses, "AI", "Experiment3")
call_anova_to_jasp_by_opponent(cleaned_responses, "human", "Experiment3")


