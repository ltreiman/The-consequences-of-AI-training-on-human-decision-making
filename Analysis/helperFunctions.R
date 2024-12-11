# Helper functions 

library(dplyr)
library(ggplot2)

# Functions used to help clean the data

find_refreshers <- function(condition_data){ # Finds number of people who refreshed the webpage
  refreshed <- condition_data %>%
    group_by(id) %>%
    summarise(num_groups = n_distinct(training_condition)) %>%
    filter(num_groups != 1)
  return(refreshed)
}

find_duplicates <- function(choices_data){
  duplicates <- choices_data %>%
    dplyr::select(id) %>%
    group_by(id) %>%
    tally() %>%
    filter(n > 26) %>%
    ungroup()
  if(nrow(duplicates) == 0){
    return("No Duplicates")
  }
  else{
    return(duplicates)
  }
}

prep_data <- function(responses){ # Clean data
  responses2<- responses %>%
    filter(practice_trial == 0) %>%
    dplyr::select(id, opponent, training_condition, receiver_offer, response, reaction_time) %>%
    mutate(fair = ifelse(receiver_offer >=  4, 1,0),
           opponent = as.factor(opponent),
           training_condition = as.factor(training_condition),
           accept = ifelse(response =='accept',1,0),
           training_condition = training_condition)
  return(responses2)
}

prep_data_for_part2 <- function(responses, subInfo){ # Needed since did not record training condition in session 2 for experiment 1
  subInfo_group <- subInfo %>% dplyr::select(id, training_condition)
  responses2 <- left_join(responses, subInfo_group, by = "id") %>%
    filter(practice_trial == 0) %>%
    dplyr::select(id, opponent, receiver_offer, response, reaction_time, training_condition) %>%
    mutate(fair = ifelse(receiver_offer >=  4, 1,0),
           opponent = as.factor(opponent),
           ai_train = as.factor(training_condition),
           accept = ifelse(response =='accept',1,0))
  return(responses2)
}

count_groups <- function(subinfo, refreshers){
  results <- subinfo %>%
    filter(!id %in% refreshers$id) %>%
    group_by(training_condition) %>%
    dplyr::select(id) %>%
    distinct() %>%
    tally()
  return(results)
}

# Plot the results
plot_graph <- function(df){
  #' Plot Graph function
  #' 
  #' @description Plots acceprance rate based on offer amount and training condition
  #'
  #' @param df dataframe containing the responses
  #' 
  #' @return ggplot graph
  df_num_prep <- df %>%
    group_by(training_condition) %>%
    summarise(num_participants = n_distinct(id)) 
  num_participants <- length(unique(df$id))
  
  num_groups <- n_distinct(df$training_condition)
  if(num_groups == 3){
    col_values = c('#7daea6','#61235d',"#cc92f8")
    col_labels = c('Control',"AI for self", 'AI for others')
    df$training_condition <- factor(df$training_condition, levels = c("control", "training_self", "training_others"))
  }  
  if(num_groups == 2){
    col_values = c('#7daea6','#61235d')
    col_labels = c('Control',"AI")
  }
  # For training effect, just use standard error
  df_training_prep <- df %>%
    mutate(receiver_offer = as.factor(receiver_offer)) %>%
    group_by(opponent, training_condition, receiver_offer) %>%
    rename(offer = receiver_offer) %>%
    summarise(acceptance_rate = mean(accept),
              sd = sd(accept)) %>%
    ungroup()
  
  df_training <- left_join(df_training_prep, df_num_prep, by = "training_condition") %>% mutate(se = sd/sqrt(num_participants))
  
  opponent.labs = c("AI partner", "Human partner")
  names(opponent.labs) <- c("AI","human")
  
  plt_training <- ggplot(df_training, aes(x = offer, y = acceptance_rate, fill = training_condition))+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_errorbar(aes(ymin = acceptance_rate - se, ymax = acceptance_rate + se), width = 0.2, position=position_dodge(.9))+
    scale_fill_discrete(name = "AI Training")+
    labs(x = "Offer", y = "P(accepted)", fill = "Training condition")+
    scale_fill_manual(values=col_values, labels=col_labels)+
    facet_grid(. ~opponent, labeller = labeller(opponent = opponent.labs))+
    scale_y_continuous(breaks = seq(0, 1, by=0.25), expand = c(0,0), limits = c(0,1.05))+
    scale_x_discrete(labels = c("$1","$2","$3","$4","$5","$6"))+
    theme(legend.position = "none")+ # Comment out if you want to include legend
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=14), strip.text.x = element_text(size = 14),
          axis.title=element_text(size=16,face="bold"))
  return(plt_training)
}

plot_by_fairness<- function(df){
  #' Plot by fairness training
  #' 
  #' @description Plots acceprance rate based on fairness of offer and training condition
  #'
  #' @param df dataframe containing the responses
  #' 
  #' @return ggplot graph
  df_num_prep <- df %>%
    group_by(training_condition) %>%
    summarise(num_participants = n_distinct(id)) 
  
  num_groups <- n_distinct(df$training_condition)
  if(num_groups == 3){
    col_values = c('#7daea6','#61235d',"#cc92f8")
    col_labels = c('Control', 'AI for self',"AI for others")
    df$training_condition <- factor(df$training_condition, levels = c("control", "training_self", "training_others"))
  }  
  if(num_groups == 2){
    col_values = c('#7daea6','#61235d')
    col_labels = c('Control',"AI")
  }
  
  df_training_prep <- df %>%
    group_by(training_condition, fair) %>%
    rename(offer = receiver_offer) %>%
    summarise(acceptance_rate = mean(accept),
              sd = sd(accept))
  df_prep <- left_join(df_training_prep, df_num_prep, by = "training_condition") %>% mutate(se = sd/sqrt(num_participants)) %>%
    mutate(fair = ifelse(fair == 1, "Fair", "Unfair"))
  
  pt <- ggplot(df_prep, aes(x = reorder(fair,+acceptance_rate), y = acceptance_rate, fill = training_condition))+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_errorbar(aes(ymin = acceptance_rate - se, ymax = acceptance_rate + se), width = 0.2, position=position_dodge(.9))+
    scale_fill_discrete(name = "AI Training")+
    scale_y_continuous(expand = c(0,0), limits = c(0,1.05))+
    labs(x = "Offer", y = "P(accepted)", fill = "Training condition")+
    scale_fill_manual(values=col_values, labels=col_labels)+ 
    theme(legend.position = "none")+ # Comment out if you want to include legend
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))
  return(pt)
}

# Prepare data to run mixed effects models
## Experiment 1 training conditions are none and proposer
## Experiment 2 has three different conditions (can be used for Experiment 4)
## Experiment 3 has two different conditions (can be used for Experiment 5)

prep_me_data_exp1 <- function(responses){
  df_logit_model <- responses %>%
    mutate(opponent_AI = ifelse(opponent == "AI", 1, -1),
           fair = ifelse(fair == 1, 1, -1),
           ai_train_proposer = ifelse(training_condition == "Proposer", 1, -1),
           receiver_offer2 = receiver_offer - mean(receiver_offer)) %>%
    dplyr::select(id, accept, opponent_AI, ai_train_proposer, fair, receiver_offer2) %>%
    rename(receiver_offer = receiver_offer2) 
  return(df_logit_model)
}

prep_me_data_exp2 <- function(responses){
  result <- responses %>% 
    mutate(opponent_AI = ifelse(opponent == "AI", 1, -1),
           fair = ifelse(fair == 1, 1, -1),
           receiver_offer2 = receiver_offer - mean(receiver_offer),
           training_condition = as.factor(training_condition)) %>%
    dplyr::select(id, accept, opponent_AI, training_condition, fair, receiver_offer2)
  return(result)
}

prep_me_data_exp3 <- function(responses){
  df_logit_model <- responses %>%
    mutate(opponent_AI = ifelse(opponent == "AI", 1, -1),
           fair = ifelse(fair == 1, 1, -1),
           ai_train_proposer = ifelse(training_condition == "training_others", 1, -1),
           receiver_offer2 = receiver_offer - mean(receiver_offer)) %>%
    dplyr::select(id, accept, opponent_AI, ai_train_proposer, fair, receiver_offer2) %>%
    rename(receiver_offer = receiver_offer2) 
  return(df_logit_model)
}

# Run mixed effects models 
run_mixed_effects_model <- function(df_logit_model){
  logit_model <- glmer(accept ~ opponent_AI * receiver_offer * ai_train_proposer + (1|id), data = df_logit_model, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5)))
  return(summary(logit_model))
}

run_two_mixed_effects_models <- function(df_logit_model){
  results <- summary(glmer(accept ~ opponent_AI*receiver_offer2*training_condition + (1|id), data = df_logit_model,family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5))))
  reference_training_others <- within(df_logit_model, training_condition <- relevel(training_condition, ref = "training_others"))
  results2 <- summary(glmer(accept ~ opponent_AI*receiver_offer2*training_condition + (1|id), data = reference_training_others, family = "binomial", control=glmerControl(optimizer="nlminbwrap",optCtrl=list(maxfun=2e5))))
  print(results)
  return(results2)
}

# ANOVA and paired t-tests functions
call_anova_to_jasp <- function(responses,experiment){
  #' Call Anova to Jasp function
  #' 
  #' @description Prepares a csv so it is ready to be used in JASP to run ANOVA
  #'
  #' @param responses dataframe containing the responses
  #' @param  experiment string of experiment. Used for directory
  #' 
  #' @note set the url directory on where you want the prepped csv to be
  url = paste("./Data/",experiment,"_anova_df.csv", sep = "")
  anova_ug <- responses %>%
    mutate(fair2 = ifelse(fair == 1, "fair", "unfair")) %>%
    group_by(id, fair2, opponent,training_condition) %>%
    summarise(prop_accept = mean(accept)) %>%
    unite(fo,fair2:opponent) %>%
    spread(key = fo, value = prop_accept)
  write.csv(anova_ug, url)
}

call_anova_to_jasp_by_opponent <- function(responses, opponent_type, experiment){
  url = paste("./Data/",experiment,"_",opponent_type,"_anova_df.csv", sep = "")
  anova_ug_post <- responses %>%
    mutate(fair2 = ifelse(fair == 1, "fair", "unfair")) %>%
    filter(opponent == opponent_type) %>%
    group_by(id, fair2, training_condition) %>%
    summarise(prop_accept = mean(accept)) %>%
    spread(key = fair2, value = prop_accept)
  write.csv(anova_ug_post, url)
}

ttest_ai_helper <- function(df, fairness){
  #' t-Test AI helper function
  #' 
  #' @description Helper function for t-test fair training interaction function
  return(t.test(acceptance_rate ~ training_condition, data = df %>% filter(fair == fairness)))
}

ttest_fair_ai_interaction <- function(responses)  {
  #' t-test fair AI interaction function
  #' 
  #' @param responses dataframe containing the responses
  #' 
  #' @description Displays  t test results between training condition for fair and unfair offer
  #' @ Only used for experiment 1
  tt <- responses %>%
    group_by(id, training_condition, fair) %>% 
    summarise(acceptance_rate = mean(accept)) 
  print("For fair offers:")
  print(ttest_ai_helper(tt,1))
  print("For Unfair Offers")
  print(ttest_ai_helper(tt,0))
}

ttest_six_pairwise <- function(responses){
  #' t-test six pairwise
  #' 
  #' @param responses dataframe containing the responses
  #' 
  #' @description Runs t-test comparing all three different training conditions
  #' @returns table of t-test results
  #' @notes Only should be used for experiment 2
  tt <- responses %>%
    group_by(id, training_condition, fair) %>% 
    summarise(acceptance_rate = mean(accept)) 
  
  groups <- as.character(unique(responses$training_condition))
  results <-  rbind(as.data.frame(t(combn(groups,2))), as.data.frame(t(combn(groups,2))))
  results$fairness <- rep(c(0,1), each = length(groups))
  results$t <- rep(0, each = dim(results)[1])
  results$df <- rep(0, each = dim(results)[1])
  results$p <- rep(0, each = dim(results)[1])
  for(i in 1:dim(results)[1]){
    df <- tt %>% filter(training_condition == results$V1[i] | training_condition == results$V2[i])
    ttest_results <- ttest_ai_helper(df, results$fairness[i])
    results$t[i] <- ttest_results$statistic
    results$df[i] <- ttest_results$parameter
    results$p[i] <- ttest_results$p.value
  }
  return(results)
}
