# Analysis code for Tandoc, Dong, & Schapiro — Object feature memory is distorted by category structure
# By Marlie Tandoc

# Please see associated readme file for description of data data and manuscript for statistical analyses and model details
# This code contains the main statistical analyses for both experiments including the main phase (interleaved part and color trials) and test phase (Experiment 2)

#Please e-mail tandoc@sas.upenn.edu for any questions
# If you have any questions please contact Marlie, tandoc@sas.upenn.edu

library(dplyr)
library(ggplot2)
library(ggsignif)
library(tidyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(stats)

# Options
options(stringsAsFactors = FALSE)
options(scipen=999)

#### Data Prep #### 

#Set directory to where current analysis script is
script_path <- rstudioapi::getActiveDocumentContext()$path
script_dir <- dirname(script_path)
setwd(script_dir)

#Read in data
data <- read.csv('data/data_main.csv') # Main phase data for both experiments
data_test <-read.csv('data/data_test.csv') # Test data for Experiment 2

#Calculate additional mean centered/contrast coded variables for mixed models
data <- data %>% mutate(type_c = ifelse(cued_type == 'shared',-1,1),
                          block_c = scale(block_num, scale = FALSE)) 


# Separate main phase into part and color trials
part_data <- filter(data, trial_type == 'part')
color_data <- filter(data, trial_type == 'color')

# Separate test phase into part and color trials
color_data_test <- filter(data_test, trial_type == 'color')
part_data_test <- filter(data_test, trial_type == 'part')

# Remove color trials that timed out/no response
color_data <- filter(color_data, color_response_type != 'none')
color_data_test <- filter(color_data_test, color_response_type != 'none')

#### Experiment 1 ####

part_data_e1 <- filter(part_data, experiment == 1)
color_data_e1 <-filter(color_data, experiment == 1)

#Participant summary data frames

# Color bias score overall ignoring block
color_sum_e1 <- color_data_e1 %>% group_by(participant,cued_type) %>% 
  summarise(color_bias_score = mean(color_response_bias))

# Color bias score for feature type (shared vs unique) in each block
color_sum_block_e1 <- color_data_e1 %>% group_by(participant,cued_type,block_num) %>% 
  summarise(color_bias_score = mean(color_response_bias),
            percent_attract = length(which(color_response_bias == 1))/length(participant),
            percent_repel = length(which(color_response_bias == -1))/length(participant),
            percent_target = length(which(color_response_type == 'target'))/length(participant),
            percent_orthogonal = length(which(color_response_bias == 0 & substr(color_response_type,1,1) =='o'))/length(participant)/2)

# Color bias score ignoring block and feature type
color_sum_e1_ignore_type <- color_data_e1 %>% group_by(participant) %>% 
  summarise(color_bias_score = mean(color_response_bias)) 

# Part bias score overall ignoring block
part_sum_e1 <- part_data_e1 %>% group_by(participant,cued_type) %>% 
  summarise(part_percent_corr = sum(part_first_attempt_corr)/length(part_first_attempt_corr))

# part bias score for feature type (shared vs unique) in each block
part_sum_block_e1 <- part_data_e1 %>% group_by(participant,cued_type,block_num) %>% 
  summarise(part_percent_corr = sum(part_first_attempt_corr)/length(part_first_attempt_corr))

# part bias score ignoring block and feature type
part_sum_e1_ignore_type <- part_data_e1 %>% group_by(participant) %>% 
  summarise(part_percent_corr = sum(part_first_attempt_corr)/length(part_first_attempt_corr))

# Statistics in order of manuscript Results section

## PART LEARNING ##

# GLM: Does part accuracy (whether they got it correct on their first attempt) change across block and feature type?
parts_model_e1 <- glmer(part_first_attempt_corr ~ type_c * block_c + (1 |participant),
                        part_data_e1, family = 'binomial')
summary(parts_model_e1)

#Post-hoc t-tests on shared vs. part accuracy in each block
p_values_bias <- c()

# Loop through each block from 1 to 6
for (block_num in 1:6) {
  result <- t.test(part_percent_corr~ cued_type, 
                   data = filter(part_sum_block_e1, block_num == !!block_num), 
                   paired = TRUE)
  p_values_bias <- c(p_values_bias, result$p.value)
}

# Uncorrected p-values (block 1-6)
p_values_bias

# Corrected p-values (FDR corrected) (block 1-6)
p_adjusted_bias <- p.adjust(p_values_bias, method = 'fdr')
p_adjusted_bias

## COLOR MEMORY ##

# Are overall color bias scores above 0?
t.test(color_sum_e1_ignore_type$color_bias_score, mu = 0)

# Is there a difference in overall color bias scores for shared vs. unique features?
t.test(color_bias_score ~ cued_type, color_sum_e1, paired = TRUE)

# Linear mixed effects model (note that independent variables are centered/contrast coded)
bias_model_e1 <- lmer(color_response_bias ~ type_c * block_c + (1|participant), color_data_e1)
summary(bias_model_e1)

#Does the slope for shared and unique features change across blocks?
em_trends_bias_e1 <- emtrends(bias_model_e1, pairwise ~ type_c, var = 'block_c', infer = T,pbkrtest.limit = 8118)
summary(em_trends_bias_e1)

#Post-hoc t-tests on shared vs. color bias scores in each block
p_values_bias <- c()

# Loop through each block from 1 to 6
for (block_num in 1:6) {
  print(paste('Block num: ', block_num, sep = ''))
  result <- t.test(color_bias_score ~ cued_type, 
                   data = filter(color_sum_block_e1, block_num == !!block_num), 
                   paired = TRUE)
  print(result)
  p_values_bias <- c(p_values_bias, result$p.value)
}

# Uncorrected p-values (block 1-6)
p_values_bias

# Corrected p-values (FDR corrected) (block 1-6)
p_adjusted_bias <- p.adjust(p_values_bias, method = 'fdr')
p_adjusted_bias


## Correlations ##

#Is there a correlation between part accuracy and color bias scores?
cor.test(color_sum_e1$color_bias_score[which(color_sum_e1$cued_type == 'shared')],
         part_sum_e1$part_percent_corr[which(part_sum_e1$cued_type == 'shared')],)

cor.test(color_sum_e1$color_bias_score[which(color_sum_e1$cued_type == 'unique')],
         part_sum_e1$part_percent_corr[which(part_sum_e1$cued_type == 'unique')],)


# Corrected p-values (FDR corrected) (block 1-6)
p_adjusted_bias <- p.adjust(p_values_bias, method = 'fdr')
p_adjusted_bias

## Color accuracy model ##
# Does color accuracy change across learning?
color_acc_model_e1 <- glmer(color_response_correct ~ type_c * block_c + (1 |participant),
                            color_data_e1, family = 'binomial')
summary(color_acc_model_e1)


## Color option analysis ##

# In block 6, do participants choose the repel foil more often for shared than unique features?
t.test(color_sum_block_e1$percent_repel[which(color_sum_block_e1$cued_type == 'shared' & color_sum_block_e1$block_num == 6)],
       color_sum_block_e1$percent_repel[which(color_sum_block_e1$cued_type == 'unique' & color_sum_block_e1$block_num == 6)],
       paired = TRUE)

#In block 6, do participants choose the other color foils more often for shared than unique features
t.test(color_sum_block_e1$percent_attract[which(color_sum_block_e1$cued_type == 'shared' & color_sum_block_e1$block_num == 6)],
       color_sum_block_e1$percent_attract[which(color_sum_block_e1$cued_type == 'unique' & color_sum_block_e1$block_num == 6)],
       paired = TRUE)

t.test(color_sum_block_e1$percent_orthogonal[which(color_sum_block_e1$cued_type == 'shared' & color_sum_block_e1$block_num == 6)],
       color_sum_block_e1$percent_orthogonal[which(color_sum_block_e1$cued_type == 'unique' & color_sum_block_e1$block_num == 6)],
       paired = TRUE)

#In block 6, do participants choose the target color more often for shared than unique features?
t.test(color_sum_block_e1$percent_target[which(color_sum_block_e1$cued_type == 'shared' & color_sum_block_e1$block_num == 6)],
       color_sum_block_e1$percent_target[which(color_sum_block_e1$cued_type == 'unique' & color_sum_block_e1$block_num == 6)],
       paired = TRUE)



#### Experiment 2 ####

part_data_e2 <- filter(part_data, experiment == 2)
color_data_e2 <-filter(color_data, experiment == 2)

#Participant summary data frames

# Color bias score overall ignoring block
color_sum_e2 <- color_data_e2 %>% group_by(participant,cued_type) %>% 
  summarise(color_bias_score = mean(color_response_bias),
            percent_attract = length(which(color_response_bias == 1))/length(participant),
            percent_repel = length(which(color_response_bias == -1))/length(participant),
            percent_target = length(which(color_response_type == 'target'))/length(participant),
            percent_orthogonal = length(which(color_response_bias == 0 & substr(color_response_type,1,1) =='o'))/length(participant)/2)

# Color bias score for feature type (shared vs unique) in each block
color_sum_block_e2 <- color_data_e2 %>% group_by(participant,cued_type,block_num) %>% 
  summarise(color_bias_score = mean(color_response_bias),
            percent_attract = length(which(color_response_bias == 1))/length(participant),
            percent_repel = length(which(color_response_bias == -1))/length(participant),
            percent_target = length(which(color_response_type == 'target'))/length(participant),
            percent_orthogonal = length(which(color_response_bias == 0 & substr(color_response_type,1,1) =='o'))/length(participant)/2)

# Color bias score ignoring block and feature type
color_sum_e2_ignore_type <- color_data_e2 %>% group_by(participant) %>% 
  summarise(color_bias_score = mean(color_response_bias))

# part bias score overall ignoring block
part_sum_e2 <- part_data_e2 %>% group_by(participant,cued_type) %>% 
  summarise(part_percent_corr = sum(part_first_attempt_corr)/length(part_first_attempt_corr))

# part bias score for feature type (shared vs unique) in each block
part_sum_block_e2 <- part_data_e2 %>% group_by(participant,cued_type,block_num) %>% 
  summarise(part_percent_corr = sum(part_first_attempt_corr)/length(part_first_attempt_corr))

# part bias score ignoring block and feature type
part_sum_e2_ignore_type <- part_data_e2 %>% group_by(participant) %>% 
  summarise(part_percent_corr = sum(part_first_attempt_corr)/length(part_first_attempt_corr))

## PART LEARNING ##

# GLM: Does part accuracy (whether they got it correct on their first attempt) change across block and feature type?
parts_model_e2 <- glmer(part_first_attempt_corr ~ type_c * block_c + (1 |participant),
                        part_data_e2, family = 'binomial')
summary(parts_model_e2)

#Post-hoc t-tests on shared vs. part accuracy in each block
p_values_bias <- c()

# Loop through each block from 1 to 6
for (block_num in 1:6) {
  result <- t.test(part_percent_corr~ cued_type, 
                   data = filter(part_sum_block_e2, block_num == !!block_num), 
                   paired = TRUE)
  p_values_bias <- c(p_values_bias, result$p.value)
}

# Uncorrected p-values (block 1-6)
p_values_bias

# Corrected p-values (FDR corrected) (block 1-6)
p_adjusted_bias <- p.adjust(p_values_bias, method = 'fdr')
p_adjusted_bias

## Color bias ##

# Is there a difference in overall color bias scores for shared vs. unique features?
t.test(color_bias_score ~ cued_type, color_sum_e2, paired = TRUE)

# Linear mixed effects model (note that independent variables are centered/contrast coded)
bias_model_e2 <- lmer(color_response_bias ~ type_c * block_c + (1|participant), color_data_e2)
summary(bias_model_e2)

#Does the slope for shared and unique features change across blocks?
em_trends_bias_e2 <- emtrends(bias_model_e2, pairwise ~ type_c, var = 'block_c', infer = T,pbkrtest.limit = 10417)
summary(em_trends_bias_e2)

#Post-hoc t-tests on shared vs. color bias scores in each block
p_values_bias <- c()

# Loop through each block from 1 to 6
for (block_num in 1:6) {
  print(paste('Block num: ', block_num, sep = ''))
  result <- t.test(color_bias_score ~ cued_type, 
                   data = filter(color_sum_block_e2, block_num == !!block_num), 
                   paired = TRUE)
  print(result)
  p_values_bias <- c(p_values_bias, result$p.value)
}

# Uncorrected p-values (block 1-6)
p_values_bias

# Corrected p-values (FDR corrected) (block 1-6)
p_adjusted_bias <- p.adjust(p_values_bias, method = 'fdr')
p_adjusted_bias

## Color accuracy model ##
# Does color accuracy change across learning?
color_acc_model_e2 <- glmer(color_response_correct ~ type_c * block_c + (1 |participant),
                            color_data_e2, family = 'binomial')
summary(color_acc_model_e2)


## Color option analysis ##

# Shared vs. unique features for each option type overall (across all blocks)

# Do participants choose the attract foil more often for shared than unique features?
t.test(color_sum_e2$percent_attract[which(color_sum_e2$cued_type == 'shared')],
       color_sum_e2$percent_attract[which(color_sum_e2$cued_type == 'unique')],
       paired = TRUE)

# Do participants choose the target color more often for shared THan unique features
t.test(color_sum_e2$percent_target[which(color_sum_e2$cued_type == 'shared')],
       color_sum_e2$percent_target[which(color_sum_e2$cued_type == 'unique')],
       paired = TRUE)

# Do participants choose the other foils more often for shared than unique features?
t.test(color_sum_e2$percent_repel[which(color_sum_e2$cued_type == 'shared')],
       color_sum_e2$percent_repel[which(color_sum_e2$cued_type == 'unique')],
       paired = TRUE)

t.test(color_sum_e2$percent_orthogonal[which(color_sum_e2$cued_type == 'shared')],
       color_sum_e2$percent_orthogonal[which(color_sum_e2$cued_type == 'unique')],
       paired = TRUE)

#Shared vs. unique features in the last block of learning for each option type

# In block 6, do participants choose the repel foil more often for shared than unique features?
t.test(color_sum_block_e2$percent_repel[which(color_sum_block_e2$cued_type == 'shared' & color_sum_block_e2$block_num == 6)],
       color_sum_block_e2$percent_repel[which(color_sum_block_e2$cued_type == 'unique' & color_sum_block_e2$block_num == 6)],
       paired = TRUE)

#In block 6, do participants choose the attract foil more often for shared than unique features
t.test(color_sum_block_e2$percent_attract[which(color_sum_block_e2$cued_type == 'shared' & color_sum_block_e2$block_num == 6)],
       color_sum_block_e2$percent_attract[which(color_sum_block_e2$cued_type == 'unique' & color_sum_block_e2$block_num == 6)],
       paired = TRUE)


## Correlations ##

#Is there a correlation between part accuracy and color bias scores?
cor.test(color_sum_e2$color_bias_score[which(color_sum_e2$cued_type == 'shared')],
         part_sum_e2$part_percent_corr[which(part_sum_e2$cued_type == 'shared')],)

cor.test(color_sum_e2$color_bias_score[which(color_sum_e2$cued_type == 'unique')],
         part_sum_e2$part_percent_corr[which(part_sum_e2$cued_type == 'unique')],)



#### Experiment 2: Test Phase ####
part_data_test <- filter(data_test, trial_type == 'part')
color_data_test<-filter(data_test, trial_type == 'color')


# Part accuracy ##

# Part accuracy during test
part_sum_test <-part_data_test %>% group_by(participant,cued_type) %>% 
  summarise(part_percent_corr = sum(part_response_correct)/length(part_response_correct))

# Does part accuracy differ as a function of the three feature types (shared_old vs. shared_new vs unique)
a1 <- aov(part_percent_corr ~ cued_type, part_sum_test)
summary(a1)

#Is part accuracy above chance during test for each feature type?
t.test(part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_new')], mu = 0)
t.test(part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_old')], mu = 0)
t.test(part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'unique')], mu = 0)


# Does part bias differ for shared features on old satellites vs. unique?
t.test(part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_old')],
       part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'unique')],
       paired = TRUE)

#Does part bias differ for shared features on old vs. new satellites?
t.test(part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_new')],
       part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_old')],
       paired = TRUE)


## Color bias ##

# Color bias analysis during test
color_sum_test <- color_data_test %>% 
  group_by(participant,cued_type) %>% 
  summarise(color_bias_score = mean(color_response_bias),
            percent_attract = length(which(color_response_bias == 1))/length(participant),
            percent_repel = length(which(color_response_bias == -1))/length(participant),
            percent_target = length(which(color_response_type == 'target'))/length(participant),
            percent_orthogonal = length(which(color_response_bias == 0 & substr(color_response_type,1,1) =='o'))/length(participant)/2)

# Does color bias differ as a function of the three feature types (shared_old vs. shared_new vs unique)
a2 <- aov(color_bias_score ~ cued_type, color_sum_test)
summary(a2)

# Pairwise t-tests between feature types

#Does color bias differ for shared features on old vs. new satellites?
t.test(color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'shared_new')],
       color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'shared_old')],
       paired = TRUE)

#Does color bias differ for shared features on new satellites vs. unique?
t.test(color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'shared_new')],
       color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)

# Does color bias differ for shared features on old satellites vs. unique?
t.test(color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'shared_old')],
       color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)


## Correlations ##

# Is there a correlation between part accuracy and color bias scores?
cor.test(color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'shared_old')],
         part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_old')],)

cor.test(color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'shared_new')],
         part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'shared_new')],)

cor.test(color_sum_test$color_bias_score[which(color_sum_test$cued_type == 'unique')],
         part_sum_test$part_percent_corr[which(part_sum_test$cued_type == 'unique')],)


## Color option analysis ##

# Do participants choose the attract foil more often for shared (trained satellites) than unique features?
t.test(color_sum_test$percent_attract[which(color_sum_test$cued_type == 'shared_old')],
       color_sum_test$percent_attract[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)

# Do participants choose the attract foil more often for shared (novel satellites) than unique features?
t.test(color_sum_test$percent_attract[which(color_sum_test$cued_type == 'shared_new')],
       color_sum_test$percent_attract[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)

t.test(color_sum_test$percent_repel[which(color_sum_test$cued_type == 'shared_new')],
       color_sum_test$percent_repel[which(color_sum_test$cued_type == 'shared_old')],
       paired = TRUE)

# Do participants choose the target color more often for shared than unique features
t.test(color_sum_test$percent_target[which(color_sum_test$cued_type == 'shared_old')],
       color_sum_test$percent_target[which(color_sum_test$cued_type == 'shared_new')],
       paired = TRUE)

t.test(color_sum_test$percent_target[which(color_sum_test$cued_type == 'shared_old')],
       color_sum_test$percent_target[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)

t.test(color_sum_test$percent_target[which(color_sum_test$cued_type == 'shared_new')],
       color_sum_test$percent_target[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)

# Do participants choose the orthogonal foil more often for shared than unique features
t.test(color_sum_test$percent_orthogonal[which(color_sum_test$cued_type == 'shared_old')],
       color_sum_test$percent_orthogonal[which(color_sum_test$cued_type == 'shared_new')],
       paired = TRUE)

t.test(color_sum_test$percent_orthogonal[which(color_sum_test$cued_type == 'shared_old')],
       color_sum_test$percent_orthogonal[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)

t.test(color_sum_test$percent_orthogonal[which(color_sum_test$cued_type == 'shared_new')],
       color_sum_test$percent_orthogonal[which(color_sum_test$cued_type == 'unique')],
       paired = TRUE)


