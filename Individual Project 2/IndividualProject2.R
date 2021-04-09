data <- read.csv("classification-output-data.csv")
RNGversion('3.5.3')

library(tidyverse)

# Creating the Confusion Matrix
table(data$scored.class, data$class, dnn = c('ScoredClass', 'ActualClass'))[2:1, 2:1]

accuracy_score <- function(df, class, scored) {
  conf <- with(df, table(scored, class))
  return((conf[1] + conf[4])/(conf[1] + conf[3] + conf[4] + conf[2]))
}
data_acc <- accuracy_score(data, class, scored.class)
    
classification_error_score <- function(df, class, scored) {
  conf <- with(df, table(scored, class))
  return((conf[3] + conf[2])/(conf[4] + conf[3] + conf[1] + conf[2]))
}
data_classif <- classification_error_score(data, class, scored.class)

precision_score <- function(df, class, scored) {
  conf <- with(df, table(scored, class))
  return((conf[4])/(conf[4] + conf[2]))
}
data_prec <- precision_score(data, class, scored.class)

sensitivity_score <- function(df, class, scored) {
  conf <- with(df, table(scored, class))
  return((conf[4])/(conf[4] + conf[3]))
}
data_sens <- sensitivity_score(data, class, scored.class)

specificity_score <- function(df, class, scored) {
  conf <- with(df, table(scored, class))
  return((conf[1])/(conf[1] + conf[2]))
}
data_spec <- specificity_score(data, class, scored.class)

f1_score <- function(df, class, scored) {
  prec.score <- precision_score(df, class, scored)
  sens.score <- sensitivity_score(df, class, scored)
  return((2 * prec.score * sens.score)/(prec.score + sens.score))
}
data_f1 <- f1_score(data, class, scored.class)

# Question 9 to be answered on pdf

library(ggplot2)
library(ggthemes)

roc_curve <- function(classification, prob) {
  
  random_guess_line <- seq(0, 1, by=0.01)
  TPRs <- c()
  FPRs <- c()
  
  for (i in 1:length(random_guess_line)){
    
    scores <- ifelse(prob >= random_guess_line[i], 1, 0)
    scored_df <- data.frame(scored.class = scores, class = classification)
    conf <- with(scored_df, table(scored.class, class))
    
    if (length(margin.table(conf,1)) == 1) {
        next
    }
      else {
      TPRs[i] <- (conf[4])/(conf[4] + conf[3])
      FPRs[i] <- (conf[2])/(conf[2] + conf[1])
    }
    
  }
  
  df_plot <- data.frame(Sens = TPRs, FalseP = FPRs)
  # Making sure we have complete cases
  df_plot <- df_plot[complete.cases(df_plot),]
  
  
  area_under_curve <- sum(abs(diff(df_plot$FalseP)) * df_plot$Sens)
  
  ROC_plot <- ggplot(df_plot, aes(x = FalseP, y = Sens)) + 
    theme_pander() + 
    geom_line(col = 'purple', alpha = 2) + 
    geom_abline(intercept = 0, slope = 1) + 
    labs(title="ROC Graph", 
         x = "False Positive Rate", 
         y = "True Positive Rate")
  
  # We have to return the plot as a list because its ggplot
  return(list(ROC_plot, area_under_curve))
}

ROC_list <- roc_curve(data$class, data$scored.probability)
ROC_plot <- ROC_list[[1]]

# Plotting the ROC Graph
ROC_plot

# AUF calculation
area_under_curve <- ROC_list[[2]]

metrics <- c(paste0("Area Under the Curve: ", area_under_curve),
             paste0("Accuracy: ", data_acc),
             paste0('Classification Error Rate: ', data_classif),
             paste0('Precision: ', data_prec),
             paste0('Sensitivity: ', data_sens),
             paste0('Specificity: ', data_spec),
             paste0('F1 Score: ', data_f1))

for (i in metrics) {
  print(i)
}

######################################################################
# Investigating Caret
library(caret)

df_conf_table <- with(data, table(scored.class, class)[2:1,2:1])
confusionMatrix(df_conf_table)
sensitivity(df_conf_table)
specificity(df_conf_table)

library(pROC)
plot(roc(data$class, data$scored.probability), main="ROC Curve from pROC Package")
auc(roc(data$class, data$scored.probability))
