library(tidyverse)
library(gmodels)

data <- read_csv("random_forest_accuracies.csv")
data$max_depth[is.na(data$max_depth)] <- 0


data$Splits <- factor(data$Splits, levels = c("Split 1 (70/30)", "Split 2 (80/20)", "Split 3 (90/10)"))
data$n_estimators <- factor(data$n_estimators, levels = c("1", "100", "500"))
data$max_depth <- factor(data$max_depth, levels = c("0", "1", "10"))

data_anova <- aov(Accuracy ~ Splits + n_estimators + max_depth + 
                     n_estimators*max_depth + n_estimators*Splits + max_depth*Splits, data = data)

# anova table 
print(summary(data_anova))

# coefficient table 
print(summary.lm(data_anova))

## box-cox transformation 
library(MASS)
boxcox(Accuracy ~ Splits + n_estimators + max_depth + n_estimators:max_depth + 
         n_estimators:Splits + max_depth:Splits, data = data, 
       lambda = seq(0, 30, len = 50), ylab = "Log Likelihood")

## diagnostic plot
plot(data_anova)

## interaction plots
interaction.plot(data$n_estimators, data$max_depth, data$Accuracy, 
                 xlab = "Number of trees", 
                 trace.label = "Maximum depth", 
                 ylab = "Mean of Accuracy")
interaction.plot(data$n_estimators, data$Splits, data$Accuracy, 
                 xlab = "Number of trees", 
                 trace.label = "Splits", 
                 ylab = "Mean of Accuracy")
interaction.plot(data$max_depth, data$Splits, data$Accuracy, 
                 xlab = "Maximum depth", 
                 trace.label = "Splits", 
                 ylab = "Mean of Accuracy")


## contrasts
LowVersusHighEstimators <- c(-1, 0, 1)
MedVersusHighEstimators <- c(0, -1, 1)
InfDepthVersusHighDepth <- c(-1, 0, 1)
InfDepthVersusMidDepth <- c(-1, 1, 0)

contrasts(data$n_estimators) <- cbind(LowVersusHighEstimators, MedVersusHighEstimators)
contrasts(data$max_depth) <- cbind(InfDepthVersusHighDepth, InfDepthVersusMidDepth)

data_contrasts_anova <- aov(Accuracy ~ Splits + n_estimators + max_depth + 
                              n_estimators:max_depth + Splits:max_depth, data = data)

summary(data_contrasts_anova, 
        split = list(n_estimators = list(LowVersusHighEstimators = 1, MedVersusHighEstimators = 2),
                     max_depth = list(InfDepthVersusHighDepth = 1, InfDepthVersusMidDepth = 2)))

# calculate the confidence interval for each effect
model_contrast_coef <- coef(summary.lm(data_contrasts_anova)) 

df_residual <- 12 # based on the aov table 
t_critical <- qt(0.975, df_residual)

ci_table <- data.frame(
  Estimate = model_contrast_coef[, "Estimate"],
  Std_Error = model_contrast_coef[, "Std. Error"],
  Lower_CI = model_contrast_coef[, "Estimate"] - t_critical * model_contrast_coef[, "Std. Error"],
  Upper_CI = model_contrast_coef[, "Estimate"] + t_critical * model_contrast_coef[, "Std. Error"]
)

print(ci_table)