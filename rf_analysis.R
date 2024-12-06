library(tidyverse)
library(gmodels)
library(MASS)

# read data from Repo 
data <- read_csv("https://raw.githubusercontent.com/moirarenata/stat404_rf_project/refs/heads/main/random_forest_accuracies.csv")

# replace 'None' level with 0 
data$max_depth[is.na(data$max_depth)] <- 0

data$Splits <- factor(data$Splits, levels = c("Split 1 (70/30)", "Split 2 (80/20)", "Split 3 (90/10)"))
data$n_estimators <- factor(data$n_estimators, levels = c("1", "100", "500"))
data$max_depth <- factor(data$max_depth, levels = c("0", "1", "10"))

# create ANOVA model with main effects, blocking, and interaction effects
data_anova <- aov(Accuracy ~ Splits + n_estimators + max_depth + 
                     n_estimators*max_depth + 
                    n_estimators*Splits + 
                    max_depth*Splits, data = data)

# summary of ANOVA model 
print(summary(data_anova))

# estimated effects table 
print(summary.lm(data_anova))

# box-cox transformation 
boxcox(Accuracy ~ Splits + n_estimators + max_depth + n_estimators:max_depth + 
         n_estimators:Splits + max_depth:Splits, 
       data = data, 
       lambda = seq(0, 30, len = 50), 
       ylab = "Log Likelihood")
title("Box-Cox Transformation Plot")

# diagnostic plots: residual vs fitted and qq-plot
plot(data_anova,  which = 1) 
plot(data_anova,  which = 2) 

# interaction plots
interaction.plot(data$n_estimators, data$max_depth, data$Accuracy, 
                 xlab = "Number of trees", 
                 trace.label = "Maximum depth", 
                 ylab = "Mean of Accuracy", 
                 main = "Interaction Plot of n_estimators, by max_depth")

interaction.plot(data$n_estimators, data$Splits, data$Accuracy, 
                 xlab = "Number of trees", 
                 trace.label = "Splits", 
                 ylab = "Mean of Accuracy", 
                 main = "Interaction Plot of n_estimators, by Splits")

interaction.plot(data$max_depth, data$Splits, data$Accuracy, 
                 xlab = "Maximum depth", 
                 trace.label = "Splits", 
                 ylab = "Mean of Accuracy", 
                 main = "Interaction Plot of max_depth, by Splits")


## Contrasts
LowVersusHighEstimators <- c(-1, 0, 1)
MedVersusHighEstimators <- c(0, -1, 1)
InfDepthVersusHighDepth <- c(-1, 0, 1)
InfDepthVersusMidDepth <- c(-1, 1, 0)

contrasts(data$n_estimators) <- cbind(LowVersusHighEstimators, MedVersusHighEstimators)
contrasts(data$max_depth) <- cbind(InfDepthVersusHighDepth, InfDepthVersusMidDepth)

# contrasts of n_estimators
contrast_table1 <- data.frame(
  Contrast = c("InfDepthVersusHighDepth", "InfDepthVersusMidDepth"),
  Level1 = c(-1, -1),
  Level2 = c(0, 1),
  Level3 = c(1, 0))
print(contrast_table1)

# contrasts of max_depth
contrast_table2 <- data.frame(
  Contrast = c("LowVersusHighEstimators", "MedVersusHighEstimators"),
  Level1 = c(-1, 0),
  Level2 = c(0, -1),
  Level3 = c(1, 1))
print(contrast_table2)

# create ANOVA model with significant effects and their contrasts
data_contrasts_anova <- aov(Accuracy ~ Splits + n_estimators + max_depth + 
                              n_estimators:max_depth + Splits:max_depth, data = data)

print(summary(data_contrasts_anova, 
        split = list(n_estimators = list(LowVersusHighEstimators = 1, MedVersusHighEstimators = 2),
                     max_depth = list(InfDepthVersusHighDepth = 1, InfDepthVersusMidDepth = 2))))

# CI of the two most significant contrasts
data_contrasts_anova.coef <- coef(summary.lm(data_contrasts_anova))
df_residual <- 12

# n_estimators: LowVersusHighEstimators
coef.LVHestimators <- data_contrasts_anova.coef[4,1]
coef.LVHestimators.se <- data_contrasts_anova.coef[4,2]
lb.LVHestimators <- coef.LVHestimators - qt(0.975,df_residual) * coef.LVHestimators.se
ub.LVHestimators <- coef.LVHestimators + qt(0.975,df_residual) * coef.LVHestimators.se

cat(sprintf("The 95%% confidence interval for the true value of the LowVersusHighEstimators contrast is (%.4f, %.4f).\n", lb.LVHestimators, ub.LVHestimators))

# max_depth: InfDepthVersusHighDepth
coef.IVHmaxdepth <- data_contrasts_anova.coef[6,1]
coef.IVHmaxdepth.se <- data_contrasts_anova.coef[6,2]
lb.IVHmaxdepth <- coef.IVHmaxdepth - qt(0.975,df_residual) * coef.IVHmaxdepth.se
ub.IVHmaxdepth <- coef.IVHmaxdepth + qt(0.975,df_residual) * coef.IVHmaxdepth.se

cat(sprintf("The 95%% confidence interval for the true value of the InfDepthVersusHighDepth contrast is (%.4f, %.4f).\n", lb.IVHmaxdepth, ub.IVHmaxdepth))
