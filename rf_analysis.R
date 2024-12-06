library(tidyverse)
library(gmodels)

data <- read_csv("random_forest_accuracies.csv")
data$max_depth[is.na(data$max_depth)] <- 0


data$Splits <- factor(data$Splits, levels = c("Split 1 (70/30)", "Split 2 (80/20)", "Split 3 (90/10)"))
data$n_estimators <- factor(data$n_estimators, levels = c("1", "100", "500"))
data$max_depth <- factor(data$max_depth, levels = c("0", "1", "10"))

data_anova <- aov(Accuracy^5 ~ Splits + n_estimators + max_depth + 
                     n_estimators*max_depth + n_estimators*Splits + max_depth*Splits, data = data)

# anova table 
print(summary(data_anova))

# coefficient table 
print(summary.lm(data_anova))

## box-cox transformation 
library(MASS)
boxcox((Accuracy)^5 ~ Splits + n_estimators + max_depth + n_estimators:max_depth + 
         n_estimators:Splits + max_depth:Splits, data = data, 
       lambda = seq(0, 5, len = 50), ylab = "Log Likelihood")

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

# # calculate the confidence interval for each effect
# 
# df_residual <- 12
# t_critical <- qt(0.975, df_residual)
# 
# ci_table <- data.frame(
#   Estimate = data_contrasts_anova.coef[, "Estimate"],
#   Std_Error = data_contrasts_anova.coef[, "Std. Error"],
#   Lower_CI = data_contrasts_anova.coef[, "Estimate"] - t_critical * data_contrasts_anova.coef[, "Std. Error"],
#   Upper_CI = data_contrasts_anova.coef[, "Estimate"] + t_critical * data_contrasts_anova.coef[, "Std. Error"]
# )
# 
# print(ci_table)

# calculate the confidence interval of the two most significant contrasts
data_contrasts_anova.coef <- coef(summary.lm(data_contrasts_anova))
df_residual <- 12

# n_estimators: LowVersusHighEstimators
coef.LVHestimators <- data_contrasts_anova.coef[4,1]
coef.LVHestimators.se <- data_contrasts_anova.coef[4,2]
lb.LVHestimators <- coef.LVHestimators - qt(0.975,df_residual) * coef.LVHestimators.se
ub.LVHestimators <- coef.LVHestimators + qt(0.975,df_residual) * coef.LVHestimators.se

sprintf("(%.4f, %.4f)", lb.LVHestimators, ub.LVHestimators)

# max_depth: InfDepthVersusHighDepth
coef.IVHmaxdepth <- data_contrasts_anova.coef[6,1]
coef.IVHmaxdepth.se <- data_contrasts_anova.coef[6,2]
lb.IVHmaxdepth <- coef.IVHmaxdepth - qt(0.975,df_residual) * coef.IVHmaxdepth.se
ub.IVHmaxdepth <- coef.IVHmaxdepth + qt(0.975,df_residual) * coef.IVHmaxdepth.se

sprintf("(%.4f, %.4f)", lb.IVHmaxdepth, ub.IVHmaxdepth)
