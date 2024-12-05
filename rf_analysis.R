library(tidyverse)

data <- read_csv("random_forest_accuracies_new.csv")
data$max_depth[is.na(data$max_depth)] <- 0


data$Splits <- factor(data$Splits, levels = c("Split 1 (70/30)", "Split 2 (80/20)", "Split 3 (90/10)"))
data$n_estimators <- factor(data$n_estimators, levels = c("1", "100", "500"))
data$max_depth <- factor(data$max_depth, levels = c("0", "1", "10"))

data_anova <- aov(Accuracy ~ Splits + n_estimators + max_depth + 
                     n_estimators*max_depth, data = data)

print(summary(data_anova))

## box-cox transformation 
library(MASS)
boxcox(Accuracy ~ Splits + n_estimators + max_depth + n_estimators:max_depth, data = data, 
       lambda = seq(0, 30, len = 50), ylab = "Log Likelihood")

## contrasts
LowVersusHighEstimators <- c(-1, 0, 1)
MedVersusHighEstimators <- c(0, -1, 1)
InfDepthVersusHighDepth <- c(-1, 0, 1)
InfDepthVersusMidDepth <- c(-1, 1, 0)

contrasts(data$n_estimators) <- cbind(LowVersusHighEstimators, MedVersusHighEstimators)
contrasts(data$max_depth) <- cbind(InfDepthVersusHighDepth, InfDepthVersusMidDepth)

data_contrasts_anova <- aov(Accuracy ~ Splits + n_estimators + max_depth + 
                              n_estimators:max_depth, data = data)

summary(data_contrasts_anova, 
        split = list(n_estimators = list(LowVersusHighEstimators = 1, MedVersusHighEstimators = 2),
                     max_depth = list(InfDepthVersusHighDepth = 1, InfDepthVersusMidDepth = 2)))

# coef(summary.lm(data_contrasts_anova))
