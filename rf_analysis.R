library(tidyverse)

data <- read_csv("random_forest_accuracies.csv")

data$Block <- factor(data$Block, levels = c("Split 1 (70/30)", "Split 2 (80/20)", "Split 3 (90/10)"))
data$n_estimators <- factor(data$n_estimators, levels = c("50", "100", "200"))
data$max_depth <- factor(data$max_depth, levels = c("None", "1", "10"))

data_anova <- aov(Accuracy ~ Block + n_estimators + max_depth + 
                     n_estimators:max_depth, data = data)

print(summary(data_anova))


## contrasts
LowVersusHighEstimators <- c(-1, 0, 1)
LowMidVersusHighEstimators <- c(1, 1, -2)
NoDepthVersusHighDepth <- c(-1, 0, 1)
LowDepthVersusHighDepth <- c(1, 1, -2)

contrast_estimators <- cbind(LowVersusHighEstimators, LowMidVersusHighEstimators)
contrast_depth <- cbind(NoDepthVersusHighDepth, LowDepthVersusHighDepth)

contrasts(data$n_estimators) <- contrast_estimators
contrasts(data$max_depth) <- contrast_depth

data_contrasts_anova <- aov(Accuracy ~ Block + n_estimators + max_depth + 
                              n_estimators:max_depth, data = data)

summary(data_contrasts_anova, 
        split = list(
          n_estimators = list(
            LowVersusHighEstimators = 1,
            LowMidVersusHighEstimators = 2
          ), 
          max_depth = list(
            NoDepthVersusHighDepth = 1, 
            LowDepthVersusHighDepth = 2
          )
        ))

