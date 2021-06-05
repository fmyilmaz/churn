
##----------------------------------------------------------------
##                       Loading Packages                       --
##----------------------------------------------------------------

library(annotater) # Annotate Package Load Calls
library(fst) # Lightning Fast Serialization of Data Frames
library(bannerCommenter) # Make Banner Comments with a Consistent Format
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(dplyr) # A Grammar of Data Manipulation
library(tidyr) # Tidy Messy Data
library(yardstick) # Tidy Characterizations of Model Performance

##----------------------------------------------------------------
##                         Reading Data                         --
##----------------------------------------------------------------

churn <- read_fst("Data/churn.fst")


##----------------------------------------------------------------
##                           Viz Data                           --
##----------------------------------------------------------------


ggplot(data = churn, aes(x = time_since_first_purchase, 
                         y = time_since_last_purchase, color = has_churned)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(midpoint = 0.5) +
  theme_bw()


##----------------------------------------------------------------
##                        logistic model                        --
##----------------------------------------------------------------

model <- glm(formula = has_churned ~ time_since_first_purchase * time_since_last_purchase,
    data = churn, family = binomial)


##----------------------------------------------------------------
##                          Prediction                          --
##----------------------------------------------------------------

exp_data <- expand_grid(
  time_since_first_purchase = seq(from = -2, to = 4, by = 0.1),
  time_since_last_purchase = seq(from = -1, to = 6, by = 0.1)
)


pred_data <- exp_data %>% 
  mutate(
    has_churned = predict(model, exp_data, type = "response")
  )


ggplot(data = churn, aes(x = time_since_first_purchase, 
                         y = time_since_last_purchase, color = has_churned)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(midpoint = 0.5) +
  theme_bw() + 
  geom_point(data = pred_data, shape = 15, size = 3)



##----------------------------------------------------------------
##                       Confusion Matrix                       --
##----------------------------------------------------------------

actual_resonse <- churn$has_churned
predicted_response <- round(fitted(model))
outcomes <- table(predicted_response, actual_resonse)
confusion <- conf_mat(outcomes)
autoplot(confusion)
summary(confusion, event_level = "second")
