setwd("C:/Users/Twinkle Patel/Desktop/West Georgia/Semester 2/Rstudio/Final Exam")

library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)

# Load the data
data <- read_dta("acs.dta")

# Subset data to include only individuals aged 18-64
filtered_data <- data %>%
  filter(age >= 18 & age <= 64)

# Sample 10% of the data
sampled_data <- filtered_data %>%
  sample_n(size = nrow(filtered_data) * 0.1, replace = FALSE)

sampled_data$metro <- as.factor(sampled_data$metro)
sampled_data$labforce <- as.factor(sampled_data$labforce)


# Descriptive analysis
# Labor Force Participation by Metropolitan Status
metro_labels <- c(
  "0" = "Status Indeterminable",
  "1" = "Not in Metro Area",
  "2" = "Metro Area: Central/Primary",
  "3" = "Metro Area: Not Central",
  "4" = "Metro Area: Central/Principal"
)

p1 <- ggplot(sampled_data, aes(x = factor(metro), fill = factor(labforce))) +
      geom_bar(position = "fill") +
      scale_x_discrete(labels = metro_labels) +  # Set custom x-axis labels
      scale_fill_discrete(
         labels = c("Not in labor force", "In labor force")
      ) +
      labs(
         title = "Labor Force Participation by Metro Status",
         x = "Metropolitan Status",
         y = "Proportion",
         fill = "Labor Force"
      ) +
      theme_minimal()

# Labor Force Participation by Gender
   metro_sex <- c(
       "1" = "Male",
       "2" = "Female"
        )

p2 <- ggplot(sampled_data, aes(x = factor(sex), fill = factor(labforce))) +
      geom_bar(position = "fill") +
      scale_x_discrete(labels = metro_sex) +  # Set custom x-axis labels
      scale_fill_discrete(
      labels = c("Not in labor force", "In labor force")) +
      labs(
         title = "Labor Force Participation by Gender",
         x = "Gender",
         y = "Proportion",
         fill = "Labor Force") +
         theme_minimal()

# Create a summarized data frame by age and labforce
income_age_summary <- sampled_data %>%
                      group_by(age, labforce) %>%
                      summarize(
                       mean_hhincome = mean(hhincome, na.rm = TRUE),  .groups = "drop")


library(scales) 

# Labor Force Participation by Age and Average Household Income
p3 <- ggplot(income_age_summary, aes(x = age, y = mean_hhincome, color = factor(labforce))) +
             geom_line(size = 1) +  # Line plot
             geom_point(size = 2) +  # Add points for emphasis
             scale_color_manual(
                values = c("red", "green"), 
                labels = c("Not in labor force", "In labor force")) +
             scale_y_continuous(
                labels = dollar) +
             labs(
                title = "Labor Force Participation by Age and Average Household Income",
                x = "Age",
                y = "Average Household Income",
                color = "Labor Force") +
             theme_minimal()


# Predictive modelling

sampled_data.2 <-  select(sampled_data, age, sex, marst,labforce, educ,  metro, 
                          race, hispan, uhrswork, poverty, classwkr)
sampled_data.2 <- na.omit(sampled_data.2)

sampled_data.2$labforce <- as.character(sampled_data.2$labforce)

sampled_data.2$labforce_dummy <- ifelse(sampled_data.2$labforce == '1', 1, 0)

# Logistic regression model

model <- glm(labforce_dummy ~ age + sex + marst  + educ  + metro +
               race + hispan + uhrswork + poverty  + classwkr,
             data = sampled_data.2,
             family = binomial)

summary(model)


sampled_data.2$labforce_dummy_prob <- predict(model, type="response")
sampled_data.2$labforce_dummy_hat <- ifelse(sampled_data.2$labforce_dummy_prob>0.5,1,0)

accuracy <- mean(sampled_data.2$labforce_dummy == sampled_data.2$labforce_dummy_hat)
print(accuracy)


# Checking predication with confusion matrix

sampled_data.2$labforce <- as.factor(sampled_data.2$labforce)

sampled_data.2$labforce_dummy_hat.1 <- as.factor(ifelse(sampled_data.2$labforce_dummy_prob>0.5,"1","2"))
confusionMatrix(sampled_data.2$labforce, sampled_data.2$labforce_dummy_hat.1)

# Cross-Validation with train

model.2 <- train(labforce_dummy ~ age + sex + marst  + educ + metro +
                   race + hispan + uhrswork + poverty  + classwkr,
                 data = sampled_data.2,
                 method = "glm",
                 family = "binomial",
                 trControl = trainControl(
                   method = "cv",
                   number = 10
                 )
)

print(model.2)

# spiting data set in train and test for further analysis
set.seed(12345)

index <- sample(1:nrow(sampled_data.2), size = nrow(sampled_data.2)*0.8, replace=FALSE)

train_data <- sampled_data.2[index,]
test_data <- sampled_data.2[-index,]


predictors <- train_data[, c("age", "sex", "marst", "educ",  "metro", 
                             "race", "hispan", "uhrswork", "poverty", "classwkr")]

train_data$labforce_dummy <- as.factor(train_data$labforce_dummy)
outcome <- train_data[, c("labforce_dummy")]

outcome.1 <- train_data$labforce_dummy

# RFE for Feature Selection
model.rfe <- rfe(x=predictors, y=outcome.1,
                 size = c(1:10),
                 rfeControl = rfeControl(
                   functions = rfFuncs,
                   method = "cv",
                   number = 5,
                   verbose = TRUE
                 )
)

print(model.rfe)


print(model.rfe)

ggplot(model.rfe, metric = "Accuracy") + theme_classic()
print(model.rfe$fit)

result.test <- predict(model.rfe, newdata = test_data)
test_data$labforce_dummy <- as.factor(test_data$labforce_dummy) 
confusionMatrix(test_data$labforce_dummy, result.test$pred)


library(caret)
library(rpart.plot)

# Train a decision tree model
set.seed(123) 

model.tree <- train(labforce_dummy ~ age + sex + marst + educ + metro + race + hispan + uhrswork + poverty + classwkr,
                    data = train_data,
                    method = "rpart",
                    trControl = trainControl(method="cv", number=10),
                    tuneLength = 10
)

print(model.tree)

# Plot the decision tree
library(rpart.plot)
rpart.plot(model.tree$finalModel) 

# Predict on the test data
predictions <- predict(model.tree, newdata = test_data)

# Compute the confusion matrix and accuracy
conf_matrix <- confusionMatrix(predictions, test_data$labforce_dummy)
print(conf_matrix)
















