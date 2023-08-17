library(ggplot2)
library(MASS)
library(RColorBrewer)
library(tidyr)
library(caret)
data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_E.csv")

#Preprocessing
percentage_large_gap <- mean(data_with_diff$diff_visit_day > 40, na.rm = TRUE) * 100
cat("Percentage of experiments with more than 40 days gap:", percentage_large_gap, "%\n")
data_with_diff <- data %>%
  arrange(PatientID, VisitDay) %>%
  group_by(PatientID) %>%
  mutate(diff_visit_day = VisitDay - lag(VisitDay)) %>%
  ungroup()

# Calculate the mean and median of the differences
mean_diff_all <- mean(data_with_diff$diff_visit_day, na.rm = TRUE)
median_diff_all <- median(data_with_diff$diff_visit_day, na.rm = TRUE)

cat("Mean of days between experiments:", mean_diff_all, "\n")
cat("Median of days between experiments:", median_diff_all, "\n")


visit_counts <- table(data4$PatientID)
patients_with_fewer_than_2_visits <- sum(visit_counts < 2)
percentage_patients_fewer_than_2_visits <- (patients_fewer_than_2_visits / length(unique(data4$PatientID))) * 100
percentage_patients_fewer_than_2_visits

patients_with_duplicates <- data %>%
  group_by(PatientID, VisitDay) %>%
  filter(n() > 1) %>%
  distinct(PatientID)

percentage_with_duplicates <- (nrow(patients_with_duplicates) / n_distinct(data$PatientID)) * 100
print(patients_with_duplicates$PatientID)
print(paste("Percentage of patients with multiple experiments on the same VisitDay: ", round(percentage_with_duplicates, 2), "%"))



#Tune

data <- data %>%
  group_by(PatientID) %>%
  mutate(Weight = n() / sum(n())) %>%
  ungroup()

formula <- PANSS_Total ~ VisitDay + P1 + P2 + P3 + P4 + P5 + P6 + P7 + N1 + N2 + N3 + N4 + N5 + N6 + N7 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16
# Cross Validation
train_control <- trainControl(method = "cv", number = 5)

tune_grid <- expand.grid(
  n.trees = c(50,150,200),
  interaction.depth = c(1, 3, 5, 10, 15),
  shrinkage = c(0.01,0.05,0.1),
  n.minobsinnode = c(5, 10, 20)
)


gbm_model_tuned <- train(
  formula,
  data = data,
  method = "gbm",
  trControl = train_control,
  verbose = FALSE,
  tuneGrid = tune_grid
)


print(gbm_model_tuned)



data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_E.csv")
#Weight
data <- data %>%
  group_by(PatientID) %>%
  mutate(Weight = n() / sum(n())) %>%
  ungroup()

set.seed(1)
train_index <- sample(1:nrow(data), nrow(data) * 0.7)
train_set <- data[train_index, ]
test_set <- data[-train_index,]

#Mixed Selection
full_formula <- PANSS_Total ~	AssessmentID+ Weight+ TxGroup + VisitDay + P1 + P2 + P3 + P4 + P5 + P6 + P7 + N1 + N2 + N3 + N4 + N5 + N6 + N7 + G1 + G2 + G3 + G4 + G5 + G6 + G7 + G8 + G9 + G10 + G11 + G12 + G13 + G14 + G15 + G16
start_formula <- PANSS_Total ~ 1
start_fit <- lm(start_formula, data=train_set)
end_fit <- stepAIC(start_fit, scope=list(lower=start_formula, upper=full_formula), direction="both")
print(step_fit)
selected_formula <- as.formula(end_fit$call[[2]])
print(selected_formula)

gbm_model <- gbm(selected_formula,
                 data = train_set,
                 n.trees = 150,
                 interaction.depth = 5,
                 shrinkage = 0.1,
                 n.minobsinnode = 20)
 
prediction_data <- data %>%
  group_by(PatientID) %>%
  filter(VisitDay == max(VisitDay)) %>%
  ungroup() %>%
  mutate(VisitDay = VisitDay + 40)  

predictions <- predict(gbm_model, prediction_data, n.trees = 150)
summary(gbm_model)

prediction_data$PANSS_Total <- predictions

result_data <- prediction_data %>%
  select(PatientID, PANSS_Total) %>%
  distinct(PatientID, .keep_all = TRUE) 

write.csv(result_data, "/Users/youniao/Desktop/FinalProjectData/Predicted_PANSS_Total.csv", row.names = FALSE)

summary(gbm_model)

#Check Rows
predicted_data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Predicted_PANSS_Total.csv")
cat("Number of rows:", nrow(predicted_data), "\n")
cat("Number of unique PatientID:", length(unique(predicted_data$PatientID)), "\n")

#Plot Result

data$Predicted_PANSS_Total <- predict(gbm_model, newdata = data, n.trees = 150)

ggplot(data, aes(x = VisitDay)) +
  geom_point(aes(y = PANSS_Total), color = "blue", alpha = 0.5) +
  geom_point(aes(y = Predicted_PANSS_Total), color = "green", alpha = 0.5) + 
  ggtitle("Actual vs Predicted") +
  xlab("VisitDay") +
  ylab("PANSS_Total") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green"), labels = c("Actual", "Predicted"))




library(caret)
data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_E.csv")
# Sort the data by VisitDay
data <- data[order(data$VisitDay),]

data <- data %>%
  arrange(PatientID, VisitDay) %>%
  group_by(PatientID) %>%
  mutate(VisitNumber = row_number()) %>%
  ungroup()

# Group by VisitNumber and calculate the median VisitDay for each group
median_visits <- data %>%
  group_by(VisitNumber) %>%
  summarise(MedianVisitDay = median(VisitDay)) %>%
  arrange(MedianVisitDay) %>%
  pull(MedianVisitDay)

median_visits <- unique(median_visits)
print(median_visits)

