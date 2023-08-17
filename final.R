library(ggplot2)
library(splines)
library(tidyr)
library(dplyr)
library(randomForest)

data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")

#data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_D.csv")

data$P_Total <- rowSums(data[, c("P1", "P2", "P3", "P4", "P5", "P6", "P7")])
day0_data <- data[data$VisitDay == 0,]


set.seed(123) 
clusters <- kmeans(day0_data$P_Total, centers = 4)
day0_data$Cluster <- clusters$cluster

data <- merge(data, day0_data[, c("PatientID", "Cluster")], by = "PatientID", all.x = TRUE)

data$Group <- paste(data$TxGroup, "Cluster", data$Cluster)

plot <- ggplot(data, aes(x = VisitDay, y = P_Total, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(group = Group)) +
  labs(title = "Polynomial Fit of P_Total vs VisitDay for each Group",
       x = "VisitDay",
       y = "P_Total",
       color = "Group") +
  theme_minimal() +
  scale_color_brewer(palette = "Set3")

print(plot)


data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")
data$N_Total <- rowSums(data[, c("N1", "N2", "N3", "N4", "N5", "N6", "N7")])

day0_data <- data[data$VisitDay == 0,]
clusters <- kmeans(day0_data$N_Total, centers = 4)
day0_data$Cluster <- clusters$cluster
data <- merge(data, day0_data[, c("PatientID", "Cluster")], by = "PatientID", all.x = TRUE)

data$Group <- paste(data$TxGroup, "Cluster", data$Cluster)

plot <- ggplot(data, aes(x = VisitDay, y = N_Total, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(group = Group)) +
  labs(title = "Polynomial Fit of N_Total vs VisitDay for each Group",
       x = "VisitDay",
       y = "N_Total",
       color = "Group") +
  theme_minimal() +
  scale_color_brewer(palette = "Set3")

print(plot)





data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")

data$G_Total <- rowSums(data[, c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12", "G13", "G14", "G15", "G16")])


day0_data <- data[data$VisitDay == 0,]

set.seed(123)  # for reproducibility
clusters <- kmeans(day0_data$G_Total, centers = 4)
day0_data$Cluster <- clusters$cluster

data <- merge(data, day0_data[, c("PatientID", "Cluster")], by = "PatientID", all.x = TRUE)

data$Group <- paste(data$TxGroup, "Cluster", data$Cluster)

plot <- ggplot(data, aes(x = VisitDay, y = G_Total, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(group = Group)) +
  labs(title = "Polynomial Fit of G_Total vs VisitDay for each Group",
       x = "VisitDay",
       y = "G_Total",
       color = "Group") +
  theme_minimal() +
  scale_color_brewer(palette = "Set3")

print(plot)

  










library(ggplot2)
data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")
data$G_Total <- rowSums(data[, c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12", "G13", "G14", "G15", "G16")])

shapes <- c(8, 9)

ggplot(data, aes(x = VisitDay, y = G_Total, color = TxGroup)) +
  geom_point(aes(shape = TxGroup), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(group = TxGroup)) +
  labs(title = "Polynomial Fit of G_Total vs VisitDay for Control and Treatment",
       x = "VisitDay",
       y = "G_Total",
       color = "TxGroup",
       shape = "TxGroup") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = shapes)




data$N_Total <- rowSums(data[, c("N1", "N2", "N3", "N4", "N5", "N6", "N7")])

shapes <- c(8, 9)

ggplot(data, aes(x = VisitDay, y = N_Total, color = TxGroup)) +
  geom_point(aes(shape = TxGroup), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(group = TxGroup)) +
  labs(title = "Polynomial Fit of N_Total vs VisitDay for Control and Treatment",
       x = "VisitDay",
       y = "N_Total",
       color = "TxGroup",
       shape = "TxGroup") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = shapes)




