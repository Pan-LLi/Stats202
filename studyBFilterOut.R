library(ggplot2)
library(MASS)
library(RColorBrewer)
print("good")

# Read the data
data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_C.csv")

filtered_data <- data_with_diff %>%
  filter(is.na(diff_visit_day) | diff_visit_day <= 40)

# Get unique countries
filtered_countries <- unique(filtered_data$Country)

# Create a function to plot data for a given country
plot_filtered_country <- function(country) {
  subset_data <- filtered_data[filtered_data$Country == country,]
  
  ggplot(subset_data, aes(x = VisitDay, y = PANSS_Total, color = TxGroup)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE) + # Degree 3 polynomial fit
    scale_color_brewer(palette = "Set1") + # Add a color scale for TxGroup
    ggtitle(paste("Scatter plot for", country)) +
    xlab("Visit Day") +
    ylab("PANSS Total") +
    theme_minimal()
}

# Loop through filtered countries and plot each one
for (country in filtered_countries) {
  print(plot_filtered_country(country))
}

# Load required libraries
library(ggplot2)

# Read the data
data <- read.csv("/Users/youniao/Desktop/FinalProjectData/Study_A.csv")

# Create the P_Total feature
data$P_Total <- rowSums(data[, c("P1", "P2", "P3", "P4", "P5", "P6", "P7")])

# Plot the polynomial fit
ggplot(data, aes(x = P_Total, y = VisitDay)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(color = "3rd Degree Polynomial")) +
  labs(title = "Polynomial Fit of P_Total vs VisitDay",
       x = "P_Total",
       y = "VisitDay",
       color = "Legend") +
  theme_minimal()
