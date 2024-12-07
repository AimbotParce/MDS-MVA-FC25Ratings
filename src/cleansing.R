library(crayon)
library(car)
library(FactoMineR)
library(chemometrics)
library(corrplot)
library(RColorBrewer)
library(PerformanceAnalytics)
library(mice)
library(dplyr)
library(readr)
library(stringr)
library(MASS)
library(lmtest)
library(sandwich)
library(cluster)

# Load datasets
male_players <- read_csv("src/data/male_players.csv")
female_players <- read_csv("src/data/female_players.csv")

# Add the 'Sex' column to each dataset
male_players$Sex <- factor("Male", levels = c("Male", "Female"))
female_players$Sex <- factor("Female", levels = c("Male", "Female"))

# Combine the datasets into one
all_players <- bind_rows(male_players, female_players)

# Remove the first two columns
all_players <- all_players[, -c(1, 2)]

# Convert appropriate columns to factors
all_players$Position <- as.factor(all_players$Position)
all_players$'Alternative positions' <- as.factor(all_players$'Alternative positions')
all_players$'Preferred foot' <- as.factor(all_players$'Preferred foot')
all_players$'Weak foot' <- factor(all_players$'Weak foot', ordered = TRUE)
all_players$'Skill moves' <- factor(all_players$'Skill moves', ordered = TRUE)
all_players$League <- as.factor(all_players$League)
all_players$Nation <- as.factor(all_players$Nation)
all_players$Team <- as.factor(all_players$Team)
all_players$'play style' <- as.factor(all_players$'play style')

# Convert Height to numeric (extract cm and convert to meters)
all_players$Height <- as.numeric(gsub("cm.*", "", all_players$Height)) / 100

# Convert Weight to numeric (extract kg and keep it as kg)
all_players$Weight <- as.numeric(gsub("kg.*", "", all_players$Weight))

#Duplicate data
duplicated_row_count <- sum(duplicated(all_players))
if (duplicated_row_count > 0) {
  print(sprintf("There are %d duplicated rows.", duplicated_row_count))
  all_players <- unique(all_pllayers)
} else {
  print("No duplicates in the dataset")
}

#Missing data
for (colname in colnames(all_players)) {
  na.count <- sum(is.na(all_players[[colname]]))
  if (na.count > 0) {
    cat(sprintf("%s has %s\n", colname, red(sprintf("%d N/As", sum(is.na(all_players[colname]))))))
  }
}

#We remove Alternative position, GK Diving, GK Handling, GK Kicking, GK Positioning, GK Reflexes and 
#Playstyle (factor variable with 1700 factors) due to the huge amount of outliers and the the type. We also remove
#as it seems more like a unique idefntificator for the players.
gk_players <- all_players %>%
  filter(Position == "GK")

all_players <- all_players[, -c(45, 50:56)]

#Outliers
for (colname in colnames(Filter(is.numeric, all_players))) {
  col = all_players[[colname]]
  q1 <- quantile(col, 0.25)
  q3 <- quantile(col, 0.75)
  iqr <- q3 - q1
  
  severe <- list(top = q3 + 3 * iqr, bot = q1 - 3 * iqr)
  mild <- list(top = q3 + 1.5 * iqr, bot = q1 - 1.5 * iqr)
  
  severe_out <- sum(col > severe$top | col < severe$bot)
  mild_out <- sum((col > mild$top & col < severe$top) | (col < mild$bot & col > severe$bot))
  if (mild_out > 0 | severe_out > 0) {
    cat(sprintf("Column %s has %d mild outliers and %d severe outliers\n", colname, mild_out, severe_out))
  }
}

# Enhanced boxplot for Ball Control with outlier markers
boxplot(all_players$`Ball Control`, 
        col = "skyblue", 
        border = "darkblue", 
        outcol = "red",  # Outlier color
        pch = 16,        # Outlier shape
        cex = 1.5,       # Outlier size
        notch = TRUE,     # Notched boxplot for better visualization
        boxwex = 0.5,     # Adjust the width of the boxes
        sub = "389 severe outliers")  # Subtitle showing the number of severe outliers

# Enhanced boxplot for Short Passing with outlier markers
boxplot(all_players$`Short Passing`, 
        col = "lightgreen", 
        border = "darkgreen", 
        outcol = "red",  # Outlier color
        pch = 16,        # Outlier shape
        cex = 1.5,       # Outlier size
        notch = TRUE,     # Notched boxplot
        boxwex = 0.5,     # Adjust the width of the boxes
        sub = "22 severe outliers")  # Subtitle showing the number of severe outliers

#Row-wise, we'll count the numeric variables in which each data point is an 
#outlier, and create a new object called `univariate_outlier_count`. As a 
#gut-driven criterion, we shall consider a row to be an outlier if it is an
#outlier in 10 or more variables. Based on this criterion, only 9 counties are.

count_outliers <- function(data) {
  # Function to check for outliers based on IQR
  is_outlier <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(x < lower_bound | x > upper_bound)
  }
  
  # Apply the outlier function to each column and sum the results for each row using dplyr
  data %>%
    mutate(outlier_count = rowSums(sapply(., is_outlier), na.rm = TRUE))
}

univariate_outlier_count <- count_outliers(Filter(is.numeric, all_players))$outlier_count
all_players[which(univariate_outlier_count >= 10),]
length(all_players[which(univariate_outlier_count >= 10),])

#Outliers can sometimes provide valuable information, so they won’t be removed from the data set just yet.

#Multivariate outliers

numeric.df <- Filter(is.numeric, all_players)
numeric.df <- numeric.df[, !colnames(numeric.df) %in% c("studypercap")]

res.out_95 <- Moutlier(numeric.df, quantile = 0.95, plot=F)
multi_outliers_95 = which((res.out_95$md > res.out_95$cutoff)&(res.out_95$rd > res.out_95$cutoff))
length(multi_outliers_95)

res.out <- Moutlier(numeric.df, quantile = 0.9999995, plot=F)
multi_outliers = which((res.out$md > res.out$cutoff)&(res.out$rd > res.out$cutoff))
length(multi_outliers)

par(mfrow = c(1,1))
plot(res.out$rd, res.out$md )
abline(h=res.out$cutoff, col="red")
abline(v=res.out$cutoff, col="red")

#Normality
# Loop through all numeric variables in the dataset
numeric_vars <- select_if(all_players, is.numeric)

# Load necessary libraries
library(dplyr)

# Loop through all numeric variables in the dataset
numeric_vars <- select_if(all_players, is.numeric)

for (var_name in names(numeric_vars)) {
  cat("Analyzing variable:", var_name, "\n")
  
  # Summarize the variable
  var_data <- numeric_vars[[var_name]]
  cat("Summary of", var_name, ":\n")
  print(summary(var_data))
  
  # Create histogram and add a normal distribution curve
  hist(var_data, breaks = 30, freq = FALSE, main = paste("Histogram of", var_name), 
       xlab = var_name, col = "lightblue", border = "black")
  curve(dnorm(x, mean(var_data), sd(var_data)), add = TRUE, col = "red", lwd = 2)
  
  # Handle large sample sizes for Shapiro-Wilk
  if (length(var_data) > 5000) {
    cat("Sample size is too large for Shapiro-Wilk test. Using a random sample of 5000.\n")
    var_data <- sample(var_data, 5000)  # Random sample of 5000 observations
  }
  
  # Perform Shapiro-Wilk normality test
  normality_test <- shapiro.test(var_data)
  cat("Shapiro-Wilk normality test p-value for", var_name, ":", normality_test$p.value, "\n\n")
  
  # Add a comment based on the p-value
  if (normality_test$p.value < 0.05) {
    cat("The data does not appear to be normally distributed (p-value < 0.05).\n\n")
  } else {
    cat("The data appears to be normally distributed (p-value >= 0.05).\n\n")
  }
}



#Save data
save(all_players, file = file.path("src/data/cleansed_data.RData"))


