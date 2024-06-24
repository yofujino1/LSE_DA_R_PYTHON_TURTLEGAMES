Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Your code here.
#Step1 : Loading and Wrangling the Data
install.packages("dplyr")
install.packages("tidyverse")


#Import libraries:
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting
library(tidyverse)
# Install the moments package and load the library.
install.packages('moments') 
library(moments)

# Install the psych package.
install.packages('psych')
# Import the psych package.
library(psych)

# Install caret package
install.packages("caret")
# Install car package
install.packages("car")

# Load car package

library(caret)  # For model evaluation
library(car)    # For model diagnostics

# Get the current working directory
current_dir <- getwd()
# Print the current working directory
print(current_dir)

# Change directory
new_directory <- "C:/Users/fuyo8001/OneDrive - Nielsen IQ/Personal/2023/LSE/Course 3/Assignment/LSE_DA301_assignment_files/LSE_DA301_assignment_files_new"
setwd(new_directory)

# Verify the current working directory
getwd()

# List files in the current working directory
list.files()

# Read "turtle_reviews.csv"
df <- read_csv("C:/Users/fuyo8001/OneDrive - Nielsen IQ/Personal/2023/LSE/Course 3/Assignment/LSE_DA301_assignment_files/LSE_DA301_assignment_files_new/turtle_reviews.csv")

# Check the structure of the dataframe
str(df)

# View the first few rows of the dataframe
head(df)

# Summary of the data
dim(df)
summary(df)

--------------------
# Check the  column names
colnames(df) 
#drop and rename columns

# Drop 'language' and 'platform'
df <- df %>%
  select(-language, -platform)
# Check the updated column names
colnames(df)

# Rename columns
df <- df %>%
  rename(remuneration = `remuneration (k£)`, spending_score = `spending_score (1-100)`)
# Check the updated column names
colnames(df)
# View the head of the data
head(df)

# Summary of the data
summary(df)
dim(df)
--------------------------
#Step 4: Exploratory Data Analysis (EDA)

#AGE
  # Examine a variable (age) through a visualisation.
  qplot(age, data=df)

# Calculate mean age
mean_age <- mean(df$age)

# Histogram of Age with KDE and mean
ggplot(data = df, aes(x = age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "pink") +
  geom_vline(xintercept = mean_age, color = "red", linetype = "dashed", size = 1) +  # Add mean line
  labs(title = "Histogram with KDE of Age", y = "Density", caption = paste("Mean age:", round(mean_age, 2)))  

#GENDER
# Create the barplot with data labels
ggplot(data = df, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +  # Add data labels
  labs(title = "Barplot of Gender", x = "Gender", y = "Count") +  
  scale_fill_manual(values = c("pink","skyblue"))  

# Calculate percentages within each group
df_percent <- df %>%
  group_by(gender) %>%
  summarise(percentage = n() / nrow(df) * 100)

# Barplot with percentages within each group
ggplot(data = df_percent, aes(x = gender, y = percentage, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Barplot of Gender (Percentage)", x = "Gender", y = "Percentage") +
  scale_fill_manual(values = c("pink","skyblue")) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3)

#EDUCATIOM
# Barplot of Education 
ggplot(data = df, aes(x = education, fill = education)) +
  geom_bar() +  # Add bars
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +  # Add data labels
  labs(title = "Barplot of Education", x = "Education", y = "Count") +  # Add axis labels
  scale_fill_discrete()  

#Education vs age
# Scatter plot with regression line
ggplot(data = df, aes(x = education, y = age)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add regression line
  labs(title = "Scatter plot of Age vs Education", x = "Education", y = "Age")

#REMUNERATION

# Calculate mean remuneration
mean_remuneration <- mean(df$remuneration, na.rm = TRUE)

# Histogram of REMUNERATION with KDE and mean line
ggplot(data = df, aes(x = remuneration)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(color = "red") +
  geom_vline(xintercept = mean_remuneration, color = "blue", linetype = "dashed", size = 1) +  # Add mean line
  labs(title = "Histogram with KDE of REMUNERATION", y = "Density") +
  annotate("text", x = mean_remuneration + 10, y = 0.01, label = paste("Mean:", round(mean_remuneration, 2)), color = "blue")  # Add text label for mean

# Boxplot of Remuneration
ggplot(data = df, aes(x = 1, y = remuneration)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Remuneration")

#LOYALTY POINTS


library(ggplot2)

# Calculate mean loyalty points
mean_loyalty_points <- mean(df$loyalty_points, na.rm = TRUE)

# Histogram of Loyalty Points with KDE and mean line
ggplot(data = df, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", aes(y = ..density..)) +  # Histogram with normalized density
  geom_density(color = "red") +  # KDE
  geom_vline(xintercept = mean_loyalty_points, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  labs(title = "Histogram with KDE of Loyalty Points", y = "Density") +  # Add axis labels
  annotate("text", x = mean_loyalty_points + 10, y = 0.01, label = paste("Mean:", round(mean_loyalty_points, 2)), color = "blue")  # Add text label for mean

#LOYALTY POINTS BY REMUNERATION BIN

# Boxplot of Loyalty Points
ggplot(data = df, aes(x = 1, y = loyalty_points)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Loyalty Points")

# Define the number of bins and bin width
num_bins <- 5
bin_width <- (max(df$remuneration) - min(df$remuneration)) / num_bins

# Create the remuneration bins
df <- df %>%
  mutate(remuneration_bin = cut(remuneration, breaks = seq(min(df$remuneration), max(df$remuneration) + bin_width, bin_width), include.lowest = TRUE, labels = FALSE))

# Calculate the average loyalty points for each remuneration bin
avg_loyalty_points <- df %>%
  group_by(remuneration_bin) %>%
  summarise(avg_loyalty_points = mean(loyalty_points, na.rm = TRUE))

#REMUNERATION VS LOYALTY POINTS
# Scatterplot of Remuneration vs Loyalty Points
ggplot(data = df, aes(x = remuneration, y = loyalty_points)) +
  geom_point() +
  labs(title = "Scatterplot of Remuneration vs Loyalty Points")
# REMUNERATION VS SPENDING_SCORE
# Scatterplot of Remuneration vs Spending Score
ggplot(data = df, aes(x = remuneration, y = spending_score)) +
  geom_point() +
  labs(title = "Scatterplot of Remuneration vs Spending Score")

# Boxplot of Spending_Score
ggplot(data = df, aes(x = 1, y = spending_score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Spending Score")


# Calculate mean spending score
mean_spending_score <- mean(df$spending_score, na.rm = TRUE)

# Histogram of Spending Score with KDE and mean line
ggplot(data = df, aes(x = spending_score)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", aes(y = ..density..)) +  # Histogram with normalized density
  geom_density(color = "red") +  # KDE
  geom_vline(xintercept = mean_spending_score, color = "blue", linetype = "dashed", size = 1) +  # Mean line
  labs(title = "Histogram with KDE of Spending Score", y = "Density") +  # Add axis labels
  annotate("text", x = mean_spending_score + 10, y = 0.01, label = paste("Mean:", round(mean_spending_score, 2)), color = "blue")  # Add text label for mean


# TOP TURTLE GAMES CONSUMED PRODUCTS


# View "product" column
df_product <- df %>% 
  select(product)
# View the first few rows of the "product" column
head(df_product)


# Number of unique products
num_unique_products <- df %>% 
  distinct(product) %>% 
  nrow()
num_unique_products

# Count the occurrences of each product
product_counts <- df %>%
  group_by(product) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(15)  # Select the top 15 products

# Create a bar plot
ggplot(product_counts, aes(x = reorder(product, -count), y = count, fill = product)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Products by Count of Observations", x = "Product", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)


# Calculate percentage of total observations
total_observations <- sum(product_counts$count)
product_counts <- product_counts %>%
  mutate(percentage = (count / total_observations))  # Remove one multiplication by 100

# Create a bar plot
ggplot(product_counts, aes(x = reorder(product, -percentage), y = percentage, fill = product)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 15 Products by Percentage of Total Observations",
       x = "Product", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Format y-axis as percentage
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)


## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisa?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriphttp://127.0.0.1:34713/graphics/plot_zoom_png?width=678&height=900tive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.

#Perform a statistical analysis and  descriptive statistics in the context of loyalty points.

# Function to calculate mean, max, min, mode, standard deviation, and IQ
calculate_descriptive <- function(data, columns) {
  # Initialize an empty dataframe to store results
  result_df <- data.frame(
    Column = character(),
    Mean = numeric(),
    Max = numeric(),
    Min = numeric(),
    Mode = character(),
    SD = numeric(),
    IQ = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Custom function to calculate mode
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Loop through each specified column
  for (col in columns) {
    # Calculate statistics
    mean_val <- round(mean(data[[col]], na.rm = TRUE), 1)
    max_val <- round(max(data[[col]], na.rm = TRUE), 1)
    min_val <- round(min(data[[col]], na.rm = TRUE), 1)
    mode_val <- as.character(get_mode(data[[col]]))
    sd_val <- round(sd(data[[col]], na.rm = TRUE), 1)
    
    # Calculate IQ (for example, as a combination of mean, max, and min)
    # You can adjust this formula based on your specific requirements
    iq_val <- round((mean_val + max_val + min_val) / 3, 1)
    
    # Create a row for the result dataframe
    result_row <- data.frame(
      Column = col,
      Mean = mean_val,
      Max = max_val,
      Min = min_val,
      Mode = mode_val,
      SD = sd_val,
      IQ = iq_val,
      stringsAsFactors = FALSE
    )
    
    # Append the row to the result dataframe
    result_df <- rbind(result_df, result_row)
  }
  
  # Return the result dataframe
  return(result_df)
}

# Specify the columns for which you want descriptive statistics
selected_columns <- c("remuneration", "loyalty_points", "spending_score", "age")

# Call the function with your dataframe 'health' and specified columns
descriptive_stats <- calculate_descriptive(df, selected_columns)

# Print the result
print(descriptive_stats)

# Save the result to a CSV file
write.csv(descriptive_stats, file = "descriptive_stats.csv", row.names = FALSE)

##  - Comment on distributions and patterns observed in the data.
# 4. Determine normality of data.

colnames(df)
#LOYALOTY - NORMALITY
# Specify qqnorm function (draw a qqplot).
qqnorm(df$loyalty_points)
# Specify qqline function.
qqline(df$loyalty_points)
## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(df$loyalty_points)
## Skewness and Kurtosis
# Specify the skewness and kurtosis functions.
skewness(df$loyalty_points) 
kurtosis(df$loyalty_points) 



#REMUNERATION - NORMALITY
# Specify qqnorm function (draw a qqplot).
qqnorm(df$remuneration)
# Specify qqline function.
qqline(df$remuneration)
## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(df$remuneration)
## Skewness and Kurtosis
# Specify the skewness and kurtosis functions.
skewness(df$remuneration) 
kurtosis(df$remuneration) 

#SPENDING SCORE - NORMALITY
# Specify qqnorm function (draw a qqplot).
qqnorm(df$spending_score)
# Specify qqline function.
qqline(df$spending_score)
## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(df$spending_score)
## Skewness and Kurtosis
# Specify the skewness and kurtosis functions.
skewness(df$spending_score) 
kurtosis(df$spending_score) 

## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Determine and justify the features to be used in a multiple linear regression model and potential

# recapt the data set.
summary(df)
dim(df)
colnames(df)
# Subset the data frame to exclude the "remuneration_bin" column
df <- subset(df, select = -remuneration_bin)

# Select numeric columns from the data frame excluding "product"
numeric_df <- df[sapply(df, is.numeric) & !(colnames(df) %in% c("product"))]

# Compute correlation matrix
correlation_matrix <- cor(numeric_df)

# Plot the correlation matrix
corPlot(numeric_df, cex = 1)

# Create a MLR for LOYALTY points
#MODEL A (remuneration + spending_score)
# specify the lm function and the variables.
modela = lm(loyalty_points~remuneration+spending_score, data=numeric_df)
# Print the summary statistics.
summary(modela)

#MODEL B (remuneration + spending_score + age)
# specify the lm function and the variables.
modelb = lm(loyalty_points~remuneration+spending_score+age, data=numeric_df)
# Print the summary statistics.
summary(modelb)


##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
# Define average values

summary(df)

# Scenario 1: High remuneration(3rd quartile), low spending_score (1st quartile), and average age
scenario1 <- data.frame(remuneration = 63.96, spending_score = 32, age = 39)

# Scenario 2: Low remuneration (1st quartile), high spending_score (3rd quartile), and average age
scenario2 <- data.frame(remuneration = 30.34, spending_score = 73 , age = 39)

# Predict using Model B
loyalty_points_B_scenario1 <- predict(modelb, newdata = scenario1)
loyalty_points_B_scenario2 <- predict(modelb, newdata = scenario2)

print("Predictions using Model B:")
print(paste("Scenario 1 (High remuneration, low spending_score):", loyalty_points_B_scenario1))
print(paste("Scenario 2 (Low remuneration, high spending_score):", loyalty_points_B_scenario2))






###############################################################################
###############################################################################
