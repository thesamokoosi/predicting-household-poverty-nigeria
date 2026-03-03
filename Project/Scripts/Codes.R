# ==========================================
# 1. LOAD LIBRARIES & DATA
# ==========================================
library(tidyverse)    # Data manipulation
library(haven)        # Loading Stata (.dta) files
library(caret)        # Machine Learning framework
library(nnet)         # Multinomial Logistic Regression
library(randomForest) # Random Forest
library(e1071)        # SVM
library(ggplot2)
library(ggcorrplot) # For the correlation heatmap
library(patchwork)  # To combine plots
library(reshape2)   # To restructure data for heatmaps

#library(vcd)          # Statistical associations (Cramer's V)


# >>> PUT THE MANUAL FUNCTION RIGHT HERE <<<
calculate_cramerV <- function(var1, var2) {
  tab <- table(var1, var2)
  chi2 <- chisq.test(tab, correct = FALSE)$statistic
  n <- sum(tab)
  r <- nrow(tab)
  c <- ncol(tab)
  v <- sqrt(chi2 / (n * min(c - 1, r - 1)))
  return(as.numeric(v))
}
# >>> END OF MANUAL FUNCTION <<<

# Load the dataset (Ensure the file is in your working directory)
setwd("C:/Users/user/Desktop/bolaji")
raw_data <- read_dta("NGHR8BFL.dta")

# ==========================================
# 2. DATA CLEANING & RECODING
# ==========================================
# Selecting your specified variables
df <- raw_data %>%
  select(
    Poverty_Level = hv270,        # Wealth Index (Target)
    Region = hv024,               # Geographic
    Residence = hv025,            # Geographic (Urban/Rural)
    HH_Size = hv009,              # Socioeconomic
    Electricity = hv206,          # Socioeconomic
    Age_Head = hv220,             # Socioeconomic
    Sex_Head = hv219,             # Socioeconomic
    Education_Head = hv106_01,  # Socioeconomic (Person 1)
    Floor_Type = hv213,           # Socioeconomic Proxy
    Land_Owner = hv244            # Socioeconomic Proxy (Occupation Proxy)
  ) %>%
  # Handle NDHS Missing Codes (9, 98, 99)
  mutate(across(everything(), ~na_if(., 9)),
         across(everything(), ~na_if(., 98)),
         across(everything(), ~na_if(., 99))) %>%
  drop_na() # Remove missing rows to ensure models run


#view(raw_data)

# Recoding for meaningful analysis
df_clean <- df %>%
  mutate(
    # Simplify Floor: Natural (Earth/Sand) vs Finished (Cement/Tile)
    Floor_Group = ifelse(as_factor(Floor_Type) %in% c("Earth", "Sand", "Dung", "Mud"), 
                         "Natural", "Finished"),
    # Convert categorical variables to Factors
    Poverty_Level = as_factor(Poverty_Level),
    Region = as_factor(Region),
    Residence = as_factor(Residence),
    Sex_Head = as_factor(Sex_Head),
    Electricity = as_factor(Electricity),
    Education_Head = as_factor(Education_Head),
    Land_Owner = as_factor(Land_Owner),
    Floor_Group = as.factor(Floor_Group)
  ) %>%
  select(-Floor_Type) # Drop raw floor variable, keep the grouped one

# ==========================================
# 3. STATISTICAL ANALYSIS (Objective 2)
# ==========================================
# Chi-Square tests to prove relationships exist
cat("Chi-Square: Education vs Poverty\n")
print(chisq.test(table(df_clean$Education_Head, df_clean$Poverty_Level)))

cat("Chi-Square: Land Ownership (Sector) vs Poverty\n")
print(chisq.test(table(df_clean$Land_Owner, df_clean$Poverty_Level)))


# ==========================================
# 3. STATISTICAL ANALYSIS (Objective 2)
# ==========================================
# A. Education vs Poverty
edu_tab <- table(df_clean$Education_Head, df_clean$Poverty_Level)
print(chisq.test(edu_tab))

# >>> USE THE NEW FUNCTION HERE <<<
edu_v <- calculate_cramerV(df_clean$Education_Head, df_clean$Poverty_Level)
cat("Cramer's V for Education:", edu_v, "\n")

# B. Land Ownership vs Poverty
land_tab <- table(df_clean$Land_Owner, df_clean$Poverty_Level)
print(chisq.test(land_tab))

# >>> USE THE NEW FUNCTION HERE <<<
land_v <- calculate_cramerV(df_clean$Land_Owner, df_clean$Poverty_Level)
cat("Cramer's V for Land Ownership:", land_v, "\n")


# ==========================================
# 4. VISUALIZATION (Geographic & Socioeconomic)
# ==========================================
# Poverty by Region (Addressing Northern Poverty in your Problem Statement)
ggplot(df_clean, aes(x = Region, fill = Poverty_Level)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Poverty Distribution by Region", y = "Proportion") +
  theme_minimal()

# Poverty by Education
ggplot(df_clean, aes(x = Education_Head, fill = Poverty_Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Poverty Level vs Education of Household Head") +
  theme_light()




#2. The Correlation/Association Heatmap
#Calculates the Cramer's V (association strength) between all variables and plots it as a heatmap. This is a "power move" for a B.Sc project!

# Create a function to calculate association between all variables
# This will show which indicators are most linked to each other

# 1. Updated Cramer's V Function (More robust)
calculate_cramerV <- function(var1, var2) {
  # If variables are identical, correlation is 1
  if (identical(var1, var2)) return(1.0)
  
  # Create the table
  tab <- table(var1, var2)
  
  # Check if table is too small (e.g., 1x1 or empty)
  if (nrow(tab) < 2 | ncol(tab) < 2) return(0.0)
  
  # Run Chi-square with a fail-safe (tryCatch)
  v <- tryCatch({
    chi2 <- chisq.test(tab, correct = FALSE)$statistic
    n <- sum(tab)
    sqrt(chi2 / (n * min(ncol(tab) - 1, nrow(tab) - 1)))
  }, error = function(e) { return(0.0) }) # Return 0 if it fails
  
  return(as.numeric(v))
}

# 2. Updated Matrix Function (Only uses categorical columns)
calc_association_matrix <- function(data) {
  # Select only Factor/Categorical columns for the heatmap
  cat_data <- data %>% select(where(is.factor))
  
  var_names <- names(cat_data)
  n <- length(var_names)
  mat <- matrix(NA, n, n, dimnames = list(var_names, var_names))
  
  for (i in 1:n) {
    for (j in 1:n) {
      mat[i, j] <- calculate_cramerV(cat_data[[i]], cat_data[[j]])
    }
  }
  return(mat)
}

# 3. Now run the heatmap generation again
assoc_matrix <- calc_association_matrix(df_clean)

# 4. Plot the heatmap (ensure ggcorrplot is loaded)
library(ggcorrplot)
ggcorrplot(assoc_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           title = "Association Heatmap of Poverty Indicators",
           colors = c("#6D9EC1", "white", "#E46726"))

#3. Geographic Visual: Poverty by Geopolitical Zone
#Your PDF mentions the "Northern parts of the country" as a problem area. This chart proves that point. code

ggplot(df_clean, aes(x = Region, fill = Poverty_Level)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  labs(title = "Poverty Depth by Geopolitical Zone",
       subtitle = "Proportion of wealth quintiles across Nigeria",
       x = "Region", y = "Proportion (0 to 1)") +
  coord_flip() # Better for long region names

#Socioeconomic Visual: Household Size vs. Poverty
# Objective 2 mentions household characteristics. Larger households are often poorer. This Boxplot shows that relationship perfectly. code

ggplot(df_clean, aes(x = Poverty_Level, y = HH_Size, fill = Poverty_Level)) +
  geom_boxplot(alpha = 0.7) +
  theme_classic() +
  labs(title = "Household Size Distribution by Wealth Level",
       x = "Wealth Quintile", y = "Number of Persons in Household") +
  scale_fill_viridis_d() # Professional color scale
  
#5. Infrastructure Visual: Floor Type & Electricity
#This shows how "Proxies" (infrastructure) relate to actual poverty levels.

p1 <- ggplot(df_clean, aes(x = Floor_Group, fill = Poverty_Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Floor Material Impact") +
  theme_minimal()

p2 <- ggplot(df_clean, aes(x = Electricity, fill = Poverty_Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Electricity Access Impact") +
  theme_minimal()

# Combine them into one image using patchwork
p1 + p2 + plot_layout(guides = "collect")


# ==========================================
# 5. DATA MODELLING (Objective 3)
# ==========================================
set.seed(123) # For reproducibility

# Split Data (80% Train, 20% Test)
trainIndex <- createDataPartition(df_clean$Poverty_Level, p = .8, list = FALSE)
train_data <- df_clean[trainIndex, ]
test_data  <- df_clean[-trainIndex, ]

#Make Shift
# 1. Check which columns have only 1 level (just to see who the culprit is)
sapply(train_data, function(x) length(unique(x)))

# 2. Automatically remove columns with only 1 unique value from the training and test sets
train_data <- train_data %>% select(where(~n_distinct(.) > 1))
test_data <- test_data %>% select(all_of(names(train_data)))

# 3. Ensure the Target Variable (Poverty_Level) is definitely a factor
train_data$Poverty_Level <- as.factor(train_data$Poverty_Level)

# 4. Re-run the Multinomial Logistic Regression
# Note: I'm adding MaxNWts because NDHS data can sometimes be large
model_mlr <- multinom(Poverty_Level ~ ., data = train_data, trace = FALSE, MaxNWts = 2000)

# 5. Confirm it worked
summary(model_mlr)


# A. Multinomial Logistic Regression
model_mlr <- multinom(Poverty_Level ~ ., data = train_data, trace = FALSE)

# B. Random Forest
model_rf <- randomForest(Poverty_Level ~ ., data = train_data, ntree = 500)
summary(model_rf)

# C. Support Vector Machine (SVM)
model_svm <- svm(Poverty_Level ~ ., data = train_data, kernel = "radial")
summary(model_svm)

# ==========================================
# 6. EVALUATION & COMPARISON (Objective 4)
# ==========================================
# Generate Predictions
pred_mlr <- predict(model_mlr, test_data)
pred_rf  <- predict(model_rf, test_data)
pred_svm <- predict(model_svm, test_data)

# Calculate Accuracy for comparison
eval_mlr <- confusionMatrix(pred_mlr, test_data$Poverty_Level)
eval_rf  <- confusionMatrix(pred_rf, test_data$Poverty_Level)
eval_svm <- confusionMatrix(pred_svm, test_data$Poverty_Level)

# Create a Comparison Table for your Chapter 4
model_comparison <- data.frame(
  Model = c("Multinomial Logit", "Random Forest", "SVM"),
  Accuracy = c(eval_mlr$overall['Accuracy'], eval_rf$overall['Accuracy'], eval_svm$overall['Accuracy']),
  Kappa = c(eval_mlr$overall['Kappa'], eval_rf$overall['Kappa'], eval_svm$overall['Kappa'])
)
print(model_comparison)

# ==========================================
# 7. FEATURE IMPORTANCE (Objective 1)
# ==========================================
# This identifies the conditions most highly associated with poverty
varImpPlot(model_rf, main = "Variable Importance in Predicting Poverty")





# ==========================================
# 3-CLASS MODEL OPTIMIZATION (STABLE VERSION)
# ==========================================

# 1. Recode the Target Variable
df_3class <- df_clean %>%
  mutate(Poverty_3Class = case_when(
    Poverty_Level %in% c("poorest", "poorer") ~ "Poor",
    Poverty_Level == "middle" ~ "Middle",
    Poverty_Level %in% c("richer", "richest") ~ "Rich"
  )) %>%
  mutate(Poverty_3Class = factor(Poverty_3Class, levels = c("Poor", "Middle", "Rich"))) %>%
  select(-Poverty_Level) %>%
  droplevels() # <--- CRITICAL: Removes empty factor levels

# 2. Split Data (80/20)
set.seed(123)
trainIndex3 <- createDataPartition(df_3class$Poverty_3Class, p = .8, list = FALSE)
train3 <- df_3class[trainIndex3, ]
test3  <- df_3class[-trainIndex3, ]

# 3. SAFETY FILTER: Remove predictors with only 1 level in the training set
# This stops the "contrasts" error for good.
train3 <- train3 %>% select(where(~ n_distinct(.) > 1))
test3  <- test3 %>% select(all_of(names(train3))) # Sync the test set

# 4. Run the 3-Class Multinomial Regression
# Added MaxNWts to handle larger data complexity
model_mlr_3class <- multinom(Poverty_3Class ~ ., data = train3, trace = FALSE, MaxNWts = 3000)

# 5. Evaluate results
pred_3class <- predict(model_mlr_3class, test3)
eval_3class <- confusionMatrix(pred_3class, test3$Poverty_3Class)

# Check your improved Accuracy
print(eval_3class$overall['Accuracy'])
print(eval_3class$table)


# ==========================================
# 3-CLASS RANDOM FOREST & SVM
# ==========================================

# 1. Random Forest (3-Class)
# Reduced ntree to 100 for speed; usually sufficient for 3 classes
model_rf_3class <- randomForest(Poverty_3Class ~ ., data = train3, ntree = 100, importance = TRUE)

# 2. Support Vector Machine (3-Class)
model_svm_3class <- svm(Poverty_3Class ~ ., data = train3, kernel = "radial", probability = TRUE)

# 3. Generate Predictions
pred_rf_3class  <- predict(model_rf_3class, test3)
pred_svm_3class <- predict(model_svm_3class, test3)

# 4. Evaluation Metrics
eval_rf3  <- confusionMatrix(pred_rf_3class, test3$Poverty_3Class)
eval_svm3 <- confusionMatrix(pred_svm_3class, test3$Poverty_3Class)

# 5. Build the Final Optimized Comparison Table
optimization_results <- data.frame(
  Model = c("Multinomial Logit (3-Class)", "Random Forest (3-Class)", "SVM (3-Class)"),
  Accuracy = c(eval_3class$overall['Accuracy'], eval_rf3$overall['Accuracy'], eval_svm3$overall['Accuracy']),
  Kappa = c(eval_3class$overall['Kappa'], eval_rf3$overall['Kappa'], eval_svm3$overall['Kappa'])
)

print(optimization_results)




# 1. Plotting the 3-Class Confusion Matrix (For Random Forest)
# This shows exactly where the model is "confused"
cm_plot_data <- as.data.frame(eval_rf3$table)

plot1 <- ggplot(cm_plot_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "#6D9EC1", high = "#E46726") +
  theme_minimal() +
  labs(title = "Confusion Matrix: 3-Class Random Forest",
       subtitle = paste("Overall Accuracy:", round(eval_rf3$overall['Accuracy'], 3)))

# 2. Model Comparison Plot (FIXED THE TYPO HERE)
# 'vjust' is the correct term for vertical adjustment
plot2 <- ggplot(optimization_results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) + # Changed vpos to vjust
  theme_classic() + 
  ylim(0, 1) +
  labs(title = "Final Model Accuracy Comparison (3-Class)", 
       y = "Accuracy Score", x = "") +
  theme(legend.position = "none")

# 3. Variable Importance Plot
# Note: This is a base R plot, it doesn't use ggplot
varImpPlot(model_rf_3class, main = "Top Poverty Predictors (3-Class Model)")

# To see the ggplot ones, just type the object names:
print(plot1)
print(plot2)


# 1. Generate the 3-class Association Matrix
assoc_matrix_3class <- calc_association_matrix(df_3class)

# 2. Visualize the Heatmap
library(ggcorrplot)
ggcorrplot(assoc_matrix_3class, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           title = "Association Heatmap: 3-Class Poverty Indicators",
           colors = c("#6D9EC1", "white", "#E46726"))

# A. Region vs 3-Class Poverty (Geographic Plot)
ggplot(df_3class, aes(x = Region, fill = Poverty_3Class)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Poor" = "#e74c3c", "Middle" = "#f1c40f", "Rich" = "#2ecc71")) +
  theme_minimal() +
  labs(title = "Poverty Tier Distribution by Geopolitical Zone", 
       y = "Proportion", fill = "Economic Tier") +
  coord_flip()

# B. Education vs 3-Class Poverty (Socioeconomic Plot)
ggplot(df_3class, aes(x = Education_Head, fill = Poverty_3Class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Poor" = "#e74c3c", "Middle" = "#f1c40f", "Rich" = "#2ecc71")) +
  theme_light() +
  labs(title = "Education of Head vs. 3-Class Poverty Tier", 
       x = "Educational Attainment", y = "Household Count")

# C. Household Size Boxplot (Numeric Plot)
ggplot(df_3class, aes(x = Poverty_3Class, y = HH_Size, fill = Poverty_3Class)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Poor" = "#e74c3c", "Middle" = "#f1c40f", "Rich" = "#2ecc71")) +
  theme_classic() +
  labs(title = "Household Size across 3 Poverty Tiers", 
       x = "Economic Tier", y = "Number of Members")

library(patchwork)

p1_3c <- ggplot(df_3class, aes(x = Floor_Group, fill = Poverty_3Class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Poor" = "#e74c3c", "Middle" = "#f1c40f", "Rich" = "#2ecc71")) +
  labs(title = "Floor Impact (3-Class)") +
  theme_minimal()

p2_3c <- ggplot(df_3class, aes(x = Electricity, fill = Poverty_3Class)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Poor" = "#e74c3c", "Middle" = "#f1c40f", "Rich" = "#2ecc71")) +
  labs(title = "Electricity Impact (3-Class)") +
  theme_minimal()

p1_3c + p2_3c + plot_layout(guides = "collect")



# A. 3-Class Confusion Matrix Heatmap
cm_3c_data <- as.data.frame(eval_rf3$table)
ggplot(cm_3c_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "#34495e", high = "#e67e22") +
  theme_minimal() +
  labs(title = "Confusion Matrix: Optimized 3-Class RF Model")

# B. 3-Class Variable Importance
varImpPlot(model_rf_3class, main = "Predictive Drivers of 3-Class Poverty")





library(tidyverse)
library(patchwork)

# 1. Target Variable Distribution (Poor vs Middle vs Rich)
p_target <- ggplot(df_3class, aes(x = Poverty_3Class, fill = Poverty_3Class)) +
  geom_bar() +
  scale_fill_manual(values = c("Poor" = "#e74c3c", "Middle" = "#f1c40f", "Rich" = "#2ecc71")) +
  theme_minimal() +
  labs(title = "A: Distribution of Poverty Tiers", x = "", y = "Household Count") +
  theme(legend.position = "none")

# 2. Key Binary Indicators (Residence, Sex, Electricity)
# We restructure the data to show them together
demo_summary <- df_3class %>%
  select(Residence, Sex_Head, Electricity) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

p_demo <- ggplot(demo_summary, aes(x = Variable, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  theme_classic() +
  labs(title = "B: Key Household Characteristics", x = "", y = "Percentage (%)") +
  scale_fill_brewer(palette = "Paired")

# Combine them into one professional figure
p_target / p_demo