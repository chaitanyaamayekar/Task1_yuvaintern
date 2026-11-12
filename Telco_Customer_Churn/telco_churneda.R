setwd("C:/Users/chaitanya/OneDrive/Desktop/Telco_Customer_Churn")
getwd()   # should print that same path

dir.create("plots",  showWarnings = FALSE)   # charts are saved here
dir.create("output", showWarnings = FALSE)   # cleaned CSV files are saved here

# install.packages(c("tidyverse", "corrplot", "knitr"))   # run once if needed
library(tidyverse)
library(corrplot)
library(knitr)

theme_set(theme_minimal(base_size = 12))

file_path <- "WA_Fn-UseC_-Telco-Customer-Churn.csv"

telco <- read.csv(file_path)
df <- telco

# ---- 1. Inspect the data -------------------------------------
dim(df)
head(df)
str(df)
summary(df)

# ---- 2. Missing values ---------------------------------------
colSums(is.na(df))                       # TotalCharges has 11 NA

# Confirm what the missing cells look like in the raw file
df_text <- read.csv(file_path, colClasses = "character")
sum(trimws(df_text$TotalCharges) == "")  # 11 blank cells

# These rows all have tenure = 0 (new customers, not billed yet)
df %>%
  filter(is.na(TotalCharges)) %>%
  select(customerID, tenure, MonthlyCharges, TotalCharges)

# Impute 0 where tenure = 0 (their true total is 0, so mean/median would be wrong)
df <- df %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), ""))) %>%
  mutate(TotalCharges = as.numeric(TotalCharges),
         TotalCharges = ifelse(is.na(TotalCharges) & tenure == 0, 0, TotalCharges))

cat("Missing values left:", sum(is.na(df)), "\n")

# ---- 3. Duplicates, labels and data types --------------------
cat("Duplicate rows:", sum(duplicated(df)), "\n")

service_cols <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection",
                  "TechSupport", "StreamingTV", "StreamingMovies")

df <- df %>%
  mutate(across(all_of(service_cols), ~ ifelse(.x == "No internet service", "No", .x)),
         MultipleLines = ifelse(MultipleLines == "No phone service", "No", MultipleLines),
         SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No")) %>%
  mutate(across(where(is.character) & !any_of("customerID"), factor),
         Contract = factor(Contract, levels = c("Month-to-month", "One year", "Two year")))

str(df)

# ---- 4. Outlier detection ------------------------------------
num_cols <- c("tenure", "MonthlyCharges", "TotalCharges")

iqr_count <- function(x) {
  q <- quantile(x, c(.25, .75)); i <- IQR(x)
  sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i)
}
outlier_table <- data.frame(
  IQR_outliers = sapply(df[num_cols], iqr_count),
  Z_outliers   = sapply(df[num_cols], function(x) sum(abs(scale(x)) > 3)))
print(outlier_table)                     # all zero

p_box <- df %>%
  select(all_of(num_cols)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = "", y = Value)) +
  geom_boxplot(fill = "steelblue", outlier.colour = "red") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Boxplots of numeric variables", x = NULL)
print(p_box); ggsave("plots/01_boxplots.png", p_box, width = 7, height = 4)

# Safeguard: IQR capping (changes nothing here because there are no outliers)
cap_iqr <- function(x) {
  q <- quantile(x, c(.25, .75)); i <- IQR(x)
  pmin(pmax(x, q[1] - 1.5 * i), q[2] + 1.5 * i)
}
df_clean <- df %>% mutate(across(all_of(num_cols), cap_iqr))
cat("Values changed by capping:",
    sum(df$TotalCharges != df_clean$TotalCharges) +
      sum(df$MonthlyCharges != df_clean$MonthlyCharges) +
      sum(df$tenure != df_clean$tenure), "\n")

# ---- 5. Normalization ----------------------------------------
minmax <- function(x) (x - min(x)) / (max(x) - min(x))

df_norm <- df_clean %>%
  mutate(across(all_of(num_cols), minmax, .names = "{.col}_minmax"),
         across(all_of(num_cols), ~ as.numeric(scale(.x)), .names = "{.col}_z"))

summary(df_norm %>% select(ends_with("_minmax"), ends_with("_z")))

# ---- 6. Encoding categorical variables -----------------------
binary_cols <- names(df_norm)[sapply(df_norm, function(x) is.factor(x) && nlevels(x) == 2)]
binary_cols <- setdiff(binary_cols, "Churn")
multi_cols  <- names(df_norm)[sapply(df_norm, function(x) is.factor(x) && nlevels(x) > 2)]

# Binary columns -> 0/1, Churn -> 1/0
df_enc <- df_norm %>%
  mutate(Churn_num = ifelse(Churn == "Yes", 1, 0),
         across(all_of(binary_cols), ~ as.integer(.x == levels(.x)[2]), .names = "{.col}_enc"))

# Multi-category columns -> one-hot (one column per category)
onehot  <- df_norm[multi_cols]
dummies <- as.data.frame(
  model.matrix(~ . - 1, data = onehot,
               contrasts.arg = lapply(onehot, contrasts, contrasts = FALSE)))
names(dummies) <- make.unique(tolower(gsub("[^A-Za-z0-9]+", "_", names(dummies))))

# Final model-ready dataset
df_model <- bind_cols(
  df_enc %>% select(customerID, ends_with("_enc"), ends_with("_minmax"), Churn_num),
  dummies)

dim(df_model)
head(df_model)

# ---- 7. Exploratory analysis ---------------------------------
# Descriptive statistics
desc <- sapply(df_clean[num_cols], function(x)
  c(Mean = mean(x), Median = median(x), SD = sd(x), Min = min(x), Max = max(x)))
print(round(desc, 2))

# Churn distribution
df_clean %>% count(Churn) %>% mutate(Percent = round(n / sum(n) * 100, 1))

# Medians by churn
df_clean %>% group_by(Churn) %>%
  summarise(across(all_of(num_cols), median), .groups = "drop")

# Numeric variables by churn
p_num <- df_clean %>%
  select(Churn, all_of(num_cols)) %>%
  pivot_longer(-Churn, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Churn, Value, fill = Churn)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Numeric variables by churn status")
print(p_num); ggsave("plots/02_numeric_by_churn.png", p_num, width = 7, height = 4)

# Churn rate by category
churn_plot <- function(var, title, file) {
  p <- df_clean %>%
    group_by(across(all_of(var))) %>%
    summarise(rate = mean(Churn == "Yes") * 100, .groups = "drop") %>%
    ggplot(aes(.data[[var]], rate, fill = .data[[var]])) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(round(rate, 1), "%")), vjust = -0.3) +
    labs(title = title, x = NULL, y = "Churn rate (%)") +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
  print(p); ggsave(file, p, width = 7, height = 4)
}
churn_plot("Contract",        "Churn rate by contract type",    "plots/03_contract.png")
churn_plot("InternetService", "Churn rate by internet service", "plots/04_internet.png")
churn_plot("PaymentMethod",   "Churn rate by payment method",   "plots/05_payment.png")
churn_plot("TechSupport",     "Churn rate by tech support",     "plots/06_techsupport.png")

# Is contract type related to churn? (chi-square test)
chisq.test(table(df_clean$Contract, df_clean$Churn))

# Correlations
cor_data <- df_enc %>% select(all_of(num_cols), ends_with("_enc"), Churn_num)
cor_mat  <- cor(cor_data)

png("plots/07_correlation.png", width = 900, height = 700)
corrplot(cor_mat, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)
dev.off()
corrplot(cor_mat, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

round(sort(cor_mat[, "Churn_num"]), 2)   # correlation of each variable with churn

# ---- 8. Key insights -----------------------------------------
rate <- function(var, lvl) round(mean(df_clean$Churn[df_clean[[var]] == lvl] == "Yes") * 100, 1)

cat("\n=== KEY INSIGHTS ===\n")
cat("1. Overall churn:", round(mean(df_clean$Churn == "Yes") * 100, 1), "%\n")
cat("2. Month-to-month churn:", rate("Contract", "Month-to-month"),
    "% vs two-year:", rate("Contract", "Two year"), "%\n")
cat("3. Median tenure - churned:", median(df_clean$tenure[df_clean$Churn == "Yes"]),
    "months vs retained:", median(df_clean$tenure[df_clean$Churn == "No"]), "months\n")
cat("4. Fiber optic churn:", rate("InternetService", "Fiber optic"),
    "% vs DSL:", rate("InternetService", "DSL"), "%\n")
cat("5. Electronic check churn:", rate("PaymentMethod", "Electronic check"), "%\n")
cat("6. No tech support churn:", rate("TechSupport", "No"),
    "% vs with tech support:", rate("TechSupport", "Yes"), "%\n")
cat("7. Correlation tenure-churn:", round(cor(df_clean$tenure, df_enc$Churn_num), 2),
    "| monthly charges-churn:", round(cor(df_clean$MonthlyCharges, df_enc$Churn_num), 2),
    "| tenure-total charges:", round(cor(df_clean$tenure, df_clean$TotalCharges), 2), "\n")

# ---- 9. Save cleaned data ------------------------------------
write.csv(df_clean, "output/telco_churn_cleaned.csv", row.names = FALSE)
write.csv(df_model, "output/telco_churn_model_ready.csv", row.names = FALSE)
cat("Both datasets saved successfully!\n")

# Confirm saved files
list.files("plots")    # should show 7 PNG charts
list.files("output")   # should show 2 CSV files
getwd()
