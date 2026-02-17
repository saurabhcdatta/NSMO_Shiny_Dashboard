# global.R - Data Preprocessing and Shared Functions
# This file runs once when the app starts

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(scales)
library(xgboost)
library(caret)
library(knitr)
library(kableExtra)

# Load data
cat("Loading NSMO data...\n")
data <- readRDS("nsmo.rds")

# VARIABLE LABELS - Add your labels from PDF
variable_labels <- list(
  "x05a" = "Satisfaction with loan application process",
  "x05b" = "Satisfaction with loan terms", 
  "x05c" = "Satisfaction with interest rate",
  "x05d" = "Satisfaction with monthly payment",
  "x05e" = "Satisfaction with loan closing",
  "x05f" = "Satisfaction with customer service",
  "x05g" = "Overall satisfaction with lender",
  "x06" = "Likelihood to recommend lender",
  "x07" = "Likelihood to use lender again"
  # TODO: Add more labels from nsmo-appendix-c-v60.pdf
)

# Helper function to get variable label
get_variable_label <- function(var_code) {
  label <- variable_labels[[var_code]]
  if (is.null(label)) return(var_code)
  return(label)
}

# Statistical significance test function
test_significance <- function(data, var, group_var, weight_var = "analysis_weight") {
  test_data <- data %>% filter(!is.na(get(var)), !is.na(get(group_var)))
  if (nrow(test_data) < 10) return(list(p_value = NA, significant = FALSE))
  
  groups <- unique(test_data[[group_var]])
  if (length(groups) != 2) return(list(p_value = NA, significant = FALSE))
  
  var_binary <- paste0(var, "_binary")
  group1_data <- test_data %>% filter(get(group_var) == groups[1])
  group2_data <- test_data %>% filter(get(group_var) == groups[2])
  
  g1_pos <- sum(group1_data$analysis_weight * (group1_data[[var_binary]] == "Positive (Very/Somewhat)"), na.rm = TRUE)
  g1_neg <- sum(group1_data$analysis_weight * (group1_data[[var_binary]] == "Negative (Not at all)"), na.rm = TRUE)
  g2_pos <- sum(group2_data$analysis_weight * (group2_data[[var_binary]] == "Positive (Very/Somewhat)"), na.rm = TRUE)
  g2_neg <- sum(group2_data$analysis_weight * (group2_data[[var_binary]] == "Negative (Not at all)"), na.rm = TRUE)
  
  cont_table <- matrix(c(round(g1_pos), round(g1_neg), round(g2_pos), round(g2_neg)), nrow = 2, byrow = TRUE)
  
  tryCatch({
    test_result <- chisq.test(cont_table, correct = FALSE)
    return(list(p_value = test_result$p.value, significant = test_result$p.value < 0.05))
  }, error = function(e) {
    return(list(p_value = NA, significant = FALSE))
  })
}

# Data preprocessing function
cat("Preprocessing data...\n")

preprocess_data <- function(df) {
  
  # Identify response variables
  response_vars <- names(df)[grepl("^x[0-9]", names(df)) & 
                              sapply(df, function(x) {
                                is.numeric(x) && length(unique(na.omit(x))) <= 10 &&
                                  all(na.omit(x) %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
                              })]
  
  # Create binary responses (1-2 = Positive, 3 = Negative)
  df_processed <- df %>%
    mutate(across(all_of(response_vars), 
                  ~case_when(
                    . %in% c(1, 2) ~ "Positive (Very/Somewhat)",
                    . == 3 ~ "Negative (Not at all)",
                    TRUE ~ NA_character_
                  ),
                  .names = "{.col}_binary"))
  
  # DEMOGRAPHICS
  df_processed <- df_processed %>%
    mutate(
      # Age
      Age = coalesce(age_o1, age_o2),
      Age_Category = case_when(
        x74r == 1 ~ "18-24",
        x74r == 2 ~ "25-34", 
        x74r == 3 ~ "35-44",
        x74r == 4 ~ "45-54",
        x74r == 5 ~ "55-64",
        x74r == 6 ~ "65-74",
        x74r == 7 ~ "75+",
        TRUE ~ "Unknown"
      ),
      
      # Sex
      Sex = coalesce(sex_o1, sex_o2),
      Sex_Label = case_when(
        Sex == 1 | x75r == 1 ~ "Male",
        Sex == 2 | x75r == 2 ~ "Female",
        TRUE ~ "Unknown"
      ),
      
      # Education (household max)
      Education_Max = pmax(x76r, x76s, na.rm = TRUE),
      Education_Label = case_when(
        Education_Max == 1 ~ "Some High School",
        Education_Max == 2 ~ "High School Graduate",
        Education_Max == 3 ~ "Tech School",
        Education_Max == 4 ~ "Some College",
        Education_Max == 5 ~ "College Graduate",
        Education_Max == 6 ~ "Post-Graduate",
        TRUE ~ "Unknown"
      ),
      
      # Hispanic
      Hispanic = case_when(
        x77r == 1 ~ "Yes",
        x77r == 2 ~ "No",
        TRUE ~ "Unknown"
      ),
      
      # Race
      Race = case_when(
        x78r == 1 ~ "White",
        x78r == 2 ~ "Black",
        x78r == 3 ~ "Asian",
        x78r == 4 ~ "Other",
        TRUE ~ "Unknown"
      ),
      
      # Work Status
      Work_Status = case_when(
        x79ra %in% c(1, 2) | (!is.na(x79rb) & is.na(x79ra) & x79rb %in% c(1, 2)) ~ "Self-Employed",
        x79ra %in% c(3, 4) | (!is.na(x79rb) & is.na(x79ra) & x79rb %in% c(3, 4)) ~ "Employed",
        x79ra == 5 | (!is.na(x79rb) & is.na(x79ra) & x79rb == 5) ~ "Retired",
        x79ra == 6 | (!is.na(x79rb) & is.na(x79ra) & x79rb == 6) ~ "Unemployed",
        x79ra == 7 | (!is.na(x79rb) & is.na(x79ra) & x79rb == 7) ~ "Not Working",
        TRUE ~ "Unknown"
      ),
      
      # Veteran
      Veteran = case_when(
        x80r %in% c(3, 4) ~ "Yes",
        !is.na(x80r) ~ "No",
        TRUE ~ "Unknown"
      )
    )
  
  # LOAN CHARACTERISTICS
  df_processed <- df_processed %>%
    mutate(
      # First Mortgage
      First_Mortgage = coalesce(first_mort_r, first_mort_s),
      First_Mortgage_Label = case_when(
        First_Mortgage == 1 ~ "First Mortgage",
        First_Mortgage == 2 ~ "Not First",
        TRUE ~ "Unknown"
      ),
      
      # Credit Scores
      Score_Origin = coalesce(score_orig_r, score_orig_s),
      FICO = Score_Origin,
      
      # FICO categories
      FICO_category = case_when(
        Score_Origin >= 800 ~ "Exceptional (800+)",
        Score_Origin >= 740 ~ "Very Good (740-799)",
        Score_Origin >= 670 ~ "Good (670-739)",
        Score_Origin >= 580 ~ "Fair (580-669)",
        Score_Origin < 580 ~ "Poor (<580)",
        TRUE ~ "Unknown"
      ),
      
      # Loan financials
      Amount_Borrowed = z41,
      Monthly_Payment = z42,
      Interest_Rate = z43,
      
      Amount_Category = case_when(
        loan_amount_cat %in% 1:8 ~ "Under $400K",
        loan_amount_cat == 9 ~ "Over $400K",
        TRUE ~ "Unknown"
      ),
      
      # Term
      Term_Label = case_when(
        term == 30 ~ "30 Years",
        !is.na(term) ~ "Other",
        TRUE ~ "Unknown"
      ),
      
      # LTV/CLTV/DTI buckets (industry standard)
      LTV_Category = case_when(
        ltv <= 80 ~ "≤80%",
        ltv <= 90 ~ "80-90%",
        ltv <= 95 ~ "90-95%",
        ltv > 95 ~ ">95%",
        TRUE ~ "Unknown"
      ),
      
      CLTV_Category = case_when(
        cltv <= 80 ~ "≤80%",
        cltv <= 90 ~ "80-90%",
        cltv <= 95 ~ "90-95%",
        cltv > 95 ~ ">95%",
        TRUE ~ "Unknown"
      ),
      
      DTI_Category = case_when(
        dti <= 36 ~ "≤36%",
        dti <= 43 ~ "36-43%",
        dti <= 50 ~ "43-50%",
        dti > 50 ~ ">50%",
        TRUE ~ "Unknown"
      ),
      
      # Loan type
      Loan_Type = case_when(
        loan_type == 1 ~ "Conventional",
        loan_type %in% c(2, 3, 4) ~ "FHA/VA/FSA",
        TRUE ~ "Unknown"
      ),
      
      # GSE
      GSE_Label = case_when(
        gse %in% c(1, 2, 3) ~ "GSE",
        !is.na(gse) ~ "Other",
        TRUE ~ "Unknown"
      ),
      
      # Metro
      Metro = case_when(
        metro_lmi %in% c(1, 2) ~ "MSA",
        !is.na(metro_lmi) ~ "Non-MSA",
        TRUE ~ "Unknown"
      ),
      
      # Jumbo
      Jumbo_Label = case_when(
        jumbo == 1 ~ "Jumbo",
        jumbo == 2 ~ "Non-Jumbo",
        TRUE ~ "Unknown"
      ),
      
      # Cash-out
      Cashout_Label = case_when(
        cashout == 1 ~ "Refinance",
        cashout == 2 ~ "Purchase",
        TRUE ~ "Unknown"
      ),
      
      Rate_Spread_Pct = rate_spread,
      PMMS_Pct = pmms
    )
  
  # FORBEARANCE
  forb_cols <- names(df_processed)[grepl("^forb", names(df_processed), ignore.case = TRUE)]
  if (length(forb_cols) > 0) {
    df_processed <- df_processed %>%
      rowwise() %>%
      mutate(Forbearance = ifelse(any(c_across(all_of(forb_cols)) == 1, na.rm = TRUE), "Yes", "No")) %>%
      ungroup()
  } else {
    df_processed$Forbearance <- "No"
  }
  
  # PERFORMANCE
  perf_cols <- names(df_processed)[grepl("^perf_status", names(df_processed), ignore.case = TRUE)]
  if (length(perf_cols) > 0) {
    df_processed <- df_processed %>%
      rowwise() %>%
      mutate(
        Last_Perf_Status = {
          perf_vals <- c_across(all_of(perf_cols))
          perf_vals <- perf_vals[!is.na(perf_vals)]
          if (length(perf_vals) > 0) perf_vals[length(perf_vals)] else NA_real_
        },
        Performance = case_when(
          Last_Perf_Status == 2 ~ "60 Days Delinquent",
          Last_Perf_Status == 6 ~ "180+ Days Delinquent",
          Last_Perf_Status %in% c(7, 8, 9) ~ "Foreclosure/Bankruptcy",
          !is.na(Last_Perf_Status) ~ "Current/Other",
          TRUE ~ "Unknown"
        )
      ) %>%
      ungroup()
  } else {
    df_processed$Performance <- "Unknown"
  }
  
  # SCORE LAST
  score_r_cols <- names(df_processed)[grepl("^score_[0-9]{4}_r$", names(df_processed), ignore.case = TRUE)]
  score_s_cols <- names(df_processed)[grepl("^score_[0-9]{4}_s$", names(df_processed), ignore.case = TRUE)]
  
  if (length(score_r_cols) > 0 || length(score_s_cols) > 0) {
    df_processed <- df_processed %>%
      rowwise() %>%
      mutate(
        Score_Last = {
          scores_r <- if(length(score_r_cols) > 0) c_across(all_of(score_r_cols)) else numeric(0)
          scores_r <- scores_r[!is.na(scores_r)]
          scores_s <- if(length(score_s_cols) > 0) c_across(all_of(score_s_cols)) else numeric(0)
          scores_s <- scores_s[!is.na(scores_s)]
          
          if (length(scores_r) > 0) {
            scores_r[length(scores_r)]
          } else if (length(scores_s) > 0) {
            scores_s[length(scores_s)]
          } else {
            NA_real_
          }
        }
      ) %>%
      ungroup()
  } else {
    df_processed$Score_Last <- NA_real_
  }
  
  # COVID PERIOD
  if ("open_year" %in% names(df) && "open_month" %in% names(df)) {
    df_processed <- df_processed %>%
      mutate(
        loan_date = as.Date(paste(open_year, open_month, "01", sep = "-"), format = "%Y-%m-%d"),
        COVID_period = case_when(
          loan_date < as.Date("2020-03-01") ~ "Pre-COVID (Before Mar 2020)",
          loan_date >= as.Date("2020-03-01") & loan_date < as.Date("2021-07-01") ~ "COVID (Mar 2020 - Jun 2021)",
          loan_date >= as.Date("2021-07-01") ~ "Post-COVID (Jul 2021+)",
          TRUE ~ "Unknown"
        )
      )
  } else {
    df_processed$COVID_period <- "Period Not Available"
  }
  
  # INSTITUTION TYPE
  if ("cu" %in% names(df)) {
    df_processed <- df_processed %>%
      mutate(Institution_Type = ifelse(cu == 1, "Credit Union", "Non-Credit Union"))
  } else {
    df_processed$Institution_Type <- "Type Not Available"
  }
  
  return(list(data = df_processed, response_vars = response_vars))
}

# Process data
processed <- preprocess_data(data)
df <- processed$data
response_vars <- processed$response_vars

# Create response variable choices with labels
response_choices <- setNames(response_vars, 
                             sapply(response_vars, function(v) {
                               label <- get_variable_label(v)
                               if (nchar(label) > 60) paste0(substr(label, 1, 57), "...") else label
                             }))

# Get available years
available_years <- sort(unique(df$open_year[!is.na(df$open_year)]))

# Standard orderings
fico_order <- c("Poor (<580)", "Fair (580-669)", "Good (670-739)", 
                "Very Good (740-799)", "Exceptional (800+)")

period_order <- c("Pre-COVID (Before Mar 2020)", 
                 "COVID (Mar 2020 - Jun 2021)", 
                 "Post-COVID (Jul 2021+)")

# Color scheme
colors_cu <- "#3498db"
colors_noncu <- "#e74c3c"
colors_positive <- "#27ae60"
colors_negative <- "#c0392b"

cat("Data preprocessing complete!\n")
cat(sprintf("Total observations: %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("Response variables: %d\n", length(response_vars)))
cat(sprintf("Available years: %d to %d\n", min(available_years, na.rm = TRUE), max(available_years, na.rm = TRUE)))
