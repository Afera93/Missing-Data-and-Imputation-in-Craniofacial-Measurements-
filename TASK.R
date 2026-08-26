# ==========================================================
# TASK 1.1
# Summarise number and percentage of missing values
# for each variable
# ==========================================================

library(dplyr)
library(ggplot2)


# ----------------------------------------------------------
# 1. Import dataset
# ----------------------------------------------------------

dat <- read.csv(
  "C:/Users/A C E R/OneDrive - Universiti Malaya/Desktop/Cranifacial Assestment/Craniofacial Data.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


# ----------------------------------------------------------
# 2. Check dataset dimensions
# ----------------------------------------------------------

dim(dat)

# Expected:
# 24 subjects
# 155 variables

# ----------------------------------------------------------
# 3. Summarise missingness for ALL variables
# ----------------------------------------------------------

missing_summary_all <- data.frame(
  Variable = names(dat),
  Missing_n = colSums(is.na(dat)),
  Missing_percent = round(
    colMeans(is.na(dat)) * 100,
    2
  )
)

# Arrange from highest to lowest missingness
missing_summary_all <- missing_summary_all %>%
  arrange(
    desc(Missing_n),
    Variable
  )

# Display table
print(
  missing_summary_all,
  row.names = FALSE
)

# ----------------------------------------------------------
# 4. Define craniofacial measurement variables
# ----------------------------------------------------------

measurement_vars <- setdiff(
  names(dat),
  c("Patient ID", "gender", "age")
)


measurement_data <- dat[
  ,
  measurement_vars,
  drop = FALSE
]

# ----------------------------------------------------------
# 5. Summarise missingness for craniofacial measurements
# ----------------------------------------------------------

missing_summary_measurements <- data.frame(
  Variable = measurement_vars,
  Missing_n = colSums(
    is.na(measurement_data)
  ),
  Missing_percent = round(
    colMeans(
      is.na(measurement_data)
    ) * 100,
    2
  )
)

missing_summary_measurements <-
  missing_summary_measurements %>%
  arrange(
    desc(Missing_n),
    Variable
  )


print(
  missing_summary_measurements,
  row.names = FALSE
)

# ----------------------------------------------------------
# 6. Keep only measurements with missing values
# ----------------------------------------------------------

missing_only <- missing_summary_measurements %>%
  filter(
    Missing_n > 0
  )


cat("\nMeasurements containing missing values:\n")

print(
  missing_only,
  row.names = FALSE
)

# ----------------------------------------------------------
# 7. Number of complete and incomplete measurements
# ----------------------------------------------------------

n_incomplete <- sum(
  missing_summary_measurements$Missing_n > 0
)

n_complete <- sum(
  missing_summary_measurements$Missing_n == 0
)

cat(
  "\nNumber of craniofacial measurements with missing values:",
  n_incomplete,
  "\n"
)

cat(
  "Number of complete craniofacial measurements:",
  n_complete,
  "\n"
)


# ----------------------------------------------------------
# 8. Overall missingness
# ----------------------------------------------------------

total_missing <- sum(
  is.na(measurement_data)
)


total_measurement_cells <-
  nrow(measurement_data) *
  ncol(measurement_data)


overall_missing_percent <- round(
  total_missing /
    total_measurement_cells *
    100,
  2
)


cat(
  "Total missing measurement values:",
  total_missing,
  "\n"
)


cat(
  "Total measurement cells:",
  total_measurement_cells,
  "\n"
)


cat(
  "Overall missing measurement percentage:",
  overall_missing_percent,
  "%\n"
)


# ----------------------------------------------------------
# 9. Summarise levels of variable-level missingness
# ----------------------------------------------------------

missing_level_summary <-
  missing_summary_measurements %>%
  count(
    Missing_n,
    Missing_percent,
    name = "Number_of_variables"
  ) %>%
  arrange(
    desc(Missing_n)
  )


cat("\nSummary of missingness levels:\n")

print(
  missing_level_summary,
  row.names = FALSE
)


# ----------------------------------------------------------
# 10. Plot measurements with missing values
# ----------------------------------------------------------

plot_data <- missing_only


plot_data$Variable <- reorder(
  plot_data$Variable,
  plot_data$Missing_percent
)


ggplot(
  plot_data,
  aes(
    x = Missing_percent,
    y = Variable
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        Missing_n,
        " (",
        Missing_percent,
        "%)"
      )
    ),
    hjust = -0.1,
    size = 3
  ) +
  scale_x_continuous(
    limits = c(
      0,
      max(plot_data$Missing_percent) + 10
    )
  ) +
  labs(
    title = "Missing Values by Craniofacial Measurement",
    x = "Missing values (%)",
    y = "Measurement"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.y = element_text(
      size = 7
    ),
    panel.grid.minor = element_blank()
  )

# ==========================================================
# TASK 1.2
# Summarise missingness for each subject
# ==========================================================


# ----------------------------------------------------------
# 1. Calculate missingness for each subject
# ----------------------------------------------------------

subject_missing <- data.frame(
  Patient_ID = dat[["Patient ID"]],
  
  Missing_n = rowSums(
    is.na(
      dat[measurement_vars]
    )
  ),
  
  Missing_percent = round(
    rowMeans(
      is.na(
        dat[measurement_vars]
      )
    ) * 100,
    2
  )
)


# ----------------------------------------------------------
# 2. Display missingness for all subjects
# ----------------------------------------------------------

print(
  subject_missing,
  row.names = FALSE
)


# ----------------------------------------------------------
# 3. Identify subjects with missing measurements
# Arrange from highest to lowest
# ----------------------------------------------------------

subjects_with_missing <- subject_missing %>%
  filter(
    Missing_n > 0
  ) %>%
  arrange(
    desc(Missing_percent)
  )


cat("\nSubjects with missing measurements:\n")

print(
  subjects_with_missing,
  row.names = FALSE
)


# ----------------------------------------------------------
# 4. Count complete and incomplete subjects
# ----------------------------------------------------------

n_subjects_missing <- sum(
  subject_missing$Missing_n > 0
)

n_subjects_complete <- sum(
  subject_missing$Missing_n == 0
)


cat(
  "\nNumber of subjects with at least one missing measurement:",
  n_subjects_missing,
  "\n"
)

cat(
  "Number of subjects with complete measurement data:",
  n_subjects_complete,
  "\n"
)


# ----------------------------------------------------------
# 5. Plot missingness by subject
# ----------------------------------------------------------

ggplot(
  subject_missing,
  aes(
    x = factor(Patient_ID),
    y = Missing_percent
  )
) +
  geom_col(
    width = 0.8
  ) +
  geom_text(
    aes(
      label = ifelse(
        Missing_n > 0,
        paste0(
          Missing_n,
          "\n(",
          Missing_percent,
          "%)"
        ),
        ""
      )
    ),
    vjust = -0.2,
    size = 2.8,
    lineheight = 0.9
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(subject_missing$Missing_percent) + 6
    ),
    breaks = seq(
      0,
      25,
      5
    )
  ) +
  labs(
    title = "Missing Values by Subject",
    x = "Patient ID",
    y = "Missing measurements (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.title = element_text(
      size = 12
    ),
    axis.text.x = element_text(
      size = 9
    ),
    axis.text.y = element_text(
      size = 10
    ),
    panel.grid.minor = element_blank()
  )

# ==========================================================
# TASK 1.3
# Visualise the missing-data pattern
# ==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ----------------------------------------------------------
# 1. Identify measurements with at least one missing value
# ----------------------------------------------------------

vars_with_missing <- measurement_vars[
  colSums(
    is.na(dat[measurement_vars])
  ) > 0
]

cat(
  "Number of measurements with missing values:",
  length(vars_with_missing),
  "\n"
)

# Expected: 51


# ----------------------------------------------------------
# 2. Reshape data into long format
# ----------------------------------------------------------

missing_heatmap <- dat %>%
  select(
    `Patient ID`,
    all_of(vars_with_missing)
  ) %>%
  pivot_longer(
    cols = -`Patient ID`,
    names_to = "Measurement",
    values_to = "Value"
  ) %>%
  mutate(
    Status = ifelse(
      is.na(Value),
      "Missing",
      "Observed"
    )
  )


# ----------------------------------------------------------
# 3. Order patients from 1 to 24
# ----------------------------------------------------------

missing_heatmap$`Patient ID` <- factor(
  missing_heatmap$`Patient ID`,
  levels = sort(
    unique(dat$`Patient ID`)
  )
)

# ----------------------------------------------------------
# 4. Order measurements by amount of missingness
# ----------------------------------------------------------

missing_order <- data.frame(
  Variable = vars_with_missing,
  Missing_n = colSums(
    is.na(dat[vars_with_missing])
  )
) %>%
  arrange(
    Missing_n,
    Variable
  )


missing_heatmap$Measurement <- factor(
  missing_heatmap$Measurement,
  levels = missing_order$Variable
)


# ----------------------------------------------------------
# 5. Missing-data heatmap
# ----------------------------------------------------------

ggplot(
  missing_heatmap,
  aes(
    x = `Patient ID`,
    y = Measurement,
    fill = Status
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "Observed" = "grey85",
      "Missing" = "black"
    )
  ) +
  labs(
    title = "Missing-Data Pattern by Subject and Measurement",
    x = "Patient ID",
    y = "Craniofacial Measurement",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.x = element_text(
      size = 9
    ),
    axis.text.y = element_text(
      size = 6
    ),
    axis.title = element_text(
      size = 11
    ),
    panel.grid = element_blank(),
    legend.position = "top"
  )


# ==========================================================
# TASK 1.4
# Co-missingness among potentially related measurements
# ==========================================================

# Function to show which patients are missing for a variable
missing_patients <- function(variable) {
  
  dat$`Patient ID`[
    is.na(dat[[variable]])
  ]
}


# ----------------------------------------------------------
# Shared ANS-labelled measurements
# ----------------------------------------------------------

ANS_variables <- c(
  "ANS-CP",
  "ANS-PNS",
  "ANS-SMP",
  "ANS-SP",
  "N-ANS",
  "N-Ro-ANS",
  "Ro-ANS",
  "S-N-ANS"
)

lapply(
  ANS_variables,
  missing_patients
)


# ----------------------------------------------------------
# Apparent corresponding pairs
# ----------------------------------------------------------

missing_patients("TM-CP")
missing_patients("TMR-CP")

missing_patients("TM-SP")
missing_patients("TMR-SP")

missing_patients("TM-MO")
missing_patients("TMR-MOR")

missing_patients("MO-TM-PNS")
missing_patients("MOR-TMR-PNS")

missing_patients("ZMs-MO-TM")
missing_patients("ZMsR-MOR-TMR")

missing_patients("ZMi-CP")
missing_patients("ZMiR-CP")

missing_patients("ZMi-SP")
missing_patients("ZMiR-SP")


# ==========================================================
# TASK 1.5
# Investigate possible missingness mechanism
# MCAR / MAR / MNAR
# ==========================================================

library(dplyr)
library(stringr)


# ----------------------------------------------------------
# 1. Convert age to years
# ----------------------------------------------------------

convert_age_to_years <- function(x) {
  
  x <- tolower(trimws(x))
  
  years <- suppressWarnings(
    as.numeric(
      str_match(
        x,
        "([0-9.]+)\\s*y"
      )[, 2]
    )
  )
  
  months <- suppressWarnings(
    as.numeric(
      str_match(
        x,
        "([0-9.]+)\\s*m"
      )[, 2]
    )
  )
  
  days <- suppressWarnings(
    as.numeric(
      str_match(
        x,
        "([0-9.]+)\\s*d"
      )[, 2]
    )
  )
  
  years[is.na(years)] <- 0
  months[is.na(months)] <- 0
  days[is.na(days)] <- 0
  
  years + months / 12 + days / 365.25
}


# ----------------------------------------------------------
# 2. Create subject-level missingness information
# ----------------------------------------------------------

missing_analysis <- dat %>%
  mutate(
    
    age_year = convert_age_to_years(age),
    
    Missing_n = rowSums(
      is.na(
        across(
          all_of(measurement_vars)
        )
      )
    ),
    
    Missing_status = ifelse(
      Missing_n > 0,
      "Any missing",
      "No missing"
    )
  )


# ----------------------------------------------------------
# 3. Check data
# ----------------------------------------------------------

missing_analysis %>%
  select(
    `Patient ID`,
    gender,
    age,
    age_year,
    Missing_n,
    Missing_status
  )


# ----------------------------------------------------------
# 4. Age vs number of missing measurements
# Spearman correlation
# ----------------------------------------------------------

age_spearman <- cor.test(
  missing_analysis$age_year,
  missing_analysis$Missing_n,
  method = "spearman",
  exact = FALSE
)

age_spearman

# ----------------------------------------------------------
# 5. Compare age between subjects
# with and without missing measurements
# ----------------------------------------------------------

age_wilcox <- wilcox.test(
  age_year ~ Missing_status,
  data = missing_analysis,
  exact = FALSE
)

age_wilcox


# ----------------------------------------------------------
# 6. Gender vs missingness status
# ----------------------------------------------------------

gender_table <- table(
  missing_analysis$gender,
  missing_analysis$Missing_status
)

gender_table


gender_fisher <- fisher.test(
  gender_table
)

gender_fisher


# ----------------------------------------------------------
# 7. Summary table
# ----------------------------------------------------------

task1_5_results <- data.frame(
  
  Assessment = c(
    "Age vs number of missing measurements",
    "Age: subjects with vs without missing data",
    "Gender vs missingness status"
  ),
  
  Statistical_test = c(
    "Spearman correlation",
    "Wilcoxon rank-sum test",
    "Fisher's exact test"
  ),
  
  Result = c(
    
    paste0(
      "rho = ",
      round(
        unname(age_spearman$estimate),
        3
      ),
      ", p = ",
      round(
        age_spearman$p.value,
        3
      )
    ),
    
    paste0(
      "p = ",
      round(
        age_wilcox$p.value,
        3
      )
    ),
    
    paste0(
      "p = ",
      round(
        gender_fisher$p.value,
        3
      )
    )
  )
)


print(
  task1_5_results,
  row.names = FALSE
)

# ==========================================================
# TASK 2.1
# Preliminary correlation assessment for imputation
# method selection
# ==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ----------------------------------------------------------
# 1. Identify craniofacial measurement variables
# ----------------------------------------------------------

measurement_vars <- setdiff(
  names(dat),
  c("Patient ID", "gender", "age")
)

length(measurement_vars)

# Expected:
# 152


# ----------------------------------------------------------
# 2. Identify measurements containing missing values
# ----------------------------------------------------------

missing_vars <- measurement_vars[
  colSums(
    is.na(dat[measurement_vars])
  ) > 0
]

length(missing_vars)

# Expected:
# 51


# ----------------------------------------------------------
# 3. Calculate pairwise Pearson correlations
#
# pairwise.complete.obs:
# correlation for each pair is calculated using only
# subjects where both measurements are observed
# ----------------------------------------------------------

cor_missing <- cor(
  dat[missing_vars],
  use = "pairwise.complete.obs",
  method = "pearson"
)


# Check dimensions
dim(cor_missing)

# Expected:
# 51 51


# ----------------------------------------------------------
# 4. Summarise correlation strength
# ----------------------------------------------------------

cor_values <- cor_missing[
  upper.tri(cor_missing)
]

# Remove any correlations that could not be calculated
cor_values <- cor_values[
  !is.na(cor_values)
]


total_pairs <- length(
  cor_values
)


strong_pairs <- sum(
  abs(cor_values) >= 0.70
)


strong_percent <- round(
  100 *
    strong_pairs /
    total_pairs,
  1
)


cat(
  "Number of incomplete measurement pairs:",
  total_pairs,
  "\n"
)

cat(
  "Number of pairs with |r| >= 0.70:",
  strong_pairs,
  "\n"
)

cat(
  "Percentage of pairs with |r| >= 0.70:",
  strong_percent,
  "%\n"
)


# ----------------------------------------------------------
# 5. Order variables according to correlation similarity
#
# Absolute correlation is used so that both strong
# positive and strong negative relationships are considered.
# ----------------------------------------------------------

cor_for_clustering <- cor_missing

# Protect against possible NA correlations
cor_for_clustering[
  is.na(cor_for_clustering)
] <- 0


distance_matrix <- as.dist(
  1 - abs(cor_for_clustering)
)


hc <- hclust(
  distance_matrix,
  method = "average"
)


ordered_vars <- colnames(
  cor_for_clustering
)[hc$order]


# ----------------------------------------------------------
# 6. Convert correlation matrix to long format
# ----------------------------------------------------------

cor_long <- as.data.frame(
  cor_missing
)


cor_long$Variable_1 <- rownames(
  cor_long
)


cor_long <- cor_long %>%
  pivot_longer(
    cols = -Variable_1,
    names_to = "Variable_2",
    values_to = "Correlation"
  )


# ----------------------------------------------------------
# 7. Apply variable ordering
# ----------------------------------------------------------

cor_long$Variable_1 <- factor(
  cor_long$Variable_1,
  levels = ordered_vars
)


cor_long$Variable_2 <- factor(
  cor_long$Variable_2,
  levels = ordered_vars
)


# ----------------------------------------------------------
# 8. Keep one triangle only
# and correlations with |r| >= 0.70
#
# row_number > col_number removes the diagonal.
# ----------------------------------------------------------

cor_long <- cor_long %>%
  mutate(
    row_number = as.numeric(
      Variable_1
    ),
    col_number = as.numeric(
      Variable_2
    )
  ) %>%
  filter(
    row_number > col_number,
    !is.na(Correlation),
    abs(Correlation) >= 0.70
  )


# ----------------------------------------------------------
# 9. Create correlation heatmap
# ----------------------------------------------------------

ggplot(
  cor_long,
  aes(
    x = Variable_2,
    y = Variable_1,
    fill = Correlation
  )
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Structure of Incomplete Craniofacial Measurements",
    subtitle = "Only correlations with |r| \u2265 0.70 are displayed",
    x = NULL,
    y = NULL,
    fill = "Pearson r"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 6
    ),
    axis.text.y = element_text(
      size = 6
    ),
    panel.grid = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    )
  )

# ==========================================================
# TASK 2.2
# Median imputation as a simple baseline
# ==========================================================


# ----------------------------------------------------------
# 1. Start from the ORIGINAL incomplete dataset
# ----------------------------------------------------------

dat_median <- dat


# ----------------------------------------------------------
# 2. Impute missing craniofacial measurements
# using the observed median of each variable
# ----------------------------------------------------------

for (v in measurement_vars) {
  
  median_value <- median(
    dat[[v]],
    na.rm = TRUE
  )
  
  dat_median[[v]][
    is.na(dat_median[[v]])
  ] <- median_value
}


# ----------------------------------------------------------
# 3. Check that no measurement values remain missing
# ----------------------------------------------------------

remaining_missing_median <- sum(
  is.na(
    dat_median[measurement_vars]
  )
)


cat(
  "Missing measurement values after median imputation:",
  remaining_missing_median,
  "\n"
)

# Expected:
# 0


# ----------------------------------------------------------
# 4. Check that original observed values were preserved
# ----------------------------------------------------------

original_matrix <- as.matrix(
  dat[measurement_vars]
)

median_matrix <- as.matrix(
  dat_median[measurement_vars]
)


observed_positions <- !is.na(
  original_matrix
)


median_observed_preserved <- all(
  median_matrix[observed_positions] ==
    original_matrix[observed_positions]
)


cat(
  "Original observed values preserved:",
  median_observed_preserved,
  "\n"
)

# Expected:
# TRUE


# ----------------------------------------------------------
# 5. Check that Patient ID, gender and age were unchanged
# ----------------------------------------------------------

cat(
  "Patient ID unchanged:",
  identical(
    dat$`Patient ID`,
    dat_median$`Patient ID`
  ),
  "\n"
)


cat(
  "Gender unchanged:",
  identical(
    dat$gender,
    dat_median$gender
  ),
  "\n"
)


cat(
  "Age unchanged:",
  identical(
    dat$age,
    dat_median$age
  ),
  "\n"
)

# Expected:
# TRUE
# TRUE
# TRUE


# ----------------------------------------------------------
# 6. Check how many values were imputed
# ----------------------------------------------------------

n_imputed_median <- sum(
  is.na(
    dat[measurement_vars]
  )
)


cat(
  "Number of values imputed:",
  n_imputed_median,
  "\n"
)

# ==========================================================
# TASK 2.3
# k-Nearest-Neighbour (kNN) imputation
# ==========================================================


# ----------------------------------------------------------
# 1. Define kNN imputation function
# ----------------------------------------------------------

knn_impute <- function(
    data,
    measurement_vars,
    k = 3
) {
  
  # Start from original incomplete data
  imputed_data <- data
  
  
  # Measurement data only
  X <- data[
    ,
    measurement_vars,
    drop = FALSE
  ]
  
  
  # --------------------------------------------------------
  # 2. Standardise measurements
  # --------------------------------------------------------
  
  variable_means <- sapply(
    X,
    function(x) {
      mean(x, na.rm = TRUE)
    }
  )
  
  
  variable_sds <- sapply(
    X,
    function(x) {
      sd(x, na.rm = TRUE)
    }
  )
  
  
  # Avoid division by zero
  variable_sds[
    is.na(variable_sds) |
      variable_sds == 0
  ] <- 1
  
  
  X_scaled <- sweep(
    X,
    2,
    variable_means,
    "-"
  )
  
  
  X_scaled <- sweep(
    X_scaled,
    2,
    variable_sds,
    "/"
  )
  
  
  # --------------------------------------------------------
  # 3. Impute one variable at a time
  # --------------------------------------------------------
  
  for (target in measurement_vars) {
    
    
    # Subjects with missing target value
    missing_rows <- which(
      is.na(data[[target]])
    )
    
    
    # Skip if no missing values
    if (length(missing_rows) == 0) {
      next
    }
    
    
    # Donors must have observed target value
    donor_rows <- which(
      !is.na(data[[target]])
    )
    
    
    # Median fallback
    fallback_value <- median(
      data[[target]],
      na.rm = TRUE
    )
    
    
    # ------------------------------------------------------
    # 4. Impute each missing value
    # ------------------------------------------------------
    
    for (recipient in missing_rows) {
      
      
      donor_distances <- rep(
        NA_real_,
        length(donor_rows)
      )
      
      
      for (d in seq_along(donor_rows)) {
        
        donor <- donor_rows[d]
        
        
        # Measurements observed for BOTH recipient and donor
        common_vars <- measurement_vars[
          
          !is.na(
            X_scaled[
              recipient,
              measurement_vars
            ]
          ) &
            
            !is.na(
              X_scaled[
                donor,
                measurement_vars
              ]
            )
        ]
        
        
        # Exclude the target measurement itself
        common_vars <- setdiff(
          common_vars,
          target
        )
        
        
        # Calculate distance if shared measurements exist
        if (length(common_vars) > 0) {
          
          differences <-
            as.numeric(
              X_scaled[
                recipient,
                common_vars
              ]
            ) -
            as.numeric(
              X_scaled[
                donor,
                common_vars
              ]
            )
          
          
          donor_distances[d] <- sqrt(
            mean(
              differences^2
            )
          )
        }
      }
      
      
      # ----------------------------------------------------
      # 5. Select usable donors
      # ----------------------------------------------------
      
      usable <- which(
        !is.na(donor_distances)
      )
      
      
      if (length(usable) > 0) {
        
        usable_donors <- donor_rows[
          usable
        ]
        
        usable_distances <- donor_distances[
          usable
        ]
        
        
        # Order nearest to furthest
        donor_order <- order(
          usable_distances
        )
        
        
        # Use up to k nearest neighbours
        n_use <- min(
          k,
          length(donor_order)
        )
        
        
        nearest_donors <- usable_donors[
          donor_order[
            1:n_use
          ]
        ]
        
        
        # Impute using mean donor value
        imputed_data[[target]][recipient] <-
          mean(
            data[[target]][
              nearest_donors
            ],
            na.rm = TRUE
          )
        
      } else {
        
        # Fallback only if no usable donor exists
        imputed_data[[target]][recipient] <-
          fallback_value
      }
    }
  }
  
  
  return(imputed_data)
}


# ==========================================================
# 6. Apply kNN with k = 3
# ==========================================================

dat_knn_k3 <- knn_impute(
  data = dat,
  measurement_vars = measurement_vars,
  k = 3
)

# ==========================================================
# 7. Check completed dataset
# ==========================================================

remaining_missing_knn <- sum(
  is.na(
    dat_knn_k3[measurement_vars]
  )
)


cat(
  "Missing measurement values after kNN imputation:",
  remaining_missing_knn,
  "\n"
)

# ==========================================================
# 8. Check preservation of original observed values
# ==========================================================

original_matrix <- as.matrix(
  dat[measurement_vars]
)

knn_matrix <- as.matrix(
  dat_knn_k3[measurement_vars]
)


observed_positions <- !is.na(
  original_matrix
)


knn_observed_preserved <- all(
  knn_matrix[observed_positions] ==
    original_matrix[observed_positions]
)


cat(
  "Original observed values preserved:",
  knn_observed_preserved,
  "\n"
)

# ==========================================================
# 9. Check Patient ID, gender and age
# ==========================================================

cat(
  "Patient ID unchanged:",
  identical(
    dat$`Patient ID`,
    dat_knn_k3$`Patient ID`
  ),
  "\n"
)

cat(
  "Gender unchanged:",
  identical(
    dat$gender,
    dat_knn_k3$gender
  ),
  "\n"
)

cat(
  "Age unchanged:",
  identical(
    dat$age,
    dat_knn_k3$age
  ),
  "\n"
)


# ==========================================================
# TASK 2.4
# Final checks and documentation
# ==========================================================


# ----------------------------------------------------------
# 1. Check for completely missing measurement variables
# ----------------------------------------------------------

completely_missing_vars <- measurement_vars[
  sapply(
    dat[measurement_vars],
    function(x) {
      all(is.na(x))
    }
  )
]


cat(
  "Number of completely missing measurement variables:",
  length(completely_missing_vars),
  "\n"
)


if (length(completely_missing_vars) > 0) {
  
  cat(
    "Completely missing variables:\n"
  )
  
  print(
    completely_missing_vars
  )
}

# Expected:
# 0


# ----------------------------------------------------------
# 2. Check how many original values required imputation
# ----------------------------------------------------------

original_missing_n <- sum(
  is.na(
    dat[measurement_vars]
  )
)


cat(
  "Number of original missing measurement values:",
  original_missing_n,
  "\n"
)

# Expected:
# 238


# ----------------------------------------------------------
# 3. Final completeness checks
# ----------------------------------------------------------

cat(
  "Missing values remaining after median imputation:",
  sum(
    is.na(
      dat_median[measurement_vars]
    )
  ),
  "\n"
)


cat(
  "Missing values remaining after kNN imputation:",
  sum(
    is.na(
      dat_knn_k3[measurement_vars]
    )
  ),
  "\n"
)

# Expected:
# 0
# 0


# ----------------------------------------------------------
# 4. Subjects requiring imputation
# ----------------------------------------------------------

subjects_imputed <- dat$`Patient ID`[
  rowSums(
    is.na(
      dat[measurement_vars]
    )
  ) > 0
]


cat(
  "Number of subjects requiring imputation:",
  length(subjects_imputed),
  "\n"
)


cat(
  "Subjects requiring imputation:\n"
)

print(
  subjects_imputed
)

# ==========================================================
# TASK 3: VALIDATE THE IMPUTATION
#
# Methods compared:
# 1. Median imputation
# 2. k-nearest-neighbours (kNN)
#
# kNN values compared:
# k = 3, 4, 5
#
# Validation approach:
# Approximately 10% of observed values in variables that
# contain real missingness are temporarily masked.
#
# The same artificially masked values are used for
# Median and all kNN settings.
# ==========================================================


# ==========================================================
# PART A: LOAD PACKAGES AND ORIGINAL DATA
# ==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# Load ORIGINAL incomplete dataset
dat <- read.csv(
  "C:/Users/A C E R/OneDrive - Universiti Malaya/Desktop/Cranifacial Assestment/Craniofacial Data.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


# Define the 152 craniofacial measurements
# Patient ID, gender and age are not imputed
measurement_vars <- setdiff(
  names(dat),
  c("Patient ID", "gender", "age")
)


# Check original dataset
cat("\n=====================================\n")
cat("ORIGINAL DATA\n")
cat("=====================================\n")

cat(
  "Dataset dimensions:",
  dim(dat),
  "\n"
)

cat(
  "Number of craniofacial measurements:",
  length(measurement_vars),
  "\n"
)

cat(
  "Original real missing values:",
  sum(is.na(dat[measurement_vars])),
  "\n"
)


# Expected:
# 24 155
# 152 measurements
# 238 real missing values


# ==========================================================
# PART B: VARIABLES WITH REAL MISSING VALUES
# ==========================================================

missing_vars <- measurement_vars[
  colSums(
    is.na(dat[measurement_vars])
  ) > 0
]


cat(
  "Variables containing real missing values:",
  length(missing_vars),
  "\n"
)

# Expected = 51


# Missing percentage for every measurement
variable_missing_percent <- sapply(
  dat[measurement_vars],
  function(x) {
    mean(is.na(x)) * 100
  }
)


# ==========================================================
# PART C: CREATE VALIDATION DATASET
# ==========================================================

# Start from original incomplete data
dat_validation <- dat


# Matrix identifying the known values that will
# temporarily be changed to NA
validation_mask <- matrix(
  FALSE,
  nrow = nrow(dat),
  ncol = length(measurement_vars),
  dimnames = list(
    NULL,
    measurement_vars
  )
)


# Fixed random seed for reproducibility
set.seed(123)


# Temporarily mask approximately 10% of the observed values
# within the 51 variables that contain real missingness
for (v in missing_vars) {
  
  # Find values that are currently observed
  observed_rows <- which(
    !is.na(dat[[v]])
  )
  
  
  # Number to mask
  n_mask <- max(
    1,
    round(
      0.10 * length(observed_rows)
    )
  )
  
  
  # Randomly choose which observed values to hide
  rows_to_mask <- sample(
    observed_rows,
    size = n_mask,
    replace = FALSE
  )
  
  
  # Record artificial mask
  validation_mask[
    rows_to_mask,
    v
  ] <- TRUE
}


# ==========================================================
# PART D: SAVE TRUE VALUES BEFORE MASKING
# ==========================================================

original_matrix <- as.matrix(
  dat[measurement_vars]
)


# Save the real known values before hiding them
true_validation_values <- original_matrix[
  validation_mask
]


# Create artificial missing values
validation_matrix <- as.matrix(
  dat_validation[measurement_vars]
)

validation_matrix[
  validation_mask
] <- NA


dat_validation[
  measurement_vars
] <- validation_matrix


# Check
cat("\n=====================================\n")
cat("VALIDATION MASK\n")
cat("=====================================\n")

cat(
  "Real missing values:",
  sum(is.na(dat[measurement_vars])),
  "\n"
)

cat(
  "Artificially masked known values:",
  sum(validation_mask),
  "\n"
)

cat(
  "Total missing values in validation dataset:",
  sum(is.na(dat_validation[measurement_vars])),
  "\n"
)

cat(
  "Saved true validation values:",
  length(true_validation_values),
  "\n"
)


# Your previous results:
# Real missing = 238
# Artificially masked = 92
# Total = 330
# True values saved = 92


# ==========================================================
# PART E: MEDIAN IMPUTATION FUNCTION
# ==========================================================

median_impute <- function(data, vars) {
  
  result <- data
  
  
  for (v in vars) {
    
    med_v <- median(
      result[[v]],
      na.rm = TRUE
    )
    
    
    missing_rows <- is.na(
      result[[v]]
    )
    
    
    result[[v]][missing_rows] <- med_v
  }
  
  
  return(result)
}


# ==========================================================
# PART F: kNN IMPUTATION FUNCTION
# ==========================================================

knn_impute <- function(
    data,
    vars,
    k) {
  
  result <- data
  
  
  # --------------------------------------------------------
  # Standardise all measurements
  # --------------------------------------------------------
  
  variable_means <- sapply(
    data[vars],
    mean,
    na.rm = TRUE
  )
  
  
  variable_sds <- sapply(
    data[vars],
    sd,
    na.rm = TRUE
  )
  
  
  # Avoid division by zero
  variable_sds[
    is.na(variable_sds) |
      variable_sds == 0
  ] <- 1
  
  
  standardised <- sweep(
    data[vars],
    2,
    variable_means,
    "-"
  )
  
  
  standardised <- sweep(
    standardised,
    2,
    variable_sds,
    "/"
  )
  
  
  standardised <- as.matrix(
    standardised
  )
  
  
  # --------------------------------------------------------
  # Impute each missing value
  # --------------------------------------------------------
  
  for (target_var in vars) {
    
    target_j <- match(
      target_var,
      vars
    )
    
    
    missing_rows <- which(
      is.na(data[[target_var]])
    )
    
    
    # If this variable has no missing values, move on
    if (length(missing_rows) == 0) {
      next
    }
    
    
    for (row_i in missing_rows) {
      
      
      # Potential donors must have an observed value
      # for the target variable
      donor_rows <- which(
        !is.na(data[[target_var]])
      )
      
      
      distances <- rep(
        NA_real_,
        length(donor_rows)
      )
      
      
      # ----------------------------------------------------
      # Calculate distance to each donor subject
      # ----------------------------------------------------
      
      for (d in seq_along(donor_rows)) {
        
        donor_i <- donor_rows[d]
        
        
        # Find measurements observed for BOTH subjects
        common_vars <- which(
          !is.na(
            standardised[row_i, ]
          ) &
            !is.na(
              standardised[donor_i, ]
            )
        )
        
        
        # Do not use the target measurement itself
        common_vars <- setdiff(
          common_vars,
          target_j
        )
        
        
        if (length(common_vars) > 0) {
          
          differences <-
            standardised[
              row_i,
              common_vars
            ] -
            standardised[
              donor_i,
              common_vars
            ]
          
          
          distances[d] <- sqrt(
            mean(
              differences^2
            )
          )
        }
      }
      
      
      # Keep only donors with valid distance
      valid <- !is.na(
        distances
      )
      
      
      donor_rows <- donor_rows[
        valid
      ]
      
      
      distances <- distances[
        valid
      ]
      
      
      # ----------------------------------------------------
      # Fallback:
      # if no valid donor exists, use variable median
      # ----------------------------------------------------
      
      if (length(donor_rows) == 0) {
        
        result[
          row_i,
          target_var
        ] <- median(
          data[[target_var]],
          na.rm = TRUE
        )
        
        next
      }
      
      
      # Sort donor subjects from nearest to furthest
      donor_order <- order(
        distances
      )
      
      
      donor_rows <- donor_rows[
        donor_order
      ]
      
      
      # Use up to k nearest neighbours
      k_use <- min(
        k,
        length(donor_rows)
      )
      
      
      nearest_rows <- donor_rows[
        seq_len(k_use)
      ]
      
      
      # Impute using mean target value
      # from nearest neighbours
      result[
        row_i,
        target_var
      ] <- mean(
        data[
          nearest_rows,
          target_var
        ],
        na.rm = TRUE
      )
    }
  }
  
  
  return(result)
}


# ==========================================================
# PART G: VALIDATE MEDIAN IMPUTATION
# ==========================================================

validation_median <- median_impute(
  dat_validation,
  measurement_vars
)


# Check
cat(
  "\nMissing after median validation imputation:",
  sum(
    is.na(
      validation_median[measurement_vars]
    )
  ),
  "\n"
)


median_matrix <- as.matrix(
  validation_median[
    measurement_vars
  ]
)


# Only evaluate artificially masked positions
median_predictions <- median_matrix[
  validation_mask
]


# Calculate RMSE
median_rmse <- sqrt(
  mean(
    (
      median_predictions -
        true_validation_values
    )^2
  )
)


# Calculate MAE
median_mae <- mean(
  abs(
    median_predictions -
      true_validation_values
  )
)


cat(
  "Median RMSE:",
  median_rmse,
  "\n"
)

cat(
  "Median MAE:",
  median_mae,
  "\n"
)


# ==========================================================
# PART H: COMPARE kNN k = 3, 4 AND 5
# ==========================================================

k_values <- c(3, 4, 5)

k_comparison <- data.frame(
  k = k_values,
  RMSE = NA_real_,
  MAE = NA_real_
)

knn_validation_list <- list()

for (j in seq_along(k_values)) {
  
  current_k <- k_values[j]
  
  # Apply kNN to the SAME validation dataset
  temp_knn <- knn_impute(
    dat_validation,
    measurement_vars,
    k = current_k
  )
  
  # Store result
  knn_validation_list[[as.character(current_k)]] <- temp_knn
  
  # Convert to matrix
  temp_matrix <- as.matrix(
    temp_knn[measurement_vars]
  )
  
  # Predictions only for the 92 artificially masked values
  temp_predictions <- temp_matrix[
    validation_mask
  ]
  
  # RMSE
  k_comparison$RMSE[j] <- sqrt(
    mean(
      (temp_predictions -
         true_validation_values)^2
    )
  )
  
  # MAE
  k_comparison$MAE[j] <- mean(
    abs(
      temp_predictions -
        true_validation_values
    )
  )
}


cat("\n=====================================\n")
cat("kNN NEIGHBOUR COMPARISON\n")
cat("=====================================\n")

print(k_comparison)

# ==========================================================
# PART I: SELECT BEST k
# ==========================================================

# Select k with lowest RMSE
# If RMSE is tied, choose the one with lower MAE

best_row <- k_comparison %>%
  arrange(RMSE, MAE) %>%
  slice(1)

best_k <- best_row$k


cat(
  "\nSelected number of neighbours:",
  best_k,
  "\n"
)

cat(
  "Selected kNN RMSE:",
  best_row$RMSE,
  "\n"
)

cat(
  "Selected kNN MAE:",
  best_row$MAE,
  "\n"
)


# ==========================================================
# PART J: USE THE SELECTED kNN RESULT
# ==========================================================

# IMPORTANT: correct R syntax
validation_knn <- knn_validation_list[[as.character(best_k)]]


knn_matrix <- as.matrix(
  validation_knn[measurement_vars]
)


knn_predictions <- knn_matrix[
  validation_mask
]


cat(
  "\nNumber of true values:",
  length(true_validation_values),
  "\n"
)

cat(
  "Median predictions:",
  length(median_predictions),
  "\n"
)

cat(
  "Selected kNN predictions:",
  length(knn_predictions),
  "\n"
)


# ==========================================================
# PART K: OVERALL MEDIAN VS SELECTED kNN
# ==========================================================

knn_rmse <- sqrt(
  mean(
    (knn_predictions -
       true_validation_values)^2
  )
)


knn_mae <- mean(
  abs(
    knn_predictions -
      true_validation_values
  )
)


knn_label <- paste0(
  "kNN (k=",
  best_k,
  ")"
)


overall_validation <- data.frame(
  Method = c(
    "Median",
    knn_label
  ),
  RMSE = c(
    median_rmse,
    knn_rmse
  ),
  MAE = c(
    median_mae,
    knn_mae
  )
)


cat("\n=====================================\n")
cat("OVERALL VALIDATION RESULTS\n")
cat("=====================================\n")

print(overall_validation)


# ==========================================================
# PART I: SELECT THE BEST k
# ==========================================================

# Lowest RMSE is selected.
# If RMSE is tied, lower MAE is preferred.

best_row <- k_comparison %>%
  arrange(
    RMSE,
    MAE
  ) %>%
  slice(
    1
  )


best_k <- best_row$k


cat(
  "\nSelected number of neighbours:",
  best_k,
  "\n"
)

cat(
  "Selected kNN RMSE:",
  best_row$RMSE,
  "\n"
)

cat(
  "Selected kNN MAE:",
  best_row$MAE,
  "\n"
)


# ==========================================================
# PART J: USE THE SELECTED kNN RESULT
# ==========================================================

validation_knn <- knn_validation_list[[as.character(best_k)]]

knn_matrix <- as.matrix(
  validation_knn[
    measurement_vars
  ]
)


knn_predictions <- knn_matrix[
  validation_mask
]


# Checks
cat(
  "\nNumber of true values:",
  length(true_validation_values),
  "\n"
)

cat(
  "Median predictions:",
  length(median_predictions),
  "\n"
)

cat(
  "Selected kNN predictions:",
  length(knn_predictions),
  "\n"
)


# ==========================================================
# PART K: OVERALL MEDIAN VS SELECTED kNN
# ==========================================================

knn_rmse <- sqrt(
  mean(
    (
      knn_predictions -
        true_validation_values
    )^2
  )
)


knn_mae <- mean(
  abs(
    knn_predictions -
      true_validation_values
  )
)


knn_label <- paste0(
  "kNN (k=",
  best_k,
  ")"
)


overall_validation <- data.frame(
  
  Method = c(
    "Median",
    knn_label
  ),
  
  RMSE = c(
    median_rmse,
    knn_rmse
  ),
  
  MAE = c(
    median_mae,
    knn_mae
  )
)


cat("\n=====================================\n")
cat("OVERALL VALIDATION RESULTS\n")
cat("=====================================\n")


print(
  overall_validation
)


# ==========================================================
# PART L: DETAILED VALIDATION DATA
# ==========================================================

mask_positions <- which(
  validation_mask,
  arr.ind = TRUE
)


validation_detail <- data.frame(
  
  Patient_ID =
    dat[["Patient ID"]][
      mask_positions[, 1]
    ],
  
  Variable =
    measurement_vars[
      mask_positions[, 2]
    ],
  
  True_Value =
    true_validation_values,
  
  Median_Predicted =
    median_predictions,
  
  kNN_Predicted =
    knn_predictions
)


# Errors
validation_detail$Median_Error <-
  validation_detail$Median_Predicted -
  validation_detail$True_Value


validation_detail$kNN_Error <-
  validation_detail$kNN_Predicted -
  validation_detail$True_Value


validation_detail$Median_Absolute_Error <-
  abs(
    validation_detail$Median_Error
  )


validation_detail$kNN_Absolute_Error <-
  abs(
    validation_detail$kNN_Error
  )


validation_detail$Median_Squared_Error <-
  validation_detail$Median_Error^2


validation_detail$kNN_Squared_Error <-
  validation_detail$kNN_Error^2


# ==========================================================
# PART M: VALIDATION BY VARIABLE
# ==========================================================

validation_by_variable <- validation_detail %>%
  group_by(
    Variable
  ) %>%
  summarise(
    
    N_masked = n(),
    
    Median_RMSE = sqrt(
      mean(
        Median_Squared_Error
      )
    ),
    
    Median_MAE = mean(
      Median_Absolute_Error
    ),
    
    kNN_RMSE = sqrt(
      mean(
        kNN_Squared_Error
      )
    ),
    
    kNN_MAE = mean(
      kNN_Absolute_Error
    ),
    
    .groups = "drop"
  )


cat("\n=====================================\n")
cat("VALIDATION BY VARIABLE\n")
cat("=====================================\n")


print(
  validation_by_variable,
  n = Inf
)


# ==========================================================
# PART N: VALIDATION BY MISSINGNESS LEVEL
# ==========================================================

validation_detail$Original_Missing_Percent <-
  variable_missing_percent[
    validation_detail$Variable
  ]


validation_detail$Missingness_Group <- cut(
  
  validation_detail$Original_Missing_Percent,
  
  breaks = c(
    -Inf,
    10,
    30,
    Inf
  ),
  
  labels = c(
    "Low (<10%)",
    "Moderate (10-30%)",
    "High (>30%)"
  )
)


validation_by_missingness <- validation_detail %>%
  group_by(
    Missingness_Group
  ) %>%
  summarise(
    
    N_masked = n(),
    
    Median_RMSE = sqrt(
      mean(
        Median_Squared_Error
      )
    ),
    
    Median_MAE = mean(
      Median_Absolute_Error
    ),
    
    kNN_RMSE = sqrt(
      mean(
        kNN_Squared_Error
      )
    ),
    
    kNN_MAE = mean(
      kNN_Absolute_Error
    ),
    
    .groups = "drop"
  )


cat("\n=====================================\n")
cat("VALIDATION BY MISSINGNESS LEVEL\n")
cat("=====================================\n")


print(
  validation_by_missingness,
  width = Inf
)


# ==========================================================
# PART O: HOW OFTEN WAS EACH METHOD BETTER?
# ==========================================================

validation_detail$Better_Method <- ifelse(
  
  validation_detail$Median_Absolute_Error <
    validation_detail$kNN_Absolute_Error,
  
  "Median",
  
  ifelse(
    
    validation_detail$kNN_Absolute_Error <
      validation_detail$Median_Absolute_Error,
    
    "kNN",
    
    "Tie"
  )
)


better_method_counts <- table(
  validation_detail$Better_Method
)


cat("\n=====================================\n")
cat("BETTER METHOD BY MASKED VALUE\n")
cat("=====================================\n")


print(
  better_method_counts
)


# ==========================================================
# PART P: PLOT OVERALL RMSE AND MAE
# ==========================================================

plot_validation <- overall_validation %>%
  pivot_longer(
    
    cols = c(
      RMSE,
      MAE
    ),
    
    names_to = "Metric",
    
    values_to = "Value"
  )


ggplot(
  plot_validation,
  aes(
    x = Method,
    y = Value,
    fill = Metric
  )
) +
  
  geom_col(
    position = "dodge"
  ) +
  
  labs(
    title = "Validation Error by Imputation Method",
    subtitle = "Lower RMSE and MAE indicate better performance",
    x = "Imputation method",
    y = "Error",
    fill = "Metric"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )


# ==========================================================
# PART Q: CREATE FINAL IMPUTED DATASETS
# ==========================================================

# IMPORTANT:
# Start again from ORIGINAL incomplete data.
# Do NOT use dat_validation here.


final_median <- median_impute(
  dat,
  measurement_vars
)


final_knn <- knn_impute(
  dat,
  measurement_vars,
  k = best_k
)


cat(
  "\nMissing in final median dataset:",
  sum(
    is.na(
      final_median[measurement_vars]
    )
  ),
  "\n"
)


cat(
  "Missing in final selected-k kNN dataset:",
  sum(
    is.na(
      final_knn[measurement_vars]
    )
  ),
  "\n"
)


# Expected = 0 for both


# ==========================================================
# PART R: DISTRIBUTION PRESERVATION TABLE
# ==========================================================

real_missing_n <- colSums(
  is.na(
    dat[measurement_vars]
  )
)


distribution_results <- data.frame(
  
  Variable =
    measurement_vars,
  
  Missing_n =
    real_missing_n,
  
  Missing_percent =
    variable_missing_percent,
  
  Observed_Mean =
    sapply(
      dat[measurement_vars],
      mean,
      na.rm = TRUE
    ),
  
  Observed_SD =
    sapply(
      dat[measurement_vars],
      sd,
      na.rm = TRUE
    ),
  
  Median_Mean =
    sapply(
      final_median[measurement_vars],
      mean
    ),
  
  Median_SD =
    sapply(
      final_median[measurement_vars],
      sd
    ),
  
  kNN_Mean =
    sapply(
      final_knn[measurement_vars],
      mean
    ),
  
  kNN_SD =
    sapply(
      final_knn[measurement_vars],
      sd
    )
)


# Only variables that originally had missingness
distribution_incomplete <-
  distribution_results %>%
  filter(
    Missing_n > 0
  ) %>%
  arrange(
    desc(Missing_percent)
  )


cat("\n=====================================\n")
cat("DISTRIBUTION PRESERVATION\n")
cat("=====================================\n")


print(
  distribution_incomplete
)


# ==========================================================
# PART S: DISTRIBUTION PLOT
# ==========================================================

# Select the 10 variables with highest original missingness
top10_vars <- distribution_incomplete %>%
  slice_head(
    n = 10
  ) %>%
  pull(
    Variable
  )


# Original observed data
observed_long <- dat %>%
  select(
    all_of(top10_vars)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Method = "Observed"
  )


# Median completed data
median_long <- final_median %>%
  select(
    all_of(top10_vars)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Method = "Median"
  )


# Selected kNN completed data
knn_long <- final_knn %>%
  select(
    all_of(top10_vars)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Method = knn_label
  )


distribution_plot_data <- bind_rows(
  observed_long,
  median_long,
  knn_long
)


# Logical order
distribution_plot_data$Method <- factor(
  distribution_plot_data$Method,
  levels = c(
    "Observed",
    "Median",
    knn_label
  )
)


ggplot(
  distribution_plot_data,
  aes(
    x = Method,
    y = Value
  )
) +
  
  geom_boxplot(
    na.rm = TRUE
  ) +
  
  facet_wrap(
    ~ Variable,
    scales = "free_y"
  ) +
  
  labs(
    title = "Distribution Preservation After Imputation",
    subtitle = "Ten measurements with the highest original missingness",
    x = NULL,
    y = "Measurement value"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold"
    )
  )


# ==========================================================
# PART T: CORRELATION PRESERVATION
# ==========================================================

# Original pairwise correlations
cor_original <- cor(
  dat[measurement_vars],
  use = "pairwise.complete.obs",
  method = "pearson"
)


# Median-imputed correlations
cor_median <- cor(
  final_median[measurement_vars],
  method = "pearson"
)


# kNN-imputed correlations
cor_knn <- cor(
  final_knn[measurement_vars],
  method = "pearson"
)


# Unique variable pairs
pair_positions <- which(
  upper.tri(
    cor_original
  ),
  arr.ind = TRUE
)


correlation_pairs <- data.frame(
  
  Variable_1 =
    rownames(cor_original)[
      pair_positions[, 1]
    ],
  
  Variable_2 =
    colnames(cor_original)[
      pair_positions[, 2]
    ],
  
  Original_r =
    cor_original[
      pair_positions
    ],
  
  Median_r =
    cor_median[
      pair_positions
    ],
  
  kNN_r =
    cor_knn[
      pair_positions
    ]
)


# Change in correlation
correlation_pairs$Median_Absolute_Change <-
  abs(
    correlation_pairs$Median_r -
      correlation_pairs$Original_r
  )


correlation_pairs$kNN_Absolute_Change <-
  abs(
    correlation_pairs$kNN_r -
      correlation_pairs$Original_r
  )


# Focus on correlations originally >= 0.70 in magnitude
strong_correlation_pairs <- correlation_pairs %>%
  filter(
    !is.na(Original_r),
    abs(Original_r) >= 0.70
  )


correlation_summary <- data.frame(
  
  Method = c(
    "Median",
    knn_label
  ),
  
  Strong_Pairs = c(
    nrow(strong_correlation_pairs),
    nrow(strong_correlation_pairs)
  ),
  
  Mean_Absolute_Change = c(
    
    mean(
      strong_correlation_pairs$
        Median_Absolute_Change,
      na.rm = TRUE
    ),
    
    mean(
      strong_correlation_pairs$
        kNN_Absolute_Change,
      na.rm = TRUE
    )
  ),
  
  Median_Absolute_Change = c(
    
    median(
      strong_correlation_pairs$
        Median_Absolute_Change,
      na.rm = TRUE
    ),
    
    median(
      strong_correlation_pairs$
        kNN_Absolute_Change,
      na.rm = TRUE
    )
  )
)


cat("\n=====================================\n")
cat("CORRELATION PRESERVATION\n")
cat("=====================================\n")


print(
  correlation_summary
)


# ==========================================================
# PART U: TOP 20 STRONG CORRELATION PAIRS
# ==========================================================

top_correlation_pairs <- strong_correlation_pairs %>%
  
  arrange(
    desc(
      abs(Original_r)
    )
  ) %>%
  
  select(
    Variable_1,
    Variable_2,
    Original_r,
    Median_r,
    kNN_r,
    Median_Absolute_Change,
    kNN_Absolute_Change
  ) %>%
  
  slice_head(
    n = 20
  ) %>%
  
  as_tibble()


cat("\n=====================================\n")
cat("TOP 20 STRONG CORRELATION PAIRS\n")
cat("=====================================\n")


print(
  top_correlation_pairs,
  n = 20
)


# ==========================================================
# PART V: SAVE RESULTS
# ==========================================================

output_folder <-
  "C:/Users/A C E R/OneDrive - Universiti Malaya/Desktop/Cranifacial Assestment/"


# k selection results
write.csv(
  k_comparison,
  paste0(
    output_folder,
    "Task3_kNN_k_comparison.csv"
  ),
  row.names = FALSE
)


# Overall validation
write.csv(
  overall_validation,
  paste0(
    output_folder,
    "Task3_overall_validation.csv"
  ),
  row.names = FALSE
)


# Detailed validation
write.csv(
  validation_detail,
  paste0(
    output_folder,
    "Task3_validation_detail.csv"
  ),
  row.names = FALSE
)


# Validation by variable
write.csv(
  validation_by_variable,
  paste0(
    output_folder,
    "Task3_validation_by_variable.csv"
  ),
  row.names = FALSE
)


# Validation by missingness level
write.csv(
  validation_by_missingness,
  paste0(
    output_folder,
    "Task3_validation_by_missingness.csv"
  ),
  row.names = FALSE
)


# Distribution preservation
write.csv(
  distribution_incomplete,
  paste0(
    output_folder,
    "Task3_distribution_preservation.csv"
  ),
  row.names = FALSE
)


# Correlation preservation
write.csv(
  correlation_summary,
  paste0(
    output_folder,
    "Task3_correlation_preservation.csv"
  ),
  row.names = FALSE
)


# Top correlation pairs
write.csv(
  top_correlation_pairs,
  paste0(
    output_folder,
    "Task3_top_correlation_pairs.csv"
  ),
  row.names = FALSE
)


# Final median-imputed dataset
write.csv(
  final_median,
  paste0(
    output_folder,
    "Craniofacial_imputed_median_FINAL.csv"
  ),
  row.names = FALSE
)


# Final kNN dataset using selected k
write.csv(
  final_knn,
  paste0(
    output_folder,
    "Craniofacial_imputed_kNN_k",
    best_k,
    "_FINAL.csv"
  ),
  row.names = FALSE
)


cat("\n=====================================\n")
cat("TASK 3 COMPLETED\n")
cat("=====================================\n")

cat(
  "Selected k for final kNN:",
  best_k,
  "\n"
)

cat(
  "All results saved successfully.\n"
)