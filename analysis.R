# ============================================================
# CCHS 2022: Cost-Related Non-Adherence to Prescription Drugs
# ============================================================


# 1. PROJECT SETUP ------------------------------------------------

# Create the folders needed for the project.
# The raw CCHS dataset can remain on your computer locally.
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)


# 2. LOAD DATA ----------------------------------------------------

# Install and load the package used to import the CSV file.
install.packages("readr")
library(readr)

# Load the CCHS 2022 public-use microdata file.
cchs_data <- read_csv(
  "data/raw/2022_CSV/CSV/Data_Données/pumf_cchs.csv"
)

# Check the dimensions of the dataset.
dim(cchs_data)


# 3. SELECT VARIABLES FOR THE PROJECT ----------------------------

# List the CCHS variables needed for the analysis.
candidate_variables <- c(
  "PCNDGCRA",
  "INCDGHH",
  "INP_05",
  "DHHGAGE",
  "DHH_SEX",
  "EDDVH3",
  "GEN_01",
  "CCC_05",
  "CCC_80",
  "WTS_M"
)

# Confirm that all required variables are present in the dataset.
candidate_variables %in% names(cchs_data)


# 4. INSPECT VARIABLE CODING --------------------------------------

# Inspect the coding and missing values for the selected variables.
lapply(
  cchs_data[candidate_variables],
  table,
  useNA = "ifany"
)


# 5. VERIFY VARIABLE DEFINITIONS ---------------------------------

# Install and load the package used to read the CCHS data dictionary PDF.
install.packages("pdftools")
library(pdftools)

# Location of the CCHS 2022 data dictionary.
dictionary_path <- paste0(
  "data/raw/2022_CSV/CSV/Documentation/",
  "CCHS_2022_DataDictionary_Freqs.pdf"
)

# Extract text from the data dictionary.
dictionary_text <- pdf_text(dictionary_path)

# Find the pages containing the outcome definition.
grep("PCNDGCRA", dictionary_text)

# Inspect the relevant pages for the outcome variable.
cat(dictionary_text[109])
cat(dictionary_text[110])

# Find pages containing the predictor definitions.
predictor_pages <- grep(
  "INCDGHH|INP_05|DHHGAGE|DHH_SEX|EDDVH3|GEN_01|CCC_05|CCC_80",
  dictionary_text
)

# Display the relevant predictor definitions.
cat(
  dictionary_text[predictor_pages],
  sep = "\n"
)


# 6. DEFINE THE ANALYSIS POPULATION -------------------------------

# Keep respondents with a valid answer for cost-related
# non-adherence to prescription drugs.
cchs_analysis <- subset(
  cchs_data,
  PCNDGCRA %in% c(1, 2)
)

# Check the number of respondents remaining.
dim(cchs_analysis)


# 7. CREATE THE BINARY OUTCOME ------------------------------------

# Create a binary outcome:
# 1 = cost-related non-adherence
# 0 = no cost-related non-adherence.
cchs_analysis$cost_nonadherence <- ifelse(
  cchs_analysis$PCNDGCRA == 1,
  1,
  0
)

# Check the distribution of the outcome.
table(cchs_analysis$cost_nonadherence)

# Calculate the percentage of respondents in each outcome category.
prop.table(
  table(cchs_analysis$cost_nonadherence)
) * 100


# 8. RECODE PREDICTOR VARIABLES ----------------------------------

# Household income.
cchs_analysis$income <- factor(
  cchs_analysis$INCDGHH,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "<$20,000",
    "$20,000-$39,999",
    "$40,000-$59,999",
    "$60,000-$79,999",
    "$80,000+"
  )
)

# Drug insurance coverage.
cchs_analysis$drug_insurance <- factor(
  cchs_analysis$INP_05,
  levels = c(1, 2),
  labels = c(
    "Yes",
    "No"
  )
)

# Age group.
cchs_analysis$age_group <- factor(
  cchs_analysis$DHHGAGE,
  levels = 1:5,
  labels = c(
    "12-17",
    "18-34",
    "35-49",
    "50-64",
    "65+"
  )
)

# Sex.
cchs_analysis$sex <- factor(
  cchs_analysis$DHH_SEX,
  levels = c(1, 2),
  labels = c(
    "Male",
    "Female"
  )
)

# Education.
cchs_analysis$education <- factor(
  cchs_analysis$EDDVH3,
  levels = 1:3,
  labels = c(
    "Less than secondary",
    "Secondary only",
    "Post-secondary"
  )
)

# Self-perceived health.
cchs_analysis$perceived_health <- factor(
  cchs_analysis$GEN_01,
  levels = 1:5,
  labels = c(
    "Excellent",
    "Very good",
    "Good",
    "Fair",
    "Poor"
  )
)

# Diabetes.
cchs_analysis$diabetes <- factor(
  cchs_analysis$CCC_05,
  levels = c(1, 2),
  labels = c(
    "Yes",
    "No"
  )
)

# High blood pressure.
cchs_analysis$high_blood_pressure <- factor(
  cchs_analysis$CCC_80,
  levels = c(1, 2),
  labels = c(
    "Yes",
    "No"
  )
)


# 9. CHECK RECODED VARIABLES -------------------------------------

# List the newly created variables.
recoded_variables <- c(
  "income",
  "drug_insurance",
  "age_group",
  "sex",
  "education",
  "perceived_health",
  "diabetes",
  "high_blood_pressure"
)

# Check the coding and missing values of the recoded variables.
lapply(
  cchs_analysis[recoded_variables],
  table,
  useNA = "ifany"
)


# 10. ASSESS MISSING DATA -----------------------------------------

# Define the variables that will be included in the regression model.
model_variables <- c(
  "cost_nonadherence",
  "income",
  "drug_insurance",
  "age_group",
  "sex",
  "education",
  "perceived_health",
  "diabetes",
  "high_blood_pressure"
)

# Count missing values for each model variable.
colSums(
  is.na(cchs_analysis[model_variables])
)

# Identify respondents with complete data for all model variables.
complete_cases <- complete.cases(
  cchs_analysis[model_variables]
)

# Count the number of complete cases.
sum(complete_cases)

# Calculate the percentage of respondents with complete data.
mean(complete_cases) * 100


# 11. DESCRIPTIVE ANALYSIS ----------------------------------------

# List the predictors for which descriptive percentages will be calculated.
predictors <- c(
  "income",
  "drug_insurance",
  "age_group",
  "sex",
  "education",
  "perceived_health",
  "diabetes",
  "high_blood_pressure"
)

# Calculate the percentage with cost-related non-adherence
# within each category of each predictor.
descriptive_results <- lapply(
  predictors,
  function(variable) {
    
    # Create a two-way table of the predictor and outcome.
    tab <- table(
      cchs_analysis[[variable]],
      cchs_analysis$cost_nonadherence
    )
    
    # Calculate the percentage with non-adherence within
    # each category of the predictor.
    percentage <- prop.table(
      tab,
      margin = 1
    )[, "1"] * 100
    
    # Store the results in a simple data frame.
    data.frame(
      category = names(percentage),
      percent_nonadherence = round(
        percentage,
        1
      )
    )
  }
)

# Display the descriptive results.
descriptive_results


# 12. MULTIVARIABLE LOGISTIC REGRESSION ---------------------------

# Restrict the analysis to respondents with complete data
# for all variables included in the model.
analysis_data <- cchs_analysis[
  complete_cases,
]

# Fit a multivariable logistic regression model.
#
# The outcome is cost-related non-adherence to prescription drugs.
# The model estimates the association between the outcome and
# the selected demographic, socioeconomic, health and insurance variables.
model <- glm(
  cost_nonadherence ~ income +
    drug_insurance +
    age_group +
    sex +
    education +
    perceived_health +
    diabetes +
    high_blood_pressure,
  data = analysis_data,
  family = binomial(
    link = "logit"
  )
)


# 13. EXTRACT AND CLEAN MODEL RESULTS -----------------------------

# Install and load broom for a clean regression results table.
install.packages("broom")
library(broom)

# Extract model results as odds ratios with 95% confidence intervals.
results <- tidy(
  model,
  exponentiate = TRUE,
  conf.int = TRUE
)

# Remove the intercept because it is not a predictor
# of substantive interest for the portfolio results table.
results <- results[
  results$term != "(Intercept)",
]


# Create a clean predictor label.
results$predictor <- ifelse(
  grepl("^income", results$term),
  "Household income",
  ifelse(
    grepl("^drug_insurance", results$term),
    "Drug insurance",
    ifelse(
      grepl("^age_group", results$term),
      "Age group",
      ifelse(
        grepl("^sex", results$term),
        "Sex",
        ifelse(
          grepl("^education", results$term),
          "Education",
          ifelse(
            grepl("^perceived_health", results$term),
            "Perceived health",
            ifelse(
              grepl("^diabetes", results$term),
              "Diabetes",
              "High blood pressure"
            )
          )
        )
      )
    )
  )
)


# Extract the category being compared with the reference category.
results$category <- results$term

results$category <- sub(
  "^income",
  "",
  results$category
)

results$category <- sub(
  "^drug_insurance",
  "",
  results$category
)

results$category <- sub(
  "^age_group",
  "",
  results$category
)

results$category <- sub(
  "^sex",
  "",
  results$category
)

results$category <- sub(
  "^education",
  "",
  results$category
)

results$category <- sub(
  "^perceived_health",
  "",
  results$category
)

results$category <- sub(
  "^diabetes",
  "",
  results$category
)

results$category <- sub(
  "^high_blood_pressure",
  "",
  results$category
)


# Remove the leading dollar sign temporarily created by
# the income variable names.
results$category <- sub(
  "^\\$",
  "",
  results$category
)

# Restore the dollar sign for household income categories.
results$category <- ifelse(
  results$predictor == "Household income",
  paste0(
    "$",
    results$category
  ),
  results$category
)


# Create a formatted 95% confidence interval.
results$confidence_interval <- paste0(
  sprintf(
    "%.2f",
    results$conf.low
  ),
  "–",
  sprintf(
    "%.2f",
    results$conf.high
  )
)


# Format p-values for easier reading.
results$p_value <- ifelse(
  results$p.value < 0.001,
  "<0.001",
  sprintf(
    "%.3f",
    results$p.value
  )
)


# Create the final portfolio-ready results table.
clean_results <- results[
  c(
    "predictor",
    "category",
    "estimate",
    "confidence_interval",
    "p_value"
  )
]


# Rename columns to make the table easier to understand.
names(clean_results) <- c(
  "Predictor",
  "Category",
  "Odds_Ratio",
  "95_CI",
  "P_value"
)


# Format odds ratios to exactly two decimal places.
clean_results$Odds_Ratio <- sprintf(
  "%.2f",
  clean_results$Odds_Ratio
)


# Save the clean results table as a CSV file.
# This creates/overwrites the portfolio results file.
write.csv(
  clean_results,
  "tables/logistic_regression_results.csv",
  row.names = FALSE
)


# Display the final clean table in RStudio.
View(clean_results)

# Also print the table in the Console so the results
# can be checked directly.
print(clean_results)


# ============================================================
# END OF ANALYSIS
# ============================================================