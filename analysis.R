# ============================================================
# CCHS 2022: Cost-Related Non-Adherence to Prescription Drugs
# ============================================================

# 1. PROJECT SETUP ------------------------------------------------

# Create the folders needed for the project
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# 2. LOAD DATA ----------------------------------------------------

# Install and load the package used to import the CSV file
install.packages("readr")
library(readr)

# Load the CCHS 2022 public-use microdata file
cchs_data <- read_csv(
  "data/raw/2022_CSV/CSV/Data_Données/pumf_cchs.csv"
)
# Check the dimensions of the dataset
dim(cchs_data)

# 3. SELECT VARIABLES FOR THE PROJECT ----------------------------

candidate_variables <- c(
  "PCNDGCRA", "INCDGHH", "INP_05", "DHHGAGE", "DHH_SEX",
  "EDDVH3", "GEN_01", "CCC_05", "CCC_80", "WTS_M"
)

# Confirm that all required variables are present
candidate_variables %in% names(cchs_data)

# 4. INSPECT VARIABLE CODING --------------------------------------

lapply(
  cchs_data[candidate_variables],
  table,
  useNA = "ifany"
)

# 5. VERIFY VARIABLE DEFINITIONS ---------------------------------

# Install and load the package used to read the PDF data dictionary
install.packages("pdftools")
library(pdftools)

# Location of the CCHS 2022 data dictionary
dictionary_path <- paste0(
  "data/raw/2022_CSV/CSV/Documentation/",
  "CCHS_2022_DataDictionary_Freqs.pdf"
)

# Extract text from the data dictionary
dictionary_text <- pdf_text(dictionary_path)

# Find and inspect the pages containing the outcome definition
grep("PCNDGCRA", dictionary_text)
cat(dictionary_text[109])
cat(dictionary_text[110])

# Inspect pages containing the predictor definitions
predictor_pages <- grep(
  "INCDGHH|INP_05|DHHGAGE|DHH_SEX|EDDVH3|GEN_01|CCC_05|CCC_80",
  dictionary_text
)

cat(dictionary_text[predictor_pages], sep = "\n")

# 6. DEFINE THE ANALYSIS POPULATION -------------------------------

# Keep respondents with a valid answer for cost-related
# non-adherence to prescription drugs
cchs_analysis <- subset(
  cchs_data,
  PCNDGCRA %in% c(1, 2)
)

dim(cchs_analysis)

# 7. CREATE THE BINARY OUTCOME ------------------------------------

# 1 = cost-related non-adherence
# 0 = no cost-related non-adherence
cchs_analysis$cost_nonadherence <- ifelse(
  cchs_analysis$PCNDGCRA == 1,
  1,
  0
)

# Check the outcome distribution
table(cchs_analysis$cost_nonadherence)

prop.table(
  table(cchs_analysis$cost_nonadherence)
) * 100

# 8. RECODE PREDICTOR VARIABLES ----------------------------------

# Household income
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

# Drug insurance coverage
cchs_analysis$drug_insurance <- factor(
  cchs_analysis$INP_05,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

# Age group
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

# Sex
cchs_analysis$sex <- factor(
  cchs_analysis$DHH_SEX,
  levels = c(1, 2),
  labels = c("Male", "Female")
)

# Education
cchs_analysis$education <- factor(
  cchs_analysis$EDDVH3,
  levels = 1:3,
  labels = c(
    "Less than secondary",
    "Secondary only",
    "Post-secondary"
  )
)

# Self-perceived health
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

# Diabetes
cchs_analysis$diabetes <- factor(
  cchs_analysis$CCC_05,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

# High blood pressure
cchs_analysis$high_blood_pressure <- factor(
  cchs_analysis$CCC_80,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

# 9. CHECK RECODED VARIABLES -------------------------------------

recoded_variables <- c(
  "income", "drug_insurance", "age_group", "sex", "education",
  "perceived_health", "diabetes", "high_blood_pressure"
)

lapply(
  cchs_analysis[recoded_variables],
  table,
  useNA = "ifany"
)

# 10. ASSESS MISSING DATA -----------------------------------------

model_variables <- c(
  "cost_nonadherence", "income", "drug_insurance", "age_group",
  "sex", "education", "perceived_health", "diabetes",
  "high_blood_pressure"
)

# Check missing values for the variables used in the model
colSums(is.na(cchs_analysis[model_variables]))

# Identify complete cases
complete_cases <- complete.cases(
  cchs_analysis[model_variables]
)

# Number and percentage of complete cases
sum(complete_cases)
mean(complete_cases) * 100

# 11. DESCRIPTIVE ANALYSIS ----------------------------------------

# Calculate the percentage with cost-related non-adherence
# within each category of each predictor
predictors <- c(
  "income", "drug_insurance", "age_group", "sex", "education",
  "perceived_health", "diabetes", "high_blood_pressure"
)

descriptive_results <- lapply(
  predictors,
  function(variable) {
    tab <- table(
      cchs_analysis[[variable]],
      cchs_analysis$cost_nonadherence
    )
    
    percentage <- prop.table(
      tab,
      margin = 1
    )[, "1"] * 100
    
    data.frame(
      category = names(percentage),
      percent_nonadherence = round(percentage, 1)
    )
  }
)

descriptive_results

# 12. MULTIVARIABLE LOGISTIC REGRESSION ---------------------------

# Restrict the analysis to complete cases
analysis_data <- cchs_analysis[complete_cases, ]

# Fit a multivariable logistic regression model
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
  family = binomial(link = "logit")
)

# 13. EXTRACT AND SAVE MODEL RESULTS ------------------------------

# Install and load broom for a clean regression results table
install.packages("broom")
library(broom)

# Convert model coefficients to odds ratios with 95% confidence intervals
results <- tidy(
  model,
  exponentiate = TRUE,
  conf.int = TRUE
)

# Save the results as a CSV file
write.csv(
  results,
  "tables/logistic_regression_results.csv",
  row.names = FALSE
)

# Display the results
View(results)
print(results)

# ============================================================
# END OF ANALYSIS
# ============================================================