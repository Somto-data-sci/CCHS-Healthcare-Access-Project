# Cost-Related Non-Adherence to Prescription Drugs: CCHS 2022 Analysis

## 1. Project Objective and Contribution

This project uses data from the 2022 Canadian Community Health Survey (CCHS) to examine factors associated with cost-related non-adherence to prescription drugs.

The analysis focuses on the relationship between cost-related non-adherence and demographic, socioeconomic, health-status, and drug-insurance characteristics.

The project demonstrates an applied health data analytics workflow using R, including data preparation, variable recoding, descriptive analysis, and multivariable logistic regression.

## 2. Research Question

Which demographic, socioeconomic, health-status, and drug-insurance factors are associated with cost-related non-adherence to prescription drugs?

## 3. Key Findings

A multivariable logistic regression model was used to estimate the odds of reporting cost-related non-adherence to prescription drugs.

### Drug Insurance Coverage

Respondents without drug insurance had higher odds of reporting cost-related non-adherence than respondents with drug insurance.

* Odds Ratio: 2.95
* 95% Confidence Interval: 2.74–3.18
* p < 0.001

### Household Income

Higher household income was generally associated with lower odds of cost-related non-adherence compared with the lowest-income group.

For respondents in households earning $80,000 or more:

* Odds Ratio: 0.51
* 95% Confidence Interval: 0.44–0.61
* p < 0.001

### Perceived Health

There was a gradient between perceived health status and cost-related non-adherence. Compared with respondents reporting excellent health, those reporting poorer health had progressively higher odds of cost-related non-adherence.

For respondents reporting poor health:

* Odds Ratio: 3.67
* 95% Confidence Interval: 3.09–4.35
* p < 0.001

### Age

Respondents aged 65 years and older had lower odds of cost-related non-adherence compared with the 12–17 reference group.

* Odds Ratio: 0.32
* 95% Confidence Interval: 0.26–0.40
* p < 0.001

## 4. Data

Data source: 2022 Canadian Community Health Survey (CCHS) public-use microdata.

The analysis used variables relating to:

* Cost-related non-adherence to prescription drugs
* Household income
* Drug insurance coverage
* Age
* Sex
* Education
* Self-perceived health
* Diabetes
* High blood pressure

The raw CCHS microdata are not included in this repository. Users wishing to reproduce the analysis should obtain the corresponding CCHS 2022 public-use dataset and update the file path in `R/analysis.R` as required.

## 5. Methods

### Outcome

Cost-related non-adherence to prescription drugs was converted into a binary outcome:

* 1 = cost-related non-adherence
* 0 = no cost-related non-adherence

### Statistical Analysis

The analysis consisted of:

1. Variable selection and inspection
2. Recoding of categorical variables
3. Assessment of missing data
4. Descriptive analysis
5. Multivariable logistic regression

The logistic regression model estimated adjusted odds ratios with 95% confidence intervals.

## 6. Technical Implementation

**Language:** R

**Key packages:**

* `readr`
* `pdftools`
* `broom`

The complete analysis workflow is available in:

`R/analysis.R`

The regression results are available in:

`tables/logistic_regression_results.csv`

## 7. Limitations

The CCHS uses a complex survey design and provides survey weights. This portfolio analysis used a conventional logistic regression model rather than a survey-weighted regression approach.

Therefore, the results should be interpreted as associations observed within the analysed sample rather than as survey-weighted national estimates.

The analysis is also observational, so the reported associations should not be interpreted as evidence of causation.

## 8. Project Purpose

This project was developed as a portfolio example of applied quantitative health research, demonstrating the use of real-world health survey data to investigate healthcare access and medication-related outcomes.

The project reflects an interest in using quantitative methods and health data to generate evidence relevant to healthcare delivery, health outcomes, and health policy.
