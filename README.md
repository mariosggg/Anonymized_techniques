---
title: "Anonymizing Patient Data with R and RStudio: A Step-by-Step Guide"
output:
  html_document:
    df_print: paged
---
  
 ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

With this age of big data and advanced analytics, it's more crucial than ever to maintain patient privacy. With policies like HIPAA (Health Insurance Portability and Accountability Act) and GDPR (General Data Protection Regulation) increasingly common, healthcare organizations must implement robust anonymization techniques to protect sensitive patient information while supporting research and data sharing.

In this tutorial, we'll learn to anonymize patient data using R and RStudio, with an eye toward best practices and practical usage. We'll cover anonymization techniques such as suppression, generalization, hashing, and more sophisticated methods such as k-anonymity and differential privacy.

## Why Anonymize Patient Data?

Anonymization is crucial for:

- **Compliance**: Meeting legal standards (e.g., HIPAA, GDPR).  
- **Data Sharing**: Enabling secure collaboration without compromising privacy.  
- **Research**: Supporting statistical analysis and machine learning while preserving confidentiality.

Requirements .

- **R** (version 4.0 or later).  
- **RStudio** (latest version)  
- **PostgreSQL database**: (optional for larger datasets).
- **Required Libraries** :

```r
install.packages(c("tidyverse", "digest", "anonymizer", "dbplyr"))
```

## Loading and Inspecting Patient Data

```{r load-libraries, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# Load necessary libraries
library(tidyverse)
library(knitr)
library(DT)
```


```{r}
# Load sample data
patient_data <- tibble(
  PatientID = 1:10,
  PatientGender = c("Male", "Female", "Female", "Male", "Male", "Female", "Male", "Female", "Male", "Female"),
  PatientAge = c(34, 29, 45, 50, 38, 42, 31, 46, 55, 37),
  Diagnosis = c("Hypertension", "Diabetes", "Asthma", "Cancer", "Hypertension", "Diabetes", "Asthma", "Cancer", "Hypertension", "Diabetes")
)
```


### Using `datatable` to print the data

```{r display-datatable}
# Display the table using datatable
datatable(patient_data)
```

## Anonymization Techniques

### a) Generalization

Generalization reduces the granularity of data, such as transforming ages into age groups.

```{r generalization}
# Generalize PatientAge into age groups
patient_data_generalized <- patient_data %>%
  mutate(AgeGroup = case_when(
    PatientAge < 40 ~ "<40",
    PatientAge >= 40 & PatientAge < 50 ~ "40-49",
    TRUE ~ "50+"
  )) %>%
  select(-PatientAge)

# Display generalized data
datatable(patient_data_generalized, options = list(pageLength = 5))
```


### f) Aggregation

Aggregation involves combining individual data records into summary statistics to prevent identification of individual records.

```{r aggregation}
# Aggregate data to show the count of patients per diagnosis
patient_data_aggregated <- patient_data %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

# Display aggregated data
datatable(patient_data_aggregated, options = list(pageLength = 5))
```

### a) Suppression

Suppression involves masking or removing specific values to prevent identification.

```{r suppression}
# Suppress PatientID by replacing it with '*'
patient_data_suppressed <- patient_data %>%
  mutate(PatientID = "***")

# Display suppressed data
datatable(patient_data_suppressed, options = list(pageLength = 5))
```


### f) Data Swapping

Data swapping exchanges values between records to maintain statistical properties while obscuring individual identities.

```{r data-swapping}
# Swap PatientGender between randomly selected records
set.seed(123)
patient_data_swapped <- patient_data %>%
  mutate(PatientGender = sample(PatientGender))

# Display swapped data
datatable(patient_data_swapped, options = list(pageLength = 5))
```

## g) k-Anonymity

k-Anonymity ensures that each record is indistinguishable from at least k-1 other records based on certain identifying attributes.

```{r k-anonymity}
# Apply k-anonymity by generalizing PatientAge and PatientGender
patient_data_k_anonymity <- patient_data %>%
  mutate(AgeGroup = case_when(
    PatientAge < 40 ~ "<40",
    PatientAge >= 40 & PatientAge < 50 ~ "40-49",
    TRUE ~ "50+"
  )) %>%
  group_by(AgeGroup, PatientGender) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  select(-PatientAge)

# Display k-anonymized data
datatable(patient_data_k_anonymity, options = list(pageLength = 5))
```

### h) l-Diversity

l-Diversity enhances k-anonymity by ensuring that each equivalence class has at least `l` well-represented sensitive values.

```{r l-diversity}
# Apply l-diversity by ensuring diversity of Diagnosis within each AgeGroup and PatientGender group
patient_data_l_diversity <- patient_data %>%
  mutate(AgeGroup = case_when(
    PatientAge < 40 ~ "<40",
    PatientAge >= 40 & PatientAge < 50 ~ "40-49",
    TRUE ~ "50+"
  )) %>%
  group_by(AgeGroup, PatientGender) %>%
  filter(n_distinct(Diagnosis) >= 2) %>%
  ungroup() %>%
  select(-PatientAge)

# Display l-diversified data
datatable(patient_data_l_diversity, options = list(pageLength = 5))
```

### i) t-Closeness

t-Closeness extends l-diversity by ensuring that the distribution of the sensitive attribute in any equivalence class is close to the distribution of the attribute in the overall table.

```{r t-closeness}
# Apply t-closeness by ensuring that the distribution of Diagnosis within each AgeGroup and PatientGender group is close to the distribution of Diagnosis in the overall dataset
overall_distribution <- patient_data %>%
  count(Diagnosis) %>%
  mutate(prop = n / sum(n))

patient_data_t_closeness <- patient_data %>%
  mutate(AgeGroup = case_when(
    PatientAge < 40 ~ "<40",
    PatientAge >= 40 & PatientAge < 50 ~ "40-49",
    TRUE ~ "50+"
  )) %>%
  group_by(AgeGroup, PatientGender) %>%
  filter(all(abs(as.numeric(prop.table(table(Diagnosis))) - overall_distribution$prop[match(names(prop.table(table(Diagnosis))), overall_distribution$Diagnosis)]) <= 0.2)) %>%
  ungroup() %>%
  select(-PatientAge)

# Display t-closeness data
datatable(patient_data_t_closeness, options = list(pageLength = 5))
```

### j) Differential Privacy

Differential privacy ensures that the removal or addition of a single data point does not significantly affect the outcome of any analysis, providing a strong guarantee of privacy.

```{r differential-privacy}
# Apply differential privacy by adding Laplace noise to the PatientAge
epsilon <- 1  # Privacy parameter
sensitivity <- 1  # Sensitivity of the query

add_laplace_noise <- function(x, epsilon, sensitivity) {
  x + rnorm(length(x), mean = 0, sd = sensitivity / epsilon)
}

patient_data_differential_privacy <- patient_data %>%
  mutate(PatientAge = add_laplace_noise(PatientAge, epsilon, sensitivity))

# Display differentially private data
datatable(patient_data_differential_privacy, options = list(pageLength = 5))
```
