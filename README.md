# 🤖 Athlete Readiness Predictive Modeling

<p align="center">

**Supervised Learning • Predictive Analytics • R • Random Forest • Logistic Regression**

</p>

Developed a supervised machine-learning framework integrating athlete wellness and objective performance data to predict CMJ and RSI outcomes and evaluate the early identification of performance suppression.

## Business Problem

Athlete wellness surveys provide valuable information about perceived stress, fatigue, soreness, sleep quality, and motivation, but subjective wellness scores alone do not indicate whether meaningful changes in physical performance will occur.

This project investigates whether longitudinal wellness indicators can predict objective performance outcomes and identify athletes experiencing meaningful suppression in countermovement jump (CMJ) and reactive strength index (RSI) performance.

The objective is to move athlete monitoring from descriptive reporting toward predictive decision support that may help practitioners identify emerging performance changes before they become more pronounced.

## Research Questions

This project evaluates three primary questions:

1. Can athlete wellness indicators predict objective CMJ and RSI performance?
2. Can wellness indicators classify meaningful CMJ or RSI suppression?
3. Which wellness indicators provide the greatest predictive value for athlete performance and readiness?

## Data

The modeling dataset integrates two longitudinal data sources:

- Athlete wellness survey data
- Objective jump-performance testing

### Wellness Predictors
- Overall Stress
- Physical Fatigue
- Soreness
- Sleep Quality
- Motivation
- Wellbeing Score
- Wellness Z-scores
- Player Position
- Illness Status

### Performance Outcomes
- Countermovement Jump (CMJ)
- Reactive Strength Index (RSI)
- CMJ Z-score
- RSI Z-score

All athlete identifiers used in the portfolio repository are anonymized.

## Methodology

### Data Preparation

- Cleaned and standardized wellness and jump-performance datasets
- Converted survey responses into ordered numeric indicators
- Evaluated missingness and baseline availability
- Integrated wellness and performance observations
- Removed incomplete performance observations
- Anonymized athlete identifiers

### Train/Test Design

Athletes were separated into training and testing datasets using an 80/20 athlete-level split. Keeping individual athletes entirely within one partition reduces the risk of information leakage caused by observations from the same athlete appearing in both training and testing data.

### Cross-Validation

Models were trained using 10-fold cross-validation on the training dataset.

### Supervised Learning

Two complementary modeling problems were evaluated:

#### Regression
Continuous CMJ and RSI performance were predicted using athlete wellness measures.

#### Classification
Performance suppression was classified at:

- -0.5 SD
- -1.0 SD
- -1.5 SD

for both CMJ and RSI.

Classification algorithms included:

- Logistic Regression
- Random Forest

### Model Evaluation

Models were evaluated using:

- Accuracy
- Sensitivity
- Specificity
- Precision
- F1 Score
- Balanced Accuracy
- ROC-AUC

Classification thresholds were also evaluated using Youden's J statistic to investigate whether alternative probability cutoffs improved sensitivity-specificity tradeoffs.


