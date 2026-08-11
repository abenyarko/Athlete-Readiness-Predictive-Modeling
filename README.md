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

## Data and Measures

### Athlete Wellness Survey

The Athlete Wellness Survey served as the primary source of subjective wellness data for this project. Athletes completed the survey to provide a snapshot of their current physical and psychological readiness.

The survey assessed:

- **Illness** – Current presence of illness-related symptoms
- **Sleep Quality** – Perceived quality and restfulness of sleep
- **Overall Stress** – Current academic, emotional, and athletic stress
- **Physical Fatigue** – Perceived level of physical fatigue
- **Motivation** – Current motivation to train
- **Soreness** – Overall perceived body soreness
- **Specific Muscle Soreness** – Location-specific soreness, tightness, or concern

The ordinal wellness responses were converted into numeric measures for analysis. These measures were used to calculate athlete-specific wellness indicators and standardized Z-scores.

Wellness data were subsequently integrated with objective neuromuscular performance measures, including **Countermovement Jump (CMJ)** and **Reactive Strength Index (RSI)**. Supervised machine-learning models were then used to investigate whether patterns in self-reported wellness could help explain or identify changes in objective performance.

### Survey Instrument

![Athlete Wellness Survey](https://github.com/abenyarko/Athlete-Readiness-Predictive-Modeling/blob/main/visuals/Wellness_Survey.png)

*Example of the wellness monitoring instrument used to collect subjective athlete-reported measures.*

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

## Technologies

- R
- caret
- randomForest
- pROC
- tidyverse
- dplyr
- ggplot2
- Logistic Regression
- Random Forest
- Cross-Validation
- ROC Analysis
- Git
- GitHub

## Skills Demonstrated

- 🤖 Supervised Machine Learning
- 📈 Predictive Modeling
- 🌲 Random Forest Modeling
- 📊 Logistic Regression
- 🧪 Train/Test Validation
- 🔁 Cross-Validation
- 🎯 Classification Threshold Optimization
- 📉 ROC & AUC Analysis
- ⚖️ Classification Model Evaluation
- 🔍 Feature Importance
- 🧹 Data Preparation & Feature Engineering
- 🏗 End-to-End Predictive Analytics Workflow

## Business Impact

This project extends athlete monitoring beyond descriptive wellness reporting by evaluating whether subjective wellness information can anticipate objective changes in physical performance.

A validated predictive framework could support practitioners by:

- Identifying athletes with elevated likelihood of performance suppression
- Prioritizing athletes for additional assessment
- Supporting individualized recovery decisions
- Combining subjective and objective monitoring data
- Moving athlete monitoring toward proactive decision support

The long-term objective is not to replace practitioner judgment, but to provide an additional evidence-based signal that supports earlier and more informed intervention.

## Lessons Learned

This project reinforced the distinction between explanatory and predictive modeling.

Predicting a composite Wellbeing Score using the same wellness indicators from which that score is calculated produces a largely predetermined relationship. Although those models may be useful for exploratory analyses and position-specific interaction testing, they are less informative as predictive models.

The predictive modeling framework therefore focuses on independent objective outcomes such as CMJ and RSI performance. This provides a more meaningful test of whether wellness information can generalize to external measures of athlete readiness.
