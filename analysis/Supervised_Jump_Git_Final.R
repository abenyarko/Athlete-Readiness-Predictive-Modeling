

  
#####=====Install Libraries=====#####

invisible(lapply(c("dplyr","tidyverse","haven","readr","broom","survey","sampling",
                   "ggplot2","cluster","factoextra","boot","rmarkdown","knitr",
                   "scales","tinytex","stargazer","kableExtra","caret", "randomForest", "xgboost",
                   "nnet", "pROC", "corrplot", "lubridate"),
                 library, character.only = TRUE))

#####=====Load CSV files=====#####

raw_wellness_2526<- read_csv("wellness excel report - 5-16-26 1778880000738.csv") 
View(raw_wellness_2526)

raw_jump_2526 <- read_csv("forcedecks trials excel report 1778618271165.csv")
View(raw_jump_2526)

#####=====Count of rows and columns=====#####

nrow(raw_wellness_2526)
ncol(raw_wellness_2526)

nrow(raw_jump_2526)
ncol(raw_jump_2526)

#####=====Check for missing values=====#####

sum(is.na(raw_jump_2526))
sum(is.na(raw_wellness_2526))

summary(raw_wellness_2526)

#####=====Check for missing data in character variables=====#####

raw_wellness_2526 |>
  select(where(is.character)) |>
  summarise(across(everything(), list(
    n_na    = ~sum(is.na(.)),
    n_empty = ~sum(. == "", na.rm = TRUE)
  ))) |>
  tidyr::pivot_longer(everything(), names_to = c("column", ".value"), names_sep = "_n_") |>
  rename(n_na = na, n_empty = empty) |>
  mutate(total_missing = n_na + n_empty) |>
  arrange(desc(total_missing))

raw_jump_2526 |>
  select(where(is.character)) |>
  summarise(across(everything(), list(
    n_na    = ~sum(is.na(.)),
    n_empty = ~sum(. == "", na.rm = TRUE)
  ))) |>
  tidyr::pivot_longer(everything(), names_to = c("column", ".value"), names_sep = "_n_") |>
  rename(n_na = na, n_empty = empty) |>
  mutate(total_missing = n_na + n_empty) |>
  arrange(desc(total_missing))

#####=====Remove Column 28 and 29 from Raw Wellness=====#####

clean_wellness_2526 <- raw_wellness_2526 %>% select(-28, -29)

#####=====Remove Column 6 and 7 from Raw Jump=====#####

clean_jump_2526 <- raw_jump_2526 %>% select(-6, -7)



#####=====Impute missing character variables=====#####

#####=====The plan is to impute "Unknown" for missing illness character data=====#####a

clean_wellness_2526 <- clean_wellness_2526 |>
  mutate(Illness = replace_na(Illness, "Unknown"))

clean_wellness_2526 |>
  select(where(is.character)) |>
  summarise(across(everything(), list(
    n_na    = ~sum(is.na(.)),
    n_empty = ~sum(. == "", na.rm = TRUE)
  ))) |>
  tidyr::pivot_longer(everything(), names_to = c("column", ".value"), names_sep = "_n_") |>
  rename(n_na = na, n_empty = empty) |>
  mutate(total_missing = n_na + n_empty) |>
  arrange(desc(total_missing))

clean_wellness_2526 |> count(Illness)



#####=====Convert Date to Date format=====#####

str(clean_wellness_2526$Date)
str(clean_jump_2526$Date)


clean_jump_2526$Date <- as.Date(clean_jump_2526$Date, format = "%m/%d/%Y")

clean_wellness_2526 <- clean_wellness_2526 |>
  mutate(
    raw_date = raw_wellness_2526$Date,
    Date = case_when(
      !is.na(mdy(raw_date, quiet = TRUE)) & mdy(raw_date, quiet = TRUE) <= Sys.Date() ~ mdy(raw_date, quiet = TRUE),
      !is.na(dmy(raw_date, quiet = TRUE)) & dmy(raw_date, quiet = TRUE) <= Sys.Date() ~ dmy(raw_date, quiet = TRUE),
      TRUE ~ NA_Date_
    )
  ) |>
  select(-raw_date)
sum(is.na(clean_wellness_2526$Date))
sum(clean_wellness_2526$Date > Sys.Date(), na.rm = TRUE)

#####=====Check the structure of the Date columns=====#####

str(clean_wellness_2526$Date)

str(clean_jump_2526$Date)


#####=====Impute missing numeric variable=====#####

summary(raw_wellness_2526)
summary(clean_wellness_2526)

#The Z scores are the only numeric columns with missingness — the raw scores, means, and stddevs are complete. 
#This pattern makes sense if Z scores were computed relative to a rolling or group baseline that wasn't always available


z_cols <- c("Wellbeing Z Score", "Soreness Z Score", "Fatigue Z Score",
            "Stress Z Score", "Sleep Z Score", "Motivation Z Score")

clean_wellness_2526 |>
  mutate(any_z_na = if_any(all_of(z_cols), is.na)) |>
  count(About, any_z_na) |>
  tidyr::pivot_wider(names_from = any_z_na, values_from = n, values_fill = 0) |>
  rename(complete = `FALSE`, missing = `TRUE`) |>
  mutate(pct_missing = round(missing / (complete + missing) * 100, 1)) |>
  arrange(desc(pct_missing))

#The most likely explanation for the 100%-missing players is that Z scores require a minimum number of prior observations to compute a rolling baseline — these players may not have had enough history in the system.
# They have real raw scores. Imputing zero ignores that actual information. If those players are systematically different, you'd introduce a systematic bias rather than random noise.
#The existing Z scores were likely computed as rolling baselines, meaning a score on day 10 is compared to days 1–9. Computing from all of a player's data uses future observations to inform past Z scores. Depending on your modeling goal, that may or may not matter.

#####=====Check Observation counts for players with 100% missing z scores=====#####

fully_missing_players <- clean_wellness_2526 |>
  mutate(any_z_na = if_any(all_of(z_cols), is.na)) |>
  group_by(About) |>
  summarise(n_rows = n(), pct_missing = mean(any_z_na) * 100) |>
  filter(pct_missing == 100)

#####=====Impute missing character variables=====#####

#####=====The plan is to impute "Unknown" for missing illness character data=====#####a

clean_wellness_2526 <- clean_wellness_2526 |>
  mutate(Illness = replace_na(Illness, "Unknown"))

clean_wellness_2526 |>
  select(where(is.character)) |>
  summarise(across(everything(), list(
    n_na    = ~sum(is.na(.)),
    n_empty = ~sum(. == "", na.rm = TRUE)
  ))) |>
  tidyr::pivot_longer(everything(), names_to = c("column", ".value"), names_sep = "_n_") |>
  rename(n_na = na, n_empty = empty) |>
  mutate(total_missing = n_na + n_empty) |>
  arrange(desc(total_missing))

clean_wellness_2526 |> count(Illness)

# =========================================================
# CONVERT WELLNESS VARIABLES TO ORDERED FACTORS
# =========================================================

clean_wellness_2526 <- clean_wellness_2526 %>%
  mutate(
    
    `Sleep Quality` = factor(
      `Sleep Quality`,
      levels = c(
        "Unable to Sleep",
        "Restless, Woke Up 2x +",
        "Average",
        "Good, Feel Refreshed",
        "Excellent, Feel Very Refreshed"
      ),
      ordered = TRUE
    ),
    
    Soreness = factor(
      Soreness,
      levels = c(
        "Painful to Move",
        "Very Sore",
        "Moderate",
        "Minimal",
        "No Soreness"
      ),
      ordered = TRUE
    ),
    
    `Overall Stress` = factor(
      `Overall Stress`,
      levels = c(
        "Very High",
        "High",
        "Average",
        "Low",
        "Very Low"
      ),
      ordered = TRUE
    ),
    
    `Physical Fatigue` = factor(
      `Physical Fatigue`,
      levels = c(
        "Very High",
        "High",
        "Average",
        "Low",
        "Very Low"
      ),
      ordered = TRUE
    ),
    
    Motivation = factor(
      Motivation,
      levels = c(
        "Very Low",
        "Low",
        "Average",
        "High",
        "Very High"
      ),
      ordered = TRUE
    )
  )

# =========================================================
# VERIFY WELLNESS FACTOR CODING
# =========================================================

str(
  clean_wellness_2526 %>%
    select(
      `Sleep Quality`,
      Soreness,
      `Overall Stress`,
      `Physical Fatigue`,
      Motivation
    )
)

levels(clean_wellness_2526$`Sleep Quality`)

levels(clean_wellness_2526$Soreness)
levels(clean_wellness_2526$`Overall Stress`)
levels(clean_wellness_2526$`Physical Fatigue`)
levels(clean_wellness_2526$Motivation)

summary(
  clean_wellness_2526 %>%
    select(
      `Sleep Quality`,
      Soreness,
      `Overall Stress`,
      `Physical Fatigue`,
      Motivation
    )
)

#####=====Check the structure of the clean_wellness_2526 dataset=====#####

summary(clean_wellness_2526)

#Create a column for day of the week based on the Date column
clean_wellness_2526$Day_of_Week <- weekdays(clean_wellness_2526$Date)

str(clean_wellness_2526$Day_of_Week)

clean_wellness_2526

#####=====Convert categorical Wellbeing indicators into numeric variables=====#####

clean_wellness_2526 <- clean_wellness_2526 %>%
  mutate(
    Overall_Stress_Num = as.numeric(factor(`Overall Stress`, 
                                           levels = c("Very High", 
                                                      "High", 
                                                      "Average", 
                                                      "Low", 
                                                      "Very Low", ordered = TRUE))), 
    Physical_Fatigue_Num = as.numeric(factor(`Physical Fatigue`, 
                                             levels = c("Very High", 
                                                        "High", 
                                                        "Average", 
                                                        "Low", 
                                                        "Very Low", ordered = TRUE))), 
    Soreness_Num = as.numeric(factor(Soreness, 
                                     levels = c("Painful to Move",
                                                "Very Sore", 
                                                "Moderate", 
                                                "Minimal", 
                                                "No Soreness", ordered = TRUE))), 
    Sleep_Quality_Num = as.numeric(factor(`Sleep Quality`, 
                                          levels = c("Unable to Sleep", 
                                                     "Restless, Woke Up 2x +", 
                                                     "Average", 
                                                     "Good, Feel Refreshed", 
                                                     "Excellent, Feel Very Refreshed", ordered = TRUE))), 
    Motivation_Num = as.numeric(factor(Motivation, 
                                       levels = c("Very Low", 
                                                  "Low", 
                                                  "Average", 
                                                  "High", 
                                                  "Very High", ordered = TRUE)))
  )

summary(clean_wellness_2526)


#####=====Scale Wellness Indicators correctly=====#####y

clean_wellness_2526 <- clean_wellness_2526 |>
  mutate(across(
    ends_with("_Num"),
    ~floor((. - 1) / (5 - 1) * (10 - 1) + 1)
  ))

View(clean_wellness_2526)



#####=====Make a categorical variable for Wellbeing Score=====#####

clean_wellness_2526 <- clean_wellness_2526 %>%
  mutate(
    Wellness_Category = cut(`Wellbeing Score`,
                        breaks = quantile(`Wellbeing Score`, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
                        include.lowest = TRUE,
                        labels = c("Low", "Medium", "High"))
    
  )

#######=====Make Mlax Position a factor=====######

clean_wellness_2526$`Mlax Position` <- as.factor(clean_wellness_2526$`Mlax Position`)


######======Rename Mlax Position column======#####

clean_wellness_2526 <- clean_wellness_2526 %>%
  rename(Mlax_Position = `Mlax Position`)

######======Create table of Wellbeing Score by Mlax Position======#####

table(clean_wellness_2526$Wellness_Category, clean_wellness_2526$Mlax_Position)

######======Remove position A,M======#####

clean_wellness_2526 <- clean_wellness_2526 |>
  mutate(Mlax_Position = if_else(Mlax_Position == "A, M", "A", as.character(Mlax_Position)))



######======Make Mlax_Position is a factor with the correct levels======#####

clean_wellness_2526$Mlax_Position <- factor(clean_wellness_2526$Mlax_Position,
                                                levels = c("A", "OM", "DM", "D", "GK", "FO"))



######======Create a column for day of the week based on the Date column======#####

clean_wellness_2526$Day_of_Week <- weekdays(clean_wellness_2526$Date)

######======Make a column for the month based on the Date column======#####

clean_wellness_2526$Month <- format(clean_wellness_2526$Date
                                        , "%B")

######======Make a column for the year based on the date column======#####

clean_wellness_2526$Year <- format(clean_wellness_2526$Date
                                       , "%Y")

#####=====Check for missing values in Z score columns=====#####

clean_wellness_2526 |>
  summarise(across(all_of(z_cols), ~sum(is.na(.))))

z_raw_map <- c(
  "Wellbeing Z Score"  = "Wellbeing Score",
  "Soreness Z Score"   = "Soreness_Num",
  "Fatigue Z Score"    = "Physical_Fatigue_Num",
  "Stress Z Score"     = "Overall_Stress_Num",
  "Sleep Z Score"      = "Sleep_Quality_Num",
  "Motivation Z Score" = "Motivation_Num"
)

for (z_col in names(z_raw_map)) {
  raw_col <- z_raw_map[[z_col]]
  
  player_stats <- clean_wellness_2526 |>
    group_by(About) |>
    summarise(
      player_mean = mean(.data[[raw_col]], na.rm = TRUE),
      player_sd   = sd(.data[[raw_col]],   na.rm = TRUE),
      .groups = "drop"
    )
  
  clean_wellness_2526 <- clean_wellness_2526 |>
    left_join(player_stats, by = "About") |>
    mutate(
      !!z_col := if_else(
        is.na(.data[[z_col]]),
        (.data[[raw_col]] - player_mean) / player_sd,
        .data[[z_col]]
      )
    ) |>
    select(-player_mean, -player_sd)
}

clean_wellness_2526 <- clean_wellness_2526 |>
  mutate(across(all_of(names(z_raw_map)), ~if_else(is.nan(.), 0, .)))

clean_wellness_2526 |>
  summarise(across(all_of(names(z_raw_map)), ~sum(is.na(.))))

####======Create Week_ID with Tuesday as the start of the week====#####

clean_jump_2526_wide <- clean_jump_2526 %>%
  
  # Create rep number within each athlete/date
  group_by(Date, `Last First Name`) %>%
  mutate(Rep = row_number()) %>%
  ungroup() %>%
  
  # Reshape to wide format
  pivot_wider(
    id_cols = c(Date, About, `Last First Name`),
    names_from = Rep,
    values_from = c(`Jump Height (Imp-Mom) in Inches`, `RSI-Mod Imp-Mom`),
    names_glue = "{.value}_Rep{Rep}"
  )
head(clean_jump_2526_wide)

clean_jump_2526_wide <- clean_jump_2526_wide |>
  select(-3, -6, -7, -10, -11)

#####=====Create a column for day of the week based on the Date column=====#####

clean_jump_2526_wide$Day_of_Week <- weekdays(clean_jump_2526_wide$Date)

#####=====Make a column for the month based on the Date column=====#####

clean_jump_2526_wide$Month <- format(clean_jump_2526_wide$Date
                                    , "%B")

#####=====Make a column for the year based on the date column=====#####

clean_jump_2526_wide$Year <- format(clean_jump_2526_wide$Date
                                   , "%Y")
#####=====Distinct responses each year=====#####

clean_wellness_2526 %>%
  count(Year)

# Distinct athlete-date survey responses by year

clean_wellness_2526 %>%
  distinct(About, Date, Year) %>%
  count(Year)
unique(clean_wellness_2526$Year)

clean_wellness_2526 %>%
  group_by(Year) %>%
  summarise(
    Responses = n(),
    Unique_Athletes = n_distinct(About)
  )
#####=====Summarize Jump data with Mean, Max, SD=====#####

summary(clean_jump_2526_wide)

clean_jump_2526_wide <- clean_jump_2526_wide %>%
  
  mutate(
    
    # ----------------------
    # JUMP HEIGHT METRICS
    # ----------------------
    
    JumpHeight_Mean = rowMeans(
      select(., starts_with("Jump Height")),
      na.rm = TRUE
    ),
    
    JumpHeight_Max = pmax(
      `Jump Height (Imp-Mom) in Inches_Rep1`,
      `Jump Height (Imp-Mom) in Inches_Rep2`,
      na.rm = TRUE
    ),
    
    JumpHeight_SD = apply(
      select(., starts_with("Jump Height")),
      1,
      sd,
      na.rm = TRUE
    ),
    
    
    # ----------------------
    # RSI METRICS
    # ----------------------
    
    RSI_Mean = rowMeans(
      select(., starts_with("RSI-Mod")),
      na.rm = TRUE
    ),
    
    RSI_Max = pmax(
      `RSI-Mod Imp-Mom_Rep1`,
      `RSI-Mod Imp-Mom_Rep2`,
      na.rm = TRUE
    ),
    
    RSI_SD = apply(
      select(., starts_with("RSI-Mod")),
      1,
      sd,
      na.rm = TRUE
    )
    
  )

#######=====Calculate Z scores and Percent Change for Jump Height and RSI=====#####

clean_jump_2526_wide <- clean_jump_2526_wide %>%
  
  group_by(`About`) %>%
  
  mutate(
    
    JumpHeight_Baseline_Mean =
      mean(JumpHeight_Mean, na.rm = TRUE),
    
    JumpHeight_Baseline_SD =
      sd(JumpHeight_Mean, na.rm = TRUE),
    
    JumpHeight_Zscore =
      (JumpHeight_Mean - JumpHeight_Baseline_Mean) /
      JumpHeight_Baseline_SD,
    
    JumpHeight_Pct_Change =
      ((JumpHeight_Mean - JumpHeight_Baseline_Mean) /
         JumpHeight_Baseline_Mean) * 100
  ) %>%
  
  ungroup()


clean_jump_2526_wide <- clean_jump_2526_wide %>%
  
  group_by(`About`) %>%
  
  mutate(
    
    RSI_Baseline_Mean =
      mean(RSI_Mean, na.rm = TRUE),
    
    RSI_Baseline_SD =
      sd(RSI_Mean, na.rm = TRUE),
    
    RSI_Zscore =
      (RSI_Mean - RSI_Baseline_Mean) /
      RSI_Baseline_SD,
    
    RSI_Pct_Change =
      ((RSI_Mean - RSI_Baseline_Mean) /
         RSI_Baseline_Mean) * 100
  ) %>%
  
  ungroup()

View(clean_jump_2526_wide)

summary(clean_jump_2526_wide)

sum(is.na(clean_jump_2526_wide))

######=====Rename About column to Name======#####

clean_jump_2526_wide <- clean_jump_2526_wide |>
  rename(Name = About)

clean_wellness_2526 <- clean_wellness_2526 |>
  rename(Name = About)

######=====Create WeekID Column=====#####

make_2026_weekid <- function(date_var) {
  date_var <- as.Date(date_var)
  
  first_start <- as.Date("2026-02-04") # Wednesday start
  second_start <- as.Date("2026-02-10") # first Tuesday-based week
  
  case_when(
    year(date_var) != 2026 ~ NA_character_,
    
    date_var >= first_start & date_var < second_start ~ "WeekID-1-2026",
    
    date_var >= second_start ~ paste0(
      "WeekID-",
      floor(as.numeric(date_var - second_start) / 7) + 2,
      "-2026"
    ),
    
    TRUE ~ NA_character_
  )
}

######=====Apply the function to create WeekID column in both datasets=====

clean_wellness_2526 <- clean_wellness_2526 %>%
  mutate(
    WeekID = make_2026_weekid(Date)
  )

clean_jump_2526_wide <- clean_jump_2526_wide %>%
  mutate(
    WeekID = make_2026_weekid(Date)
  )

clean_wellness_2526 %>%
  select(Date, WeekID) %>%
  arrange(Date) %>%
  distinct() %>%
  print(n = 20)

#####=====Reorder Columns=====#####

head(clean_jump_2526_wide)
head((clean_wellness_2526))

clean_jump_2526_wide <- clean_jump_2526_wide %>%
  
  rename(
    
    CMJ_Rep1 = `Jump Height (Imp-Mom) in Inches_Rep1`,
    CMJ_Rep2 = `Jump Height (Imp-Mom) in Inches_Rep2`,

    CMJ_Mean = JumpHeight_Mean,
    CMJ_Max  = JumpHeight_Max,
    CMJ_SD   = JumpHeight_SD,
    
    CMJ_Baseline_Mean = JumpHeight_Baseline_Mean,
    CMJ_Baseline_SD   = JumpHeight_Baseline_SD,
    CMJ_Zscore        = JumpHeight_Zscore,
    CMJ_Pct_Change    = JumpHeight_Pct_Change,
    
  )

clean_jump_2526_wide <- clean_jump_2526_wide %>%
  
  rename(
    RSI_Rep1 = `RSI-Mod Imp-Mom_Rep1`,
    RSI_Rep2 = `RSI-Mod Imp-Mom_Rep2`,
    
  )

clean_jump_2526_wide <- clean_jump_2526_wide |>
  
  select(
    
    # ----------------------
    # IDENTIFIERS
    # ----------------------
    Date,
    Name,
    Day_of_Week,
    WeekID,
    Month,
    Year,
    
    # ----------------------
    # RAW REPS
    # ----------------------
    `CMJ_Rep1`,
    `CMJ_Rep2`,
    
    `RSI_Rep1`,
    `RSI_Rep2`,
    
    # ----------------------
    # SESSION METRICS
    # ----------------------
    CMJ_Mean,
    CMJ_Max,
    CMJ_SD,
    
    RSI_Mean,
    RSI_Max,
    RSI_SD,
    
    # ----------------------
    # LONGITUDINAL METRICS
    # ----------------------
    CMJ_Baseline_Mean,
    CMJ_Baseline_SD,
    CMJ_Zscore,
    CMJ_Pct_Change,
    
    RSI_Baseline_Mean,
    RSI_Baseline_SD,
    RSI_Zscore,
    RSI_Pct_Change
    
  )

clean_wellness_2526 <- clean_wellness_2526 |>
  
  select(
    
    # ----------------------
    # IDENTIFIERS
    # ----------------------
    Date,
    Name,
    Day_of_Week,
    WeekID,
    Month,
    Year,
    Mlax_Position,
    Illness,
    
    # ----------------------
    # RAW WELLNESS VARIABLES
    # ----------------------
    `Sleep Quality`,
    `Overall Stress`,
    `Physical Fatigue`,
    Motivation,
    Soreness,
    
    # ----------------------
    # WELLBEING SCORE
    # ----------------------
    `Wellbeing Score`,
    `Wellbeing Stddev`,
    `Wellbeing Z Score`,
    
    # ----------------------
    # SORENESS METRICS
    # ----------------------
    `Soreness Mean`,
    `Soreness Stddev`,
    `Soreness Z Score`,
    
    # ----------------------
    # FATIGUE METRICS
    # ----------------------
    `Fatigue Mean`,
    `Fatigue Stddev`,
    `Fatigue Z Score`,
    
    # ----------------------
    # STRESS METRICS
    # ----------------------
    `Stress Mean`,
    `Stress Stddev`,
    `Stress Z Score`,
    
    # ----------------------
    # SLEEP METRICS
    # ----------------------
    `Sleep Mean`,
    `Sleep Stddev`,
    `Sleep Z Score`,
    
    # ----------------------
    # MOTIVATION METRICS
    # ----------------------
    `Motivation Mean`,
    `Motivation Stddev`,
    `Motivation Z Score`,
    
    # ----------------------
    # NUMERIC ML VARIABLES
    # ----------------------
    Overall_Stress_Num,
    Physical_Fatigue_Num,
    Soreness_Num,
    Sleep_Quality_Num,
    Motivation_Num
    
  )

#####=====Check for NAs=====##### 

# =========================
# FUNCTION TO CHECK NAs
# =========================

check_na_summary <- function(df, df_name) {
  
  numeric_na <- df %>%
    summarise(across(
      where(is.numeric),
      ~ sum(is.na(.))
    )) %>%
    pivot_longer(
      everything(),
      names_to = "Variable",
      values_to = "NA_Count"
    ) %>%
    filter(NA_Count > 0) %>%
    arrange(desc(NA_Count)) %>%
    mutate(Type = "Numeric")
  
  character_na <- df %>%
    summarise(across(
      where(~ is.character(.) | is.factor(.)),
      ~ sum(is.na(.) | . == "")
    )) %>%
    pivot_longer(
      everything(),
      names_to = "Variable",
      values_to = "NA_Count"
    ) %>%
    filter(NA_Count > 0) %>%
    arrange(desc(NA_Count)) %>%
    mutate(Type = "Character/Factor")
  
  bind_rows(numeric_na, character_na) %>%
    mutate(DataFrame = df_name) %>%
    select(DataFrame, Type, Variable, NA_Count)
  
}

# =========================
# CHECK BOTH DATA FRAMES
# =========================

wellness_na_summary <- check_na_summary(
  clean_wellness_2526,
  "clean_wellness_2526"
)

jump_na_summary <- check_na_summary(
  clean_jump_2526_wide,
  "clean_jump_2526_wide"
)

# =========================
# VIEW RESULTS
# =========================

wellness_na_summary
jump_na_summary

# Count NA values in Mlax_Position
sum(is.na(clean_wellness_2526$Mlax_Position))

# View rows with missing Mlax_Position
clean_wellness_2526 %>%
  filter(is.na(Mlax_Position))

# Optional: check frequency table including NAs
table(clean_wellness_2526$Mlax_Position, useNA = "ifany")




# ---------------------------------------------------------
# DATAFRAME FOR STATISTICAL INFERENCE / RESEARCH ANALYSIS
# ---------------------------------------------------------
# This dataframe preserves legitimate NA values for session
# standard deviation (SD) metrics when only one jump rep
# was completed during a session.
#
# Rationale:
# A standard deviation cannot be mathematically computed
# from a single observation, so these NAs are retained to
# preserve statistical validity and transparency.
#
# Rep-level missing values are also preserved because a
# missing second rep represents an unperformed jump, not
# a jump height or RSI value of 0.
#
# This version of the data should be used for:
# - Survey-weighted analyses
# - Regression inference
# - Confidence intervals
# - Hypothesis testing
# - Descriptive statistical reporting
# - Publication-quality analyses
#
# Keeping these NAs ensures that variability measures are
# not artificially fabricated during inferential analyses.
# ---------------------------------------------------------

clean_jump_2526_inference <- clean_jump_2526_wide

# ---------------------------------------------------------
# DATAFRAME FOR MACHINE LEARNING / PREDICTIVE MODELING
# ---------------------------------------------------------
# This dataframe replaces ML-problematic NA values with 0
# for SD and Z-score metrics while preserving legitimate
# missing rep values.
#
# Rationale:
# Most machine learning algorithms (Random Forest,
# XGBoost, caret workflows, clustering methods, etc.)
# require complete feature matrices and do not handle
# missing predictor values automatically.
#
# Source of the NA values:
#
# 1. Single-Rep Sessions
# A small number of athlete sessions only included one
# completed jump rep instead of two. Because standard
# deviation (SD) cannot be mathematically computed from a
# single observation, JumpHeight_SD and RSI_SD were
# originally returned as NA.
#
# For machine learning purposes, these SD values are
# replaced with 0 to represent no observed within-session
# variability rather than missing information.
#
# IMPORTANT:
# Rep-level NA values are intentionally preserved.
# A missing second rep indicates the athlete only
# completed one jump and should NOT be interpreted as
# a performance value of 0.
#
# 2. Single-Session Athletes
# A small number of athletes only appeared once in the
# dataset across the season. Since baseline SD cannot be
# computed from a single session, their Z-scores were
# originally returned as NA.
#
# For predictive modeling compatibility, undefined athlete-specific Z-scores from single-session athletes are operationally imputed to 0, representing no observed deviation from the available athlete-specific reference. 
#This is a modeling assumption and should not be interpreted as evidence that the athlete was physiologically at baseline.
#
# This version of the data should be used for:
# - Machine Learning
#
# These imputations are operationally reasonable for
# predictive modeling while ensuring model compatibility.
# ---------------------------------------------------------

clean_jump_2526_ml <- clean_jump_2526_wide |>
  
#####======Keep only WeekID-1-2026 through WeekID-11-2026======#####

  filter(
    WeekID %in% paste0("WeekID-", 1:11, "-2026")
  ) |>
  
  mutate(
    
    CMJ_Zscore = if_else(
      is.na(CMJ_Zscore),
      0,CMJ_Zscore
    ),
    
    RSI_Zscore = if_else(
      is.na(RSI_Zscore),
      0,
      RSI_Zscore
    ),
    
    CMJ_SD = if_else(
      is.na(CMJ_SD),
      0,
      CMJ_SD
    ),
    
    RSI_SD = if_else(
      is.na(RSI_SD),
      0,
      RSI_SD
    )
    
  ) |>
  
#####=====Round all numeric variables to 3 decimals=====#####
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )


# Machine Learning data frame for Wellness
# =========================================================
# CLEAN WELLNESS ML DATASET
# KEEP ONLY THE DAY THAT STARTS THE WEEKID
# (Tuesday for WeekID-2+ ; Wednesday for WeekID-1)
# =========================================================

clean_wellness_2526_ml <- clean_wellness_2526 |>
  
  # Keep only WeekID-1-2026 through WeekID-11-2026
  filter(
    WeekID %in% paste0("WeekID-", 1:11, "-2026")
  ) |>
  
  # Keep only the day that starts each WeekID
  filter(
    (WeekID == "WeekID-1-2026" & weekdays(Date) == "Wednesday") |
      (WeekID != "WeekID-1-2026" & weekdays(Date) == "Tuesday")
  ) |>
  
  # Round all numeric variables to 3 decimals
  mutate(
    across(
      where(is.numeric),
      ~ round(., 3)
    )
  )

#####=====Check for Duplicates=====#####

clean_wellness_2526_ml %>%
  count(Name, WeekID) %>%
  filter(n > 1)

clean_jump_2526_ml %>%
  count(Name, WeekID) %>%
  filter(n > 1)

#####=====Check for Duplicates in Jump Data=====#####


clean_wellness_2526_ml <- clean_wellness_2526_ml %>%
  distinct()



clean_wellness_2526_ml <- clean_wellness_2526_ml %>%
  group_by(Name, WeekID) %>%
  slice(1) %>%
  ungroup()

# =========================================================
# LINK WELLNESS + JUMP DATASETS
# =========================================================

combined_ml_data <- clean_wellness_2526_ml %>%
  
  left_join(
    clean_jump_2526_ml,
    by = c("Name", "WeekID"),
    suffix = c("_survey", "_jump")
  )

# =========================================================
# CHECK LINKAGE QUALITY
# =========================================================

# Missing jump outcomes after join
combined_ml_data %>%
  summarise(
    missing_cmj = sum(is.na(CMJ_Mean)),
    missing_rsi = sum(is.na(RSI_Mean))
  )

# Check duplicate athlete-weeks
combined_ml_data %>%
  count(Name, WeekID) %>%
  filter(n > 1)

# =========================================================
# CORRECT MISSING PLAYER POSITIONS
# =========================================================

# =========================================================
# POSITION DATA QUALITY
# =========================================================
# Missing position values were reconciled against verified
# roster information during private preprocessing.
#
# Personally identifiable athlete information used during
# data reconciliation has been removed from the public
# analysis workflow.
# =========================================================


combined_ml_data %>%
  filter(is.na(Mlax_Position)) %>%
  count(Name)

# =========================================================
# CREATE FINAL MODELING DATASET
# (complete cases for supervised learning)
# =========================================================

Survey_Jump_supervised_model_data <- combined_ml_data %>%
  
  filter(
    !is.na(CMJ_Mean),
    !is.na(RSI_Mean)
  )

View(Survey_Jump_supervised_model_data)

# =========================================================
# ANONYMIZE PLAYER NAMES
# survey_jump_supervised_model_data
# =========================================================
# ---------------------------------------------------------
# 1) Create temporary Player_ID
# ---------------------------------------------------------

Survey_Jump_supervised_model_data <- Survey_Jump_supervised_model_data %>%
  mutate(Player_ID = row_number())

# ---------------------------------------------------------
# 2) Give each Name the same Player_ID
# ---------------------------------------------------------

Survey_Jump_supervised_model_data <- Survey_Jump_supervised_model_data %>%
  group_by(Name) %>%
  mutate(Player_ID = first(Player_ID)) %>%
  ungroup()



# ---------------------------------------------------------
# 3) Anonymize Names
# ---------------------------------------------------------

Survey_Jump_supervised_model_data <- Survey_Jump_supervised_model_data %>%
  mutate(
    Name = paste0("Player_", Player_ID)
  ) %>%
  select(-Player_ID)

Survey_Jump_supervised_model_data <-
  Survey_Jump_supervised_model_data %>%
  rename(Player_ID = Name)

# ---------------------------------------------------------
# 4) View Results
# ---------------------------------------------------------



View(Survey_Jump_supervised_model_data)

head(Survey_Jump_supervised_model_data)

summary(Survey_Jump_supervised_model_data)




# =========================================================
# MODELING DATA
# =========================================================

supervised_model_data <- Survey_Jump_supervised_model_data

supervised_model_data <- Survey_Jump_supervised_model_data %>%
  mutate(
    Illness = factor(Illness),
    Mlax_Position = factor(
      Mlax_Position,
      levels = c("A", "OM", "DM", "D", "GK", "FO")
    )
  )

# =========================================================
# TRAIN / TEST SPLIT
# =========================================================

# =========================================================
# ATHLETE-LEVEL TRAIN / TEST SPLIT
# Used to evaluate generalization to unseen athletes
# =========================================================
set.seed(701)

players <- unique(supervised_model_data$Player_ID)

train_players <- sample(
  players,
  size = round(length(players) * 0.80)
)

train_data <- supervised_model_data %>%
  filter(Player_ID %in% train_players)

test_data <- supervised_model_data %>%
  filter(!Player_ID %in% train_players)
#################################################

# =========================================================
# CROSS VALIDATION
# =========================================================

train_control <- trainControl(
  method = "cv",
  number = 10
)

# MODEL NAMING CONVENTION
# P suffix = Mlax_Position included as predictor
# No P suffix = Mlax_Position excluded

# =========================================================
# MODEL A
# Wellbeing Score → CMJ Mean
# =========================================================

cmj_model_WellnessP <- train(
  CMJ_Mean ~
    `Wellbeing Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)



cmj_model_WellnessP 
summary(cmj_model_WellnessP$finalModel)
tidy(cmj_model_WellnessP$finalModel)
glance(cmj_model_WellnessP$finalModel)
varImp(cmj_model_WellnessP)

# =========================================================
# MODEL B
# Wellness Components → CMJ Mean
# =========================================================

cmj_model_Wellness_IndicatorsP <- train(
  CMJ_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

cmj_model_Wellness_IndicatorsP
summary(cmj_model_Wellness_IndicatorsP$finalModel)
tidy(cmj_model_Wellness_IndicatorsP$finalModel)
glance(cmj_model_Wellness_IndicatorsP$finalModel)
varImp(cmj_model_Wellness_IndicatorsP)


# =========================================================
# MODEL C
# Wellbeing Z Score → CMJ Z Score
# =========================================================

cmj_model_Wellness_ZP <- train(
  CMJ_Zscore ~
    `Wellbeing Z Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)   

cmj_model_Wellness_ZP
summary(cmj_model_Wellness_ZP$finalModel)
tidy(cmj_model_Wellness_ZP$finalModel)
glance(cmj_model_Wellness_ZP$finalModel)
varImp(cmj_model_Wellness_ZP)


# =========================================================
# MODEL D
# Wellness Z Components → CMJ Z Score
# =========================================================

cmj_model_Wellness_Indicators_ZP <- train(
  CMJ_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

cmj_model_Wellness_Indicators_ZP
summary(cmj_model_Wellness_Indicators_ZP$finalModel)
tidy(cmj_model_Wellness_Indicators_ZP$finalModel)
glance(cmj_model_Wellness_Indicators_ZP$finalModel)
varImp(cmj_model_Wellness_Indicators_ZP)

# =========================================================
# RSI MODELS
# =========================================================

# =========================================================
# MODEL A
# Wellbeing Score → RSI mean
# =========================================================
rsi_model_WellnessP <- train(
  RSI_Mean ~
    `Wellbeing Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_WellnessP
summary(rsi_model_WellnessP$finalModel)
tidy(rsi_model_WellnessP$finalModel)
glance(rsi_model_WellnessP$finalModel)
varImp(rsi_model_WellnessP)

# =========================================================
# MODEL B
# Wellness Components → RSI Mean
# =========================================================
rsi_model_Wellness_IndicatorsP <- train(
  RSI_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness_IndicatorsP
summary(rsi_model_Wellness_IndicatorsP$finalModel)
tidy(rsi_model_Wellness_IndicatorsP$finalModel)
glance(rsi_model_Wellness_IndicatorsP$finalModel)
varImp(rsi_model_Wellness_IndicatorsP) 


# =========================================================
# MODEL C
# Wellbeing Z Score → RSI Z Score
# =========================================================

rsi_model_Wellness_ZP <- train(
  RSI_Zscore ~
    `Wellbeing Z Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness_ZP
summary(rsi_model_Wellness_ZP$finalModel)
tidy(rsi_model_Wellness_ZP$finalModel)
glance(rsi_model_Wellness_ZP$finalModel)
varImp(rsi_model_Wellness_ZP)

# =========================================================
# MODEL D
# Wellness Z Components → RSI Z Score
# =========================================================

rsi_model_Wellness_Indicators_ZP <- train(
  RSI_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness_Indicators_ZP
summary(rsi_model_Wellness_Indicators_ZP$finalModel)
tidy(rsi_model_Wellness_Indicators_ZP$finalModel)
glance(rsi_model_Wellness_Indicators_ZP$finalModel)
varImp(rsi_model_Wellness_Indicators_ZP)


# =========================================================
# MODEL 
# Wellness Components → CMJ Mean - Random Forest
# =========================================================


cmj_rf_Wellness_IndicatorsP <- train(
  CMJ_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(cmj_rf_Wellness_IndicatorsP)

# =========================================================
# MODEL 
# Wellness Z Components → CMJ Z Score - Random Forest
# =========================================================

cmj_rf_Wellness_Indicators_ZP <- train(
  CMJ_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(cmj_rf_Wellness_Indicators_ZP)

# =========================================================
# MODEL 
# Wellness Components → RSI Mean - Random Forest
# =========================================================

rsi_rf_Wellness_IndicatorsP <- train(
  RSI_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(rsi_rf_Wellness_IndicatorsP)


# =========================================================
# MODEL 
# Wellness Z Components → RSI Z Score - Random Forest
# =========================================================

rsi_rf_Wellness_Indicators_ZP <- train(
  RSI_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Mlax_Position +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(rsi_rf_Wellness_Indicators_ZP)


#######=====Check Random Forest Model Resamples=====#####

resamples(list(
  CMJ_Raw_RF = cmj_rf_Wellness_IndicatorsP,
  CMJ_Z_RF   = cmj_rf_Wellness_Indicators_ZP,
  RSI_Raw_RF = rsi_rf_Wellness_IndicatorsP,
  RSI_Z_RF   = rsi_rf_Wellness_Indicators_ZP
)) %>%
  summary()


####=====Remove MLAX Position from the ML models=====######

# =========================================================
# MODEL A
# Wellbeing Score → CMJ Mean
# =========================================================

cmj_model_Wellness <- train(
  CMJ_Mean ~
    `Wellbeing Score` +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

cmj_model_Wellness 
summary(cmj_model_Wellness$finalModel)
tidy(cmj_model_Wellness$finalModel)
glance(cmj_model_Wellness$finalModel)
varImp(cmj_model_Wellness)

# =========================================================
# MODEL B
# Wellness Components → CMJ Mean
# =========================================================

cmj_model_Wellness_Indicators <- train(
  CMJ_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

cmj_model_Wellness_Indicators
summary(cmj_model_Wellness_Indicators$finalModel)
tidy(cmj_model_Wellness_Indicators$finalModel)
glance(cmj_model_Wellness_Indicators$finalModel)
varImp(cmj_model_Wellness_Indicators)


# =========================================================
# MODEL C
# Wellbeing Z Score → CMJ Z Score
# =========================================================

cmj_model_Wellness_Z <- train(
  CMJ_Zscore ~
    `Wellbeing Z Score` +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)   

cmj_model_Wellness_Z
summary(cmj_model_Wellness_Z$finalModel)
tidy(cmj_model_Wellness_Z$finalModel)
glance(cmj_model_Wellness_Z$finalModel)
varImp(cmj_model_Wellness_Z)


# =========================================================
# MODEL D
# Wellness Z Components → CMJ Z Score
# =========================================================

cmj_model_Wellness_Indicators_Z <- train(
  CMJ_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

cmj_model_Wellness_Indicators_Z
summary(cmj_model_Wellness_Indicators_Z$finalModel)
tidy(cmj_model_Wellness_Indicators_Z$finalModel)
glance(cmj_model_Wellness_Indicators_Z$finalModel)
varImp(cmj_model_Wellness_Indicators_Z)

# =========================================================
# RSI MODELS
# =========================================================

# =========================================================
# MODEL A
# Wellbeing Score → RSI mean
# =========================================================
rsi_model_Wellness <- train(
  RSI_Mean ~
    `Wellbeing Score` +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness
summary(rsi_model_Wellness$finalModel)
tidy(rsi_model_Wellness$finalModel)
glance(rsi_model_Wellness$finalModel)
varImp(rsi_model_Wellness)

# =========================================================
# MODEL B
# Wellness Components → RSI Mean
# =========================================================
rsi_model_Wellness_Indicators <- train(
  RSI_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness_Indicators
summary(rsi_model_Wellness_Indicators$finalModel)
tidy(rsi_model_Wellness_Indicators$finalModel)
glance(rsi_model_Wellness_Indicators$finalModel)
varImp(rsi_model_Wellness_Indicators) 


# =========================================================
# MODEL C
# Wellbeing Z Score → RSI Z Score
# =========================================================

rsi_model_Wellness_Z <- train(
  RSI_Zscore ~
    `Wellbeing Z Score` +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness_Z
summary(rsi_model_Wellness_Z$finalModel)
tidy(rsi_model_Wellness_Z$finalModel)
glance(rsi_model_Wellness_Z$finalModel)
varImp(rsi_model_Wellness_Z)

# =========================================================
# MODEL D
# Wellness Z Components → RSI Z Score
# =========================================================

rsi_model_Wellness_Indicators_Z <- train(
  RSI_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Illness,
  data = train_data,
  method = "lm",
  trControl = train_control
)

rsi_model_Wellness_Indicators_Z
summary(rsi_model_Wellness_Indicators_Z$finalModel)
tidy(rsi_model_Wellness_Indicators_Z$finalModel)
glance(rsi_model_Wellness_Indicators_Z$finalModel)
varImp(rsi_model_Wellness_Indicators_Z)

# =========================================================
# MODEL 
# Wellness Components → CMJ Mean - Random Forest
# =========================================================


cmj_rf_Wellness_Indicators <- train(
  CMJ_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(cmj_rf_Wellness_Indicators)

# =========================================================
# MODEL 
# Wellness Z Components → CMJ Z Score - Random Forest
# =========================================================

cmj_rf_Wellness_Indicators_Z <- train(
  CMJ_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(cmj_rf_Wellness_Indicators_Z)


# =========================================================
# MODEL 
# Wellness Components → RSI Mean - Random Forest
# =========================================================

rsi_rf_Wellness_Indicators <- train(
  RSI_Mean ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(rsi_rf_Wellness_Indicators)


# =========================================================
# MODEL 
# Wellness Z Components → RSI Z Score - Random Forest
# =========================================================

rsi_rf_Wellness_Indicators_Z <- train(
  RSI_Zscore ~
    `Stress Z Score` +
    `Fatigue Z Score` +
    `Soreness Z Score` +
    `Sleep Z Score` +
    `Motivation Z Score` +
    Illness,
  data = train_data,
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

varImp(rsi_rf_Wellness_Indicators_Z)





##########==========================================================#######


#Classification Model
#Wellness Survey-Based Classification of Below-Baseline Performance########
#We want to know whether Tuesday wellness survey data can classify athletes who are likely to perform meaningfully below their normal CMJ or RSI baseline.
#Suppression was operationally defined as performance at or below specified athlete-standardized thresholds (−0.5, −1.0, and −1.5 SD). The term represents below-baseline performance and should not be interpreted as a clinical diagnosis.

# =========================================================
# 1. CREATE CLASSIFICATION OUTCOMES
# =========================================================

classification_data <- supervised_model_data %>%
  mutate(
    
    Suppressed_CMJ_05 =
      factor(
        if_else(CMJ_Zscore <= -0.5, "Yes", "No"),
        levels = c("No", "Yes")
      ),
    
    Suppressed_CMJ_10 =
      factor(
        if_else(CMJ_Zscore <= -1.0, "Yes", "No"),
        levels = c("No", "Yes")
      ),
    
    Suppressed_CMJ_15 =
      factor(
        if_else(CMJ_Zscore <= -1.5, "Yes", "No"),
        levels = c("No", "Yes")
      ),
    
    Suppressed_RSI_05 =
      factor(
        if_else(RSI_Zscore <= -0.5, "Yes", "No"),
        levels = c("No", "Yes")
      ),
    
    Suppressed_RSI_10 =
      factor(
        if_else(RSI_Zscore <= -1.0, "Yes", "No"),
        levels = c("No", "Yes")
      ),
    
    Suppressed_RSI_15 =
      factor(
        if_else(RSI_Zscore <= -1.5, "Yes", "No"),
        levels = c("No", "Yes")
      )
    
  )

table(classification_data$Suppressed_CMJ_05)
table(classification_data$Suppressed_CMJ_10)
table(classification_data$Suppressed_CMJ_15)

table(classification_data$Suppressed_RSI_05)
table(classification_data$Suppressed_RSI_10)
table(classification_data$Suppressed_RSI_15)

prop.table(table(classification_data$Suppressed_CMJ_05))
prop.table(table(classification_data$Suppressed_CMJ_10))
prop.table(table(classification_data$Suppressed_CMJ_15))

prop.table(table(classification_data$Suppressed_RSI_05))
prop.table(table(classification_data$Suppressed_RSI_10))
prop.table(table(classification_data$Suppressed_RSI_15))

# =========================================================
# 3. ATHLETE-LEVEL TRAIN / TEST SPLIT
# =========================================================

set.seed(701)

players <- unique(classification_data$Player_ID)

train_players <- sample(
  players,
  size = round(length(players) * 0.80)
)

train_data <- classification_data %>%
  filter(Player_ID %in% train_players)

test_data <- classification_data %>%
  filter(!Player_ID %in% train_players)

# =========================================================
# 4. CLASSIFICATION CONTROL
# =========================================================

class_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# =========================================================
# 5. FUNCTION TO BUILD CLASSIFICATION DATA
# =========================================================

make_class_data <- function(train_data, test_data, outcome_var) {
  
  train_set <- train_data %>%
    select(
      all_of(outcome_var),
      `Stress Z Score`,
      `Fatigue Z Score`,
      `Soreness Z Score`,
      `Sleep Z Score`,
      `Motivation Z Score`
    ) %>%
    drop_na()
  
  test_set <- test_data %>%
    select(
      all_of(outcome_var),
      `Stress Z Score`,
      `Fatigue Z Score`,
      `Soreness Z Score`,
      `Sleep Z Score`,
      `Motivation Z Score`
    ) %>%
    drop_na()
  
  names(train_set)[1] <- "Outcome"
  names(test_set)[1] <- "Outcome"
  
  list(
    train = train_set,
    test = test_set
  )
}

# =========================================================
# 6. FUNCTION TO RUN LOGISTIC + RANDOM FOREST
# =========================================================

run_class_models <- function(train_set, test_set, model_label) {
  
  set.seed(701)
  
  # -------------------------------------------------------
  # Logistic regression
  # -------------------------------------------------------
  
  log_model <- train(
    Outcome ~ .,
    data = train_set,
    method = "glm",
    family = "binomial",
    trControl = class_control,
    metric = "ROC"
  )
  
  # -------------------------------------------------------
  # Random forest
  # -------------------------------------------------------
  
  set.seed(701)
  
  rf_model <- train(
    Outcome ~ .,
    data = train_set,
    method = "rf",
    trControl = class_control,
    metric = "ROC",
    importance = TRUE
  )
  
  # -------------------------------------------------------
  # Class predictions
  # -------------------------------------------------------
  
  log_pred <- predict(
    log_model,
    newdata = test_set
  )
  
  rf_pred <- predict(
    rf_model,
    newdata = test_set
  )
  
  # -------------------------------------------------------
  # Probability predictions
  # -------------------------------------------------------
  
  log_prob <- predict(
    log_model,
    newdata = test_set,
    type = "prob"
  )
  
  rf_prob <- predict(
    rf_model,
    newdata = test_set,
    type = "prob"
  )
  
  # -------------------------------------------------------
  # Confusion matrices
  # -------------------------------------------------------
  
  log_confusion <- confusionMatrix(
    log_pred,
    test_set$Outcome,
    positive = "Yes"
  )
  
  rf_confusion <- confusionMatrix(
    rf_pred,
    test_set$Outcome,
    positive = "Yes"
  )
  
  # -------------------------------------------------------
  # Print model results
  # -------------------------------------------------------
  
  cat("\n=================================================\n")
  cat(model_label, "\n")
  cat("=================================================\n")
  
  cat("\n--- Logistic Regression ---\n")
  print(log_model)
  print(log_confusion)
  print(varImp(log_model))
  
  cat("\n--- Random Forest ---\n")
  print(rf_model)
  print(rf_confusion)
  print(varImp(rf_model))
  
  # -------------------------------------------------------
  # ROC and AUC
  # -------------------------------------------------------
  
  log_roc <- pROC::roc(
    response = test_set$Outcome,
    predictor = log_prob$Yes,
    levels = c("No", "Yes"),
    direction = "<",
    quiet = TRUE
  )
  
  rf_roc <- pROC::roc(
    response = test_set$Outcome,
    predictor = rf_prob$Yes,
    levels = c("No", "Yes"),
    direction = "<",
    quiet = TRUE
  )
  
  log_auc <- as.numeric(
    pROC::auc(log_roc)
  )
  
  rf_auc <- as.numeric(
    pROC::auc(rf_roc)
  )
  
  cat("\n--- ROC / AUC ---\n")
  cat("Logistic AUC:", round(log_auc, 3), "\n")
  cat("Random Forest AUC:", round(rf_auc, 3), "\n")
  
  # -------------------------------------------------------
  # Automatically plot combined ROC curve
  # -------------------------------------------------------
  
  plot(
    log_roc,
    legacy.axes = TRUE,
    lwd = 2,
    main = paste("ROC Curve:", model_label)
  )
  
  lines(
    rf_roc,
    lwd = 2,
    lty = 2
  )
  
  # Chance-performance reference line
  abline(
    a = 0,
    b = 1,
    lty = 3
  )
  
  legend(
    "bottomright",
    legend = c(
      paste(
        "Logistic Regression AUC =",
        round(log_auc, 3)
      ),
      paste(
        "Random Forest AUC =",
        round(rf_auc, 3)
      )
    ),
    lwd = 2,
    lty = c(1, 2),
    bty = "n"
  )
  
  # -------------------------------------------------------
  # Return all model objects and results
  # -------------------------------------------------------
  
  list(
    model_label = model_label,
    test_data = test_set,
    logistic_model = log_model,
    rf_model = rf_model,
    
    logistic_predictions = log_pred,
    rf_predictions = rf_pred,
    
    logistic_probabilities = log_prob,
    rf_probabilities = rf_prob,
    
    logistic_confusion = log_confusion,
    rf_confusion = rf_confusion,
    
    logistic_auc = log_auc,
    rf_auc = rf_auc,
    
    logistic_roc = log_roc,
    rf_roc = rf_roc
  )
}

# =========================================================
# 7. RUN MODELS: CMJ -0.5
# =========================================================

cmj_05_data <- make_class_data(
  train_data,
  test_data,
  "Suppressed_CMJ_05"
)

cmj_05_results <- run_class_models(
  cmj_05_data$train,
  cmj_05_data$test,
  "CMJ Suppression Threshold: -0.5 SD"
)

# =========================================================
# 8. RUN MODELS: CMJ -1.0
# =========================================================

cmj_10_data <- make_class_data(
  train_data,
  test_data,
  "Suppressed_CMJ_10"
)

cmj_10_results <- run_class_models(
  cmj_10_data$train,
  cmj_10_data$test,
  "CMJ Suppression Threshold: -1.0 SD"
)

# =========================================================
# CMJ -1.5
# =========================================================

cmj_15_data <- make_class_data(
  train_data,
  test_data,
  "Suppressed_CMJ_15"
)

cmj_15_results <- run_class_models(
  cmj_15_data$train,
  cmj_15_data$test,
  "CMJ Suppression Threshold: -1.5 SD"
)


# =========================================================
# 9. RUN MODELS: RSI -0.5
# =========================================================

rsi_05_data <- make_class_data(
  train_data,
  test_data,
  "Suppressed_RSI_05"
)

rsi_05_results <- run_class_models(
  rsi_05_data$train,
  rsi_05_data$test,
  "RSI Suppression Threshold: -0.5 SD"
)

# =========================================================
# 10. RUN MODELS: RSI -1.0
# =========================================================

rsi_10_data <- make_class_data(
  train_data,
  test_data,
  "Suppressed_RSI_10"
)

rsi_10_results <- run_class_models(
  rsi_10_data$train,
  rsi_10_data$test,
  "RSI Suppression Threshold: -1.0 SD"
)

# =========================================================
# RSI -1.5
# =========================================================

rsi_15_data <- make_class_data(
  train_data,
  test_data,
  "Suppressed_RSI_15"
)

rsi_15_results <- run_class_models(
  rsi_15_data$train,
  rsi_15_data$test,
  "RSI Suppression Threshold: -1.5 SD"
)


##### Investigating Stress Findings#####

summary(supervised_model_data$Overall_Stress_Num)

summary(supervised_model_data$Physical_Fatigue_Num)

summary(supervised_model_data$Soreness_Num)

summary(supervised_model_data$Sleep_Quality_Num)

summary(supervised_model_data$Motivation_Num)

table(supervised_model_data$Overall_Stress_Num) 

table(supervised_model_data$Physical_Fatigue_Num)

table(supervised_model_data$Soreness_Num)

table(supervised_model_data$Sleep_Quality_Num)

table(supervised_model_data$Motivation_Num)

cor(
  supervised_model_data %>%
    select(
      Overall_Stress_Num,
      Physical_Fatigue_Num,
      Soreness_Num,
      Sleep_Quality_Num,
      Motivation_Num
    ),
  use = "pairwise.complete.obs"
)

lm(
  CMJ_Zscore ~ Overall_Stress_Num,
  data = supervised_model_data
)

lm(
  CMJ_Zscore ~
    Overall_Stress_Num +
    Physical_Fatigue_Num +
    Soreness_Num +
    Sleep_Quality_Num +
    Motivation_Num,
  data = supervised_model_data
)

lm(
  RSI_Zscore ~ Overall_Stress_Num,
  data = supervised_model_data
)
summary(
  lm(
    RSI_Zscore ~
      Overall_Stress_Num +
      Physical_Fatigue_Num +
      Soreness_Num +
      Sleep_Quality_Num +
      Motivation_Num,
    data = supervised_model_data
  )
)


# =========================================================
# 1. CREATE PREDICTIONS
# =========================================================

cmjz_lm_pos_pred <- predict(
  cmj_model_Wellness_Indicators_ZP,
  newdata = test_data
)

cmjz_rf_pos_pred <- predict(
  cmj_rf_Wellness_Indicators_ZP,
  newdata = test_data
)

cmjz_lm_nopos_pred <- predict(
  cmj_model_Wellness_Indicators_Z,
  newdata = test_data
)

cmjz_rf_nopos_pred <- predict(
  cmj_rf_Wellness_Indicators_Z,
  newdata = test_data
)


# =========================================================
# 2. CREATE PLOT DATAFRAMES
# =========================================================

cmjz_lm_pos_plot <- data.frame(
  Actual = test_data$CMJ_Zscore,
  Predicted = cmjz_lm_pos_pred,
  Model = "LM Position",
  Position = test_data$Mlax_Position
)

cmjz_rf_pos_plot <- data.frame(
  Actual = test_data$CMJ_Zscore,
  Predicted = cmjz_rf_pos_pred,
  Model = "RF Position",
  Position = test_data$Mlax_Position
)

cmjz_lm_nopos_plot <- data.frame(
  Actual = test_data$CMJ_Zscore,
  Predicted = cmjz_lm_nopos_pred,
  Model = "LM No Position",
  Position = test_data$Mlax_Position
)

cmjz_rf_nopos_plot <- data.frame(
  Actual = test_data$CMJ_Zscore,
  Predicted = cmjz_rf_nopos_pred,
  Model = "RF No Position",
  Position = test_data$Mlax_Position
)

cmjz_all_plots <- bind_rows(
  cmjz_lm_pos_plot,
  cmjz_rf_pos_plot,
  cmjz_lm_nopos_plot,
  cmjz_rf_nopos_plot
) %>%
  mutate(
    Residual = Actual - Predicted
  )


# =========================================================
# 3. TEST-SET R-SQUARED VALUES
# =========================================================

cmjz_results <- cmjz_all_plots %>%
  group_by(Model) %>%
  summarise(
    Rsquared = cor(Actual, Predicted, use = "complete.obs")^2,
    RMSE = RMSE(Predicted, Actual),
    MAE = MAE(Predicted, Actual),
    .groups = "drop"
  ) %>%
  arrange(desc(Rsquared))

cmjz_results




#####=====Actual vs Predicted: All Models=====#####

ggplot(
  cmjz_all_plots,
  aes(
    x = Actual,
    y = Predicted
  )
) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~Model) +
  labs(
    title = "CMJ Z-score: Actual vs Predicted",
    x = "Actual CMJ Z-score",
    y = "Predicted CMJ Z-score"
  ) +
  theme_minimal()

#####=====Actual vs Predicted Colored by Position=====#####

ggplot(
  cmjz_all_plots,
  aes(
    x = Actual,
    y = Predicted,
    color = Position
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~Model) +
  labs(
    title = "CMJ Z-score: Actual vs Predicted by Position",
    x = "Actual CMJ Z-score",
    y = "Predicted CMJ Z-score",
    color = "Position"
  ) +
  theme_minimal()

#####=====Residual Plots=====#####

ggplot(
  cmjz_all_plots,
  aes(
    x = Predicted,
    y = Residual
  )
) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~Model) +
  labs(
    title = "CMJ Z-score Residual Plots",
    x = "Predicted CMJ Z-score",
    y = "Residual"
  ) +
  theme_minimal()

#####=====Model Comparison=====#####

ggplot(
  cmjz_results,
  aes(
    x = reorder(Model, Rsquared),
    y = Rsquared
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "CMJ Z-score Model Comparison",
    x = "",
    y = expression(R^2)
  ) +
  theme_minimal()



#####=====Add R² Labels to Comparison Plot=====#####

ggplot(
  cmjz_results,
  aes(
    x = reorder(Model, Rsquared),
    y = Rsquared
  )
) +
  geom_col() +
  geom_text(
    aes(label = round(Rsquared, 3)),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = "CMJ Z-score Model Comparison",
    x = "",
    y = expression(R^2)
  ) +
  theme_minimal()


cmjz_results



#####=====Create RSI predictions=====#####

# Linear Regression Position

rsiz_lm_pos_pred <- predict(
  rsi_model_Wellness_Indicators_ZP,
  newdata = test_data
)

# RF Position

rsiz_rf_pos_pred <- predict(
  rsi_rf_Wellness_Indicators_ZP,
  newdata = test_data
)

# Linear Regression No Position

rsiz_lm_nopos_pred <- predict(
  rsi_model_Wellness_Indicators_Z,
  newdata = test_data
)

# RF No Position

rsiz_rf_nopos_pred <- predict(
  rsi_rf_Wellness_Indicators_Z,
  newdata = test_data
)




#####=====Build Plot Dataframes=====#####

rsiz_lm_pos_plot <- data.frame(
  Actual = test_data$RSI_Zscore,
  Predicted = rsiz_lm_pos_pred,
  Model = "LM Position",
  Position = test_data$Mlax_Position
)

rsiz_rf_pos_plot <- data.frame(
  Actual = test_data$RSI_Zscore,
  Predicted = rsiz_rf_pos_pred,
  Model = "RF Position",
  Position = test_data$Mlax_Position
)

rsiz_lm_nopos_plot <- data.frame(
  Actual = test_data$RSI_Zscore,
  Predicted = rsiz_lm_nopos_pred,
  Model = "LM No Position",
  Position = test_data$Mlax_Position
)

rsiz_rf_nopos_plot <- data.frame(
  Actual = test_data$RSI_Zscore,
  Predicted = rsiz_rf_nopos_pred,
  Model = "RF No Position",
  Position = test_data$Mlax_Position
)

rsiz_all_plots <- bind_rows(
  rsiz_lm_pos_plot,
  rsiz_rf_pos_plot,
  rsiz_lm_nopos_plot,
  rsiz_rf_nopos_plot
) %>%
  mutate(
    Residual = Actual - Predicted
  )

#####=====Calculate RSI Test set Metrics=====#####

rsiz_results <- rsiz_all_plots %>%
  group_by(Model) %>%
  summarise(
    Rsquared = cor(
      Actual,
      Predicted,
      use = "complete.obs"
    )^2,
    
    RMSE = RMSE(
      Predicted,
      Actual
    ),
    
    MAE = MAE(
      Predicted,
      Actual
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(Rsquared))

rsiz_results



#####=====Actual vs Predicted=====d

ggplot(
  rsiz_all_plots,
  aes(
    Actual,
    Predicted
  )
) +
  geom_point(
    size = 3,
    alpha = .7
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~Model) +
  labs(
    title = "RSI Z-score: Actual vs Predicted",
    x = "Actual RSI Z-score",
    y = "Predicted RSI Z-score"
  ) +
  theme_minimal()



#####=====Actual vs Predicted Colored by Position=====#####

ggplot(
  rsiz_all_plots,
  aes(
    Actual,
    Predicted,
    color = Position
  )
) +
  geom_point(
    size = 3,
    alpha = .8
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~Model) +
  labs(
    title = "RSI Z-score: Actual vs Predicted by Position"
  ) +
  theme_minimal()


#####=====Residual Plots=====#####

ggplot(
  rsiz_all_plots,
  aes(
    Predicted,
    Residual
  )
) +
  geom_point(
    size = 3,
    alpha = .7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~Model) +
  labs(
    title = "RSI Z-score Residual Plots"
  ) +
  theme_minimal()

#####=====Model Comparison=====#####

ggplot(
  rsiz_results,
  aes(
    reorder(Model, Rsquared),
    Rsquared
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = round(Rsquared, 3)
    ),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = "RSI Z-score Model Comparison",
    x = "",
    y = expression(R^2)
  ) +
  theme_minimal()

#####=====Function to plot variable importance for random forest models=====#####

plot_variable_importance <- function(model, title){
  
  importance <- varImp(model)
  
  plot(
    importance,
    top = 5,
    main = title
  )
  
}

#####=====Variable Importance Plots=====#####

plot_variable_importance(
  cmj_rf_Wellness_IndicatorsP,
  "CMJ Mean Variable Importance"
)

plot_variable_importance(
  cmj_rf_Wellness_Indicators_ZP,
  "CMJ Z-score Variable Importance"
)

plot_variable_importance(
  rsi_rf_Wellness_Indicators_ZP,
  "RSI Z-score Variable Importance"
)

plot_variable_importance(
  cmj_05_results$rf_model,
  "CMJ Classification (-0.5 SD)"
)

plot_variable_importance(
  cmj_10_results$rf_model,
  "CMJ Classification (-1.0 SD)"
)

plot_variable_importance(
  cmj_15_results$rf_model,
  "CMJ Classification (-1.5 SD)"
)

plot_variable_importance(
  rsi_05_results$rf_model,
  "RSI Classification (-0.5 SD)"
)

plot_variable_importance(
  rsi_10_results$rf_model,
  "RSI Classification (-1.0 SD)"
)

plot_variable_importance(
  rsi_15_results$rf_model,
  "RSI Classification (-1.5 SD)"
)


#####========#######
# =========================================================
# CORRELATION MATRIX OF WELLNESS SURVEY COMPONENTS
# =========================================================

#####=======########


wellness_cor_matrix <- supervised_model_data %>% 
  select(
    Overall_Stress_Num,
    Physical_Fatigue_Num,
    Soreness_Num,  
    Sleep_Quality_Num,
    Motivation_Num
  ) %>%
  cor(
    use = "pairwise.complete.obs",
    method = "pearson"
  )

round(wellness_cor_matrix, 2)

colnames(wellness_cor_matrix) <- c(
  "Stress",
  "Fatigue",
  "Soreness",
  "Sleep Quality",
  "Motivation"
)

rownames(wellness_cor_matrix) <- c(
  "Stress",
  "Fatigue",
  "Soreness",
  "Sleep Quality",
  "Motivation"
)

corrplot(
  wellness_cor_matrix,
  method = "color",
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  addCoef.col = "black",
  addCoef.cex = 0.8,
  diag = FALSE,
  mar = c(0, 0, 2, 0),
  number.cex = 0.8,
  title = "Correlation Matrix: Wellness Survey Components"
)


cmj_suppression_heatmap_data <- classification_data %>%
  filter(
    !is.na(Mlax_Position),
    !is.na(Suppressed_CMJ_10)
  ) %>%
  group_by(Mlax_Position) %>%
  summarise(
    Total_Observations = n(),
    Suppressed_Observations = sum(Suppressed_CMJ_10 == "Yes"),
    Suppression_Percentage =
      100 * Suppressed_Observations / Total_Observations,
    .groups = "drop"
  )

cmj_suppression_heatmap_data

cmj_suppression_heatmap <- ggplot(
  cmj_suppression_heatmap_data,
  aes(
    x = "CMJ -1.0 SD",
    y = Mlax_Position,
    fill = Suppression_Percentage
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(
        round(Suppression_Percentage, 1),
        "%"
      )
    ),
    size = 4
  ) +
  scale_fill_gradient(
    low = "white",
    high = "darkred",
    limits = c(
      0,
      max(
        cmj_suppression_heatmap_data$Suppression_Percentage,
        na.rm = TRUE
      )
    )
  ) +
  labs(
    title = "CMJ Suppression Rate by Position",
    subtitle = "Suppression defined as CMJ Z-score ≤ -1.0 SD",
    x = NULL,
    y = "Position",
    fill = "Suppressed (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

cmj_suppression_heatmap

#######=====CMJ Suppression Heatmap by Position and Threshold=====#####


cmj_all_thresholds <- classification_data %>%
  select(
    Mlax_Position,
    Suppressed_CMJ_05,
    Suppressed_CMJ_10,
    Suppressed_CMJ_15
  ) %>%
  pivot_longer(
    cols = starts_with("Suppressed_CMJ"),
    names_to = "Threshold",
    values_to = "Suppressed"
  ) %>%
  filter(
    !is.na(Mlax_Position),
    !is.na(Suppressed)
  ) %>%
  mutate(
    Threshold = recode(
      Threshold,
      Suppressed_CMJ_05 = "-0.5 SD",
      Suppressed_CMJ_10 = "-1.0 SD",
      Suppressed_CMJ_15 = "-1.5 SD"
    ),
    Threshold = factor(
      Threshold,
      levels = c("-0.5 SD", "-1.0 SD", "-1.5 SD")
    )
  ) %>%
  group_by(
    Mlax_Position,
    Threshold
  ) %>%
  summarise(
    Total_Observations = n(),
    Suppressed_Observations = sum(Suppressed == "Yes"),
    Suppression_Percentage =
      100 * Suppressed_Observations / Total_Observations,
    .groups = "drop"
  )

cmj_all_threshold_heatmap <- ggplot(
  cmj_all_thresholds,
  aes(
    x = Threshold,
    y = Mlax_Position,
    fill = Suppression_Percentage
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(
        round(Suppression_Percentage, 1),
        "%"
      )
    ),
    size = 4
  ) +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  ) +
  labs(
    title = "CMJ Suppression Rate by Position and Threshold",
    x = "Suppression Threshold",
    y = "Position",
    fill = "Suppressed (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

cmj_all_threshold_heatmap

########=====RSI Suppression Heatmap by Position and Threshold=====######


rsi_all_thresholds <- classification_data %>%
  select(
    Mlax_Position,
    Suppressed_RSI_05,
    Suppressed_RSI_10,
    Suppressed_RSI_15
  ) %>%
  pivot_longer(
    cols = starts_with("Suppressed_RSI"),
    names_to = "Threshold",
    values_to = "Suppressed"
  ) %>%
  filter(
    !is.na(Mlax_Position),
    !is.na(Suppressed)
  ) %>%
  mutate(
    Threshold = recode(
      Threshold,
      Suppressed_RSI_05 = "-0.5 SD",
      Suppressed_RSI_10 = "-1.0 SD",
      Suppressed_RSI_15 = "-1.5 SD"
    ),
    Threshold = factor(
      Threshold,
      levels = c("-0.5 SD", "-1.0 SD", "-1.5 SD")
    )
  ) %>%
  group_by(
    Mlax_Position,
    Threshold
  ) %>%
  summarise(
    Total_Observations = n(),
    Suppressed_Observations = sum(Suppressed == "Yes"),
    Suppression_Percentage =
      100 * Suppressed_Observations / Total_Observations,
    .groups = "drop"
  )

rsi_all_threshold_heatmap <- ggplot(
  rsi_all_thresholds,
  aes(
    x = Threshold,
    y = Mlax_Position,
    fill = Suppression_Percentage
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(
        round(Suppression_Percentage, 1),
        "%"
      )
    ),
    size = 4
  ) +
  scale_fill_gradient(
    low = "white",
    high = "darkblue"
  ) +
  labs(
    title = "RSI Suppression Rate by Position and Threshold",
    x = "Suppression Threshold",
    y = "Position",
    fill = "Suppressed (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )

rsi_all_threshold_heatmap



#####=====Building the classification model comparison table=====#####

extract_classification_metrics <- function(results) {
  
  data.frame(
    
    Outcome = results$model_label,
    
    Model = c("Logistic Regression", "Random Forest"),
    
    Accuracy = c(
      as.numeric(results$logistic_confusion$overall["Accuracy"]),
      as.numeric(results$rf_confusion$overall["Accuracy"])
    ),
    
    Sensitivity = c(
      as.numeric(results$logistic_confusion$byClass["Sensitivity"]),
      as.numeric(results$rf_confusion$byClass["Sensitivity"])
    ),
    
    Specificity = c(
      as.numeric(results$logistic_confusion$byClass["Specificity"]),
      as.numeric(results$rf_confusion$byClass["Specificity"])
    ),
    
    Precision = c(
      as.numeric(results$logistic_confusion$byClass["Pos Pred Value"]),
      as.numeric(results$rf_confusion$byClass["Pos Pred Value"])
    ),
    
    F1 = c(
      as.numeric(results$logistic_confusion$byClass["F1"]),
      as.numeric(results$rf_confusion$byClass["F1"])
    ),
    
    Balanced_Accuracy = c(
      as.numeric(results$logistic_confusion$byClass["Balanced Accuracy"]),
      as.numeric(results$rf_confusion$byClass["Balanced Accuracy"])
    ),
    
    AUC = c(
      results$logistic_auc,
      results$rf_auc
    )
    
  )
  
}

#######=====Combine all classification metrics into a single summary table=====#####

classification_summary <- dplyr::bind_rows(
  
  extract_classification_metrics(cmj_05_results),
  
  extract_classification_metrics(cmj_10_results),
  
  extract_classification_metrics(cmj_15_results),
  
  extract_classification_metrics(rsi_05_results),
  
  extract_classification_metrics(rsi_10_results),
  
  extract_classification_metrics(rsi_15_results)
  
)

classification_summary <- classification_summary %>%
  mutate(
    
    Accuracy = round(Accuracy, 3),
    
    Sensitivity = round(Sensitivity, 3),
    
    Specificity = round(Specificity, 3),
    
    Precision = round(Precision, 3),
    
    F1 = round(F1, 3),
    
    Balanced_Accuracy = round(Balanced_Accuracy, 3),
    
    AUC = round(AUC, 3)
    
  )

classification_summary

# =========================================================
# THRESHOLD OPTIMIZATION USING TRAINING CV PREDICTIONS
# =========================================================
# Thresholds are selected ONLY from out-of-fold predictions
# generated during cross-validation on the training data.
#
# The held-out test set is NOT used to select thresholds.
# =========================================================

get_cv_optimal_threshold <- function(
    results,
    model = c("logistic", "rf")
) {
  
  model <- match.arg(model)
  
  # -------------------------------------------------------
  # Select cross-validation predictions
  # -------------------------------------------------------
  
  if (model == "logistic") {
    
    cv_predictions <- results$logistic_model$pred
    
  } else {
    
    cv_predictions <- results$rf_model$pred
    
    # -----------------------------------------------------
    # Ensure RF predictions correspond to the final/best
    # tuning parameters selected by caret
    # -----------------------------------------------------
    
    best_tune <- results$rf_model$bestTune
    
    for (parameter in names(best_tune)) {
      
      cv_predictions <- cv_predictions[
        cv_predictions[[parameter]] == best_tune[[parameter]],
      ]
      
    }
  }
  
  # -------------------------------------------------------
  # Make sure class levels are correct
  # -------------------------------------------------------
  
  cv_predictions$obs <- factor(
    cv_predictions$obs,
    levels = c("No", "Yes")
  )
  
  # -------------------------------------------------------
  # Build ROC curve using TRAINING CV predictions
  # -------------------------------------------------------
  
  cv_roc <- pROC::roc(
    response = cv_predictions$obs,
    predictor = cv_predictions$Yes,
    levels = c("No", "Yes"),
    direction = "<",
    quiet = TRUE
  )
  
  # -------------------------------------------------------
  # Determine Youden-optimal threshold
  # -------------------------------------------------------
  
  best_threshold <- pROC::coords(
    cv_roc,
    x = "best",
    best.method = "youden",
    ret = c(
      "threshold",
      "sensitivity",
      "specificity"
    ),
    transpose = FALSE
  )
  
  # -------------------------------------------------------
  # If multiple thresholds are equally optimal,
  # prioritize higher sensitivity for screening
  # -------------------------------------------------------
  
  if (nrow(best_threshold) > 1) {
    
    best_threshold <- best_threshold %>%
      arrange(
        desc(sensitivity),
        desc(specificity)
      ) %>%
      slice(1)
    
  }
  
  threshold <- as.numeric(
    best_threshold$threshold[1]
  )
  
  # -------------------------------------------------------
  # Return threshold information
  # -------------------------------------------------------
  
  list(
    
    threshold = threshold,
    
    cv_sensitivity =
      as.numeric(best_threshold$sensitivity[1]),
    
    cv_specificity =
      as.numeric(best_threshold$specificity[1]),
    
    cv_auc =
      as.numeric(pROC::auc(cv_roc)),
    
    cv_roc = cv_roc
    
  )
}
# =========================================================
# APPLY CV-OPTIMIZED THRESHOLD TO HELD-OUT TEST DATA
# =========================================================

evaluate_optimized_test <- function(
    results,
    model = c("logistic", "rf")
) {
  
  model <- match.arg(model)
  
  # -------------------------------------------------------
  # Get threshold from TRAINING cross-validation
  # -------------------------------------------------------
  
  threshold_info <- get_cv_optimal_threshold(
    results,
    model
  )
  
  threshold <- threshold_info$threshold
  
  # -------------------------------------------------------
  # True outcomes from HELD-OUT TEST SET
  # -------------------------------------------------------
  
  truth <- factor(
    results$test_data$Outcome,
    levels = c("No", "Yes")
  )
  
  # -------------------------------------------------------
  # Get HELD-OUT TEST probabilities
  # -------------------------------------------------------
  
  if (model == "logistic") {
    
    test_probabilities <-
      results$logistic_probabilities$Yes
    
    test_auc <-
      results$logistic_auc
    
  } else {
    
    test_probabilities <-
      results$rf_probabilities$Yes
    
    test_auc <-
      results$rf_auc
    
  }
  
  # -------------------------------------------------------
  # Apply locked CV threshold to test probabilities
  # -------------------------------------------------------
  
  optimized_predictions <- ifelse(
    test_probabilities >= threshold,
    "Yes",
    "No"
  )
  
  optimized_predictions <- factor(
    optimized_predictions,
    levels = c("No", "Yes")
  )
  
  # -------------------------------------------------------
  # Final HELD-OUT TEST confusion matrix
  # -------------------------------------------------------
  
  optimized_confusion <- caret::confusionMatrix(
    data = optimized_predictions,
    reference = truth,
    positive = "Yes"
  )
  
  # -------------------------------------------------------
  # Return results
  # -------------------------------------------------------
  
  list(
    
    threshold = threshold,
    
    cv_auc =
      threshold_info$cv_auc,
    
    cv_sensitivity =
      threshold_info$cv_sensitivity,
    
    cv_specificity =
      threshold_info$cv_specificity,
    
    predictions =
      optimized_predictions,
    
    probabilities =
      test_probabilities,
    
    truth =
      truth,
    
    confusion =
      optimized_confusion,
    
    test_auc =
      as.numeric(test_auc)
    
  )
}

# =========================================================
# CMJ OPTIMIZED TEST EVALUATION
# =========================================================

cmj05_log_opt <- evaluate_optimized_test(
  cmj_05_results,
  "logistic"
)

cmj05_rf_opt <- evaluate_optimized_test(
  cmj_05_results,
  "rf"
)

cmj10_log_opt <- evaluate_optimized_test(
  cmj_10_results,
  "logistic"
)

cmj10_rf_opt <- evaluate_optimized_test(
  cmj_10_results,
  "rf"
)

cmj15_log_opt <- evaluate_optimized_test(
  cmj_15_results,
  "logistic"
)


cmj15_rf_opt <- evaluate_optimized_test(
  cmj_15_results,
  "rf"
)


# =========================================================
# RSI OPTIMIZED TEST EVALUATION
# =========================================================

rsi05_log_opt <- evaluate_optimized_test(
  rsi_05_results,
  "logistic"
)

rsi05_rf_opt <- evaluate_optimized_test(
  rsi_05_results,
  "rf"
)

rsi10_log_opt <- evaluate_optimized_test(
  rsi_10_results,
  "logistic"
)

rsi10_rf_opt <- evaluate_optimized_test(
  rsi_10_results,
  "rf"
)

rsi15_log_opt <- evaluate_optimized_test(
  rsi_15_results,
  "logistic"
)


rsi15_rf_opt <- evaluate_optimized_test(
  rsi_15_results,
  "rf"
)

cmj05_log_opt$threshold
cmj05_rf_opt$threshold

cmj10_log_opt$threshold
cmj10_rf_opt$threshold

cmj15_log_opt$threshold
cmj15_rf_opt$threshold


rsi05_log_opt$threshold
rsi05_rf_opt$threshold

rsi10_log_opt$threshold
rsi10_rf_opt$threshold

rsi15_log_opt$threshold
rsi15_rf_opt$threshold

cat("\n================ CMJ -0.5 Logistic ================\n")
cat("Threshold:", cmj05_log_opt$threshold, "\n")
print(cmj05_log_opt$confusion)

cat("\n================ CMJ -0.5 Random Forest ================\n")
cat("Threshold:", cmj05_rf_opt$threshold, "\n")
print(cmj05_rf_opt$confusion)

cat("\n================ CMJ -1.0 Logistic ================\n")
cat("Threshold:", cmj10_log_opt$threshold, "\n")
print(cmj10_log_opt$confusion)

cat("\n================ CMJ -1.0 Random Forest ================\n")
cat("Threshold:", cmj10_rf_opt$threshold, "\n")
print(cmj10_rf_opt$confusion)

cat("\n================ CMJ -1.5 Logistic ================\n")
cat("Threshold:", cmj15_log_opt$threshold, "\n")
print(cmj15_log_opt$confusion)

cat("\n================ CMJ -1.5 Random Forest ================\n")
cat("Threshold:", cmj15_rf_opt$threshold, "\n")
print(cmj15_rf_opt$confusion)

cat("\n================ RSI -0.5 Logistic ================\n")
cat("Threshold:", rsi05_log_opt$threshold, "\n")
print(rsi05_log_opt$confusion)

cat("\n================ RSI -0.5 Random Forest ================\n")
cat("Threshold:", rsi05_rf_opt$threshold, "\n")
print(rsi05_rf_opt$confusion)

cat("\n================ RSI -1.0 Logistic ================\n")
cat("Threshold:", rsi10_log_opt$threshold, "\n")
print(rsi10_log_opt$confusion)

cat("\n================ RSI -1.0 Random Forest ================\n")
cat("Threshold:", rsi10_rf_opt$threshold, "\n")
print(rsi10_rf_opt$confusion)

cat("\n================ RSI -1.5 Logistic ================\n")
cat("Threshold:", rsi15_log_opt$threshold, "\n")
print(rsi15_log_opt$confusion)

cat("\n================ RSI -1.5 Random Forest ================\n")
cat("Threshold:", rsi15_rf_opt$threshold, "\n")
print(rsi15_rf_opt$confusion)

# =========================================================
# FUNCTION TO EXTRACT OPTIMIZED RESULTS
# =========================================================

extract_optimized_metrics <- function(opt_result,
                                      outcome,
                                      model_name,
                                      auc){
  
  cm <- opt_result$confusion
  
  data.frame(
    
    Outcome = outcome,
    
    Model = model_name,
    
    Threshold = round(opt_result$threshold,3),
    
    Accuracy = round(as.numeric(cm$overall["Accuracy"]),3),
    
    Sensitivity = round(as.numeric(cm$byClass["Sensitivity"]),3),
    
    Specificity = round(as.numeric(cm$byClass["Specificity"]),3),
    
    Precision = round(as.numeric(cm$byClass["Pos Pred Value"]),3),
    
    F1 = round(as.numeric(cm$byClass["F1"]),3),
    
    Balanced_Accuracy =
      round(as.numeric(cm$byClass["Balanced Accuracy"]),3),
    
    AUC = round(as.numeric(auc),3)
    
  )
  
}

optimized_classification_summary <- dplyr::bind_rows(
  
  # CMJ -0.5
  extract_optimized_metrics(cmj05_log_opt,
                            cmj_05_results$model_label,
                            "Logistic Regression",
                            cmj_05_results$logistic_auc),
  
  extract_optimized_metrics(cmj05_rf_opt,
                            cmj_05_results$model_label,
                            "Random Forest",
                            cmj_05_results$rf_auc),
  
  # CMJ -1.0
  extract_optimized_metrics(cmj10_log_opt,
                            cmj_10_results$model_label,
                            "Logistic Regression",
                            cmj_10_results$logistic_auc),
  
  extract_optimized_metrics(cmj10_rf_opt,
                            cmj_10_results$model_label,
                            "Random Forest",
                            cmj_10_results$rf_auc),
  
  # CMJ -1.5
  extract_optimized_metrics(cmj15_log_opt,
                            cmj_15_results$model_label,
                            "Logistic Regression",
                            cmj_15_results$logistic_auc),
  
  extract_optimized_metrics(cmj15_rf_opt,
                            cmj_15_results$model_label,
                            "Random Forest",
                            cmj_15_results$rf_auc),
  
  # RSI -0.5
  extract_optimized_metrics(rsi05_log_opt,
                            rsi_05_results$model_label,
                            "Logistic Regression",
                            rsi_05_results$logistic_auc),
  
  extract_optimized_metrics(rsi05_rf_opt,
                            rsi_05_results$model_label,
                            "Random Forest",
                            rsi_05_results$rf_auc),
  
  # RSI -1.0
  extract_optimized_metrics(rsi10_log_opt,
                            rsi_10_results$model_label,
                            "Logistic Regression",
                            rsi_10_results$logistic_auc),
  
  extract_optimized_metrics(rsi10_rf_opt,
                            rsi_10_results$model_label,
                            "Random Forest",
                            rsi_10_results$rf_auc),
  
  # RSI -1.5
  extract_optimized_metrics(rsi15_log_opt,
                            rsi_15_results$model_label,
                            "Logistic Regression",
                            rsi_15_results$logistic_auc),
  
  extract_optimized_metrics(rsi15_rf_opt,
                            rsi_15_results$model_label,
                            "Random Forest",
                            rsi_15_results$rf_auc)
  
)

optimized_classification_summary
