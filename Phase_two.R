# Libraries
library(tidyverse)
library(inspectdf)
library(tidymodels)
library(car)
library(vip)
library(rpart.plot)


# Load the raw data
raw <- read_csv("australia.csv")

dim(raw)
head(raw)

# Select only the columns we need
df <- select(raw,
             # Identifiers and time
             RecordNo, endtime,
             
             # Demographics
             age, gender, state, household_size, employment_status,
             
             # Mask wearing
             i12_health_1, i12_health_22, i12_health_23, i12_health_25,
             
             # Other protective behaviours
             i12_health_2, i12_health_3, i12_health_4, i12_health_5, i12_health_6, i12_health_7, i12_health_8,
             i12_health_11, i12_health_12, i12_health_13, i12_health_14,
             i12_health_15, i12_health_16,
             
             # Non-household contacts
             i13_health,
             
             # Ease and willingness to isolate
             i10_health, i11_health,
             
             # Trust in government
             i1_health,
             
             # Perceived susceptibility
             i7a_health,
             
             # Perceived severity
             r1_1,
             
             # Wellbeing and mental health
             cantril_ladder,
             PHQ4_1, PHQ4_2, PHQ4_3, PHQ4_4
)

dim(df)
head(df, 5)


# Parse date and create survey week
df <- mutate(df,
             date_part   = str_match(endtime, "(\\d{2}/\\d{2}/\\d{4})")[,2],
             survey_date = dmy(date_part),
             survey_week = as.integer(survey_date - min(survey_date, na.rm = TRUE)) / 14
)


# Check missing values
inspect_na(df)

# Replace blank strings with NA
df <- mutate(df,
             gender            = ifelse(str_replace_all(gender, " ", "") == "", NA_character_, gender),
             state             = ifelse(str_replace_all(state, " ", "") == "", NA_character_, state),
             household_size    = ifelse(str_replace_all(household_size, " ", "") == "", NA_character_, household_size),
             employment_status = ifelse(str_replace_all(employment_status, " ", "") == "", NA_character_, employment_status),
             i12_health_1      = ifelse(str_replace_all(i12_health_1, " ", "") == "", NA_character_, i12_health_1),
             i12_health_2      = ifelse(str_replace_all(i12_health_2, " ", "") == "", NA_character_, i12_health_2),
             i12_health_3      = ifelse(str_replace_all(i12_health_3, " ", "") == "", NA_character_, i12_health_3),
             i12_health_4      = ifelse(str_replace_all(i12_health_4, " ", "") == "", NA_character_, i12_health_4),
             i12_health_5      = ifelse(str_replace_all(i12_health_5, " ", "") == "", NA_character_, i12_health_5),
             i12_health_6      = ifelse(str_replace_all(i12_health_6, " ", "") == "", NA_character_, i12_health_6),
             i12_health_7      = ifelse(str_replace_all(i12_health_7, " ", "") == "", NA_character_, i12_health_7),
             i12_health_8      = ifelse(str_replace_all(i12_health_8, " ", "") == "", NA_character_, i12_health_8),
             i12_health_11     = ifelse(str_replace_all(i12_health_11, " ", "") == "", NA_character_, i12_health_11),
             i12_health_12     = ifelse(str_replace_all(i12_health_12, " ", "") == "", NA_character_, i12_health_12),
             i12_health_13     = ifelse(str_replace_all(i12_health_13, " ", "") == "", NA_character_, i12_health_13),
             i12_health_14     = ifelse(str_replace_all(i12_health_14, " ", "") == "", NA_character_, i12_health_14),
             i12_health_15     = ifelse(str_replace_all(i12_health_15, " ", "") == "", NA_character_, i12_health_15),
             i12_health_16     = ifelse(str_replace_all(i12_health_16, " ", "") == "", NA_character_, i12_health_16),
             i12_health_22     = ifelse(str_replace_all(i12_health_22, " ", "") == "", NA_character_, i12_health_22),
             i12_health_23     = ifelse(str_replace_all(i12_health_23, " ", "") == "", NA_character_, i12_health_23),
             i12_health_25     = ifelse(str_replace_all(i12_health_25, " ", "") == "", NA_character_, i12_health_25),
             i10_health        = ifelse(str_replace_all(i10_health, " ", "") == "", NA_character_, i10_health),
             i11_health        = ifelse(str_replace_all(i11_health, " ", "") == "", NA_character_, i11_health),
             PHQ4_1            = ifelse(str_replace_all(PHQ4_1, " ", "") == "", NA_character_, PHQ4_1),
             PHQ4_2            = ifelse(str_replace_all(PHQ4_2, " ", "") == "", NA_character_, PHQ4_2),
             PHQ4_3            = ifelse(str_replace_all(PHQ4_3, " ", "") == "", NA_character_, PHQ4_3),
             PHQ4_4            = ifelse(str_replace_all(PHQ4_4, " ", "") == "", NA_character_, PHQ4_4)
)

# Convert columns to correct data types
df <- mutate(df,
             cantril_ladder = as.numeric(cantril_ladder),
             i1_health      = as.numeric(i1_health),
             i7a_health     = as.numeric(i7a_health),
             r1_1           = as.numeric(str_match(r1_1, "(\\d+)")[,2]),
             i13_health     = as.numeric(i13_health),
             age            = as.numeric(age)
)

df <- mutate(df,
             gender = as.factor(gender),
             state  = as.factor(state)
)

count(df, state)


# Recode Likert scale text variables

# Mask wearing
df <- mutate(df,
             m1_1_num = ifelse(i12_health_1 == "Not at all", 1,
                               ifelse(i12_health_1 == "Rarely", 2,
                                      ifelse(i12_health_1 == "Sometimes", 3,
                                             ifelse(i12_health_1 == "Frequently", 4,
                                                    ifelse(i12_health_1 == "Always", 5, NA_real_))))),
             
             m1_2_num = ifelse(i12_health_22 == "Not at all", 1,
                               ifelse(i12_health_22 == "Rarely", 2,
                                      ifelse(i12_health_22 == "Sometimes", 3,
                                             ifelse(i12_health_22 == "Frequently", 4,
                                                    ifelse(i12_health_22 == "Always", 5, NA_real_))))),
             
             m1_3_num = ifelse(i12_health_23 == "Not at all", 1,
                               ifelse(i12_health_23 == "Rarely", 2,
                                      ifelse(i12_health_23 == "Sometimes", 3,
                                             ifelse(i12_health_23 == "Frequently", 4,
                                                    ifelse(i12_health_23 == "Always", 5, NA_real_))))),
             
             m1_4_num = ifelse(i12_health_25 == "Not at all", 1,
                               ifelse(i12_health_25 == "Rarely", 2,
                                      ifelse(i12_health_25 == "Sometimes", 3,
                                             ifelse(i12_health_25 == "Frequently", 4,
                                                    ifelse(i12_health_25 == "Always", 5, NA_real_)))))
)

# Other protective behaviours: i12_health_5 to i12_health_16
df <- mutate(df,
             m9_1_num = ifelse(i12_health_5 == "Not at all", 1,
                               ifelse(i12_health_5 == "Rarely", 2,
                                      ifelse(i12_health_5 == "Sometimes", 3,
                                             ifelse(i12_health_5 == "Frequently", 4,
                                                    ifelse(i12_health_5 == "Always", 5, NA_real_))))),
             
             m9_2_num = ifelse(i12_health_6 == "Not at all", 1,
                               ifelse(i12_health_6 == "Rarely", 2,
                                      ifelse(i12_health_6 == "Sometimes", 3,
                                             ifelse(i12_health_6 == "Frequently", 4,
                                                    ifelse(i12_health_6 == "Always", 5, NA_real_))))),
             
             m9_3_num = ifelse(i12_health_7 == "Not at all", 1,
                               ifelse(i12_health_7 == "Rarely", 2,
                                      ifelse(i12_health_7 == "Sometimes", 3,
                                             ifelse(i12_health_7 == "Frequently", 4,
                                                    ifelse(i12_health_7 == "Always", 5, NA_real_))))),
             
             m9_4_num = ifelse(i12_health_8 == "Not at all", 1,
                               ifelse(i12_health_8 == "Rarely", 2,
                                      ifelse(i12_health_8 == "Sometimes", 3,
                                             ifelse(i12_health_8 == "Frequently", 4,
                                                    ifelse(i12_health_8 == "Always", 5, NA_real_))))),
             
             m9_5_num = ifelse(i12_health_11 == "Not at all", 1,
                               ifelse(i12_health_11 == "Rarely", 2,
                                      ifelse(i12_health_11 == "Sometimes", 3,
                                             ifelse(i12_health_11 == "Frequently", 4,
                                                    ifelse(i12_health_11 == "Always", 5, NA_real_))))),
             
             m9_6_num = ifelse(i12_health_12 == "Not at all", 1,
                               ifelse(i12_health_12 == "Rarely", 2,
                                      ifelse(i12_health_12 == "Sometimes", 3,
                                             ifelse(i12_health_12 == "Frequently", 4,
                                                    ifelse(i12_health_12 == "Always", 5, NA_real_))))),
             
             m9_7_num = ifelse(i12_health_13 == "Not at all", 1,
                               ifelse(i12_health_13 == "Rarely", 2,
                                      ifelse(i12_health_13 == "Sometimes", 3,
                                             ifelse(i12_health_13 == "Frequently", 4,
                                                    ifelse(i12_health_13 == "Always", 5, NA_real_))))),
             
             m9_8_num = ifelse(i12_health_14 == "Not at all", 1,
                               ifelse(i12_health_14 == "Rarely", 2,
                                      ifelse(i12_health_14 == "Sometimes", 3,
                                             ifelse(i12_health_14 == "Frequently", 4,
                                                    ifelse(i12_health_14 == "Always", 5, NA_real_))))),
             
             m9_9_num = ifelse(i12_health_15 == "Not at all", 1,
                               ifelse(i12_health_15 == "Rarely", 2,
                                      ifelse(i12_health_15 == "Sometimes", 3,
                                             ifelse(i12_health_15 == "Frequently", 4,
                                                    ifelse(i12_health_15 == "Always", 5, NA_real_))))),
             
             m9_10_num = ifelse(i12_health_16 == "Not at all", 1,
                                ifelse(i12_health_16 == "Rarely", 2,
                                       ifelse(i12_health_16 == "Sometimes", 3,
                                              ifelse(i12_health_16 == "Frequently", 4,
                                                     ifelse(i12_health_16 == "Always", 5, NA_real_))))),
             
             m9_11_num = ifelse(i12_health_2 == "Not at all", 1,
                                ifelse(i12_health_2 == "Rarely", 2,
                                       ifelse(i12_health_2 == "Sometimes", 3,
                                              ifelse(i12_health_2 == "Frequently", 4,
                                                     ifelse(i12_health_2 == "Always", 5, NA_real_))))),
             
             m9_12_num = ifelse(i12_health_3 == "Not at all", 1,
                                ifelse(i12_health_3 == "Rarely", 2,
                                       ifelse(i12_health_3 == "Sometimes", 3,
                                              ifelse(i12_health_3 == "Frequently", 4,
                                                     ifelse(i12_health_3 == "Always", 5, NA_real_))))),
             
             m9_13_num = ifelse(i12_health_4 == "Not at all", 1,
                                ifelse(i12_health_4 == "Rarely", 2,
                                       ifelse(i12_health_4 == "Sometimes", 3,
                                              ifelse(i12_health_4 == "Frequently", 4,
                                                     ifelse(i12_health_4 == "Always", 5, NA_real_)))))
)

# i10_health: ease of self-isolation
df <- mutate(df,
             i10_health_num = ifelse(i10_health == "Very difficult", 1,
                                     ifelse(i10_health == "Somewhat difficult", 2,
                                            ifelse(i10_health == "Neither easy nor difficult", 3,
                                                   ifelse(i10_health == "Somewhat easy", 4,
                                                          ifelse(i10_health == "Very easy", 5, NA_real_)))))
)

# i11_health: willingness to self-isolate
df <- mutate(df,
             i11_health_num = ifelse(i11_health == "Very unwilling", 1,
                                     ifelse(i11_health == "Somewhat unwilling", 2,
                                            ifelse(i11_health == "Neither willing nor unwilling", 3,
                                                   ifelse(i11_health == "Somewhat willing", 4,
                                                          ifelse(i11_health == "Very willing", 5, NA_real_)))))
)

# PHQ4 mental health items (0 = Not at all, 3 = Nearly every day)
df <- mutate(df,
             phq4_1_num = ifelse(PHQ4_1 == "Not at all", 0,
                                 ifelse(PHQ4_1 == "Several days", 1,
                                        ifelse(PHQ4_1 == "More than half the days", 2,
                                               ifelse(PHQ4_1 == "Nearly every day", 3, NA_real_)))),
             
             phq4_2_num = ifelse(PHQ4_2 == "Not at all", 0,
                                 ifelse(PHQ4_2 == "Several days", 1,
                                        ifelse(PHQ4_2 == "More than half the days", 2,
                                               ifelse(PHQ4_2 == "Nearly every day", 3, NA_real_)))),
             
             phq4_3_num = ifelse(PHQ4_3 == "Not at all", 0,
                                 ifelse(PHQ4_3 == "Several days", 1,
                                        ifelse(PHQ4_3 == "More than half the days", 2,
                                               ifelse(PHQ4_3 == "Nearly every day", 3, NA_real_)))),
             
             phq4_4_num = ifelse(PHQ4_4 == "Not at all", 0,
                                 ifelse(PHQ4_4 == "Several days", 1,
                                        ifelse(PHQ4_4 == "More than half the days", 2,
                                               ifelse(PHQ4_4 == "Nearly every day", 3, NA_real_))))
)

# PHQ4 status classification
df <- mutate(df,
             phq4_1_declined = ifelse(is.na(PHQ4_1), FALSE, PHQ4_1 == "Prefer not to say"),
             phq4_2_declined = ifelse(is.na(PHQ4_2), FALSE, PHQ4_2 == "Prefer not to say"),
             phq4_3_declined = ifelse(is.na(PHQ4_3), FALSE, PHQ4_3 == "Prefer not to say"),
             phq4_4_declined = ifelse(is.na(PHQ4_4), FALSE, PHQ4_4 == "Prefer not to say")
)

df <- mutate(df,
             phq4_declined = phq4_1_declined | phq4_2_declined | phq4_3_declined | phq4_4_declined,
             phq4_blank    = is.na(PHQ4_1) | is.na(PHQ4_2) | is.na(PHQ4_3) | is.na(PHQ4_4),
             
             phq4_status = ifelse(phq4_declined, "Declined",
                                  ifelse(phq4_blank,   "Not available",
                                         "Answered")),
             phq4_status = as.factor(phq4_status)
)

count(df, phq4_status)

df <- mutate(df,
             phq4_total = ifelse(phq4_status == "Answered",
                                 phq4_1_num + phq4_2_num + phq4_3_num + phq4_4_num,
                                 NA_real_)
)


# Create target variables
df <- mutate(df,
             # Average mask wearing score across 4 scenarios
             face_mask_score = (m1_1_num + m1_2_num + m1_3_num + m1_4_num) / 4,
             
             # Binary mask compliance: 1 = Frequently/Always (score >= 4), 0 = not compliant
             face_mask_wearing = ifelse(face_mask_score >= 4, 1, 0),
             
             # Average score across other protective behaviours
             other_behaviour_score = (m9_1_num + m9_2_num + m9_3_num + m9_4_num +
                                        m9_5_num + m9_6_num + m9_7_num + m9_8_num +
                                        m9_9_num + m9_10_num + m9_11_num + m9_12_num + m9_13_num) / 13,
             
             # Combined general behaviour score
             general_behaviour_score = (face_mask_score + other_behaviour_score) / 2,
             
             # Binary general behaviour
             general_behaviour = ifelse(general_behaviour_score >= 4, 1, 0)
)

mean(df$face_mask_wearing, na.rm = TRUE) * 100
mean(df$general_behaviour, na.rm = TRUE) * 100


# Clean household size
df <- mutate(df,
             household_size_num = ifelse(household_size == "8 or more", 8,
                                         ifelse(household_size == "Prefer not to say" | household_size == "Don't know",
                                                NA_real_,
                                                as.numeric(household_size)))
)

# Clean employment status into broad categories
df <- mutate(df,
             employment_clean = ifelse(str_detect(employment_status, "ull time"), "Full time",
                                       ifelse(str_detect(employment_status, "art time"), "Part time",
                                              ifelse(str_detect(employment_status, "Retired"), "Retired",
                                                     ifelse(str_detect(employment_status, "Unemployed"), "Unemployed",
                                                            ifelse(str_detect(employment_status, "Not working"), "Not working",
                                                                   "Other"))))),
             employment_clean = as.factor(employment_clean)
)

count(df, employment_clean)

# Check for duplicates
sum(duplicated(select(df, -RecordNo)))

# Drop rows with missing values in key variables
nrow(df)

df_clean <- filter(df,
                   !is.na(face_mask_wearing) & !is.na(general_behaviour) &
                     !is.na(age) & !is.na(gender) & !is.na(state) &
                     !is.na(household_size_num) & !is.na(employment_clean) &
                     !is.na(cantril_ladder) & !is.na(i11_health_num) &
                     !is.na(survey_week) & !is.na(other_behaviour_score))

nrow(df_clean)
nrow(df) - nrow(df_clean)
inspect_na(df_clean)


# Select and rename final variables
df_final <- select(df_clean,
                   record_no                = RecordNo,
                   survey_date,
                   survey_week,
                   state,
                   age,
                   gender,
                   household_size           = household_size_num,
                   employment               = employment_clean,
                   contacts                 = i13_health,
                   wellbeing                = cantril_ladder,
                   govt_handling            = i1_health,
                   perceived_susceptibility = i7a_health,
                   perceived_severity       = r1_1,
                   ease_of_isolation        = i10_health_num,
                   willingness_to_isolate   = i11_health_num,
                   phq4_status,
                   phq4_total,
                   other_behaviour_score,
                   face_mask_wearing,
                   general_behaviour
)

dim(df_final)
head(df_final, 5)

# Removing outliers
df_final <- mutate(df_final,
                   govt_handling            = ifelse(govt_handling > 10, NA, govt_handling),
                   perceived_susceptibility = ifelse(perceived_susceptibility > 10, NA, perceived_susceptibility)
)

dim(df_final)

# Descriptive summary
inspect_num(df_final)

mean(df_final$age, na.rm = TRUE)
sd(df_final$age, na.rm = TRUE)
max(df_final$survey_week, na.rm = TRUE)
min(df_final$survey_week, na.rm = TRUE)


# Build model dataset
df_model <- df_final %>%
  select(-perceived_susceptibility, -ease_of_isolation, -govt_handling, -contacts) %>%
  mutate(face_mask_wearing = as.factor(
    ifelse(face_mask_wearing == 1, "yes", "no"))) %>%
  select(face_mask_wearing,
         survey_week, state, age, gender, household_size, employment,
         wellbeing, perceived_severity,
         willingness_to_isolate,
         phq4_status,
         other_behaviour_score) %>%
  filter(!is.na(face_mask_wearing) & !is.na(survey_week) & !is.na(state) &
           !is.na(age) & !is.na(gender) & !is.na(household_size) & !is.na(employment) &
           !is.na(wellbeing) & !is.na(perceived_severity) & !is.na(willingness_to_isolate) &
           !is.na(phq4_status) & !is.na(other_behaviour_score))

dim(df_model)
count(df_model, face_mask_wearing)

# Model without state
df_model_no_state <- df_final %>%
  select(-perceived_susceptibility, -ease_of_isolation, -govt_handling, -contacts) %>%
  mutate(face_mask_wearing = as.factor(
    ifelse(face_mask_wearing == 1, "yes", "no"))) %>%
  select(face_mask_wearing,
         survey_week, age, gender, household_size, employment,
         wellbeing, perceived_severity,
         willingness_to_isolate,
         phq4_status,
         other_behaviour_score) %>%
  filter(!is.na(face_mask_wearing) & !is.na(survey_week) &
           !is.na(age) & !is.na(gender) & !is.na(household_size) & !is.na(employment) &
           !is.na(wellbeing) & !is.na(perceived_severity) & !is.na(willingness_to_isolate) &
           !is.na(phq4_status) & !is.na(other_behaviour_score))

dim(df_model_no_state)
count(df_model_no_state, face_mask_wearing)


# Replace state with measurable state-level predictors

oxcgrt_raw <- read_csv("OxCGRT_AUS_latest.csv")

oxcgrt <- filter(oxcgrt_raw, Jurisdiction == "STATE_TOTAL")
oxcgrt <- mutate(oxcgrt,
                 state           = RegionName,
                 ox_date         = ymd(Date),
                 h6              = `H6M_Facial Coverings`,
                 c6              = `C6M_Stay at home requirements`,
                 fortnight_index = as.integer(as.integer(ox_date - min(df_final$survey_date, na.rm = TRUE)) / 14)
)

oxcgrt_avg <- oxcgrt %>%
  group_by(state, fortnight_index) %>%
  summarise(h6_fortnight_avg = mean(h6, na.rm = TRUE),
            c6_fortnight_avg = mean(c6, na.rm = TRUE))
sum(is.na(oxcgrt_avg$h6_fortnight_avg))

abs_indicators <- read_csv("abs_state_indicators_2021.csv")

# Check state names match
count(df_final, state)
count(oxcgrt_avg, state)
count(abs_indicators, state)

df_final_p2 <- mutate(df_final,
                      fortnight_index = as.integer(as.integer(survey_date - min(survey_date, na.rm = TRUE)) / 14)
)

df_final_p2 <- left_join(df_final_p2, oxcgrt_avg, by = c("state", "fortnight_index"))
df_final_p2 <- left_join(df_final_p2, abs_indicators, by = "state")

nrow(df_final)
nrow(df_final_p2)
sum(is.na(df_final_p2$h6_fortnight_avg))
sum(is.na(df_final_p2$median_weekly_household_income))

df_model_phase2 <- df_final_p2 %>%
  select(-perceived_susceptibility, -ease_of_isolation, -govt_handling, -contacts, -state) %>%
  mutate(face_mask_wearing = as.factor(
    ifelse(face_mask_wearing == 1, "yes", "no"))) %>%
  select(face_mask_wearing,
         survey_week, age, gender, household_size, employment,
         wellbeing, perceived_severity,
         willingness_to_isolate,
         phq4_status,
         other_behaviour_score,
         h6_fortnight_avg,
         median_weekly_household_income,
         pct_bachelor_degree_plus,
         pct_born_overseas,
         c6_fortnight_avg) %>%
  filter(!is.na(face_mask_wearing) & !is.na(survey_week) &
           !is.na(age) & !is.na(gender) & !is.na(household_size) & !is.na(employment) &
           !is.na(wellbeing) & !is.na(perceived_severity) & !is.na(willingness_to_isolate) &
           !is.na(phq4_status) & !is.na(other_behaviour_score) &
           !is.na(h6_fortnight_avg) & !is.na(median_weekly_household_income) &
           !is.na(pct_bachelor_degree_plus) & !is.na(pct_born_overseas) &
           !is.na(c6_fortnight_avg))

dim(df_model_phase2)
count(df_model_phase2, face_mask_wearing)

# Exploratory plots

# Mask compliance by state
ggplot(df_final, aes(x = state, fill = as.factor(face_mask_wearing))) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "State",
       y = "Proportion",
       fill = "Mask Compliant",
       title = "Mask Compliance by State")

# Mask compliance rate by survey week
df_final %>%
  group_by(survey_week) %>%
  summarise(
    n             = sum(!is.na(face_mask_wearing)),
    pct_compliant = mean(face_mask_wearing, na.rm = TRUE) * 100
  ) %>%
  filter(n >= 30) %>%
  ggplot(aes(x = survey_week, y = pct_compliant)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(x = "Survey Week",
       y = "% Mask Compliant",
       title = "Mask Compliance Over Time")

# Other behaviour score by mask wearing status
ggplot(df_final, aes(x = as.factor(face_mask_wearing), y = other_behaviour_score)) +
  geom_boxplot() +
  labs(x = "Mask Compliant (0 = No, 1 = Yes)",
       y = "Other Protective Behaviour Score (1-5)",
       title = "General Protective Behaviour Score by Mask Compliance")


# Train/test split (70/30)

set.seed(1914220)
df_split_ns <- initial_split(df_model_no_state, prop = 0.7)
df_train_no_state <- training(df_split_ns)
df_test_no_state  <- testing(df_split_ns)

# No State Logistic Regression

lr_ns <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(face_mask_wearing ~ ., data = df_train_no_state)

# Coefficients and p-values
summary(lr_ns$fit)

# Overall significance of each predictor
Anova(lr_ns$fit)

# Predictions on test set
lr_ns_preds   <- predict(lr_ns, new_data = df_test_no_state, type = "class")
lr_ns_probs   <- predict(lr_ns, new_data = df_test_no_state, type = "prob")
lr_ns_results <- bind_cols(lr_ns_preds, lr_ns_probs, df_test_no_state)

# Confusion matrix
lr_ns_cm <- lr_ns_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(lr_ns_cm)
autoplot(lr_ns_cm, type = "heatmap")

# Sensitivity and specificity
lr_ns_sens <- (lr_ns_results %>% filter(face_mask_wearing == "yes") %>%
                 summarise(sens = mean(.pred_class == "yes")))$sens
lr_ns_spec <- (lr_ns_results %>% filter(face_mask_wearing == "no") %>%
                 summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(lr_ns_sens, 3), "\n")
cat("Specificity:", round(lr_ns_spec, 3), "\n")

# ROC curve
lr_ns_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - lr_ns_spec, color = "red") +
  geom_hline(yintercept = lr_ns_sens,     color = "red") +
  labs(title = "No State: Logistic Regression — ROC Curve")

# AUC
lr_ns_auc <- lr_ns_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("No State Logistic Regression AUC:", round(lr_ns_auc$.estimate, 3), "\n")


# No State Classification Tree

ct_ns <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(face_mask_wearing ~ ., data = df_train_no_state)

rpart.plot(ct_ns$fit, extra = 1, type = 2,
           main = "No State: Classification Tree")

# Predictions on test set
ct_ns_preds   <- predict(ct_ns, new_data = df_test_no_state, type = "class")
ct_ns_probs   <- predict(ct_ns, new_data = df_test_no_state, type = "prob")
ct_ns_results <- bind_cols(ct_ns_preds, ct_ns_probs, df_test_no_state)

# Confusion matrix
ct_ns_cm <- ct_ns_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(ct_ns_cm)
autoplot(ct_ns_cm, type = "heatmap")

# Sensitivity and specificity
ct_ns_sens <- (ct_ns_results %>% filter(face_mask_wearing == "yes") %>%
                 summarise(sens = mean(.pred_class == "yes")))$sens
ct_ns_spec <- (ct_ns_results %>% filter(face_mask_wearing == "no") %>%
                 summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(ct_ns_sens, 3), "\n")
cat("Specificity:", round(ct_ns_spec, 3), "\n")

# ROC curve
ct_ns_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - ct_ns_spec, color = "red") +
  geom_hline(yintercept = ct_ns_sens,     color = "red") +
  labs(title = "No State: Classification Tree — ROC Curve")

# AUC
ct_ns_auc <- ct_ns_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("No State Classification Tree AUC:", round(ct_ns_auc$.estimate, 3), "\n")

# Variable importance
ct_ns %>% vip() + labs(title = "No State: Classification Tree — Variable Importance")
ct_ns %>% vi()


# No State Random Forest

rf_ns <- rand_forest(mode = "classification") %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(face_mask_wearing ~ ., data = df_train_no_state)

# Predictions on test set
rf_ns_preds   <- predict(rf_ns, new_data = df_test_no_state, type = "class")
rf_ns_probs   <- predict(rf_ns, new_data = df_test_no_state, type = "prob")
rf_ns_results <- bind_cols(rf_ns_preds, rf_ns_probs, df_test_no_state)

# Confusion matrix
rf_ns_cm <- rf_ns_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(rf_ns_cm)
autoplot(rf_ns_cm, type = "heatmap")

# Sensitivity and specificity
rf_ns_sens <- (rf_ns_results %>% filter(face_mask_wearing == "yes") %>%
                 summarise(sens = mean(.pred_class == "yes")))$sens
rf_ns_spec <- (rf_ns_results %>% filter(face_mask_wearing == "no") %>%
                 summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(rf_ns_sens, 3), "\n")
cat("Specificity:", round(rf_ns_spec, 3), "\n")

# ROC curve
rf_ns_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - rf_ns_spec, color = "red") +
  geom_hline(yintercept = rf_ns_sens,     color = "red") +
  labs(title = "No State: Random Forest — ROC Curve")

# AUC
rf_ns_auc <- rf_ns_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("No State Random Forest AUC:", round(rf_ns_auc$.estimate, 3), "\n")

# Variable importance
rf_ns %>% vip() + labs(title = "No State: Random Forest — Variable Importance")
rf_ns %>% vi()

# With state Train/Test

set.seed(1914220)
df_split <- initial_split(df_model, prop = 0.7)
df_train <- training(df_split)
df_test  <- testing(df_split)

# Logistic Regression

lrfit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(face_mask_wearing ~ ., data = df_train)

# Coefficients and p-values
summary(lrfit$fit)

# Overall significance of each predictor
Anova(lrfit$fit)

# Predictions on test set
lr_preds   <- predict(lrfit, new_data = df_test, type = "class")
lr_probs   <- predict(lrfit, new_data = df_test, type = "prob")
lr_results <- bind_cols(lr_preds, lr_probs, df_test)

# Confusion matrix
lr_cm <- lr_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(lr_cm)
autoplot(lr_cm, type = "heatmap")

# Sensitivity and specificity
lr_sens <- (lr_results %>% filter(face_mask_wearing == "yes") %>%
              summarise(sens = mean(.pred_class == "yes")))$sens
lr_spec <- (lr_results %>% filter(face_mask_wearing == "no") %>%
              summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(lr_sens, 3), "\n")
cat("Specificity:", round(lr_spec, 3), "\n")

# ROC curve
lr_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - lr_spec, color = "red") +
  geom_hline(yintercept = lr_sens,     color = "red") +
  labs(title = "Logistic Regression — ROC Curve")

# AUC
lr_auc <- lr_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("Logistic Regression AUC:", round(lr_auc$.estimate, 3), "\n")


# Classification Tree

ctree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(face_mask_wearing ~ ., data = df_train)

rpart.plot(ctree$fit, extra = 1, type = 2,
           main = "Classification Tree — Face Mask Wearing")

# Predictions on test set
ct_preds   <- predict(ctree, new_data = df_test, type = "class")
ct_probs   <- predict(ctree, new_data = df_test, type = "prob")
ct_results <- bind_cols(ct_preds, ct_probs, df_test)

# Confusion matrix
ct_cm <- ct_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(ct_cm)
autoplot(ct_cm, type = "heatmap")

# Sensitivity and specificity
ct_sens <- (ct_results %>% filter(face_mask_wearing == "yes") %>%
              summarise(sens = mean(.pred_class == "yes")))$sens
ct_spec <- (ct_results %>% filter(face_mask_wearing == "no") %>%
              summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(ct_sens, 3), "\n")
cat("Specificity:", round(ct_spec, 3), "\n")

# ROC curve
ct_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - ct_spec, color = "red") +
  geom_hline(yintercept = ct_sens,     color = "red") +
  labs(title = "Classification Tree — ROC Curve")

# AUC
ct_auc <- ct_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("Classification Tree AUC:", round(ct_auc$.estimate, 3), "\n")

# Variable importance
ctree %>% vip() + labs(title = "Classification Tree — Variable Importance")
ctree %>% vi()

# Random Forest

rf <- rand_forest(mode = "classification") %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(face_mask_wearing ~ ., data = df_train)

# Predictions on test set
rf_preds   <- predict(rf, new_data = df_test, type = "class")
rf_probs   <- predict(rf, new_data = df_test, type = "prob")
rf_results <- bind_cols(rf_preds, rf_probs, df_test)

# Confusion matrix
rf_cm <- rf_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(rf_cm)
autoplot(rf_cm, type = "heatmap")

# Sensitivity and specificity
rf_sens <- (rf_results %>% filter(face_mask_wearing == "yes") %>%
              summarise(sens = mean(.pred_class == "yes")))$sens
rf_spec <- (rf_results %>% filter(face_mask_wearing == "no") %>%
              summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(rf_sens, 3), "\n")
cat("Specificity:", round(rf_spec, 3), "\n")

# ROC curve
rf_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - rf_spec, color = "red") +
  geom_hline(yintercept = rf_sens,     color = "red") +
  labs(title = "Random Forest — ROC Curve")

# AUC
rf_auc <- rf_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("Random Forest AUC:", round(rf_auc$.estimate, 3), "\n")

# Variable importance
rf %>% vip() + labs(title = "Random Forest — Variable Importance")
rf %>% vi()

#Phase 2 split

set.seed(1914220)
df_split_p2 <- initial_split(df_model_phase2, prop = 0.7)
df_train_p2 <- training(df_split_p2)
df_test_p2  <- testing(df_split_p2)

# Phase 2 Logistic Regression

lr_p2 <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(face_mask_wearing ~ ., data = df_train_p2)

# Coefficients and p-values
summary(lr_p2$fit)

# Overall significance of each predictor
Anova(lr_p2$fit)

# Predictions on test set
lr_p2_preds   <- predict(lr_p2, new_data = df_test_p2, type = "class")
lr_p2_probs   <- predict(lr_p2, new_data = df_test_p2, type = "prob")
lr_p2_results <- bind_cols(lr_p2_preds, lr_p2_probs, df_test_p2)

# Confusion matrix
lr_p2_cm <- lr_p2_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(lr_p2_cm)
autoplot(lr_p2_cm, type = "heatmap")

# Sensitivity and specificity
lr_p2_sens <- (lr_p2_results %>% filter(face_mask_wearing == "yes") %>%
                 summarise(sens = mean(.pred_class == "yes")))$sens
lr_p2_spec <- (lr_p2_results %>% filter(face_mask_wearing == "no") %>%
                 summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(lr_p2_sens, 3), "\n")
cat("Specificity:", round(lr_p2_spec, 3), "\n")

# ROC curve
lr_p2_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - lr_p2_spec, color = "red") +
  geom_hline(yintercept = lr_p2_sens,     color = "red") +
  labs(title = "Phase 2: Logistic Regression — ROC Curve")

# AUC
lr_p2_auc <- lr_p2_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("Phase 2 Logistic Regression AUC:", round(lr_p2_auc$.estimate, 3), "\n")


# Phase 2 Classification Tree

ct_p2 <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(face_mask_wearing ~ ., data = df_train_p2)

rpart.plot(ct_p2$fit, extra = 1, type = 2,
           main = "Phase 2: Classification Tree")

# Predictions on test set
ct_p2_preds   <- predict(ct_p2, new_data = df_test_p2, type = "class")
ct_p2_probs   <- predict(ct_p2, new_data = df_test_p2, type = "prob")
ct_p2_results <- bind_cols(ct_p2_preds, ct_p2_probs, df_test_p2)

# Confusion matrix
ct_p2_cm <- ct_p2_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(ct_p2_cm)
autoplot(ct_p2_cm, type = "heatmap")

# Sensitivity and specificity
ct_p2_sens <- (ct_p2_results %>% filter(face_mask_wearing == "yes") %>%
                 summarise(sens = mean(.pred_class == "yes")))$sens
ct_p2_spec <- (ct_p2_results %>% filter(face_mask_wearing == "no") %>%
                 summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(ct_p2_sens, 3), "\n")
cat("Specificity:", round(ct_p2_spec, 3), "\n")

# ROC curve
ct_p2_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - ct_p2_spec, color = "red") +
  geom_hline(yintercept = ct_p2_sens,     color = "red") +
  labs(title = "Phase 2: Classification Tree — ROC Curve")

# AUC
ct_p2_auc <- ct_p2_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("Phase 2 Classification Tree AUC:", round(ct_p2_auc$.estimate, 3), "\n")

# Variable importance
ct_p2 %>% vip() + labs(title = "Phase 2: Classification Tree — Variable Importance")
ct_p2 %>% vi()


# Phase 2 Random Forest

rf_p2 <- rand_forest(mode = "classification") %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(face_mask_wearing ~ ., data = df_train_p2)

# Predictions on test set
rf_p2_preds   <- predict(rf_p2, new_data = df_test_p2, type = "class")
rf_p2_probs   <- predict(rf_p2, new_data = df_test_p2, type = "prob")
rf_p2_results <- bind_cols(rf_p2_preds, rf_p2_probs, df_test_p2)

# Confusion matrix
rf_p2_cm <- rf_p2_results %>% conf_mat(.pred_class, truth = face_mask_wearing)
print(rf_p2_cm)
autoplot(rf_p2_cm, type = "heatmap")

# Sensitivity and specificity
rf_p2_sens <- (rf_p2_results %>% filter(face_mask_wearing == "yes") %>%
                 summarise(sens = mean(.pred_class == "yes")))$sens
rf_p2_spec <- (rf_p2_results %>% filter(face_mask_wearing == "no") %>%
                 summarise(spec = mean(.pred_class == "no")))$spec

cat("Sensitivity:", round(rf_p2_sens, 3), "\n")
cat("Specificity:", round(rf_p2_spec, 3), "\n")

# ROC curve
rf_p2_results %>%
  roc_curve(.pred_yes, truth = face_mask_wearing, event_level = "second") %>%
  autoplot() +
  geom_vline(xintercept = 1 - rf_p2_spec, color = "red") +
  geom_hline(yintercept = rf_p2_sens,     color = "red") +
  labs(title = "Phase 2: Random Forest — ROC Curve")

# AUC
rf_p2_auc <- rf_p2_results %>% roc_auc(.pred_yes, truth = face_mask_wearing, event_level = "second")
cat("Phase 2 Random Forest AUC:", round(rf_p2_auc$.estimate, 3), "\n")

# Variable importance
rf_p2 %>% vip() + labs(title = "Phase 2: Random Forest — Variable Importance")
rf_p2 %>% vi()

# Model comparison summary

lr_ns_acc <- (lr_ns_cm$table[1,1] + lr_ns_cm$table[2,2]) / sum(lr_ns_cm$table)
ct_ns_acc <- (ct_ns_cm$table[1,1] + ct_ns_cm$table[2,2]) / sum(ct_ns_cm$table)
rf_ns_acc <- (rf_ns_cm$table[1,1] + rf_ns_cm$table[2,2]) / sum(rf_ns_cm$table)

lr_acc    <- (lr_cm$table[1,1]    + lr_cm$table[2,2])    / sum(lr_cm$table)
ct_acc    <- (ct_cm$table[1,1]    + ct_cm$table[2,2])    / sum(ct_cm$table)
rf_acc    <- (rf_cm$table[1,1]    + rf_cm$table[2,2])    / sum(rf_cm$table)

lr_p2_acc <- (lr_p2_cm$table[1,1] + lr_p2_cm$table[2,2]) / sum(lr_p2_cm$table)
ct_p2_acc <- (ct_p2_cm$table[1,1] + ct_p2_cm$table[2,2]) / sum(ct_p2_cm$table)
rf_p2_acc <- (rf_p2_cm$table[1,1] + rf_p2_cm$table[2,2]) / sum(rf_p2_cm$table)

tibble(
  Phase       = c("No State", "No State", "No State",
                  "Phase 1 (with State)", "Phase 1 (with State)", "Phase 1 (with State)",
                  "Phase 2", "Phase 2", "Phase 2"),
  Model       = c("Logistic Regression", "Classification Tree", "Random Forest",
                  "Logistic Regression", "Classification Tree", "Random Forest",
                  "Logistic Regression", "Classification Tree", "Random Forest"),
  AUC         = c(round(lr_ns_auc$.estimate, 3), round(ct_ns_auc$.estimate, 3), round(rf_ns_auc$.estimate, 3),
                  round(lr_auc$.estimate,    3), round(ct_auc$.estimate,    3), round(rf_auc$.estimate,    3),
                  round(lr_p2_auc$.estimate, 3), round(ct_p2_auc$.estimate, 3), round(rf_p2_auc$.estimate, 3)),
  Sensitivity = c(round(lr_ns_sens, 3), round(ct_ns_sens, 3), round(rf_ns_sens, 3),
                  round(lr_sens,    3), round(ct_sens,    3), round(rf_sens,    3),
                  round(lr_p2_sens, 3), round(ct_p2_sens, 3), round(rf_p2_sens, 3)),
  Specificity = c(round(lr_ns_spec, 3), round(ct_ns_spec, 3), round(rf_ns_spec, 3),
                  round(lr_spec,    3), round(ct_spec,    3), round(rf_spec,    3),
                  round(lr_p2_spec, 3), round(ct_p2_spec, 3), round(rf_p2_spec, 3)),
  Accuracy    = c(round(lr_ns_acc, 3), round(ct_ns_acc, 3), round(rf_ns_acc, 3),
                  round(lr_acc,    3), round(ct_acc,    3), round(rf_acc,    3),
                  round(lr_p2_acc, 3), round(ct_p2_acc, 3), round(rf_p2_acc, 3))
) %>% print()
