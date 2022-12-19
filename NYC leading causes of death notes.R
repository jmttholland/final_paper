library(tidyverse)
options(scipen = 99999)

# Importing data
setwd("~/Documents/CCNY Eco MA/B2000/FInal paper")
data <- read.csv("New_York_City_Leading_Causes_of_Death.csv")

# Defining functions
diff_of_means_test_stat <- function (var1, var2, n1, n2, diff) {
  se <- sqrt((var1/n1)+(var2/n2))
  test_stat <- diff/se
  return(test_stat)
}

# Cleaning up data and removing rows where death rate is unavailable as it is our only continuous variable
data$Age.Adjusted.Death.Rate <-  as.numeric(data$Age.Adjusted.Death.Rate)
data$Year <- as.factor(data$Year)
data$Death.Rate <- as.numeric(data$Death.Rate)
data$Race.Ethnicity <- as.factor(data$Race.Ethnicity)
data$Leading.Cause <- as.factor(data$Leading.Cause)
data$Deaths <- as.numeric(data$Deaths)
data$Sex[data$Sex == "F"] <- "Female" # Because some are coded as F
data$Sex[data$Sex == "M"] <- "Male" # Because some are coded as M
data$Sex <- as.factor(data$Sex)
good_data <- complete.cases(data$Age.Adjusted.Death.Rate > 0.1)
useable_data <- subset(data, good_data)
nrow(useable_data) # We now now have 819 rows total after death rate NAs are removed

levels(useable_data$Leading.Cause)
# Changing names of causes for 2019 to correspond to earlier years as there are some changes in coding in 2019 and dropping those factors
useable_data$Leading.Cause[useable_data$Leading.Cause == "Assault (Homicide: U01-U02, Y87.1, X85-Y09)"] <- "Assault (Homicide: Y87.1, X85-Y09)" 
useable_data$Leading.Cause[useable_data$Leading.Cause == 
                             "Accidents Except Drug Poisoning (V01-X39, X43, X45-X59, Y85-Y86)"] <- "Accidents Except Drug Posioning (V01-X39, X43, X45-X59, Y85-Y86)"
useable_data$Leading.Cause[useable_data$Leading.Cause == "Chronic Liver Disease and Cirrhosis (K70, K73-K74)"] <- "Chronic Liver Disease and Cirrhosis (K70, K73)"
useable_data$Leading.Cause[useable_data$Leading.Cause == 
                             "Intentional Self-Harm (Suicide: U03, X60-X84, Y87.0)"] <- "Intentional Self-Harm (Suicide: X60-X84, Y87.0)"
levels(useable_data$Leading.Cause)
useable_data$Leading.Cause <- factor(useable_data$Leading.Cause)
# Adding year dummies
year_logicals <- NULL
for (year in levels(useable_data$Year)) {
  logi_year <- useable_data$Year == year
  year_logicals <- cbind(year_logicals, logi_year)
}
year_logicals <- as.data.frame(year_logicals)
colnames(year_logicals) <- levels(useable_data$Year)
useable_data <- cbind(useable_data, year_logicals)

# Adding race dummies
useable_data <- cbind(useable_data, 
                      Hispanic = useable_data$Race.Ethnicity == "Hispanic",
                      Black = useable_data$Race.Ethnicity == "Black Non-Hispanic" |
                                          useable_data$Race.Ethnicity == "Non-Hispanic Black",
                      White = useable_data$Race.Ethnicity == "Non-Hispanic White" |
                                          useable_data$Race.Ethnicity == "White Non-Hispanic",
                      Asian_and_Pacific_Islander = useable_data$Race.Ethnicity == "Asian and Pacific Islander",
                      Other = useable_data$Race.Ethnicity == "Other Race/ Ethnicity"
)

# Adding leading causes dummies
leading_cause_logicals <- NULL
for (cause in levels(useable_data$Leading.Cause)) {
  logi_cause <- useable_data$Leading.Cause == cause
  leading_cause_logicals <- cbind(leading_cause_logicals, logi_cause)
}
leading_cause_logicals <- as.data.frame(leading_cause_logicals)
colnames(leading_cause_logicals) <- levels(useable_data$Leading.Cause)
useable_data <- cbind(useable_data, leading_cause_logicals)
leading_causes <- as.data.frame(summary(useable_data$Leading.Cause))

# Dividing age adjusted death rate by 100,000 to get 'p' for each given observation (as
# each death rate stat is out of 100,000) and adding variance column for age-adjusted death rate
useable_data <- cbind(useable_data, age_adjusted_p = useable_data$Age.Adjusted.Death.Rate / 100000)
useable_data <- cbind(useable_data, 
                      age_adjusted_var = useable_data$age_adjusted_p*(1 - useable_data$age_adjusted_p))

# Adding race/ethnicity column to correspond to dummy race categories
useable_data <- cbind(useable_data, Dummy.Race = as.factor(ifelse(useable_data$Race.Ethnicity == "Hispanic", "Hispanic", 
                                           ifelse(useable_data$Race.Ethnicity == "Black Non-Hispanic" |
                                                    useable_data$Race.Ethnicity == "Non-Hispanic Black", "Black",
                                                  ifelse(useable_data$Race.Ethnicity == "Non-Hispanic White" |
                                                           useable_data$Race.Ethnicity == "White Non-Hispanic", "White",
                                                         ifelse(useable_data$Race.Ethnicity == "Asian and Pacific Islander", "Asian and Pacific Islander", 
                                                                ifelse(useable_data$Race.Ethnicity == "Other Race/ Ethnicity" |
                                                                         useable_data$Race.Ethnicity == "Not Stated/Unknown", "Other","")))))))
levels(useable_data$Race.Ethnicity)
levels(useable_data$Dummy.Race)

# Dropping unused causes from factor
levels(useable_data$Leading.Cause)
useable_data$Leading.Cause <-  factor(useable_data$Leading.Cause)
levels(useable_data$Leading.Cause)

# Creating male and female subsets
useable_male <- subset(useable_data, useable_data$Sex == "Male")
useable_female <- subset(useable_data, useable_data$Sex == "Female")

# Creating yearly subsets and dropping unused causes from factor for each year
list_of_years <- split(useable_data, useable_data$Year)
colnames(list_of_years$`2007`) <- colnames(useable_data)
colnames(list_of_years$`2008`) <- colnames(useable_data)
colnames(list_of_years$`2009`) <- colnames(useable_data)
colnames(list_of_years$`2010`) <- colnames(useable_data)
colnames(list_of_years$`2011`) <- colnames(useable_data)
colnames(list_of_years$`2012`) <- colnames(useable_data)
colnames(list_of_years$`2013`) <- colnames(useable_data)
colnames(list_of_years$`2014`) <- colnames(useable_data)
colnames(list_of_years$`2019`) <- colnames(useable_data)
data_2007 <- as.data.frame(list_of_years$`2007`)
data_2008 <- as.data.frame(list_of_years$`2008`)
data_2009 <- as.data.frame(list_of_years$`2009`)
data_2010 <- as.data.frame(list_of_years$`2010`)
data_2011 <- as.data.frame(list_of_years$`2011`)
data_2012 <- as.data.frame(list_of_years$`2012`)
data_2013 <- as.data.frame(list_of_years$`2013`)
data_2014 <- as.data.frame(list_of_years$`2014`)
data_2019 <- as.data.frame(list_of_years$`2019`)
data_2007 <- cbind(data_2007, leading_cause_factor = factor(data_2007$Leading.Cause))
data_2008 <- cbind(data_2008, leading_cause_factor = factor(data_2008$Leading.Cause))
data_2009 <- cbind(data_2009, leading_cause_factor = factor(data_2009$Leading.Cause))
data_2010 <- cbind(data_2010, leading_cause_factor = factor(data_2010$Leading.Cause))
data_2011 <- cbind(data_2011, leading_cause_factor = factor(data_2011$Leading.Cause))
data_2012 <- cbind(data_2012, leading_cause_factor = factor(data_2012$Leading.Cause))
data_2013 <- cbind(data_2013, leading_cause_factor = factor(data_2013$Leading.Cause))
data_2014 <- cbind(data_2014, leading_cause_factor = factor(data_2014$Leading.Cause))
data_2019 <- cbind(data_2019, leading_cause_factor = factor(data_2019$Leading.Cause))

leading_causes_by_year <- cbind("2007" = as.data.frame(table(data_2007$Leading.Cause)),  
                                "2008" = as.data.frame(table(data_2008$Leading.Cause)), 
                                "2009" = as.data.frame(table(data_2009$Leading.Cause)), 
                                "2010" = as.data.frame(table(data_2010$Leading.Cause)), 
                                "2011" = as.data.frame(table(data_2011$Leading.Cause)), 
                                "2012" = as.data.frame(table(data_2012$Leading.Cause)), 
                                "2013" = as.data.frame(table(data_2013$Leading.Cause)),
                                "2014" = as.data.frame(table(data_2014$Leading.Cause)),
                                "2019" = as.data.frame(table(data_2019$Leading.Cause)))

# Initial graphs to get understanding of sex and race trends by death rate

ggplot(data = useable_data, mapping = aes(x = Dummy.Race, y = Age.Adjusted.Death.Rate, col = Sex)) +
  geom_point() # Highest few death rate points are all male; both Black and White seem to have the highest death rate concentrations
ggplot(data = useable_data, mapping = aes(x = Sex, y = Age.Adjusted.Death.Rate, col = Dummy.Race)) +
  geom_point() # Female death rate more concentrated lower down the death rate axis

# Age-adjusted death rate and dummy race graphs for all years

aggregate_race_regression <- lm(data = useable_data, Age.Adjusted.Death.Rate ~ Hispanic + Black + White + Asian_and_Pacific_Islander)
summary(aggregate_race_regression)
# No overall statistically significant relationship, other than a lower death rate per 100000 for Asians and Pacific Islanders, and only significant with 95% confidence.
# Being black is associated with a small increase in the death rate, but it is not statistically significant in this regression

# 4 logistic regressions, one for each race, to determine likelihood of being one of the four races given a certain death rate
ggplot_dummy_race <- function (dummy_race, y_title) {
  ggplot(data = useable_data, mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(dummy_race), col = Sex)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
    ylab(y_title)
}
ggplot_dummy_race(dummy_race = useable_data$Hispanic, y_title = "Hispanic")
ggplot_dummy_race(dummy_race = useable_data$Black, y_title = "Black")
ggplot_dummy_race(dummy_race = useable_data$White, y_title = "White")
ggplot_dummy_race(dummy_race = useable_data$Asian_and_Pacific_Islander, y_title = "Asian and Pacific Islander")
# These logistic regression plots give us an idea that there is a higher chance of being black vs all other races as the death rate increases across all leading causes.
# There also seems to be a negative relationship between being Asian and PI and increasing death rate

# Aggregate race regressions
logit_model_hispanic_all_years <- glm(useable_data$Hispanic ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_all_years) # Hispanic logit
logit_model_black_all_years <- glm(useable_data$Black ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_all_years) # Black logit
logit_model_white_all_years <- glm(useable_data$White ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_all_years) # White logit
logit_model_asian_and_pi_all_years <- glm(useable_data$Asian_and_Pacific_Islander ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_all_years) # Asian and Pacific Islander logit
# Statistically significant relationships exist for logistic regressions on Black and Asian and Pacific Islander populations
coefficients_black <- coefficients(logit_model_black_all_years)
coefficients_asian_and_pi <- coefficients(logit_model_asian_and_pi_all_years)
odds_race_given_death_rate_all_years <- NULL
for (multiplier in c(1, 100, 200, 300, 400, 500)) {
  odds_black_given_death_rate <- exp(coefficients_black[2] * multiplier)
  p_black_given_death_rate <- exp(coefficients_black[2] * multiplier) / (1 + (exp(coefficients_black[2] * multiplier)))
  odds_asian_and_pi_given_death_rate <- exp(coefficients_asian_and_pi[2] * multiplier)
  p_asian_and_pi_given_death_rate <-exp(coefficients_asian_and_pi[2] * multiplier) / (1 + (exp(coefficients_asian_and_pi[2] * multiplier)))
  odds_race_given_death_rate_all_years <- rbind(odds_race_given_death_rate_all_years, 
                                 c(multiplier, odds_black_given_death_rate, p_black_given_death_rate, odds_asian_and_pi_given_death_rate, p_asian_and_pi_given_death_rate))
}
colnames(odds_race_given_death_rate_all_years) <- c("Increase in death rate", "Black odds change", "Probability Black", "Asian and PI odds change", "Probability Asian and PI")
    # For an increase in the death rate of 100, the odds of being black increases by about 50%
    # A similar but negative relationship exists for the Asian and Pacific Islander Population

# Age-adjusted death rate and sex

aggregate_sex_regression <- lm(data = useable_data, Age.Adjusted.Death.Rate ~ Sex)
summary(aggregate_sex_regression)
# Statistically significant relationship. Being male is associated with, on average for all years and all leading causes, 21 more deaths per 100000 people.
ggplot(data = useable_data, mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(Sex)-1, col = Dummy.Race)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  ylab("male = 1")
logit_model_male_all_years <- glm((as.numeric(useable_data$Sex) - 1) ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_male_all_years)
# This logistic regression for sex shows that there is a statistically significant relationship between a higher death rate and the odds of being male
coefficients_sex <- coefficients(logit_model_male_all_years)
odds_male_given_death_rate_all_years <- NULL
for (multiplier in c(1, 100, 200, 300, 400, 500)) {
  odds_male_given_death_rate <- exp(coefficients_sex[2] * multiplier)
  p_male_given_death_rate <- (exp(coefficients_sex[2] * multiplier)) / (1 + (exp(coefficients_sex[2] * multiplier)))
  odds_male_given_death_rate_all_years <- rbind(odds_male_given_death_rate_all_years, 
                                                c(multiplier, odds_male_given_death_rate, p_male_given_death_rate))
}
colnames(odds_male_given_death_rate_all_years) <- c("Increase in death rate", "Male odds change", "Probability odds change")
odds_male_given_death_rate_all_years <- as.data.frame(odds_male_given_death_rate_all_years)
# An increase of 100 in the death rate is associated with an increase in the odds of being male of about 100%

# Analysis by year
# 2019

levels(data_2019$Leading.Cause)

# Creating list of causes for which there is an 
# age adjusted death rate for each of the 4 races and for both sexes (for 2019)
complete_race_and_sex_causes_2019 <- NULL
for (cause in levels(data_2019$Leading.Cause)) {
  subset_by_cause <- subset(data_2019, data_2019$Leading.Cause == cause)
  if(sum(subset_by_cause[, 17] == TRUE & subset_by_cause[, 3] == "Male") >= 1 & #Hispanic and male
      sum(subset_by_cause[, 17] == TRUE & subset_by_cause[, 3] == "Female") >= 1 & #Hispanic and female
      sum(subset_by_cause[, 18] == TRUE & subset_by_cause[, 3] == "Male") >= 1 & # Black and male
      sum(subset_by_cause[, 18] == TRUE & subset_by_cause[, 3] == "Female") >= 1 & # Black and female
      sum(subset_by_cause[, 19] == TRUE & subset_by_cause[, 3] == "Male") >= 1 & # White and male
      sum(subset_by_cause[, 19] == TRUE & subset_by_cause[, 3] == "Female") >= 1 & # White and female
      sum(subset_by_cause[, 20] == TRUE & subset_by_cause[, 3] == "Male") >= 1 & # Asian and male
      sum(subset_by_cause[, 20] == TRUE & subset_by_cause[, 3] == "Female") >= 1) { # Asian and female
    complete_race_and_sex_causes_2019 <- cbind(complete_race_and_sex_causes_2019, cause)
  } 
}
# There are 8 leading causes for which there are at least one male and one female 
# age-adjusted death rate for each race for us to use to compare means from 2019

# Hypothesis tests for difference in means across races in 2019 per leading cause

# Means tested against White age-adjusted death rate for males
mean_differences_2019_males_vs_white <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(17, 18, 20)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, 19] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Male" &
                                        data_2019[, 19] == TRUE &
                                        data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                            var2 = var_2,
                            n1 = 100000,
                            n2 = 100000,
                            diff = p_1 - p_2)
    mean_differences_2019_males_vs_white <- rbind(mean_differences_2019_males_vs_white, 
                                                  c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_males_vs_white) <- c("Cause", "Race", "Stat")
mean_differences_2019_males_vs_white <- as.data.frame(mean_differences_2019_males_vs_white)
mean_differences_2019_males_vs_white$Race[mean_differences_2019_males_vs_white$Race == 17] <- "Hispanic"
mean_differences_2019_males_vs_white$Race[mean_differences_2019_males_vs_white$Race == 18] <- "Black"
mean_differences_2019_males_vs_white$Race[mean_differences_2019_males_vs_white$Race == 20] <- "Asian_and_Pacific_Islander"
mean_differences_2019_males_vs_white <- cbind(mean_differences_2019_males_vs_white,
                                              p_value = pnorm(abs(as.numeric(mean_differences_2019_males_vs_white$Stat)), lower.tail = FALSE))
significance_vs_white_male <- ifelse(mean_differences_2019_males_vs_white$p_value > .05, "Not significant at 95% confidence",
                       (ifelse(between(mean_differences_2019_males_vs_white$p_value, .02, .05), "Significant at 95%",
                       (ifelse(between(mean_differences_2019_males_vs_white$p_value, .01, .02), "Significant at 98%",
                       (ifelse(mean_differences_2019_males_vs_white$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_males_vs_white <- cbind(mean_differences_2019_males_vs_white, significance_vs_white_male)

# Means tested against White age-adjusted death rate for females
mean_differences_2019_females_vs_white <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(17, 18, 20)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, 19] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Female" &
                                      data_2019[, 19] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_females_vs_white <- rbind(mean_differences_2019_females_vs_white, 
                                                  c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_females_vs_white) <- c("Cause", "Race", "Stat")
mean_differences_2019_females_vs_white <- as.data.frame(mean_differences_2019_females_vs_white)
mean_differences_2019_females_vs_white$Race[mean_differences_2019_females_vs_white$Race == 17] <- "Hispanic"
mean_differences_2019_females_vs_white$Race[mean_differences_2019_females_vs_white$Race == 18] <- "Black"
mean_differences_2019_females_vs_white$Race[mean_differences_2019_females_vs_white$Race == 20] <- "Asian_and_Pacific_Islander"
mean_differences_2019_females_vs_white <- cbind(mean_differences_2019_females_vs_white,
                                              p_value = pnorm(abs(as.numeric(mean_differences_2019_females_vs_white$Stat)), lower.tail = FALSE))
significance_vs_white_female <- ifelse(mean_differences_2019_females_vs_white$p_value > .05, "Not significant at 95% confidence",
                                     (ifelse(between(mean_differences_2019_females_vs_white$p_value, .02, .05), "Significant at 95%",
                                             (ifelse(between(mean_differences_2019_females_vs_white$p_value, .01, .02), "Significant at 98%",
                                                     (ifelse(mean_differences_2019_females_vs_white$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_females_vs_white <- cbind(mean_differences_2019_females_vs_white, significance_vs_white_female)

# Means tested against Hispanic age-adjusted death rate for males
mean_differences_2019_males_vs_hispanic <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(18, 19, 20)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, 17] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Male" &
                                      data_2019[, 17] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_males_vs_hispanic <- rbind(mean_differences_2019_males_vs_hispanic, 
                                                  c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_males_vs_hispanic) <- c("Cause", "Race", "Stat")
mean_differences_2019_males_vs_hispanic <- as.data.frame(mean_differences_2019_males_vs_hispanic)
mean_differences_2019_males_vs_hispanic$Race[mean_differences_2019_males_vs_hispanic$Race == 19] <- "White"
mean_differences_2019_males_vs_hispanic$Race[mean_differences_2019_males_vs_hispanic$Race == 18] <- "Black"
mean_differences_2019_males_vs_hispanic$Race[mean_differences_2019_males_vs_hispanic$Race == 20] <- "Asian_and_Pacific_Islander"
mean_differences_2019_males_vs_hispanic <- cbind(mean_differences_2019_males_vs_hispanic,
                                              p_value = pnorm(abs(as.numeric(mean_differences_2019_males_vs_hispanic$Stat)), lower.tail = FALSE))
significance_vs_hispanic_male <- ifelse(mean_differences_2019_males_vs_hispanic$p_value > .05, "Not significant at 95% confidence",
                                     (ifelse(between(mean_differences_2019_males_vs_hispanic$p_value, .02, .05), "Significant at 95%",
                                             (ifelse(between(mean_differences_2019_males_vs_hispanic$p_value, .01, .02), "Significant at 98%",
                                                     (ifelse(mean_differences_2019_males_vs_hispanic$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_males_vs_hispanic <- cbind(mean_differences_2019_males_vs_hispanic, significance_vs_hispanic_male)

# Means tested against Hispanic age-adjusted death rate for females
mean_differences_2019_females_vs_hispanic <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(18, 19, 20)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, 17] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Female" &
                                      data_2019[, 17] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_females_vs_hispanic <- rbind(mean_differences_2019_females_vs_hispanic, 
                                                    c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_females_vs_hispanic) <- c("Cause", "Race", "Stat")
mean_differences_2019_females_vs_hispanic <- as.data.frame(mean_differences_2019_females_vs_hispanic)
mean_differences_2019_females_vs_hispanic$Race[mean_differences_2019_females_vs_hispanic$Race == 19] <- "White"
mean_differences_2019_females_vs_hispanic$Race[mean_differences_2019_females_vs_hispanic$Race == 18] <- "Black"
mean_differences_2019_females_vs_hispanic$Race[mean_differences_2019_females_vs_hispanic$Race == 20] <- "Asian_and_Pacific_Islander"
mean_differences_2019_females_vs_hispanic <- cbind(mean_differences_2019_females_vs_hispanic,
                                                p_value = pnorm(abs(as.numeric(mean_differences_2019_females_vs_hispanic$Stat)), lower.tail = FALSE))
significance_vs_hispanic_female <- ifelse(mean_differences_2019_females_vs_hispanic$p_value > .05, "Not significant at 95% confidence",
                                       (ifelse(between(mean_differences_2019_females_vs_hispanic$p_value, .02, .05), "Significant at 95%",
                                               (ifelse(between(mean_differences_2019_females_vs_hispanic$p_value, .01, .02), "Significant at 98%",
                                                       (ifelse(mean_differences_2019_females_vs_hispanic$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_females_vs_hispanic <- cbind(mean_differences_2019_females_vs_hispanic, significance_vs_hispanic_female)

# Means tested against Black age-adjusted death rate for males
mean_differences_2019_males_vs_black <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(17, 19, 20)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, 18] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Male" &
                                      data_2019[, 18] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_males_vs_black <- rbind(mean_differences_2019_males_vs_black, 
                                                     c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_males_vs_black) <- c("Cause", "Race", "Stat")
mean_differences_2019_males_vs_black <- as.data.frame(mean_differences_2019_males_vs_black)
mean_differences_2019_males_vs_black$Race[mean_differences_2019_males_vs_black$Race == 19] <- "White"
mean_differences_2019_males_vs_black$Race[mean_differences_2019_males_vs_black$Race == 17] <- "Hispanic"
mean_differences_2019_males_vs_black$Race[mean_differences_2019_males_vs_black$Race == 20] <- "Asian_and_Pacific_Islander"
mean_differences_2019_males_vs_black <- cbind(mean_differences_2019_males_vs_black,
                                                 p_value = pnorm(abs(as.numeric(mean_differences_2019_males_vs_black$Stat)), lower.tail = FALSE))
significance_vs_black_male <- ifelse(mean_differences_2019_males_vs_black$p_value > .05, "Not significant at 95% confidence",
                                        (ifelse(between(mean_differences_2019_males_vs_black$p_value, .02, .05), "Significant at 95%",
                                                (ifelse(between(mean_differences_2019_males_vs_black$p_value, .01, .02), "Significant at 98%",
                                                        (ifelse(mean_differences_2019_males_vs_black$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_males_vs_black <- cbind(mean_differences_2019_males_vs_black, significance_vs_black_male)

# Means tested against Black age-adjusted death rate for females
mean_differences_2019_females_vs_black <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(17, 19, 20)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, 18] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Female" &
                                      data_2019[, 18] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_females_vs_black <- rbind(mean_differences_2019_females_vs_black, 
                                                       c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_females_vs_black) <- c("Cause", "Race", "Stat")
mean_differences_2019_females_vs_black <- as.data.frame(mean_differences_2019_females_vs_black)
mean_differences_2019_females_vs_black$Race[mean_differences_2019_females_vs_black$Race == 19] <- "White"
mean_differences_2019_females_vs_black$Race[mean_differences_2019_females_vs_black$Race == 17] <- "Hispanic"
mean_differences_2019_females_vs_black$Race[mean_differences_2019_females_vs_black$Race == 20] <- "Asian_and_Pacific_Islander"
mean_differences_2019_females_vs_black <- cbind(mean_differences_2019_females_vs_black,
                                                   p_value = pnorm(abs(as.numeric(mean_differences_2019_females_vs_black$Stat)), lower.tail = FALSE))
significance_vs_black_female <- ifelse(mean_differences_2019_females_vs_black$p_value > .05, "Not significant at 95% confidence",
                                          (ifelse(between(mean_differences_2019_females_vs_black$p_value, .02, .05), "Significant at 95%",
                                                  (ifelse(between(mean_differences_2019_females_vs_black$p_value, .01, .02), "Significant at 98%",
                                                          (ifelse(mean_differences_2019_females_vs_black$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_females_vs_black <- cbind(mean_differences_2019_females_vs_black, significance_vs_black_female)

# Means tested against Asian and Pacific Islander age-adjusted death rate for males
mean_differences_2019_males_vs_asian_and_pi <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(17, 18, 19)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, 20] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Male" &
                                      data_2019[, 20] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Male" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_males_vs_asian_and_pi <- rbind(mean_differences_2019_males_vs_asian_and_pi, 
                                                  c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_males_vs_asian_and_pi) <- c("Cause", "Race", "Stat")
mean_differences_2019_males_vs_asian_and_pi <- as.data.frame(mean_differences_2019_males_vs_asian_and_pi)
mean_differences_2019_males_vs_asian_and_pi$Race[mean_differences_2019_males_vs_asian_and_pi$Race == 19] <- "White"
mean_differences_2019_males_vs_asian_and_pi$Race[mean_differences_2019_males_vs_asian_and_pi$Race == 17] <- "Hispanic"
mean_differences_2019_males_vs_asian_and_pi$Race[mean_differences_2019_males_vs_asian_and_pi$Race == 18] <- "Black"
mean_differences_2019_males_vs_asian_and_pi <- cbind(mean_differences_2019_males_vs_asian_and_pi,
                                              p_value = pnorm(abs(as.numeric(mean_differences_2019_males_vs_asian_and_pi$Stat)), lower.tail = FALSE))
significance_vs_asian_and_pi_male <- ifelse(mean_differences_2019_males_vs_asian_and_pi$p_value > .05, "Not significant at 95% confidence",
                                     (ifelse(between(mean_differences_2019_males_vs_asian_and_pi$p_value, .02, .05), "Significant at 95%",
                                             (ifelse(between(mean_differences_2019_males_vs_asian_and_pi$p_value, .01, .02), "Significant at 98%",
                                                     (ifelse(mean_differences_2019_males_vs_asian_and_pi$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_males_vs_asian_and_pi <- cbind(mean_differences_2019_males_vs_asian_and_pi, significance_vs_asian_and_pi_male)

# Means tested against Asian and Pacific Islander age-adjusted death rate for females
mean_differences_2019_females_vs_asian_and_pi <- NULL
for (cause in complete_race_and_sex_causes_2019) {
  for (race in c(17, 18, 19)) {
    var_1 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, 20] == TRUE &
                                          data_2019$Leading.Cause == cause]
    var_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                          data_2019[, race] == TRUE &
                                          data_2019$Leading.Cause == cause]
    p_1 <- data_2019$age_adjusted_p[data_2019$Sex == "Female" &
                                      data_2019[, 20] == TRUE &
                                      data_2019$Leading.Cause == cause]
    p_2 <- data_2019$age_adjusted_var[data_2019$Sex == "Female" &
                                        data_2019[, race] == TRUE &
                                        data_2019$Leading.Cause == cause]
    test_stat <- diff_of_means_test_stat(var1 = var_1,
                                         var2 = var_2,
                                         n1 = 100000,
                                         n2 = 100000,
                                         diff = p_1 - p_2)
    mean_differences_2019_females_vs_asian_and_pi <- rbind(mean_differences_2019_females_vs_asian_and_pi, 
                                                    c(cause, race, test_stat))
  }
}
colnames(mean_differences_2019_females_vs_asian_and_pi) <- c("Cause", "Race", "Stat")
mean_differences_2019_females_vs_asian_and_pi <- as.data.frame(mean_differences_2019_females_vs_asian_and_pi)
mean_differences_2019_females_vs_asian_and_pi$Race[mean_differences_2019_females_vs_asian_and_pi$Race == 19] <- "White"
mean_differences_2019_females_vs_asian_and_pi$Race[mean_differences_2019_females_vs_asian_and_pi$Race == 17] <- "Hispanic"
mean_differences_2019_females_vs_asian_and_pi$Race[mean_differences_2019_females_vs_asian_and_pi$Race == 18] <- "Black"
mean_differences_2019_females_vs_asian_and_pi <- cbind(mean_differences_2019_females_vs_asian_and_pi,
                                                p_value = pnorm(abs(as.numeric(mean_differences_2019_females_vs_asian_and_pi$Stat)), lower.tail = FALSE))
significance_vs_asian_and_pi_female <- ifelse(mean_differences_2019_females_vs_asian_and_pi$p_value > .05, "Not significant at 95% confidence",
                                       (ifelse(between(mean_differences_2019_females_vs_asian_and_pi$p_value, .02, .05), "Significant at 95%",
                                               (ifelse(between(mean_differences_2019_females_vs_asian_and_pi$p_value, .01, .02), "Significant at 98%",
                                                       (ifelse(mean_differences_2019_females_vs_asian_and_pi$p_value < .01, "Significant at 99%", "")))))))
mean_differences_2019_females_vs_asian_and_pi <- cbind(mean_differences_2019_females_vs_asian_and_pi, significance_vs_asian_and_pi_female)

aadr_mean_differences_by_race_2019 <- list(mean_differences_2019_males_vs_white, mean_differences_2019_females_vs_white,
                                             mean_differences_2019_males_vs_hispanic, mean_differences_2019_females_vs_hispanic,
                                             mean_differences_2019_males_vs_black, mean_differences_2019_females_vs_black,
                                             mean_differences_2019_males_vs_asian_and_pi, mean_differences_2019_females_vs_asian_and_pi)

# Hispanic logistic regressions by race by year
logit_model_hispanic_2019 <- glm(data = data_2019, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2019) # Hispanic logit 2019
logit_model_hispanic_2014 <- glm(data = data_2014, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2014) # Hispanic logit 2014
logit_model_hispanic_2013 <- glm(data = data_2013, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2013) # Hispanic logit 2013
logit_model_hispanic_2012 <- glm(data = data_2012, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2012) # Hispanic logit 2012
logit_model_hispanic_2011 <- glm(data = data_2011, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2011) # Hispanic logit 2011
logit_model_hispanic_2010 <- glm(data = data_2010, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2010) # Hispanic logit 2010
logit_model_hispanic_2009 <- glm(data = data_2009, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2009) # Hispanic logit 2009
logit_model_hispanic_2008 <- glm(data = data_2008, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2008) # Hispanic logit 2008 
logit_model_hispanic_2007 <- glm(data = data_2007, Hispanic ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_2007) # Hispanic logit 2007
# No statistically significant relationships

# Black logit 2019
logit_model_black_2019 <- glm(data = data_2019, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2019) 
ggplot(data = data_2019, mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(Black), col = Sex)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) 
# No statistically significant relationship on black 2019 logit
logit_model_black_2019_female <- glm(data = subset(useable_female, useable_female$Year == "2019"), Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2019_female) 
ggplot(data = subset(useable_female, useable_female$Year == "2019"), mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(Black), col = Sex)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) 
# Nor for female alone
logit_model_black_2019_female <- glm(data = subset(useable_female, useable_female$Year == "2019"), Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2019_female) 
ggplot(data = subset(useable_female, useable_female$Year == "2019"), mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(Black), col = Sex)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) 
# Nor for male alone
ggplot_logit_race_2019_female <- function (race) {
  ggplot(data = subset(data_2019, data_2019$Sex == "Female"), mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(race), col = Sex)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) 
}
logit_model_black_2019 <- glm(data = data_2019, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2019) # Black logit 2019
logit_model_black_2014 <- glm(data = data_2014, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2014) # Black logit 2014
logit_model_black_2013 <- glm(data = data_2013, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2013) # Black logit 2013
logit_model_black_2012 <- glm(data = data_2012, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2012) # Black logit 2012
logit_model_black_2011 <- glm(data = data_2011, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2011) # Black logit 2011
logit_model_black_2010 <- glm(data = data_2010, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2010) # Black logit 2010
logit_model_black_2009 <- glm(data = data_2009, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2009) # Black logit 2009
logit_model_black_2008 <- glm(data = data_2008, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2008) # Black logit 2008 
logit_model_black_2007 <- glm(data = data_2007, Black ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_2007) # Black logit 2007
# No statistically significant relationships

# White logistic regressions by race by year
logit_model_white_2019 <- glm(data = data_2019, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2019) # White logit 2019
logit_model_white_2014 <- glm(data = data_2014, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2014) # White logit 2014
logit_model_white_2013 <- glm(data = data_2013, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2013) # White logit 2013
logit_model_white_2012 <- glm(data = data_2012, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2012) # White logit 2012
logit_model_white_2011 <- glm(data = data_2011, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2011) # White logit 2011
logit_model_white_2010 <- glm(data = data_2010, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2010) # White logit 2010
logit_model_white_2009 <- glm(data = data_2009, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2009) # White logit 2009
logit_model_white_2008 <- glm(data = data_2008, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2008) # White logit 2008 
logit_model_white_2007 <- glm(data = data_2007, White ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_2007) # White logit 2007
# No statistically significant relationships

# Asian_and_Pacific_Islander logistic regressions by race by year
logit_model_asian_and_pi_2019 <- glm(data = data_2019, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2019) # Asian_and_Pacific_Islander logit 2019
logit_model_asian_and_pi_2014 <- glm(data = data_2014, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2014) # Asian_and_Pacific_Islander logit 2014
logit_model_asian_and_pi_2013 <- glm(data = data_2013, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2013) # Asian_and_Pacific_Islander logit 2013
logit_model_asian_and_pi_2012 <- glm(data = data_2012, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2012) # Asian_and_Pacific_Islander logit 2012
logit_model_asian_and_pi_2011 <- glm(data = data_2011, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2011) # Asian_and_Pacific_Islander logit 2011
logit_model_asian_and_pi_2010 <- glm(data = data_2010, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2010) # Asian_and_Pacific_Islander logit 2010
logit_model_asian_and_pi_2009 <- glm(data = data_2009, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2009) # Asian_and_Pacific_Islander logit 2009
logit_model_asian_and_pi_2008 <- glm(data = data_2008, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2008) # Asian_and_Pacific_Islander logit 2008 
logit_model_asian_and_pi_2007 <- glm(data = data_2007, Asian_and_Pacific_Islander ~ Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_2007) # Asian_and_Pacific_Islander logit 2007
# No statistically significant relationships


# Regressing by leading cause across all years

# Black women by all significant leading causes from 2019
logit_female_all_other_causes <- glm(data = subset(useable_female, useable_female$Leading.Cause == "All Other Causes"), 
                                     as.numeric(Black) ~ Age.Adjusted.Death.Rate, 
                                     family = binomial(link = logit))
summary(logit_female_all_other_causes)
ggplot(data = subset(useable_female, useable_female$Leading.Cause == "All Other Causes"), mapping = aes(x = Age.Adjusted.Death.Rate, y = as.numeric(Black))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)
# All of the data black female data points are above all the other points in terms of death rate for All Other Causes

# Regular linear regression for females across the causes for which the hypothesis tests for 2019 showed any significant relationship
lm_useable_female_all_other_causes <- lm(data = subset(useable_female, useable_female$Leading.Cause == "All Other Causes"), Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_female_all_other_causes)
# Significant relationship with 67 more deaths per 100000 expected for black women for 'all other causes' as leading cause
lm_useable_female_diabetes <- lm(data = subset(useable_female, useable_female$Leading.Cause == "Diabetes Mellitus (E10-E14)"), Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_female_diabetes)
# Significant relationship with 22 more deaths per 100000 expected for black women for diabetes as leading cause
lm_useable_female_heart_disease <- lm(data = subset(useable_female, useable_female$Leading.Cause == "Diseases of Heart (I00-I09, I11, I13, I20-I51)"), 
                                      Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_female_heart_disease)
# Significant relationship with 107 more deaths per 100000 expected for black women for heart disease as leading cause
lm_useable_female_hypertension <- lm(data = subset(useable_female, useable_female$Leading.Cause == "Essential Hypertension and Renal Diseases (I10, I12)"), 
                                      Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_female_hypertension)
# Significant relationship with 10 more deaths per 100000 expected for black women for hypertension as leading cause

# Regular linear regression for males across the causes for which the hypothesis tests for 2019 showed any significant relationship
lm_useable_male_all_other_causes <- lm(data = subset(useable_male, useable_male$Leading.Cause == "All Other Causes"), Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_male_all_other_causes)
# Significant relationship with 95 more deaths per 100000 expected for black men for 'all other causes' as leading cause
lm_useable_male_diabetes <- lm(data = subset(useable_male, useable_male$Leading.Cause == "Diabetes Mellitus (E10-E14)"), Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_male_diabetes)
# Significant relationship with 26 more deaths per 100000 expected for black men for diabetes as leading cause
lm_useable_male_heart_disease <- lm(data = subset(useable_male, useable_male$Leading.Cause == "Diseases of Heart (I00-I09, I11, I13, I20-I51)"), 
                                      Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_male_heart_disease)
# Significant relationship with 159 more deaths per 100000 expected for black men for heart disease as leading cause
lm_useable_male_hypertension <- lm(data = subset(useable_male, useable_male$Leading.Cause == "Essential Hypertension and Renal Diseases (I10, I12)"), 
                                     Age.Adjusted.Death.Rate ~ Dummy.Race)
summary(lm_useable_male_hypertension)
# Significant relationship with 12 more deaths per 100000 expected for black men for hypertension as leading cause

data_2007_ordered_death_rate <- data_2007[order(data_2007$Age.Adjusted.Death.Rate),]
data_2008_ordered_death_rate <- data_2008[order(data_2008$Age.Adjusted.Death.Rate),]
data_2009_ordered_death_rate <- data_2009[order(data_2009$Age.Adjusted.Death.Rate),]
data_2010_ordered_death_rate <- data_2010[order(data_2010$Age.Adjusted.Death.Rate),]
data_2011_ordered_death_rate <- data_2011[order(data_2011$Age.Adjusted.Death.Rate),]
data_20012_ordered_death_rate <- data_2012[order(data_2012$Age.Adjusted.Death.Rate),]
data_2013_ordered_death_rate <- data_2013[order(data_2013$Age.Adjusted.Death.Rate),]
data_2014_ordered_death_rate <- data_2014[order(data_2014$Age.Adjusted.Death.Rate),]
data_2019_ordered_death_rate <- data_2019[order(data_2019$Age.Adjusted.Death.Rate),]




