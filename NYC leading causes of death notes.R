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

# Cleaning up data and removing rows where death rate is unavailable as the 
# data is pretty useless without the death rate, right?
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

# Dividing age adjusted death rate by 100,000 to get 'p' for each given observation, as
# each death rate stat is out of 100,000 and adding variance column for age-adjusted death rate

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
data_2007$Leading.Cause <- factor(data_2007$Leading.Cause)
data_2008$Leading.Cause <- factor(data_2008$Leading.Cause)
data_2009$Leading.Cause <- factor(data_2009$Leading.Cause)
data_2010$Leading.Cause <- factor(data_2010$Leading.Cause)
data_2011$Leading.Cause <- factor(data_2011$Leading.Cause)
data_2012$Leading.Cause <- factor(data_2012$Leading.Cause)
data_2013$Leading.Cause <- factor(data_2013$Leading.Cause)
data_2014$Leading.Cause <- factor(data_2014$Leading.Cause)
data_2019$Leading.Cause <- factor(data_2019$Leading.Cause)

# Graphs and regressions for aggregate data (all years)

  # Age-adjusted death rate by dummy race

    # Graphs
ggplot(data = useable_data, mapping = aes(x = Dummy.Race, y = Age.Adjusted.Death.Rate, col = Sex)) +
  geom_point() 
ggplot(data = useable_data, mapping = aes(x = Sex, y = Age.Adjusted.Death.Rate, col = Dummy.Race)) +
  geom_point()
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
    # These logistic regression plots give us an idea that there is a higher chance of being black vs all other races as the death rate increases accross all leading causes

    # Regressions
logit_model_hispanic_all_years <- glm(useable_data$Hispanic ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_hispanic_all_years) # Hispanic logit
logit_model_black_all_years <- glm(useable_data$Black ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_black_all_years) # Black logit
logit_model_white_all_years <- glm(useable_data$White ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_white_all_years) # White logit
logit_model_asian_and_pi_all_years <- glm(useable_data$Asian_and_Pacific_Islander ~ useable_data$Age.Adjusted.Death.Rate, family = binomial(link = logit))
summary(logit_model_asian_and_pi_all_years) # Asian and Pacific Islander logit

coefficients_black <- coefficients(logit_model_black_all_years)
coefficients_asian_and_pi <- coefficients(logit_model_asian_and_pi_all_years)
odds_given_death_rate_all_years <- NULL
for (multiplier in c(1, 100, 200, 300, 400, 500)) {
  odds_black_given_death_rate <- exp(coefficients_black[2] * multiplier)
  p_black_given_death_rate <- exp(coefficients_black[2] * multiplier) / (1 + (exp(coefficients_black[2] * multiplier)))
  odds_asian_and_pi_given_death_rate <- exp(coefficients_asian_and_pi[2] * multiplier)
  p_asian_and_pi_given_death_rate <-exp(coefficients_asian_and_pi[2] * multiplier) / (1 + (exp(coefficients_asian_and_pi[2] * multiplier)))
  odds_given_death_rate_all_years <- rbind(odds_given_death_rate_all_years, 
                                 c(multiplier, odds_black_given_death_rate, p_black_given_death_rate, odds_asian_and_pi_given_death_rate, p_asian_and_pi_given_death_rate))
}
colnames(odds_given_death_rate_all_years) <- c("Increase in death rate", "Black odds change", "Probability Black", "Asian and PI odds change", "Probability Asian and PI")
odds_given_death_rate_all_years <- as.data.frame(odds_given_death_rate_all_years)
    # Statistically significant relationships exist for logistic regressions on Black and Asian and Pacific Islander populations
    # For an increase in the death rate of 100, the odds of being black increases by a ratio of 1.4, and the probability of being black goes from just over 1/2 to 6/10
    # A similar but negative relationship exists for the Asian and Pacific Islander Population

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

# Logisitic regressions for 2019 by race

ggplot(data = useable_data, mapping = aes(x = Leading.Cause, y = Age.Adjusted.Death.Rate)) +
  geom_point(aes(col = Race.Ethnicity)) +
  theme(axis.text.x = element_text(angle = 45, size = 1))

ggplot(data = useable_data, mapping = aes(x = Sex, y = Age.Adjusted.Death.Rate)) +
  geom_point()
aggregate_sex_regression <- lm(data = useable_data, Age.Adjusted.Death.Rate ~ Sex)
summary(aggregate_sex_regression)
# Statistically significant relationship. Being male is associated with, on average for all years and all leading causes, 21 more deaths per 100000 people.

aggregate_race_regression <- lm(data = useable_data, Age.Adjusted.Death.Rate ~ Hispanic + Black + White + Asian_and_Pacific_Islander)
summary(aggregate_race_regression)
# No overall statistically significant relationship, other than a lower death rate per 100000 for Asians and Pacific Islanders, and only significant with 95% confidence.







#  -------------------------------------
# levels(useable_data$Leading.Cause)
# asthma_etc <- subset(useable_data, useable_data$Leading.Cause == "Chronic Lower Respiratory Diseases (J40-J47)")
# mean_differences_2019_males_asthma <- NULL
# for (race in c(17, 18, 20)) {
#   var_1 <- asthma_etc$age_adjusted_var[asthma_etc$Sex == "Male" &
#                                                 asthma_etc[, 19] == TRUE &
#                                                 asthma_etc$Year == 2019]
#   var_2 <- asthma_etc$age_adjusted_var[asthma_etc$Sex == "Male" &
#                                          asthma_etc[, race] == TRUE &
#                                          asthma_etc$Year == 2019]
#   p_1 <- asthma_etc$age_adjusted_p[asthma_etc$Sex == "Male" &
#                                      asthma_etc[, 19] == TRUE &
#                                      asthma_etc$Year == 2019]
#   p_2 <- asthma_etc$age_adjusted_p[asthma_etc$Sex == "Male" &
#                                      asthma_etc[, race] == TRUE &
#                                      asthma_etc$Year == 2019]
#   stat <- diff_of_means_test_stat(var1 = var_1, 
#                                        var2 = var_2, 
#                                        n1 = 100000, 
#                                        n2 = 100000, 
#                                        diff = p_1 - p_2)
#   mean_differences_2019_males_asthma <- rbind(mean_differences_2019_males_asthma, 
#                                               c(cause, race, stat))
# }
# colnames(mean_differences_2019_males_asthma) <- c("Cause", "Race", "Stat")
# mean_differences_2019_males_asthma <- as.data.frame(mean_differences_2019_males_asthma)
# mean_differences_2019_males_asthma$Race[mean_differences_2019_males_asthma[, 2] == 17] <- "Hispanic"
# mean_differences_2019_males_asthma$Race[mean_differences_2019_males_asthma[, 2] == 18] <- "Black"
# mean_differences_2019_males_asthma$Race[mean_differences_2019_males_asthma[, 2] == 20] <- "Asian and Pacific Islander"
# mean_differences_2019_males_asthma <- cbind(mean_differences_2019_males_asthma, 
#                                             pnorm(as.numeric(mean_differences_2019_males_asthma$Stat)))
# 
# mean_differences_2019_females_asthma <- NULL
# for (race in c(17, 18, 20)) {
#   var_1 <- asthma_etc$age_adjusted_var[asthma_etc$Sex == "Female" &
#                                          asthma_etc[, 19] == TRUE &
#                                          asthma_etc$Year == 2019]
#   var_2 <- asthma_etc$age_adjusted_var[asthma_etc$Sex == "Female" &
#                                          asthma_etc[, race] == TRUE &
#                                          asthma_etc$Year == 2019]
#   p_1 <- asthma_etc$age_adjusted_p[asthma_etc$Sex == "Female" &
#                                      asthma_etc[, 19] == TRUE &
#                                      asthma_etc$Year == 2019]
#   p_2 <- asthma_etc$age_adjusted_p[asthma_etc$Sex == "Female" &
#                                      asthma_etc[, race] == TRUE &
#                                      asthma_etc$Year == 2019]
#   stat <- diff_of_means_test_stat(var1 = var_1, 
#                                   var2 = var_2, 
#                                   n1 = 100000, 
#                                   n2 = 100000, 
#                                   diff = p_1 - p_2)
#   mean_differences_2019_females_asthma <- rbind(mean_differences_2019_females_asthma, 
#                                               c(cause, race, stat))
# }
# colnames(mean_differences_2019_females_asthma) <- c("Cause", "Race", "Stat")
# mean_differences_2019_females_asthma <- as.data.frame(mean_differences_2019_males_asthma)
# mean_differences_2019_females_asthma$Race[mean_differences_2019_males_asthma[, 2] == 17] <- "Hispanic"
# mean_differences_2019_females_asthma$Race[mean_differences_2019_males_asthma[, 2] == 18] <- "Black"
# mean_differences_2019_females_asthma$Race[mean_differences_2019_males_asthma[, 2] == 20] <- "Asian and Pacific Islander"
# mean_differences_2019_females_asthma <- cbind(mean_differences_2019_females_asthma, 
#                                             pnorm(as.numeric(mean_differences_2019_females_asthma$Stat)))
# -----------------------------------



# Multivariate dummy regression for age adjusted death rate over race for all years and all leading causes
model_2019_for_race_predictors <- lm(data = data_2019, 
                                         Age.Adjusted.Death.Rate ~ Hispanic + Black + White
                                         + Asian_and_Pacific_Islander)
summary(model_2019_for_race_predictors)

# Logistic regression of death rate over black
logistic_regression_black <- glm(data = useable_data, Black ~ Age.Adjusted.Death.Rate, 
                                 family = binomial(link = logit))
summary(logistic_regression_black)
coefficients_black <- coefficients(logistic_regression_black)
odds_black_given_death_rate <- exp(coefficients_black[2])
# A statistically significant increase in the odds of being black given an increase in the death rate. For every 1 extra death per 100000, 
# the odds of the death rate being a black death rate increases by 1.003972. Can I multiply this linerarly?
odds_black_given_death_rate*100
as.numeric(data_with_race_factors$Black)
ggplot(data = useable_data, aes(x = Age.Adjusted.Death.Rate, y = as.numeric(Black))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)


mean_black <- mean(data_with_race_factors$Age.Adjusted.Death.Rate[data_with_race_factors$Black == 1])
mean_not_black <- mean(data_with_race_factors$Age.Adjusted.Death.Rate[data_with_race_factors$Black == 0])
var_black <- var(data_with_race_factors$Age.Adjusted.Death.Rate[data_with_race_factors$Black == 1])
var_not_black <- var(data_with_race_factors$Age.Adjusted.Death.Rate[data_with_race_factors$Black == 0])

black_vs_rest_se <- se_of_mean_diff(variance1 = var_black, variance2 = var_not_black, 
                n1 = length(data_with_race_factors$Age.Adjusted.Death.Rate[data_with_race_factors$Black == 1]),
                n2 = length(data_with_race_factors$Age.Adjusted.Death.Rate[data_with_race_factors$Black == 0]))
black_vs_rest_test_stat <- diff_of_means_test_stat(
  se = black_vs_rest_se, diff = (mean_black - mean_not_black))
pnorm(black_vs_rest_test_stat)

str(data_with_race_factors)
for (race in colnames(data_with_race_factors[,8:12])) {
  plot <- ggplot(data = data_with_race_factors, aes(x = race, y = Age.Adjusted.Death.Rate)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  return(plot)
}
ggplot(data = data_with_race_factors, aes(x = Hispanic, y = Age.Adjusted.Death.Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

data_hispanic$Hispanic <- factor(data_hispanic$Hispanic)
levels(data_hispanic$Hispanic)
ggplot(data = data_hispanic, aes(x = Age.Adjusted.Death.Rate, y = Hispanic)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = 'binomial'))
logit <- glm(data_hispanic$Hispanic ~ data_hispanic$Age.Adjusted.Death.Rate, 
             family = binomial(link = "logit"))
plot(fitted.values(logit))

str(data_hispanic$Hispanic)
data_hispanic$Hispanic <- factor(data_hispanic$Hispanic)
summary(data_hispanic$Hispanic)
good_data = data_hispanic$Age.Adjusted.Death.Rate > 0.1
useable_data <- subset(data_hispanic, good_data)
useable_data$Age.Adjusted.Death.Rate <- as.numeric(useable_data$Age.Adjusted.Death.Rate)
logit <- glm(useable_data$Hispanic ~ useable_data$Age.Adjusted.Death.Rate, 
             family = binomial(link = "logit"))
plot(logit)
summary(logit)
ggplot(data = useable_data, aes(Age.Adjusted.Death.Rate, as.numeric(Hispanic))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



# Functions:
# Standard error of difference in means
se_of_mean_difference <- function(variance1, variance2, n1, n2) {
  se <- sqrt((variance1/n1) + (variance2/n2))
  return(se)
}
# Standardized test stat for difference in means
test_stat_mean_difference <- function(mean1, mean2, se) {
  diff <- mean1 - mean2
  test_stat <- diff / se
  return(test_stat)
}

men <- subset(data, data$Sex == "Male")
women <- subset(data, data$Sex == "Female")
mean_male <- mean(as.numeric(men$Death.Rate), na.rm = TRUE)
mean_female <- mean(as.numeric(women$Death.Rate), na.rm = TRUE)
sd_male_death_rate <- sd(men$Death.Rate, na.rm = TRUE)
sd_female_deeath_rate <- sd(women$Death.Rate, na.rm = TRUE)
var_male <- sd_male_death_rate^2
var_female <- sd_female_deeath_rate^2
n_male <- nrow(men)
n_female <- nrow(women)
?matrix
se_of_men_and_women <- se_of_mean_difference(variance1 = var_male, variance2 = var_female,
                                             n1 = n_male, n2 = n_female)
test_stat_men_and_women_death_rate <- test_stat_mean_difference(mean1 = mean_male,
                                                                mean2 = mean_female,
                                                                se = se_of_men_and_women)
pnorm(test_stat_men_and_women_death_rate, lower.tail = FALSE)
# Can NOT reject the null hypothesis that there is no overall difference between male and 
# female death rates for all leading causes taken together.

# Regressing death rate over categorical variable of Hispanic
data$Race.Ethnicity <-  as.factor(data$Race.Ethnicity)
levels(data$Race.Ethnicity)
data_races <- data.frame(matrix(data = NA, ncol = 0, nrow = nrow(data)))
for (level in levels(data$Race.Ethnicity)) {
  column <- ifelse(data$Race.Ethnicity == level, 1, 0)
  data_races <- cbind(data_races, column)
  colnames(data_races) <- level
}  
data_races <- cbind(data, Hispanic = ifelse(data$Race.Ethnicity == "Hispanic", 1, 0),
                    )
data_hispanic$Death.Rate <- as.numeric(data_hispanic$Death.Rate)
good_data_hispanic <- subset(data_hispanic, !is.na(data_hispanic$Death.Rate))
death_rate_over_hispanic <- lm(data = good_data_hispanic,
                               Death.Rate ~ Hispanic)
death_rate_over_hispanic
plot(x = good_data_hispanic$Hispanic, y = good_data_hispanic$Death.Rate)

data_hispanic$Hispanic <-  factor(data_hispanic$Hispanic, levels = c(0,1), 
                                     labels = c("Other","Hispanic"))
levels(data_hispanic$Hispanic)
as.numeric(data_hispanic$Hispanic)
