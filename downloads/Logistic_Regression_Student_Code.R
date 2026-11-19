# INTRODUCTION TO LOGISTIC REGRESSION
# Run from the top, one section at a time in RStudio.
# Use Ctrl+Enter (Windows) or Cmd+Enter (Mac).
# Slide numbers refer to the number in the slide footer.

# Run this line ONCE if carData is not installed:
# install.packages("carData")

library(carData)


# Slides 2-3: Study hours and passing ---------------------------------------
# These are the same 80 students shown in the slides.
# passed = 1 means pass; passed = 0 means fail.
students <- data.frame(
  hours = c(
    0.519347, 0.748962, 0.834579, 1.097381, 1.298797, 1.299606, 1.336623, 1.476760, 1.597487, 1.993529,
    2.018569, 2.020751, 2.207228, 2.410827, 2.671600, 2.759811, 2.763809, 2.781555, 2.961136, 3.224548,
    3.315170, 3.643936, 3.672470, 3.674331, 3.754226, 3.954711, 4.004012, 4.138824, 4.199737, 4.238815,
    4.318984, 4.385666, 4.427016, 4.679564, 4.690384, 5.077408, 5.177658, 5.193450, 5.262618, 5.284499,
    5.292113, 5.594253, 5.698089, 5.755569, 5.794952, 5.847991, 6.193771, 6.240829, 6.499404, 6.503662,
    6.573486, 6.593475, 6.900909, 6.963287, 7.010436, 7.147888, 7.162384, 7.171646, 7.247649, 7.344360,
    7.402481, 7.649047, 7.872177, 7.949569, 7.980880, 8.122772, 8.126052, 8.360778, 8.402724, 8.437088,
    8.576950, 8.597015, 8.626708, 8.851958, 9.358550, 9.557418, 9.750000, 9.751251, 9.845261, 9.850769
  ),
  passed = c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 0, 1, 0, 0,
    0, 1, 1, 0, 0, 1, 0, 1, 1, 1,
    0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
    1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  )
)


# Slide 2: Try a linear regression -----------------------------------------
# First, look at the points and fit a line by eye.
plot(passed ~ hours, data = students,
     xlab = "Study hours", ylab = "Outcome / fitted mean",
     ylim = c(-0.3, 1.4), pch = 16, col = "darkgreen")
text(c(1, 5, 9), 1.3, c("A", "B", "C"))

fit_lm <- lm(passed ~ hours, data = students)
abline(fit_lm, col = "orange", lwd = 2)
coef(fit_lm)

# Predict at A, B, and C: 1, 5, and 9 study hours.
ABC <- data.frame(location = c("A", "B", "C"), hours = c(1, 5, 9))
ABC$predicted_mean <- predict(fit_lm, newdata = ABC)
ABC
# Expected: about -0.090, 0.487, and 1.065. Which are valid probabilities?


# Slide 3: Fit a logistic regression ---------------------------------------
# The formula is response ~ predictor, just as in lm().
# family = binomial tells glm() to fit logistic regression for our 0/1 response.
fit_logistic <- glm(passed ~ hours, data = students, family = binomial)
coef(fit_logistic)

# type = "response" asks for predicted probabilities.
ABC$predicted_probability <- predict(fit_logistic, newdata = ABC,
                                     type = "response")
ABC

# Draw the logistic curve over the same points and OLS line.
hours_grid <- data.frame(hours = seq(0, 10, length.out = 100))
p_grid <- predict(fit_logistic, newdata = hours_grid, type = "response")
plot(passed ~ hours, data = students,
     xlab = "Study hours", ylab = "Outcome / fitted mean",
     ylim = c(-0.3, 1.4), pch = 16, col = "darkgreen")
text(c(1, 5, 9), 1.3, c("A", "B", "C"))
abline(fit_lm, col = "orange", lty = 2, lwd = 2)
lines(hours_grid$hours, p_grid, col = "blue", lwd = 2)
# Orange dashed line: OLS. Blue curve: logistic regression.


# Slide 4: A Bernoulli outcome ---------------------------------------------
# If p = 0.80, Y is 1 with probability 0.80 and 0 otherwise.
p <- 0.80
data.frame(Y = c(0, 1), probability = c(1 - p, p))
1 * p + 0 * (1 - p)  # The conditional mean equals p.


# Slide 5: Logistic function and its inverse -------------------------------
eta <- 2
p <- exp(eta) / (1 + exp(eta))
p
log(p / (1 - p))  # Recover eta = 2: this is the logit.


# Slides 6-7: Probability and odds -----------------------------------------
p <- c(0.20, 0.50, 0.80)
Odds <- p / (1 - p)
data.frame(p, Odds)
# Probability 0.80 means odds of 4: about four passes per failure.

Odds <- 3
Odds / (1 + Odds)  # Probability = 0.75, not 3.


# Slides 8-9: Compare recovery odds ----------------------------------------
p_placebo <- 0.50
p_vaccine <- 0.80
Odds_placebo <- p_placebo / (1 - p_placebo)
Odds_vaccine <- p_vaccine / (1 - p_vaccine)
Odds_placebo  # 1
Odds_vaccine  # 4

OR <- Odds_vaccine / Odds_placebo
OR                        # 4 times the odds of recovery.
p_vaccine / p_placebo      # Probability ratio = 1.6, not 4.
1 / OR                    # Reversing the comparison gives 0.25.


# Slide 10: Meet the Titanic data ------------------------------------------
titanic <- TitanicSurvival
head(titanic)
nrow(titanic)  # 1,309 passengers.

# Use passengers with recorded age for ALL the following calculations.
d <- subset(titanic, !is.na(age))
nrow(d)  # 1,046 passengers.

# Make the 0/1 response and dummy predictor explicit.
d$y <- as.integer(d$survived == "yes")      # 1 = survived; 0 = died.
d$Female <- as.integer(d$sex == "female")  # 1 = female; 0 = male.
d$Age30 <- d$age - 30                     # Age measured from 30 years.
head(d)


# Slide 11: Calculate the group odds ratio ---------------------------------
table(d$sex, d$survived)
# Counts: male 135 survived / 523 died; female 292 survived / 96 died.

p_male <- 135 / (135 + 523)
p_female <- 292 / (292 + 96)
p_male    # About 0.205.
p_female  # About 0.753.

Odds_male <- 135 / 523
Odds_female <- 292 / 96
Odds_male    # About 0.258.
Odds_female  # About 3.042.

OR_table <- Odds_female / Odds_male
OR_table  # About 11.78: female-to-male survival odds ratio.


# Slides 12-15: Why exponentiate a slope? ----------------------------------
# Keep all other predictors fixed; increase one predictor by 1.
beta_j <- log(2)
Odds_old <- 0.25
Odds_new <- Odds_old * exp(beta_j)

log(Odds_new) - log(Odds_old)  # The log-odds change equals beta_j.
beta_j
Odds_new / Odds_old          # The odds ratio equals exp(beta_j).
exp(beta_j)                  # 2: odds double (answer D on slide 15).


# Slide 16: Fit the sex-only Titanic model ---------------------------------
# y is the response. Female is the predictor. Male is the reference group.
fit1 <- glm(y ~ Female, data = d, family = binomial)
coef(fit1)
# Expected: intercept about -1.354307; Female about 2.466712.

# Exponentiate the SLOPE to obtain the female-to-male odds ratio.
exp(coef(fit1)["Female"])  # About 11.78.
OR_table                  # Same OR as the count table on slide 11.

# Exponentiating the intercept gives male odds, not an odds ratio.
exp(coef(fit1)["(Intercept)"])  # About 0.258.


# Slide 17: Add age to the model -------------------------------------------
# The + sign includes both predictors.
fit2 <- glm(y ~ Female + Age30, data = d, family = binomial)
coef(fit2)
# Expected: intercept -1.352902; Female 2.460689; Age30 -0.004254.

exp(coef(fit2)["Female"])  # About 11.71: compare passengers of the SAME age.
# The intercept describes a 30-year-old male: Female = 0 and Age30 = 0.


# Slide 18: Interpret the age coefficient ----------------------------------
beta_age <- coef(fit2)["Age30"]
exp(beta_age)       # One extra year: odds multiply by about 0.9958.
exp(10 * beta_age)  # Ten extra years: odds multiply by about 0.958.
100 * (1 - exp(10 * beta_age))  # About 4.2% lower odds, holding sex fixed.


# Slides 19-20: Predict for a female passenger aged 40 ----------------------
new_passenger <- data.frame(Female = 1, Age30 = 10)

# Work through the slide using the fitted coefficients.
b <- coef(fit2)
eta <- b["(Intercept)"] + b["Female"] * 1 + b["Age30"] * 10
eta  # About 1.065: this is log-odds.

Odds <- exp(eta)
Odds  # About 2.902: odds for this passenger profile.

p <- Odds / (1 + Odds)
p  # About 0.744: predicted survival probability.

# R can calculate the same probability directly.
predict(fit2, newdata = new_passenger, type = "response")
# exp(eta) gives profile odds; exp(a slope) gives a comparison's odds ratio.


# Slide 21: Does doubling odds double probability? -------------------------
p_old <- c(0.20, 0.80)
Odds_old <- p_old / (1 - p_old)
Odds_new <- 2 * Odds_old
p_new <- Odds_new / (1 + Odds_new)
change_in_p <- p_new - p_old
data.frame(p_old, Odds_old, Odds_new, p_new, change_in_p)
# New probabilities: about 0.333 and 0.889.
# The probability change depends on the starting probability.


# Slide 22: Try the exit check before running these answers -----------------
OR <- 0.75
100 * (1 - OR)  # 25% lower odds for one more unit, other predictors fixed.

eta <- log(3)
Odds <- exp(eta)
Odds / (1 + Odds)  # Predicted probability = 0.75.
