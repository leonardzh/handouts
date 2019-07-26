# Linear models

library(readr)
library(dplyr)
library(ggplot2)
person <- read_csv(
  file = 'data/census_pums/sample.csv',
  col_types = cols_only(
    AGEP = 'i',  # Age
    WAGP = 'd',  # Wages or salary income past 12 months
    SCHL = 'i',  # Educational attainment
    SEX = 'f',   # Sex
    OCCP = 'f',  # Occupation recode based on 2010 OCC codes
    WKHP = 'i')) # Usual hours worked per week past 12 months


person <- within(person, {
  SCHL <- factor(SCHL)
  levels(SCHL) <- list(
    'Incomplete' = c(1:15),
    'High School' = 16,
    'College Credit' = 17:20,
    'Bachelor\'s' = 21,
    'Master\'s' = 22:23,
    'Doctorate' = 24)}) %>%
  filter(
    WAGP > 0,
    WAGP < max(WAGP, na.rm = TRUE))

# Formula Notation

fit <- lm(
  formula = WAGP ~ SCHL,
  data = person)

ggplot(person, aes(x=SCHL,y=WAGP)) + geom_boxplot()
fit

fit <- lm(
  formula = log(WAGP) ~ SCHL,
  data = person)
summary(fit)

fit <- lm(
  formula = log(WAGP) ~ AGEP,
  data = person)
summary(fit)
ggplot(person, aes(x=AGEP,y=log(WAGP))) + geom_point()
fit <- lm(
  ...,
  person)

# Metadata matters

fit <- lm(
  ...,
  person)

# GLM families

fit <- glm(log(WAGP)~SCHL,
  family = gaussian,
  data = person)
summary(fit)
# Logistic Regression

fit <- glm(SEX ~ WAGP,
  family = binomial,
  data = person)
summary(fit)

ggplot(person, aes(x=WAGP,y=SEX)) + geom_point()

anova(fit,update(fit,))

levels(person$SEX)
anova(fit, update(fit, SEX~1), test = 'Chisq')

# Random Intercept

library(lme4)
fit <- lmer(
  log(WAGP) ~ (1|OCCP) + SCHL,
  data = person)

summary(fit)
# Random Slope

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person)
summary(fit)


fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person,
  control = lmerControl(optimizer = 'bobyqa'))

ggplot(person,
  aes(x = WKHP, y = log(WAGP), color = SCHL)) +
  geom_point() +
  geom_line(aes(y = predict(fit))) +
  labs(title = 'Random intercept and slope with lmer')
