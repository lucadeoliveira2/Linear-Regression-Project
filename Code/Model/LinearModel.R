# Install Libraries #
install.packages('broom')
install.packages('caTools')
install.packages('corrplot')
library(broom)
library(caTools)
library(corrplot)

# Model 1 - All variables #
head(salaries)
str(salaries)
names(salaries)

lmfit1 <- lm(data = salaries, formula = Salaries ~.)
summary(lmfit1)
5758000/mean(salaries$Salaries)


# Comments
# - We have only 3 significant predictors including Age, Games and Personal Fouls
# - This seems odd and is likely due to high collinearity, for example PTS being correlated to Games most likely
# - R^2 = 0.6512 and RSE = 5758000 (66.7% error)
# - Model is poor

# Model 2 - Backward Selection #

step(lmfit1)
lmfit2 <- lm(formula = Salaries ~ Age + G + GS + TRB + X3PA + X2PA + FT +
               AST + BLK + PF + PTS, data = salaries)
summary(lmfit2)
vif(lmfit2)
corrplot(cor(salaries[c(3,4,5,22,23,25,27,28)]), 'number') # PTS correlated hevaily with X3PA, X2PA and FT
lmfit2 <- lm(formula = Salaries ~ Age + G + GS + TRB +
               AST + BLK + PF + PTS, data = salaries)
summary(lmfit2)
vif(lmfit2) # All have a low VIF score

# Comments
# - Removed plenty of predictors without much effect
# - Some high collinearity
# - R^2 dropped to 0.6232 and RSE to 67.2%

# Model 3 - Forward Selection #

RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-1]) {
  
  formula_str <- paste("Salaries ~", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit) # Most important is PTS, as expected lets continue


RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-c(1,28)]) {
  formula_str <- paste("Salaries ~ PTS + ", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit)


RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-c(1,3,28)]) {
  formula_str <- paste("Salaries ~ PTS + Age + ", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit)


RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-c(1,3,4,28)]) {
  formula_str <- paste("Salaries ~ PTS + Age + G +", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit)


RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-c(1,3,4,21, 28)]) {
  formula_str <- paste("Salaries ~ PTS + Age + G + DRB +", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit)


RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-c(1,3,4,21,23,28)]) {
  formula_str <- paste("Salaries ~ PTS + Age + G + DRB + AST +", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit)


RSE <- 100000000
myfit <- NULL
mypredictor <- NULL

for (predictor in names(salaries)[-c(1,3,4,16,21,23,28)]) {
  formula_str <- paste("Salaries ~ PTS + Age + G + DRB + AST + eFG. +", predictor)
  lmfit <- lm(formula_str, data = salaries)
  
  rss <- sum(lmfit$residuals^2)
  df <- length(lmfit$residuals) - length(lmfit$coefficients)
  lmfit_RSE <- sqrt(rss / df)
  
  if (lmfit_RSE < RSE) {
    RSE <- lmfit_RSE
    myfit <- lmfit
    mypredictor <- predictor
  }
}

cat("Best predictor:", mypredictor, "\n")
cat("Best RSE:", RSE, "\n")
summary(myfit) # We are now not significant anymore

lmfit3 <- lm(data = salaries, Salaries ~ PTS + Age + G + DRB + AST + eFG.)
summary(lmfit3)
5783000/mean(salaries$Salaries)
vif(lmfit3)
par(mfrow = c(2,2))
plot(lmfit3)

# Comments
# - Model is not as good, R^2 = 0.6221 with 67.1% error, so not good at all
# - We must now try to reduce the changing variance

# Model 4 - Transforming Response Variable #

lmfit4 <- update(lmfit3, I(Salaries^0.35)~.)
summary(lmfit4)
(57.35/mean(salaries$Salaries^0.35))
plot(lmfit4)
log(8,2)

(51)^(1/0.35)


# Comments
# - We have managed to reduce the variance, as evidnt by the residual plot, some still exists though
# - We tried to match the R^2 as much as possible whilst decreasing the variance.
# R^2 is at 0.6231, error dropped to 61.0%

# Model 5 - Polynomials and Interaction Terms #
cor(salaries[, c('Salaries','PTS', 'Age', 'G', 'DRB', 'AST', 'eFG.')])

lmfit5 <- update(lmfit4, ~ . + PTS:AST)
summary(lmfit5)
plot(lmfit5)

lmfit5 <- update(lmfit4, ~ . + AST:DRB)
summary(lmfit5)

lmfit5 <- update(lmfit4, ~. + I(PTS^2))
summary(lmfit5)

lmfit5 <- update(lmfit5, ~. + I(PTS^2) + AST:PTS)
summary(lmfit4)


par(mfrow = c(2,2))
plot(lmfit5)

# Comments
# - The use of polynomials and interaction terms does not seem to do much, although the interaction term between PTS:AST is very close to significant, still won't use
# - We now need to investigate if performance improves when removing any outlier and high leverage points


# Model 6 - removing high cook's distance points #

cooksD <- cooks.distance(lmfit4)
highCD <- names(cooksD[cooksD > 3*mean(cooksD)])
salaries_lowCD <- salaries[!rownames(salaries) %in% highCD,]
lmfit6 <- update(lmfit4, data = salaries_lowCD)
summary(lmfit6)
plot(lmfit6)
(51.07/mean(salaries_lowCD$Salaries^0.35))^0.35


# Comments
# - Removing points with hugh cooks distance and high leverage made a significant difference to our model
# - R^2 increased to 0.6791 and RSE to 58.5%
# - May remopve high leverage points as last task

# Model 7 - High Leverage Points #

hatpoints <- hatvalues(lmfit6)
high_hat <- hatpoints[hatpoints > 3*mean(hatpoints)]
salaries_lowhat <- salaries_lowCD[!rownames(salaries_lowCD) %in% names(high_hat),]
lmfit7 <- update(lmfit6, data = salaries_lowhat)
summary(lmfit6)
confint(lmfit7)
abs(-31.0500+5.4841+9.0844-0.4453+13.4180+8.8564-116.9958)
(111.6483+3*5.4841)^(1/0.35)

# Comments
# - This performs worse, so fit6 is better.

# Conclusions #
# - We have built a multiple linear regression model using forward selection, accounting for collinearity between predictors.
# - We then accounted for the increase in variance as salary increases and transformed the response variable such that it better fits the model. This substantially improved the model.
# - The use of polynomial regression and interaction terms had little effect on the performance or variance.
# - We then removed points with high cooks distance. These points are prone to severly affecting the regression coefficients. The model improved again.
# - High leverage points were not influential in the model, so these were ignored. No outliers found.
# - Overall the model performs pretty poorly, with only around two thirds of the variance being explained by the model.
# - Even when making predictions, we have an RSE of 58.5%. 
# - Interpreting the PTS coefficient, we have that an increase in 1 PPG results in an increase of f(x)^(1/0.35)






