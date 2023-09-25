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

# Comments
# - Model is not as good, R^2 = 0.6221 with 67.1% error, so not good at all
# - We must now try to incorporate polynomials and interaction terms, we will model 2 as fewer coefficients but almost same R^2 and RSE





