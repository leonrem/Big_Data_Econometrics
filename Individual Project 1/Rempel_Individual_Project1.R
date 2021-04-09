cat("\014") 
rm(list = ls())
gc()

# For replicability and to have the correct random draw from seeds
RNGversion('3.5.3')

### starter code provided to create test and train dataset

# Load the diabetes data
library(lars)
data(diabetes)
data.all <- data.frame(cbind(diabetes$x, y = diabetes$y))

# Partition the patients into two groups: training (75%) and test (25%)
n <- dim(data.all)[1]           # sample size = 442
set.seed(1306)                  # set random number generator seed to enable 
                                # repeatability of results
test <- sample(n, round(n/4))   # randomly sample 25% test
data.train <- data.all[-test,]
data.test <- data.all[test,]
x <- model.matrix(y ~ ., data = data.all)[,-1] # define predictor matrix 
                                               # excl intercept col of 1s
x.train <- x[-test,]            # define training predictor matrix
x.test <- x[test,]              # define test predictor matrix
y <- data.all$y                 # define response variable
y.train <- y[-test]             # define training response variable
y.test <- y[test]               # define test response variable
n.train <- dim(data.train)[1]   # training sample size = 332
n.test <- dim(data.test)[1]     # test sample size = 110

##########################################################
# Looking at relationships within the dataset
pairs(data.train)

# Packages for data exploration
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(ggthemes)
library(plyr)

# Creating a Correlation Graph
corr <- cor(data.train)
p.mat <- cor_pmat(data.train)

ggcorrplot(corr, type = "lower", outline.col = "white", 
                 p.mat = p.mat, ggtheme = 'theme_minimal',
                 legend.title = 'Correlation\n     Value', 
                 colors = c('green', 'white', 'red'),
                 title = 'Training Set Correlations', lab = T)

# Creating Histogram Graphs
mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(age))
age.hist <- ggplot(data.train, aes(x=age, color = as.factor(sex))) + 
                   geom_histogram(fill = 'white', bins = 40) + 
                   geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                              size = 2, linetype = 'dashed') + 
                  theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(bmi))
bmi.hist <- ggplot(data.train, aes(x=bmi, color = as.factor(sex))) + 
                  geom_histogram(fill = 'white', bins = 40) + 
                  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                             size = 2, linetype = 'dashed') + 
                  theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(map))
map.hist <- ggplot(data.train, aes(x=map, color = as.factor(sex))) + 
                  geom_histogram(fill = 'white', bins = 40) + 
                  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                             size = 2, linetype = 'dashed') + 
                  theme(legend.position = 'none')


mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(tc))
tc.hist <- ggplot(data.train, aes(x=tc, color = as.factor(sex))) + 
                   geom_histogram(fill = 'white', bins = 40) + 
                   geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                              size = 2, linetype = 'dashed') + 
                   theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(ldl))
ldl.hist <- ggplot(data.train, aes(x=ldl, color = as.factor(sex))) + 
                   geom_histogram(fill = 'white', bins = 40) + 
                   geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                              size = 2, linetype = 'dashed') + 
                   theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(hdl))
hdl.hist <- ggplot(data.train, aes(x=hdl, color = as.factor(sex))) + 
                   geom_histogram(fill = 'white', bins = 40) + 
                   geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                              size = 2, linetype = 'dashed') + 
                   theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(tch))
tch.hist <- ggplot(data.train, aes(x=tch, color = as.factor(sex))) + 
                  geom_histogram(fill = 'white', bins = 40) + 
                  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                             size = 2, linetype = 'dashed') + 
                  theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(ltg))
ltg.hist <- ggplot(data.train, aes(x=ltg, color = as.factor(sex))) + 
                  geom_histogram(fill = 'white', bins = 40) + 
                  geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                             size = 2, linetype = 'dashed') + 
                  theme(legend.position = 'none')

mu <- ddply(data.train, 'sex', summarise, grp.mean=mean(glu))
glu.hist <- ggplot(data.train, aes(x=glu, color = as.factor(sex))) + 
                 geom_histogram(fill = 'white', bins = 40) + 
                 geom_vline(data = mu, aes(xintercept=grp.mean, color=as.factor(sex)), 
                            size = 2, linetype = 'dashed') + 
                 theme(legend.position = 'none')

grid.arrange(age.hist, bmi.hist, map.hist, tc.hist, ldl.hist, hdl.hist,
             tch.hist, ltg.hist, glu.hist, ncol = 3, 
             top = 'Histograms of Continuous Vars')

par(mfrow=c(1,1))

##########################################################
# Least Squares Regression Model

reg.OLS.all <- lm(y ~., data = data.train)
summary(reg.OLS.all)
OLS.pred <- predict.lm(reg.OLS.all, newdata = data.test, se.fit = T)
OLS.MSE <- mean((y.test-OLS.pred$fit)^2)

# The standard error of the prediction or the model is taken by dividing the standard
# deviation of the errors by the square root of the sample size
OLS.se <- sd((data.test$y - OLS.pred$fit)^2)/sqrt(n.test) 

# Model Diagnostics
par(mfrow=c(2,2))
plot(reg.OLS.all)

##########################################################
# Best Subset Regression Model with min BIC

library(leaps)

reg.OLS.best <- regsubsets(y~., data = data.train, nvmax = 11)
sum.reg.OLS.best <- summary(reg.OLS.best)

par(mfrow=c(1,2))
plot(x = 1:10, y = sum.reg.OLS.best$bic, type = 'b', ylab = 'BIC Score', xlab = '# of Variables')
title(main = 'Best Subset Variables vs BIC')
points(6, min(sum.reg.OLS.best$bic), col = "red", cex = 2, pch = 20)
plot(reg.OLS.best, scale = "bic", main = "Predictor Variables vs. BIC")


min.bic.model <- coef(reg.OLS.best, which.min(sum.reg.OLS.best$bic))
min.bic.model # Printing out the coefficents of the best subsets model according to BIC
OLS.best.model <- lm(y~ sex + bmi + map + tc + tch + ltg, data.train)
summary(OLS.best.model)

test.mat <- model.matrix(y~., data = data.test)
OLS.best.pred <- test.mat[,names(min.bic.model)] %*% min.bic.model

OLS.best.MSE <- mean((y.test - OLS.best.pred)^2)
OLS.best.se <- sd((y.test - OLS.best.pred)^2)/sqrt(n.test)

# Model Diagnostics
par(mfrow=c(2,2))
plot(OLS.best.model)

##########################################################
# Best Subset with 10 fold CV

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call [[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  return(mat[,xvars ]%*%coefi)
}

k <- 10
set.seed(1306)
folds <- sample(1:k, nrow(data.train), replace = TRUE)
cv.errors <- matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

for (i in 1:k) {
  model <- regsubsets(y~., data = data.train[folds != i,], nvmax = 10)
  for (j in 1:10) {
    pred <- predict(model, data.train[folds == i,], id = j)
    cv.errors[i, j] = mean((data.train$y[folds == i] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)

par(mfrow=c(1,1))
plot(mean.cv.errors, type = 'b', ylab = 'Mean CV Errors', xlab = '# of Variables')
title(main='Best Subset Variables vs Mean CV Errors \n-10 folds-')
points(6, min(mean.cv.errors), col = "red", cex = 2, pch = 20)

# We create the 6 variable model 
reg.OLS.best.CV <- regsubsets(y~., data=data.train, nvmax = 11)
# First we look at the coefficients
coef(reg.OLS.best.CV, id = 6) # They end up being the same as the best subsets with BIC
OLS.best.CV.model <- lm(y~ sex + bmi + map + tc + tch + ltg, data.train)
summary(OLS.best.CV.model)

OLS.best.CV.pred <- predict.regsubsets(reg.OLS.best.CV, data.test, id = 6)
OLS.best.CV.MSE <- mean((y.test - OLS.best.CV.pred)^2)
OLS.best.CV.se <- sd((y.test - OLS.best.CV.pred)^2)/sqrt(n.test)

# Model Diagnostics
par(mfrow=c(2,2))
plot(OLS.best.CV.model)

##########################################################
# Ridge with 10 fold CV

library(glmnet)

par(mfrow = c(1,2))
grid <- 10^seq(10, -2, length = 100)

ridge.models <- glmnet(x.train, y.train, alpha = 0, lambda = grid, thresh = 1e-12)
plot(ridge.models, xvar = "lambda", label = TRUE)
title(sub = 'Ridge Shrinkage of Coefficents', font.sub = 2)

set.seed(1306)
cv.out.ridge <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out.ridge)
title(sub ='Ridge Regression Lambdas -10 folds-', font.sub = 2)
ridge.bestlamb <- cv.out.ridge$lambda.1se

# Print our the ridge best lambda within 1 standard error
ridge.bestlamb

# Determining the model coefficients
ridge.model <- glmnet(x.train, y.train, alpha = 0, lambda = ridge.bestlamb)
ridge.coef <- predict(ridge.model, s = ridge.bestlamb, type = "coefficients")

# Print out the coefficients
ridge.coef

# Determining the model measures of interests
ridge.pred <- predict(cv.out.ridge, s = ridge.bestlamb, newx = x.test)
ridge.MSE <- mean((y.test - ridge.pred)^2)
ridge.se <- sd((y.test-ridge.pred)^2)/sqrt(n.test)

# Print out the error measurements
ridge.MSE
ridge.se

##########################################################
# Lasso with 10 fold CV

par(mfrow = c(1,2))

lasso.models <- glmnet(x.train, y.train, alpha = 1, lambda = grid, thresh = 1e-12)
plot(lasso.models, xvar = "lambda", label = TRUE)
title(sub = 'Lasso Shrinkage of Coefficents', font.sub = 2)

set.seed(1306)
cv.out.lasso <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.out.lasso)
title(sub ='Lasso Regression Lambdas -10 folds-', font.sub = 2)
lasso.bestlamb <- cv.out.lasso$lambda.1se

# Print out the best lasso lambda within 1 standard error
lasso.bestlamb

# Determining the model coefficients
lasso.model <- glmnet(x.train, y.train, alpha = 1, lambda = lasso.bestlamb)
lasso.coef <- predict(lasso.model, s = lasso.bestlamb, type = "coefficients")

# Print out the coefficients
lasso.coef

# Determining the model measures of interests
lasso.pred <- predict(cv.out.lasso, s = lasso.bestlamb, newx = x.test)
lasso.MSE <- mean((y.test - lasso.pred)^2)
lasso.se <- sd((y.test-lasso.pred)^2)/sqrt(n.test)

# Print out the error measurements
lasso.MSE
lasso.se