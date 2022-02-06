# Question 1

# setting random seed to 123
set.seed(123)

# Creating data set with 998 observations and a normal error term
x <- rnorm(998)
y = -0.1 * x + 5 + rnorm(998, 1 , 0.08) #y =  -0.1x + 5 + e
df998 <- data.frame(x, y)
lm998 <- lm(y ~ x, data = df998)
summary(lm.1998)

# Generating 1000 observations with 2 outliers added
df1000 <- rbind(df998, c(10, 15), c(12,12))
lm1000 <- lm(y ~ x, data = df1000)
summary(lm1000)

# Generating plot of the new data with 1,000 observations including the 2 outliers
plot(df1000,
     main = "Fig: Graph Portraying Sensitivity of Regression Models to Outliers",
     xlab = "x axis",
     ylab = "y axis")
abline(a = coef(lm998)[1],
       b = coef(lm998)[2],
       col = "blue")
abline(a = coef(lm1000)[1],
       b = coef(lm1000)[2],
       col = "red")
legend("topleft", legend=c("Original Regression Line","Regression Line with Outliers"), col=c("blue", "red"), lty = 1:1, cex = 0.5)


# Question 2
# Question 2(a)

#import lalonde dataset & arm library
library(Matching)
data(lalonde)
library(arm)

# include the age squared column into the dataset
lalonde$age_sq <- lalonde$age^2

# only select treat units within the dataset
lalonde.treat <- lalonde[which(lalonde$treat == 1), ]

# Fitting the regression model as mentioned in the question
lm.lalonde <- lm(re78 ~ age + age_sq + educ + treat + treat * age + re74 + re75, data = lalonde)
summary(lm.lalonde)

# Running the simulation
set.seed(123)
iterations <- 10000
sim.lalonde <- sim(lm.lalonde, n.sims = iterations)

# Predict re78 for every unit of age holding at their means for every coefficient sets
simulated.ys_mean.treat <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Holding educ, re74, and re75 at their means
mean.educ.treat <- mean(lalonde.treat$educ)
mean.re74.treat <- mean(lalonde.treat$re74)
mean.re75.treat <- mean(lalonde.treat$re75)

# Creating the simulation table 
for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age^2, mean.educ.treat, 1, mean.re74.treat, mean.re75.treat, 1 * age)
  for (i in 1:iterations) {
    simulated.ys_mean.treat[i, age + 1 - min(lalonde.treat$age)] <- sum(Xs*sim.lalonde@coef[i,])
  }
}

conf.intervals_mean.treat <- apply(simulated.ys_mean.treat, 2, quantile, probs = c(0.025, 0.975))
table_mean.treat <- t(data.frame(conf.intervals_mean.treat))
colnames(table_mean.treat) <- c("Re78 Lower Bound", "Re78 Upper Bound")
table_mean.treat <- data.frame(table_mean.treat, mean.educ.treat, mean.re74.treat, mean.re75.treat)
rownames(table_mean.treat) <- min(lalonde$age):max(lalonde$age)
View(table_mean.treat)

# plotting the simulation in the graph
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-5000,20000), 
     main = "Fig: Confidence Interval of Re78 by Age With Predictors Held at The Means", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean.treat[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean.treat[2, age - min(lalonde$age) + 1],
    lwd = 2)
}


# Question 2(b)
# only select control units within the dataset
lalonde.ctrl <- lalonde[which(lalonde$treat == 0), ]

# Fitting the regression model as mentioned in the question
lm.lalonde <- lm(re78 ~ age + age_sq + educ + treat + treat * age + re74 + re75, data = lalonde)
summary(lm.lalonde)

# Running the simulation
set.seed(123)
iterations <- 10000
sim.lalonde <- sim(lm.lalonde, n.sims = iterations)

# Predict re78 for every unit of age holding every set of coefficients at the means
simulated.ys_mean.ctrl <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

mean.educ.ctrl <- mean(lalonde.ctrl$educ)
mean.re74.ctrl <- mean(lalonde.ctrl$re74)
mean.re75.ctrl <- mean(lalonde.ctrl$re75)

# Creating the simulation table 
for (age in min(lalonde$age):max(lalonde$age)) {
  Xs <- c(1, age, age^2, mean.educ.ctrl, 0, mean.re74.ctrl, mean.re75.ctrl, 0 * age)
  for (i in 1:iterations) {
    simulated.ys_mean.ctrl[i, age + 1 - min(lalonde$age)] <- sum(Xs*sim.lalonde@coef[i,])
  }
}

conf.intervals_mean.ctrl <- apply(simulated.ys_mean.ctrl, 2, quantile, probs = c(0.025, 0.975))
table_mean.ctrl <- t(data.frame(conf.intervals_mean.ctrl))
colnames(table_mean.ctrl) <- c("Re78 Lower Bound", "Re78 Upper Bound")
table_mean.ctrl <- data.frame(table_mean.ctrl, mean.educ.ctrl, mean.re74.ctrl, mean.re75.ctrl)
rownames(table_mean.ctrl) <- min(lalonde$age):max(lalonde$age)
View(table_mean.ctrl)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde.ctrl$age),max(lalonde.ctrl$age)), 
     ylim = c(-5000,20000), 
     main = "Fig: Confidence Interval of Untreated Re78 by Age With Predictors Held at Means", xlab = "Age", 
     ylab = "Re78")

for (age in min(lalonde.ctrl$age):max(lalonde.ctrl$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean.ctrl[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean.ctrl[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

# Question 2(c)

# calculating 95% interval of expected values for the treatment effect
simulated.ys_mean.treatment <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))
for (age in min(lalonde$age):max(lalonde$age)) {
  Xs.treat <- c(1, age, age^2, mean.educ.treat, 1, mean.re74.treat, mean.re75.treat, 1 * age)
  Xs.ctrl <- c(1, age, age^2, mean.educ.ctrl, 0, mean.re74.ctrl, mean.re75.ctrl, 0 * age)
  for (i in 1:iterations) {
    simulated.ys_mean.treatment[i, age + 1 - min(lalonde$age)] <- sum(Xs.treat*sim.lalonde@coef[i,]) - sum(Xs.ctrl*sim.lalonde@coef[i,])
  }
}

conf.intervals_mean.treatment <- apply(simulated.ys_mean.treatment, 2, quantile, probs = c(0.025, 0.975))
table_mean.treatment <- t(data.frame(conf.intervals_mean.treatment))
colnames(table_mean.treatment) <- c("Treatment Effect Lower Bound", "Treatment Effect Upper Bound")
table_mean.treatment <- data.frame(table_mean.treatment, mean.educ.treat, mean.re74.treat, mean.re75.treat, mean.educ.ctrl, mean.re74.ctrl, mean.re75.ctrl)
rownames(table_mean.treatment) <- min(lalonde.ctrl$age):max(lalonde.ctrl$age)
View(table_mean.treatment)

# plotting the treatment confidence intervals
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-5000,20000), 
     main = "Fig: Treatment Effects by Age With Predictors Held at The Means", xlab = "Age", 
     ylab = "Treatment Effects")

# Creating the simulation table
for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_mean.treatment[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_mean.treatment[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

# Question 2(d)

simulated.ys_med.treatment <- matrix(NA, nrow = iterations, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Holding educ, re74, and re75 at their medians
med.educ.ctrl <- median(lalonde.ctrl$educ)
med.re74.ctrl <- median(lalonde.ctrl$re74)
med.re75.ctrl <- median(lalonde.ctrl$re75)
med.educ.treat <- median(lalonde.treat$educ)
med.re74.treat <- median(lalonde.treat$re74)
med.re75.treat <- median(lalonde.treat$re75)

# creating the simulation table
for (age in min(lalonde$age):max(lalonde$age)) {
  Xs.treat <- c(1, age, age^2, med.educ.treat, 1, med.re74.treat, med.re75.treat, 1 * age)
  Xs.ctrl <- c(1, age, age^2, med.educ.ctrl, 0, med.re74.ctrl, med.re75.ctrl, 0 * age)
  for (i in 1:iterations) {
    simulated.ys_med.treatment[i, age + 1 - min(lalonde$age)] <- sum(Xs.treat*sim.lalonde@coef[i,]) - sum(Xs.ctrl*sim.lalonde@coef[i,]) + rnorm(1, 0, sim.lalonde@sigma[i])
  }
}

conf.intervals_med.treatment <- apply(simulated.ys_med.treatment, 2, quantile, probs = c(0.025, 0.975))
table_med.treatment <- t(data.frame(conf.intervals_med.treatment))
colnames(table_med.treatment) <- c("Treatment Effect Lower Bound", "Treatment Effect Upper Bound")
table_med.treatment <- data.frame(table_med.treatment, med.educ.treat, med.re74.treat, med.re75.treat, med.educ.ctrl, med.re74.ctrl, med.re75.ctrl)
rownames(table_med.treatment) <- min(lalonde$age):max(lalonde$age)
View(table_med.treatment)

# plotting the treatment effect confidence intervals
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-20000,20000), 
     main = "Fig: Treatment Effects by Age With Predictors Held at the Medians", xlab = "Age", 
     ylab = "Treatment Effects")

# Creating the simulation table
for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = conf.intervals_med.treatment[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = conf.intervals_med.treatment[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

# Question 3 (a)
# bootstrapping function
iters <- 10000
storage <- rep(NA, iters)
for (i in 1:iters) {
  temp_lm = lm(MATH_SCORE ~ TREATMENT, data = foo[sample(1:nrow(foo), nrow(foo), replace = T),])
  storage[i] <- temp_lm$coefficients[2]
}

quantile(storage, c(0.025, 0.975))


# regression model as mentioned in the question and determining confidence interval for the coefficient value
lm.foo <- lm(MATH_SCORE ~ TREATMENT, data = foo)
confint(lm.foo)[2,]


# Question 3 (b)
# plotting the histogram
hist(storage,
    main="Fig: Histogram of Bootstrapped-Sample Coefficients",
    xlab="Bootstrapped Coefficients",
    col="red")
    

# Question 4

# bootstrap function and using an example from the afterschool data to show the bootstrap function is working
foo <- read.csv(url("https://tinyurl.com/y2prc9xq"))
foo.wo_na.math <- foo[!is.na(foo$MATH_SCORE), ]
lm.foo <- lm(MATH_SCORE ~ TREATMENT, data = foo.wo_na.math)

rsquared <- function(ytrue, ypred) {
  iters <- 300000
  storage <- rep(NA, iters)
  for (i in 1:iters){
    indexes <- sample(1:length(ytrue), length(ytrue), replace = T)
    new_ytrue <- ytrue[indexes]
    new_ypred <- ypred[indexes]
    rss <- sum((new_ytrue - new_ypred)**2)
    tss <- sum((new_ytrue - mean(new_ytrue))**2)
    storage[i] <- (1 - rss/tss)
  }
  return(mean(storage))
}

rsquared(foo.wo_na.math$MATH_SCORE, lm.foo$fitted.values)
summary(lm.foo)$r.sq

# Question 5

# following instructions from the question prompt
foo <- read.csv(url("https://tinyurl.com/yx8tqf3k"))
library(boot)
set.seed(12345)
test_set_rows <- sample(1:length(foo$age), 2000, replace = FALSE) 
foo.test_set <- foo[test_set_rows,]
foo.train_set <- foo[-test_set_rows,]
foo

# first model taking education as the independent variable
glm.fit.1 <- glm(treat ~ education, data=foo.train_set, family = binomial)
cv.err <- cv.glm(foo.train_set, glm.fit.1)
cv.err$delta

mean((foo.test_set$treat - predict(glm.fit.1, foo.test_set, type="response")) ^ 2)

# second model taking age as the independent variable
glm.fit.2 <- glm(treat ~ age, data=foo.train_set, family = binomial)
cv.err <- cv.glm(foo.train_set, glm.fit.2)
cv.err$delta

mean((foo.test_set$treat - predict(glm.fit.2, foo.test_set, type="response")) ^ 2)

# third model taking black, hispanic, and black interacting with hispanic as the independent variables
glm.fit.3 <- glm(treat ~ black + hispanic + black:hispanic, data=foo.train_set, family = binomial)
cv.err <- cv.glm(foo.train_set, glm.fit.3)
cv.err$delta
mean((foo.test_set$treat - predict(glm.fit.3, foo.test_set, type="response")) ^ 2)

# fourth model taking age, black, hispanic, black interacting with hispanic, and married as the independent variables
glm.fit.4 <- glm(treat ~ age + black + hispanic + black:hispanic + married, data=foo.train_set, family = binomial)
cv.err <- cv.glm(foo.train_set, glm.fit.4)
cv.err$delta
mean((foo.test_set$treat - predict(glm.fit.4, foo.test_set, type="response")) ^ 2)

# fifth model taking age, black, hispanic, black interacting with hispanic, married, u74, and married interacting with u74 as the independent variables
glm.fit.5 <- glm(treat ~ age + black + hispanic + black:hispanic + married + u74 + married:u74, data=foo.train_set, family = binomial)
cv.err <- cv.glm(foo.train_set, glm.fit.5)
cv.err$delta
mean((foo.test_set$treat - predict(glm.fit.5, foo.test_set, type="response")) ^ 2)
