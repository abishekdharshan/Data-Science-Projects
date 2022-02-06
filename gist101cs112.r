# Q1
# prereq packages
install.packages("rgenoud")
library(Matching)
library(rgen)
library(rgenoud)

library(Matching)

# Q1A

fake <- read.csv("https://tinyurl.com/yxzo52ez")

X.1 <- cbind(fake$age, fake$educ, fake$black, fake$hisp, fake$married, fake$nodegr,
           fake$re74, fake$re75, fake$u74, fake$u75)

# Running GenMatch function
genout <- GenMatch(Tr = fake$treat, X = X.1, estimand = "ATE", 
                   M = 1, pop.size = 20, max.generations = 10, wait.generations = 1)


Y <- fake$re78

mout.1 <- Match(Y = Y, Tr = fake$treat, X = X.1, estimand = "ATT", Weight.matrix = genout)
summary(mout.1)


mb.1 <- MatchBalance(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp + fake$married
                   + fake$nodegr + fake$re74 + fake$re75 + fake$u74 + fake$u75,
                   match.out = mout, nboots = 500)


upper.1 <- mout.1$est + 1.96*mout.1$se.standard
lower.1 <- mout.1$est - 1.96*mout.1$se.standard
conf.1 <- c(lower.1, upper.1)
conf.1

# Q1B

fake <- read.csv("https://tinyurl.com/yxzo52ez")

X.2 <- cbind(fake$age*fake$educ, fake$age*fake$black, fake$married*fake$black, fake$married*fake$educ,
           fake$age, fake$educ, fake$black, fake$hisp, fake$married, fake$nodegr,
           fake$re74, fake$re75, fake$u74, fake$u75)

# Running GenMatch function.
genout.2 <- GenMatch(Tr = fake$treat, X = X.2, estimand = "ATT", 
                   M = 1, pop.size = 16, max.generations = 5, wait.generations = 1)


Y <- fake$re78

mout.2 <- Match(Y = Y, Tr = fake$treat, X = X.2, estimand = "ATT", Weight.matrix = genout.2)
summary(mout.2)

mb.2 <- MatchBalance(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp + fake$married
                   + fake$nodegr + fake$re74 + fake$re75 + fake$u74 + fake$u75,
                   match.out = mout.2, nboots = 500)

upper.2 <- mout.2$est + 1.96*mout.2$se.standard
lower.2 <- mout.2$est - 1.96*mout.2$se.standard
conf.2 <- c(lower.2, upper.2)
conf.2

# Q1C

X.3 <- cbind(fake$age*fake$educ, fake$age*fake$black, fake$married*fake$black, fake$married*fake$educ,
             fake$age, fake$educ, fake$black, fake$hisp, fake$married, fake$nodegr,
             fake$re74, fake$re75, fake$u74, fake$u75)

# Running GetMatch Function
genout.3 <- GenMatch(Tr = fake$treat, X = X.3, estimand = "ATT", 
                     M = 1, pop.size = 16, max.generations = 5, wait.generations = 1, replace = F)


mout.3 <- Match(Y = Y, Tr = fake$treat, X = X.3, estimand = "ATT", Weight.matrix = genout.3, replace = F)
summary(mout.3)

mb.3 <- MatchBalance(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp + fake$married
                     + fake$nodegr + fake$re74 + fake$re75 + fake$u74 + fake$u75,
                     match.out = mout.3, nboots = 500)

upper.3 <- mout.3$est + 1.96*mout.3$se.standard
lower.3 <- mout.3$est - 1.96*mout.3$se.standard
conf.3 <- c(lower.3, upper.3)
conf.3


# Q1D

X.4 <- cbind(fake$age, fake$educ)
genout.4 <- GenMatch(Tr = fake$treat, X = X.4, estimand = "ATT", 
                     M = 1, pop.size = 16, max.generations = 5, wait.generations = 1)

mout.4 <- Match(Y = Y, Tr = fake$treat, X = X.4, estimand = "ATT", Weight.matrix = genout.4)
summary(mout.4)

mb.4 <- MatchBalance(fake$treat ~ fake$age + fake$educ, match.out = mout.4, nboots = 500)

upper.4 <- mout.4$est + 1.96*mout.4$se.standard
lower.4 <- mout.4$est - 1.96*mout.4$se.standard
conf.4 <- c(lower.4, upper.4)
conf.4

# Q1E

View(fake)
install.packages("normalize")
library(normalize)

fake.age.scaled <- scale(fake$age)
fake.edu.scaled <- scale(fake$educ)

data.scaled <- cbind(fake, fake.age.scaled, fake.edu.scaled)
View(data.scaled)

X.5 <- cbind(data.scaled$fake.age.scaled, data.scaled$fake.edu.scaled)

genout.5 <- GenMatch(Tr = fake$treat, X = X.5, estimand = "ATT", 
                     M = 1, pop.size = 100, max.generations = 5, wait.generations = 1)

mout.5 <- Match(Y = Y, Tr = fake$treat, X = X.5, estimand = "ATT", Weight.matrix = genout.5)
summary(mout.5)

mb.5 <- MatchBalance(fake$treat ~ data.scaled$fake.age.scaled + data.scaled$fake.edu.scaled, 
                     match.out = mout.5, nboots = 500)

upper.5 <- mout.5$est + 1.96*mout.5$se.standard
lower.5 <- mout.5$est - 1.96*mout.5$se.standard
conf.5 <- c(lower.5, upper.5)
conf.5

# Q1F

X.6 <- cbind(data.scaled$fake.age.scaled, data.scaled$fake.edu.scaled)

genout.6 <- GenMatch(Tr = fake$treat, X = X.6, estimand = "ATT", 
                     M = 1, pop.size = 16, max.generations = 5, wait.generations = 1,
                     caliper = c(0.01, 10000))

mout.6 <- Match(Y = Y, Tr = fake$treat, X = X.6, estimand = "ATT", Weight.matrix = genout.6, caliper = 0.01)
summary(mout.6)

mb.6 <- MatchBalance(fake$treat ~ data.scaled$fake.age.scaled + data.scaled$fake.edu.scaled, 
                     match.out = mout.6, nboots = 500)

upper.6 <- mout.6$est + 1.96*mout.6$se.standard
lower.6 <- mout.6$est - 1.96*mout.6$se.standard
conf.6 <- c(lower.6, upper.6)
conf.6


# Q1G

X.7 <- cbind(data.scaled$fake.age.scaled, data.scaled$fake.edu.scaled)

genout.7 <- GenMatch(Tr = fake$treat, X = X.7, estimand = "ATT", 
                     M = 10, pop.size = 16, max.generations = 5, wait.generations = 1,
                     caliper = 0.01)

mout.7 <- Match(Y = Y, Tr = fake$treat, X = X.7, estimand = "ATT", Weight.matrix = genout.7)
 summary(mout.7)

mb.7 <- MatchBalance(fake$treat ~ data.scaled$fake.age.scaled + data.scaled$fake.edu.scaled, 
                     match.out = mout.7, nboots = 500)

upper.7 <- mout.7$est + 1.96*mout.7$se.standard
lower.7 <- mout.7$est - 1.96*mout.7$se.standard
conf.7 <- c(lower.7, upper.7)
conf.7

?GenMatch

# Q1H

X.8 <- cbind(data.scaled$fake.age.scaled, data.scaled$fake.edu.scaled)

genout.8 <- GenMatch(Tr = fake$treat, X = X.8, BalanceMatrix = data.scaled$age, estimand = "ATT", 
                     M = 10, pop.size = 16, max.generations = 5, wait.generations = 1,
                     caliper = 0.01)

?GenMatch

mout.8 <- Match(Y = Y, Tr = fake$treat, X = X.8, estimand = "ATT", Weight.matrix = genout.8)
summary(mout.8)

mb.8 <- MatchBalance(fake$treat ~ data.scaled$fake.age.scaled + data.scaled$fake.edu.scaled, match.out = mout.8, nboots = 500)

upper.8 <- mout.8$est + 1.96*mout.8$se.standard
lower.8 <- mout.8$est - 1.96*mout.8$se.standard
conf.8 <- c(lower.8, upper.8)
conf.8


# Q1I

X.10 <- cbind(fake$age, fake$educ, fake$black, fake$hisp, fake$married, fake$nodegr,
             fake$re74, fake$re75, fake$u74, fake$u75)

genout.10 <- GenMatch(Tr = fake$treat, X = X.10, estimand = "ATT", 
                     M = 1, pop.size = 16, max.generations = 5, wait.generations = 1)


glm1 <- glm(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp + fake$married
            + fake$nodegr + fake$re74 + fake$re75 + fake$u74 + fake$u75, 
            family=binomial)

#save data objects

X.glm <- glm1$fitted

Y <- fake$re78

Treat <- fake$treat

length(Y)
length(Treat)


rr <- Match(Y=Y, Tr=Treat, X=X.glm, M=1, Weight.matrix = genout.10)

summary(rr)

mb <- MatchBalance(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp + fake$married
                   + fake$nodegr + fake$re74 + fake$re75 + fake$u74 + fake$u75,
                   match.out=rr, nboots=10)


# Q2

library(Matching)

fake <- read.csv("https://tinyurl.com/yxzo52ez")
head(fake)

# Multivariate Matching w/ Replacement while mathching on all pretreatment variables
# Estimate...  1598.5 AI SE......  989.53 
# Worst balance: p = 0.25153
X.1 = cbind(fake$age, fake$educ, fake$black, fake$hisp, fake$married, fake$nodegr,
            fake$re74, fake$re75, fake$u74, fake$u75)
Tr = fake$treat
Y = fake$re78

genout.1 <- GenMatch(Tr = Tr, X = X.1, estimand = "ATT",
                     pop.size = 10, max.generations = 10,
                     wait.generations = 4)
mout.1 <- Match(Tr = Tr, X = X.1, estimand = "ATT", Weight.matrix = genout.1)
mb.1 <- MatchBalance(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp +
                     fake$married + fake$nodegr + fake$re74 + fake$re75 + fake$u74 +
                     fake$u75, match.out = mout.1)

mout.1.tr <- Match(Tr = Tr, Y = Y, X = X.1, estimand = "ATT", Weight.matrix = genout.1)

# Same as 1 above, except include interactions for age*educ, age*black, married*black, & married*educ --- don’t forget to set the data set w/ the prefix fake$
X.2 = cbind(fake$age, fake$educ, fake$black, fake$hisp, fake$married, fake$nodegr,
            fake$re74, fake$re75, fake$u74, fake$u75,
            fake$age*fake$educ, fake$age*fake$black, fake$married*fake$black, fake$married*fake$educ)

genout.2 <- GenMatch(Tr = Tr, X = X.2, estimand = "ATT",
                     pop.size = 10, max.generations = 10,
                     wait.generations = 4)

mout.2 <- Match(Tr = Tr, X = X.2, estimand = "ATT", Weight.matrix = genout.2)
mb.2 <- MatchBalance(fake$treat ~ fake$age + fake$educ + fake$black + fake$hisp +
                       fake$married + fake$nodegr + fake$re74 + fake$re75 + fake$u74 +
                       fake$u75 + fake$age:fake$educ + fake$age:fake$black +
                       fake$married:fake$black + fake$married:fake$educ, match.out = mout.2)

?GenMatch
?lapply


library(Matching)

lalonde$age


by_three <- function(x) {
  return(x*3)
}

a <- c(1, 2, 3, 4)
lapply(lalonde$age, by_three)