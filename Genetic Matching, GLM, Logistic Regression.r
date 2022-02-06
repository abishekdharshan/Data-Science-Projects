# Question 2
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
colnames(foo)
foo <- foo[, c(6:8, 11:16,99, 50, 114, 49, 52, 63, 136, 109, 126, 48, 160, 142, 10, 108)]
foo <- foo[c(-19, -47), ]
which(is.na(foo) == TRUE)

# 'untype4': 'multidimensional peacekeeeping/peacebuilding' is the Treatment Indicator 
# 'pbs2s3': 'democracy' & 'peace' is the Outcome

glm1 <- glm(pbs2s3 ~ wartype + logdead + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4,
            data = foo, family = binomial)

mean.wartype <- mean(foo$wartype)
mean.logdead <- mean(foo$logdead)
mean.factnum <- mean(foo$factnum)
mean.factnum2 <- mean(foo$factnum2)
mean.trnsfcap <- mean(foo$trnsfcap)
mean.develop <- mean(foo$develop)
mean.exp <- mean(foo$exp)
mean.decade <- mean(foo$decade)
mean.treaty <- mean(foo$treaty)

get_logit <- function(X, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*X)
  return(exp(logit) / (1 + exp(logit)))
}

storage.original.treat <- rep(NA, 315)
storage.original.control <- rep(NA, 315)

for (wardur in 1:315) {
  
  X.treat <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1)
  X.control <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0)
  
  storage.original.treat[wardur]  <- get_logit(X.treat, coef(glm1))
  storage.original.control[wardur]  <- get_logit(X.control, coef(glm1))
}

original_y <- storage.original.treat - storage.original.control

# modified glm
glm.modified <- glm(pbs2s3 ~ wartype + logdead + wardur + factnum + factnum2 + 
                      trnsfcap + develop + exp + decade + treaty + wardur:untype4 + untype4,
                    data = foo, family = binomial)

glm.modified

storage.modified.treat <- rep(NA, 315)
storage.modified.control <- rep(NA, 315)
for (wardur in 1:315) {
  
  X.treat <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1, wardur*1)
  X.control <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0, wardur*0)
  
  storage.modified.treat[wardur]  <- get_logit(X.treat, coef(glm.modified))
  storage.modified.control[wardur]  <- get_logit(X.control, coef(glm.modified))
}


modified_y <- storage.modified.treat - storage.modified.control

# new modified glm
glm.new.modified <- glm(pbs2s3 ~ wartype + logdead + wardur + factnum + factnum2 + 
                          trnsfcap + develop + exp + decade + treaty + wardur*logcost + exp*untype4 + untype4,
                        data = foo, family = binomial)
glm.new.modified


mean.logcost <- mean(foo$logcost)

storage.new.modified.treat <- rep(NA, 315)
storage.new.modified.control <- rep(NA, 315)
for (wardur in 1:315) {
  
  X.treat <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
               mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, mean.logcost, 1, wardur*mean.logcost, mean.exp*1)
  X.control <- c(mean.wartype, mean.logdead, wardur, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, mean.logcost, 0, wardur*mean.logcost, mean.exp*0)
  
  storage.new.modified.treat[wardur]  <- get_logit(X.treat, coef(glm.new.modified))
  storage.new.modified.control[wardur]  <- get_logit(X.control, coef(glm.new.modified))
}


new_modified_y <- storage.new.modified.treat - storage.new.modified.control
new_modified_y

# plot original model
plot(1:315, original_y, type="l", lty="dotted", ylim = c(0, 0.8), xlab="War duration (months)", ylab="Marginal effect from untype4", col="blue")

# plot modified model
lines(x = 1:315, modified_y, ylim = c(0, 0.8), col="blue")

# plot new modified model
lines(x = 1:315, new_modified_y, ylim = c(0, 0.8), col="lightgoldenrod4")

# adding legend onto graph
legend("bottomright", legend = c("Original Model", "Modified Model w/ Interaction Term (wardur*untype4)", "New Modified Model w/ Interaction Term (exp*untype4) & (wardur*logcost)"), col = c("blue", "blue", "lightgoldenrod4"), lty = c("dotted","solid", "solid"), bty = "o", pt.cex = 2, cex = 0.9, text.col = "black", horiz = F , inset = c(0.02, 0.06))









# Question 4

library(Matching)
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[c(-19, -47), ]

# treatment definition
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != "None")] <- 1
foo$Tr <- Tr

foo$uncint

# standard error definition
se <- function(x, na.rm=F) sqrt(var(x, na.rm=na.rm)/length(x[!is.na(x)]))

head(foo)

# logistic regression
glm3.2yr <- glm(pbs2l ~ wartype + logdead + wardur + factnum + factnum2 + 
                  trnsfcap + develop + exp + decade + treaty + Tr, 
                data = foo, family = binomial)

NAs <- is.na(foo$pbs5l) 
glm3.5yr <- glm(pbs5l ~ wartype + logdead + wardur + factnum + factnum2 + 
                  trnsfcap + develop + exp + decade + treaty + Tr, 
                data = foo[!NAs], family = binomial)

# 2 years success logistic regression
foo.counter_factual <- foo
foo.counter_factual$Tr <- rep(1, nrow(foo)) - foo$Tr
counter.factuals <- predict(glm3.2yr, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo))

treated <- foo$Tr == 1
unit_treat_effects[treated] <- glm3.2yr$fitted.values[treated] - counter.factuals[treated]
unit_treat_effects[!treated] <- counter.factuals[!treated] - glm3.2yr$fitted.values[!treated]
mean(unit_treat_effects)
se(unit_treat_effects)

mb.2yr <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                         trnsfcap + develop + exp + decade + treaty, data = foo, nboots=500)

# 5 years success logistic regression
foo.counter_factual <- foo[!NAs,]
foo.counter_factual$Tr <- 1 - foo$Tr[!NAs]
counter.factuals <- predict(glm3.5yr, newdata=foo.counter_factual, type="response")
unit_treat_effects <- rep(NA, nrow(foo[!NAs,]))

mask <- foo[!NAs,]$Tr == 1
unit_treat_effects[mask] <- glm3.5yr$fitted.values[mask] - counter.factuals[mask]
unit_treat_effects[!mask] <- counter.factuals[!mask] - glm3.5yr$fitted.values[!mask]
mean(unit_treat_effects)
se(unit_treat_effects)

mb.5yr <- MatchBalance(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                         trnsfcap + develop + exp + decade + treaty, data = foo, nboots=500)


# Next Part

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
colnames(foo)
foo <- foo[, c(34, 35, 52, 6:8, 11:16, 99, 50, 108, 114, 49, 63, 124:127, 136, 109, 126, 48, 160, 142, 10)]

# Removing Data NAs
foo <- foo[complete.cases(foo), ]
which(is.na(foo) == TRUE)
head(foo)

# Treatment Definition
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != "None")] <- 1
foo$Tr <- Tr

# Propensity Score Matching Section
prop.sc <- glm(Tr ~ wartype + logdead + wardur + factnum +
                 factnum2 + trnsfcap + develop +
                 exp + decade + treaty, 
               data=foo, family = binomial)
Tr <- foo$Tr
X <- prop.sc$fitted
Y2 <- foo$pbs2l
Y5 <- foo$pbs5l

mout.2yr <- Match(Tr=Tr, X=X, Y=Y2)
summary(mout.2yr)
MatchBalance(Tr ~ wartype + logdead + wardur + factnum +
               factnum2 + trnsfcap + develop +
               exp + decade + treaty, 
             data=foo, match.out = mout.2yr, nboots = 1000)

mout.5yr <- Match(Tr=Tr, X=X, Y=Y5)
MatchBalance(Tr ~ wartype + logdead + wardur + factnum +
               factnum2 + trnsfcap + develop +
               exp + decade + treaty, 
             data=foo, match.out = mout.5yr, nboots = 1000)
summary(mout.5yr)


# Genetic Matching Section
Tr <- foo$Tr
X <- cbind(foo$wartype, foo$logdead, foo$wardur, foo$factnum,
           foo$factnum2, foo$trnsfcap, foo$develop, foo$exp,
           foo$decade, foo$treaty)
Y2 <- foo$pbs2l
Y5 <- foo$pbs5l

genout <- GenMatch(Tr=Tr, X=X, pop.size = 250, max.generations = 30)
mout.2yr <- Match(Tr=Tr, X=X, Y=Y2, Weight.matrix = genout)
MatchBalance(Tr ~ wartype + logdead + wardur + factnum +
               factnum2 + trnsfcap + develop +
               exp + decade + treaty, 
             data=foo, match.out = mout.2yr)
summary(mout.2yr)

mout.5yr <- Match(Tr=Tr, X=X, Y=Y5, Weight.matrix = genout)
MatchBalance(Tr ~ wartype + logdead + wardur + factnum +
               factnum2 + trnsfcap + develop +
               exp + decade + treaty, 
             data=foo, match.out = mout.5yr)
summary(mout.5yr)


# Genetic Matching with Propensity Score Section
glm.gen_match <- glm(Tr ~ wartype + logdead + wardur + factnum + factnum2 + 
                       trnsfcap + develop + exp + decade + treaty, 
                     data = foo, family = binomial)
Tr <- foo$Tr
Y2 <- foo$pbs2l
Y5 <- foo$pbs5l
X <- cbind(glm.gen_match$fitted, foo$wartype, foo$logdead, foo$wardur, foo$factnum,
           foo$factnum2, foo$trnsfcap, foo$develop, foo$exp,
           foo$decade, foo$treaty)

genout <- GenMatch(Tr=Tr, X=X, pop.size = 250, max.generations = 30)
mout.2yr <- Match(Tr=Tr, X=X, Y=Y2, Weight.matrix = genout)
MatchBalance(Tr ~ wartype + logdead + wardur + factnum +
               factnum2 + trnsfcap + develop +
               exp + decade + treaty, 
             data=foo, match.out = mout.2yr)
summary(mout.2yr)

mout.5yr <- Match(Tr=Tr, X=X, Y=Y5, Weight.matrix = genout)
MatchBalance(Tr ~ wartype + logdead + wardur + factnum +
               factnum2 + trnsfcap + develop +
               exp + decade + treaty, 
             data=foo, match.out = mout.5yr)
summary(mout.5yr)
