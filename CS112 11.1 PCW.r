# Section 5 Replication

library("rbounds")
library("Matching")
foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))
foo

# Generalized linear model function

glmmain <- glm(party ~  I(district^2) + district  + ngirls + nboys + totchi + I(totchi^2) + reg3 + reg4 + reg5 + reg6 + reg7 + reg8 + reg9 + OthParty , data=foo)

# data objects are saved

X <- glmmain$fitted

Y <- foo$party

Tr <- foo$Dems



rrA <- Match(Y=Y, Tr=Tr, X=X, M=3);

summary(rrA)

genout <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1,
                   pop.size=16, max.generations=10, wait.generations=1)


mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout)
                     
mb <- MatchBalance(party ~  I(district^2) + district  + ngirls + nboys
                   + totchi + I(totchi^2) + reg3 + reg4 + reg5 + reg6 
                   + reg7 + reg8 + reg9 + OthParty , data=foo,
                   match.out=mout, nboots=500)