library(Matching)
library(rgenoud)
library(foreign)
library(parallel)
library(rbounds)
library(dplyr)


initial_data <- read.dta("maintmp.dta")

# Table 2 Replication

# Obtaining Treatment Effect
initial_model <- lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                       d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                       d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                       d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                       d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                       d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                       d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                       d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=initial_data)
summary(initial_model)


insurance <- subset(initial_data, d_hinsEMP==1)
obesity_ins <- subset(insurance, d_obese==1)
no_obesity_ins <- subset(insurance, d_obese==0)
no_insurance <- subset(initial_data, d_hinsEMP==0)
obesity_no_ins <-subset(no_insurance, d_obese==1)
no_obesity_no_ins <-subset(no_insurance, d_obese==0)

te_insurance <- mean(predict(initial_model, newdata=obesity_ins))-mean(predict(initial_model, newdata=no_obesity_ins))
te_no_insurance <- mean(predict(initial_model, newdata=obesity_no_ins))-mean(predict(initial_model, newdata=no_obesity_no_ins))


# We are working with only the female gender to find out the implications for black women, white women, and women belonging to other ethnicities

# Women belonging to other race(s)
oth_race <- initial_data[which(initial_data$d_race_o==1 & initial_data$d_sexf==1), ]

model_oth_race<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
             d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
             d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
             d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
             d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
             d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
             d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
             d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=oth_race)

summary(model_oth_race)

insurance_oth_race <- subset(oth_race, d_hinsEMP==1)
obesity_ins_oth_race <- subset(insurance, d_obese==1)
no_obesity_ins_oth_race <- subset(insurance, d_obese==0)
no_insurance_oth_race <- subset(oth_race, d_hinsEMP==0)
obesity_no_ins_oth_race <-subset(no_insurance, d_obese==1)
no_obesity_no_ins_oth_race <-subset(no_insurance, d_obese==0)

te_insurance_oth_race <- mean(predict(model_oth_race, newdata=obesity_ins_oth_race))-mean(predict(model_oth_race, newdata=no_obesity_ins_oth_race))
te_no_insurance_oth_race <- mean(predict(model_oth_race, newdata=obesity_no_ins_oth_race))-mean(predict(model_oth_race, newdata=no_obesity_no_ins_oth_race))

# Subset of other race to insurance
oth_race <- subset(oth_race, d_hinsEMP == 1)

# Outline the variables to perform matching upon
attach(oth_race)

X <-cbind(d_sexf,childany,childf,age,d_urban_res
          ,srvy_yr,AFQTrevised,educ,tenure,NumberEmp,d_race_b,d_race_o
          ,d_marrnever,d_marroth,d_ind_ag,d_ind_for
          ,d_ind_mining,d_ind_const,d_ind_mfrg,d_ind_transp,d_ind_wtrade
          ,d_ind_rtrade,d_ind_finance,d_ind_bus_svc,d_ind_pers_svc
          ,d_ind_entert,d_ind_prof_svc,d_occ_mgmt,d_occ_tech
          ,d_occ_admin,d_occ_svc,d_occ_farming,d_occ_prodxn
          ,d_occ_operators)
detach(oth_race)

# Performing Mahalanobis Distance Matching
Maha_dist_oth_race <- Match(Tr=oth_race$d_obese,X=X, Weight = 2)
summary(Maha_dist_oth_race)
MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
               srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
               d_race_b+d_race_o+d_marrnever+d_marroth+
               d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
               d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
               d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
               d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
               d_occ_farming+d_occ_prodxn+d_occ_operators,data=oth_race, match.out = Maha_dist_oth_race)


n_core <-detectCores()-1

cl <- makeCluster(n_core)

# Performing Genetic Matching
genout <- GenMatch(Tr=oth_race$d_obese,X=X,pop.size = 500, max.generations = 20, wait.generations = 4,M=1, cluster=cl)
mout <- Match(Tr=oth_race$d_obese,X=X, Weight.matrix = genout)

mb<- MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
                     srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                     d_race_b+d_race_o+d_marrnever+d_marroth+
                     d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                     d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                     d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                     d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                     d_occ_farming+d_occ_prodxn+d_occ_operators,data=oth_race, match.out = mout)

stopCluster(cl)

# Performing Sensitivity Analysis
sens1 <- Match(Y=oth_race$CPS_hourly_rec,Tr=oth_race$d_obese,X=X, Weight.matrix = genout)
summary(sens1)

psens(sens1, Gamma=4, GammaInc = 0.2)










# Women belonging to the white race
wht_race <- initial_data[which(initial_data$d_race_w==1 & initial_data$d_sexf==1), ]

model_wht_race<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                  d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                  d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                  d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                  d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                  d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                  d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                  d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=wht_race)

summary(model_wht_race)

insurance_wht_race <- subset(wht_race, d_hinsEMP==1)
obesity_ins_wht_race <- subset(insurance, d_obese==1)
no_obesity_ins_wht_race <- subset(insurance, d_obese==0)
no_insurance_wht_race <- subset(wht_race, d_hinsEMP==0)
obesity_no_ins_wht_race <-subset(no_insurance, d_obese==1)
no_obesity_no_ins_wht_race <-subset(no_insurance, d_obese==0)

te_insurance_wht_race <- mean(predict(model_wht_race, newdata=obesity_ins_wht_race))-mean(predict(model_wht_race, newdata=no_obesity_ins_wht_race))
te_no_insurance_wht_race <- mean(predict(model_wht_race, newdata=obesity_no_ins_wht_race))-mean(predict(model_wht_race, newdata=no_obesity_no_ins_wht_race))

# Subset of white race to insurance
wht_race <- subset(wht_race, d_hinsEMP == 1)

wht_race <- sample_n(wht_race, 500)


# Outline the variables to perform matching upon
attach(wht_race)

Xw <-cbind(d_sexf,childany,childf,age,d_urban_res
          ,srvy_yr,AFQTrevised,educ,tenure,NumberEmp,d_race_b,d_race_o
          ,d_marrnever,d_marroth,d_ind_ag,d_ind_for
          ,d_ind_mining,d_ind_const,d_ind_mfrg,d_ind_transp,d_ind_wtrade
          ,d_ind_rtrade,d_ind_finance,d_ind_bus_svc,d_ind_pers_svc
          ,d_ind_entert,d_ind_prof_svc,d_occ_mgmt,d_occ_tech
          ,d_occ_admin,d_occ_svc,d_occ_farming,d_occ_prodxn
          ,d_occ_operators)
detach(wht_race)

# Performing Mahalanobis Distance Matching
Maha_dist_wht_race <- Match(Y=wht_race$CPS_hourly_rec,Tr=wht_race$d_obese,X=Xw, Weight = 2)
summary(Maha_dist_wht_race)
MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
               srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
               d_race_b+d_race_o+d_marrnever+d_marroth+
               d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
               d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
               d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
               d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
               d_occ_farming+d_occ_prodxn+d_occ_operators,data=wht_race, match.out = Maha_dist_wht_race)


n_core <-detectCores()-1

cl <- makeCluster(n_core)

# Performing Genetic Matching
genout_w <- GenMatch(Tr=wht_race$d_obese,X=Xw,pop.size = 500, max.generations = 20, wait.generations = 4,M=1, cluster=cl)
mout_w <- Match(Tr=wht_race$d_obese,X=Xw, Weight.matrix = genout_w)

mb_w<- MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
                    srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                    d_race_b+d_race_o+d_marrnever+d_marroth+
                    d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                    d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                    d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                    d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                    d_occ_farming+d_occ_prodxn+d_occ_operators,data=wht_race, match.out = mout_w)

stopCluster(cl)

# Performing Sensitivity Analysis
sens1_w <- Match(Y=wht_race$CPS_hourly_rec,Tr=wht_race$d_obese,X=Xw, Weight.matrix = genout_w)
summary(sens1_w)

psens(sens1_w, Gamma=2, GammaInc = 0.1)










# Women belonging to the black race
blk_race <- initial_data[which(initial_data$d_race_b==1 & initial_data$d_sexf==1), ]

model_blk_race<-lm(CPS_hourly_rec~d_obese+d_hinsEMP+d_obesinsEMP+d_sexf+childany+childf+
                  d_marrnever+d_marroth+age+age2+d_educ9_12+d_educ13_up+d_AFQT_0_25+d_AFQT_25_50+d_AFQT_50_75+
                  d_AFQT_75_100+d_urban_res+d_tenure0_1+d_tenure1_3+d_tenure3_6+d_tenure6_up+d_emp0_9+d_emp10_24+
                  d_emp25_49+d_emp50_999+d_emp1000_up+d_year1989+d_year1990+d_year1992+d_year1993+d_year1994+
                  d_year1996+d_year1998+d_year2000+d_year2002+d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+
                  d_ind_mfrg+d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+d_ind_bus_svc+d_ind_pers_svc+
                  d_ind_entert+d_ind_prof_svc+d_ind_pub_ad+d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+d_occ_farming+
                  d_occ_prodxn+d_occ_operators+d_occ_military, weight=sample_wt, data=blk_race)

summary(model_wht_race)

insurance_blk_race <- subset(blk_race, d_hinsEMP==1)
obesity_ins_blk_race <- subset(insurance, d_obese==1)
no_obesity_ins_blk_race <- subset(insurance, d_obese==0)
no_insurance_blk_race <- subset(blk_race, d_hinsEMP==0)
obesity_no_ins_blk_race <-subset(no_insurance, d_obese==1)
no_obesity_no_ins_blk_race <-subset(no_insurance, d_obese==0)

te_insurance_blk_race <- mean(predict(model_blk_race, newdata=obesity_ins_blk_race))-mean(predict(model_blk_race, newdata=no_obesity_ins_blk_race))
te_no_insurance_blk_race <- mean(predict(model_blk_race, newdata=obesity_no_ins_blk_race))-mean(predict(model_blk_race, newdata=no_obesity_no_ins_blk_race))

# Subset of black race to insurance
blk_race <- subset(blk_race, d_hinsEMP == 1)
blk_race <- sample_n(blk_race, 500)

# Outline the variables to perform matching upon
attach(blk_race)

Xb <-cbind(d_sexf,childany,childf,age,d_urban_res
           ,srvy_yr,AFQTrevised,educ,tenure,NumberEmp,d_race_b,d_race_o
           ,d_marrnever,d_marroth,d_ind_ag,d_ind_for
           ,d_ind_mining,d_ind_const,d_ind_mfrg,d_ind_transp,d_ind_wtrade
           ,d_ind_rtrade,d_ind_finance,d_ind_bus_svc,d_ind_pers_svc
           ,d_ind_entert,d_ind_prof_svc,d_occ_mgmt,d_occ_tech
           ,d_occ_admin,d_occ_svc,d_occ_farming,d_occ_prodxn
           ,d_occ_operators)
detach(blk_race)

# Performing Mahalanobis Distance Matching
Maha_dist_blk_race <- Match(Tr=blk_race$d_obese,X=Xb, Weight = 2)
summary(Maha_dist_blk_race)
MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
               srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
               d_race_b+d_race_o+d_marrnever+d_marroth+
               d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
               d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
               d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
               d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
               d_occ_farming+d_occ_prodxn+d_occ_operators,data=blk_race, match.out = Maha_dist_blk_race)


n_core <-detectCores()-1

cl <- makeCluster(n_core)

# Performing Genetic Matching
genout_b <- GenMatch(Tr=blk_race$d_obese,X=Xb,pop.size = 200, max.generations = 20, wait.generations = 10,M=1, cluster=cl)
mout_b <- Match(Tr=wht_race$d_obese,X=Xb, Weight.matrix = genout_b)

mb_b<- MatchBalance(d_obese~d_sexf+childany+childf+age+d_urban_res+
                      srvy_yr+AFQTrevised+educ+tenure+NumberEmp+
                      d_race_b+d_race_o+d_marrnever+d_marroth+
                      d_ind_ag+d_ind_for+d_ind_mining+d_ind_const+d_ind_mfrg+
                      d_ind_transp+d_ind_wtrade+d_ind_rtrade+d_ind_finance+
                      d_ind_bus_svc+d_ind_pers_svc+d_ind_entert+d_ind_prof_svc+
                      d_occ_mgmt+d_occ_tech+d_occ_admin+d_occ_svc+
                      d_occ_farming+d_occ_prodxn+d_occ_operators,data=blk_race, match.out = mout_b)

stopCluster(cl)

# Performing Sensitivity Analysis
sens1_b <- Match(Y=blk_race$CPS_hourly_rec,Tr=blk_race$d_obese,X=Xb, Weight.matrix = genout_b)
summary(sens1_b)

psens(sens1_b, Gamma=4, GammaInc = 0.2) 