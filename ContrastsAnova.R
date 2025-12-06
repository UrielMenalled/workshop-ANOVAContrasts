#Contrasts and type III anova in R
#by: Uriel Menalled

##Contrasts matter for non-continuous variables, they are how the model "understands" these terms.
#In anova, this matters because it affects how the null model is made
library(lmerTest)
library(emmeans)
library(tidyverse)

#example with a 3-level factor-----
##contr.treatment (R default): Compares each level to a reference level
contr.treatment(n = 3)
##contr.sum: Compares each level to the overall mean.
contr.sum(n = 3)
##contr.poly: Looks for linear and quadratic trends across ordered levels.
contr.poly(n = 3)
##contr.helmert: Compares each level to the average of previous levels.
contr.helmert(n = 3)


#RECOMENDATIONS----
#Use contr.treatment if you care about interpreting beta coefficencts and not using car::Anova(mod, type = 3)
#Use contr.sum for type III anova and factors (as.factor or as.character)
#Use contr.poly for type III anova and ordinal variables (as.ordinal)
#options(contrasts = c("contr.sum", "contr.poly")) will gobally do contr.sum for factors and contr.poly for ordinal variables in ALL Models

#lmerTest::anova() seems to automatically set contr.sum and do type III, so it only really matters for car::Anova(mod, type = 3)
#The author of {car} warns that contr.treatment will be wrong for car::Anova(mod, type = 3)

#PROOFS of recommendations----
#When you have one variable, there is no impact.
#This is because the null model is just the global mean of the response
data(sleepstudy)
sleepstudy <- as.data.frame(sleepstudy)
sleepstudy$Days <- as.factor(sleepstudy$Days) #Made day a factor because continous variables are unaffectd 

m1_sleep <- lm(Reaction ~ Days,
               contrasts = list(Days = "contr.treatment"),
               sleepstudy)
m2_sleep <- lm(Reaction ~ Days,
               contrasts = list(Days = "contr.sum"),
               sleepstudy)

#results are the same
anova(m1_sleep)
anova(m2_sleep)
car::Anova(m1_sleep, type = 3, test.statistic = "F")
car::Anova(m2_sleep, type = 3, test.statistic = "F")

#Here is another example of a one-way anova
data(Orthodont,package="nlme")
Orthodont <- as.data.frame(Orthodont)

m1_Ortho_oneWay <- lm(distance ~ Sex,
                      contrasts = list(Sex = "contr.treatment"),
                      data=Orthodont)
m2_Ortho_oneWay <- lm(distance ~ Sex,
                      contrasts = list(Sex = "contr.sum"),
                      data=Orthodont)

#notice that, like before, results are the same regardless of anova function used
anova(m1_Ortho_oneWay)
anova(m2_Ortho_oneWay)
car::Anova(m1_Ortho_oneWay, type = 3, test.statistic = "F")
car::Anova(m2_Ortho_oneWay, type = 3, test.statistic = "F")

#we can understand the difference in contrasts by looking at the summary
summary(m1_Ortho_oneWay)
#notice that Sex = male is the reference.
#At Sex = male, distance is 24.9687. Distance at Sex = female is 24.9687 + (-2.3210) = 22.6477
#KEEP THIS LOGIC IN MIND 
summary(m2_Ortho_oneWay)
#now, notice that the intercept (23.8082) is the average distance of the two sexes (FYI, I'm going to call averages raw means later..)
#because (24.9687+22.6477)/2 = 23.8082 and so in this case, the estimate (1.1605) is showing the difference of each sex from the mean
#if there were multiple variables, you'd be comparing relative to a grand mean: literately just the mean of all variable levels
#KEEP LOGIC THIS IN MIND

#When you have more than one variable, there is an impact on car::Anova(mod, type = 3)
#lmerTest::anova() seems to be robust
Orthodont$age <-as.factor(Orthodont$age)

m1_Ortho <- lm(distance ~ age*Sex,
                 contrasts = list(age = "contr.treatment",
                                  Sex = "contr.treatment"),
                 data=Orthodont)
m2_Ortho <- lm(distance ~ age*Sex,
                 contrasts = list(age = "contr.sum",
                                  Sex = "contr.sum"),
                 data=Orthodont)
summary(m1_Ortho) 
#Using the SAME LOGIC AS BEFORE, remember that SexFemale is simply comparing female (non-reference) to male (reference)
#However, now there is also a reference value for age (age = 8)
#so the difference in female vs male (-1.6932 and it's p-value is 0.0658) is constrain to age = 8
#we can prove this is in fact because
#intercept = mean(distance(sex = male & age = 8)) = 22.875
mean(Orthodont[Orthodont$Sex == "Male" & Orthodont$age == "8",]$distance) #intercept = raw mean of reference group
#and so SexFemale = intercept - mean(distance(sex = female & age = 8)) = 1.693182
22.875 - mean(Orthodont[Orthodont$Sex == "Female" & Orthodont$age == "8",]$distance)
#age is also relative to male, for instance, age10&male is 23.8125 which is also intercept + age10
mean(Orthodont[Orthodont$Sex == "Male" & Orthodont$age == "10",]$distance)

summary(m2_Ortho)
#Using the SAME LOGIC AS BEFORE, remeber that everything is relative to the grand mean (23.8082), which is calculated as the mean of all variable levels <- confirm with Tyler
#The raw mean weights each n equally: overall mean = total/n.
#However, the grand mean weighs each group equally: grand mean = group means/number of groups.
mean(as.vector(tapply(Orthodont$distance, list(Orthodont$Sex,Orthodont$age), mean))) #grand mean calculation

tapply(Orthodont$distance, Orthodont$Sex, mean) #raw means
23.8082 + 1.1605 #24.96875 is the same as the male raw mean
23.8082 - 1.1605 #22.64773 is the same as the female raw mean

#something strange... unbalanced groups don't play well with the grand mean!
tapply(Orthodont$distance, Orthodont$age, mean) #raw means
23.8082 - 1.7798 #22.0284 is NOT raw mean of age = 8
23.8082 - 0.7884 #23.0198 is NOT raw mean of age = 10
23.8082 + 0.5966 #24.4048 is NOT raw mean of age = 12
23.8082 + 1.7798 + 0.7884 - 0.5966 #25.7798 is NOT raw mean of age = 14

as.data.frame(emmeans(m2_Ortho, ~age)) #Doesn't match raw means, but it matches model calculations...
as.data.frame(emmeans(m2_Ortho, ~age), weights = "equal") #Doesn't match raw means, but it matches model calculations...
as.data.frame(emmeans(m2_Ortho, ~age, weights = "proportional")) #RIGHT! Matches raw means
as.data.frame(emmeans(m2_Ortho, ~age, weights = "cells")) #RIGHT! Matches raw means

#remember that the grand mean is calculted, weighing each group equally
#thus the means of the age don't match raw means because they are averaging over sex, treating each sex as weighted equally.
table(Orthodont$Sex, Orthodont$age)

#proof that when NOT averaging over Sex (which is unbalanced) the model estimates of age match the raw means.
m2_Ortho.age <- lm(distance ~ age, contrasts = list(age = "contr.sum"),data=Orthodont)
summary(m2_Ortho.age)
tapply(Orthodont$distance, Orthodont$age, mean)
24.0231 - 1.8380 #22.1851 is age = 8
24.0231 - 0.8565 #23.1666 is age = 10
24.0231 + 0.6250 #24.6481 is age = 12
24.0231 + 1.8380 + 0.8565 - 0.6250 #26.0926 is age = 14
as.data.frame(emmeans(m2_Ortho.age, ~age), weights = "equal") #correct, because not averaging over sex
as.data.frame(emmeans(m2_Ortho.age, ~age, weights = "proportional")) #same to, weights = "equal" because not averaging over sex

#consistent results with lmerTest::anova()
anova(m1_Ortho)
# anova(m1_Ortho, ddf = "Kenward-Roger") #not sure why it doesn't work anymore
# anova(m2_Ortho, ddf = "Kenward-Roger") #not sure why it doesn't work anymore

#inconsistent results with car::Anova(), sex is not diff in contr.treatment THIS IS WHERE IT COMES TOGETHER!!!!
car::Anova(m1_Ortho, type = 3, test.statistic = "F") # <- NOTICE THAT THE Sex p-value is the same as the summary for m1_Ortho, indicating that the null model (intercept) is using the reference!
car::Anova(m2_Ortho, type = 3, test.statistic = "F") # <- NOTICE THAT THE Sex p-value is the same as the summary for m2_Ortho, indicating that the null model (intercept) is using the grand mean!

#main effects differ in car::Anova because:
#m1 = Impact of considering Sex differences relative to the null model where age is the reference value
#m2 = Impact of considering Sex differences relative to the null model where age is the average value

#joint_test() is an alternate way to calculate type III-*like* anova tests
#it automatically uses contr.sum and centers continuous variables.
#However it is based on estimated marginal means, not model coefficients, so just use it as a sanity-check

#I just used it as a nice alternate test. It was a sanity check for me!
joint_tests(m1_Ortho)
joint_tests(m2_Ortho)

#Does messing with contrasts affect modeling of continuous variables? YES!!!-----
Orthodont<-
  Orthodont %>% 
  mutate(weight = 
           case_when(Sex == "Male" ~ rnorm(n(),mean = 150, sd = 10),
                     Sex == "Female" ~ rnorm(n(),mean = 120, sd = 10)))
str(Orthodont)

m1b_Ortho <- lmer(distance ~ age*Sex*weight + (1|Subject),
                  contrasts = list(age = "contr.treatment",
                                   Sex = "contr.treatment"),
                  data=Orthodont)
m2b_Ortho <- lmer(distance ~ age*Sex*weight + (1|Subject),
                  contrasts = list(age = "contr.sum",
                                   Sex = "contr.sum"),
                  data=Orthodont)

#consistent results with lmerTest::anova()
anova(m1b_Ortho, ddf = "Kenward-Roger")
anova(m2b_Ortho, ddf = "Kenward-Roger")

#inconsistent results with car::Anova(), different values
car::Anova(m1b_Ortho, type = 3, test.statistic = "F")
car::Anova(m2b_Ortho, type = 3, test.statistic = "F")

#This is because
#m1b_Ortho = Impact of considering weight difference relative to the null model where age and sex are at their reference values
#m2b_Ortho = Impact of considering weight difference relative to the null model where age and sex are at their average values

#if you want to confirm contrasts of a fitted model
attr(model.matrix(m1b_Ortho),"contrasts")

#FINAL NOTE----------
##If doing Type III ANOVA: Almost always set options(contrasts = c("contr.sum", "contr.poly")) at the start of your code!
##comparing back to a global mean makes sense—most descriptions of type III ANOVA assume this!
##Mayyybeee use dummy coding in type III ANOVA if you really care about a reference group, but tread VERY carefully b/c it's not the standard for type III Anova, type I ANOVA won't be affected because there is no null model
##"anova()" from the lme4 package won't automatically set contr.sum! this is only true for "anova()" from the lmerTest package.
##post-hoc emmean tests are not affected by contrast setting, but if you have unbalanced data do consider if it is for them to match raw data / assume a balanced design

#useful links:
##https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/#DEVIATION <- contr.treatment vs. contr.sum
##https://cscu.cornell.edu/wp-content/uploads/emmeans.pdf <- emmean and unbalance
##https://cscu.cornell.edu/workshop/interpreting-linear-models-regression-and-anova/ <- video 1, but if you have the time, watch them all

