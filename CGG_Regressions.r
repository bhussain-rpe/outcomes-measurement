setwd("G:/RPE/PROJECTS & PROGRAMS/Outcome measurement/CGG Regression Analysis")

mydata <- read.csv("G:/RPE/PROJECTS & PROGRAMS/Outcome measurement/CGG Regression Analysis/CGG2018RegressionData.csv")

attach(mydata)

names(mydata)

head(mydata)

#--------------------------------1)APPLICATION-----------------------------------(?)

# 1) Applied by Attended 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(applied ~ attended, data = mydata)
summary(linreg)

# 1)b) Applied by Attended, controlling for region 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(applied ~ attended + DEVELOPMENT.REGION, data = mydata)
summary(linreg)

# 1)c) Applied by Attended, controlling for region + category + amount requested
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(applied ~ attended + DEVELOPMENT.REGION, + CATEGORY + REQUESTED.AMOUNT, family=binomial(link='logit'),data = mydata)
summary(linreg)

#-------------------------------2)SUCCESSFUL APPLICATION-------------------------

# 2) Success by Attended
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended, data = subset(mydata,applied==1))
summary(linreg)

# 2)b) Success by Attended, controlling for pass/fail 
#This should get rid of the effect of missed documents on the success, but 
#why does fail not have a negative coefficient?
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + PASS.FAIL, data = subset(mydata,applied==1))
summary(linreg)

# 2)c) Success by Attended, controlling for category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + CATEGORY, data = subset(mydata,applied==1))
summary(linreg)

# 2) d) Success by Attended, controlling for region 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + DEVELOPMENT.REGION, data = subset(mydata,applied==1))
summary(linreg)

# 2) e) Success by Attended, controlling for region + category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + DEVELOPMENT.REGION + CATEGORY, data = subset(mydata,applied==1))
summary(linreg)

# 2) f) Success by Attended, controlling for region + category + amount requested 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ attended + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, data = subset(mydata,applied==1))
summary(linreg)

#*******************LOGIT****************

# 2) Success by Attended
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2)b) Success by Attended, controlling for pass/fail 
#This should get rid of the effect of missed documents on the success, but 
#why does fail not have a negative coefficient?
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + PASS.FAIL, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2)c) Success by Attended, controlling for category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + CATEGORY, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) d) Success by Attended, controlling for region 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + DEVELOPMENT.REGION, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) e) Success by Attended, controlling for region + category 
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + DEVELOPMENT.REGION + CATEGORY, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) f) Success by Attended, controlling for region + category + amount requested ******
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

# 2) g) Success by Attended, controlling for region + category + amount requested (with URBAN)
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = glm(success ~ attended + URBAN + attended*URBAN + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, family=binomial(link='logit'), data = subset(mydata,applied==1))
summary(linreg)

#-------------------------------------3)SCORE--------------------------------------

# 3) Score by Attended 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(SCORE ~ attended, data = subset(mydata,applied==1))
summary(linreg)

# 3)b) Score by Attended, controlling for amount requested 
# note that amount requested should have a positive effect on score
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(SCORE ~ attended + REQUESTED.AMOUNT, data = subset(mydata,applied==1))
summary(linreg)

# 3)c) Score by Attended, controlling for amount requested + region + category 
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(SCORE ~ attended + DEVELOPMENT.REGION + CATEGORY + REQUESTED.AMOUNT, data = subset(mydata,applied==1))
summary(linreg)

#--------------------------------------4)PASS/FAIL---------------------------------
                                                                                                     
# 4) Pass/Fail by Attended 
mydata$passfail = ifelse(mydata$PASS.FAIL == "PASS", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(passfail ~ attended, data = subset(mydata,applied==1))
summary(linreg)

#---------------------------------------5)MISC--------------------------------------

# 5)Success by Score
mydata$success = ifelse(mydata$SUCCESS == "Yes", 1,0)
mydata$applied = ifelse(mydata$APPLIED == "Yes", 1,0)
mydata$attended = ifelse(mydata$ATTENDED.WEBINAR == "Yes",1,0)
linreg = lm(success ~ SCORE, data = subset(mydata,applied==1))
summary(linreg)




