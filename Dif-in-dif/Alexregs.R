setwd("G:/RPE/PROJECTS & PROGRAMS/Outcome measurement/CGG Regression Analysis")
mydata <- read.csv("G:/RPE/PROJECTS & PROGRAMS/Outcome measurement/CGG Regression Analysis/webdata.csv")


mydata$AttPre3 <- mydata$Attendees*mydata$Pre3
mydata$AttPre2 <- mydata$Attendees*mydata$Pre2
mydata$AttPre1 <- mydata$Attendees*mydata$Pre1
mydata$AttPost1 <- mydata$Attendees*mydata$Post1
mydata$AttPost2 <- mydata$Attendees*mydata$Post2
mydata$AttPost3 <- mydata$Attendees*mydata$Post3
mydata$AttPost4 <- mydata$Attendees*mydata$Post4


treatall = lm(Hits ~ Attendees + Pre3 + Pre2 + Post1 + Post2 + Post3 + Post4 + AttPre3 + AttPre2 + AttPost1 + AttPost2 + AttPost3 + AttPost4, data=mydata)
summary(treatall)



mydata$AnyAttendees <- ifelse(mydata$Attendees>0,1,0)

mydata$AnyPre3 <- mydata$AnyAttendees*mydata$Pre3
mydata$AnyPre2 <- mydata$AnyAttendees*mydata$Pre2
mydata$AnyPre1 <- mydata$AnyAttendees*mydata$Pre1
mydata$AnyPost1 <- mydata$AnyAttendees*mydata$Post1
mydata$AnyPost2 <- mydata$AnyAttendees*mydata$Post2
mydata$AnyPost3 <- mydata$AnyAttendees*mydata$Post3
mydata$AnyPost4 <- mydata$AnyAttendees*mydata$Post4

treatany = lm(Hits ~ AnyAttendees + Pre3 + Pre2 + Post1 + Post2 + Post3 + Post4 + AnyPre3 + AnyPre2 + AnyPost1 + AnyPost2 + AnyPost3 + AnyPost4, data=mydata)
summary(treatany)


treatkink = lm(Hits ~ AnyAttendees + Attendees + Pre3 + Pre2 + Post1 + Post2 + Post3 + Post4 + AnyPre3 + AnyPre2 + AnyPost1 + AnyPost2 + AnyPost3 + AnyPost4 + AttPre3 + AttPre2 + AttPost1 + AttPost2 + AttPost3 + AttPost4, data=mydata)
summary(treatkink)