#Web Data Analysis
#install packages
install.packages("readxl")
library(readxl)

#set working directory
setwd("/Users/Marwaalgethami/Desktop/data science")
getwd()

#read file
web_date <- read_excel("internet_dataset.xlsx")

#view file 
View(web_date)

#summary about file
summary(web_date)

#dimention of the file... 32109 obs. of 8 var
dim(web_date)

table(web_date$Continent)


###################
 # to find the correlation between Uniquepageviews and  Visits
cor(web_date$Uniquepageviews,web_date$Visits)


anova_test<-aov(Uniquepageviews~Visits, data=web_date)
summary(anova_test)

################
# to find the varibles that affect exit variable
anova_test2<-aov(Exits~.,data = web_date)
summary(anova_test2)
        

#########
#to find the variables that effect on the time on page. 
anova_test3<-aov(Timeinpage~.,data = web_date)
summary(anova_test3)


##############
#Q5: determine the factors that are impacting the bounce.
#data for the variable bounces has to be between 0 and 1, 
web_date$Bounces=web_date$Bounces*0.01
genLinModel<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = web_date, family = "binomial")
summary(genLinModel)

genLinModel$aic
