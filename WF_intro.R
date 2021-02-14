
library(dplyr)#for aggregating, data manipulation, etc.
library(ggplot2)#professional graphics
library(lubridate)#for working with dates
library(readxl) #we were given a master excel sheet

#start with a clean slate!
rm(list = ls())
setwd("C:/Users/matth/Desktop/OneDrive - Drake University/SPRING 2021/STAT 190/Project 1")

#Read in all 4 data frames
projects <- read_excel("Drake Data Set 20201222B.xlsx", sheet = "Projects")
risks <- read_excel("Drake Data Set 20201222B.xlsx", sheet = "Risks")
issues <- read_excel("Drake Data Set 20201222B.xlsx", sheet = "Issues")
crs <- read_excel("Drake Data Set 20201222B.xlsx", sheet = "Change Requests")

View(projects)


#QUESTION: Can we think of "projects" as containing pre-project-initiation data?
#---- "inherent risk" assessed pre project 
#and risks, issues, crs containing post-project-initiation data?

#1st: just look at the data
head(projects) #or View(projects)
head(risks) #or View(risks) #note EACH PROJECT HAS A ROW
head(issues) #or View(issues)
head(crs) #or View(crs)


#What columns do we have
colnames(projects)
colnames(risks)
colnames(issues)
colnames(crs)

#Those are some gruesome column names. Lots of "\r", "\n", -, and spaces, 
#which don't play (very) nicely in R

#Here's an easy way to clean those names up:

colnames(projects) <- gsub("[\r\n -]","",colnames(projects) )
colnames(risks) <- gsub("[\r\n -]","",colnames(risks) )
colnames(issues) <- gsub("[\r\n -]","",colnames(issues) )
colnames(crs) <- gsub("[\r\n -]","",colnames(crs) )

#More detailed look at the data
#PROJECTS and their categorized predicted risks
str(projects)

#Columns ManualOverrideRisk - StrategicAlignmentRisk (18 columns)
#These are risks assessed BEFORE the project starts
#We want to investigate whether or not these can forecast realized risks/issues/changes
#that occur AFTER the project starts

#there appears to be a project ID
#note that one line = one project

#REALIZED RISKS 
str(risks)
#again, there is a project ID
#however, there are multiple lines per project. 
#one line = one risk?
#View(risks)

#QUESTION: many projects with 1 row and missing values... does this mean there were no documented risks?
#----> yes.

#QUESTION: What is an example of a "risk"?
#----> a risk 'might' happen but an issue happened. 
#----> manager may say, "if it happens it will be really bad" (rating in data set)
#----> e.g., COVID was an issue, but they didn't forsee it so it was never a risk"
#----> see RiskResponse

#QUESTION: What would a 'successful' project look like in this risk data set?
#-----> the number of issues, risks, cr is the important part
#-----> may weight them by probability or impact
#-----> "weighting" scheme might be interesting


#REALIZED ISSUES 
str(issues)
#same kind of situation as risks
#we will eventually want to use the 18 risks to predict both realized risks and issues


#PROJECTS
#check `Overall Risk`
#this is the pre-project start assessment of overall risk of the project and it should be categorical
#categorical ---> make a frequency table
table(projects$OverallRisk)
#Nothing of concern here

#make a visual of this frequency table
ggplot(data = projects) +
  geom_bar(aes(x = OverallRisk))
#hmmm, we've got some missing values. Missing values can be ambiguous in meaning.
#QUESTION: What does this say about a project? e.g., data entry error vs "zero" risk, etc..?
#----> risk rating is contingent on status
#initiating projects may not have overall risk


#Also, look at that ordering. 
#Should tell R that there is an inherent ordering to some of these variables. 
#this will make for better tables, figures, and eventually more sensible models too
projects$OverallRisk <- factor(projects$OverallRisk, 
                               levels = c("Low", "Moderate", "High"))
#in the code above, we listed the levels as they should appear in order
ggplot(data = projects) +
  geom_bar(aes(x = OverallRisk))
#better.

#can do this for any of the 18 initial assessments of inherent risks
#see if there is anything weird that needs to be addressed


#check `Manual Override`
table(projects$ManualOverrideRisk)
#Here, the character string "N/A" probably stands for NA or missing
#make a visual of this frequency table

ggplot(data = projects) +
  geom_bar(aes(x = ManualOverrideRisk))
#alright, so we've got a lot of "N/A" and actual NA values
#for now, replace "N/A" with NA.
projects$ManualOverrideRisk[projects$ManualOverrideRisk == "N/A"] <- NA
projects$ManualOverrideRisk <- factor(projects$ManualOverrideRisk ,
                                      levels = c("Low", "Moderate", "High"))


#There is an ManualOverrideRisk and an OverallRisk: is there a difference?
ggplot(data = projects) +
  geom_bar(aes(x = OverallRisk, fill = ManualOverrideRisk))

table(projects$OverallRisk, projects$ManualOverrideRisk)
#they are they same, except for where manual override is missing
#thus, we likely wouldn't put them both in a model as x variables
#----> use overall risk instead of manual override

#RISKS
#after the project starts, risks can potentially happen
#each row represents one of those risks
#risks have potential impacts
ggplot(data = risks) +
  geom_bar(aes(x = RiskImpactRating)) #would re-order using factor()
#risks have different types
ggplot(data = risks) +
  geom_bar(aes(x = RiskType))
#there appears to be different kinds of risks
#QUESTION: does WF care about these various kinds of risks?
#e.g., maybe it's more important to reduce financial risks
#than it is to prevent schedule risks
#-----> differences between the types is interesting
#historically, financial wasn't considered at all. "open checkbook"

#riskresponse - a risk was identified, how was it dealt with?
ggplot(data = risks) +
  geom_bar(aes(x = RiskResponse))


#QUESTION: is the number or risks/issues important?
#Or, when a risk/issue occurs in the timeine of a project? (we are given dates...)
#Or is it more a question of whether or not a risk/issue occurred at all?


#We could do similar investigations for issues and crs


#The number of risks is likely important:
#count the number of risks per project
#count the number of high impact risks
risks_agg <- risks %>%
  group_by(PRJID) %>% #want to aggregate by unique PRJID
  summarise(
    n = sum(RiskImpactRating %in%c("High", "Low", "Medium")), #count number of not NA
    n_HighImpact = sum(RiskImpactRating %in%c("High"))) #count only number of "High"


summary(risks$CreatedDate)
#that's odd. 
#This is because Microsoft Excel logs dates as the deviation from December 30, 1899
#(shrugging emoji)

#This is how we'll tell R to recognize this as a date, and correctly
risks$Created_Date <- as.Date(as.numeric(risks$CreatedDate), 
                              origin = "1899-12-30")
#check to see if it worked:
summary(risks$Created_Date)
#that's better
#you would do this for all date variables in any data set
