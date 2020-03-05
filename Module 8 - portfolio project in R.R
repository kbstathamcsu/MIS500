
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lattice)
library(ggpubr)
#Step 1 - Importing Data
#_______________________________________________________

#Importing the csv data 
#data<-read.csv(file.choose())
data<-read.csv("C:\\Users\\kbsta\\R work\\MIS500 data\\survey_results_public.csv")


#Step 1.1 - Validate data for correctness
#______________________________________________________

#Count of Rows and columns
dim(data)

#View top 10 rows of the dataset
head(data,10)
     
#step 2.0 - Lot's of columns. Create a new data set with only the columns that may impact the independent variable ConvertedSalary
#---------------------------------------------------------------------------------------
data2 <- select(data, "Respondent", "ConvertedSalary", "Age", "YearsCodingProf", "CareerSatisfaction", "CompanySize", 
                "ErgonomicDevices", "Exercise", "Gender",  "Dependents", "DevType", "LanguageWorkedWith")

head(data2,10)


#step 2.1 - Exploring and transforming data.  find unique values for categorial columns
levels(factor(data2$CareerSatisfaction))
levels(factor(data2$CompanySize))
levels(factor(data2$YearsCodingProf))
levels(factor(data2$ErgonomicDevices))
levels(factor(data2$Exercise))
levels(factor(data2$Gender))
levels(factor(data2$Age))
levels(factor(data2$Dependents))
levels(factor(data2$DevType))
levels(factor(data2$LanguageWorkedWith))


#Step 2.2 put categorial variables with ordinal values in the correct order.

#Put CareerSatisfaction in correct order


data2$CareerSatisfaction <- factor(data2$CareerSatisfaction,levels = 
                                  c("Extrmemly dissatisfied", "Moderately dissatisfied", "Slightly dissatisfied", 
                                    "Neither satisfied nor dissatisfied", "Slightly satisfied", "Moderately satisfied",
                                    "Extremely satisfied"))

#Put Age in correct order
data2$Age <- factor(data2$Age,levels = 
                                     c("Under 18 years old", "18 - 24 years old", "25 - 34 years old", 
                                       "35 - 44 years old", "45 - 54 years old", "55 - 64 years old",
                                       "65 years or older"))
#Put YearsCodingProf in correct order
data2$YearsCodingProf <- factor(data2$YearsCodingProf,levels = 
                      c("0-2 years", "3-5 years", "6-8 years", "9-11 years", "12-14 years", "15-17 years",
                        "18-20 years", "21-23 years", "24-26 years", "27-29 years", "30 or more years"
                        ))

#Put CompanySize in correct order
data2$CompanySize <- factor(data2$CompanySize,levels = 
                                  c("Fewer than 10 employees", "10 to 19 employees", "20 to 99 employees", "100 to 499 employee", 
                                    "12-14 years", "15-17 years", "500 to 999 employees", "1,000 to 4,999 employees", 
                                    "5,000 to 9,999 employees", "10,000 or more employees"
                                  ))

#Put Exercise in correct order
data2$Exercise <- factor(data2$Exercise,levels = 
                              c("I don't typically exercise", "1 - 2 times per week", "3 - 4 times per week", "Daily or almost every day"
                              ))



#  TODO - Following code not working.  Too many choices for gender,  if not Female or Male, then set Gender to Other
#data2$Gender <- lapply(data2$Gender, as.character)
# if (as.numeric(factor(data2$Gender)) != 1){
#   if (as.numeric(factor(data2$Gender)) != 9){
#     factor(data2$Gender) <- 3
#     data2$Gender = "Other"
#   }
# }
#as.numeric(factor(data2$YearsCodingProf)
# data2$Gender <- factor(data2$Gender,levels = 
#                            c("Female", "Male", "Other"
#                            ))



#Step 2.3 Convert Converted Salary to numeric
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

data2$ConvertedSalary <- replaceCommas(data2$ConvertedSalary)

#step3.0 - run scatter plots with converted salary and other variables that may effect salary.
data3 <- data2

#step 3.1
splom(~data3[c("ConvertedSalary", "Age", "YearsCodingProf")], groups=NULL, data=data3,
      axis.line.tck = 0,
      axis.text.alpha = 0)

#step 3.2  scatter plot of salary with other independent vars
splom(~data3[c(2, 5:8)], groups=NULL, data=data3,
      axis.line.tck = 0,
      axis.text.alpha = 0)

#step 3.3 - convert years coding and age to a numeric value to see if it changes the look of the scatter plot.
data3$YearCodingProfAsNumber <- as.numeric(factor(data3$YearsCodingProf))
data3$AgeAsNumber <- as.numeric(factor(data3$Age))

splom(~data3[c("ConvertedSalary", "AgeAsNumber", "YearCodingProfAsNumber")], groups=NULL, data=data3,
      axis.line.tck = 0,
      axis.text.alpha = 0)

#Step 4 - summarise statistics
#Step 4.1 do a summary of convertedSalary grouped by age group
data3 %>%
  filter( is.na(Age) == FALSE) %>% 
  group_by(Age) %>%
  
  summarise(
    
    count = n(),
    
    mean = mean(ConvertedSalary, na.rm = TRUE),
    
    sd = sd(ConvertedSalary, na.rm = TRUE)
    
  )

#step 4.2 do a summary of convertedSalary grouped by YearsCodingProf
data3 %>%
  filter( is.na(YearsCodingProf) == FALSE) %>% 
  group_by(YearsCodingProf) %>%
  
  summarise(
    
    count = n(),
    
    mean = mean(ConvertedSalary, na.rm = TRUE),
    
    sd = sd(ConvertedSalary, na.rm = TRUE)
    
  )

# step 5 - bar charts
#Step 5.1 - Bar chart Age vs.the mean of the converted salary.  Remove records with Age = NA
#_______________________________________________________
ggplot(subset(data3, !is.na(Age)), aes(x=factor(Age), y=ConvertedSalary)) + 
  #  geom_bar(stat = "identity", fill = "darkblue") +
  stat_summary(fun.y = "mean", geom="bar", fill="darkblue") +
  scale_x_discrete("Age")+ scale_y_continuous(name = waiver(), breaks = waiver(),
                                              minor_breaks = waiver(), labels = waiver(), limits = NULL,
                                              expand = waiver(),
                                              trans = "identity", position = "left", sec.axis = waiver()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + labs(title = "Age and Salary Correlation")

#Step 5.2 - Bar chart Years coding vs.the mean of the converted salary.  Remove records with years coding = NA
ggplot(subset(data3, !is.na(YearsCodingProf)), aes(x=factor(YearsCodingProf), y=ConvertedSalary)) + 
  #  geom_bar(stat = "identity", fill = "darkblue") +
  stat_summary(fun.y = "mean", geom="bar", fill="darkblue") +
  scale_x_discrete("Years coding professionally")+ scale_y_continuous(name = waiver(), breaks = waiver(),
                                                                minor_breaks = waiver(), labels = waiver(), limits = NULL,
                                                                expand = waiver(),
                                                                trans = "identity", position = "left", sec.axis = waiver()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + labs(title = "Years Professional Coding and mean Salary Correlation")

#Step 6 hypothesis testing using t-test

require(graphics)

# Compute independent t-test between salaries of small companies vs. big companies for the same # years professional coding experience.
# create a dataset with just records for YearsCodingProf of 21-23 years and  CompanySize of 20 to 99 employees and 10,000 or more employees
dataT <- subset(data3, YearsCodingProf == '21-23 years' & (CompanySize == '20 to 99 employees' | CompanySize == '10,000 or more employees'))

dim(dataT)
head(dataT, 50)

group_by(dataT, dataT$CompanySize) %>%
  
  summarise(
    
    count = n(),
    
    mean = mean(ConvertedSalary, na.rm = TRUE),
    
    sd = sd(ConvertedSalary, na.rm = TRUE)
    
  )

res <- t.test(ConvertedSalary ~ CompanySize, data = dataT, var.equal = TRUE)

res

plot(ConvertedSalary ~ CompanySize, data = dataT)


# Shapiro-Wilk normality test for salary at small companies
with(dataT, shapiro.test(ConvertedSalary[CompanySize == "20 to 99 employees"]))# p = 0.1
# Shapiro-Wilk normality test for salary at very larg companies
with(dataT, shapiro.test(ConvertedSalary[CompanySize == "10,000 or more employees"])) # p = 0.6

#ftest to test for variance
res.ftest <- var.test(ConvertedSalary ~ CompanySize, data = dataT)
res.ftest

#t test for the hypothesis of the mean salary being equal between the two samples.  var.equal set to FALSE
res2 <- t.test(ConvertedSalary ~ CompanySize, data = dataT, var.equal = FALSE)

res2
#t test with var.equal set to FALSE
resAlt <- t.test(ConvertedSalary ~ CompanySize, data = dataT, var.equal = FALSE, alternative = "greater")

resAlt


# Plot salary by company size and color by company size
library("ggpubr")
ggboxplot(dataT, x = "CompanySize", y = "ConvertedSalary", 
          color = "CompanySize", palette = c("#00AFBB", "#E7B800"),
          ylab = "ConvertedSalary", xlab = "CompanySize")




#----------------------------------------------------------------------------------------------------------------------------
#**** just some extra statistics I explored.  ******************

#separate out DevType text by ";" into spearate columns i.e

#data4 <- separate(data3, "DevType", c("DevType1", "DevType2", "DevType3", "DevType4", "DevType5", "DevType6"), sep=";")
#head(data4, 10)

#step 4.0
#Step 4.1 Compute summary statistics for the ConvertedSalary by CareerSatisfaction groups 
data3 %>%
  filter( is.na(CareerSatisfaction) == FALSE) %>% 
  group_by(CareerSatisfaction) %>%
  
  summarise(
    
    count = n(),
    
    mean = mean(ConvertedSalary, na.rm = TRUE),
    
    sd = sd(ConvertedSalary, na.rm = TRUE)
    
  )

# Under 18 years old had a mean salary of $528000.  Looks suspicious.  Lets only look at under 18 records
dataUnder18 <- subset(data3, Age == 'Under 18 years old')
head(dataUnder18)
# Let's eliminate the under 18 records and try the scatter plot again
dataOver18 <- subset(data3, Age != 'Under 18 years old')

#step 3 - Determine which independent variables influence salary
#step 3.1 - summary of the dependent and independant variables
summary(dataOver18)

dataOver18 %>%
  filter( is.na(Age) == FALSE) %>% 
  group_by(Age) %>%
  
  summarise(
    
    count = n(),
    
    mean = mean(ConvertedSalary, na.rm = TRUE),
    
    sd = sd(ConvertedSalary, na.rm = TRUE)
    
  )


#step 3.2 - scatter plot
dataOver18$YearCodingProfAsNumber <- as.numeric(factor(dataOver18$YearsCodingProf))
dataOver18$AgeAsNumber <- as.numeric(factor(dataOver18$Age))

#Step 4 - Bar chart job satistfaction vs.the mean of the converted salary.  Remove records with JobSatisfaction = NA
#_______________________________________________________
ggplot(subset(data2, !is.na(CareerSatisfaction)), aes(x=factor(CareerSatisfaction), y=ConvertedSalary)) + 
  #  geom_bar(stat = "identity", fill = "darkblue") +
  stat_summary(fun.y = "mean", geom="bar", fill="darkblue") +
  scale_x_discrete("Job Satisfaction Type")+ scale_y_continuous(name = waiver(), breaks = waiver(),
                                                                minor_breaks = waiver(), labels = waiver(), limits = NULL,
                                                                expand = waiver(),
                                                                trans = "identity", position = "left", sec.axis = waiver()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + labs(title = "Job Satisfaction and Salary Correlation")

#Step 4.1 - Bar chart YearsCodingProf vs.the mean of the converted salary.  Remove records with YearsCodingProf = NA

#step 5 - Assign numerical values to jobSatisfaction and use chi square correlation between JobSatisfaction and Salary
data2$JobSatisfactionAsNumber <- as.numeric(factor(data2$JobSatisfaction)) -1
table(data2$JobSatisfactionAsNumber, data2$ConvertedSalary)
chisq.test(data2$JobSatisfactionAsNumber, data2$ConvertedSalary)

#step 5.1 - Assign catagorical  values to ConvertedSalary and use chi square correlation between JobSatisfaction and Salary
data2$ConvertedSalaryAsCat <- cut(data2$ConvertedSalary, br=c(-1,30000,60000,90000,120000))
table(data2$JobSatisfaction, data2$ConvertedSalaryAsCat)
chisq.test(data2$JobSatisfaction, data2$ConvertedSalaryAsCat )

#step 5.5 - chi square correlation between JobSatisfaction and Gender
table(data2$JobSatisfaction, data2$Gender)
chisq.test(data2$JobSatisfaction, data2$Gender)

#step 6.  Assign numerical values to jobSatisfaction and run correlation function
data2$JobSatisfactionAsNumber <- as.numeric(factor(data2$JobSatisfaction)) -1
cor(data2$JobSatisfactionAsNumber, data2$ConvertedSalary, use="complete.obs", method="pearson")
cov(data2$JobSatisfactionAsNumber, data2$ConvertedSalary, use="complete.obs")
