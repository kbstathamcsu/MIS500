
library(ggplot2)
#Step 1 - Importing Data
#_______________________________________________________

#Importing the xlxs London Burrough carbon emissions data 

data<-read.csv("C:\\Users\\kbsta\\R work\\MIS500 data\\carbon-emissions-borough-per-capita.csv")


#Step 2 - Validate data for correctness
#______________________________________________________

#Count of Rows and columns
dim(data)

#View top 10 rows of the dataset
head(data,10)

#Step 3 - Data transformation.  Only use a subset of the data and convert to correct formats.
#____________________________________________________________________________________________

#Lots of carbon emissions data.  Only select out the Grand totals for 2013 and 2014 

dataGrandTotals <- subset(data, select = c(Name, Grand.Total.2013, Grand.Total.2014), na.rm = TRUE)
dataGrandTotals

#only select the London Burroughs locations (rows 1 - 33).

londonGrandTotals <- dataGrandTotals[1:33,]

# Carbon emissions(value) is a character string with commas.  Convert the value to a number so we can do statistical computations on this value.
library(dplyr)

replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

londonGrandTotals$Grand.Total.2013 <- replaceCommas(londonGrandTotals$Grand.Total.2013)
londonGrandTotals$Grand.Total.2014 <- replaceCommas(londonGrandTotals$Grand.Total.2014)

#convert the dataframe from a wide format to a long format so we can group by year
library(data.table)
long <- melt(setDT(londonGrandTotals), id.vars = c("Name"), variable.name = "year")

head(long, 50)


#Step 4 - Statistical methods.
#___________________________________________

#Compute summary statistics by groups


group_by(long, long$year) %>%
  
  summarise(
    
    count = n(),
    
    mean = mean(value, na.rm = TRUE),
    
    sd = sd(value, na.rm = TRUE)
    
  )

require(graphics)

# Compute independent t-test between two years 2013 and 2014)

res <- t.test(value ~ year, data = long, var.equal = TRUE)

res

plot(value ~ year, data = long)



#test whether the average emissions in 2013 is more than the average emissions in year 2014:

t.test(value ~ year, data = long,
       
       var.equal = TRUE, alternative = "greater")




