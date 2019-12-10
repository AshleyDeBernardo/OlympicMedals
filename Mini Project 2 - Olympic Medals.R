install.packages("httr")
install.packages("XML")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("car")

library(httr)
library(XML)
library(stringr)
library(dplyr)
library(ggplot2)
library(caret)
library(car)

#Sets URL to pull data from
url <- 'https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table'

#Pulls the data from Wikipedia
get_object <- GET(url) 
Olympic.Medals <- htmlParse(get_object) 

#makes a list of all tables on the Wikipedia page
page.tables <- readHTMLTable(Olympic.Medals, stringsAsFactors = FALSE)

#View page tables to determine which is the one you want the data from
View(page.tables)

#Creates data frame from the data pulled from wikipedia
Olympic.Medals.Table <- page.tables[[2]]

#Delete first and second rows containing column names
Olympic.Medals.Table <- Olympic.Medals.Table[-c(1), ]
Olympic.Medals.Table <- Olympic.Medals.Table[-c(1), ]
#Deletes last row containing totals
Olympic.Medals.Table <- Olympic.Medals.Table[-c(153), ]
#Sets column names of data frame
colnames(Olympic.Medals.Table) <- c("Team", "SummerNumber", "SummerGold", 
                        "SummerSilver", "SummerBronze", "SummerTotal",
                        "WinterNumber", "WinterGold", "WinterSilver",
                        "WinterBronze", "WinterTotal", "CombinedNumber",
                        "CombinedGold", "CombinedSilver", 
                        "CombinedBronze", "CombinedTotal") 

#For loop to set columns 2 to 16 as numeric
for (i in (2:16)){
  Olympic.Medals.Table[ , i] <- as.numeric(Olympic.Medals.Table[ , i])
}

#Regression

#Summer total medals vs. winter total medals
Summer.Winter = lm(SummerTotal ~ WinterTotal, data = Olympic.Medals.Table) 
summary(Summer.Winter)

#Number of summer games competed in vs. summer total medals
Number.Summer = lm(SummerTotal ~ SummerNumber, data = Olympic.Medals.Table)
summary(Number.Summer)

#Number of winter games competed in vs. winter total medals
Number.Winter = lm(WinterTotal ~ WinterNumber, data = Olympic.Medals.Table)
summary(Number.Winter)

#Number of combined games competed in vs. combined total medals
Number.Combined = lm(CombinedTotal ~ CombinedNumber, data = Olympic.Medals.Table)
summary(Number.Combined)

#Number of summer gold medals vs. number of winter gold medals
GoldSummer.GoldWinter = lm(SummerGold ~ WinterGold, data = Olympic.Medals.Table) 
summary(GoldSummer.GoldWinter)


