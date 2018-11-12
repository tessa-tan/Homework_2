
#Importing WHO dataset into R 
WHO <- read.csv("WHO.csv") 

WHO$Country[which.min(WHO$LiteracyRate)] #Answer to d. country with the lowest literacy rate

# Subset with only the countries in Europe
WHO.Europe = subset(WHO, Region == "Europe")
WHO.Europe$Country[which.max(WHO.Europe$GNI)] #Answer to e. Richest country in Europe based on GNI

# Subset with only the countries in Africa
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy) #Answer to f. Mean Life expectancy of countries in Africa

sum(WHO$Population > 10000) # Answer to g. Number of countries with population greater than 10,000

# Subset with only the countries in Americas
WHO.Americas = subset(WHO, Region == "Americas")
WHO.Americas <- WHO.Americas[order(-WHO.Americas$ChildMortality),] #arrange descending by ChildMortality
head(WHO.Americas$Country,5) #outputs the first 5 countries of the sorted by ChildMortality data set / answer to h.


# Question 2, NBA Dataset
#install.packages("readxl")
#library(readxl)
#NBA <- read_excel("Historical NBA Performance.xlsx")

NBA <- read.csv("Historical NBA Performance.csv")
Bulls <- subset(NBA, Team=="Bulls") #subset Bulls data
Bulls$Year[which.max(Bulls$Winning.Percentage)] #Answer to a. The year Bulls has the highest winning percentage

EvenWP <- subset(NBA, Winning.Percentage==0.5)
EvenWP$Team #Answer to b. Teams with an even win-loss record in a year


# Question 3, Seasons_Stats Dataset
S_Stats <- read.csv("Seasons_Stats.csv") #opens dataset and stores it into df S_Stats
#S_Stats_maxX3PAr <- subset(S_Stats, X3PAr == max(S_Stats$X3PAr,na.rm=TRUE)) #subsets the players with the max X3PAr
#S_Stats_maxX3PAr$Player #a. Player with the highest 3-pt attempt rate in a season.
#S_Stats$Player[which.max(S_Stats$FTr)] #b. Player with the highest free throw rate in a season.

Lebron_Stats = subset(S_Stats, Player == "LeBron James") #subset LeBron's stats 
Lebron_Stats$Year[which.max(Lebron_Stats$PTS)] #c. What year/season does Lebron James scored the highest?

# d. What year/season does Michael Jordan scored the highest?
Jordan_Stats = subset(S_Stats, Player == "Michael Jordan*")
Jordan_Stats$Year[which.max(Jordan_Stats$PTS)]

#e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
Bryant_Stats = subset(S_Stats, Player == "Kobe Bryant")
Bryant_Stats$PER[which.min(Bryant_Stats$MP)]


# Question 4 National Universities Rankings.csv
NUS <- read.csv("National Universities Rankings.csv")

str(NUS)
#Because the data we need are factors, convert them into numeric first
NUS_Integers <- NUS #use NUS_Integers as converted df so we don't change orig data in case
NUS_Integers$Undergrad.Enrollment <- as.numeric(as.character(gsub(',',"",NUS$Undergrad.Enrollment)))
NUS_Integers$Tuition.and.fees <- as.numeric(gsub("[\\$,]","",NUS_Integers$Tuition.and.fees))

# a. University with the most number of undergrads
NUS_Integers$Name[which.max(NUS_Integers$Undergrad.Enrollment)]

# b. Average Tuition in the Top 10 University
top10s <- subset(NUS_Integers, Rank < 11)
mean(top10s$Tuition.and.fees)
