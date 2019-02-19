setwd("C:/Users/Stuart Toda/Documents/Senior 1st Semester/Applied Multivariate Stats/Final Project")

county <- read.csv("Data by County.CSV")

View(county)

newcounty <- county [-63,]
summary(newcounty)

a <- county[-63,-1]
View(a)
b <- a[,-1:-7]
View(b)
county_stats <- b[,-3]
View(county_stats)

summary(county_stats)

#compute the correlation matrix because data is not standardized
county_cor <- cor(county_stats)



#bartlett test-whether there is enough correlation to continue on with analysis
install.packages("psych")
library(psych)
cortest.bartlett(county_cor,n=62) 
  #if bartlett test is significant then move on. Reminder - if p-value is low then it is significant


#if we do covariance test, what happens?
#We can't move on!.....
#run KMO test - sampling adequacy (book says its basically useless)
install.packages("rela")
library(rela)
county_matrix<-as.matrix(county_stats)
county_paf<-paf(county_matrix,eigcrit=1,convcrit=.001)
summary(county_paf)

#find the determinant of correlation matrix (we want this to be positive and DEF not zero); if it's zero we can't move on
det(county_cor)

#Principal components analysis
pc_county_cor<- principal(county_cor,nfactors=11,rotate="none")#nfactors = number of variables
pc_county_cor
pc_county <- prcomp(county_stats, scale = TRUE)
#diagrams
scree(county_cor, main = "scree plot")
fa.diagram(pc_county_cor)
biplot(pc_county)



  #Finding PC Scores

pc_county_cor_1 <- prcomp(county_stats, scale = TRUE)

#I am finding principal compoenents scores
pc_scores <- pc_county_cor_1$x
View(pc_scores)
#Then I will isolate the principal components to 1 and 2 because thats the only one that has an eigenvalue over 1 indicated by the scree plot

pc1 <- pc_scores[,1]
View(pc1)

sorted_pc1 <- sort(pc1)
View(sorted_pc1)

pc2<- pc_scores[,2]
View(pc2)

sorted_pc2 <- sort(pc2)
View(sorted_pc2)
###############################################
head(county_stats)

attach(county_stats)
crime_linear <- lm(Agg..Assault.Rate.per.Capita~Robbery.Rate.per.Capita+Population.Density..per.Square.Mile.+MV..Theft.Rate.per.Capita+Larceny.Rate.per.Capita)

summary(crime_linear)
