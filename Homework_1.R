#Homework 1
library(statnet)
library(UserNetR)
data("FIFA_Nether")

?FIFA_Nether

#simple visualization
plot(FIFA_Nether, displaylabels=TRUE)

#network size
network.size(FIFA_Nether)
summary(FIFA_Nether,print.adj=TRUE)

#density
gden(FIFA_Nether)

#number of components (subgroup in which all actors are conected, directely or indirectely)
components(FIFA_Nether)

#diameter
gd <- geodist(FIFA_Nether)
max(gd$gdist)

#clustering by transitivity
gtrans(FIFA_Nether,mode="graph")

#Homework  MORENO DATASET (pag 11)
install.packages('statnet')
library(statnet)
library(UserNetR)
data(Moreno)

#simple visualization
gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

#network size
network.size(Moreno)
summary(Moreno,print.adj=FALSE)

#density
#calculated by hand
den_hand <- 2*46/(33*32)
den_hand
#calculated with function
gden(Moreno)

#number of components (subgroup in which all actors are conected, directely or indirectely)
components(Moreno)

#diameter
lgc <- component.largest(Moreno,result="graph")
gd <- geodist(lgc)
max(gd$gdist)

#clustering by transitivity
gtrans(Moreno,mode="graph")
