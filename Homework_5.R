# HOMEWORK 5
# Federico Ferrero
# 4/19/2021

# clear your memory
rm(list=ls())

# load packages
library(ergm)
library(UserNetR)
library(igraph)
library(statnet)
library(ggplot2)
library(network)

# load Florentine marriage data matrix 
data(flo)
flo

# create a network object out of the adjacency matrix 
flomarriage <- network(flo, directed=FALSE)
flomarriage[,] 
class(flomarriage)

# create a vector indicating the wealth of each family (in thousands of lira) 
# and add it to the network object as a covariate
wealth <- flomarriage %v% "wealth" <-c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3)
wealth
flomarriage

# load a network object of the Florentine data 
data(florentine)
?florentine
flomarriage

#degree distribution
degree(flomarriage)

deg_dis <- as.data.frame(table(degree(flomarriage)))
colnames(deg_dis) <- c('Degree','Frequency')
plot(deg_dis, main="Degree Distribution Florentine Network")

# calculate eigenvector centrality of each node
?evcent
network.vertex.names(flomarriage) # it gives me the names of nodes
evcent(flomarriage)

# create a visualization of the network where nodes are sized by wealth
plot(flomarriage, label= network.vertex.names(flomarriage), vertex.cex=wealth/15,
     main= "Florentine Network by Wealth")

network.vertex.names(flomarriage)
wealth

dev.off()

# set ERGM: a null model
library(ergm)
null <- ergm(flomarriage~edges,
               control=control.ergm(seed=40))
class(null)
summary(null)

# the null model is constrained by the number of edges in the observed network
# The probability of an edge being drawn should in theory be the same as density - let’s check.
plogis(coef(null))
gden(flomarriage)

# add node attribute: wealth
mod1 <- ergm(flomarriage~edges+nodecov('wealth'),
             control=control.ergm(seed=40))
summary(mod1)

# calculate betweenness centrality of each node
network.vertex.names(flomarriage) # it gives me the names of nodes
betw<-betweenness(flomarriage, gmode="graph") # gmode is set to "digraph" by default."digraph" indicates that edges should be
betw
# and add it to the network object as a covariate
betw <- flomarriage %v% "betw" <- betw
flomarriage

# add betweenness centrality as a dyad attribute to the model
mod2 <- ergm(flomarriage~edges+nodecov('wealth')+nodematch('betw'),
             control=control.ergm(seed=40))
summary(mod2)
