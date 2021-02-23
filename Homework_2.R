# Homework 2
# PSCI 7381
# Prof. Dr. Santoro
# Student: Federico Ferrero

# clean 
rm(list=ls())

# sociomatrix creation
sociomatrix <- rbind(c(0,1,0,0,0,0),
                     c(0,0,1,0,0,0),
                     c(1,0,0,0,0,0),
                     c(0,0,0,0,1,0),
                     c(0,0,0,0,0,0),
                     c(1,0,0,0,0,0))
rownames(sociomatrix) <- c("1","2","3","4","5","6")
colnames(sociomatrix) <- c("1","2","3","4","5","6")

sociomatrix

# edgelist format creation
edgelist <- rbind(c(1,2),
                  c(2,3),
                  c(3,1),
                  c(4,5),
                  c(6,1))
edgelist

# Converting the sociomatrix into a network object using STANET
library(statnet)
net1 <- network(sociomatrix,matrix.type="adjacency")
class(net1)
summary(net1)

# plot the network to verify if this coincides with the example provided
gplot(net1, vertex.col = 2, displaylabels = TRUE)

# Converting the sociomatrix into a network object using IGRAPH
detach (package:statnet)
library(igraph)

inet1 <- graph.adjacency(sociomatrix)
class(inet1)
summary(inet1)

# Converting the edgelist into a network object using STANET
library(statnet)

net2 <- network(edgelist,matrix.type="edgelist")
network.vertex.names(net2) <- c("1","2","3","4","5","6")
class(net2)
summary(net2)

# plot the network  to verify if this coincide with the example provided
gplot(net2, vertex.col = 2, displaylabels = TRUE)

# Converting the edgelist into a network object using IGRAPH
detach (package:statnet)
library(igraph)

inet2 <- graph.edgelist(edgelist)
class(inet2)
summary(inet2)

# degree of nodes
net1 %v% 'vertex.names' #it gives me the names of nodes
degree(inet2) # gmode is set to "digraph" by default.

# plot the network  to verify if this coincide with the example provided
gplot(net2, vertex.col = 2, displaylabels = TRUE)

# Converting the edgelist into a network object using IGRAPH
detach (package:statnet)
library(igraph)

# IGRAPH: creating network using edgelist
inet2 <- graph.edgelist(edgelist)
class(inet2)
summary(inet2)
