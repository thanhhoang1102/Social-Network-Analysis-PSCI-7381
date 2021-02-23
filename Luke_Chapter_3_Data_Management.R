# Homework 2
# PSCI 7381
# Prof. Dr. Santoro
# Student: Federico Ferrero
# Chapter 3 Luke

# clean 
rm(list=ls())

# libraries
library(statnet)
library(network)

# Creating a network object (with adjacency matrices)
netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency")
class(net1)

summary(net1)

# plot the network

gplot(net1, vertex.col = 2, displaylabels = TRUE)

# creating network with edge list (the same network as net1)

netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2 <- network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")
summary(net2)

# plot the network

gplot(net2, vertex.col = 2, displaylabels = TRUE)

# transformations
as.sociomatrix(net1)

class(as.sociomatrix(net1))

# A more general coercion function is as.matrix(). It can be used to produce
#a sociomatrix or an edgelist matrix.

all(as.matrix(net1) == as.sociomatrix(net1))
as.matrix(net1,matrix.type = "edgelist")

# set attributes to nodes (assigning gender to the nodes in net1)

set.vertex.attribute(net1, "gender", c("F", "F", "M",
                                       "F", "M"))
# set attributes to nodes (assigning all degree to the nodes in net1)

net1 %v% "alldeg" <- degree(net1)

# see the attributes assigned
list.vertex.attributes(net1)

summary(net1)

# see the actual values stores in a vertex attribute
get.vertex.attribute(net1, "gender")
net1 %v% "alldeg"

# now, assigning tie attributes

list.edge.attributes(net1)

set.edge.attribute(net1,"rndval",
                   runif(network.size(net1),0,1))
list.edge.attributes(net1)

summary(net1 %e% "rndval")

summary(get.edge.attribute(net1,"rndval"))

# an example: 5 members indicate how much like each other (from 0 to 3)
netval1 <- rbind(c(0,2,3,0,0),
                 c(0,0,3,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,2,0,0))
netval1 <- network(netval1,matrix.type="adjacency",
                   ignore.eval=FALSE,names.eval="like") #this line tells the network function to evaluate the actual values in the sociomatrix, and store those values in a new edge attribute called ‘like.’
network.vertex.names(netval1) <- c("A","B","C","D","E")
list.edge.attributes(netval1)
get.edge.attribute(netval1, "like")

as.sociomatrix(netval1) #indicates that nodes are related or not

as.sociomatrix(netval1,"like") #indicates the value of the tie according to "like"

# creating a network object in igraph
