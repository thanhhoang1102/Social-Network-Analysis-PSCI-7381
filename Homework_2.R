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

# Converting the sociomatrix into a network object using STATNET
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

# Converting the edgelist into a network object using STATNET
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

# summary of the net
summary(net1)

# identifying isolates
length(isolates(net1))

# degree of nodes
degree(inet1) # gmode is set to "digraph" by default

# create a vertex (node) attribute: nationality (either American or Mexican)  
library(statnet)
set.vertex.attribute (net1, "nationality", c("Mexican", "Mexican", "American", 
                                             "American", "Mexican", "Mexican"))

# see the attributes assigned
list.vertex.attributes(net1)
get.vertex.attribute(net1, "nationality")
summary(net1)

# create an edge (tie) attribute: number of sent emails
list.edge.attributes(net1)
set.edge.attribute(net1,"emails", c("1", "2", "5", 
                                    "7", "3"))
# see the attributes assigned
list.edge.attributes(net1)
summary(get.edge.attribute(net1,"emails"))
as.sociomatrix(net1, "emails")

# Importing edgelist data into R
detach (package:statnet)
library(igraph)

edges<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/Edgelist-Media-Edges.csv", header=T, as.is=T)
nodes<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/Edgelist-Media-Nodes.csv", header=T, as.is=T)

network <- graph_from_data_frame(d=edges, vertices=nodes, directed = TRUE)
network
summary(network)

# transform network from igraph to network (statnet) and obtain the summary
library(intergraph)
class(network)

networkasnet <- asNetwork(network) 
class(networkasnet)
summary(networkasnet)

# degree of nodes
V(network)$media
degree(network) # gmode is set to "digraph" by default

# betweenness centrality
betweenness(network) # gmode is set to "digraph" by default.

# plot
plot(network, edge.arrow.size=.4,vertex.label=V(network)$media)
