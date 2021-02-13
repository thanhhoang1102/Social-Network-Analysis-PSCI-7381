# Homework 1
# PSCI 7381
# Prof. Dr. Santoro
# Student: Federico Ferrero

# loading data and libraries
library(statnet)
library(UserNetR)
data("FIFA_Nether")

?FIFA_Nether

# simple visualization
plot(FIFA_Nether, displaylabels=TRUE)

# network size
network.size(FIFA_Nether)
summary(FIFA_Nether,print.adj=TRUE)

# density
gden(FIFA_Nether)
# density calculated by hand in a directed network
den_hand <- 108/(11*10)
den_hand

# number of components (subgroup in which all actors are conected, directely or indirectely)
help(components)
components(FIFA_Nether)

# diameter
gd <- geodist(FIFA_Nether)
max(gd$gdist)

# clustering by transitivity
gtrans(FIFA_Nether,mode="graph")
gtrans(FIFA_Nether,mode="digraph")

# degree centrality

FIFA_Nether %v% 'vertex.names' #it gives me the names of nodes
degree(FIFA_Nether) # gmode is set to "digraph" by default.

# closeness centrality

FIFA_Nether %v% 'vertex.names' #it gives me the names of nodes
closeness(FIFA_Nether) # gmode is set to "digraph" by default.

# betweenness centrality

FIFA_Nether %v% 'vertex.names' #it gives me the names of nodes
betweenness(FIFA_Nether) # gmode is set to "digraph" by default.
