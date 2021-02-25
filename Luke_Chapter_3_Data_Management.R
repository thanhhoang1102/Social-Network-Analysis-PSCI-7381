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
detach (package:statnet)#because it has some same names as those found in igraph, so, to avoid confusion
library(igraph)

# creating network with adjacency using igraph
inet1 <- graph.adjacency(netmat1)
class(inet1)
summary(inet1)
str(inet1)

# creating network with edgelist using igraph
inet2 <- graph.edgelist(netmat2)
summary(inet2)

# set attributes to nodes and ties using igraph

V(inet2)$name <- c("A","B","C","D","E")
E(inet2)$val <- c(1:6)
summary(inet2)
str(inet2)

# going back and forth between statnet and igraph
install.packages('intergraph')
library(intergraph)
class(net1)

net1igraph <- asIgraph(net1) #convierto net1, que es de clase "network" (statnet) en igraph
class(net1igraph)#verify

summary(net1igraph)

# tranformation  in the opposite direction, we need to use "asNetwok"

net1network <- asNetwork(net1igraph) #convierto net1, que es de clase "igraph" en "network" (statnet)
class(net1network)
summary(net1network)

str(net1igraph)

#### IMPORTING NETWORK DATA

detach("package:igraph", unload=TRUE)
library(statnet)

netmat3 <- rbind(c("A","B"),
                 c("A","C"),
                 c("B","C"),
                 c("B","D"),
                 c("C","B"),
                 c("E","C"))
net.df <- data.frame(netmat3)
net.df

write.csv(net.df, file = "MyData.csv",
          row.names = FALSE)
net.edge <- read.csv(file="MyData.csv")
net_import <- network(net.edge,
                      matrix.type="edgelist")
summary(net_import)

gden(net_import) # density

## Filtering based on node values: categories (filtering only female nodes)
n1F <- get.inducedSubgraph(net1,
                           which(net1 %v% "gender" == "F"))
n1F[,]
gplot(n1F,displaylabels=TRUE)

## Filtering based on node values: numerical (filtering only nodes with degree greater of equals to 2)
deg <- net1 %v% "alldeg"
n2 <- net1 %s% which(deg > 1)
gplot(n2,displaylabels=TRUE)

##removing isolates (bides with degree of 0)
library(UserNetR)
data(ICTS_G10)
gden(ICTS_G10)
length(isolates(ICTS_G10)) # gives me 96 isolated nodes

n3 <- ICTS_G10 # make a copy of the network due to security
delete.vertices(n3,isolates(n3))
gden(n3) # density
length(isolates(n3)) #now there is not isolates nodes

## Filtering based on edge values
data(DHHS) #54 tobacco control experts working together / edge width based on value of
#"colab" attruibute (pag 35)
d <- DHHS
gden(d)
op <- par(mar = rep(0, 4))
gplot(d,gmode="graph",edge.lwd=d %e% 'collab',
      edge.col="grey50",vertex.col="lightblue",
      vertex.cex=1.0,vertex.sides=20)
par(op)

as.sociomatrix(d)[1:6,1:6] # see ties

list.edge.attributes(d) # see ties attributes

as.sociomatrix(d,attrname="collab")[1:6,1:6] # see ties attributes in sociomatrix

table(d %e%"collab") # see distribution of tie values

d.val <- as.sociomatrix(d,attrname="collab")
d.val[d.val < 3] <- 0
d.filt <- as.network(d.val, directed=FALSE,
                     matrix.type="a",ignore.eval=FALSE,
                     names.eval="collab")
summary(d.filt,print.adj=FALSE)

# now plot the filtered network
op <- par(mar = rep(0, 4))
gplot(d.filt,gmode="graph",displaylabels=TRUE,
      vertex.col="lightblue",vertex.cex=1.3,
      label.cex=0.4,label.pos=5,
      displayisolates=FALSE)
par(op)

# improved version of the plot
op <- par(mar = rep(0, 4))
d.val <- as.sociomatrix(d,attrname="collab")
gplot(d.val,gmode="graph",thresh=2,
      vertex.col="lightblue",vertex.cex=1.3,
      label.cex=0.4,label.pos=5,
      displayisolates=FALSE)
par(op)

# transforming a directed network to a non-directed one

net1mat <- symmetrize(net1,rule="weak")
net1mat

net1symm <- network(net1mat,matrix.type="adjacency")
network.vertex.names(net1symm) <- c("A","B","C","D","E")
summary(net1symm)
