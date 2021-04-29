# FINAL PROJECT
# Social Network Analysis

# clear your memory
rm(list=ls())

# importing edgelist data into R
library(statnet)
library(UserNetR)
library(igraph)


edges<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/final_edges_2.csv", header=T, as.is=T)
nodes<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/final_nodes_2.csv", header=T, as.is=T)


networkasigraph <- graph_from_data_frame(d=edges, vertices=nodes, directed = TRUE)
networkasigraph
class(networkasigraph)

summary(networkasigraph)

# from igraph to statnet
library(intergraph)
class(networkasigraph)

networkasnet <- asNetwork(networkasigraph)

class(networkasnet)
summary(networkasnet)

# size of network
network.size(networkasnet)

# degree centrality
network.vertex.names(networkasnet) # it gives me the names of nodes
degree <-degree(networkasnet, gmode="digraph")
mean(degree)

# in degree centrality
network.vertex.names(networkasnet) # it gives me the names of nodes
in_degree <-degree(networkasnet, gmode="digraph", cmode="indegree")

in_degree
mean(in_degree)

# out degree centrality
mean(nodes$number_tweets)

# betweenness
betweenness <-betweenness(networkasnet, gmode="digraph")
betweenness

#degree distribution graphically
deg_dis <- as.data.frame(table(degree(networkasnet, gmode="digraph")))
colnames(deg_dis) <- c('Degree','Frequency')
plot(deg_dis, main="Degree Distribution #AlevelResults Network")

# plot
plot(networkasigraph, layout= layout_with_graphopt, main="Main relations #AlevelResults discussion")

# defining labels according degrees
node_label <-V(networkasigraph)$node_label <- unname(ifelse(degree(networkasigraph)[V(networkasigraph)] > 5, names(V(networkasigraph)), "")) 

# defining color edges depending on type of link
edge_color <-E(networkasigraph)$color <- ifelse(E(networkasigraph)$type_link=="reply",'blue','hotpink')


# Changing node size using centrality measures
degree <- degree(networkasigraph, mode="in") # all, in, out
degree
V(networkasigraph)$size <- degree*3
plot(networkasigraph, layout=layout_nicely,
     edge.arrow.size=.2, 
     vertex.label = node_label,
     vertex.label = node_label,
     color.edge= edge_color,
     main="Main relations #AlevelResults discussion")


# Changing the color of the edges depending on the type of tie
E(networkasigraph)$color <- ifelse(E(networkasigraph)$type=="reply",'blue','hotpink')
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.2, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.8, color.edge=E(network2)$color, main="Making an Optimal Visualization")
?layout
?plot
gden(networkasnet)

?gden
# indegree
?degree
degree(network)
closeness(networkasigraph)
betweenness(network)


degree(network, mode = "in")

in_deg <- network %v% "in_deg" <- degree(network, mode = "in")
flomarriage # verify if it was added
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

# ergm
library(ergm)

# load Florentine marriage data matrix 
data(flo)
flo

# create a network object out of the adjacency matrix 
flomarriage <- network(flo, directed=FALSE)
flomarriage[,] 
class(flomarriage)

# create a vector indicating the wealth of each family (in thousands of lira) 
# and add it to the network object as a covariate
in_degree <- networkasnet %v% "in_degree" <-c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3)
wealth
flomarriage

# load a network object of the Florentine data 
data(florentine)
?florentine
flomarriage

#degree of nodes
network.vertex.names(flomarriage) # it gives me the names of nodes
degree(flomarriage)

#degree distribution graphically
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

dev.off()

######################### ERGM: a null model
library(ergm)
null <- ergm(networkasnet~edges)
class(null)
summary(null)

# the null model is constrained by the number of edges in the observed network
# The probability of an edge being drawn should in theory be the same as density - let's check.
plogis(coef(null))
gden(networkasnet)

# add node attribute: wealth
mod1 <- ergm(networkasnet~edges+nodecov('user_followers_count')+
               nodecov('user_followed_count')+
               nodecov('number_tweets')+
               nodematch('Profile'))
summary(mod1)


# calculate probability of observing tie between families of different incomes
p_edg <- coef(mod1) [1]
p_edg
p_wealth <- coef(mod1) [2]
p_wealth
plogis(p_edg + 146*p_wealth + 3*p_wealth) #prob of link between 2 diff families in term of wealth

plogis(p_edg + 146*p_wealth + 103*p_wealth) #prob of link between 2 similar families in term of wealth


# calculate betweenness centrality of each node
network.vertex.names(flomarriage) # it gives me the names of nodes
betweenness(flomarriage, gmode="graph") # gmode is set to "digraph" by default."digraph" indicates edges 
#are directed

# add betweenness centrality class as a dyad attribute to the model
?degree
in_degree <- networkasnet %v% "in_degree" <-degree(networkasnet)
flomarriage # verify if it was added

mod2 <- ergm(flomarriage ~ edges+nodecov('wealth')+absdiff("betw"))
summary(mod2)

# add two measures of local structure to the model
mod3 <- ergm(flomarriage~edges+nodecov('wealth')++absdiff("betw")+gwesp(0.5, fixed=TRUE)+ triangle)
summary(mod3)

library('stargazer')
stargazer(mod3, title = "Testing ERGM model", out="table1.txt")

# Analyzing the model fit
gof <- gof(mod1)
# execute goodness of fit function and creates a new object, gof

# produces some gof plots from the object
par(mfrow=c(2,2))
plot(gof)

# see gof meassures
gof

# small p-values indicate problems
# b/c they indicate that there is a statistically significant difference
# between observed value of the statistic and the simulated values
# whereas good fit would have it that these values should be similar.
