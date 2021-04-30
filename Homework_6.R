# FINAL PROJECT
# Social Network Analysis

# clear your memory
rm(list=ls())


# importing data into R
library(statnet)
library(UserNetR)
library(igraph)
library(sna)


edges<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/final_edges_2.csv", header=T, as.is=T)
nodes<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/final_nodes_2.csv", header=T, as.is=T)


networkasigraph <- graph_from_data_frame(d=edges, vertices=nodes, directed = TRUE)
networkasigraph
class(networkasigraph)# verify the class of object: it's an object


# from igraph to statnet and ask for summary
library(intergraph)
networkasnet <- asNetwork(networkasigraph)
class(networkasnet)# verify the class of object: it's a network object
summary(networkasnet)

# size of network
network.size(networkasnet)

# degree centrality
mean(degree(networkasigraph, mode = "in")) # all, in, out

mean(degree(networkasigraph, mode = "out")) # all, in, out

summary(degree(networkasigraph, mode = "all")) # all, in, out
mean(nodes$number_tweets)

# betweenness
betweenness(networkasigraph)
summary(betweenness(networkasigraph))

#degree distribution
degreedist(networkasnet, gmode="digraph")

# plotting the network
# removing the loops
networkasigraph <- simplify(networkasigraph, remove.multiple = F, remove.loops = T) 
summary(networkasigraph)

# defining labels according degrees
node_label <- V(networkasigraph)$node_label <- unname(ifelse(degree(networkasigraph)[V(networkasigraph)] > 7, 
                                                             names(V(networkasigraph)), "")) 

# defining color edges depending on type of link (reply = blue / retweet = hotpink)
edge_color <-E(networkasigraph)$color <- ifelse(E(networkasigraph)$type_link=="reply",'blue','hotpink')

# changing node size using centrality measures (in)
in_degree <- degree(networkasigraph, mode="in") # all, in, out
in_degree
V(networkasigraph)$size <- in_degree*1.41

# giving colors to nodes according main profiles
V(networkasigraph)[V(networkasigraph)$Profile == "Politician"]$color <- "brown2"
V(networkasigraph)[V(networkasigraph)$Profile == "Professor/Researcher"]$color <- "cadetblue"
V(networkasigraph)[V(networkasigraph)$Profile == "Media organization"]$color <- "goldenrod2"
V(networkasigraph)[V(networkasigraph)$Profile == "Union Leader"]$color <- "lightpink"
V(networkasigraph)[V(networkasigraph)$Profile == "Other"]$color <- "yellowgreen"

# plot
plot(networkasigraph, layout=layout_nicely,
     edge.arrow.size=.19,
     vertex.label.color="black",
     vertex.label = node_label,
     vertex.label = node_label,
     color.edge= edge_color,
     main="First retweets, replies, and mentions in #AlevelResults discussion")

# adding legendas
legend(x=-1.3, y=-1.1,legend=c("Politician","Professor/Researcher", "Media Organization", 
                               "Union Leader", "Other Citizen"), pch=21,
       col= c("brown2","cadetblue","goldenrod2","lightpink","yellowgreen"), 
       pt.bg= c("brown2","cadetblue","goldenrod2","lightpink","yellowgreen"),text.font= 8, 
       pt.cex=2, cex=.9, bty="n", ncol=1)

legend(x=0.4, y=-1.2, legend=c("Reply/Mentions", "Retweet"),
       col=c("blue","hotpink"), lty=1, cex=0.9, box.lty=0, text.font= 8)

# ERGM model
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
