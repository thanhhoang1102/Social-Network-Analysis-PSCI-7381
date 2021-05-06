# FINAL PROJECT
# Social Network Analysis

# clear your memory
rm(list=ls())

# set your working directory path
setwd("C:/Users/feder/Desktop")

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
summary(degree(networkasigraph, mode = "in")) 

summary(degree(networkasigraph, mode = "out")) 

summary(degree(networkasigraph, mode = "all"))

mean(nodes$number_tweets)

# betweenness
betweenness(networkasigraph)
summary(betweenness(networkasigraph))
table(betweenness(networkasigraph))

# eigenvector
eigen(networkasigraph)
summary(betweenness(networkasigraph))
table(betweenness(networkasigraph)))
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

# ploting egonet of the participant with higher betweeness centrality

edges_ego<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/edges_ego_net.csv", header=T, as.is=T)
nodes_ego<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/nodes_ego_net.csv", header=T, as.is=T)


networkasigraph2 <- graph_from_data_frame(d=edges_ego, vertices=nodes_ego, directed = TRUE)
networkasigraph2
class(networkasigraph2)# verify the class of object: it's an object

# from igraph to statnet and ask for summary
library(intergraph)
networkasnet2 <- asNetwork(networkasigraph2)
class(networkasnet2)# verify the class of object: it's a network object
summary(networkasnet2)

# plotting the egonet of the participant with higest betweeness centrality
# defining color edges depending on type of link (reply = blue / retweet = hotpink)
edge_color <-E(networkasigraph2)$color <- ifelse(E(networkasigraph2)$type_link=="reply",'blue','hotpink')

# changing node size using centrality measures (in)
in_degree <- degree(networkasigraph2, mode="in") # all, in, out
in_degree
V(networkasigraph2)$size <- in_degree*5.1


# giving colors to nodes according main profiles
V(networkasigraph2)[V(networkasigraph2)$Profile == "Politician"]$color <- "brown2"
V(networkasigraph2)[V(networkasigraph2)$Profile == "Professor/Researcher"]$color <- "cadetblue"
V(networkasigraph2)[V(networkasigraph2)$Profile == "Media organization"]$color <- "goldenrod2"
V(networkasigraph2)[V(networkasigraph2)$Profile == "Union Leader"]$color <- "lightpink"
V(networkasigraph2)[V(networkasigraph2)$Profile == "Other"]$color <- "yellowgreen"
V(networkasigraph2)[V(networkasigraph2)$Profile == "Writer"]$color <- "yellow"
V(networkasigraph2)[V(networkasigraph2)$Profile == "Activist"]$color <- "magenta"

# plot
plot(networkasigraph2, layout=layout_nicely,
     edge.arrow.size=.10,
     vertex.label.color="black",
     color.edge= edge_color,
     main="Main Broker in #AlevelResults discussion")

# adding legendas
legend(x=-3.3, y=-1.1,legend=c("Politician","Professor/Researcher", "Media Organization", 
                               "Union Leader", "Other Citizen", "Writer", "Activist"), pch=21,
       col= c("brown2","cadetblue","goldenrod2","lightpink","yellowgreen", "yellow", "magenta"), 
       pt.bg= c("brown2","cadetblue","goldenrod2","lightpink","yellowgreen", "yellow", "magenta"),text.font= 8, 
       pt.cex=2, cex=.8, bty="n", ncol=1)

legend(x=0.4, y=-1.2, legend=c("Reply/Mentions", "Retweet"),
       col=c("blue","hotpink"), lty=1, cex=0.8, box.lty=0, text.font= 8)

dev.off()

# ERGM model
library(ergm)
set.seed(510) # set a random seed so we can produce exactly the same results

finalmodel <- ergm(networkasnet~edges+nodecov('user_followers_count')+
                       nodecov('user_followed_count')+
                       nodecov('number_tweets')+
                       nodematch('Profile'), control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                                                  MCMLE.maxit=10),verbose=TRUE)
summary(finalmodel)
# interpretation on "Number of tweets" coefficient
exp(0.2004)
# 1.221891
# As we can see, number of tweets published is a positive significant predictor of linkage in #AlevelResults network.
# Holding all other effects constant, considering specifically the number of tweets 
# posted by each user, the relative likelihood of observing a link in the network is 1.22.

# interpretation of "Profile" coefficient
# edges captures the number of edges in the model 
# a homophily term that captures the effect of two actors in the network 
# having the same Twitter account profile
# nodematch("Profile")= counts the number of cases in which any two nodes sharing an edge 
# have that same qualitative attribute (in this case the profile)

# log odds of a tie across profiles in the Twitter discussion is -6.204 (edges coefficient)
# Take the inverse logistic transformation
# logit^{-1}(-6.204) = I/(1 + exp(6.204)) = 0.0020 = probability of a tie across profiles
library(gtools)
a <- inv.logit(-6.204)
a
# what about odds in the same profile?
# compute the log odds of such a tie = -6.204 - (-1.078) = -5.126
# take the same inverse logistic transformation
# exp(-5.126)/(1 + exp(-5.126)) = 0.0059 = probability of a tie within profiles
b<- inv.logit(-5.126)
b
b-a

# The probability of forming a within-profile edge is 0.0038 higher than forming an across-profile edge

# print in a table finalmodel outputs
library('stargazer')
stargazer(finalmodel, title = "Testing ERGM model", out="table_ergm_final.txt")

# analyzing the model fit
gof <- gof(finalmodel)

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

#
#
#
#
#
#
#
#
#
#
#
#
#
##################### working with bigger network##############3

# clear your memory
rm(list=ls())

# set your working directory path
setwd("C:/Users/feder/Desktop")

# importing data into R
library(statnet)
library(UserNetR)
library(igraph)
library(sna)

edges<- read.csv("C:/Users/feder/Desktop/edges_retweet.csv", header=T, as.is=T)
nodes<- read.csv("C:/Users/feder/Desktop/nodes_retweets.csv", header=T, as.is=T)


networkasigraph <- graph_from_data_frame(d=edges, vertices=nodes, directed = TRUE)
networkasigraph
class(networkasigraph)# verify the class of object: it's an object

# removing the loops
networkasigraph <- simplify(networkasigraph, remove.multiple = F, remove.loops = T) 
summary(networkasigraph)

# from igraph to statnet and ask for summary
library(intergraph)
networkasnet <- asNetwork(networkasigraph)
class(networkasnet)# verify the class of object: it's a network object
summary(networkasnet)

# size of network
network.size(networkasnet)

# betweenness total network
betweenness(networkasigraph)
summary(betweenness(networkasigraph))

# add betweenness centrality 
betw <- networkasnet %v% "betw" <-betweenness(networkasigraph)
networkasnet # verify if it was added
betw

# filtering network
network_filtered <- get.inducedSubgraph(networkasnet,
                                        which(betw > 185000))#the betweenness media


class(network_filtered)

#plotting network
library(ggplot)
gplot(network_filtered, displaylabels=TRUE)


# defining labels according degrees
node_label <- V(network_filtered)$node_label <- unname(ifelse(betw > 150000, 
                                                              names(V(network_filtered)), "")) 






library(ggplot2)


# changing node size using betweenness
betw
V(network_filtered)$size <- betw/1

nodesize<- V(network_filtered)$size <- betw

# filtering by betweenness
network_filtered <- networkasnet %s% which(betw > 26.89)# the media



# defining labels according degrees
node_label <- V(networkasigraph)$node_label <- unname(ifelse(betweenness(networkasigraph)[V(networkasigraph)] > 150000, 
                                                             names(V(networkasigraph)), "")) 

# defining color edges depending on type of link (reply = blue / retweet = hotpink)
edge_color <-E(networkasigraph)$color <- ifelse(E(networkasigraph)$type_link=="reply",'blue','hotpink')

# changing node size using centrality measures (in)

V(network_filtered)$size <- betw
class(network_filtered)
# plot
plot(network_filtered,
     edge.arrow.size=.19,
     vertex.label.color="black",
     vertex.label = node_label,
     main="Title")









# giving colors to nodes according main profiles
V(networkasigraph)[V(networkasigraph)$Profile == "Politician"]$color <- "brown2"
V(networkasigraph)[V(networkasigraph)$Profile == "Professor/Researcher"]$color <- "cadetblue"
V(networkasigraph)[V(networkasigraph)$Profile == "Media organization"]$color <- "goldenrod2"
V(networkasigraph)[V(networkasigraph)$Profile == "Union Leader"]$color <- "lightpink"
V(networkasigraph)[V(networkasigraph)$Profile == "Other"]$color <- "yellowgreen"


color.edge= edge_color,
# adding legendas
legend(x=-1.3, y=-1.1,legend=c("Politician","Professor/Researcher", "Media Organization", 
                               "Union Leader", "Other Citizen"), pch=21,
       col= c("brown2","cadetblue","goldenrod2","lightpink","yellowgreen"), 
       pt.bg= c("brown2","cadetblue","goldenrod2","lightpink","yellowgreen"),text.font= 8, 
       pt.cex=2, cex=.9, bty="n", ncol=1)

legend(x=0.4, y=-1.2, legend=c("Reply/Mentions", "Retweet"),
       col=c("blue","hotpink"), lty=1, cex=0.9, box.lty=0, text.font= 8)


#degree distribution
degreedist(networkasnet, gmode="digraph")

# plotting the network
# removing the loops
networkasigraph <- simplify(networkasigraph, remove.multiple = F, remove.loops = T) 
summary(networkasigraph)

# defining labels according betweenness
node_label <- V(networkasigraph)$node_label <- unname(ifelse(betweenness(networkasigraph)[V(networkasigraph)] > 27, 
                                                             names(V(networkasigraph)), "")) 
# changing node size using betweenness
betw <- betweenness(networkasigraph)
betw
V(networkasigraph)$size <- betw/20

# plot
plot(network_filtered, layout=layout_nicely,
     edge.arrow.size=.19,
     vertex.label.color="black",
     vertex.label = node_label,
     vertex.label = node_label,
     main="TITLE")
