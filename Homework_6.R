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

################# ERGM model #########################
library(ergm)
set.seed(510) # set a random seed so we can produce exactly the same results
model1 <- ergm(networkasnet ~ edges+ transitiveties,
             control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                  MCMLE.maxit=10),verbose=TRUE)
summary(model1)
# interpretation model 1
exp(2.6147278) 
# 13.66
# Holding all other effects constant, the relative likelihood of observing 
# clustering in the network is 13.66.

model2 <- ergm(networkasnet ~ edges+ mutual,
               control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                    MCMLE.maxit=10),verbose=TRUE)

summary(model2)

model3 <- ergm(networkasnet~edges+nodematch('Profile'), 
               control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                    MCMLE.maxit=10),verbose=TRUE)

summary(model3)
# edges captures the number of edges in the model 
# a homophily term that captures the effect of two actors in the network 
# having the same Twitter account profile
# nodematch("Profile")= counts the number of cases in which any two nodes sharing an edge 
# have that same qualitative attribute (in this case the profile)

# log odds of an edge in the twitter network: 
# -6.059 X change in edge count + -0.814 x change in count of same profile edges

# log odds of a tie across profiles in the Twitter discussion is -6.059 (edges coefficient)
# Take the inverse logistic transformation
# logit^{-1}(-6.059) = I/(1 + exp(6.059)) = 0.0023 = probability of a tie across profiles
library(gtools)
a <- inv.logit(-6.059)
a
# what about odds in the same level?
# compute the log odds of such a tie = -6.059 - (-0.814) = -5.245
# take the same inverse logistic transformation
# exp(-5.245)/(1 + exp(-5.245)) = 0.0052 = probability of a tie within profiles
b<- inv.logit(-5.245)
b
b-a

# The probability of forming a within-profile edge is 0.0029 higher than forming an across-profile edge

finalmodel <- ergm(networkasnet ~ edges+ transitiveties+ mutual+nodematch('Profile'),
                   control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                        MCMLE.maxit=10),verbose=TRUE)
summary(finalmodel)
library('stargazer')
stargazer(finalmodel, title = "Testing ERGM model", out="table1.txt")

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

# other ergm
model3 <- ergm(networkasnet~edges+nodecov('user_followers_count')+
                       nodecov('user_followed_count')+
                       nodecov('number_tweets')+
                       nodematch('Profile'), control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                                                  MCMLE.maxit=10),verbose=TRUE)


##################### working with bigger network


edges<- read.csv("C:/Users/feder/Desktop/edges_retweet.csv", header=T, as.is=T)
nodes<- read.csv("C:/Users/feder/Desktop/nodes_retweets.csv", header=T, as.is=T)


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

# betweenness
betweenness(networkasigraph)
summary(betweenness(networkasigraph))

# add betweenness centrality class as a dyad attribute to the model
betw <- networkasnet %v% "betw" <-betweenness(networkasigraph)
networkasnet # verify if it was added

network_filtered <- get.inducedSubgraph(networkasnet,
                           which(betw > 26.89))#the betweenness media

library(ggplot2)

# defining labels according degrees
node_label <- V(network_filtered)$node_label <- unname(ifelse(betweenness(network_filtered)[V(network_filtered)] > 150000, 
                                                             names(V(network_filtered)), "")) 

# changing node size using betweenness
betw
V(network_filteres)$size <- betw*1.41
gplot(network_filtered,displaylabels=FALSE)

# filtering by betweenness
network_filtered <- networkasnet %s% which(betw > 26.89)# the media

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


library(ergm)
model1 <- ergm(networkasnet ~ edges+ transitiveties)

model2 <- ergm(networkasnet ~ edges+ mutual,
               control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                    MCMLE.maxit=10),verbose=TRUE)
