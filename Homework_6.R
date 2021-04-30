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
model1 <- ergm(networkasnet ~ edges+ transitiveties,
             control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                  MCMLE.maxit=10),verbose=TRUE)

model2 <- ergm(networkasnet ~ edges+ mutual,
               control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                    MCMLE.maxit=10),verbose=TRUE)

model3 <- ergm(networkasnet~edges+nodecov('user_followers_count')+
                     nodecov('user_followed_count')+
                     nodecov('number_tweets')+
                     nodematch('Profile'), control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                                                MCMLE.maxit=10),verbose=TRUE)

finalmodel <- ergm(networkasnet ~ edges+ transitiveties+ mutual+ nodecov('user_followers_count')+
                           nodecov('user_followed_count')+
                           nodecov('number_tweets')+
                           nodematch('Profile'),
                   control=control.ergm(MCMC.samplesize=500,MCMC.burnin=1000,
                                        MCMLE.maxit=10),verbose=TRUE)
                   
summary(model1)
summary(model2)
summary(model3)
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
