# SNA 
# Federico Ferrero
# Luke: Chapter 11: ERGM

library(UserNetR)
library(igraph)
data(TCnetworks)
TCcnt <- TCnetworks$TCcnt
TCcoll <- TCnetworks$TCcoll

TCdiss <- TCnetworks$TCdiss
TCdist <- TCnetworks$TCdist
summary(TCdiss,print.adj=FALSE)

components(TCdiss)
gden(TCdiss)
centralization(TCdiss,betweenness,mode='graph')

deg <- degree(TCdiss,gmode='graph')
lvl <- TCdiss %v% 'agency_lvl'
plot(TCdiss,usearrows=FALSE,displaylabels=TRUE,
     vertex.cex=log(deg),
     vertex.col=lvl+1,
     label.pos=3,label.cex=.7,
     edge.lwd=0.5,edge.col="grey75")
legend("bottomleft",legend=c("Local","State",
                             "National"),
       col=2:4,pch=19,pt.cex=1.5)
