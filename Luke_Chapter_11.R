# SNA 
# Federico Ferrero
# Luke: Chapter 11: ERGM

library(ggplot2)
library(UserNetR)
library(igraph)
library(statnet)
library(ergm)
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

# setting ERGM (a null model, only the edges term)
library(ergm)
DSmod0 <- ergm(TCdiss~edges,
               control=control.ergm(seed=40))
class(DSmod0)
summary(DSmod0)

plogis(coef(DSmod0))

# Including nodes attributes
scatter.smooth(TCdiss %v% 'tob_yrs',
               degree(TCdiss,gmode='graph'),
               xlab='Years of Tobacco Experience',
               ylab='Degree')

DSmod1 <- ergm(TCdiss ~ edges +
                 nodefactor('lead_agency') +
                 nodecov('tob_yrs') ,
               control=control.ergm(seed=40))
summary(DSmod1)

# Including dyadic predictors
DSmod2a <- ergm(TCdiss ~ edges +
                  nodecov('tob_yrs') +
                  nodematch('agency_lvl'),
                control=control.ergm(seed=40))
summary(DSmod2a)

DSmod2b <- ergm(TCdiss ~ edges +
                  nodecov('tob_yrs') +
                  nodematch('agency_lvl',diff=TRUE),
                control=control.ergm(seed=40))
summary(DSmod2b)

DSmod2c <- ergm(TCdiss ~ edges +
                  nodecov('tob_yrs') +
                  nodemix('agency_lvl',base=1),
                control=control.ergm(seed=40))
summary(DSmod2c)

# Including relational terms as predictors
DSmod3 <- ergm(TCdiss ~ edges +
                 nodecov('tob_yrs') +
                 nodematch('agency_lvl',diff=TRUE) +
                 edgecov(TCdist,attr='distance') +
                 edgecov(TCcnt,attr='contact'),
               control=control.ergm(seed=40))
summary(DSmod3)

# Including local structural predictors (dyad dependency) : GWESP
DSmod4 <- ergm(TCdiss ~ edges +
                 nodecov('tob_yrs') +
                 nodematch('agency_lvl',diff=TRUE) +
                 edgecov(TCdist,attr='distance') +
                 edgecov(TCcnt,attr="contact") +
                 gwesp(0.7, fixed=TRUE),
               control=control.ergm(seed=40))
summary(DSmod4)


# predicting values
prd_prob1 <- plogis(-6.31 + 2*1*.099 + 1.52 +
                            4*1.042 + .858*(.50^4))
prd_prob1
prd_prob2 <- plogis(-6.31 + 2*5*.099 +
                            1*1.042 + .858*(.50^4))
prd_prob2

# fit of model
DSmod.fit <- gof(DSmod4,
                 GOF = ~distance + espartners +
                         degree + triadcensus,
                 burnin=1e+5, interval = 1e+5)
summary(DSmod.fit)

op <- par(mfrow=c(2,2))
plot(DSmod.fit,cex.axis=1.6,cex.label=1.6)
par(op)

mcmc.diagnostics(DSmod4)

#simulating networks based on fit model
sim4 <- simulate(DSmod4, nsim=1, seed=569)
summary(sim4,print.adj=FALSE)

op <- par(mfrow=c(1,2),mar=c(0,0,2,0))
lvlobs <- TCdiss %v% 'agency_lvl'
plot(TCdiss,usearrows=FALSE,
     vertex.col=lvl+1,
     edge.lwd=0.5,edge.col="grey75",
     main="Observed TC network")
lvl4 <- sim4 %v% 'agency_lvl'
plot(sim4,usearrows=FALSE,
     vertex.col=lvl4+1,
     edge.lwd=0.5,edge.col="grey75",
     main="Simulated network - Model 4")
par(op)
