### Analysis class ###

## Install packages ## 
install.packages("devtools")
install.packages("usethis")
install.packages("processx")
library(usethis)
library(processx)
library(devtools)
install_github("jason-morgan/ina")


## Load all packages and data ##
library(network)
library(ina)

data("Krackhardt")
force(Krackhardt)
  # you can see the following vertex attributes: 
      # Age, department, level, tenure, vertex names
?Krackhardt

## Make into a network object ##
Net <- Krackhardt
class(Net)

## Create temporary object to house the vertex attributes ##
tenure <- get.vertex.attribute(Net, "Tenure")
tenure

age <- get.vertex.attribute(Net, "Age")
age

department <- get.vertex.attribute(Net, "Department")
department

level <- get.vertex.attribute(Net, "Level")
level

names <- get.vertex.attribute(Net, "vertex.names")
names

## Simple Plots ##
library(statnet)
class(Net)

# With igraph
  # library(igraph)
  # network <- graph.adjacency(Net, directed=TRUE)

# If you needed to turn it into a network object.... 
network1 <- network(Net, matrix.type="adjacency")
class(network1)

# Plot 
gplot(Net)

plot(Net, usearrows=FALSE, vertex.cex=1.5, label=department, displaylabel=T, vertex.col="Department")

## Descriptive Information ##
gden(Net)

#... etc.

## ERGM Specification 
  # See slides

library(ergm)
?ergm

# Null model 
null <- ergm(Net ~ edges, control=control.ergm(seed=40))
class(null)
summary(null)
  # egdes = intercept term for the model, ensures that simulated networks have the same # of edges as the observed network

# Build the model w/ endogenous covariates
set.seed(510)
  # Set a random seed so we can produce exactly the same results

# Reciprocity =  mutual
mod1 <- ergm(Net ~ edges + mutual, 
             control=control.ergm(
               MCMC.samplesize=5000, 
               MCMC.burnin=10000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod1)

  # Control conditions are based off of an MCMC estimation procedure; don't have to specify any/all
  # larger values of each will, in general, lead to more accurate estimates
  # but, with larger values, model will take longer to run
  # decrease values for faster run times
  # increase for published results

# some nodes more active in advice seeking = star statistics
mod2 <- ergm(Net ~ edges + ostar(2:3), 
             control=control.ergm(
               MCMC.samplesize=5000, 
               MCMC.burnin=10000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod2)

# clustering = transitivity
mod3 <- ergm(Net ~ edges + transitiveties, 
             control=control.ergm(
               MCMC.samplesize=5000, 
               MCMC.burnin=10000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod3)

# All endogenous together 
mod4 <- ergm(Net ~ edges + mutual + ostar(2:3) + transitiveties, 
             control=control.ergm(
               MCMC.samplesize=5000, 
               MCMC.burnin=10000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod4)

##  Exogenous covariates

# Reports to
mod5 <- ergm(Net ~ edges + mutual + ostar(2:3) + transitiveties
             + edgecov("reportsto"), 
             control=control.ergm(
               MCMC.samplesize=5000, 
               MCMC.burnin=10000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod5)

# Nodecov = quantitative node attribute
  # nodeicov = tendency to receive incoming ties (directed networks only)
  # nodeocov = tendency to send outcoming ties (directed only)
  # absdiff = absolute difference 
# Nodefactor = categorical attribute

# Add tenure
mod6 <- ergm(Net ~ edges + mutual + ostar(2:3) + transitiveties
             + edgecov("reportsto")
             + nodecov("Tenure"), 
             control=control.ergm(
               MCMC.samplesize=1000, 
               MCMC.burnin=5000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod6)
  # Reduced MCMC sizes, to save time

# Tenure, icov and ocov
mod7 <- ergm(Net ~ edges + mutual + ostar(2:3) + transitiveties
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure"), 
             control=control.ergm(
               MCMC.samplesize=1000, 
               MCMC.burnin=5000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod7)
# Reduced MCMC sizes, to save time

# Add age
mod8 <- ergm(Net ~ edges + mutual + ostar(2:3) + transitiveties
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure")
             + nodeicov("Age") + nodeocov("Age"), 
             control=control.ergm(
               MCMC.samplesize=1000, 
               MCMC.burnin=5000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(mod8)


## Analyzing the model fit
gof1 <- gof(mod8)
  # execute goodness of fit function and creates a new object, gof1

par(mfrow=c(2,2))
plot(gof1)
  # produces some gof plots from the object
gof1
  # small pvalues indicate problems
  # b/c they indicate that there is a statistically significant difference
    # between observed value of the statistic and the simulated values
    # whereas good fit would have it that these values should be similar.









