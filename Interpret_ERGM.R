### Interpretation ###

## Load packages ## 

library(usethis)
library(processx)
library(devtools)
library(network)
library(ina)
library(ergm)


# Other packages
  # install for interpret function
remotes::install_github("leifeld/btergm")
library(btergm)


## Load data ##
data("Krackhardt")

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

## ERGM Model ##

set.seed(510)
# Set a random seed so we can produce exactly the same results


# Final model 
modfin <- ergm(Net ~ edges + mutual + ostar(2:3) + transitiveties
             + edgecov("reportsto")
             + nodeicov("Tenure") + nodeocov("Tenure")
             + nodeicov("Age") + nodeocov("Age"), 
             control=control.ergm(
               MCMC.samplesize=1000, 
               MCMC.burnin=5000, 
               MCMLE.maxit=10), 
             verbose=TRUE)
summary(modfin)

## Network level interpretation = conditional log odds

# out-two-star
exp(0.266257) 
  # 1.305
  # Holding all other effects constant, the relative likelihood of observing a network
  # that includes one more out-two-star is 1.31
  # Let's try with a simplier model...

# simple model 
  # edges captures the number of edges in the model 
  # a homophily term that captures the effect of two actors in the netwok being at the same level in the company
  # nodematch("Level") = counts the number of cases in which any two nodes sharing an edge have that same qualitative attibute
mod0 <- ergm(Net ~ edges + nodematch("Level"), 
                control=control.ergm(
                  MCMC.samplesize=1000, 
                  MCMC.burnin=5000, 
                  MCMLE.maxit=10), 
                verbose=TRUE)
summary(mod0)
  # log odds of an edge in the advice network: 
      # 0.047 X change in edge count + -0.400 x change in count of same level edges

# log odds of a tie across levels in a company is 0.047 (edges coefficient)
# Take the inverse logistic transformation
  # logit^{-1}(0.047) = I/(1 + exp(-0.047)) = 0.5119 = probability of a tie across levels

library(gtools)
inv.logit(0.047)
  # 0.5117

# what about odds in the same level?
  # compute the log odds of such a tie = 0.04763 - (-0.4004) = 0.44808
  # take the same inverse logistic transformation
      # exp(0.448)/(1 + exp(0.448)) = 1.56/1 + 1.56 = 0.609
inv.logit(0.448)
  # 0.6101

# The probability of forming a within-level edge is 0.098 higher than forming an across-level edge


# Interpret function..
library(xergm)
interpret(mod0, type="dyad", i=1, j=2)
  # these are dyadwise probabilities 
  # The probability that actor 'i' will have a tie to edge 'j' is 0.25, conditional on the states of other relationships in the network
  # if this pair is somehow representative or of particular interest, this can be interesting or illuminating
  # however, it also may be useful to consider these proabilities across all dyads in the network, using the interpret function

?interpret
