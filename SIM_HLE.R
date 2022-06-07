# install.packages("markovchain")
library(markovchain)
library(tidyverse)
library(here)
source("Functions.R")

TR <- read_csv(here("Data","TRv6.csv") )

# define a subset
TRsub <- TR %>% filter(sex == "f",
                       edu == "terciary",
                       time == 1996)

# starting proportions in each state
init <- TRsub[1, c("s1_prop","s2_prop")] %>% unlist()
names(init) <- c("H", "U")

# Make the submatrices of U, the transient matrix
HH <- pi2u(pivec = TRsub$m11, from = "H", to = "H")
HU <- pi2u(pivec = TRsub$m12, from = "H", to = "U")
UH <- pi2u(pivec = TRsub$m21, from = "U", to = "H")
UU <- pi2u(pivec = TRsub$m22, from = "U", to = "U")


# terciary edu females
U <- u2U(HH = HH, # healthy to healthy
         HU = HU, # healthy to unhealthy
         UH = UH, # unhealthy to healthy
         UU = UU) # unhealthy to unhealthy

  
# complete the Markov matrix with Dead state,
# this turns it into a stochastic matrix (colSums = 1)
U <- cbind(U, 0)
U <- rbind(U, 1 - colSums(U)) # death probabilities are 1-sum(transient_probabilities) ...

# Need to name the dims, concatenating state and age
age_state   <- c(outer(seq(48, 110, by = 2), paste0("::", c("H","U")), paste0), "Dead")
(dimnames(U) <- list(to = age_state, from = age_state))

# ---------------
# switch from Leslie-Caswell to standard Markov orientation!!!!
# Gotta remember this!!!
U <- t(U)
# ---------------
  
# make s4 transition matrix from markovchain package
  # RTFM
mcHLE <- new("markovchain", 
            states = rownames(U),
            byrow = TRUE, 
            transitionMatrix = U,
            name = "HLE")
  
# how many sequences should we generate?
  
# n = time steps
# object = markovchain object
# t0 = starting state + age
# replicate it N times.


# If you want to account for an initial mixture, then
# Do this twice, and vary the size of N accordingly...

# init, using prevalence
N  <- 2e4
Ns <- round(init * N)
set.seed(2021)

RHRS_H  <- replicate(Ns["H"],
                     rmarkovchain(n = 31, 
                                  object = mcHLE, 
                                  t0 = "48::H", 
                                  parallel = TRUE)
) 
RHRS_U  <- replicate(Ns["U"],
                     rmarkovchain(n = 31, 
                                  object = mcHLE, 
                                  t0 = "48::U", 
                                  parallel = TRUE)
) 

# stick together in one population,
# note, t0 from the random generator is not included
# in the trajectories, so we need to append it to be 
# consistent with the other approaches.
RHRS                   <- cbind(rbind("H",RHRS_H), rbind("U",RHRS_U))

# only need the states, don't need the age part of the labels
RHRS_clean             <- gsub(".*:","",RHRS)
  dim(RHRS_clean)
  RHRS_clean[,1]
a                    <- seq(48, 110, by = 2)
rownames(RHRS_clean) <- a
colnames(RHRS_clean) <- 1:ncol(RHRS_clean)
 
# see the first one:
(RHRS_clean[,1] == "H") %>% sum() %>% '*'(2)

# Calculate HLE and ULE (compare w values in Sullivan.R, increase N to get closer)
(HLE <- sum(RHRS_clean == "H") / N  * 2)
(ULE <- sum(RHRS_clean == "U") / N  * 2)

# But this approach gives lots of goodies for free

# This is an equivalent way to get HLE
mean(colSums(RHRS_clean == "H")*2)

# but also distributional statistic you want:
# SD of HLE
sd(colSums(RHRS_clean == "H")*2)
# Quantiles of HLE (note the 2-year age bins :-())
quantile(colSums(RHRS_clean == "H")*2, c(.25,.5,.75))

# or the unhealthy part
sd(colSums(RHRS_clean == "U")*2)
quantile(colSums(RHRS_clean == "U")*2, c(.25,.5,.75))

# Note, distribution measures are measuring
# between-individual variability, NOT the variance
# of the estimate. For that, you'd want to repeat 
# the simulation many times (1000?) and take quantiles
# of the HLE estimates.

(colSums(RHRS_clean == "U")*2) %>% 
  hist(main = "Unhealthy life distribution")
(colSums(RHRS_clean == "H")*2) %>% 
  hist(main="Healthy life distribution")

# But really, I mean, really, simulated life courses are sooooo rich.
# you can look at spell distributions, etc, etc. easy peasy, you just
# need to learn to deal with trajectory data a bit.


