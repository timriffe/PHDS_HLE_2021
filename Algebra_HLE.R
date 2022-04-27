
# helpful for data operations
library(tidyverse)

# some custom functions
source("Functions.R")

# Read in the data (Daniel Schneider gave these to me, haha)
TR <- read_csv("Data/TRv6.csv") 

# take a peek: we have many strata
head(TR) %>% view()

# These are the things we can subset on.
unique(TR$time) 
unique(TR$sex) 
unique(TR$edu) 
unique(TR$age)

# define a subset
TRsub <- TR %>% filter(sex == "f",
              edu == "terciary",
              time == 1996)

# starting proportions in each state
init <- TRsub[1,c("s1_prop","s2_prop")] %>% unlist()
names(init) <- c("H","U")

# Make the submatrices of U, the transient matrix
HH <- pi2u(pivec = TRsub$m11, from = "H", to = "H")
HU <- pi2u(pivec = TRsub$m12, from = "H", to = "U")
UH <- pi2u(pivec = TRsub$m21, from = "U", to = "H")
UU <- pi2u(pivec = TRsub$m22, from = "U", to = "U")

# we need to bind these like this:
# |-------|
# | HH UH |
# | HU UU |
# |-------|

U <- u2U(HH = HH, # healthy to healthy
         HU = HU, # healthy to unhealthy
         UH = UH, # unhealthy to healthy
         UU = UU) # unhealthy to unhealthy

U %>% View()
# so far this is all matrix architecture, now we have 
# the transient matrix, which we can transform to the 
# fundamental matrix

N <- U2N(U,  interval = 2)
# (I - U) ^{-1} = solve(I - U)
# This is where we stop with the algebra and move to 
# tidyverse for book-keeping.

# This thing has what we want: conditional expected time spent in each
# state and age (conditional on survival and starting state!)
# There are matrix tricks to select what we want. But this is where
# we instead use tidyverse to grab what we need:
N50 <-
  N %>% 
  reshape2::melt(varnames = c("to","from"),
       value.name = "time") %>% 
  mutate(to = as.character(to),
         from = as.character(from)) %>% 
  separate(col = "to", 
           sep = "::",
           into = c("to","age2"),
           convert = TRUE) %>% 
  separate(col = "from", 
           sep = "::",
           into = c("from","age1"),
           convert = TRUE) %>% 
  filter(age1 == 50,
         age2 > age1,
         to != "D")
N50

# calculate HLE and ULE from it:
HLE <-
  N50 %>% 
  group_by(from, to) %>% 
  summarize(Ex_cond = sum(time), .groups = "drop") %>% 
  mutate(init = ifelse(from == "H", init[1], init[2]),
         Ex = Ex_cond * init) %>% 
  group_by(to) %>% 
  summarize(Ex = sum(Ex), .groups = "drop")

HLE$Ex
c(28.2758869643443, 5.61608147350015)
# -------------------------------------------
# Some extra code for weighting together
# an average survival curve and prevalence,
# the Sullivan goods
# no need to run this, the results are
# pasted into the top of the Sullivan script.
# ------------------------------------------


lx <- N50 %>% 
  mutate(init = ifelse(from == "H", init["H"], init["U"]),
         time2 = time * init) %>% 
  group_by(age2) %>% 
  summarize(lx = sum(time2)) %>% 
  pull(lx)

pix <-
  N50 %>% 
  mutate(init = ifelse(from == "H", init["H"], init["U"]),
         time2 = time * init) %>% 
  group_by(to, age2) %>% 
  summarize(time2 = sum(time2),
            .groups = "drop") %>% 
  pivot_wider(names_from = to, values_from = time2) %>% 
  mutate(pix = H / (H + U)) %>% 
  pull(pix)

dput(pix)
dput(lx)
