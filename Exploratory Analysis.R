library(tidyverse)

library(stats4)

library(kdensity)

set.seed(16)
x <- 1:25
y <- rnorm(25, mean = 6.3, sd = 2.2)

ysamp <- tibble(val = y)

ggplot(data = ysamp) +
  geom_point(aes(val, 0)) +
  geom_density(aes(val)) +
  xlim(-2,15) +
  geom_function(fun = dnorm, args = list(mean = 6.3, sd = 2.2))



stats4::mle(x)

densy <- density(y)

plot(density(y))

densy(6.2)

ykde = kdensity(y)
plot(ykde,  -2,15)

ykderealgap <- function(x) {dnorm(x, mean = 6.3, sd = 2.2) - ykde(x)}

ykderealgapsquared <- function(x){ykderealgap(x)*ykderealgap(x)}

## This shoud equal zero because areas should both equal 1
integrate(ykderealgap  , lower = -100, upper = 100)

## This should not equal zero- should be more interesting
integrate(ykderealgapsquared  , lower = -100, upper = 100)


## Let's do the same calculation using a completely inapproptiate pdf
ggplot(data = ysamp) +
  geom_point(aes(val, 0)) +
  geom_density(aes(val)) +
  xlim(-2,15) +
  geom_function(fun = dgamma, args = list(shape = 6.3, rate = 2.2))

ykdegammagap <- function(x) {dgamma(x, shape = 6.3, rate = 2.2) - ykde(x)}
ykdegammagapsquared <- function(x){ykdegammagap(x)*ykdegammagap(x)}

## This shoud equal zero because areas should both equal 1
integrate(ykdegammagap  , lower = -100, upper = 100)

## This should not equal zero- should be more interesting
integrate(ykdegammagapsquared  , lower = -100, upper = 100)

## Suppose I want to find the best possible gamma curve
## to the kde on the sample

## It's going to be a bit gung-ho



























## Let's now use the whiting data that we have previously considered to be gamma-esque
## We first reproduce what we've already done
## Import data
## Get rid of duds
## Sketch the KDE


load("data/stomach_dataset.Rdata")
library(dplyr)
library(tidyverse)
library(bbmle)
library(mizer)

## We here only have species, prey & predator weight and the number of prey
df <- stom_df |>
  transmute(Species = pred_species,
            wprey = prey_weight_g,
            wpredator = pred_weight_g,
            Nprey = prey_count / n_stomachs)

ogDF <- df


## Remove any observation with wprey = 0
df = df %>% 
  filter(df$wprey != 0)


## Add ppmr and log ppmr columns for each observation

df = df %>% 
  mutate(ppmr = wpredator/wprey, l = log(wpredator/wprey))


whitingDF <- filter(df, df$Species == "Merlangius merlangus")

ggplot(whitingDF, aes(x = l)) +
  geom_density()

gammdf <- tibble(
  l = seq(0, 15, length.out=200),
  d = dgamma(l, shape = 5, scale=0.8)
)
ggplot(whitingDF, aes(x = l), adjust = 1.5) +
  geom_density() +
  geom_line(data = gammdf, aes(l, d))

whitingKDE <- kdensity(whitingDF$l)
plot(whitingKDE)

gammaWhitingGap <- function(x , siap, maint)
{dgamma(x, shape = siap, scale = maint) - whitingKDE(x)}

errDF <- tibble(
  poin = seq(0, 15, length.out=200),
  dif = gammaWhitingGap(poin, 5,0.8)
)


ggplot(whitingDF, aes(x = l), adjust = 1.5) +
  geom_density() +
  geom_line(data = gammdf, aes(l, d), col = "blue") + 
  geom_line(data = errDF, aes(poin, dif), col = "red")


## Integrate the square of the gap
## Define the square
gammaWhitingGapSquared <- function (x, siap, maint) {gammaWhitingGap(x, siap, maint)^2}

gammaErrorSq <- function (siap, maint) {integrate }

