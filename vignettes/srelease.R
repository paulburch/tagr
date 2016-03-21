## ------------------------------------------------------------------------
library(tagr)
attach(carptags)
head(carptags)

## ------------------------------------------------------------------------
with(carptags, table(season, release))

## ------------------------------------------------------------------------
(rel1 <- nrow(carptags[carptags$release==1,]))
(rel2 <- nrow(carptags[carptags$release==2,]))


## ------------------------------------------------------------------------
(recaps1 <- with(carptags, table(season, release))[,1])
(recaps2 <- with(carptags, table(season, release))[,2])


## ------------------------------------------------------------------------
(rate1 <- (recaps1[1]/rel1))

## ------------------------------------------------------------------------
(rate2 <- (recaps2[1]/rel2))

## ------------------------------------------------------------------------
(rel1 - (rel1 * (recaps1[1] / (rel1 * (rate2))))) / (rel1+rel2)


## ------------------------------------------------------------------------
ls <- with(carptags, table(season, tags))
ls_prop <- ls[,1]/ls[,2]
cbind(ls, ls_prop)


## ------------------------------------------------------------------------
catches <- c(3181, 4377, 2417, 1253)


## ------------------------------------------------------------------------
tagged_catch <- with(carptags, (table(season)))


## ------------------------------------------------------------------------
prop <- tagged_catch/catches


## ------------------------------------------------------------------------
1 - (prop[3] / prop[2])


## ------------------------------------------------------------------------
1 - (prop[4] / prop[3])


## ------------------------------------------------------------------------
attach(carphauls)
head(carphauls)

## ------------------------------------------------------------------------
(prior_recaps <- recaps1[1:3] + recaps2[1:3])


## ------------------------------------------------------------------------
(recaps <- recaps1[4] + recaps2[4])
recaps

## ------------------------------------------------------------------------
sum(carphauls$tagged)

## ------------------------------------------------------------------------
set.seed(15)

## ------------------------------------------------------------------------
est_1 <- single_release(tags = (rel1 + rel2),
                        catch = carphauls$catch,
                        recaps = carphauls$tagged,
                        prior_recaps = prior_recaps,
                        method = "Chapman",
                        unit = "numbers",
                        type = 2,
                        tag_mort = 0.2,
                        nat_mort = 0.04,
                        chronic_mort = 0.17)

summary(est_1)


## ------------------------------------------------------------------------
ci_1 <- bootstrap(est_1, nboot=1000)
summary(ci_1)

## ------------------------------------------------------------------------
est_2 <- single_release(tags = (rel1 + rel2),
                        catch = carphauls$catch,
                        recaps = carphauls$tagged,
                        prior_recaps = prior_recaps,
                        method = "Chapman",
                        unit = "numbers",
                        type = 2,
                        tag_mort = 0.25,
                        nat_mort = 0.04,
                        chronic_mort = 0.17)

summary(est_2)


## ------------------------------------------------------------------------
ci_2 <- bootstrap(est_2, nboot=1000)
summary(ci_2)

## ------------------------------------------------------------------------
est_3 <- single_release(tags = (rel1 + rel2),
                        catch = carphauls$catch,
                        recaps = carphauls$tagged,
                        prior_recaps = prior_recaps,
                        method = "Chapman",
                        unit = "numbers",
                        type = 2,
                        tag_mort = 0.3,
                        nat_mort = 0.04,
                        chronic_mort = 0.17)

summary(est_3)


## ------------------------------------------------------------------------
ci_3 <- bootstrap(est_3, nboot=1000)
summary(ci_3)

## ------------------------------------------------------------------------
sessionInfo()

