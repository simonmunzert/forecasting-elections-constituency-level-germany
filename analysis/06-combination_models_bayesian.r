# Forecasting Elections at the Constituency Level
# Simon Munzert

## preparations -----------------------------------
# load packages
source("packages.r")


### compute party-district forecast variances -------------------------

# careful: the procedure requires a lot of RAM (>8GB). it can be advisable to drop large objects when no longer needed with rm() and gc()

## load historical and simulation data from projection model
load("prepared_data/data_prep_model_projection.RData")
load("bugs_simulations/proj_bugs_sim_wo_all.RData")

# party-district-election specific deviation matrix
n.sims <- nrow(proj_bugs_sim_dat_all)
y <- wk.results.melt$firstshare
y.mat <- matrix(rep(y, n.sims), ncol = length(y), byrow = TRUE)
mu <-  dplyr::select(proj_bugs_sim_dat_all, starts_with("mu"))
mu.proj.dev <- mu - y.mat
year.2013.proj <- wk.results.melt$year == 2013

bayesEst <- function(x, lvl, digits) {
  med <- round(median(x, na.rm=T), digits)
  prob <- (1 - lvl)/2
  percLo <- round(quantile(x, probs=prob, na.rm=T), digits)
  percHi <- round(quantile(x, probs=c(1-prob), na.rm=T), digits)
  estimate <- c(med, percLo, percHi)
  return(estimate)
}

# median party-district-election specific deviation
mu.proj.dev.est <- apply(mu.proj.dev, 2, bayesEst, .95, 3)
wk.results.melt$mu.proj.dev.est <- mu.proj.dev.est [1,]

# party-district forecast variance, uncorrected model
wk.results.melt$resid.proj.raw <- wk.results.melt$project - wk.results.melt$firstshare
wk.results.melt.by_partyXwkr <- group_by(wk.results.melt, partyXwkr)
forecast.variance.proj.raw <- dplyr::summarise(wk.results.melt.by_partyXwkr, forecast.variance.proj.raw = mean((resid.proj.raw)^2, na.rm = TRUE))

# party-district forecast variance, corrected model
wk.results.melt.by_partyXwkr <- group_by(wk.results.melt, partyXwkr)
forecast.variance.proj <- dplyr::summarise(wk.results.melt.by_partyXwkr, forecast.variance.proj = mean((mu.proj.dev.est)^2, na.rm = TRUE))
wk.results.melt.proj <- merge(wk.results.melt, forecast.variance.proj, by = "partyXwkr")
wk.results.melt.proj <- merge(wk.results.melt.proj, forecast.variance.proj.raw, by = "partyXwkr")
wk.results.melt.proj$partyXyear <- interaction(wk.results.melt.proj$party, wk.results.melt.proj$year)

# clean up workspace to gain RAM
rm(mu)
rm(y.mat)
gc()


## load historical and simulation data from polls model
load("prepared_data/data_prep_model_polls.RData")
load("bugs_simulations/polls_bugs_sim_wo_all.RData")

# party-district-election specific deviation matrix
n.sims <- nrow(polls_bugs_sim_dat_all)
y <- wk.results.melt$firstshare
y.mat <- matrix(rep(y, n.sims), ncol = length(y), byrow = TRUE)
mu <-  dplyr::select(polls_bugs_sim_dat_all, starts_with("mu"))
mu.poll.dev <- mu - y.mat
year.2013.polls <- wk.results.melt$year == 2013

bayesEst <- function(x, lvl, digits) {
  med <- round(median(x, na.rm=T), digits)
  prob <- (1 - lvl)/2
  percLo <- round(quantile(x, probs=prob, na.rm=T), digits)
  percHi <- round(quantile(x, probs=c(1-prob), na.rm=T), digits)
  estimate <- c(med, percLo, percHi)
  return(estimate)
}


# median party-district-election specific deviation
mu.poll.dev.est <- apply(mu.poll.dev, 2, bayesEst, .95, 3)
wk.results.melt$mu.poll.dev.est <- mu.poll.dev.est[1,]

# party-district forecast variance, uncorrected model
wk.results.melt$resid.polls.raw <- wk.results.melt$mu - wk.results.melt$firstshare
wk.results.melt.by_partyXwkr <- group_by(wk.results.melt, partyXwkr)
forecast.variance.polls.raw <- dplyr::summarise(wk.results.melt.by_partyXwkr, forecast.variance.polls.raw = mean((resid.polls.raw)^2, na.rm = TRUE))

# party-district forecast variance, corrected model
wk.results.melt.by_partyXwkr <- group_by(wk.results.melt, partyXwkr)
forecast.variance.polls <- dplyr::summarise(wk.results.melt.by_partyXwkr, forecast.variance.polls = mean((mu.poll.dev.est)^2, na.rm = TRUE))
wk.results.melt.polls <- merge(wk.results.melt, forecast.variance.polls, by = "partyXwkr")
wk.results.melt.polls <- merge(wk.results.melt.polls, forecast.variance.polls.raw, by = "partyXwkr")



## merge projection and polls model data -------------------
wk.results.melt <- merge(wk.results.melt.proj, wk.results.melt.polls, by = c("year", "party", "wkr_nr2013", "partyXwkr", "partyXyear", "firstshare", "winner"), all = TRUE)


## generate forecast weights -------------------------------
wk.results.melt$forecast.weight <- wk.results.melt$forecast.variance.polls / (wk.results.melt$forecast.variance.polls + wk.results.melt$forecast.variance.proj)
wk.results.melt$forecast.weight.raw <- wk.results.melt$forecast.variance.polls.raw / (wk.results.melt$forecast.variance.polls.raw + wk.results.melt$forecast.variance.proj.raw)

wk.results.melt.cdsu <- filter(wk.results.melt, year == 2009, party == "cdsu")
weight.cdsu <- wk.results.melt.cdsu$forecast.weight
wk.results.melt.spd <- filter(wk.results.melt, year == 2009, party == "spd")
weight.spd <- wk.results.melt.spd$forecast.weight
wk.results.melt.fdp <- filter(wk.results.melt, year == 2009, party == "fdp")
weight.fdp <- wk.results.melt.fdp$forecast.weight
wk.results.melt.gru <- filter(wk.results.melt, year == 2009, party == "gru")
weight.gru <- wk.results.melt.gru$forecast.weight
wk.results.melt.lin <- filter(wk.results.melt, year == 2009, party == "lin")
weight.lin <- wk.results.melt.lin$forecast.weight
wk.results.melt.2013 <- filter(wk.results.melt, year == 2013)
weight.all <- wk.results.melt.2013$forecast.weight

## plot weights
pdf ("../figures/hist_weights.pdf", height=5, width=8, family="URWTimes")
par(mar=c(2,4,2,2), lheight = .8)     # b, l, t, r
par(oma=c(.2,.2,.2,.2))
par(mfrow=c(2,3), pty = "s")
plot(density(weight.cdsu), xlim=c(0,1), main="CDU/CSU", xlab="Weight")
rug(weight.cdsu)
plot(density(weight.spd), xlim=c(0,1), main="SPD", xlab="Weight")
rug(weight.spd)
plot(density(weight.fdp), xlim=c(0,1), main="FDP", xlab="Weight")
rug(weight.fdp)
plot(density(weight.gru), xlim=c(0,1), main="B90/Die Grünen", xlab="Weight")
rug(weight.gru)
plot(density(weight.lin), xlim=c(0,1), main="Die Linke", xlab="Weight")
rug(weight.lin)
dev.off()




## extract raw forecasts ------------------------------
polls.2013.df <- filter(wk.results.melt.polls, year==2013)
polls.2013.df.wide <- reshape(polls.2013.df, idvar = "wkr_nr2013", direction = "wide", timevar = "party")
polls.2013.df.wide <- polls.2013.df.wide[order(polls.2013.df.wide$wkr_nr2013),]
mu.parties <- dplyr::select(polls.2013.df.wide, mu.cdsu, mu.spd, mu.fdp, mu.gru, mu.lin)
polls.2013.winners.uncorr <- apply(mu.parties, 1, which.max)

proj.2013.df <- filter(wk.results.melt.proj, year==2013)
proj.2013.df.wide <- reshape(proj.2013.df, idvar = "wkr_nr2013", direction = "wide", timevar = "party")
proj.2013.df.wide <- proj.2013.df.wide[order(proj.2013.df.wide$wkr_nr2013),]
mu.parties <- dplyr::select(proj.2013.df.wide, project.cdsu, project.spd, project.fdp, project.gru, project.lin)
proj.2013.winners.uncorr <- apply(mu.parties, 1, which.max)

comb.2013.df.wide <- reshape(wk.results.melt.2013, idvar = "wkr_nr2013", direction = "wide", timevar = "party")
comb.2013.df.wide <- comb.2013.df.wide[order(comb.2013.df.wide$wkr_nr2013),]
comb.2013.df.wide$comb.cdsu.raw <- comb.2013.df.wide$forecast.weight.raw.cdsu * comb.2013.df.wide$project.cdsu + (1 - comb.2013.df.wide$forecast.weight.raw.cdsu) * comb.2013.df.wide$mu.cdsu
comb.2013.df.wide$comb.spd.raw <- comb.2013.df.wide$forecast.weight.raw.spd * comb.2013.df.wide$project.spd + (1 - comb.2013.df.wide$forecast.weight.raw.spd) * comb.2013.df.wide$mu.spd
comb.2013.df.wide$comb.fdp.raw <- comb.2013.df.wide$forecast.weight.raw.fdp * comb.2013.df.wide$project.fdp + (1 - comb.2013.df.wide$forecast.weight.raw.fdp) * comb.2013.df.wide$mu.fdp
comb.2013.df.wide$comb.gru.raw <- comb.2013.df.wide$forecast.weight.raw.gru * comb.2013.df.wide$project.gru + (1 - comb.2013.df.wide$forecast.weight.raw.gru) * comb.2013.df.wide$mu.gru
comb.2013.df.wide$comb.lin.raw <- comb.2013.df.wide$forecast.weight.raw.lin * comb.2013.df.wide$project.lin + (1 - comb.2013.df.wide$forecast.weight.raw.lin) * comb.2013.df.wide$mu.lin
mu.parties <- dplyr::select(comb.2013.df.wide, comb.cdsu.raw, comb.spd.raw, comb.fdp.raw, comb.gru.raw, comb.lin.raw)
comb.2013.winners.uncorr <- apply(mu.parties, 1, which.max)

comb.2013.df.wide$comb.equalweight.cdsu.raw <- .5*comb.2013.df.wide$project.cdsu + .5*comb.2013.df.wide$mu.cdsu
comb.2013.df.wide$comb.equalweight.spd.raw <- .5*comb.2013.df.wide$project.spd + .5*comb.2013.df.wide$mu.spd
comb.2013.df.wide$comb.equalweight.fdp.raw <- .5*comb.2013.df.wide$project.fdp + .5*comb.2013.df.wide$mu.fdp
comb.2013.df.wide$comb.equalweight.gru.raw <- .5*comb.2013.df.wide$project.gru + .5*comb.2013.df.wide$mu.gru
comb.2013.df.wide$comb.equalweight.lin.raw <- .5*comb.2013.df.wide$project.lin + .5*comb.2013.df.wide$mu.lin
mu.parties <- dplyr::select(comb.2013.df.wide, comb.equalweight.cdsu.raw, comb.equalweight.spd.raw, comb.equalweight.fdp.raw, comb.equalweight.gru.raw, comb.equalweight.lin.raw)
comb.equalweight.2013.winners.uncorr <- apply(mu.parties, 1, which.max)

## assess distribution of winners
table(polls.2013.winners.uncorr)
table(proj.2013.winners.uncorr)
table(comb.2013.winners.uncorr)
table(comb.equalweight.2013.winners.uncorr)



## extract corrected forecasts --------------------------
load("bugs_simulations/polls_bugs_sim_wo_all.RData")
mu <-  dplyr::select(polls_bugs_sim_dat_all, starts_with("mu"))
mu.polls.2013 <- mu[,year.2013.polls]
set.seed(123)
mu.polls.2013 <- sample_n(mu.polls.2013, size = 3000, replace = FALSE) # sample 3000 simulations to get equal number of simulations per forecast
rm(proj_bugs_sim_dat_all)

load("bugs_simulations/proj_bugs_sim_wo_all.RData")
mu <-  dplyr::select(proj_bugs_sim_dat_all, starts_with("mu"))
mu.proj.2013 <- mu[,year.2013.proj]
set.seed(123)
mu.proj.2013 <- sample_n(mu.proj.2013, size = 3000, replace = FALSE) # sample 3000 simulations to get equal number of simulations per forecast
rm(polls_bugs_sim_dat_all)
gc()

start_n <- seq(1,ncol(mu.proj.2013), 299)
mu.proj.2013.cdsu <- mu.proj.2013[,start_n[1]:(start_n[2]-1)]
mu.proj.2013.fdp <- mu.proj.2013[,start_n[2]:(start_n[3]-1)]
mu.proj.2013.gru <- mu.proj.2013[,start_n[3]:(start_n[4]-1)]
mu.proj.2013.lin <- mu.proj.2013[,start_n[4]:(start_n[5]-1)]
mu.proj.2013.spd <- mu.proj.2013[,start_n[5]:ncol(mu.proj.2013)]

start_n <- seq(1,ncol(mu.polls.2013), 299)
mu.polls.2013.cdsu <- mu.polls.2013[,start_n[1]:(start_n[2]-1)]
mu.polls.2013.fdp <- mu.polls.2013[,start_n[2]:(start_n[3]-1)]
mu.polls.2013.gru <- mu.polls.2013[,start_n[3]:(start_n[4]-1)]
mu.polls.2013.lin <- mu.polls.2013[,start_n[4]:(start_n[5]-1)]
mu.polls.2013.spd <- mu.polls.2013[,start_n[5]:ncol(mu.polls.2013)]

# apply historically informed weights to combine forecasts
mu.combined.2013 <- (as.matrix(mu.proj.2013) %*% diag(weight.all)) + (as.matrix(mu.polls.2013) %*% diag(1-weight.all)) # multiply district-party-specific columns in mu.proj.2013 and mu.polls.2013 with vector of district-party-specific weights
start_n <- seq(1,ncol(mu.combined.2013), 299)
mu.combined.2013.cdsu <- mu.combined.2013[,start_n[1]:(start_n[2]-1)]
mu.combined.2013.fdp <- mu.combined.2013[,start_n[2]:(start_n[3]-1)]
mu.combined.2013.gru <- mu.combined.2013[,start_n[3]:(start_n[4]-1)]
mu.combined.2013.lin <- mu.combined.2013[,start_n[4]:(start_n[5]-1)]
mu.combined.2013.spd <- mu.combined.2013[,start_n[5]:ncol(mu.combined.2013)]

# apply equal weights to combine forecasts
mu.combined.equalweight.2013 <- (as.matrix(mu.proj.2013) %*% diag(rep(.5, length(weight.all)))) + (as.matrix(mu.polls.2013)  %*% diag(rep(.5, length(weight.all))))
start_n <- seq(1,ncol(mu.combined.equalweight.2013), 299)
mu.combined.equalweight.2013.cdsu <- mu.combined.equalweight.2013[,start_n[1]:(start_n[2]-1)]
mu.combined.equalweight.2013.fdp <- mu.combined.equalweight.2013[,start_n[2]:(start_n[3]-1)]
mu.combined.equalweight.2013.gru <- mu.combined.equalweight.2013[,start_n[3]:(start_n[4]-1)]
mu.combined.equalweight.2013.lin <- mu.combined.equalweight.2013[,start_n[4]:(start_n[5]-1)]
mu.combined.equalweight.2013.spd <- mu.combined.equalweight.2013[,start_n[5]:ncol(mu.combined.equalweight.2013)]




## compute forecasted winners, corrected models ---------
mu.proj.2013.winner <- matrix(NA, nrow=3000, ncol=299)
for (i in 1:299) {
  for (j in 1:3000) {
    mu.proj.2013.winner[j,i] <- which.max(c(mu.proj.2013.cdsu[j,i],
                                                mu.proj.2013.fdp[j,i],
                                                mu.proj.2013.gru[j,i],
                                                mu.proj.2013.lin[j,i],
                                                mu.proj.2013.spd[j,i]
    )) 
  }
}
mu.proj.2013.winner[mu.proj.2013.winner=="1"] <- "cdsu"
mu.proj.2013.winner[mu.proj.2013.winner=="2"] <- "fdp"
mu.proj.2013.winner[mu.proj.2013.winner=="3"] <- "gru"
mu.proj.2013.winner[mu.proj.2013.winner=="4"] <- "lin"
mu.proj.2013.winner[mu.proj.2013.winner=="5"] <- "spd"

mu.polls.2013.winner <- matrix(NA, nrow=3000, ncol=299)
for (i in 1:299) {
  for (j in 1:3000) {
    mu.polls.2013.winner[j,i] <- which.max(c(mu.polls.2013.cdsu[j,i],
                                                mu.polls.2013.fdp[j,i],
                                                mu.polls.2013.gru[j,i],
                                                mu.polls.2013.lin[j,i],
                                                mu.polls.2013.spd[j,i]
    )) 
  }
}
mu.polls.2013.winner[mu.polls.2013.winner=="1"] <- "cdsu"
mu.polls.2013.winner[mu.polls.2013.winner=="2"] <- "fdp"
mu.polls.2013.winner[mu.polls.2013.winner=="3"] <- "gru"
mu.polls.2013.winner[mu.polls.2013.winner=="4"] <- "lin"
mu.polls.2013.winner[mu.polls.2013.winner=="5"] <- "spd"

mu.combined.2013.winner <- matrix(NA, nrow=3000, ncol=299)
for (i in 1:299) {
for (j in 1:3000) {
  mu.combined.2013.winner[j,i] <- which.max(c(mu.combined.2013.cdsu[j,i],
                                            mu.combined.2013.fdp[j,i],
                                            mu.combined.2013.gru[j,i],
                                            mu.combined.2013.lin[j,i],
                                            mu.combined.2013.spd[j,i]
                                            )) 
}
}
mu.combined.2013.winner[mu.combined.2013.winner=="1"] <- "cdsu"
mu.combined.2013.winner[mu.combined.2013.winner=="2"] <- "fdp"
mu.combined.2013.winner[mu.combined.2013.winner=="3"] <- "gru"
mu.combined.2013.winner[mu.combined.2013.winner=="4"] <- "lin"
mu.combined.2013.winner[mu.combined.2013.winner=="5"] <- "spd"


mu.combined.equalweight.2013.winner <- matrix(NA, nrow=3000, ncol=299)
for (i in 1:299) {
  for (j in 1:3000) {
    mu.combined.equalweight.2013.winner[j,i] <- which.max(c(mu.combined.equalweight.2013.cdsu[j,i],
                                                mu.combined.equalweight.2013.fdp[j,i],
                                                mu.combined.equalweight.2013.gru[j,i],
                                                mu.combined.equalweight.2013.lin[j,i],
                                                mu.combined.equalweight.2013.spd[j,i]
    )) 
  }
}
mu.combined.equalweight.2013.winner[mu.combined.equalweight.2013.winner==1] <- "cdsu"
mu.combined.equalweight.2013.winner[mu.combined.equalweight.2013.winner=="2"] <- "fdp"
mu.combined.equalweight.2013.winner[mu.combined.equalweight.2013.winner=="3"] <- "gru"
mu.combined.equalweight.2013.winner[mu.combined.equalweight.2013.winner=="4"] <- "lin"
mu.combined.equalweight.2013.winner[mu.combined.equalweight.2013.winner=="5"] <- "spd"

winner.2013.pred.proj <- apply(mu.proj.2013.winner, 2, function(x) names(sort(table(x), decreasing = TRUE))[1])
winner.2013.pred.polls <- apply(mu.polls.2013.winner, 2, function(x) names(sort(table(x), decreasing = TRUE))[1])
winner.2013.pred.combined <- apply(mu.combined.2013.winner, 2, function(x) names(sort(table(x), decreasing = TRUE))[1])
winner.2013.pred.combined.equalweight <- apply(mu.combined.equalweight.2013.winner, 2, function(x) names(sort(table(x), decreasing = TRUE))[1])


## assess predicted winning probabilities
prob.2013.pred.combined <- apply(mu.combined.2013.winner, 2, function(x) sort(table(x), decreasing = TRUE)[1])
prob.2013.pred.combined <- prob.2013.pred.combined/3000
hist(prob.2013.pred.combined)

diff <- mu.combined.2013.cdsu - mu.combined.2013.spd
diffmeans <- colMeans(diff)
plot(abs(diffmeans), prob.2013.pred.combined)

## assess distribution of winners
table(winner.2013.pred.proj)
table(winner.2013.pred.polls)
table(winner.2013.pred.combined)
table(winner.2013.pred.combined.equalweight)



## generate table: forecast reports ---------------------
# table: nr, district name, projection winner, polls winner, combined winner

# load district data
wk.results <- read.dta("../data/constituencies/districts_1994_2013.dta")

# load historical data
wk.results.2013 <- read.csv("../data/constituencies/results_2013.csv", header = 2013, sep=";")
summary(wk.results.2013)
wk.results.2013$district.name <- wk.results$wkr_name_2013
wk.results.2013$winner.2013.true <- as.character(apply(cbind(wk.results.2013$cdsu1share, wk.results.2013$spd1share, wk.results.2013$fdp1share, wk.results.2013$gru1share, wk.results.2013$lin1share), 1, which.max))
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==1] <- "CDU/CSU"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==2] <- "SPD"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==3] <- "FDP"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==4] <- "Greens"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==5] <- "Left"

# add forecast winners

# true winners
wk.results.2013$winner.2013.true <- as.character(apply(cbind(wk.results.2013$cdsu1share, wk.results.2013$spd1share, wk.results.2013$fdp1share, wk.results.2013$gru1share, wk.results.2013$lin1share), 1, which.max))
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==1] <- "CDU/CSU"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==2] <- "SPD"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==3] <- "FDP"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==4] <- "Greens"
wk.results.2013$winner.2013.true[wk.results.2013$winner.2013.true==5] <- "Left"

# winners, uncorrected model
wk.proj.2013.uncorr <- dplyr::select(proj.2013.df.wide, wkr_nr2013, project.cdsu, project.spd, project.fdp, project.gru, project.lin)
names(wk.proj.2013.uncorr) <- c("wknr", "cdsu.proj.uncorr", "spd.proj.uncorr", "fdp.proj.uncorr", "gru.proj.uncorr", "lin.proj.uncorr")
wk.polls.2013.uncorr <- dplyr::select(polls.2013.df.wide, wkr_nr2013, mu.cdsu, mu.spd, mu.fdp, mu.gru, mu.lin)
names(wk.polls.2013.uncorr) <- c("wknr", "cdsu.polls.uncorr", "spd.polls.uncorr", "fdp.polls.uncorr", "gru.polls.uncorr", "lin.polls.uncorr")
wk.comb.2013.uncorr <- dplyr::select(comb.2013.df.wide, wkr_nr2013, comb.cdsu.raw, comb.spd.raw, comb.fdp.raw, comb.gru.raw, comb.lin.raw)
names(wk.comb.2013.uncorr) <- c("wknr", "cdsu.comb.uncorr", "spd.comb.uncorr", "fdp.comb.uncorr", "gru.comb.uncorr", "lin.comb.uncorr")
wk.comb.equalweight.2013.uncorr <- dplyr::select(comb.2013.df.wide, wkr_nr2013, comb.equalweight.cdsu.raw, comb.equalweight.spd.raw, comb.equalweight.fdp.raw, comb.equalweight.gru.raw, comb.equalweight.lin.raw)
names(wk.comb.equalweight.2013.uncorr) <- c("wknr", "cdsu.comb.equalweight.uncorr", "spd.comb.equalweight.uncorr", "fdp.comb.equalweight.uncorr", "gru.comb.equalweight.uncorr", "lin.comb.equalweight.uncorr")

wk.results.2013 <- merge(wk.results.2013, wk.polls.2013.uncorr, by = "wknr")
wk.results.2013 <- merge(wk.results.2013, wk.proj.2013.uncorr, by = "wknr")
wk.results.2013 <- merge(wk.results.2013, wk.comb.2013.uncorr, by = "wknr")
wk.results.2013 <- merge(wk.results.2013, wk.comb.equalweight.2013.uncorr, by = "wknr")

wk.results.2013$winner.2013.proj.uncorr <- as.character(apply(cbind(wk.results.2013$cdsu.proj.uncorr, wk.results.2013$spd.proj.uncorr, wk.results.2013$fdp.proj.uncorr, wk.results.2013$gru.proj.uncorr, wk.results.2013$lin.proj.uncorr), 1, which.max))
wk.results.2013$winner.2013.polls.uncorr <- as.character(apply(cbind(wk.results.2013$cdsu.polls.uncorr, wk.results.2013$spd.polls.uncorr, wk.results.2013$fdp.polls.uncorr, wk.results.2013$gru.polls.uncorr, wk.results.2013$lin.polls.uncorr), 1, which.max))
wk.results.2013$winner.2013.comb.uncorr <- as.character(apply(cbind(wk.results.2013$cdsu.comb.uncorr, wk.results.2013$spd.comb.uncorr, wk.results.2013$fdp.comb.uncorr, wk.results.2013$gru.comb.uncorr, wk.results.2013$lin.comb.uncorr), 1, which.max))
wk.results.2013$winner.2013.comb.equalweight.uncorr <- as.character(apply(cbind(wk.results.2013$cdsu.comb.equalweight.uncorr, wk.results.2013$spd.comb.equalweight.uncorr, wk.results.2013$fdp.comb.equalweight.uncorr, wk.results.2013$gru.comb.equalweight.uncorr, wk.results.2013$lin.comb.equalweight.uncorr), 1, which.max))

wk.results.2013$winner.2013.proj.uncorr[wk.results.2013$winner.2013.proj.uncorr==1] <- "CDU/CSU"
wk.results.2013$winner.2013.proj.uncorr[wk.results.2013$winner.2013.proj.uncorr==2] <- "SPD"
wk.results.2013$winner.2013.proj.uncorr[wk.results.2013$winner.2013.proj.uncorr==3] <- "FDP"
wk.results.2013$winner.2013.proj.uncorr[wk.results.2013$winner.2013.proj.uncorr==4] <- "Greens"
wk.results.2013$winner.2013.proj.uncorr[wk.results.2013$winner.2013.proj.uncorr==5] <- "Left"

wk.results.2013$winner.2013.polls.uncorr[wk.results.2013$winner.2013.polls.uncorr==1] <- "CDU/CSU"
wk.results.2013$winner.2013.polls.uncorr[wk.results.2013$winner.2013.polls.uncorr==2] <- "SPD"
wk.results.2013$winner.2013.polls.uncorr[wk.results.2013$winner.2013.polls.uncorr==3] <- "FDP"
wk.results.2013$winner.2013.polls.uncorr[wk.results.2013$winner.2013.polls.uncorr==4] <- "Greens"
wk.results.2013$winner.2013.polls.uncorr[wk.results.2013$winner.2013.polls.uncorr==5] <- "Left"

wk.results.2013$winner.2013.comb.uncorr[wk.results.2013$winner.2013.comb.uncorr==1] <- "CDU/CSU"
wk.results.2013$winner.2013.comb.uncorr[wk.results.2013$winner.2013.comb.uncorr==2] <- "SPD"
wk.results.2013$winner.2013.comb.uncorr[wk.results.2013$winner.2013.comb.uncorr==3] <- "FDP"
wk.results.2013$winner.2013.comb.uncorr[wk.results.2013$winner.2013.comb.uncorr==4] <- "Greens"
wk.results.2013$winner.2013.comb.uncorr[wk.results.2013$winner.2013.comb.uncorr==5] <- "Left"

wk.results.2013$winner.2013.comb.equalweight.uncorr[wk.results.2013$winner.2013.comb.equalweight.uncorr==1] <- "CDU/CSU"
wk.results.2013$winner.2013.comb.equalweight.uncorr[wk.results.2013$winner.2013.comb.equalweight.uncorr==2] <- "SPD"
wk.results.2013$winner.2013.comb.equalweight.uncorr[wk.results.2013$winner.2013.comb.equalweight.uncorr==3] <- "FDP"
wk.results.2013$winner.2013.comb.equalweight.uncorr[wk.results.2013$winner.2013.comb.equalweight.uncorr==4] <- "Greens"
wk.results.2013$winner.2013.comb.equalweight.uncorr[wk.results.2013$winner.2013.comb.equalweight.uncorr==5] <- "Left"


# winners, corrected model
wk.results.2013$winner.2013.pred.proj <- winner.2013.pred.proj
wk.results.2013$winner.2013.pred.polls <- winner.2013.pred.polls
wk.results.2013$winner.2013.pred.combined <- winner.2013.pred.combined
wk.results.2013$winner.2013.pred.combined.equalweight <- winner.2013.pred.combined.equalweight


wk.results.2013$cdsu.2013.pred.proj <- colMeans(mu.proj.2013.cdsu)
wk.results.2013$spd.2013.pred.proj <- colMeans(mu.proj.2013.spd)
wk.results.2013$fdp.2013.pred.proj <- colMeans(mu.proj.2013.fdp)
wk.results.2013$gru.2013.pred.proj <- colMeans(mu.proj.2013.gru)
wk.results.2013$lin.2013.pred.proj <- colMeans(mu.proj.2013.lin)

wk.results.2013$cdsu.2013.pred.polls  <- colMeans(mu.polls.2013.cdsu)
wk.results.2013$spd.2013.pred.polls <- colMeans(mu.polls.2013.spd)
wk.results.2013$fdp.2013.pred.polls <- colMeans(mu.polls.2013.fdp)
wk.results.2013$gru.2013.pred.polls <- colMeans(mu.polls.2013.gru)
wk.results.2013$lin.2013.pred.polls <- colMeans(mu.polls.2013.lin)

wk.results.2013$cdsu.2013.pred.combined <- colMeans(mu.combined.2013.cdsu)
wk.results.2013$spd.2013.pred.combined <- colMeans(mu.combined.2013.spd)
wk.results.2013$fdp.2013.pred.combined <- colMeans(mu.combined.2013.fdp)
wk.results.2013$gru.2013.pred.combined <- colMeans(mu.combined.2013.gru)
wk.results.2013$lin.2013.pred.combined <- colMeans(mu.combined.2013.lin)
wk.results.2013$prob.2013.pred.combined <- round(prob.2013.pred.combined, 3)

wk.results.2013$cdsu.2013.pred.combined.equalweight <- colMeans(mu.combined.equalweight.2013.cdsu)
wk.results.2013$spd.2013.pred.combined.equalweight <- colMeans(mu.combined.equalweight.2013.spd)
wk.results.2013$fdp.2013.pred.combined.equalweight <- colMeans(mu.combined.equalweight.2013.fdp)
wk.results.2013$gru.2013.pred.combined.equalweight <- colMeans(mu.combined.equalweight.2013.gru)
wk.results.2013$lin.2013.pred.combined.equalweight <- colMeans(mu.combined.equalweight.2013.lin)

wk.results.2013$winner.2013.pred.proj[wk.results.2013$winner.2013.pred.proj=="cdsu"] <- "CDU/CSU"
wk.results.2013$winner.2013.pred.proj[wk.results.2013$winner.2013.pred.proj=="spd"] <- "SPD"
wk.results.2013$winner.2013.pred.proj[wk.results.2013$winner.2013.pred.proj=="fdp"] <- "FDP"
wk.results.2013$winner.2013.pred.proj[wk.results.2013$winner.2013.pred.proj=="gru"] <- "Greens"
wk.results.2013$winner.2013.pred.proj[wk.results.2013$winner.2013.pred.proj=="lin"] <- "Left"

wk.results.2013$winner.2013.pred.polls[wk.results.2013$winner.2013.pred.polls=="cdsu"] <- "CDU/CSU"
wk.results.2013$winner.2013.pred.polls[wk.results.2013$winner.2013.pred.polls=="spd"] <- "SPD"
wk.results.2013$winner.2013.pred.polls[wk.results.2013$winner.2013.pred.polls=="fdp"] <- "FDP"
wk.results.2013$winner.2013.pred.polls[wk.results.2013$winner.2013.pred.polls=="gru"] <- "Greens"
wk.results.2013$winner.2013.pred.polls[wk.results.2013$winner.2013.pred.polls=="lin"] <- "Left"

wk.results.2013$winner.2013.pred.combined[wk.results.2013$winner.2013.pred.combined=="cdsu"] <- "CDU/CSU"
wk.results.2013$winner.2013.pred.combined[wk.results.2013$winner.2013.pred.combined=="spd"] <- "SPD"
wk.results.2013$winner.2013.pred.combined[wk.results.2013$winner.2013.pred.combined=="fdp"] <- "FDP"
wk.results.2013$winner.2013.pred.combined[wk.results.2013$winner.2013.pred.combined=="gru"] <- "Greens"
wk.results.2013$winner.2013.pred.combined[wk.results.2013$winner.2013.pred.combined=="lin"] <- "Left"

wk.results.2013$winner.2013.pred.combined.equalweight[wk.results.2013$winner.2013.pred.combined.equalweight=="cdsu"] <- "CDU/CSU"
wk.results.2013$winner.2013.pred.combined.equalweight[wk.results.2013$winner.2013.pred.combined.equalweight=="spd"] <- "SPD"
wk.results.2013$winner.2013.pred.combined.equalweight[wk.results.2013$winner.2013.pred.combined.equalweight=="fdp"] <- "FDP"
wk.results.2013$winner.2013.pred.combined.equalweight[wk.results.2013$winner.2013.pred.combined.equalweight=="gru"] <- "Greens"
wk.results.2013$winner.2013.pred.combined.equalweight[wk.results.2013$winner.2013.pred.combined.equalweight=="lin"] <- "Left"

replaceCSU <- function(df, x) {
repl <- ifelse(x == "CDU/CSU" & as.numeric(rownames(df)) >= 213 & as.numeric(rownames(df)) <= 257, "CSU",
            ifelse(x == "CDU/CSU" & (as.numeric(rownames(df)) < 213 | as.numeric(rownames(df)) > 257), "CDU", x))
return(repl)
}
wk.results.2013$winner.2013.true <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.true)
wk.results.2013$winner.2013.pred.proj <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.pred.proj)
wk.results.2013$winner.2013.pred.polls <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.pred.polls)
wk.results.2013$winner.2013.pred.combined <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.pred.combined)
wk.results.2013$winner.2013.pred.combined.equalweight <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.pred.combined.equalweight)
wk.results.2013$winner.2013.proj.uncorr <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.proj.uncorr)
wk.results.2013$winner.2013.polls.uncorr <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.polls.uncorr)
wk.results.2013$winner.2013.comb.uncorr <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.comb.uncorr)
wk.results.2013$winner.2013.comb.equalweight.uncorr <- replaceCSU(wk.results.2013, wk.results.2013$winner.2013.comb.equalweight.uncorr)


# export forecast table
district_str_length <- str_length(wk.results.2013$district.name)
district_name_points <- ifelse(district_str_length > 20, "...", "")
district_name_shortened <- paste0(str_sub(wk.results.2013$district.name, 1,20), district_name_points)
forecast.table <- data.frame(district.name = district_name_shortened,
                             forecast.winner.true = wk.results.2013$winner.2013.true,
                             forecast.winner.proj = wk.results.2013$winner.2013.pred.proj,
                             forecast.winner.polls = wk.results.2013$winner.2013.pred.polls,
                             forecast.winner.combined = wk.results.2013$winner.2013.pred.combined,
                             cdsu.pred.voteshare = wk.results.2013$cdsu.2013.pred.combined,
                             spd.pred.voteshare = wk.results.2013$spd.2013.pred.combined,
                             fdp.pred.voteshare = wk.results.2013$fdp.2013.pred.combined,
                             gru.pred.voteshare = wk.results.2013$gru.2013.pred.combined,
                             lin.pred.voteshare = wk.results.2013$lin.2013.pred.combined)


forecast.table.latex.xtab <- xtable(forecast.table, digits=2)
caption(forecast.table.latex.xtab) <- "Overview of 2013 forecasts"
print(forecast.table.latex.xtab, hline.after = seq(5, 299, 5), type="latex",table.placement = "t!", caption.placement="top", file="../figures/forecast.table.tex")




## plot: check the sum of estimated shares --------------

sum.shares13.est <- wk.results.2013$cdsu.2013.pred.combined +
                    wk.results.2013$spd.2013.pred.combined + 
                    wk.results.2013$fdp.2013.pred.combined + 
                    wk.results.2013$gru.2013.pred.combined + 
                    wk.results.2013$lin.2013.pred.combined
  
shares13.est.over1 <- ifelse(sum.shares13.est>1,1,0)
table(shares13.est.over1)
mean13.over1 <- mean(shares13.est.over1, na.rm=T)

sum.shares13 <- wk.results.2013$cdsu1share +
  wk.results.2013$spd1share + 
  wk.results.2013$fdp1share + 
  wk.results.2013$gru1share + 
  wk.results.2013$lin1share
shares13.over1 <- ifelse(sum.shares13>1,1,0)
shares13.over1 <- ifelse(sum.shares13>1,1,0)
table(shares13.over1)

pdf(file="../figures/sum_voteshares2013.pdf", height=4.5, width=12, family="URWTimes")
par(mar=c(3,4.1,2,0))
par(oma=c(0,1,1,1))
par(mfrow=c(1,3), pty = "s")
plot(sum.shares13.est, sum.shares13,  ylim=c(.6, 1.05), xlim = c(.6, 1.05), xlab="Forecast", ylab="Outcome", cex.lab=1.5)
title(main="", line=1, cex.main=1.5)
text(155, .6 , paste("Share of sums over 1 = 0"), pos=4, cex=1.2)
abline(lm(sum.shares13 ~ sum.shares13.est), lty=2, lwd=2, col="blue")
abline(h=1, lty=2, lwd=2, col="red")
abline(v=1, lty=2, lwd=2, col="red")
plot(sum.shares13, ylim=c(.6, 1.05), xlab="", ylab="", xaxt="n", yaxt = "n", cex.lab=1.5)
title(main="Sum of vote shares, 2013", line=1, cex.main=1.5)
text(155, .6 , paste("Share of sums over 1 = 0"), pos=4, cex=1.2)
abline(h=1, lty=2, lwd=2, col="red")
plot(sum.shares13.est, ylim=c(.6, 1.05), xlab="", ylab="", xaxt="n", yaxt = "n", cex.lab=1.5)
title(main="Sum of estimated vote shares, 2013", line=1, cex.main=1.5)
abline(h=1, lty=2, lwd=2, col="red")
text(155, .6 , paste("Share of sums over 1 =", format(mean13.over1, digits=2)), pos=4, cex=1.2)	
dev.off()





## plot forecast distribution of seats ----------------------

tab <- table(mu.combined.2013.winner[1,])
cdsu <- tab["cdsu"]
spd <- tab["spd"]
gru <- tab["gru"]
lin <- tab["lin"]
fdp <- tab["fdp"]
for (i in 2:3000) {
  tab2 <- table(mu.combined.2013.winner[i,])
  cdsu <- c(cdsu, tab2["cdsu"])
  spd <-  c(spd, tab2["spd"])
  gru <-  c(gru, tab2["gru"])
  lin <-  c(lin, tab2["lin"])
  fdp <-  c(fdp, tab2["fdp"])
}

pdf("../figures/totalMandatesForecast2013.pdf",width=8,height=2,family="URWTimes")
par(mar=c(2.3,4,1.5,1),mgp=c(2, 1, 0),oma=c(0,0,0,0))
par(mfrow=c(1,4), pty = "s")
plot(table(cdsu)/3000, col="Black",lwd=2,main="CDU/CSU", xlim=c(223,260), xlab = "Forecast direct mandates", ylab = "Probability")
plot(table(spd)/3000, col="Black",lwd=2,main="SPD", xlim=c(33,72), xlab = "Forecast direct mandates", ylab = "Probability")
plot(table(gru)/3000, col="Black",lwd=2,main="B'90/Die Grünen", xlim=c(0,2), xlab = "Forecast direct mandates", ylab = "Probability")
plot(table(lin)/3000, col="Black",lwd=2,main="Die Linke", xlim=c(2,5), xlab = "Forecast direct mandates", ylab = "Probability")
dev.off()



## compute MAEs and percentage correct forecasts ---------------

# uniform swing, uncorrected
mean(c(
	abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.proj.uncorr),
	abs(wk.results.2013$spd1share - wk.results.2013$spd.proj.uncorr),
	abs(wk.results.2013$fdp1share - wk.results.2013$fdp.proj.uncorr),
	abs(wk.results.2013$gru1share - wk.results.2013$gru.proj.uncorr),
	abs(wk.results.2013$lin1share - wk.results.2013$lin.proj.uncorr)))

# polling, uncorrected
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.polls.uncorr),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.polls.uncorr),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.polls.uncorr),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.polls.uncorr),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.polls.uncorr)))

# combined, uncorrected
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.comb.uncorr),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.comb.uncorr),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.comb.uncorr),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.comb.uncorr),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.comb.uncorr)))

# combined, uncorrected, equalweight
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.comb.equalweight.uncorr),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.comb.equalweight.uncorr),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.comb.equalweight.uncorr),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.comb.equalweight.uncorr),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.comb.equalweight.uncorr)))

# uniform swing, corrected
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.2013.pred.proj),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.2013.pred.proj),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.2013.pred.proj),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.2013.pred.proj),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.2013.pred.proj)))

# polling, corrected
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.2013.pred.polls),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.2013.pred.polls),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.2013.pred.polls),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.2013.pred.polls),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.2013.pred.polls)))


# combined, corrected
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.2013.pred.combined),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.2013.pred.combined),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.2013.pred.combined),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.2013.pred.combined),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.2013.pred.combined)))

# combined, corrected, equalweight
mean(c(
  abs(wk.results.2013$cdsu1share - wk.results.2013$cdsu.2013.pred.combined.equalweight),
  abs(wk.results.2013$spd1share - wk.results.2013$spd.2013.pred.combined.equalweight),
  abs(wk.results.2013$fdp1share - wk.results.2013$fdp.2013.pred.combined.equalweight),
  abs(wk.results.2013$gru1share - wk.results.2013$gru.2013.pred.combined.equalweight),
  abs(wk.results.2013$lin1share - wk.results.2013$lin.2013.pred.combined.equalweight)))


# compute percentage of correct forecasts, uncorrected models
table(wk.results.2013$winner.2013.true, wk.results.2013$winner.2013.proj.uncorr)
table(wk.results.2013$winner.2013.true, wk.results.2013$winner.2013.polls.uncorr)
table(wk.results.2013$winner.2013.true, wk.results.2013$winner.2013.comb.uncorr)
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.proj.uncorr))
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.polls.uncorr))
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.comb.uncorr))
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.comb.equalweight.uncorr))

# compute percentage of correct forecasts, corrected models
table(wk.results.2013$winner.2013.true, wk.results.2013$winner.2013.pred.proj)
table(wk.results.2013$winner.2013.true, wk.results.2013$winner.2013.pred.polls)
table(wk.results.2013$winner.2013.true, wk.results.2013$winner.2013.pred.combined)
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.pred.proj))
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.pred.polls))
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.pred.combined))
prop.table(table(wk.results.2013$winner.2013.true == wk.results.2013$winner.2013.pred.combined.equalweight))




## identify marginal districts -----------------------------

# and performance within this subsample
sub <- wk.results.2013[,c("cdsu1share","spd1share","fdp1share","gru1share","lin1share")]
wk.results.2013$winnershare <- apply(sub, 1, function(x){sort(x,partial=5)[5]})
wk.results.2013$secondshare <- apply(sub, 1, function(x){sort(x,partial=4)[4]})
wk.results.2013$marginality <- wk.results.2013$winnershare - wk.results.2013$secondshare

# compute percentage of correct forecasts, marginal districts 
wk.results.2013.marginal <- filter(wk.results.2013, marginality < .1)

prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.proj.uncorr))
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.polls.uncorr))
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.comb.uncorr))
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.comb.equalweight.uncorr))


table(wk.results.2013.marginal$winner.2013.true, wk.results.2013.marginal$winner.2013.pred.proj)
table(wk.results.2013.marginal$winner.2013.true, wk.results.2013.marginal$winner.2013.pred.polls)
table(wk.results.2013.marginal$winner.2013.true, wk.results.2013.marginal$winner.2013.pred.combined)
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.pred.proj))
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.pred.polls))
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.pred.combined))
prop.table(table(wk.results.2013.marginal$winner.2013.true == wk.results.2013.marginal$winner.2013.pred.combined.equalweight))


## plot district winner maps -----------------------------

spdf.area$wknr <- spdf.area$WKR_NR
spdf.results <- spdf.area
spdf.results$winner.2013.true <- wk.results.2013.df$winner.2013.true
spdf.results$winner.2013.forecast <- wk.results.2013.df$forecast.winner.combined


# plot map of districts covered, colors identify predicted winner
pdf ("../figures/map_results.pdf", height=8, width=6, family="URWTimes")
par(mar=c(.1,.1,.1,.1), lheight = .8)
par(oma=c(.1,.1,.1,.1))
plot(spdf.results)
plot(spdf.results[spdf.results$winner.2013.true=="CDU/CSU",], col=rgb(80,80,80, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.true=="SPD",], col=rgb(222,45,38, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.true=="FDP",], col=rgb(254,178,76, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.true=="B'90/Die Gr?nen",], col=rgb(49,163,84, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.true=="Die Linke",], col=rgb(136,86,167, max=255), add=T)
#centroids <- getSpPPolygonsLabptSlots(spdf.results)
#invisible(shadowtext(centroids, labels=as.character(spdf.results$WKR_NR), cex=.7, col="black", bg="white", font=1))
dev.off()

pdf ("../figures/map_forecasts.pdf", height=8, width=6, family="URWTimes")
par(mar=c(.1,.1,.1,.1), lheight = .8)
par(oma=c(.1,.1,.1,.1))
plot(spdf.results)
plot(spdf.results[spdf.results$winner.2013.forecast=="CDU/CSU",], col=rgb(80,80,80, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.forecast=="SPD",], col=rgb(222,45,38, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.forecast=="FDP",], col=rgb(254,178,76, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.forecast=="B'90/Die Gr?nen",], col=rgb(49,163,84, max=255), add=T)
plot(spdf.results[spdf.results$winner.2013.forecast=="Die Linke",], col=rgb(136,86,167, max=255), add=T)
#centroids <- getSpPPolygonsLabptSlots(spdf.results)
#invisible(shadowtext(centroids, labels=as.character(spdf.results$WKR_NR), cex=.7, col="black", bg="white", font=1))
dev.off()

names(forecasts.projection.2013)
names(forecasts.poll.2013)
forecast.combined.2013.cdsu



## plot predicted vs. true scatterplots -----------------------------

# plot forecast vs. true 
pdf(file="../figures/pvt_2013.pdf", height=8, width=12, family="URWTimes")
par(mar=c(2,2,2,2), lheight = .8)     # b, l, t, r
par(oma=c(4,4,3,2))
par(mfrow=c(3,5), pty = "s")
### 2013, projection forecast
plot(wk.results.2013.df$cdsu1share, forecasts.projection.2013$cdsu.2013.proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7, main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(2, at = .35, label = "Uniform swing", las = 0, tick=F, outer=T, cex.axis=2, line=0)
	axis(3, at = .35, label = "CDU/CSU", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$cdsu1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.projection.2013$cdsu.2013.proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
plot(wk.results.2013.df$spd1share, forecasts.projection.2013$spd.2013.proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "SPD", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$spd1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.projection.2013$spd.2013.proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
plot(wk.results.2013.df$fdp1share, forecasts.projection.2013$fdp.2013.proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "FDP", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$fdp1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.projection.2013$fdp.2013.proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
plot(wk.results.2013.df$gru1share, forecasts.projection.2013$gru.2013.proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "B90/Die Gr?nen", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$gru1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.projection.2013$gru.2013.proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
plot(wk.results.2013.df$lin1share, forecasts.projection.2013$lin.2013.proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "Die Linke", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$lin1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.projection.2013$lin.2013.proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
### 2013, polling forecast
plot(wk.results.2013.df$cdsu1share, forecasts.poll.2013$cdsu.2013.poll, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7, main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(2, at = .35, label = "Polling", las = 0, tick=F, outer=T, cex.axis=2, line=0)
	#axis(3, at = .35, label = "CDU/CSU", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$cdsu1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.poll.2013$cdsu.2013.poll, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
plot(wk.results.2013.df$spd1share, forecasts.poll.2013$spd.2013.poll, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "SPD", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$spd1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.poll.2013$spd.2013.poll, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
plot(wk.results.2013.df$fdp1share, forecasts.poll.2013$fdp.2013.poll, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "FDP", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$fdp1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.poll.2013$fdp.2013.poll, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
plot(wk.results.2013.df$gru1share, forecasts.poll.2013$gru.2013.poll, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "B90/Die Gr?nen", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$gru1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.poll.2013$gru.2013.poll, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
plot(wk.results.2013.df$lin1share, forecasts.poll.2013$lin.2013.poll, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "Die Linke", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$lin1share, na.rm=T), lty = 2)
	abline(v = mean(forecasts.poll.2013$lin.2013.poll, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
### 2013, combined forecast
plot(wk.results.2013.df$cdsu1share, forecast.combined.2013$forecast.combined.2013.cdsu, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7, main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(2, at = .35, label = "Combined forecast", las = 0, tick=F, outer=T, cex.axis=2, line=0)
	#axis(3, at = .35, label = "CDU/CSU", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$cdsu1share, na.rm=T), lty = 2)
	abline(v = mean(forecast.combined.2013$forecast.combined.2013.cdsu, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
plot(wk.results.2013.df$spd1share, forecast.combined.2013$forecast.combined.2013.spd, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "SPD", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$spd1share, na.rm=T), lty = 2)
	abline(v = mean(forecast.combined.2013$forecast.combined.2013.spd, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
plot(wk.results.2013.df$fdp1share, forecast.combined.2013$forecast.combined.2013.fdp, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "FDP", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$fdp1share, na.rm=T), lty = 2)
	abline(v = mean(forecast.combined.2013$forecast.combined.2013.fdp, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
plot(wk.results.2013.df$gru1share, forecast.combined.2013$forecast.combined.2013.gru, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "B90/Die Gr?nen", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$gru1share, na.rm=T), lty = 2)
	abline(v = mean(forecast.combined.2013$forecast.combined.2013.gru, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
plot(wk.results.2013.df$lin1share, forecast.combined.2013$forecast.combined.2013.lin, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	#axis(3, at = .35, label = "Die Linke", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(wk.results.2013.df$lin1share, na.rm=T), lty = 2)
	abline(v = mean(forecast.combined.2013$forecast.combined.2013.lin, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
dev.off()	




## inspect forecast errors -----------------------------

# identify falsely predicted districts
wk.results.2013.df$forecast.failure <- ifelse(wk.results.2013.df$winner.2013.true != wk.results.2013.df$forecast.winner.combined, 1, 0)
wk.results.2013.df$district.name[wk.results.2013.df$winner.2013.true != wk.results.2013.df$forecast.winner.combined]
wk.results.2013.df$winner.2013.true[wk.results.2013.df$winner.2013.true != wk.results.2013.df$forecast.winner.combined]

vote1shares <- cbind(wk.results.2013.df$cdsu1share,wk.results.2013.df$spd1share,wk.results.2013.df$fdp1share,wk.results.2013.df$gru1share,wk.results.2013.df$lin1share)
wk.results.2013.df$winner.share <- apply(vote1shares, 1, max)
wk.results.2013.df$second.share <- apply(vote1shares, 1, function(x) {sort(x,partial=4)[4] })
wk.results.2013.df$winning.margin <- wk.results.2013.df$winner.share - wk.results.2013.df$second.share
summary(wk.results.2013.df$winning.margin)

# generate table of forecast failures
failures.df <- wk.results.2013.df[wk.results.2013.df$forecast.failure==1,]

failures.table <- data.frame(wknr=failures.df$wknr,district.name=str_sub(failures.df$district.name,1,20),fc.poll=failures.df$forecast.winner.poll,fc.proj=failures.df$forecast.winner.projection,fc.combined=failures.df$forecast.winner.combined,winner=failures.df$winner.2013.true, margin=failures.df$winning.margin)
failures.table <- failures.table[order(failures.table$margin),]
rownames(failures.table) <- NULL

failures.table.latex.xtab <- xtable(failures.table, digits=3)
caption(failures.table.latex.xtab) <- "Failed forecasts"
print(failures.table.latex.xtab, type="latex",table.placement = "t!", caption.placement="top", file="../figures/failures.table.tex")

forecasts.2013.df <- cbind(forecast.combined.2013, wk.results.2013.df)
write.dta(forecasts.2013.df, "forecasts_ger_2013.dta", version = 9)



### evaluation
uncorr.swing <- c(224, 70, 0, 1, 4)
uncorr.poll <- c(290, 9, 0, 0, 0) 
uncorr.comb.ew <- c(264, 31, 0, 1, 4)
uncorr.comb <- c(229, 65, 0, 1, 4)
swing <- c(223, 71, 0, 1, 4)
poll <- c(270, 27, 0, 1, 1)
comb.ew <- c(245, 50, 0, 1, 3)
comb <- c(242, 53, 0, 1, 3)
election.de <- c(224, 69, 0, 1, 5)
spiegel.de <- c(181, 89, 0, 3, 13)
actual <- c(236, 58, 0, 1, 4)
seats_df <- data.frame(uncorr.swing, uncorr.poll, uncorr.comb.ew, uncorr.comb, swing, poll, comb.ew, comb, election.de, spiegel.de)

apply(seats_df, 2, function(x) {abs(x - actual) %>% sum})



