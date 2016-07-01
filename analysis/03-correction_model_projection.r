# Forecasting Elections at the Constituency Level
# Simon Munzert


# load packages
source("packages.r")

# load prepared district data
load("prepared_data/data_prep_model_projection.RData")


## estimation -----------------------------------

## set first share variable empty for leave-one-out estimation
table(wk.results.melt$year)
wk.results.melt$firstshare[wk.results.melt$year == 1994] <- NA # repeat process for all elections until 2009


## set up Bayesian model

# define variables
y <- wk.results.melt$firstshare
project <- wk.results.melt$project
dominance <- wk.results.melt$dominance
newrun <- wk.results.melt$newrun
party <- as.numeric(as.factor(wk.results.melt$party))
partyXwkr <- as.numeric(as.factor(wk.results.melt$partyXwkr))
N <- nrow(wk.results.melt)
J <- length(unique(wk.results.melt$party))
K <- length(unique(wk.results.melt$partyXwkr))


# bugs model
proj_bugs <- function(){

  # main model  
  for (i in 1:N){
  y[i] ~ dnorm(mu[i], tau.y[party[i]])
  mu[i]  <- alpha[party[i]] +
              b.project[party[i]]*project[i] + 
              b.dominance[party[i]]*dominance[i] + 
              b.newrun[party[i]]*newrun[i] + 
              u[partyXwkr[i]]
  }
  
  # varying slopes and intercepts
  for (j in 1:J) {
  alpha[j] ~ dnorm(alpha.hat[j], tau.alpha)  
  b.project[j] ~ dnorm(b.project.hat[j], tau.b.project)
  b.dominance[j] ~ dnorm(b.dominance.hat[j], tau.b.dominance)
  b.newrun[j] ~ dnorm(b.newrun.hat[j], tau.b.newrun)
  alpha.hat[j] <- mu.alpha
  b.project.hat[j] <- mu.b.project
  b.dominance.hat[j] <- mu.b.dominance
  b.newrun.hat[j] <- mu.b.newrun
  tau.y[j] <- pow(sigma.y[j], -2)
  sigma.y[j] ~ dunif(0, 100) # prior
  }
  
  for (k in 1:K) {
  u[k] ~ dnorm(0, tau.u)
  }
  
  # Priors 
  mu.alpha ~ dnorm(0, .0001)
  mu.b.project ~ dnorm(0, .0001)
  mu.b.dominance ~ dnorm(0, .0001)
  mu.b.newrun ~ dnorm(0, .0001)
  tau.alpha <- pow(sigma.alpha, -2)
  tau.b.project <- pow(sigma.b.project, -2)
  tau.b.dominance <- pow(sigma.b.dominance, -2)
  tau.b.newrun <- pow(sigma.b.newrun, -2) 
  tau.u <- pow(sigma.u, -2)
  sigma.alpha ~ dunif(0, 100)
  sigma.b.project ~ dunif(0, 100)
  sigma.b.dominance ~ dunif(0, 100)
  sigma.b.newrun ~ dunif(0, 100)
  sigma.u ~ dunif(0,100)  
}
  
write.model(proj_bugs, "C:/WinBUGS14/Models/ger_projection_bugs.txt")

# prepare data
proj_bugs_data <- list("y","project","dominance","newrun","party","partyXwkr","N","J","K")
proj_bugs <- "C:/WinBUGS14/Models/ger_projection_bugs.txt"

# set inits
set.seed(123)
proj_bugs_inits <- function(){list(alpha=rnorm(J, mean = 0, sd = 0.01), b.project=rnorm(J,1), b.dominance=rnorm(J,1), b.newrun=rnorm(J,1), u=rnorm(K, mean = 0, sd = 0.01), sigma.y=runif(J,0,1), sigma.alpha=runif(1,0,1), sigma.b.project=runif(1,0,1), sigma.b.dominance=runif(1,0,1), sigma.b.newrun=runif(1,0,1), sigma.u=runif(1,0,1))}

# Run MCMC
proj_bugs_sim <- bugs(data=proj_bugs_data, inits=proj_bugs_inits, model.file=proj_bugs, parameters.to.save=c("alpha", "b.project", "b.dominance", "b.newrun", "u", "sigma.y", "sigma.alpha", "sigma.b.project", "sigma.b.dominance", "sigma.b.newrun", "sigma.u", "mu"), n.chains=3, n.iter=10000, n.burnin=9000, n.thin=1, bugs.directory="C:/WinBUGS14", debug=T) 

plot(proj_bugs_sim)
print(proj_bugs_sim, digits=2)

save(proj_bugs_sim, file = "bugs_simulations/proj_bugs_sim_wo1994.RData")



## process model output -----------------------

# create data.frame from simulation run
bugsSimDat <- function(x) {
attach.bugs(x)
proj_bugs_sim_dat <- data.frame(alpha_1 = alpha[,1],
                                alpha_2 = alpha[,2],
                                alpha_3 = alpha[,3],
                                alpha_4 = alpha[,4],
                                alpha_5 = alpha[,5],
                                b.project_1 = b.project[,1],
                                b.project_2 = b.project[,2],
                                b.project_3 = b.project[,3],
                                b.project_4 = b.project[,4],
                                b.project_5 = b.project[,5],
                                b.dominance_1 = b.dominance[,1], 
                                b.dominance_2 = b.dominance[,2],
                                b.dominance_3 = b.dominance[,3],
                                b.dominance_4 = b.dominance[,4],
                                b.dominance_5 = b.dominance[,5],  
                                b.newrun_1 = b.newrun[,1], 
                                b.newrun_2 = b.newrun[,2],
                                b.newrun_3 = b.newrun[,3],
                                b.newrun_4 = b.newrun[,4],
                                b.newrun_5 = b.newrun[,5], 
                                sigma.u = sigma.u,
                                sigma.y_1 = sigma.y[,1],
                                sigma.y_2 = sigma.y[,2],
                                sigma.y_3 = sigma.y[,3],
                                sigma.y_4 = sigma.y[,4],                                                        sigma.y_5 = sigma.y[,5],
                                mu = mu
                                )
detach.bugs()
return(proj_bugs_sim_dat)
}

# create combined data frame from simulations

load("bugs_simulations/proj_bugs_sim_wo1994.RData")
proj_bugs_sim_dat_wo1994 <- bugsSimDat(proj_bugs_sim)
load("bugs_simulations/proj_bugs_sim_wo1998.RData")
proj_bugs_sim_dat_wo1998 <- bugsSimDat(proj_bugs_sim)
load("bugs_simulations/proj_bugs_sim_wo2002.RData")
proj_bugs_sim_dat_wo2002 <- bugsSimDat(proj_bugs_sim)
load("bugs_simulations/proj_bugs_sim_wo2005.RData")
proj_bugs_sim_dat_wo2005 <- bugsSimDat(proj_bugs_sim)
load("bugs_simulations/proj_bugs_sim_wo2009.RData")
proj_bugs_sim_dat_wo2009 <- bugsSimDat(proj_bugs_sim)

proj_bugs_sim_dat_all <- Reduce(function(...) merge(..., all = TRUE), list(proj_bugs_sim_dat_wo1994, proj_bugs_sim_dat_wo1998, proj_bugs_sim_dat_wo2002, proj_bugs_sim_dat_wo2005, proj_bugs_sim_dat_wo2009))

save(proj_bugs_sim_dat_all, file = "bugs_simulations/proj_bugs_sim_wo_all.RData")



# function to evaluate simulations
bayesEst <- function(x, lvl, digits) {
med <- round(median(x), digits)
prob <- (1 - lvl)/2
percLo <- round(quantile(x, probs=prob), digits)
percHi <- round(quantile(x, probs=c(1-prob)), digits)
estimate <- c(med, paste0("[", percLo, ";", percHi, "]"))
return(estimate)
}

# extract parameter estimates
alphaCDSU <- bayesEst(proj_bugs_sim_dat_all$alpha_1, .95, 3)
alphaSPD <- bayesEst(proj_bugs_sim_dat_all$alpha_5, .95, 3)
alphaFDP <- bayesEst(proj_bugs_sim_dat_all$alpha_2, .95, 3)
alphaGRU <- bayesEst(proj_bugs_sim_dat_all$alpha_3, .95, 3)
alphaLIN <- bayesEst(proj_bugs_sim_dat_all$alpha_4, .95, 3)

b.projectCDSU <- bayesEst(proj_bugs_sim_dat_all$b.project_1, .95, 3)
b.projectSPD <- bayesEst(proj_bugs_sim_dat_all$b.project_5, .95, 3)
b.projectFDP <- bayesEst(proj_bugs_sim_dat_all$b.project_2, .95, 3)
b.projectGRU <- bayesEst(proj_bugs_sim_dat_all$b.project_3, .95, 3)
b.projectLIN <- bayesEst(proj_bugs_sim_dat_all$b.project_4, .95, 3)

b.dominanceCDSU <- bayesEst(proj_bugs_sim_dat_all$b.dominance_1, .95, 3)
b.dominanceSPD <- bayesEst(proj_bugs_sim_dat_all$b.dominance_5, .95, 3)
b.dominanceFDP <- bayesEst(proj_bugs_sim_dat_all$b.dominance_2, .95, 3)
b.dominanceGRU <- bayesEst(proj_bugs_sim_dat_all$b.dominance_3, .95, 3)
b.dominanceLIN <- bayesEst(proj_bugs_sim_dat_all$b.dominance_4, .95, 3)

b.newrunCDSU <- bayesEst(proj_bugs_sim_dat_all$b.newrun_1, .95, 3)
b.newrunSPD <- bayesEst(proj_bugs_sim_dat_all$b.newrun_5, .95, 3)
b.newrunFDP <- bayesEst(proj_bugs_sim_dat_all$b.newrun_2, .95, 3)
b.newrunGRU <- bayesEst(proj_bugs_sim_dat_all$b.newrun_3, .95, 3)
b.newrunLIN <- bayesEst(proj_bugs_sim_dat_all$b.newrun_4, .95, 3)

sigma.u.est <- bayesEst(proj_bugs_sim_dat_all$sigma.u, .95, 3)
sigma.y.CDSU.est <- bayesEst(proj_bugs_sim_dat_all$sigma.y_1, .95, 3)
sigma.y.SPD.est <- bayesEst(proj_bugs_sim_dat_all$sigma.y_5, .95, 3)
sigma.y.FDP.est <- bayesEst(proj_bugs_sim_dat_all$sigma.y_2, .95, 3)
sigma.y.GRU.est <- bayesEst(proj_bugs_sim_dat_all$sigma.y_3, .95, 3)
sigma.y.LIN.est <- bayesEst(proj_bugs_sim_dat_all$sigma.y_4, .95, 3)

# for documentation: retrieve model estimates
N <- nrow(wk.results.melt)
model.table <-  as.data.frame(matrix(NA,nrow=32,ncol=3))
colnames(model.table) <- c("Variable name", "Estimate", "95\\% CI")
model.table[,1] <- c(
  'Intercept $\\alpha$',
  '---CDU/CSU',
  '---SPD',
  '---FDP',
  '---B90/Die Gr\\"unen',
  '---Die Linke',
  'Projection estimate $\\beta^\\text{proj}$',
  '---CDU/CSU',
  '---SPD',
  '---FDP',
  '---B90/Die Gr\\"unen',
  '---Die Linke',
  'Party dominance $\\beta^\\text{dom}$',
  '---CDU/CSU',
  '---SPD',
  '---FDP',
  '---B90/Die Gr\\"unen',
  '---Die Linke',
  'Pioneer  $\\beta^\\text{pio}$',
  '---CDU/CSU',
  '---SPD',
  '---FDP',
  '---B90/Die Gr\\"unen',
  '---Die Linke',
 'Party-district-level variance $\\sigma^2_{\\xi}$ ',
 'Residual variance $\\sigma^2_{\\eta}$',
 '---CDU/CSU',
 '---SPD',
 '---FDP',
 '---B90/Die Gr\\"unen',
 '---Die Linke', 
 'N')
model.table[2,2:3] <- alphaCDSU
model.table[3,2:3] <- alphaSPD
model.table[4,2:3] <- alphaFDP
model.table[5,2:3] <- alphaGRU
model.table[6,2:3] <- alphaLIN

model.table[8,2:3] <- b.projectCDSU
model.table[9,2:3] <- b.projectSPD
model.table[10,2:3] <- b.projectFDP
model.table[11,2:3] <- b.projectGRU
model.table[12,2:3] <- b.projectLIN

model.table[14,2:3] <- b.dominanceCDSU
model.table[15,2:3] <- b.dominanceSPD
model.table[16,2:3] <- b.dominanceFDP
model.table[17,2:3] <- b.dominanceGRU
model.table[18,2:3] <- b.dominanceLIN

model.table[20,2:3] <- b.newrunCDSU
model.table[21,2:3] <- b.newrunSPD
model.table[22,2:3] <- b.newrunFDP
model.table[23,2:3] <- b.newrunGRU
model.table[24,2:3] <- b.newrunLIN

model.table[25,2:3] <- sigma.u.est
model.table[27,2:3] <- sigma.y.CDSU.est
model.table[28,2:3] <- sigma.y.SPD.est
model.table[29,2:3] <- sigma.y.FDP.est
model.table[30,2:3] <- sigma.y.GRU.est
model.table[31,2:3] <- sigma.y.LIN.est
model.table[32,2] <- N

table.latex.xtab <- xtable(model.table, digits=3)
caption(table.latex.xtab) <- "Bayesian estimates of the model of party first vote shares, based on projection model"
print(table.latex.xtab, type="latex", sanitize.text.function=function(x){x}, table.placement = "t!", include.rownames = FALSE, caption.placement="top", file="../figures/table.data.projection.model.bayesian.tex")



## evaluate corrected predictions ------------------

# select corrected prediction variables
proj_bugs_sim_dat_all_mu <- dplyr::select(proj_bugs_sim_dat_all, starts_with("mu"))

# analyze corrected prediction
mu.est <- apply(proj_bugs_sim_dat_all_mu, 2, bayesEst, .95, 6)
mu.est <- as.numeric(mu.est[1,])
wk.results.melt$forecast <- mu.est
plot(dplyr::select(wk.results.melt, forecast, project, firstshare))

# generate predicted winner for uncorrected and corrected prediction
wk.results.melt$wkrXyear <- interaction(wk.results.melt$wkr_nr2013,wk.results.melt$year)
wk.results.melt.by_wkrXyear <- group_by(wk.results.melt, wkrXyear)
  
wk.winner.pred <- summarise(wk.results.melt.by_wkrXyear, winner.share.pred = sort(project, decreasing = TRUE)[1])
wk.second.pred <- summarise(wk.results.melt.by_wkrXyear, winner.share.pred = sort(project, decreasing = TRUE)[2])
table(wk.winner.pred[,2] > wk.second.pred[,2])
wk.winner.pred.corr <- summarise(wk.results.melt.by_wkrXyear, winner.share.pred.corr = sort(forecast, decreasing = TRUE)[1])
wk.second.pred.corr <- summarise(wk.results.melt.by_wkrXyear, winner.share.pred.corr = sort(forecast, decreasing = TRUE)[2])
table(wk.winner.pred.corr[,2] > wk.second.pred.corr[,2])

wk.results.melt <- merge(wk.results.melt, wk.winner.pred, by = "wkrXyear")
wk.results.melt <- merge(wk.results.melt, wk.winner.pred.corr, by = "wkrXyear")

wk.results.melt$winner.pred <- ifelse(round(wk.results.melt$project, 6) == round(wk.results.melt$winner.share.pred, 6), 1, 0)
wk.results.melt$winner.pred.corr <- ifelse(round(wk.results.melt$forecast, 6) == round(wk.results.melt$winner.share.pred.corr, 6), 1, 0)
table(wk.results.melt$winner.pred,wk.results.melt$winner.pred.corr)

# deviation
wk.results.melt$resid.pred <- wk.results.melt$project - wk.results.melt$firstshare
wk.results.melt$resid.pred.corr <- wk.results.melt$forecast - wk.results.melt$firstshare

mae <- function(var, pty, yr) {
  data = wk.results.melt
  dat <- filter(data, party == pty & year == yr)
  mean.abs.err <- mean(abs(dat[,var]), na.rm = TRUE)
  return(mean.abs.err)
}

# correct shares
correctWinner <- function(yr) {
  dat <- dplyr::filter(wk.results.melt, year == yr, winner == 1)
  tab <- table(dat$winner.pred == dat$winner)
  correct.share <- (tab[2])/sum(tab)
  return(correct.share)
}
correctWinnerCorr <- function(yr) {
  dat <- dplyr::filter(wk.results.melt, year == yr, winner == 1)
  tab <- table(dat$winner.pred.corr == dat$winner)
  correct.share <- (tab[2])/sum(tab)
  return(correct.share)
}


### MAE FORECAST BENCHMARK BEFORE AND AFTER META-CORRECTION
benchmark.table.mae <-  as.data.frame(matrix(NA,nrow=6,ncol=6))
colnames(benchmark.table.mae) <- c('CDU/CSU', 'SPD', 'FDP', 'B90/Die Gr\\"unen', 'Die Linke', '\\% of correctly predicted winners')
rownames(benchmark.table.mae) <- c("Uncorrected", "1994", "1998", "2002", "2005", "2009")

benchmark.table.mae[2,] <- c(mae("resid.pred","cdsu",1994), mae("resid.pred","spd",1994), mae("resid.pred","fdp",1994), mae("resid.pred","gru",1994), mae("resid.pred","lin",1994),correctWinner(1994)*100)
benchmark.table.mae[3,] <- c(mae("resid.pred","cdsu",1998), mae("resid.pred","spd",1998), mae("resid.pred","fdp",1998), mae("resid.pred","gru",1998), mae("resid.pred","lin",1998),correctWinner(1998)*100)
benchmark.table.mae[4,] <- c(mae("resid.pred","cdsu",2002), mae("resid.pred","spd",2002), mae("resid.pred","fdp",2002), mae("resid.pred","gru",2002), mae("resid.pred","lin",2002),correctWinner(2002)*100)
benchmark.table.mae[5,] <- c(mae("resid.pred","cdsu",2005), mae("resid.pred","spd",2005), mae("resid.pred","fdp",2005), mae("resid.pred","gru",2005), mae("resid.pred","lin",2005),correctWinner(2005)*100)
benchmark.table.mae[6,] <- c(mae("resid.pred","cdsu",2009), mae("resid.pred","spd",2009), mae("resid.pred","fdp",2009), mae("resid.pred","gru",2009), mae("resid.pred","lin",2009),correctWinner(2009)*100)

benchmark.table.mae.corr <-  as.data.frame(matrix(NA,nrow=6,ncol=6))
colnames(benchmark.table.mae.corr) <- c('CDU/CSU', 'SPD', 'FDP', 'B90/Die Gr\\"unen', 'Die Linke', '\\% of correctly predicted winners')
rownames(benchmark.table.mae.corr) <- c("Corrected", "1994", "1998", "2002", "2005", "2009")
benchmark.table.mae.corr[2,] <- c(mae("resid.pred.corr","cdsu",1994), mae("resid.pred.corr","spd",1994), mae("resid.pred.corr","fdp",1994), mae("resid.pred.corr","gru",1994), mae("resid.pred.corr","lin",1994),correctWinnerCorr(1994)*100)
benchmark.table.mae.corr[3,] <- c(mae("resid.pred.corr","cdsu",1998), mae("resid.pred.corr","spd",1998), mae("resid.pred.corr","fdp",1998), mae("resid.pred.corr","gru",1998), mae("resid.pred.corr","lin",1998),correctWinnerCorr(1998)*100)
benchmark.table.mae.corr[4,] <- c(mae("resid.pred.corr","cdsu",2002), mae("resid.pred.corr","spd",2002), mae("resid.pred.corr","fdp",2002), mae("resid.pred.corr","gru",2002), mae("resid.pred.corr","lin",2002),correctWinnerCorr(2002)*100)
benchmark.table.mae.corr[5,] <- c(mae("resid.pred.corr","cdsu",2005), mae("resid.pred.corr","spd",2005), mae("resid.pred.corr","fdp",2005), mae("resid.pred.corr","gru",2005), mae("resid.pred.corr","lin",2005),correctWinnerCorr(2005)*100)
benchmark.table.mae.corr[6,] <- c(mae("resid.pred.corr","cdsu",2009), mae("resid.pred.corr","spd",2009), mae("resid.pred.corr","fdp",2009), mae("resid.pred.corr","gru",2009), mae("resid.pred.corr","lin",2009),correctWinnerCorr(2009)*100)

# compare tables
benchmark.table.mae <- round(benchmark.table.mae, 3)
benchmark.table.mae.corr <- round(benchmark.table.mae.corr, 3)

compare.table <- benchmark.table.mae >= benchmark.table.mae.corr
compare.table <- ifelse(is.na(compare.table),"",compare.table)
compare.table <- ifelse(compare.table == "TRUE","\\cellcolor{gray!25}",compare.table)
compare.table <- ifelse(compare.table == "FALSE","",compare.table)

# highlight improved cells
benchmark.table.mae.corr.mat <- as.matrix(benchmark.table.mae.corr)
compare.table.mat <- as.matrix(compare.table)
benchmark.table.mae.corr.highlight <- matrix(paste(compare.table.mat, benchmark.table.mae.corr.mat , sep=" "), 
        nrow=nrow(compare.table.mat), dimnames=dimnames(compare.table.mat) )

benchmark.table <- rbind(benchmark.table.mae, benchmark.table.mae.corr.highlight)

table.latex.xtab <- xtable(benchmark.table, digits=3)
caption(table.latex.xtab) <- "Benchmark results for the projection forecasting model, uncorrected and corrected forecasts"
print(table.latex.xtab, sanitize.text.function=function(x){x}, type="latex",table.placement = "t!", caption.placement="top", file="../figures/table.data.benchmarks.projection.tex")





# predicted vs. true plot ---------------------------------------

pvtProjectionPlot <- function(yr) {
pdf(file=paste0("../figures/pvt_projection_", yr, ".pdf"), height=5, width=12, family="URWTimes")
par(mar=c(2,2,2,2), lheight = .8)     # b, l, t, r
par(oma=c(4,4,3,2))
par(mfrow=c(2,5), pty = "s")
### before correction
dat <- filter(wk.results.melt, party == "cdsu" & year == yr)
plot(dat$firstshare, dat$proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(2, at = .35, label = "Before correction", las = 0, tick=F, outer=T, cex.axis=2, line=0)
	axis(3, at = .35, label = "CDU/CSU", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
	abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
	abline(v = mean(dat$proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
dat <- filter(wk.results.melt, party == "spd" & year == yr)
plot(dat$firstshare, dat$proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "SPD", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
dat <- filter(wk.results.melt, party == "fdp" & year == yr)
plot(dat$firstshare, dat$proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "FDP", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
dat <- filter(wk.results.melt, party == "gru" & year == yr)
plot(dat$firstshare, dat$proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "B90/Die Gr?nen", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
dat <- filter(wk.results.melt, party == "lin" & year == yr)
plot(dat$firstshare, dat$proj, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(3, at = .35, label = "Die Linke", las = 0, tick=F, outer=T, cex.axis=2, line=-2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$proj, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
### after correction
dat <- filter(wk.results.melt, party == "cdsu" & year == yr)
plot(dat$firstshare, dat$forecast, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
	axis(2, at = .35, label = "After correction", las = 0, tick=F, outer=T, cex.axis=2, line=0)
	abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
	abline(v = mean(dat$forecast, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at = .35, label = "True values", las = 0, tick=F, outer=T, cex.axis=1.2, line=0)		
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
dat <- filter(wk.results.melt, party == "spd" & year == yr)
plot(dat$firstshare, dat$forecast, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$forecast, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at = .35, label = "True values", las = 0, tick=F, outer=T, cex.axis=1.2, line=0)		
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
dat <- filter(wk.results.melt, party == "fdp" & year == yr)
plot(dat$firstshare, dat$forecast, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$forecast, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at = .35, label = "True values", las = 0, tick=F, outer=T, cex.axis=1.2, line=0)		
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
dat <- filter(wk.results.melt, party == "gru" & year == yr)
plot(dat$firstshare, dat$forecast, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$forecast, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at = .35, label = "True values", las = 0, tick=F, outer=T, cex.axis=1.2, line=0)		
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()	
dat <- filter(wk.results.melt, party == "lin" & year == yr)
plot(dat$firstshare, dat$forecast, ylim=c(0,.7), xlim=c(0,.7), lty=1, lwd=.5, col=rgb(.1,.1,.1,alpha=.5,max=1), pch=16, cex=.7,  main="", ylab="", xlab="", axes=F, cex.main=2)
abline(h = mean(dat$firstshare, na.rm=T), lty = 2)
abline(v = mean(dat$forecast, na.rm=T), lty = 2)
	abline(0,1)
	axis(1, at = .35, label = "True values", las = 0, tick=F, outer=T, cex.axis=1.2, line=0)		
	axis(1, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	axis(2, at=c(0,.2,.4,.6), labels=T, cex.axis=1.2)
	box()
dev.off()	
}

pvtProjectionPlot(yr = 1994)
pvtProjectionPlot(yr = 1998)
pvtProjectionPlot(yr = 2002)
pvtProjectionPlot(yr = 2005)
pvtProjectionPlot(yr = 2009)

