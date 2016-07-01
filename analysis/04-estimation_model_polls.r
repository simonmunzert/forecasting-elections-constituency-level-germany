# Forecasting Elections at the Constituency Level
# Simon Munzert



## preparations -----------------------------------
# load packages
source("packages.r")

# import data
forsa2002.df <- read.dta("../data/polls/forsa2002wk.dta")
forsa2002.df <- forsa2002.df[forsa2002.df$voteint <= 6,] # people with vote intention
forsa2002.df <- forsa2002.df[order(forsa2002.df$wknr2002_sample),] # order data frame by district
forsa2002.0508.df <- forsa2002.df[forsa2002.df$yearmonth >= 200205 & forsa2002.df$yearmonth <= 200208, ]
forsa2002.month.df <- forsa2002.0508.df

forsa2005.df <- read.dta("../data/polls/forsa2005wk.dta")
forsa2005.df <- forsa2005.df[forsa2005.df$voteint <= 6,] 
forsa2005.df <- forsa2005.df[order(forsa2005.df$wknr2005_sample),] 
forsa2005.0508.df <- forsa2005.df[forsa2005.df$yearmonth >= 200505 & forsa2005.df$yearmonth <= 200508, ]
forsa2005.month.df <- forsa2005.0508.df

forsa2009.df <- read.dta("../data/polls/forsa2009wk.dta")
forsa2009.df <- forsa2009.df[forsa2009.df$voteint <= 6,] 
forsa2009.df <- forsa2009.df[order(forsa2009.df$wknr2009_sample),]
forsa2009.0508.df <- forsa2009.df[forsa2009.df$yearmonth >= 200905 & forsa2009.df$yearmonth <= 200908, ]
forsa2009.month.df <- forsa2009.0508.df

forsa2013.df <- read.dta("../data/polls/forsa2013wk.dta")
forsa2013.df <- forsa2013.df[forsa2013.df$voteint <= 6,] 
forsa2013.df <- forsa2013.df[order(forsa2013.df$wknr2013_sample),]
forsa2013.0508.df <- forsa2013.df[forsa2013.df$yearmonth >= 201305 & forsa2013.df$yearmonth <= 201308, ]
forsa2013.month.df <- forsa2013.0508.df




## 2002 model -----------------------------------

# generate number of obs in district 
df.nobs <- data.frame(as.numeric(names(table(forsa2002.month.df$wknr2002_sample))), as.numeric(table(forsa2002.month.df$wknr2002_sample)))
colnames(df.nobs) <- c("WKR_NR", "nobs")

# read in shape file
x <- readShapePoly("../data/geography/GER 2002/Wahlkreise2002.shp")
summary(x)
x$WKR_NR <- x$WKR
x$AREA <- x$Shape_Area

# union districts entailing multiple polygons
gpclibPermit()
sp.districts <- unionSpatialPolygons(x, x$WKR_NR)

# extract area size ,transform to log inverse district area size
dist.size = as.data.frame(cbind(x$AREA/1000000, x$WKR_NR))
colnames(dist.size) <- c("area","WKR_NR")
size.area <- tapply(dist.size$area, dist.size$WKR_NR, sum)
loginvdistsize <- as.numeric(log((size.area)^-1) - (mean(log((size.area)^-1)))) # area size = log inverse district size
df.areasize <- data.frame(c(1:299), loginvdistsize)
colnames(df.areasize) <- c("WKR_NR", "loginvdistsize")
df.area <- merge(df.areasize, df.nobs, by=c("WKR_NR"), all = TRUE)

# create spatial polygon data frame
spdf.area = SpatialPolygonsDataFrame(sp.districts, df.area)
spdf.area <- spdf.area[order(spdf.area$WKR_NR),]  # important: order data frame by WKR_NR
#plot(spdf.area)
#invisible(text(getSpPPolygonsLabptSlots(spdf.area), labels=as.character(round(spdf.area$loginvdistsize, 2)), cex=0.5))

# create list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.area)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")

# dependent variables
y.cdsu <- forsa2002.month.df$cdu
y.spd <- forsa2002.month.df$spd
y.fdp <- forsa2002.month.df$fdp
y.gru <- forsa2002.month.df$gru
y.lin <- forsa2002.month.df$lin

# independent variables
recall.cdsu <- ifelse(forsa2002.month.df$recall == 1, 1, 0)
recall.spd <- ifelse(forsa2002.month.df$recall == 2, 1, 0)
recall.fdp <- ifelse(forsa2002.month.df$recall == 3, 1, 0)
recall.gru <- ifelse(forsa2002.month.df$recall == 4, 1, 0)
recall.lin <- ifelse(forsa2002.month.df$recall == 5, 1, 0)

# number of districts
N <- length(spdf.area)
# number of non-missing districts
ns <- length(spdf.area$nobs[!is.na(spdf.area$nobs)])
# IDs of non-missing districts
s <- spdf.area$WKR_NR[!is.na(spdf.area$nobs)] 
# number of observations per district (non-miss. districts only)
n <- spdf.area$nobs[!is.na(spdf.area$nobs)]
# cumulative no. of obs. per district (non-miss. districts only)
temp.dist.obs <- spdf.area$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)])

# idiosyncratic district effect
u.cdsu = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.spd = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.fdp = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.gru = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.lin = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))


### Winbugs estimation

# data and model
data1 <- list("N", "y.cdsu", "y.spd", "y.fdp", "y.gru", "y.lin", "ns", "s", "n", "cumn", "u.cdsu", "u.spd", "u.fdp", "u.gru", "u.lin", "loginvdistsize", "recall.cdsu", "recall.spd", "recall.fdp", "recall.gru", "recall.lin")
model.bugs <- "bugs_simulations/model.carmrp.txt"

# inits
inits <- source("bugs_simulations/model.carmrp.inits.txt")$value

# parameters to save
parameters.to.save <- source("bugs_simulations/model.carmrp.params.txt")$value

# run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.bugs, inits=inits, parameters.to.save=parameters.to.save, n.chains=3, n.iter=2000, n.burnin=1000, n.thin=1, bugs.directory="C:/Program Files (x86)/WinBUGS14", debug=F) 

# analysis of output
attach.bugs(model1.sim)
plot(model1.sim)
#print(model1.sim, digits=5)

# save model workspace
save(model1.sim, file = "bugs_simulations/modelwk.forsa2002.0508.RData")


### poststratification
load("bugs_simulations/modelwk.forsa2002.0508.RData")
attach(model1.sim)

# prepare data set with poststratification cells
wk.results.df <- read.csv("../data/constituencies/btw1998wkr.csv", header = TRUE, sep=";")
summary(wk.results.df)
wk.results.df$cdsu2share <- (wk.results.df$cdu2 + wk.results.df$csu2) / wk.results.df$waehler
wk.results.df$spd2share <- wk.results.df$spd2 / wk.results.df$waehler
wk.results.df$fdp2share <- wk.results.df$fdp2 / wk.results.df$waehler
wk.results.df$gru2share <- wk.results.df$gru2 / wk.results.df$waehler
wk.results.df$lin2share <- wk.results.df$pds2 / wk.results.df$waehler
wk.results.df$WKR_NR <- as.numeric(wk.results.df$wkr_nr)

# load district panel data set in order to match correct WKR_NR
key.98.02 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_1998_2002.xlsx"),sheet=1) 
key.98.02$wkr_nr1998[is.na(key.98.02$wkr_nr1998)] <- key.98.02$source01_2002[is.na(key.98.02$wkr_nr1998)]
wk.post.key <- data.frame("WKR_NR_new"=key.98.02$wkr_nr2002, "WKR_NR"=key.98.02$wkr_nr1998)
wk.post.key <- wk.post.key[!is.na(wk.post.key$WKR_NR_new),]
wk.post.key <- wk.post.key[order(wk.post.key$WKR_NR_new),]

# merge results data set with new WKR_NR (old results are given over to the new WKR_NR)
df.wk.results <- data.frame(wk.results.df$WKR_NR, wk.results.df$cdsu2share, wk.results.df$spd2share, wk.results.df$fdp2share, wk.results.df$gru2share, wk.results.df$lin2share)
colnames(df.wk.results) <- c("WKR_NR", "cdsu2share", "spd2share", "fdp2share", "gru2share", "lin2share")
df.post <- merge(wk.post.key, df.wk.results, by="WKR_NR", all=T)
df.post <- merge(df.post, df.area, by="WKR_NR", all=T) # add area information
summary(df.post)
df.post$WKR_NR <- NULL
names(df.post)[names(df.post)=="WKR_NR_new"] <- "WKR_NR"
df.post <- df.post[!is.na(df.post$WKR_NR),] # drop WKR which have no successor

df.post.exp <- data.frame(WKR_NR = rep(df.post$WKR_NR, 5), loginvdistsize = rep(df.post$loginvdistsize, 5), cdsu2 = rep(df.post$cdsu2share, 5), spd2 = rep(df.post$spd2share, 5), fdp2 = rep(df.post$fdp2share, 5), gru2 = rep(df.post$gru2share, 5), lin2 = rep(df.post$lin2share, 5), partyshare = rep(NA, 5))
df.post.exp <- df.post.exp[order(df.post.exp$WKR_NR),]
df.post.exp$party <- seq(1,5,1)
df.post.exp$partyshare[df.post.exp$party==1] <- df.post.exp$cdsu2[df.post.exp$party==1]
df.post.exp$partyshare[df.post.exp$party==2] <- df.post.exp$spd2[df.post.exp$party==2]
df.post.exp$partyshare[df.post.exp$party==3] <- df.post.exp$fdp2[df.post.exp$party==3]
df.post.exp$partyshare[df.post.exp$party==4] <- df.post.exp$gru2[df.post.exp$party==4]
df.post.exp$partyshare[df.post.exp$party==5] <- df.post.exp$lin2[df.post.exp$party==5]
df.post.exp$cdsu2 <- ifelse(df.post.exp$party==1, 1, 0)
df.post.exp$spd2 <- ifelse(df.post.exp$party==2, 1, 0)
df.post.exp$fdp2 <- ifelse(df.post.exp$party==3, 1, 0)
df.post.exp$gru2 <- ifelse(df.post.exp$party==4, 1, 0)
df.post.exp$lin2 <- ifelse(df.post.exp$party==5, 1, 0)
df.post.exp$row.names <- NULL

# extract mcmc estimates
beta.cdsu <- model1.sim$sims.list$beta.cdsu
b.cdsu.recall.cdsu <- model1.sim$sims.list$b.cdsu.recall.cdsu
b.cdsu.recall.spd <- model1.sim$sims.list$b.cdsu.recall.spd
b.cdsu.recall.fdp <- model1.sim$sims.list$b.cdsu.recall.fdp
b.cdsu.recall.gru <- model1.sim$sims.list$b.cdsu.recall.gru
b.cdsu.recall.lin <- model1.sim$sims.list$b.cdsu.recall.lin
v.cdsu <- model1.sim$sims.list$v.cdsu
beta.spd <- model1.sim$sims.list$beta.spd
b.spd.recall.cdsu <- model1.sim$sims.list$b.spd.recall.cdsu
b.spd.recall.spd <- model1.sim$sims.list$b.spd.recall.spd
b.spd.recall.fdp <- model1.sim$sims.list$b.spd.recall.fdp
b.spd.recall.gru <- model1.sim$sims.list$b.spd.recall.gru
b.spd.recall.lin <- model1.sim$sims.list$b.spd.recall.lin
v.spd <- model1.sim$sims.list$v.spd
beta.fdp <- model1.sim$sims.list$beta.fdp
b.fdp.recall.cdsu <- model1.sim$sims.list$b.fdp.recall.cdsu
b.fdp.recall.spd <- model1.sim$sims.list$b.fdp.recall.spd
b.fdp.recall.fdp <- model1.sim$sims.list$b.fdp.recall.fdp
b.fdp.recall.gru <- model1.sim$sims.list$b.fdp.recall.gru
b.fdp.recall.lin <- model1.sim$sims.list$b.fdp.recall.lin
v.fdp <- model1.sim$sims.list$v.fdp
beta.gru <- model1.sim$sims.list$beta.gru
b.gru.recall.cdsu <- model1.sim$sims.list$b.gru.recall.cdsu
b.gru.recall.spd <- model1.sim$sims.list$b.gru.recall.spd
b.gru.recall.fdp <- model1.sim$sims.list$b.gru.recall.fdp
b.gru.recall.gru <- model1.sim$sims.list$b.gru.recall.gru
b.gru.recall.lin <- model1.sim$sims.list$b.gru.recall.lin
v.gru <- model1.sim$sims.list$v.gru
beta.lin <- model1.sim$sims.list$beta.lin
b.lin.recall.cdsu <- model1.sim$sims.list$b.lin.recall.cdsu
b.lin.recall.spd <- model1.sim$sims.list$b.lin.recall.spd
b.lin.recall.fdp <- model1.sim$sims.list$b.lin.recall.fdp
b.lin.recall.gru <- model1.sim$sims.list$b.lin.recall.gru
b.lin.recall.lin <- model1.sim$sims.list$b.lin.recall.lin
v.lin <- model1.sim$sims.list$v.lin

# fill n.sims x n.cells sheet with predictions
L <- nrow(df.post.exp)
mu.l.cdsu <- array(NA,c(n.sims,L))
mu.l.spd <- array(NA,c(n.sims,L))
mu.l.fdp <- array(NA,c(n.sims,L))
mu.l.gru <- array(NA,c(n.sims,L))
mu.l.lin <- array(NA,c(n.sims,L))
for (l in 1:L){
mu.l.cdsu[,l] <- inv.logit(beta.cdsu[,df.post.exp$WKR_NR[l]] + b.cdsu.recall.cdsu*df.post.exp$cdsu2[l] + b.cdsu.recall.spd*df.post.exp$spd2[l] + b.cdsu.recall.fdp*df.post.exp$fdp2[l] + b.cdsu.recall.gru*df.post.exp$gru2[l] + b.cdsu.recall.lin*df.post.exp$lin2[l] + v.cdsu[,df.post.exp$WKR_NR[l]])
mu.l.spd[,l] <- inv.logit(beta.spd[,df.post.exp$WKR_NR[l]] + b.spd.recall.cdsu*df.post.exp$cdsu2[l] + b.spd.recall.spd*df.post.exp$spd2[l] + b.spd.recall.fdp*df.post.exp$fdp2[l] + b.spd.recall.gru*df.post.exp$gru2[l] + b.spd.recall.lin*df.post.exp$lin2[l] + v.spd[,df.post.exp$WKR_NR[l]])
mu.l.fdp[,l] <- inv.logit(beta.fdp[,df.post.exp$WKR_NR[l]] + b.fdp.recall.cdsu*df.post.exp$cdsu2[l] + b.fdp.recall.spd*df.post.exp$fdp2[l] + b.fdp.recall.fdp*df.post.exp$fdp2[l] + b.fdp.recall.gru*df.post.exp$gru2[l] + b.fdp.recall.lin*df.post.exp$lin2[l] + v.fdp[,df.post.exp$WKR_NR[l]])
mu.l.gru[,l] <- inv.logit(beta.gru[,df.post.exp$WKR_NR[l]] + b.gru.recall.cdsu*df.post.exp$cdsu2[l] + b.gru.recall.spd*df.post.exp$gru2[l] + b.gru.recall.fdp*df.post.exp$gru2[l] + b.gru.recall.gru*df.post.exp$gru2[l] + b.gru.recall.lin*df.post.exp$lin2[l] + v.gru[,df.post.exp$WKR_NR[l]])
mu.l.lin[,l] <- inv.logit(beta.lin[,df.post.exp$WKR_NR[l]] + b.lin.recall.cdsu*df.post.exp$cdsu2[l] + b.lin.recall.spd*df.post.exp$lin2[l] + b.lin.recall.fdp*df.post.exp$lin2[l] + b.lin.recall.gru*df.post.exp$gru2[l] + b.lin.recall.lin*df.post.exp$lin2[l] + v.lin[,df.post.exp$WKR_NR[l]])
}

# weight by population shares and collapse to district level: "multiply" each row of the predicted matrix with partyshare vector
mu.w.cdsu <- sweep(mu.l.cdsu,MARGIN=2,df.post.exp$partyshare,`*`)
mu.w.spd <- sweep(mu.l.spd,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.fdp <- sweep(mu.l.fdp,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.gru <- sweep(mu.l.gru,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.lin <- sweep(mu.l.lin,MARGIN=2,df.post.exp$partyshare,`*`) 

# collapse the cells to district level
mu.j.cdsu <- array(NA, c(n.sims, N))
mu.j.spd <- array(NA, c(n.sims, N))
mu.j.fdp <- array(NA, c(n.sims, N))
mu.j.gru <- array(NA, c(n.sims, N))
mu.j.lin <- array(NA, c(n.sims, N))
for (i in 1:n.sims) {
mu.j.cdsu[i,] <- tapply(mu.w.cdsu[i,], df.post.exp$WKR_NR, sum)
mu.j.spd[i,] <- tapply(mu.w.spd[i,], df.post.exp$WKR_NR, sum)
mu.j.fdp[i,] <- tapply(mu.w.fdp[i,], df.post.exp$WKR_NR, sum)
mu.j.gru[i,] <- tapply(mu.w.gru[i,], df.post.exp$WKR_NR, sum)
mu.j.lin[i,] <- tapply(mu.w.lin[i,], df.post.exp$WKR_NR, sum)
}

# median SAEs plus SDs and 95%-CIs
wk.cdsu.mu <- 0
wk.cdsu.sd <- 0
wk.cdsu.90lo <- 0
wk.cdsu.90hi <- 0
wk.spd.mu <- 0
wk.spd.sd <- 0
wk.spd.90lo <- 0
wk.spd.90hi <- 0
wk.fdp.mu <- 0
wk.fdp.sd <- 0
wk.fdp.90lo <- 0
wk.fdp.90hi <- 0
wk.gru.mu <- 0
wk.gru.sd <- 0
wk.gru.90lo <- 0
wk.gru.90hi <- 0
wk.lin.mu <- 0
wk.lin.sd <- 0
wk.lin.90lo <- 0
wk.lin.90hi <- 0
for (i in 1:N){
wk.cdsu.mu[i] <- median(mu.j.cdsu[,i])
wk.cdsu.sd[i] <- sd(mu.j.cdsu[,i])
wk.cdsu.90lo[i] <- quantile(mu.j.cdsu[,i],probs=c(.05), na.rm=T)
wk.cdsu.90hi[i] <- quantile(mu.j.cdsu[,i],probs=c(.95), na.rm=T)
wk.spd.mu[i] <- median(mu.j.spd[,i])
wk.spd.sd[i] <- sd(mu.j.spd[,i])
wk.spd.90lo[i] <- quantile(mu.j.spd[,i],probs=c(.05), na.rm=T)
wk.spd.90hi[i] <- quantile(mu.j.spd[,i],probs=c(.95), na.rm=T)
wk.fdp.mu[i] <- median(mu.j.fdp[,i])
wk.fdp.sd[i] <- sd(mu.j.fdp[,i])
wk.fdp.90lo[i] <- quantile(mu.j.fdp[,i],probs=c(.05), na.rm=T)
wk.fdp.90hi[i] <- quantile(mu.j.fdp[,i],probs=c(.95), na.rm=T)
wk.gru.mu[i] <- median(mu.j.gru[,i])
wk.gru.sd[i] <- sd(mu.j.gru[,i])
wk.gru.90lo[i] <- quantile(mu.j.gru[,i],probs=c(.05), na.rm=T)
wk.gru.90hi[i] <- quantile(mu.j.gru[,i],probs=c(.95), na.rm=T)
wk.lin.mu[i] <- median(mu.j.lin[,i])
wk.lin.sd[i] <- sd(mu.j.lin[,i])
wk.lin.90lo[i] <- quantile(mu.j.lin[,i],probs=c(.05), na.rm=T)
wk.lin.90hi[i] <- quantile(mu.j.lin[,i],probs=c(.95), na.rm=T)
}

# load election results
wk.results.true.df <- read.csv("../data/constituencies/btw2002wkr.csv", header = TRUE, sep=";")
summary(wk.results.true.df)
wk.results.true.df$cdsu1share <- (wk.results.true.df$cdu1 + wk.results.true.df$csu1) / wk.results.true.df$zweitstimmegueltig
wk.results.true.df$spd1share <- wk.results.true.df$spd1 / wk.results.true.df$waehler
wk.results.true.df$fdp1share <- wk.results.true.df$fdp1 / wk.results.true.df$waehler
wk.results.true.df$gru1share <- wk.results.true.df$gru1 / wk.results.true.df$waehler
wk.results.true.df$lin1share <- wk.results.true.df$pds1 / wk.results.true.df$waehler
wk.results.true.df$cdsu2share <- (wk.results.true.df$cdu2 + wk.results.true.df$csu2) / wk.results.true.df$zweitstimmegueltig
wk.results.true.df$spd2share <- wk.results.true.df$spd2 / wk.results.true.df$waehler
wk.results.true.df$fdp2share <- wk.results.true.df$fdp2 / wk.results.true.df$waehler
wk.results.true.df$gru2share <- wk.results.true.df$gru2 / wk.results.true.df$waehler
wk.results.true.df$lin2share <- wk.results.true.df$pds2 / wk.results.true.df$waehler
wk.results.true.df$WKR_NR <- as.numeric(wk.results.true.df$wkr_nr)

# save poststratification results
wk.estimates.2002.0508 <- list(
mu.j.cdsu, mu.j.spd, mu.j.fdp, mu.j.gru, mu.j.lin, 
wk.cdsu.mu, wk.spd.mu, wk.fdp.mu, wk.gru.mu, wk.lin.mu, 
wk.cdsu.90lo, wk.spd.90lo, wk.fdp.90lo, wk.gru.90lo, wk.lin.90lo,
wk.cdsu.90hi, wk.spd.90hi, wk.fdp.90hi, wk.gru.90hi, wk.lin.90hi,
wk.results.true.df
)
names(wk.estimates.2002.0508) <- c("mu.j.cdsu", "mu.j.spd", "mu.j.fdp", "mu.j.gru", "mu.j.lin", 
"wk.cdsu.mu", "wk.spd.mu", "wk.fdp.mu", "wk.gru.mu", "wk.lin.mu", 
"wk.cdsu.90lo", "wk.spd.90lo", "wk.fdp.90lo", "wk.gru.90lo", "wk.lin.90lo",
"wk.cdsu.90hi", "wk.spd.90hi", "wk.fdp.90hi", "wk.gru.90hi", "wk.lin.90hi",
"wk.results.true.df")
save(wk.estimates.2002.0508, file="bugs_simulations/modelwkpost.forsa2002.0508.RData")


# election results vs estimates
pollname <- "2002_0508"
pdf(file=paste("../figures/pvt_polls_", pollname, ".pdf", sep=""), height=4, width=16, family="URWTimes")
par(mar=c(3,5,2,1))
par(oma=c(2,2,2,1))
par(mfcol=c(1,5), pty = "s")
#cdsu
plotCI(wk.cdsu.mu, wk.results.true.df$cdsu1share, ui=wk.cdsu.90hi, li=wk.cdsu.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="Election vote share", main="CDU/CSU", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$cdsu1share~wk.cdsu.mu), col="blue")
#spd
plotCI(wk.spd.mu, wk.results.true.df$spd1share, ui=wk.spd.90hi, li=wk.spd.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="SPD", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$spd1share~wk.spd.mu), col="blue")
#fdp
plotCI(wk.fdp.mu, wk.results.true.df$fdp1share, ui=wk.fdp.90hi, li=wk.fdp.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="FDP", cex.lab=2, cex.main=2, cex.axis=1.5)
axis(3,at = .35, label = pollname, cex.axis=2, line=2, tick=F)
abline(0,1)
abline(lm(wk.results.true.df$fdp1share~wk.fdp.mu), col="blue")
#gru
plotCI(wk.gru.mu, wk.results.true.df$gru1share, ui=wk.gru.90hi, li=wk.gru.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="B90/Die Gr?nen", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$gru1share~wk.gru.mu), col="blue")
#lin
plotCI(wk.lin.mu, wk.results.true.df$lin1share, ui=wk.lin.90hi, li=wk.lin.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="Die Linke", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$lin1share~wk.lin.mu), col="blue")
dev.off()





## 2005 model -----------------------------------

# number of observations
df.nobs <- data.frame(as.numeric(names(table(forsa2005.month.df$wknr2005_sample))), as.numeric(table(forsa2005.month.df$wknr2005_sample)))
colnames(df.nobs) <- c("WKR_NR", "nobs")

# read in shape file
x <- readShapePoly("../data/geography/GER 2005/Geometrie_Wahlkreise_16DBT.shp")

# union districts entailing multiple polygons
gpclibPermit()
sp.districts <- unionSpatialPolygons(x, x$WKR_NR)

# extract area size ,transform to log inverse district area size
dist.size = as.data.frame(cbind(x$AREA/1000000, x$WKR_NR))
colnames(dist.size) <- c("area","WKR_NR")
size.area <- tapply(dist.size$area, dist.size$WKR_NR, sum)
loginvdistsize <- as.numeric(log((size.area)^-1) - (mean(log((size.area)^-1)))) # area size = log inverse district size
df.areasize <- data.frame(c(1:299), loginvdistsize)
colnames(df.areasize) <- c("WKR_NR", "loginvdistsize")
df.area <- merge(df.areasize, df.nobs, by=c("WKR_NR"), all = TRUE)

# create spatial polygon data frame
spdf.area = SpatialPolygonsDataFrame(sp.districts, df.area)
spdf.area <- spdf.area[order(spdf.area$WKR_NR),]  # important: order data frame by WKR_NR
#plot(spdf.area)
#invisible(text(getSpPPolygonsLabptSlots(spdf.area), labels=as.character(round(spdf.area$loginvdistsize, 2)), cex=0.5))

# create list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.area)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")

# dependent variables
y.cdsu <- forsa2005.month.df$cdu
y.spd <- forsa2005.month.df$spd
y.fdp <- forsa2005.month.df$fdp
y.gru <- forsa2005.month.df$gru
y.lin <- forsa2005.month.df$lin

# independent variables
recall.cdsu <- ifelse(forsa2005.month.df$recall == 1, 1, 0)
recall.spd <- ifelse(forsa2005.month.df$recall == 2, 1, 0)
recall.fdp <- ifelse(forsa2005.month.df$recall == 3, 1, 0)
recall.gru <- ifelse(forsa2005.month.df$recall == 4, 1, 0)
recall.lin <- ifelse(forsa2005.month.df$recall == 5, 1, 0)

# number of districts
N <- length(spdf.area)
# number of non-missing districts
ns <- length(spdf.area$nobs[!is.na(spdf.area$nobs)])
# IDs of non-missing districts
s <- spdf.area$WKR_NR[!is.na(spdf.area$nobs)] 
# number of observations per district (non-miss. districts only)
n <- spdf.area$nobs[!is.na(spdf.area$nobs)]
# cumulative no. of obs. per district (non-miss. districts only)
temp.dist.obs <- spdf.area$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)])

# idiosyncratic district effect
u.cdsu = ifelse(!is.na(spdf.area$nobs), NA, 0)
u.spd = ifelse(!is.na(spdf.area$nobs), NA, 0)
u.fdp = ifelse(!is.na(spdf.area$nobs), NA, 0)
u.gru = ifelse(!is.na(spdf.area$nobs), NA, 0)
u.lin = ifelse(!is.na(spdf.area$nobs), NA, 0)


### Winbugs estimation

# data and model
data1 <- list("N", "y.cdsu", "y.spd", "y.fdp", "y.gru", "y.lin", "ns", "s", "n", "cumn", "u.cdsu", "u.spd", "u.fdp", "u.gru", "u.lin", "loginvdistsize", "recall.cdsu", "recall.spd", "recall.fdp", "recall.gru", "recall.lin")
model.bugs <- "bugs_simulations/model.carmrp.txt"

# inits
inits <- source("bugs_simulations/model.carmrp.inits.txt")$value

# parameters to save
parameters.to.save <- source("bugs_simulations/model.carmrp.params.txt")$value

# run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.bugs, inits=inits, parameters.to.save=parameters.to.save, n.chains=3, n.iter=2000, n.burnin=1000, n.thin=1, bugs.directory="C:/Program Files (x86)/WinBUGS14", debug=T) 

# analysis of output
attach.bugs(model1.sim)
plot(model1.sim)
#print(model1.sim, digits=5)

# save model workspace
save(model1.sim, file = "bugs_simulations/modelwk.forsa2005.0508.RData")



### poststratification
load("bugs_simulations/modelwk.forsa2005.0508.RData")
attach(model1.sim)

# prepare data set with poststratification cells
wk.results.df <- read.csv("../data/constituencies/btw2002wkr.csv", header = TRUE, sep=";")
summary(wk.results.df)
wk.results.df$cdsu2share <- (wk.results.df$cdu2 + wk.results.df$csu2) / wk.results.df$waehler
wk.results.df$spd2share <- wk.results.df$spd2 / wk.results.df$waehler
wk.results.df$fdp2share <- wk.results.df$fdp2 / wk.results.df$waehler
wk.results.df$gru2share <- wk.results.df$gru2 / wk.results.df$waehler
wk.results.df$lin2share <- wk.results.df$pds2 / wk.results.df$waehler
wk.results.df$WKR_NR <- as.numeric(wk.results.df$wkr_nr)

# load district panel data set in order to match correct WKR_NR
key.02.05 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_2002_2005.xlsx"),sheet=1) 
key.02.05$wkr_nr2002[is.na(key.02.05$wkr_nr2002)] <- key.02.05$source01_2005[is.na(key.02.05$wkr_nr2002)]
wk.post.key <- data.frame("WKR_NR_new"=key.02.05$wkr_nr2005, "WKR_NR"=key.02.05$wkr_nr2002)
wk.post.key <- wk.post.key[!is.na(wk.post.key$WKR_NR_new),]
wk.post.key <- wk.post.key[order(wk.post.key$WKR_NR_new),]

# merge results data set with new WKR_NR (old results are given over to the new WKR_NR)
df.wk.results <- data.frame(wk.results.df$WKR_NR, wk.results.df$cdsu2share, wk.results.df$spd2share, wk.results.df$fdp2share, wk.results.df$gru2share, wk.results.df$lin2share)
colnames(df.wk.results) <- c("WKR_NR", "cdsu2share", "spd2share", "fdp2share", "gru2share", "lin2share")
df.post <- merge(wk.post.key, df.wk.results, by="WKR_NR", all=T)
df.post <- merge(df.post, df.area, by="WKR_NR", all=T) # add area information
summary(df.post)
df.post$WKR_NR <- NULL
df.post.exp <- data.frame(WKR_NR = rep(df.post$WKR_NR, 5), loginvdistsize = rep(df.post$loginvdistsize, 5), cdsu2 = rep(df.post$cdsu2share, 5), spd2 = rep(df.post$spd2share, 5), fdp2 = rep(df.post$fdp2share, 5), gru2 = rep(df.post$gru2share, 5), lin2 = rep(df.post$lin2share, 5), partyshare = rep(NA, 5))
df.post.exp <- df.post.exp[order(df.post.exp$WKR_NR),]
df.post.exp$party <- seq(1,5,1)
df.post.exp$partyshare[df.post.exp$party==1] <- df.post.exp$cdsu2[df.post.exp$party==1]
df.post.exp$partyshare[df.post.exp$party==2] <- df.post.exp$spd2[df.post.exp$party==2]
df.post.exp$partyshare[df.post.exp$party==3] <- df.post.exp$fdp2[df.post.exp$party==3]
df.post.exp$partyshare[df.post.exp$party==4] <- df.post.exp$gru2[df.post.exp$party==4]
df.post.exp$partyshare[df.post.exp$party==5] <- df.post.exp$lin2[df.post.exp$party==5]
df.post.exp$cdsu2 <- ifelse(df.post.exp$party==1, 1, 0)
df.post.exp$spd2 <- ifelse(df.post.exp$party==2, 1, 0)
df.post.exp$fdp2 <- ifelse(df.post.exp$party==3, 1, 0)
df.post.exp$gru2 <- ifelse(df.post.exp$party==4, 1, 0)
df.post.exp$lin2 <- ifelse(df.post.exp$party==5, 1, 0)
df.post.exp$row.names <- NULL

names(df.post)[names(df.post)=="WKR_NR_new"] <- "WKR_NR"
df.post <- df.post[!is.na(df.post$WKR_NR),] # drop WKR which have no successor

# extract mcmc estimates
beta.cdsu <- model1.sim$sims.list$beta.cdsu
b.cdsu.recall.cdsu <- model1.sim$sims.list$b.cdsu.recall.cdsu
b.cdsu.recall.spd <- model1.sim$sims.list$b.cdsu.recall.spd
b.cdsu.recall.fdp <- model1.sim$sims.list$b.cdsu.recall.fdp
b.cdsu.recall.gru <- model1.sim$sims.list$b.cdsu.recall.gru
b.cdsu.recall.lin <- model1.sim$sims.list$b.cdsu.recall.lin
v.cdsu <- model1.sim$sims.list$v.cdsu
beta.spd <- model1.sim$sims.list$beta.spd
b.spd.recall.cdsu <- model1.sim$sims.list$b.spd.recall.cdsu
b.spd.recall.spd <- model1.sim$sims.list$b.spd.recall.spd
b.spd.recall.fdp <- model1.sim$sims.list$b.spd.recall.fdp
b.spd.recall.gru <- model1.sim$sims.list$b.spd.recall.gru
b.spd.recall.lin <- model1.sim$sims.list$b.spd.recall.lin
v.spd <- model1.sim$sims.list$v.spd
beta.fdp <- model1.sim$sims.list$beta.fdp
b.fdp.recall.cdsu <- model1.sim$sims.list$b.fdp.recall.cdsu
b.fdp.recall.spd <- model1.sim$sims.list$b.fdp.recall.spd
b.fdp.recall.fdp <- model1.sim$sims.list$b.fdp.recall.fdp
b.fdp.recall.gru <- model1.sim$sims.list$b.fdp.recall.gru
b.fdp.recall.lin <- model1.sim$sims.list$b.fdp.recall.lin
v.fdp <- model1.sim$sims.list$v.fdp
beta.gru <- model1.sim$sims.list$beta.gru
b.gru.recall.cdsu <- model1.sim$sims.list$b.gru.recall.cdsu
b.gru.recall.spd <- model1.sim$sims.list$b.gru.recall.spd
b.gru.recall.fdp <- model1.sim$sims.list$b.gru.recall.fdp
b.gru.recall.gru <- model1.sim$sims.list$b.gru.recall.gru
b.gru.recall.lin <- model1.sim$sims.list$b.gru.recall.lin
v.gru <- model1.sim$sims.list$v.gru
beta.lin <- model1.sim$sims.list$beta.lin
b.lin.recall.cdsu <- model1.sim$sims.list$b.lin.recall.cdsu
b.lin.recall.spd <- model1.sim$sims.list$b.lin.recall.spd
b.lin.recall.fdp <- model1.sim$sims.list$b.lin.recall.fdp
b.lin.recall.gru <- model1.sim$sims.list$b.lin.recall.gru
b.lin.recall.lin <- model1.sim$sims.list$b.lin.recall.lin
v.lin <- model1.sim$sims.list$v.lin

# fill n.sims x n.cells sheet with predictions
L <- nrow(df.post.exp)
mu.l.cdsu <- array(NA,c(n.sims,L))
mu.l.spd <- array(NA,c(n.sims,L))
mu.l.fdp <- array(NA,c(n.sims,L))
mu.l.gru <- array(NA,c(n.sims,L))
mu.l.lin <- array(NA,c(n.sims,L))
for (l in 1:L){
  mu.l.cdsu[,l] <- inv.logit(beta.cdsu[,df.post.exp$WKR_NR[l]] + b.cdsu.recall.cdsu*df.post.exp$cdsu2[l] + b.cdsu.recall.spd*df.post.exp$spd2[l] + b.cdsu.recall.fdp*df.post.exp$fdp2[l] + b.cdsu.recall.gru*df.post.exp$gru2[l] + b.cdsu.recall.lin*df.post.exp$lin2[l] + v.cdsu[,df.post.exp$WKR_NR[l]])
  mu.l.spd[,l] <- inv.logit(beta.spd[,df.post.exp$WKR_NR[l]] + b.spd.recall.cdsu*df.post.exp$cdsu2[l] + b.spd.recall.spd*df.post.exp$spd2[l] + b.spd.recall.fdp*df.post.exp$fdp2[l] + b.spd.recall.gru*df.post.exp$gru2[l] + b.spd.recall.lin*df.post.exp$lin2[l] + v.spd[,df.post.exp$WKR_NR[l]])
  mu.l.fdp[,l] <- inv.logit(beta.fdp[,df.post.exp$WKR_NR[l]] + b.fdp.recall.cdsu*df.post.exp$cdsu2[l] + b.fdp.recall.spd*df.post.exp$fdp2[l] + b.fdp.recall.fdp*df.post.exp$fdp2[l] + b.fdp.recall.gru*df.post.exp$gru2[l] + b.fdp.recall.lin*df.post.exp$lin2[l] + v.fdp[,df.post.exp$WKR_NR[l]])
  mu.l.gru[,l] <- inv.logit(beta.gru[,df.post.exp$WKR_NR[l]] + b.gru.recall.cdsu*df.post.exp$cdsu2[l] + b.gru.recall.spd*df.post.exp$gru2[l] + b.gru.recall.fdp*df.post.exp$gru2[l] + b.gru.recall.gru*df.post.exp$gru2[l] + b.gru.recall.lin*df.post.exp$lin2[l] + v.gru[,df.post.exp$WKR_NR[l]])
  mu.l.lin[,l] <- inv.logit(beta.lin[,df.post.exp$WKR_NR[l]] + b.lin.recall.cdsu*df.post.exp$cdsu2[l] + b.lin.recall.spd*df.post.exp$lin2[l] + b.lin.recall.fdp*df.post.exp$lin2[l] + b.lin.recall.gru*df.post.exp$gru2[l] + b.lin.recall.lin*df.post.exp$lin2[l] + v.lin[,df.post.exp$WKR_NR[l]])
}

# weight by population shares and collapse to district level: "multiply" each row of the predicted matrix with partyshare vector
mu.w.cdsu <- sweep(mu.l.cdsu,MARGIN=2,df.post.exp$partyshare,`*`)
mu.w.spd <- sweep(mu.l.spd,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.fdp <- sweep(mu.l.fdp,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.gru <- sweep(mu.l.gru,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.lin <- sweep(mu.l.lin,MARGIN=2,df.post.exp$partyshare,`*`) 

# collapse the cells to district level
mu.j.cdsu <- array(NA, c(n.sims, N))
mu.j.spd <- array(NA, c(n.sims, N))
mu.j.fdp <- array(NA, c(n.sims, N))
mu.j.gru <- array(NA, c(n.sims, N))
mu.j.lin <- array(NA, c(n.sims, N))
for (i in 1:n.sims) {
  mu.j.cdsu[i,] <- tapply(mu.w.cdsu[i,], df.post.exp$WKR_NR, sum)
  mu.j.spd[i,] <- tapply(mu.w.spd[i,], df.post.exp$WKR_NR, sum)
  mu.j.fdp[i,] <- tapply(mu.w.fdp[i,], df.post.exp$WKR_NR, sum)
  mu.j.gru[i,] <- tapply(mu.w.gru[i,], df.post.exp$WKR_NR, sum)
  mu.j.lin[i,] <- tapply(mu.w.lin[i,], df.post.exp$WKR_NR, sum)
}


# median SAEs plus SDs and 95%-CIs
wk.cdsu.mu <- 0
wk.cdsu.sd <- 0
wk.cdsu.90lo <- 0
wk.cdsu.90hi <- 0
wk.spd.mu <- 0
wk.spd.sd <- 0
wk.spd.90lo <- 0
wk.spd.90hi <- 0
wk.fdp.mu <- 0
wk.fdp.sd <- 0
wk.fdp.90lo <- 0
wk.fdp.90hi <- 0
wk.gru.mu <- 0
wk.gru.sd <- 0
wk.gru.90lo <- 0
wk.gru.90hi <- 0
wk.lin.mu <- 0
wk.lin.sd <- 0
wk.lin.90lo <- 0
wk.lin.90hi <- 0
for (i in 1:N){
  wk.cdsu.mu[i] <- median(mu.j.cdsu[,i])
  wk.cdsu.sd[i] <- sd(mu.j.cdsu[,i])
  wk.cdsu.90lo[i] <- quantile(mu.j.cdsu[,i],probs=c(.05), na.rm=T)
  wk.cdsu.90hi[i] <- quantile(mu.j.cdsu[,i],probs=c(.95), na.rm=T)
  wk.spd.mu[i] <- median(mu.j.spd[,i])
  wk.spd.sd[i] <- sd(mu.j.spd[,i])
  wk.spd.90lo[i] <- quantile(mu.j.spd[,i],probs=c(.05), na.rm=T)
  wk.spd.90hi[i] <- quantile(mu.j.spd[,i],probs=c(.95), na.rm=T)
  wk.fdp.mu[i] <- median(mu.j.fdp[,i])
  wk.fdp.sd[i] <- sd(mu.j.fdp[,i])
  wk.fdp.90lo[i] <- quantile(mu.j.fdp[,i],probs=c(.05), na.rm=T)
  wk.fdp.90hi[i] <- quantile(mu.j.fdp[,i],probs=c(.95), na.rm=T)
  wk.gru.mu[i] <- median(mu.j.gru[,i])
  wk.gru.sd[i] <- sd(mu.j.gru[,i])
  wk.gru.90lo[i] <- quantile(mu.j.gru[,i],probs=c(.05), na.rm=T)
  wk.gru.90hi[i] <- quantile(mu.j.gru[,i],probs=c(.95), na.rm=T)
  wk.lin.mu[i] <- median(mu.j.lin[,i])
  wk.lin.sd[i] <- sd(mu.j.lin[,i])
  wk.lin.90lo[i] <- quantile(mu.j.lin[,i],probs=c(.05), na.rm=T)
  wk.lin.90hi[i] <- quantile(mu.j.lin[,i],probs=c(.95), na.rm=T)
}

# load election results
wk.results.true.df <- read.csv("../data/constituencies/btw2005wkr.csv", header = TRUE, sep=";")
summary(wk.results.true.df)
wk.results.true.df$cdsu1share <- (wk.results.true.df$cdu1 + wk.results.true.df$csu1) / wk.results.true.df$zweitstimmegueltig
wk.results.true.df$spd1share <- wk.results.true.df$spd1 / wk.results.true.df$waehler
wk.results.true.df$fdp1share <- wk.results.true.df$fdp1 / wk.results.true.df$waehler
wk.results.true.df$gru1share <- wk.results.true.df$gru1 / wk.results.true.df$waehler
wk.results.true.df$lin1share <- wk.results.true.df$pds1 / wk.results.true.df$waehler
wk.results.true.df$cdsu2share <- (wk.results.true.df$cdu2 + wk.results.true.df$csu2) / wk.results.true.df$zweitstimmegueltig
wk.results.true.df$spd2share <- wk.results.true.df$spd2 / wk.results.true.df$waehler
wk.results.true.df$fdp2share <- wk.results.true.df$fdp2 / wk.results.true.df$waehler
wk.results.true.df$gru2share <- wk.results.true.df$gru2 / wk.results.true.df$waehler
wk.results.true.df$lin2share <- wk.results.true.df$pds2 / wk.results.true.df$waehler
wk.results.true.df$WKR_NR <- as.numeric(wk.results.true.df$wkr_nr)


# save poststratification results
wk.estimates.2005.0508 <- list(
  mu.j.cdsu, mu.j.spd, mu.j.fdp, mu.j.gru, mu.j.lin, 
  wk.cdsu.mu, wk.spd.mu, wk.fdp.mu, wk.gru.mu, wk.lin.mu, 
  wk.cdsu.90lo, wk.spd.90lo, wk.fdp.90lo, wk.gru.90lo, wk.lin.90lo,
  wk.cdsu.90hi, wk.spd.90hi, wk.fdp.90hi, wk.gru.90hi, wk.lin.90hi,
  wk.results.true.df
)
names(wk.estimates.2005.0508) <- c("mu.j.cdsu", "mu.j.spd", "mu.j.fdp", "mu.j.gru", "mu.j.lin", 
                                   "wk.cdsu.mu", "wk.spd.mu", "wk.fdp.mu", "wk.gru.mu", "wk.lin.mu", 
                                   "wk.cdsu.90lo", "wk.spd.90lo", "wk.fdp.90lo", "wk.gru.90lo", "wk.lin.90lo",
                                   "wk.cdsu.90hi", "wk.spd.90hi", "wk.fdp.90hi", "wk.gru.90hi", "wk.lin.90hi",
                                   "wk.results.true.df")
save(wk.estimates.2005.0508, file="bugs_simulations/modelwkpost.forsa2005.0508.RData")


# election results vs estimates
pollname <- "2005.0508"
pdf(file=paste("../figures/pvt_polls_", pollname, ".pdf", sep=""), height=4, width=16, family="URWTimes")
par(mar=c(3,5,2,1))
par(oma=c(2,2,2,1))
par(mfcol=c(1,5), pty = "s")
#cdsu
plotCI(wk.cdsu.mu, wk.results.true.df$cdsu1share, ui=wk.cdsu.90hi, li=wk.cdsu.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="Election vote share", main="CDU/CSU", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$cdsu1share~wk.cdsu.mu), col="blue")
#spd
plotCI(wk.spd.mu, wk.results.true.df$spd1share, ui=wk.spd.90hi, li=wk.spd.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="SPD", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$spd1share~wk.spd.mu), col="blue")
#fdp
plotCI(wk.fdp.mu, wk.results.true.df$fdp1share, ui=wk.fdp.90hi, li=wk.fdp.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="FDP", cex.lab=2, cex.main=2, cex.axis=1.5)
axis(3,at = .35, label = pollname, cex.axis=2, line=2, tick=F)
abline(0,1)
abline(lm(wk.results.true.df$fdp1share~wk.fdp.mu), col="blue")
#gru
plotCI(wk.gru.mu, wk.results.true.df$gru1share, ui=wk.gru.90hi, li=wk.gru.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="B90/Die Gr?nen", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$gru1share~wk.gru.mu), col="blue")
#lin
plotCI(wk.lin.mu, wk.results.true.df$lin1share, ui=wk.lin.90hi, li=wk.lin.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="Die Linke", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$lin1share~wk.lin.mu), col="blue")
dev.off()





## 2009 model -----------------------------------

# number of observations
df.nobs <- data.frame(as.numeric(names(table(forsa2009.month.df$wknr2009_sample))), as.numeric(table(forsa2009.month.df$wknr2009_sample)))
colnames(df.nobs) <- c("WKR_NR", "nobs")
summary(df.nobs$nobs)

# read in shape file
x <- readShapePoly("../data/geography/GER 2009/Geometrie_Wahlkreise_17DBT.shp")
summary(x)

# union districts entailing multiple polygons
gpclibPermit()
sp.districts <- unionSpatialPolygons(x, x$WKR_NR)

# extract area size ,transform to log inverse district area size
dist.size = as.data.frame(cbind(x$AREA/1000000, x$WKR_NR))
colnames(dist.size) <- c("area","WKR_NR")
size.area <- tapply(dist.size$area, dist.size$WKR_NR, sum)
loginvdistsize <- as.numeric(log((size.area)^-1) - (mean(log((size.area)^-1)))) # area size = log inverse district size
df.areasize <- data.frame(c(1:299), loginvdistsize)
colnames(df.areasize) <- c("WKR_NR", "loginvdistsize")
df.area <- merge(df.areasize, df.nobs, by=c("WKR_NR"), all = TRUE)

# create spatial polygon data frame
spdf.area = SpatialPolygonsDataFrame(sp.districts, df.area)
spdf.area <- spdf.area[order(spdf.area$WKR_NR),]  # important: order data frame by WKR_NR
#plot(spdf.area)
#invisible(text(getSpPPolygonsLabptSlots(spdf.area), labels=as.character(round(spdf.area$loginvdistsize, 2)), cex=0.5))

# create list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.area)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")

# dependent variables
y.cdsu <- forsa2009.month.df$cdu
y.spd <- forsa2009.month.df$spd
y.fdp <- forsa2009.month.df$fdp
y.gru <- forsa2009.month.df$gru
y.lin <- forsa2009.month.df$lin

# independent variables
recall.cdsu <- ifelse(forsa2009.month.df$recall == 1, 1, 0)
recall.spd <- ifelse(forsa2009.month.df$recall == 2, 1, 0)
recall.fdp <- ifelse(forsa2009.month.df$recall == 3, 1, 0)
recall.gru <- ifelse(forsa2009.month.df$recall == 4, 1, 0)
recall.lin <- ifelse(forsa2009.month.df$recall == 5, 1, 0)

# number of districts
N <- length(spdf.area)
# number of non-missing districts
ns <- length(spdf.area$nobs[!is.na(spdf.area$nobs)])
# IDs of non-missing districts
s <- spdf.area$WKR_NR[!is.na(spdf.area$nobs)] 
# number of observations per district (non-miss. districts only)
n <- spdf.area$nobs[!is.na(spdf.area$nobs)]
# cumulative no. of obs. per district (non-miss. districts only)
temp.dist.obs <- spdf.area$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)])

# idiosyncratic district effect
u.cdsu = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.spd = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.fdp = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.gru = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.lin = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))

### Winbugs estimation

# data and model
data1 <- list("N", "y.cdsu", "y.spd", "y.fdp", "y.gru", "y.lin", "ns", "s", "n", "cumn", "u.cdsu", "u.spd", "u.fdp", "u.gru", "u.lin", "loginvdistsize", "recall.cdsu", "recall.spd", "recall.fdp", "recall.gru", "recall.lin")
model.bugs <- "bugs_simulations/model.carmrp.txt"

# inits
inits <- source("bugs_simulations/model.carmrp.inits.txt")$value

# parameters to save
parameters.to.save <- source("bugs_simulations/model.carmrp.params.txt")$value

# run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.bugs, inits=inits, parameters.to.save=parameters.to.save, n.chains=3, n.iter=2000, n.burnin=1000, n.thin=1, bugs.directory="C:/Program Files (x86)/WinBUGS14", debug=T) 

# analysis of output
attach.bugs(model1.sim)
plot(model1.sim)
#print(model1.sim, digits=5)

# save model workspace
save(model1.sim, file = "bugs_simulations/modelwk.forsa2009.0508.RData")


### poststratification
load("bugs_simulations/modelwk.forsa2009.0508.RData")
attach(model1.sim)

# prepare data set with poststratification cells
wk.results.df <- read.csv("../data/constituencies/btw2005wkr.csv", header = TRUE, sep=";")
summary(wk.results.df)
wk.results.df$cdsu2share <- (wk.results.df$cdu2 + wk.results.df$csu2) / wk.results.df$waehler
wk.results.df$spd2share <- wk.results.df$spd2 / wk.results.df$waehler
wk.results.df$fdp2share <- wk.results.df$fdp2 / wk.results.df$waehler
wk.results.df$gru2share <- wk.results.df$gru2 / wk.results.df$waehler
wk.results.df$lin2share <- wk.results.df$pds2 / wk.results.df$waehler
wk.results.df$WKR_NR <- as.numeric(wk.results.df$wkr_nr)

# load district panel data set in order to match correct WKR_NR
key.05.09 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_2005_2009.xlsx"),sheet=1) 
key.05.09$wkr_nr2005[is.na(key.05.09$wkr_nr2005)] <- key.05.09$source01_2009[is.na(key.05.09$wkr_nr2005)]
wk.post.key <- data.frame("WKR_NR_new"=key.05.09$wkr_nr2009, "WKR_NR"=key.05.09$wkr_nr2005)
wk.post.key <- wk.post.key[!is.na(wk.post.key$WKR_NR_new),]
wk.post.key <- wk.post.key[order(wk.post.key$WKR_NR_new),]

# merge results data set with new WKR_NR (old results are given over to the new WKR_NR)
df.wk.results <- data.frame(wk.results.df$WKR_NR, wk.results.df$cdsu2share, wk.results.df$spd2share, wk.results.df$fdp2share, wk.results.df$gru2share, wk.results.df$lin2share)
colnames(df.wk.results) <- c("WKR_NR", "cdsu2share", "spd2share", "fdp2share", "gru2share", "lin2share")
df.post <- merge(wk.post.key, df.wk.results, by="WKR_NR", all=T)
df.post <- merge(df.post, df.area, by="WKR_NR", all=T) # add area information
summary(df.post)
df.post$WKR_NR <- NULL
names(df.post)[names(df.post)=="WKR_NR_new"] <- "WKR_NR"
df.post <- df.post[!is.na(df.post$WKR_NR),] # drop WKR which have no successor

df.post.exp <- data.frame(WKR_NR = rep(df.post$WKR_NR, 5), loginvdistsize = rep(df.post$loginvdistsize, 5), cdsu2 = rep(df.post$cdsu2share, 5), spd2 = rep(df.post$spd2share, 5), fdp2 = rep(df.post$fdp2share, 5), gru2 = rep(df.post$gru2share, 5), lin2 = rep(df.post$lin2share, 5), partyshare = rep(NA, 5))
df.post.exp <- df.post.exp[order(df.post.exp$WKR_NR),]
df.post.exp$party <- seq(1,5,1)
df.post.exp$partyshare[df.post.exp$party==1] <- df.post.exp$cdsu2[df.post.exp$party==1]
df.post.exp$partyshare[df.post.exp$party==2] <- df.post.exp$spd2[df.post.exp$party==2]
df.post.exp$partyshare[df.post.exp$party==3] <- df.post.exp$fdp2[df.post.exp$party==3]
df.post.exp$partyshare[df.post.exp$party==4] <- df.post.exp$gru2[df.post.exp$party==4]
df.post.exp$partyshare[df.post.exp$party==5] <- df.post.exp$lin2[df.post.exp$party==5]
df.post.exp$cdsu2 <- ifelse(df.post.exp$party==1, 1, 0)
df.post.exp$spd2 <- ifelse(df.post.exp$party==2, 1, 0)
df.post.exp$fdp2 <- ifelse(df.post.exp$party==3, 1, 0)
df.post.exp$gru2 <- ifelse(df.post.exp$party==4, 1, 0)
df.post.exp$lin2 <- ifelse(df.post.exp$party==5, 1, 0)
df.post.exp$row.names <- NULL

# extract mcmc estimates
beta.cdsu <- model1.sim$sims.list$beta.cdsu
b.cdsu.recall.cdsu <- model1.sim$sims.list$b.cdsu.recall.cdsu
b.cdsu.recall.spd <- model1.sim$sims.list$b.cdsu.recall.spd
b.cdsu.recall.fdp <- model1.sim$sims.list$b.cdsu.recall.fdp
b.cdsu.recall.gru <- model1.sim$sims.list$b.cdsu.recall.gru
b.cdsu.recall.lin <- model1.sim$sims.list$b.cdsu.recall.lin
v.cdsu <- model1.sim$sims.list$v.cdsu
beta.spd <- model1.sim$sims.list$beta.spd
b.spd.recall.cdsu <- model1.sim$sims.list$b.spd.recall.cdsu
b.spd.recall.spd <- model1.sim$sims.list$b.spd.recall.spd
b.spd.recall.fdp <- model1.sim$sims.list$b.spd.recall.fdp
b.spd.recall.gru <- model1.sim$sims.list$b.spd.recall.gru
b.spd.recall.lin <- model1.sim$sims.list$b.spd.recall.lin
v.spd <- model1.sim$sims.list$v.spd
beta.fdp <- model1.sim$sims.list$beta.fdp
b.fdp.recall.cdsu <- model1.sim$sims.list$b.fdp.recall.cdsu
b.fdp.recall.spd <- model1.sim$sims.list$b.fdp.recall.spd
b.fdp.recall.fdp <- model1.sim$sims.list$b.fdp.recall.fdp
b.fdp.recall.gru <- model1.sim$sims.list$b.fdp.recall.gru
b.fdp.recall.lin <- model1.sim$sims.list$b.fdp.recall.lin
v.fdp <- model1.sim$sims.list$v.fdp
beta.gru <- model1.sim$sims.list$beta.gru
b.gru.recall.cdsu <- model1.sim$sims.list$b.gru.recall.cdsu
b.gru.recall.spd <- model1.sim$sims.list$b.gru.recall.spd
b.gru.recall.fdp <- model1.sim$sims.list$b.gru.recall.fdp
b.gru.recall.gru <- model1.sim$sims.list$b.gru.recall.gru
b.gru.recall.lin <- model1.sim$sims.list$b.gru.recall.lin
v.gru <- model1.sim$sims.list$v.gru
beta.lin <- model1.sim$sims.list$beta.lin
b.lin.recall.cdsu <- model1.sim$sims.list$b.lin.recall.cdsu
b.lin.recall.spd <- model1.sim$sims.list$b.lin.recall.spd
b.lin.recall.fdp <- model1.sim$sims.list$b.lin.recall.fdp
b.lin.recall.gru <- model1.sim$sims.list$b.lin.recall.gru
b.lin.recall.lin <- model1.sim$sims.list$b.lin.recall.lin
v.lin <- model1.sim$sims.list$v.lin

# fill n.sims x n.cells sheet with predictions
L <- nrow(df.post.exp)
mu.l.cdsu <- array(NA,c(n.sims,L))
mu.l.spd <- array(NA,c(n.sims,L))
mu.l.fdp <- array(NA,c(n.sims,L))
mu.l.gru <- array(NA,c(n.sims,L))
mu.l.lin <- array(NA,c(n.sims,L))
for (l in 1:L){
  mu.l.cdsu[,l] <- inv.logit(beta.cdsu[,df.post.exp$WKR_NR[l]] + b.cdsu.recall.cdsu*df.post.exp$cdsu2[l] + b.cdsu.recall.spd*df.post.exp$spd2[l] + b.cdsu.recall.fdp*df.post.exp$fdp2[l] + b.cdsu.recall.gru*df.post.exp$gru2[l] + b.cdsu.recall.lin*df.post.exp$lin2[l] + v.cdsu[,df.post.exp$WKR_NR[l]])
  mu.l.spd[,l] <- inv.logit(beta.spd[,df.post.exp$WKR_NR[l]] + b.spd.recall.cdsu*df.post.exp$cdsu2[l] + b.spd.recall.spd*df.post.exp$spd2[l] + b.spd.recall.fdp*df.post.exp$fdp2[l] + b.spd.recall.gru*df.post.exp$gru2[l] + b.spd.recall.lin*df.post.exp$lin2[l] + v.spd[,df.post.exp$WKR_NR[l]])
  mu.l.fdp[,l] <- inv.logit(beta.fdp[,df.post.exp$WKR_NR[l]] + b.fdp.recall.cdsu*df.post.exp$cdsu2[l] + b.fdp.recall.spd*df.post.exp$fdp2[l] + b.fdp.recall.fdp*df.post.exp$fdp2[l] + b.fdp.recall.gru*df.post.exp$gru2[l] + b.fdp.recall.lin*df.post.exp$lin2[l] + v.fdp[,df.post.exp$WKR_NR[l]])
  mu.l.gru[,l] <- inv.logit(beta.gru[,df.post.exp$WKR_NR[l]] + b.gru.recall.cdsu*df.post.exp$cdsu2[l] + b.gru.recall.spd*df.post.exp$gru2[l] + b.gru.recall.fdp*df.post.exp$gru2[l] + b.gru.recall.gru*df.post.exp$gru2[l] + b.gru.recall.lin*df.post.exp$lin2[l] + v.gru[,df.post.exp$WKR_NR[l]])
  mu.l.lin[,l] <- inv.logit(beta.lin[,df.post.exp$WKR_NR[l]] + b.lin.recall.cdsu*df.post.exp$cdsu2[l] + b.lin.recall.spd*df.post.exp$lin2[l] + b.lin.recall.fdp*df.post.exp$lin2[l] + b.lin.recall.gru*df.post.exp$gru2[l] + b.lin.recall.lin*df.post.exp$lin2[l] + v.lin[,df.post.exp$WKR_NR[l]])
}

# weight by population shares and collapse to district level: "multiply" each row of the predicted matrix with partyshare vector
mu.w.cdsu <- sweep(mu.l.cdsu,MARGIN=2,df.post.exp$partyshare,`*`)
mu.w.spd <- sweep(mu.l.spd,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.fdp <- sweep(mu.l.fdp,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.gru <- sweep(mu.l.gru,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.lin <- sweep(mu.l.lin,MARGIN=2,df.post.exp$partyshare,`*`) 

# collapse the cells to district level
mu.j.cdsu <- array(NA, c(n.sims, N))
mu.j.spd <- array(NA, c(n.sims, N))
mu.j.fdp <- array(NA, c(n.sims, N))
mu.j.gru <- array(NA, c(n.sims, N))
mu.j.lin <- array(NA, c(n.sims, N))
for (i in 1:n.sims) {
  mu.j.cdsu[i,] <- tapply(mu.w.cdsu[i,], df.post.exp$WKR_NR, sum)
  mu.j.spd[i,] <- tapply(mu.w.spd[i,], df.post.exp$WKR_NR, sum)
  mu.j.fdp[i,] <- tapply(mu.w.fdp[i,], df.post.exp$WKR_NR, sum)
  mu.j.gru[i,] <- tapply(mu.w.gru[i,], df.post.exp$WKR_NR, sum)
  mu.j.lin[i,] <- tapply(mu.w.lin[i,], df.post.exp$WKR_NR, sum)
}


# median SAEs plus SDs and 95%-CIs
wk.cdsu.mu <- 0
wk.cdsu.sd <- 0
wk.cdsu.90lo <- 0
wk.cdsu.90hi <- 0
wk.spd.mu <- 0
wk.spd.sd <- 0
wk.spd.90lo <- 0
wk.spd.90hi <- 0
wk.fdp.mu <- 0
wk.fdp.sd <- 0
wk.fdp.90lo <- 0
wk.fdp.90hi <- 0
wk.gru.mu <- 0
wk.gru.sd <- 0
wk.gru.90lo <- 0
wk.gru.90hi <- 0
wk.lin.mu <- 0
wk.lin.sd <- 0
wk.lin.90lo <- 0
wk.lin.90hi <- 0
for (i in 1:N){
  wk.cdsu.mu[i] <- median(mu.j.cdsu[,i])
  wk.cdsu.sd[i] <- sd(mu.j.cdsu[,i])
  wk.cdsu.90lo[i] <- quantile(mu.j.cdsu[,i],probs=c(.05), na.rm=T)
  wk.cdsu.90hi[i] <- quantile(mu.j.cdsu[,i],probs=c(.95), na.rm=T)
  wk.spd.mu[i] <- median(mu.j.spd[,i])
  wk.spd.sd[i] <- sd(mu.j.spd[,i])
  wk.spd.90lo[i] <- quantile(mu.j.spd[,i],probs=c(.05), na.rm=T)
  wk.spd.90hi[i] <- quantile(mu.j.spd[,i],probs=c(.95), na.rm=T)
  wk.fdp.mu[i] <- median(mu.j.fdp[,i])
  wk.fdp.sd[i] <- sd(mu.j.fdp[,i])
  wk.fdp.90lo[i] <- quantile(mu.j.fdp[,i],probs=c(.05), na.rm=T)
  wk.fdp.90hi[i] <- quantile(mu.j.fdp[,i],probs=c(.95), na.rm=T)
  wk.gru.mu[i] <- median(mu.j.gru[,i])
  wk.gru.sd[i] <- sd(mu.j.gru[,i])
  wk.gru.90lo[i] <- quantile(mu.j.gru[,i],probs=c(.05), na.rm=T)
  wk.gru.90hi[i] <- quantile(mu.j.gru[,i],probs=c(.95), na.rm=T)
  wk.lin.mu[i] <- median(mu.j.lin[,i])
  wk.lin.sd[i] <- sd(mu.j.lin[,i])
  wk.lin.90lo[i] <- quantile(mu.j.lin[,i],probs=c(.05), na.rm=T)
  wk.lin.90hi[i] <- quantile(mu.j.lin[,i],probs=c(.95), na.rm=T)
}

# load election results
wk.results.true.df <- read.csv("../data/constituencies/btw2009wkr.csv", header = TRUE, sep=";")
summary(wk.results.true.df)
wk.results.true.df$cdsu1share <- (wk.results.true.df$cdu1 + wk.results.true.df$csu1) / wk.results.true.df$zweitstimmegueltig
wk.results.true.df$spd1share <- wk.results.true.df$spd1 / wk.results.true.df$waehler
wk.results.true.df$fdp1share <- wk.results.true.df$fdp1 / wk.results.true.df$waehler
wk.results.true.df$gru1share <- wk.results.true.df$gru1 / wk.results.true.df$waehler
wk.results.true.df$lin1share <- wk.results.true.df$pds1 / wk.results.true.df$waehler
wk.results.true.df$cdsu2share <- (wk.results.true.df$cdu2 + wk.results.true.df$csu2) / wk.results.true.df$zweitstimmegueltig
wk.results.true.df$spd2share <- wk.results.true.df$spd2 / wk.results.true.df$waehler
wk.results.true.df$fdp2share <- wk.results.true.df$fdp2 / wk.results.true.df$waehler
wk.results.true.df$gru2share <- wk.results.true.df$gru2 / wk.results.true.df$waehler
wk.results.true.df$lin2share <- wk.results.true.df$pds2 / wk.results.true.df$waehler
wk.results.true.df$WKR_NR <- as.numeric(wk.results.true.df$wkr_nr)



# save poststratification results
wk.estimates.2009.0508 <- list(
  mu.j.cdsu, mu.j.spd, mu.j.fdp, mu.j.gru, mu.j.lin, 
  wk.cdsu.mu, wk.spd.mu, wk.fdp.mu, wk.gru.mu, wk.lin.mu, 
  wk.cdsu.90lo, wk.spd.90lo, wk.fdp.90lo, wk.gru.90lo, wk.lin.90lo,
  wk.cdsu.90hi, wk.spd.90hi, wk.fdp.90hi, wk.gru.90hi, wk.lin.90hi,
  wk.results.true.df
)
names(wk.estimates.2009.0508) <- c("mu.j.cdsu", "mu.j.spd", "mu.j.fdp", "mu.j.gru", "mu.j.lin", 
                                   "wk.cdsu.mu", "wk.spd.mu", "wk.fdp.mu", "wk.gru.mu", "wk.lin.mu", 
                                   "wk.cdsu.90lo", "wk.spd.90lo", "wk.fdp.90lo", "wk.gru.90lo", "wk.lin.90lo",
                                   "wk.cdsu.90hi", "wk.spd.90hi", "wk.fdp.90hi", "wk.gru.90hi", "wk.lin.90hi",
                                   "wk.results.true.df")
save(wk.estimates.2009.0508, file="bugs_simulations/modelwkpost.forsa2009.0508.RData")



# election results vs estimates
pollname <- "2009_0508"
pdf(file=paste("../figures/pvt_polls_", pollname, ".pdf", sep=""), height=4, width=16, family="URWTimes")
par(mar=c(3,5,2,1))
par(oma=c(2,2,2,1))
par(mfcol=c(1,5), pty = "s")
#cdsu
plotCI(wk.cdsu.mu, wk.results.true.df$cdsu1share, ui=wk.cdsu.90hi, li=wk.cdsu.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="Election vote share", main="CDU/CSU", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$cdsu1share~wk.cdsu.mu), col="blue")
#spd
plotCI(wk.spd.mu, wk.results.true.df$spd1share, ui=wk.spd.90hi, li=wk.spd.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="SPD", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$spd1share~wk.spd.mu), col="blue")
#fdp
plotCI(wk.fdp.mu, wk.results.true.df$fdp1share, ui=wk.fdp.90hi, li=wk.fdp.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="FDP", cex.lab=2, cex.main=2, cex.axis=1.5)
axis(3,at = .35, label = pollname, cex.axis=2, line=2, tick=F)
abline(0,1)
abline(lm(wk.results.true.df$fdp1share~wk.fdp.mu), col="blue")
#gru
plotCI(wk.gru.mu, wk.results.true.df$gru1share, ui=wk.gru.90hi, li=wk.gru.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="B90/Die Gr?nen", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$gru1share~wk.gru.mu), col="blue")
#lin
plotCI(wk.lin.mu, wk.results.true.df$lin1share, ui=wk.lin.90hi, li=wk.lin.90lo, err="x", barcol="darkgrey", gap=0, sfrac=0, pch=1, xlim = c(0,.7), ylim = c(0,.7), xlab="District poll estimate", ylab="", main="Die Linke", cex.lab=2, cex.main=2, cex.axis=1.5)
abline(0,1)
abline(lm(wk.results.true.df$lin1share~wk.lin.mu), col="blue")
dev.off()




## 2013 model -----------------------------------

# generate number of obs in district 
df.nobs <- data.frame(as.numeric(names(table(forsa2013.month.df$wknr2013_sample))), as.numeric(table(forsa2013.month.df$wknr2013_sample)))
colnames(df.nobs) <- c("WKR_NR", "nobs")


### summary statistics, raw survey data
# no. of cases per study
forsa2013.df.list <- list(forsa2013.01.df, forsa2013.02.df, forsa2013.03.df, forsa2013.04.df, forsa2013.05.df, forsa2013.06.df, forsa2013.07.df, forsa2013.08.df, forsa2013.09.df)
(nobs <- sapply(forsa2013.df.list, nrow))
# total coverage
coverage <- vector()
for (i in 1:length(forsa2013.df.list)) {
  coverage[i] <- length(unique(forsa2013.df.list[[i]]$wknr2013_sample))
}
# average, minimum, maximum number of respondents per district
mean.obs <- vector()
sd.obs <- vector()
min.obs <- vector()
max.obs <- vector()
for (i in 1:length(forsa2013.df.list)) {
  df.nobs <- data.frame(as.numeric(names(table(forsa2013.df.list[[i]]$wknr2013_sample))), as.numeric(table(forsa2013.df.list[[i]]$wknr2013_sample)))
  colnames(df.nobs) <- c("WKR_NR", "nobs")
  mean.obs[i] <- mean(df.nobs$nobs)
  sd.obs[i] <- sd(df.nobs$nobs)
  min.obs[i] <- min(df.nobs$nobs)
  max.obs[i] <- max(df.nobs$nobs)
}
# generate latex table
table.latex <- data.frame(nobs, coverage, mean.obs, sd.obs, min.obs, max.obs)
rownames(table.latex) <- c("01/2013", "02/2013", "03/2013", "04/2013", "05/2013", "06/2013", "07/2013", "08/2013", "09/2013")
colnames(table.latex) <- c("$N$","J", "mean($N_{j}$)","sd(N_j)","min(N_j)","max(N_j)")
table.latex.xtab <- xtable(table.latex, digits=0)
caption(table.latex.xtab) <- "Summary statistics of raw poll data"
print(table.latex.xtab, type="latex",table.placement = "t!", caption.placement="top", file="table2013.forsa.tex")


### prepare data

# number of observations
df.nobs <- data.frame(as.numeric(names(table(forsa2013.month.df$wknr2013_sample))), as.numeric(table(forsa2013.month.df$wknr2013_sample)))
colnames(df.nobs) <- c("WKR_NR", "nobs")
summary(df.nobs$nobs)

# read in shape file
x <- readShapePoly("C:/Users/SMunzert/Documents/Dropbox/Uni/Dissertation/Data/Germany Federal Election 2013/Geography/GER 2013/Geometrie_Wahlkreise_18DBT.shp")
summary(x)

# union districts entailing multiple polygons
gpclibPermit()
sp.districts <- unionSpatialPolygons(x, x$WKR_NR)

# extract area size ,transform to log inverse district area size
x$AREA <- sapply(slot(x, "polygons"), slot, "area") 
dist.size = as.data.frame(cbind(x$AREA/1000000, x$WKR_NR))
colnames(dist.size) <- c("area","WKR_NR")
size.area <- tapply(dist.size$area, dist.size$WKR_NR, sum)
loginvdistsize <- as.numeric(log((size.area)^-1) - (mean(log((size.area)^-1)))) # area size = log inverse district size
df.areasize <- data.frame(c(1:299), loginvdistsize)
colnames(df.areasize) <- c("WKR_NR", "loginvdistsize")
df.area <- merge(df.areasize, df.nobs, by=c("WKR_NR"), all = TRUE)

# create spatial polygon data frame
spdf.area = SpatialPolygonsDataFrame(sp.districts, df.area)
spdf.area <- spdf.area[order(spdf.area$WKR_NR),]  # important: order data frame by WKR_NR
#plot(spdf.area)
#invisible(text(getSpPPolygonsLabptSlots(spdf.area), labels=as.character(round(spdf.area$loginvdistsize, 2)), cex=0.5))

# create list of neighbors for WinBUGS
nb.districts <- poly2nb(spdf.area)
nb <- unlist(nb.districts)
weight <- rep(1, times=length(nb))
num <- card(nb.districts)
spdata <- list("nb","weight", "num")

# dependent variables
y.cdsu <- forsa2013.month.df$cdu
y.spd <- forsa2013.month.df$spd
y.fdp <- forsa2013.month.df$fdp
y.gru <- forsa2013.month.df$gru
y.lin <- forsa2013.month.df$lin

# independent variables
recall.cdsu <- ifelse(forsa2013.month.df$recall == 1, 1, 0)
recall.spd <- ifelse(forsa2013.month.df$recall == 2, 1, 0)
recall.fdp <- ifelse(forsa2013.month.df$recall == 3, 1, 0)
recall.gru <- ifelse(forsa2013.month.df$recall == 4, 1, 0)
recall.lin <- ifelse(forsa2013.month.df$recall == 5, 1, 0)

# number of districts
N <- length(spdf.area)
# number of non-missing districts
ns <- length(spdf.area$nobs[!is.na(spdf.area$nobs)])
# IDs of non-missing districts
s <- spdf.area$WKR_NR[!is.na(spdf.area$nobs)] 
# number of observations per district (non-miss. districts only)
n <- spdf.area$nobs[!is.na(spdf.area$nobs)]
# cumulative no. of obs. per district (non-miss. districts only)
temp.dist.obs <- spdf.area$nobs
temp.dist.obs <- temp.dist.obs[(is.na(temp.dist.obs))==F]
temp.cumn <-  cumsum(temp.dist.obs)
cumn <- c(0,temp.cumn[1:(length(temp.cumn)-1)])

# idiosyncratic district effect
u.cdsu = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.spd = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.fdp = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.gru = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))
u.lin = as.numeric(ifelse(!is.na(spdf.area$nobs), NA, 0))

### Winbugs estimation

# data and model
data1 <- list("N", "y.cdsu", "y.spd", "y.fdp", "y.gru", "y.lin", "ns", "s", "n", "cumn", "u.cdsu", "u.spd", "u.fdp", "u.gru", "u.lin", "loginvdistsize", "recall.cdsu", "recall.spd", "recall.fdp", "recall.gru", "recall.lin")
model.bugs <- "C:/Users/SMunzert/Documents/Dropbox/winbugsmodelcode/model.carmrp.txt"

# inits
inits <- source("C:/Users/SMunzert/Documents/Dropbox/winbugsmodelcode/model.carmrp.inits.txt")$value

# parameters to save
parameters.to.save <- source("C:/Users/SMunzert/Documents/Dropbox/winbugsmodelcode/model.carmrp.params.txt")$value

# run MCMC
model1.sim <- bugs(data=c(data1, spdata), model.file=model.bugs, inits=inits, parameters.to.save=parameters.to.save, n.chains=3, n.iter=2000, n.burnin=1000, n.thin=1, bugs.directory="C:/Program Files (x86)/WinBUGS14", debug=T) 

# analysis of output
attach.bugs(model1.sim)
plot(model1.sim)
#print(model1.sim, digits=5)

# save model workspace
save(model1.sim, file = "modelwk.forsa2013.0508.RData")


### poststratification
load("modelwk.forsa2013.0508.RData")
attach(model1.sim)

# prepare data set with poststratification cells
wk.results.df <- read.csv("C:/Users/SMunzert/Documents/Dropbox/Uni/Dissertation/Data/Germany Federal Election 2013/Constituencies/Results/btw2009wkr.csv", header = TRUE, sep=";")
summary(wk.results.df)
wk.results.df$cdsu2share <- (wk.results.df$cdu2 + wk.results.df$csu2) / wk.results.df$waehler
wk.results.df$spd2share <- wk.results.df$spd2 / wk.results.df$waehler
wk.results.df$fdp2share <- wk.results.df$fdp2 / wk.results.df$waehler
wk.results.df$gru2share <- wk.results.df$gru2 / wk.results.df$waehler
wk.results.df$lin2share <- wk.results.df$pds2 / wk.results.df$waehler
wk.results.df$WKR_NR <- as.numeric(wk.results.df$wkr_nr)

# load district panel data set in order to match correct WKR_NR
key.09.13 <- readWorksheet(loadWorkbook("~/Dropbox/Uni/Dissertation/Data/Germany Federal Election 2013/Constituencies/wkrkey_2009_2013.xlsx"),sheet=1) 
key.09.13$wkr_nr2009[is.na(key.09.13$wkr_nr2009)] <- key.09.13$source01_2013[is.na(key.09.13$wkr_nr2009)]
wk.post.key <- data.frame("WKR_NR_new"=key.09.13$wkr_nr2013, "WKR_NR"=key.09.13$wkr_nr2009)
wk.post.key <- wk.post.key[!is.na(wk.post.key$WKR_NR_new),]
wk.post.key <- wk.post.key[order(wk.post.key$WKR_NR_new),]

# merge results data set with new WKR_NR (old results are given over to the new WKR_NR)
df.wk.results <- data.frame(wk.results.df$WKR_NR, wk.results.df$cdsu2share, wk.results.df$spd2share, wk.results.df$fdp2share, wk.results.df$gru2share, wk.results.df$lin2share)
colnames(df.wk.results) <- c("WKR_NR", "cdsu2share", "spd2share", "fdp2share", "gru2share", "lin2share")
df.post <- merge(wk.post.key, df.wk.results, by="WKR_NR", all=T)
df.post <- merge(df.post, df.area, by="WKR_NR", all=T) # add area information
summary(df.post)
df.post$WKR_NR <- NULL
names(df.post)[names(df.post)=="WKR_NR_new"] <- "WKR_NR"
df.post <- df.post[!is.na(df.post$WKR_NR),] # drop WKR which have no successor

df.post.exp <- data.frame(WKR_NR = rep(df.post$WKR_NR, 5), loginvdistsize = rep(df.post$loginvdistsize, 5), cdsu2 = rep(df.post$cdsu2share, 5), spd2 = rep(df.post$spd2share, 5), fdp2 = rep(df.post$fdp2share, 5), gru2 = rep(df.post$gru2share, 5), lin2 = rep(df.post$lin2share, 5), partyshare = rep(NA, 5))
df.post.exp <- df.post.exp[order(df.post.exp$WKR_NR),]
df.post.exp$party <- seq(1,5,1)
df.post.exp$partyshare[df.post.exp$party==1] <- df.post.exp$cdsu2[df.post.exp$party==1]
df.post.exp$partyshare[df.post.exp$party==2] <- df.post.exp$spd2[df.post.exp$party==2]
df.post.exp$partyshare[df.post.exp$party==3] <- df.post.exp$fdp2[df.post.exp$party==3]
df.post.exp$partyshare[df.post.exp$party==4] <- df.post.exp$gru2[df.post.exp$party==4]
df.post.exp$partyshare[df.post.exp$party==5] <- df.post.exp$lin2[df.post.exp$party==5]
df.post.exp$cdsu2 <- ifelse(df.post.exp$party==1, 1, 0)
df.post.exp$spd2 <- ifelse(df.post.exp$party==2, 1, 0)
df.post.exp$fdp2 <- ifelse(df.post.exp$party==3, 1, 0)
df.post.exp$gru2 <- ifelse(df.post.exp$party==4, 1, 0)
df.post.exp$lin2 <- ifelse(df.post.exp$party==5, 1, 0)
df.post.exp$row.names <- NULL

# extract mcmc estimates
beta.cdsu <- model1.sim$sims.list$beta.cdsu
b.cdsu.recall.cdsu <- model1.sim$sims.list$b.cdsu.recall.cdsu
b.cdsu.recall.spd <- model1.sim$sims.list$b.cdsu.recall.spd
b.cdsu.recall.fdp <- model1.sim$sims.list$b.cdsu.recall.fdp
b.cdsu.recall.gru <- model1.sim$sims.list$b.cdsu.recall.gru
b.cdsu.recall.lin <- model1.sim$sims.list$b.cdsu.recall.lin
v.cdsu <- model1.sim$sims.list$v.cdsu
beta.spd <- model1.sim$sims.list$beta.spd
b.spd.recall.cdsu <- model1.sim$sims.list$b.spd.recall.cdsu
b.spd.recall.spd <- model1.sim$sims.list$b.spd.recall.spd
b.spd.recall.fdp <- model1.sim$sims.list$b.spd.recall.fdp
b.spd.recall.gru <- model1.sim$sims.list$b.spd.recall.gru
b.spd.recall.lin <- model1.sim$sims.list$b.spd.recall.lin
v.spd <- model1.sim$sims.list$v.spd
beta.fdp <- model1.sim$sims.list$beta.fdp
b.fdp.recall.cdsu <- model1.sim$sims.list$b.fdp.recall.cdsu
b.fdp.recall.spd <- model1.sim$sims.list$b.fdp.recall.spd
b.fdp.recall.fdp <- model1.sim$sims.list$b.fdp.recall.fdp
b.fdp.recall.gru <- model1.sim$sims.list$b.fdp.recall.gru
b.fdp.recall.lin <- model1.sim$sims.list$b.fdp.recall.lin
v.fdp <- model1.sim$sims.list$v.fdp
beta.gru <- model1.sim$sims.list$beta.gru
b.gru.recall.cdsu <- model1.sim$sims.list$b.gru.recall.cdsu
b.gru.recall.spd <- model1.sim$sims.list$b.gru.recall.spd
b.gru.recall.fdp <- model1.sim$sims.list$b.gru.recall.fdp
b.gru.recall.gru <- model1.sim$sims.list$b.gru.recall.gru
b.gru.recall.lin <- model1.sim$sims.list$b.gru.recall.lin
v.gru <- model1.sim$sims.list$v.gru
beta.lin <- model1.sim$sims.list$beta.lin
b.lin.recall.cdsu <- model1.sim$sims.list$b.lin.recall.cdsu
b.lin.recall.spd <- model1.sim$sims.list$b.lin.recall.spd
b.lin.recall.fdp <- model1.sim$sims.list$b.lin.recall.fdp
b.lin.recall.gru <- model1.sim$sims.list$b.lin.recall.gru
b.lin.recall.lin <- model1.sim$sims.list$b.lin.recall.lin
v.lin <- model1.sim$sims.list$v.lin

# fill n.sims x n.cells sheet with predictions
L <- nrow(df.post.exp)
mu.l.cdsu <- array(NA,c(n.sims,L))
mu.l.spd <- array(NA,c(n.sims,L))
mu.l.fdp <- array(NA,c(n.sims,L))
mu.l.gru <- array(NA,c(n.sims,L))
mu.l.lin <- array(NA,c(n.sims,L))
for (l in 1:L){
  mu.l.cdsu[,l] <- inv.logit(beta.cdsu[,df.post.exp$WKR_NR[l]] + b.cdsu.recall.cdsu*df.post.exp$cdsu2[l] + b.cdsu.recall.spd*df.post.exp$spd2[l] + b.cdsu.recall.fdp*df.post.exp$fdp2[l] + b.cdsu.recall.gru*df.post.exp$gru2[l] + b.cdsu.recall.lin*df.post.exp$lin2[l] + v.cdsu[,df.post.exp$WKR_NR[l]])
  mu.l.spd[,l] <- inv.logit(beta.spd[,df.post.exp$WKR_NR[l]] + b.spd.recall.cdsu*df.post.exp$cdsu2[l] + b.spd.recall.spd*df.post.exp$spd2[l] + b.spd.recall.fdp*df.post.exp$fdp2[l] + b.spd.recall.gru*df.post.exp$gru2[l] + b.spd.recall.lin*df.post.exp$lin2[l] + v.spd[,df.post.exp$WKR_NR[l]])
  mu.l.fdp[,l] <- inv.logit(beta.fdp[,df.post.exp$WKR_NR[l]] + b.fdp.recall.cdsu*df.post.exp$cdsu2[l] + b.fdp.recall.spd*df.post.exp$fdp2[l] + b.fdp.recall.fdp*df.post.exp$fdp2[l] + b.fdp.recall.gru*df.post.exp$gru2[l] + b.fdp.recall.lin*df.post.exp$lin2[l] + v.fdp[,df.post.exp$WKR_NR[l]])
  mu.l.gru[,l] <- inv.logit(beta.gru[,df.post.exp$WKR_NR[l]] + b.gru.recall.cdsu*df.post.exp$cdsu2[l] + b.gru.recall.spd*df.post.exp$gru2[l] + b.gru.recall.fdp*df.post.exp$gru2[l] + b.gru.recall.gru*df.post.exp$gru2[l] + b.gru.recall.lin*df.post.exp$lin2[l] + v.gru[,df.post.exp$WKR_NR[l]])
  mu.l.lin[,l] <- inv.logit(beta.lin[,df.post.exp$WKR_NR[l]] + b.lin.recall.cdsu*df.post.exp$cdsu2[l] + b.lin.recall.spd*df.post.exp$lin2[l] + b.lin.recall.fdp*df.post.exp$lin2[l] + b.lin.recall.gru*df.post.exp$gru2[l] + b.lin.recall.lin*df.post.exp$lin2[l] + v.lin[,df.post.exp$WKR_NR[l]])
}

# weight by population shares and collapse to district level: "multiply" each row of the predicted matrix with partyshare vector
mu.w.cdsu <- sweep(mu.l.cdsu,MARGIN=2,df.post.exp$partyshare,`*`)
mu.w.spd <- sweep(mu.l.spd,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.fdp <- sweep(mu.l.fdp,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.gru <- sweep(mu.l.gru,MARGIN=2,df.post.exp$partyshare,`*`) 
mu.w.lin <- sweep(mu.l.lin,MARGIN=2,df.post.exp$partyshare,`*`) 

# collapse the cells to district level
mu.j.cdsu <- array(NA, c(n.sims, N))
mu.j.spd <- array(NA, c(n.sims, N))
mu.j.fdp <- array(NA, c(n.sims, N))
mu.j.gru <- array(NA, c(n.sims, N))
mu.j.lin <- array(NA, c(n.sims, N))
for (i in 1:n.sims) {
  mu.j.cdsu[i,] <- tapply(mu.w.cdsu[i,], df.post.exp$WKR_NR, sum)
  mu.j.spd[i,] <- tapply(mu.w.spd[i,], df.post.exp$WKR_NR, sum)
  mu.j.fdp[i,] <- tapply(mu.w.fdp[i,], df.post.exp$WKR_NR, sum)
  mu.j.gru[i,] <- tapply(mu.w.gru[i,], df.post.exp$WKR_NR, sum)
  mu.j.lin[i,] <- tapply(mu.w.lin[i,], df.post.exp$WKR_NR, sum)
}


# median SAEs plus SDs and 95%-CIs
wk.cdsu.mu <- 0
wk.cdsu.sd <- 0
wk.cdsu.90lo <- 0
wk.cdsu.90hi <- 0
wk.spd.mu <- 0
wk.spd.sd <- 0
wk.spd.90lo <- 0
wk.spd.90hi <- 0
wk.fdp.mu <- 0
wk.fdp.sd <- 0
wk.fdp.90lo <- 0
wk.fdp.90hi <- 0
wk.gru.mu <- 0
wk.gru.sd <- 0
wk.gru.90lo <- 0
wk.gru.90hi <- 0
wk.lin.mu <- 0
wk.lin.sd <- 0
wk.lin.90lo <- 0
wk.lin.90hi <- 0
for (i in 1:N){
  wk.cdsu.mu[i] <- median(mu.j.cdsu[,i])
  wk.cdsu.sd[i] <- sd(mu.j.cdsu[,i])
  wk.cdsu.90lo[i] <- quantile(mu.j.cdsu[,i],probs=c(.05), na.rm=T)
  wk.cdsu.90hi[i] <- quantile(mu.j.cdsu[,i],probs=c(.95), na.rm=T)
  wk.spd.mu[i] <- median(mu.j.spd[,i])
  wk.spd.sd[i] <- sd(mu.j.spd[,i])
  wk.spd.90lo[i] <- quantile(mu.j.spd[,i],probs=c(.05), na.rm=T)
  wk.spd.90hi[i] <- quantile(mu.j.spd[,i],probs=c(.95), na.rm=T)
  wk.fdp.mu[i] <- median(mu.j.fdp[,i])
  wk.fdp.sd[i] <- sd(mu.j.fdp[,i])
  wk.fdp.90lo[i] <- quantile(mu.j.fdp[,i],probs=c(.05), na.rm=T)
  wk.fdp.90hi[i] <- quantile(mu.j.fdp[,i],probs=c(.95), na.rm=T)
  wk.gru.mu[i] <- median(mu.j.gru[,i])
  wk.gru.sd[i] <- sd(mu.j.gru[,i])
  wk.gru.90lo[i] <- quantile(mu.j.gru[,i],probs=c(.05), na.rm=T)
  wk.gru.90hi[i] <- quantile(mu.j.gru[,i],probs=c(.95), na.rm=T)
  wk.lin.mu[i] <- median(mu.j.lin[,i])
  wk.lin.sd[i] <- sd(mu.j.lin[,i])
  wk.lin.90lo[i] <- quantile(mu.j.lin[,i],probs=c(.05), na.rm=T)
  wk.lin.90hi[i] <- quantile(mu.j.lin[,i],probs=c(.95), na.rm=T)
}

save.image(file="bugs_simulations/modelwkpost.image.forsa2013.0508.RData")


# save poststratification results
wk.estimates.2013.0508 <- list(
  mu.j.cdsu, mu.j.spd, mu.j.fdp, mu.j.gru, mu.j.lin, 
  wk.cdsu.mu, wk.spd.mu, wk.fdp.mu, wk.gru.mu, wk.lin.mu, 
  wk.cdsu.90lo, wk.spd.90lo, wk.fdp.90lo, wk.gru.90lo, wk.lin.90lo,
  wk.cdsu.90hi, wk.spd.90hi, wk.fdp.90hi, wk.gru.90hi, wk.lin.90hi
)
names(wk.estimates.2013.0508) <- c("mu.j.cdsu", "mu.j.spd", "mu.j.fdp", "mu.j.gru", "mu.j.lin", 
                                   "wk.cdsu.mu", "wk.spd.mu", "wk.fdp.mu", "wk.gru.mu", "wk.lin.mu", 
                                   "wk.cdsu.90lo", "wk.spd.90lo", "wk.fdp.90lo", "wk.gru.90lo", "wk.lin.90lo",
                                   "wk.cdsu.90hi", "wk.spd.90hi", "wk.fdp.90hi", "wk.gru.90hi", "wk.lin.90hi")
save(wk.estimates.2013.0508, file="bugs_simulations/modelwkpost.forsa2013.0508.RData")





## generate post-processible data frame of polling model estimates ------


# load district data
wk.results <- read.dta("prepared_data/data_districts_1994_2013.dta")

# import workspace image
load("bugs_simulations/modelwkpost.forsa2002.0508.RData")
load("bugs_simulations/modelwkpost.forsa2005.0508.RData")
load("bugs_simulations/modelwkpost.forsa2009.0508.RData")
load("bugs_simulations/modelwkpost.forsa2013.0508.RData")


# extract estimates
wk.estimates.2002 <- data.frame(wkr_nr2002=1:299,cdsu.mu.2002=wk.estimates.2002.0508$wk.cdsu.mu,spd.mu.2002=wk.estimates.2002.0508$wk.spd.mu,fdp.mu.2002=wk.estimates.2002.0508$wk.fdp.mu,gru.mu.2002=wk.estimates.2002.0508$wk.gru.mu,lin.mu.2002=wk.estimates.2002.0508$wk.lin.mu)
wk.estimates.2005 <- data.frame(wkr_nr2005=1:299,cdsu.mu.2005=wk.estimates.2005.0508$wk.cdsu.mu,spd.mu.2005=wk.estimates.2005.0508$wk.spd.mu,fdp.mu.2005=wk.estimates.2005.0508$wk.fdp.mu,gru.mu.2005=wk.estimates.2005.0508$wk.gru.mu,lin.mu.2005=wk.estimates.2005.0508$wk.lin.mu)
wk.estimates.2009 <- data.frame(wkr_nr2009=1:299,cdsu.mu.2009=wk.estimates.2009.0508$wk.cdsu.mu,spd.mu.2009=wk.estimates.2009.0508$wk.spd.mu,fdp.mu.2009=wk.estimates.2009.0508$wk.fdp.mu,gru.mu.2009=wk.estimates.2009.0508$wk.gru.mu,lin.mu.2009=wk.estimates.2009.0508$wk.lin.mu)
wk.estimates.2013 <- data.frame(wkr_nr2013=1:299,cdsu.mu.2013=wk.estimates.2013.0508$wk.cdsu.mu,spd.mu.2013=wk.estimates.2013.0508$wk.spd.mu,fdp.mu.2013=wk.estimates.2013.0508$wk.fdp.mu,gru.mu.2013=wk.estimates.2013.0508$wk.gru.mu,lin.mu.2013=wk.estimates.2013.0508$wk.lin.mu)

# load election results
wk.results <- read.dta("../data/constituencies/districts_2002_2013.dta")

# match estimates to election results
wk.results <- merge(wk.results, wk.estimates.2002, by = "wkr_nr2002", all.x = TRUE)
wk.results <- merge(wk.results, wk.estimates.2005, by = "wkr_nr2005", all.x = TRUE)
wk.results <- merge(wk.results, wk.estimates.2009, by = "wkr_nr2009", all.x = TRUE)
wk.results <- merge(wk.results, wk.estimates.2013, by = "wkr_nr2013", all.x = TRUE)

wk.results$cdsu1_2002 <- wk.results$cdu1_2002 + wk.results$csu1_2002
wk.results$cdsu1_2005 <- wk.results$cdu1_2005 + wk.results$csu1_2005
wk.results$cdsu1_2009 <- wk.results$cdu1_2009 + wk.results$csu1_2009


# compute district winner variable
wk.results$cdsu1_2002 <- wk.results$cdu1_2002 + wk.results$csu1_2002
wk.results$cdsu1_2005 <- wk.results$cdu1_2005 + wk.results$csu1_2005
wk.results$cdsu1_2009 <- wk.results$cdu1_2009 + wk.results$csu1_2009
wk.results$winner2002 <- as.character(apply(cbind(wk.results$cdsu1_2002, wk.results$spd1_2002, wk.results$fdp1_2002, wk.results$gru1_2002, wk.results$pds1_2002), 1, which.max))
wk.results$winner2002[wk.results$winner2002=="1"] <- "cdsu"
wk.results$winner2002[wk.results$winner2002=="2"] <- "spd"
wk.results$winner2002[wk.results$winner2002=="3"] <- "fdp"
wk.results$winner2002[wk.results$winner2002=="4"] <- "gru"
wk.results$winner2002[wk.results$winner2002=="5"] <- "lin"
wk.results$winner2005 <- as.character(apply(cbind(wk.results$cdsu1_2005, wk.results$spd1_2005, wk.results$fdp1_2005, wk.results$gru1_2005, wk.results$pds1_2005), 1, which.max))
wk.results$winner2005[wk.results$winner2005=="1"] <- "cdsu"
wk.results$winner2005[wk.results$winner2005=="2"] <- "spd"
wk.results$winner2005[wk.results$winner2005=="3"] <- "fdp"
wk.results$winner2005[wk.results$winner2005=="4"] <- "gru"
wk.results$winner2005[wk.results$winner2005=="5"] <- "lin"
wk.results$winner2009 <- as.character(apply(cbind(wk.results$cdsu1_2009, wk.results$spd1_2009, wk.results$fdp1_2009, wk.results$gru1_2009, wk.results$pds1_2009), 1, which.max))
wk.results$winner2009[wk.results$winner2009=="1"] <- "cdsu"
wk.results$winner2009[wk.results$winner2009=="2"] <- "spd"
wk.results$winner2009[wk.results$winner2009=="3"] <- "fdp"
wk.results$winner2009[wk.results$winner2009=="4"] <- "gru"
wk.results$winner2009[wk.results$winner2009=="5"] <- "lin"

# compute district winner variable
wk.results$cdsu_winner_2002 <- ifelse(wk.results$winner2002=="cdsu", 1, 0)
wk.results$cdsu_winner_2005 <- ifelse(wk.results$winner2005=="cdsu", 1, 0)
wk.results$cdsu_winner_2009 <- ifelse(wk.results$winner2009=="cdsu", 1, 0)
wk.results$spd_winner_2002 <- ifelse(wk.results$winner2002=="spd", 1, 0)
wk.results$spd_winner_2005 <- ifelse(wk.results$winner2005=="spd", 1, 0)
wk.results$spd_winner_2009 <- ifelse(wk.results$winner2009=="spd", 1, 0)
wk.results$fdp_winner_2002 <- ifelse(wk.results$winner2002=="fdp", 1, 0)
wk.results$fdp_winner_2005 <- ifelse(wk.results$winner2005=="fdp", 1, 0)
wk.results$fdp_winner_2009 <- ifelse(wk.results$winner2009=="fdp", 1, 0)
wk.results$gru_winner_2002 <- ifelse(wk.results$winner2002=="gru", 1, 0)
wk.results$gru_winner_2005 <- ifelse(wk.results$winner2005=="gru", 1, 0)
wk.results$gru_winner_2009 <- ifelse(wk.results$winner2009=="gru", 1, 0)
wk.results$lin_winner_2002 <- ifelse(wk.results$winner2002=="lin", 1, 0)
wk.results$lin_winner_2005 <- ifelse(wk.results$winner2005=="lin", 1, 0)
wk.results$lin_winner_2009 <- ifelse(wk.results$winner2009=="lin", 1, 0)


# manual reshape
wkrname.vars <- names(wk.results)[str_detect(names(wk.results), "wkr_name")]
residual.vars <- names(wk.results)[str_detect(names(wk.results), "resid.")]
firstshare.vars <- names(wk.results)[str_detect(names(wk.results), "1share")]
winner.vars <- names(wk.results)[str_detect(names(wk.results), "_winner_")]
mu.vars <-  names(wk.results)[str_detect(names(wk.results), "[.]mu[.]")]

wk.results.melt.firstshare <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=firstshare.vars)
wk.results.melt.firstshare$party <- str_extract(wk.results.melt.firstshare$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.firstshare$year <- str_extract(wk.results.melt.firstshare$variable, "2002|2005|2009|2013")
wk.results.melt.firstshare$firstshare <- wk.results.melt.firstshare$value
wk.results.melt.firstshare$variable <- NULL
wk.results.melt.firstshare$value <- NULL

wk.results.melt.mu <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=mu.vars)
wk.results.melt.mu$party <- str_extract(wk.results.melt.mu$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.mu$year <- str_extract(wk.results.melt.mu$variable, "2002|2005|2009|2013")
wk.results.melt.mu$mu <- wk.results.melt.mu$value
wk.results.melt.mu$variable <- NULL
wk.results.melt.mu$value <- NULL

wk.results.melt.winner <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=winner.vars)
wk.results.melt.winner$party <- str_extract(wk.results.melt.winner$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.winner$year <- str_extract(wk.results.melt.winner$variable, "2002|2005|2009|2013")
wk.results.melt.winner$winner <- wk.results.melt.winner$value
wk.results.melt.winner$variable <- NULL
wk.results.melt.winner$value <- NULL

wk.results.melt <- merge(wk.results.melt.mu, wk.results.melt.firstshare, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.winner, by=c("year", "party", "wkr_nr2013"), all = TRUE)

wk.results.melt$year <- as.factor(wk.results.melt$year)

# generate interaction variables
wk.results.melt$partyXyear <- interaction(wk.results.melt$party,wk.results.melt$year)
wk.results.melt$partyXwkr <- interaction(wk.results.melt$wkr_nr2013,wk.results.melt$party)

save(wk.results.melt, file = "prepared_data/data_prep_model_polls.RData")







