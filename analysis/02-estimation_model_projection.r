# Forecasting Elections at the Constituency Level
# Simon Munzert

## preparations -----------------------------------
# load packages
source("packages.r")

# load district data
wk.results <- read.dta("prepared_data/data_districts_1994_2013.dta")


## create variables -------------------------------

# identify winning party
wk.results$cdsu1_1990 <- wk.results$cdu1_1990 + wk.results$csu1_1990
wk.results$cdsu1_1994 <- wk.results$cdu1_1994 + wk.results$csu1_1994
wk.results$cdsu1_1998 <- wk.results$cdu1_1998 + wk.results$csu1_1998
wk.results$cdsu1_2002 <- wk.results$cdu1_2002 + wk.results$csu1_2002
wk.results$cdsu1_2005 <- wk.results$cdu1_2005 + wk.results$csu1_2005
wk.results$cdsu1_2009 <- wk.results$cdu1_2009 + wk.results$csu1_2009
wk.results$winner1990 <- as.character(apply(cbind(wk.results$cdsu1_1990, wk.results$spd1_1990, wk.results$fdp1_1990, wk.results$gru1_1990, wk.results$pds1_1990), 1, which.max))
wk.results$winner1990[wk.results$winner1990=="1"] <- "cdsu"
wk.results$winner1990[wk.results$winner1990=="2"] <- "spd"
wk.results$winner1990[wk.results$winner1990=="3"] <- "fdp"
wk.results$winner1990[wk.results$winner1990=="4"] <- "gru"
wk.results$winner1990[wk.results$winner1990=="5"] <- "lin"
wk.results$winner1994 <- as.character(apply(cbind(wk.results$cdsu1_1994, wk.results$spd1_1994, wk.results$fdp1_1994, wk.results$gru1_1994, wk.results$pds1_1994), 1, which.max))
wk.results$winner1994[wk.results$winner1994=="1"] <- "cdsu"
wk.results$winner1994[wk.results$winner1994=="2"] <- "spd"
wk.results$winner1994[wk.results$winner1994=="3"] <- "fdp"
wk.results$winner1994[wk.results$winner1994=="4"] <- "gru"
wk.results$winner1994[wk.results$winner1994=="5"] <- "lin"
wk.results$winner1998 <- as.character(apply(cbind(wk.results$cdsu1_1998, wk.results$spd1_1998, wk.results$fdp1_1998, wk.results$gru1_1998, wk.results$pds1_1998), 1, which.max))
wk.results$winner1998[wk.results$winner1998=="1"] <- "cdsu"
wk.results$winner1998[wk.results$winner1998=="2"] <- "spd"
wk.results$winner1998[wk.results$winner1998=="3"] <- "fdp"
wk.results$winner1998[wk.results$winner1998=="4"] <- "gru"
wk.results$winner1998[wk.results$winner1998=="5"] <- "lin"
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

# create dummy variables for winning party
wk.results$cdsu_winner_1994 <- ifelse(wk.results$winner1994=="cdsu", 1, 0)
wk.results$cdsu_winner_1998 <- ifelse(wk.results$winner1998=="cdsu", 1, 0)
wk.results$cdsu_winner_2002 <- ifelse(wk.results$winner2002=="cdsu", 1, 0)
wk.results$cdsu_winner_2005 <- ifelse(wk.results$winner2005=="cdsu", 1, 0)
wk.results$cdsu_winner_2009 <- ifelse(wk.results$winner2009=="cdsu", 1, 0)
wk.results$spd_winner_1994 <- ifelse(wk.results$winner1994=="spd", 1, 0)
wk.results$spd_winner_1998 <- ifelse(wk.results$winner1998=="spd", 1, 0)
wk.results$spd_winner_2002 <- ifelse(wk.results$winner2002=="spd", 1, 0)
wk.results$spd_winner_2005 <- ifelse(wk.results$winner2005=="spd", 1, 0)
wk.results$spd_winner_2009 <- ifelse(wk.results$winner2009=="spd", 1, 0)
wk.results$fdp_winner_1994 <- ifelse(wk.results$winner1994=="fdp", 1, 0)
wk.results$fdp_winner_1998 <- ifelse(wk.results$winner1998=="fdp", 1, 0)
wk.results$fdp_winner_2002 <- ifelse(wk.results$winner2002=="fdp", 1, 0)
wk.results$fdp_winner_2005 <- ifelse(wk.results$winner2005=="fdp", 1, 0)
wk.results$fdp_winner_2009 <- ifelse(wk.results$winner2009=="fdp", 1, 0)
wk.results$gru_winner_1994 <- ifelse(wk.results$winner1994=="gru", 1, 0)
wk.results$gru_winner_1998 <- ifelse(wk.results$winner1998=="gru", 1, 0)
wk.results$gru_winner_2002 <- ifelse(wk.results$winner2002=="gru", 1, 0)
wk.results$gru_winner_2005 <- ifelse(wk.results$winner2005=="gru", 1, 0)
wk.results$gru_winner_2009 <- ifelse(wk.results$winner2009=="gru", 1, 0)
wk.results$lin_winner_1994 <- ifelse(wk.results$winner1994=="lin", 1, 0)
wk.results$lin_winner_1998 <- ifelse(wk.results$winner1998=="lin", 1, 0)
wk.results$lin_winner_2002 <- ifelse(wk.results$winner2002=="lin", 1, 0)
wk.results$lin_winner_2005 <- ifelse(wk.results$winner2005=="lin", 1, 0)
wk.results$lin_winner_2009 <- ifelse(wk.results$winner2009=="lin", 1, 0)

# create pioneer variable
wk.results$cdsu_newrun_1994 <- ifelse(wk.results$cdsu1share_1990==0, 1, 0)
wk.results$cdsu_newrun_1998 <- ifelse(wk.results$cdsu1share_1994==0, 1, 0)
wk.results$cdsu_newrun_2002 <- ifelse(wk.results$cdsu1share_1998==0, 1, 0)
wk.results$cdsu_newrun_2005 <- ifelse(wk.results$cdsu1share_2002==0, 1, 0)
wk.results$cdsu_newrun_2009 <- ifelse(wk.results$cdsu1share_2005==0, 1, 0)
wk.results$cdsu_newrun_2013 <- ifelse(wk.results$cdsu1share_2009==0, 1, 0)
wk.results$spd_newrun_1994 <- ifelse(wk.results$spd1share_1990==0, 1, 0)
wk.results$spd_newrun_1998 <- ifelse(wk.results$spd1share_1994==0, 1, 0)
wk.results$spd_newrun_2002 <- ifelse(wk.results$spd1share_1998==0, 1, 0)
wk.results$spd_newrun_2005 <- ifelse(wk.results$spd1share_2002==0, 1, 0)
wk.results$spd_newrun_2009 <- ifelse(wk.results$spd1share_2005==0, 1, 0)
wk.results$spd_newrun_2013 <- ifelse(wk.results$spd1share_2009==0, 1, 0)
wk.results$fdp_newrun_1994 <- ifelse(wk.results$fdp1share_1990==0, 1, 0)
wk.results$fdp_newrun_1998 <- ifelse(wk.results$fdp1share_1994==0, 1, 0)
wk.results$fdp_newrun_2002 <- ifelse(wk.results$fdp1share_1998==0, 1, 0)
wk.results$fdp_newrun_2005 <- ifelse(wk.results$fdp1share_2002==0, 1, 0)
wk.results$fdp_newrun_2009 <- ifelse(wk.results$fdp1share_2005==0, 1, 0)
wk.results$fdp_newrun_2013 <- ifelse(wk.results$fdp1share_2009==0, 1, 0)
wk.results$gru_newrun_1994 <- ifelse(wk.results$gru1share_1990==0, 1, 0)
wk.results$gru_newrun_1998 <- ifelse(wk.results$gru1share_1994==0, 1, 0)
wk.results$gru_newrun_2002 <- ifelse(wk.results$gru1share_1998==0, 1, 0)
wk.results$gru_newrun_2005 <- ifelse(wk.results$gru1share_2002==0, 1, 0)
wk.results$gru_newrun_2009 <- ifelse(wk.results$gru1share_2005==0, 1, 0)
wk.results$gru_newrun_2013 <- ifelse(wk.results$gru1share_2009==0, 1, 0)
wk.results$lin_newrun_1994 <- ifelse(wk.results$lin1share_1990==0, 1, 0)
wk.results$lin_newrun_1998 <- ifelse(wk.results$lin1share_1994==0, 1, 0)
wk.results$lin_newrun_2002 <- ifelse(wk.results$lin1share_1998==0, 1, 0)
wk.results$lin_newrun_2005 <- ifelse(wk.results$lin1share_2002==0, 1, 0)
wk.results$lin_newrun_2009 <- ifelse(wk.results$lin1share_2005==0, 1, 0)
wk.results$lin_newrun_2013 <- ifelse(wk.results$lin1share_2009==0, 1, 0)

# create indicator of incumbent party
wk.results$cdsu_lastwinner_1994 <- ifelse(wk.results$winner1990=="cdsu", 1, 0)
wk.results$cdsu_lastwinner_1998 <- ifelse(wk.results$winner1994=="cdsu", 1, 0)
wk.results$cdsu_lastwinner_2002 <- ifelse(wk.results$winner1998=="cdsu", 1, 0)
wk.results$cdsu_lastwinner_2005 <- ifelse(wk.results$winner2002=="cdsu", 1, 0)
wk.results$cdsu_lastwinner_2009 <- ifelse(wk.results$winner2005=="cdsu", 1, 0)
wk.results$cdsu_lastwinner_2013 <- ifelse(wk.results$winner2009=="cdsu", 1, 0)
wk.results$spd_lastwinner_1994 <- ifelse(wk.results$winner1990=="spd", 1, 0)
wk.results$spd_lastwinner_1998 <- ifelse(wk.results$winner1994=="spd", 1, 0)
wk.results$spd_lastwinner_2002 <- ifelse(wk.results$winner1998=="spd", 1, 0)
wk.results$spd_lastwinner_2005 <- ifelse(wk.results$winner2002=="spd", 1, 0)
wk.results$spd_lastwinner_2009 <- ifelse(wk.results$winner2005=="spd", 1, 0)
wk.results$spd_lastwinner_2013 <- ifelse(wk.results$winner2009=="spd", 1, 0)
wk.results$fdp_lastwinner_1994 <- ifelse(wk.results$winner1990=="fdp", 1, 0)
wk.results$fdp_lastwinner_1998 <- ifelse(wk.results$winner1994=="fdp", 1, 0)
wk.results$fdp_lastwinner_2002 <- ifelse(wk.results$winner1998=="fdp", 1, 0)
wk.results$fdp_lastwinner_2005 <- ifelse(wk.results$winner2002=="fdp", 1, 0)
wk.results$fdp_lastwinner_2009 <- ifelse(wk.results$winner2005=="fdp", 1, 0)
wk.results$fdp_lastwinner_2013 <- ifelse(wk.results$winner2009=="fdp", 1, 0)
wk.results$gru_lastwinner_1994 <- ifelse(wk.results$winner1990=="gru", 1, 0)
wk.results$gru_lastwinner_1998 <- ifelse(wk.results$winner1994=="gru", 1, 0)
wk.results$gru_lastwinner_2002 <- ifelse(wk.results$winner1998=="gru", 1, 0)
wk.results$gru_lastwinner_2005 <- ifelse(wk.results$winner2002=="gru", 1, 0)
wk.results$gru_lastwinner_2009 <- ifelse(wk.results$winner2005=="gru", 1, 0)
wk.results$gru_lastwinner_2013 <- ifelse(wk.results$winner2009=="gru", 1, 0)
wk.results$lin_lastwinner_1994 <- ifelse(wk.results$winner1990=="lin", 1, 0)
wk.results$lin_lastwinner_1998 <- ifelse(wk.results$winner1994=="lin", 1, 0)
wk.results$lin_lastwinner_2002 <- ifelse(wk.results$winner1998=="lin", 1, 0)
wk.results$lin_lastwinner_2005 <- ifelse(wk.results$winner2002=="lin", 1, 0)
wk.results$lin_lastwinner_2009 <- ifelse(wk.results$winner2005=="lin", 1, 0)
wk.results$lin_lastwinner_2013 <- ifelse(wk.results$winner2009=="lin", 1, 0)

# create district dominance variable
wk.results$cdsu_dominance_1994 <- ifelse(wk.results$winner1990=="cdsu", 1, 0)
wk.results$cdsu_dominance_1998 <- ifelse(wk.results$winner1990=="cdsu" & wk.results$winner1994=="cdsu", 1, 0)
wk.results$cdsu_dominance_2002 <- ifelse(wk.results$winner1990=="cdsu" & wk.results$winner1994=="cdsu" & wk.results$winner1998=="cdsu", 1, 0)
wk.results$cdsu_dominance_2005 <- ifelse(wk.results$winner1994=="cdsu" & wk.results$winner1998=="cdsu" & wk.results$winner2002=="cdsu", 1, 0)
wk.results$cdsu_dominance_2009 <- ifelse(wk.results$winner1998=="cdsu" & wk.results$winner2002=="cdsu" & wk.results$winner2005=="cdsu", 1, 0)
wk.results$cdsu_dominance_2013 <- ifelse(wk.results$winner2002=="cdsu" & wk.results$winner2005=="cdsu" & wk.results$winner2009=="cdsu", 1, 0)
wk.results$spd_dominance_1994 <- ifelse(wk.results$winner1990=="spd", 1, 0)
wk.results$spd_dominance_1998 <- ifelse(wk.results$winner1990=="spd" & wk.results$winner1994=="spd", 1, 0)
wk.results$spd_dominance_2002 <- ifelse(wk.results$winner1990=="spd" & wk.results$winner1994=="spd" & wk.results$winner1998=="spd", 1, 0)
wk.results$spd_dominance_2005 <- ifelse(wk.results$winner1994=="spd" & wk.results$winner1998=="spd" & wk.results$winner2002=="spd", 1, 0)
wk.results$spd_dominance_2009 <- ifelse(wk.results$winner1998=="spd" & wk.results$winner2002=="spd" & wk.results$winner2005=="spd", 1, 0)
wk.results$spd_dominance_2013 <- ifelse(wk.results$winner2002=="spd" & wk.results$winner2005=="spd" & wk.results$winner2009=="spd", 1, 0)
wk.results$fdp_dominance_1994 <- ifelse(wk.results$winner1990=="fdp", 1, 0)
wk.results$fdp_dominance_1998 <- ifelse(wk.results$winner1990=="fdp" & wk.results$winner1994=="fdp", 1, 0)
wk.results$fdp_dominance_2002 <- ifelse(wk.results$winner1990=="fdp" & wk.results$winner1994=="fdp" & wk.results$winner1998=="fdp", 1, 0)
wk.results$fdp_dominance_2005 <- ifelse(wk.results$winner1994=="fdp" & wk.results$winner1998=="fdp" & wk.results$winner2002=="fdp", 1, 0)
wk.results$fdp_dominance_2009 <- ifelse(wk.results$winner1998=="fdp" & wk.results$winner2002=="fdp" & wk.results$winner2005=="fdp", 1, 0)
wk.results$fdp_dominance_2013 <- ifelse(wk.results$winner2002=="fdp" & wk.results$winner2005=="fdp" & wk.results$winner2009=="fdp", 1, 0)
wk.results$gru_dominance_1994 <- ifelse(wk.results$winner1990=="gru", 1, 0)
wk.results$gru_dominance_1998 <- ifelse(wk.results$winner1990=="gru" & wk.results$winner1994=="gru", 1, 0)
wk.results$gru_dominance_2002 <- ifelse(wk.results$winner1990=="gru" & wk.results$winner1994=="gru" & wk.results$winner1998=="gru", 1, 0)
wk.results$gru_dominance_2005 <- ifelse(wk.results$winner1994=="gru" & wk.results$winner1998=="gru" & wk.results$winner2002=="gru", 1, 0)
wk.results$gru_dominance_2009 <- ifelse(wk.results$winner1998=="gru" & wk.results$winner2002=="gru" & wk.results$winner2005=="gru", 1, 0)
wk.results$gru_dominance_2013 <- ifelse(wk.results$winner2002=="gru" & wk.results$winner2005=="gru" & wk.results$winner2009=="gru", 1, 0)
wk.results$lin_dominance_1994 <- ifelse(wk.results$winner1990=="lin", 1, 0)
wk.results$lin_dominance_1998 <- ifelse(wk.results$winner1990=="lin" & wk.results$winner1994=="lin", 1, 0)
wk.results$lin_dominance_2002 <- ifelse(wk.results$winner1990=="lin" & wk.results$winner1994=="lin" & wk.results$winner1998=="lin", 1, 0)
wk.results$lin_dominance_2005 <- ifelse(wk.results$winner1994=="lin" & wk.results$winner1998=="lin" & wk.results$winner2002=="lin", 1, 0)
wk.results$lin_dominance_2009 <- ifelse(wk.results$winner1998=="lin" & wk.results$winner2002=="lin" & wk.results$winner2005=="lin", 1, 0)
wk.results$lin_dominance_2013 <- ifelse(wk.results$winner2002=="lin" & wk.results$winner2005=="lin" & wk.results$winner2009=="lin", 1, 0)



## estimate uniform swing model for past elections --------------

# identify party-specific national swings: aggregated fvs at current election - aggregated fvs at past election 
(wk.results$swing.cdsu_2009 <- mean(wk.results$cdsu1share_2009, na.rm=T) - mean(wk.results$cdsu1share_2005, na.rm=T))
(wk.results$swing.cdsu_2005 <- mean(wk.results$cdsu1share_2005, na.rm=T) - mean(wk.results$cdsu1share_2002, na.rm=T))
(wk.results$swing.cdsu_2002 <- mean(wk.results$cdsu1share_2002, na.rm=T) - mean(wk.results$cdsu1share_1998, na.rm=T))
(wk.results$swing.cdsu_1998 <- mean(wk.results$cdsu1share_1998, na.rm=T) - mean(wk.results$cdsu1share_1994, na.rm=T))
(wk.results$swing.cdsu_1994 <- mean(wk.results$cdsu1share_1994, na.rm=T) - mean(wk.results$cdsu1share_1990, na.rm=T))
(wk.results$swing.spd_2009 <- mean(wk.results$spd1share_2009, na.rm=T) - mean(wk.results$spd1share_2005, na.rm=T))
(wk.results$swing.spd_2005 <- mean(wk.results$spd1share_2005, na.rm=T) - mean(wk.results$spd1share_2002, na.rm=T))
(wk.results$swing.spd_2002 <- mean(wk.results$spd1share_2002, na.rm=T) - mean(wk.results$spd1share_1998, na.rm=T))
(wk.results$swing.spd_1998 <- mean(wk.results$spd1share_1998, na.rm=T) - mean(wk.results$spd1share_1994, na.rm=T))
(wk.results$swing.spd_1994 <- mean(wk.results$spd1share_1994, na.rm=T) - mean(wk.results$spd1share_1990, na.rm=T))
(wk.results$swing.fdp_2009 <- mean(wk.results$fdp1share_2009, na.rm=T) - mean(wk.results$fdp1share_2005, na.rm=T))
(wk.results$swing.fdp_2005 <- mean(wk.results$fdp1share_2005, na.rm=T) - mean(wk.results$fdp1share_2002, na.rm=T))
(wk.results$swing.fdp_2002 <- mean(wk.results$fdp1share_2002, na.rm=T) - mean(wk.results$fdp1share_1998, na.rm=T))
(wk.results$swing.fdp_1998 <- mean(wk.results$fdp1share_1998, na.rm=T) - mean(wk.results$fdp1share_1994, na.rm=T))
(wk.results$swing.fdp_1994 <- mean(wk.results$fdp1share_1994, na.rm=T) - mean(wk.results$fdp1share_1990, na.rm=T))
(wk.results$swing.gru_2009 <- mean(wk.results$gru1share_2009, na.rm=T) - mean(wk.results$gru1share_2005, na.rm=T))
(wk.results$swing.gru_2005 <- mean(wk.results$gru1share_2005, na.rm=T) - mean(wk.results$gru1share_2002, na.rm=T))
(wk.results$swing.gru_2002 <- mean(wk.results$gru1share_2002, na.rm=T) - mean(wk.results$gru1share_1998, na.rm=T))
(wk.results$swing.gru_1998 <- mean(wk.results$gru1share_1998, na.rm=T) - mean(wk.results$gru1share_1994, na.rm=T))
(wk.results$swing.gru_1994 <- mean(wk.results$gru1share_1994, na.rm=T) - mean(wk.results$gru1share_1990, na.rm=T))
(wk.results$swing.lin_2009 <- mean(wk.results$lin1share_2009, na.rm=T) - mean(wk.results$lin1share_2005, na.rm=T))
(wk.results$swing.lin_2005 <- mean(wk.results$lin1share_2005, na.rm=T) - mean(wk.results$lin1share_2002, na.rm=T))
(wk.results$swing.lin_2002 <- mean(wk.results$lin1share_2002, na.rm=T) - mean(wk.results$lin1share_1998, na.rm=T))
(wk.results$swing.lin_1998 <- mean(wk.results$lin1share_1998, na.rm=T) - mean(wk.results$lin1share_1994, na.rm=T))
(wk.results$swing.lin_1994 <- mean(wk.results$lin1share_1994, na.rm=T) - mean(wk.results$lin1share_1990, na.rm=T))

# compute projections: past first vote share + wk.results$swing
wk.results$cdsu_2009.proj <- wk.results$cdsu1share_2005 + wk.results$swing.cdsu_2009
wk.results$cdsu_2005.proj <- wk.results$cdsu1share_2002 + wk.results$swing.cdsu_2005
wk.results$cdsu_2002.proj <- wk.results$cdsu1share_1998 + wk.results$swing.cdsu_2002
wk.results$cdsu_1998.proj <- wk.results$cdsu1share_1994 + wk.results$swing.cdsu_1998
wk.results$cdsu_1994.proj <- wk.results$cdsu1share_1990 + wk.results$swing.cdsu_1994
wk.results$spd_2009.proj <- wk.results$spd1share_2005 + wk.results$swing.spd_2009
wk.results$spd_2005.proj <- wk.results$spd1share_2002 + wk.results$swing.spd_2005
wk.results$spd_2002.proj <- wk.results$spd1share_1998 + wk.results$swing.spd_2002
wk.results$spd_1998.proj <- wk.results$spd1share_1994 + wk.results$swing.spd_1998
wk.results$spd_1994.proj <- wk.results$spd1share_1990 + wk.results$swing.spd_1994
wk.results$fdp_2009.proj <- wk.results$fdp1share_2005 + wk.results$swing.fdp_2009
wk.results$fdp_2005.proj <- wk.results$fdp1share_2002 + wk.results$swing.fdp_2005
wk.results$fdp_2002.proj <- wk.results$fdp1share_1998 + wk.results$swing.fdp_2002
wk.results$fdp_1998.proj <- wk.results$fdp1share_1994 + wk.results$swing.fdp_1998
wk.results$fdp_1994.proj <- wk.results$fdp1share_1990 + wk.results$swing.fdp_1994
wk.results$gru_2009.proj <- wk.results$gru1share_2005 + wk.results$swing.gru_2009
wk.results$gru_2005.proj <- wk.results$gru1share_2002 + wk.results$swing.gru_2005
wk.results$gru_2002.proj <- wk.results$gru1share_1998 + wk.results$swing.gru_2002
wk.results$gru_1998.proj <- wk.results$gru1share_1994 + wk.results$swing.gru_1998
wk.results$gru_1994.proj <- wk.results$gru1share_1990 + wk.results$swing.gru_1994
wk.results$lin_2009.proj <- wk.results$lin1share_2005 + wk.results$swing.lin_2009
wk.results$lin_2005.proj <- wk.results$lin1share_2002 + wk.results$swing.lin_2005
wk.results$lin_2002.proj <- wk.results$lin1share_1998 + wk.results$swing.lin_2002
wk.results$lin_1998.proj <- wk.results$lin1share_1994 + wk.results$swing.lin_1998
wk.results$lin_1994.proj <- wk.results$lin1share_1990 + wk.results$swing.lin_1994



## out-of-sample estimate for 2013 election --------------

### generate 2013 forecast
result.2009 <- c(.338, .230, .146, .107, .119)
forecast.nat.2013 <- c(.381, .282, .054, .135, .077) # Selb/Munzert
#forecast.nat.2013 <- c(.389, .263, .065, .100, .087) # election.de
#forecast.nat.2013 <- c(.391, .258, .059, .110, .082) # pollyvote

swing.2013 <- forecast.nat.2013 - result.2009
swing.cdsu_2013 <- swing.2013[1]
swing.spd_2013 <- swing.2013[2]
swing.fdp_2013 <- swing.2013[3]
swing.gru_2013 <- swing.2013[4]
swing.lin_2013 <- swing.2013[5]

wk.results$cdsu_2013.proj <- wk.results$cdsu1share_2009 + swing.cdsu_2013
wk.results$spd_2013.proj <- wk.results$spd1share_2009 + swing.spd_2013
wk.results$fdp_2013.proj <- wk.results$fdp1share_2009 + swing.fdp_2013
wk.results$gru_2013.proj <- wk.results$gru1share_2009 + swing.gru_2013
wk.results$lin_2013.proj <- wk.results$lin1share_2009 + swing.lin_2013


## reshape data -------------------------------------------
firstshare.vars <- names(wk.results)[str_detect(names(wk.results), "1share")][1:30]
project.vars <- names(wk.results)[str_detect(names(wk.results), "proj")]
lastwinner.vars <- names(wk.results)[str_detect(names(wk.results), "lastwinner")]
winner.vars <- names(wk.results)[str_detect(names(wk.results), "_winner_")]
dominance.vars <-  names(wk.results)[str_detect(names(wk.results), "dominance")]
newrun.vars <-  names(wk.results)[str_detect(names(wk.results), "newrun")]
swing.vars <-  names(wk.results)[str_detect(names(wk.results), "swing")]
incumbent.vars <- names(wk.results)[str_detect(names(wk.results), "_inc")]


wk.results.melt.firstshare <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=firstshare.vars)
wk.results.melt.firstshare$party <- str_extract(wk.results.melt.firstshare$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.firstshare$year <- str_extract(wk.results.melt.firstshare$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.firstshare$firstshare <- wk.results.melt.firstshare$value
wk.results.melt.firstshare$variable <- NULL
wk.results.melt.firstshare$value <- NULL

wk.results.melt.project <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=project.vars)
wk.results.melt.project$party <- str_extract(wk.results.melt.project$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.project$year <- str_extract(wk.results.melt.project$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.project$project <- wk.results.melt.project$value
wk.results.melt.project$variable <- NULL
wk.results.melt.project$value <- NULL

wk.results.melt.lastwinner <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=lastwinner.vars)
wk.results.melt.lastwinner$party <- str_extract(wk.results.melt.lastwinner$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.lastwinner$year <- str_extract(wk.results.melt.lastwinner$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.lastwinner$lastwinner <- wk.results.melt.lastwinner$value
wk.results.melt.lastwinner$variable <- NULL
wk.results.melt.lastwinner$value <- NULL

wk.results.melt.dominance <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=dominance.vars)
wk.results.melt.dominance$party <- str_extract(wk.results.melt.dominance$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.dominance$year <- str_extract(wk.results.melt.dominance$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.dominance$dominance <- wk.results.melt.dominance$value
wk.results.melt.dominance$variable <- NULL
wk.results.melt.dominance$value <- NULL

wk.results.melt.newrun <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=newrun.vars)
wk.results.melt.newrun$party <- str_extract(wk.results.melt.newrun$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.newrun$year <- str_extract(wk.results.melt.newrun$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.newrun$newrun <- wk.results.melt.newrun$value
wk.results.melt.newrun$variable <- NULL
wk.results.melt.newrun$value <- NULL

wk.results.melt.swing <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=swing.vars)
wk.results.melt.swing$party <- str_extract(wk.results.melt.swing$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.swing$year <- str_extract(wk.results.melt.swing$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.swing$swing <- wk.results.melt.swing$value
wk.results.melt.swing$variable <- NULL
wk.results.melt.swing$value <- NULL

wk.results.melt.winner <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=winner.vars)
wk.results.melt.winner$party <- str_extract(wk.results.melt.winner$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.winner$year <- str_extract(wk.results.melt.winner$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.winner$winner <- wk.results.melt.winner$value
wk.results.melt.winner$variable <- NULL
wk.results.melt.winner$value <- NULL


wk.results.melt.incumbent <- melt(wk.results, id=c("wkr_nr2013"), measure.vars=incumbent.vars)
wk.results.melt.incumbent$party <- str_extract(wk.results.melt.incumbent$variable, "cdsu|spd|fdp|gru|lin")
wk.results.melt.incumbent$year <- str_extract(wk.results.melt.incumbent$variable, "1990|1994|1998|2002|2005|2009|2013")
wk.results.melt.incumbent$incumbent <- wk.results.melt.incumbent$value
wk.results.melt.incumbent$variable <- NULL
wk.results.melt.incumbent$value <- NULL


wk.results.melt <- merge(wk.results.melt.firstshare, wk.results.melt.project, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.lastwinner, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.dominance, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.newrun, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.swing, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.winner, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt <- merge(wk.results.melt, wk.results.melt.incumbent, by=c("year", "party", "wkr_nr2013"), all = TRUE)
wk.results.melt$year <- as.factor(wk.results.melt$year)


# generate interaction variables
wk.results.melt$partyXwkr <- interaction(wk.results.melt$wkr_nr2013,wk.results.melt$party)
wk.results.melt$wkrXyear <- interaction(wk.results.melt$wkr_nr2013,wk.results.melt$year)


## export data --------------------------------------
wk.results.melt <- wk.results.melt[wk.results.melt$year != 1990,]
save(wk.results.melt, file = "prepared_data/data_prep_model_projection.RData")

