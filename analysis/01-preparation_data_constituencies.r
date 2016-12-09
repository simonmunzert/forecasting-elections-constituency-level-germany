# Forecasting Elections at the Constituency Level
# Simon Munzert

## preparations -----------------------------------

# load packages
source("packages.r")


## process data -----------------------------------

# import district keys
key.09.13 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_2009_2013.xlsx"),sheet=1)
key.05.09 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_2005_2009.xlsx"),sheet=1) 
key.02.05 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_2002_2005.xlsx"),sheet=1) 
key.98.02 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_1998_2002.xlsx"),sheet=1) 
key.94.98 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_1994_1998.xlsx"),sheet=1) 
key.90.94 <- readWorksheet(loadWorkbook("../data/constituencies/wkrkey_1990_1994.xlsx"),sheet=1) 


# prepare district keys
	# 1. for new districts, the first of the source districts is used as predecessor
key.09.13$wkr_nr2009[is.na(key.09.13$wkr_nr2009)] <- key.09.13$source01_2013[is.na(key.09.13$wkr_nr2009)]
key.05.09$wkr_nr2005[is.na(key.05.09$wkr_nr2005)] <- key.05.09$source01_2009[is.na(key.05.09$wkr_nr2005)]
key.02.05$wkr_nr2002[is.na(key.02.05$wkr_nr2002)] <- key.02.05$source01_2005[is.na(key.02.05$wkr_nr2002)]
key.98.02$wkr_nr1998[is.na(key.98.02$wkr_nr1998)] <- key.98.02$source01_2002[is.na(key.98.02$wkr_nr1998)]
key.94.98$wkr_nr1994[is.na(key.94.98$wkr_nr1994)] <- key.94.98$source01_1998[is.na(key.94.98$wkr_nr1994)]
key.90.94$wkr_nr1990[is.na(key.90.94$wkr_nr1990)] <- key.90.94$source01_1994[is.na(key.90.94$wkr_nr1990)]
	# 2. dead districts are deleted for the newest election
key.09.13 <- key.09.13[!is.na(key.09.13$wkr_nr2013),]

# merge district keys to 2013 district data frame
districts.df <- merge(key.09.13, key.05.09, by.x = "wkr_nr2009", by.y = "wkr_nr2009", all.x = TRUE)
districts.df <- merge(districts.df, key.02.05, by.x = "wkr_nr2005", by.y = "wkr_nr2005", all.x = TRUE)
districts.df <- merge(districts.df, key.98.02, by.x = "wkr_nr2002", by.y = "wkr_nr2002", all.x = TRUE)
districts.df <- merge(districts.df, key.94.98, by.x = "wkr_nr1998", by.y = "wkr_nr1998", all.x = TRUE)
districts.df <- merge(districts.df, key.90.94, by.x = "wkr_nr1994", by.y = "wkr_nr1994", all.x = TRUE)
select.namevars <- names(districts.df)[str_detect(names(districts.df), "wkr_name|source") == FALSE]
districts.df <- subset(districts.df, select = select.namevars)

districts.df <- districts.df[c("wkr_nr2013","wkr_nr2009","wkr_nr2005","wkr_nr2002","wkr_nr1998","wkr_nr1994","wkr_nr1990","change2013","new2013","change2009","new2009","change2005","new2005","change2002","new2002","change1998","new1998","change1994","new1994")] 

# recode change/new variables
districts.df$change2013[is.na(districts.df$change2013) == TRUE] <- 0
districts.df$change2009[is.na(districts.df$change2009) == TRUE] <- 0
districts.df$change2005[is.na(districts.df$change2005) == TRUE] <- 0
districts.df$change2002[is.na(districts.df$change2002) == TRUE] <- 0
districts.df$change1998[is.na(districts.df$change1998) == TRUE] <- 0
districts.df$change1994[is.na(districts.df$change1994) == TRUE] <- 0
districts.df$new2013[is.na(districts.df$new2013) == TRUE] <- 0
districts.df$new2009[is.na(districts.df$new2009) == TRUE] <- 0
districts.df$new2005[is.na(districts.df$new2005) == TRUE] <- 0
districts.df$new2002[is.na(districts.df$new2002) == TRUE] <- 0
districts.df$new1998[is.na(districts.df$new1998) == TRUE] <- 0
districts.df$new1994[is.na(districts.df$new1994) == TRUE] <- 0

# import district results
results.2013 <- read.csv("../data/constituencies/btw2013wkr.csv", header = T, sep=";")
names(results.2013) <- paste(names(results.2013), ".2013", sep="")
results.2009 <- read.csv("../data/constituencies/btw2009wkr.csv", header = T, sep=";")
names(results.2009) <- paste(names(results.2009), ".2009", sep="")
results.2005 <- read.csv("../data/constituencies/btw2005wkr.csv", header = T, sep=";")
names(results.2005) <- paste(names(results.2005), ".2005", sep="")
results.2002 <- read.csv("../data/constituencies/btw2002wkr.csv", header = T, sep=";")
names(results.2002) <- paste(names(results.2002), ".2002", sep="")
results.1998 <- read.csv("../data/constituencies/btw1998wkr.csv", header = T, sep=";")
names(results.1998) <- paste(names(results.1998), ".1998", sep="")
results.1994 <- read.csv("../data/constituencies/btw1994wkr.csv", header = T, sep=";")
names(results.1994) <- paste(names(results.1994), ".1994", sep="")
results.1990 <- read.csv("../data/constituencies/btw1990wkr.csv", header = T, sep=";")
names(results.1990) <- paste(names(results.1990), ".1990", sep="")


# match district results to district data frame
districts.df <- merge(districts.df, results.1990, by.x = "wkr_nr1990", by.y = "wkr_nr.1990", all.x = TRUE)
districts.df <- merge(districts.df, results.1994, by.x = "wkr_nr1994", by.y = "wkr_nr.1994", all.x = TRUE)
districts.df <- merge(districts.df, results.1998, by.x = "wkr_nr1998", by.y = "wkr_nr.1998", all.x = TRUE)
districts.df <- merge(districts.df, results.2002, by.x = "wkr_nr2002", by.y = "wkr_nr.2002", all.x = TRUE)
districts.df <- merge(districts.df, results.2005, by.x = "wkr_nr2005", by.y = "wkr_nr.2005", all.x = TRUE)
districts.df <- merge(districts.df, results.2009, by.x = "wkr_nr2009", by.y = "wkr_nr.2009", all.x = TRUE)
districts.df <- merge(districts.df, results.2013, by.x = "wkr_nr2013", by.y = "wkr_nr.2013", all.x = TRUE)

# generate district-level vote shares
districts.df$cdsu1share.1990 <- (districts.df$cdu1.1990 + districts.df$csu1.1990) / districts.df$zweitstimmegueltig.1990
districts.df$spd1share.1990 <- districts.df$spd1.1990 / districts.df$waehler.1990
districts.df$fdp1share.1990 <- districts.df$fdp1.1990 / districts.df$waehler.1990
districts.df$gru1share.1990 <- districts.df$gru1.1990 / districts.df$waehler.1990
districts.df$lin1share.1990 <- districts.df$pds1.1990 / districts.df$waehler.1990
districts.df$cdsu2share.1990 <- (districts.df$cdu2.1990 + districts.df$csu2.1990) / districts.df$zweitstimmegueltig.1990
districts.df$spd2share.1990 <- districts.df$spd2.1990 / districts.df$waehler.1990
districts.df$fdp2share.1990 <- districts.df$fdp2.1990 / districts.df$waehler.1990
districts.df$gru2share.1990 <- districts.df$gru2.1990 / districts.df$waehler.1990
districts.df$lin2share.1990 <- districts.df$pds2.1990 / districts.df$waehler.1990
districts.df$cdsu1share.1994 <- (districts.df$cdu1.1994 + districts.df$csu1.1994) / districts.df$zweitstimmegueltig.1994
districts.df$spd1share.1994 <- districts.df$spd1.1994 / districts.df$waehler.1994
districts.df$fdp1share.1994 <- districts.df$fdp1.1994 / districts.df$waehler.1994
districts.df$gru1share.1994 <- districts.df$gru1.1994 / districts.df$waehler.1994
districts.df$lin1share.1994 <- districts.df$pds1.1994 / districts.df$waehler.1994
districts.df$cdsu2share.1994 <- (districts.df$cdu2.1994 + districts.df$csu2.1994) / districts.df$zweitstimmegueltig.1994
districts.df$spd2share.1994 <- districts.df$spd2.1994 / districts.df$waehler.1994
districts.df$fdp2share.1994 <- districts.df$fdp2.1994 / districts.df$waehler.1994
districts.df$gru2share.1994 <- districts.df$gru2.1994 / districts.df$waehler.1994
districts.df$lin2share.1994 <- districts.df$pds2.1994 / districts.df$waehler.1994
districts.df$cdsu1share.1998 <- (districts.df$cdu1.1998 + districts.df$csu1.1998) / districts.df$zweitstimmegueltig.1998
districts.df$spd1share.1998 <- districts.df$spd1.1998 / districts.df$waehler.1998
districts.df$fdp1share.1998 <- districts.df$fdp1.1998 / districts.df$waehler.1998
districts.df$gru1share.1998 <- districts.df$gru1.1998 / districts.df$waehler.1998
districts.df$lin1share.1998 <- districts.df$pds1.1998 / districts.df$waehler.1998
districts.df$cdsu2share.1998 <- (districts.df$cdu2.1998 + districts.df$csu2.1998) / districts.df$zweitstimmegueltig.1998
districts.df$spd2share.1998 <- districts.df$spd2.1998 / districts.df$waehler.1998
districts.df$fdp2share.1998 <- districts.df$fdp2.1998 / districts.df$waehler.1998
districts.df$gru2share.1998 <- districts.df$gru2.1998 / districts.df$waehler.1998
districts.df$lin2share.1998 <- districts.df$pds2.1998 / districts.df$waehler.1998
districts.df$cdsu1share.2002 <- (districts.df$cdu1.2002 + districts.df$csu1.2002) / districts.df$zweitstimmegueltig.2002
districts.df$spd1share.2002 <- districts.df$spd1.2002 / districts.df$waehler.2002
districts.df$fdp1share.2002 <- districts.df$fdp1.2002 / districts.df$waehler.2002
districts.df$gru1share.2002 <- districts.df$gru1.2002 / districts.df$waehler.2002
districts.df$lin1share.2002 <- districts.df$pds1.2002 / districts.df$waehler.2002
districts.df$cdsu2share.2002 <- (districts.df$cdu2.2002 + districts.df$csu2.2002) / districts.df$zweitstimmegueltig.2002
districts.df$spd2share.2002 <- districts.df$spd2.2002 / districts.df$waehler.2002
districts.df$fdp2share.2002 <- districts.df$fdp2.2002 / districts.df$waehler.2002
districts.df$gru2share.2002 <- districts.df$gru2.2002 / districts.df$waehler.2002
districts.df$lin2share.2002 <- districts.df$pds2.2002 / districts.df$waehler.2002
districts.df$cdsu1share.2005 <- (districts.df$cdu1.2005 + districts.df$csu1.2005) / districts.df$zweitstimmegueltig.2005
districts.df$spd1share.2005 <- districts.df$spd1.2005 / districts.df$waehler.2005
districts.df$fdp1share.2005 <- districts.df$fdp1.2005 / districts.df$waehler.2005
districts.df$gru1share.2005 <- districts.df$gru1.2005 / districts.df$waehler.2005
districts.df$lin1share.2005 <- districts.df$pds1.2005 / districts.df$waehler.2005
districts.df$cdsu2share.2005 <- (districts.df$cdu2.2005 + districts.df$csu2.2005) / districts.df$zweitstimmegueltig.2005
districts.df$spd2share.2005 <- districts.df$spd2.2005 / districts.df$waehler.2005
districts.df$fdp2share.2005 <- districts.df$fdp2.2005 / districts.df$waehler.2005
districts.df$gru2share.2005 <- districts.df$gru2.2005 / districts.df$waehler.2005
districts.df$lin2share.2005 <- districts.df$pds2.2005 / districts.df$waehler.2005
districts.df$cdsu1share.2009 <- (districts.df$cdu1.2009 + districts.df$csu1.2009) / districts.df$zweitstimmegueltig.2009
districts.df$spd1share.2009 <- districts.df$spd1.2009 / districts.df$waehler.2009
districts.df$fdp1share.2009 <- districts.df$fdp1.2009 / districts.df$waehler.2009
districts.df$gru1share.2009 <- districts.df$gru1.2009 / districts.df$waehler.2009
districts.df$lin1share.2009 <- districts.df$pds1.2009 / districts.df$waehler.2009
districts.df$cdsu2share.2009 <- (districts.df$cdu2.2009 + districts.df$csu2.2009) / districts.df$zweitstimmegueltig.2009
districts.df$spd2share.2009 <- districts.df$spd2.2009 / districts.df$waehler.2009
districts.df$fdp2share.2009 <- districts.df$fdp2.2009 / districts.df$waehler.2009
districts.df$gru2share.2009 <- districts.df$gru2.2009 / districts.df$waehler.2009
districts.df$lin2share.2009 <- districts.df$pds2.2009 / districts.df$waehler.2009


# correct string format in variable wkr_nr1998
districts.df$wkr_nr1998 <- as.numeric(districts.df$wkr_nr1998)


# append incumbency information from Kurella/Pappi/Bräuninger
incumbency.dat <- read.dta("../data/Constituencies/Wahlkreispanel_ab1990.dta", convert.factors = FALSE)
incumbency.dat.sub <- select(incumbency.dat, wkr_nummer, year, ends_with("_inc"), ends_with("_u"))

# prepare 1994 data
incumbency.dat.sub.1994 <- filter(incumbency.dat.sub, year == 1994) %>% rename(cdsu_inc_1994 = cdu_k_inc, spd_inc_1994 = spd_k_inc, fdp_inc_1994 = fdp_k_inc, gru_inc_1994 = gru_k_inc, lin_inc_1994 = pds_k_inc,
                                                                               cdsu_inc_u_1994 = cdu_k_inc_u, spd_inc_u_1994 = spd_k_inc_u, fdp_inc_u_1994 = fdp_k_inc_u, gru_inc_u_1994 = gru_k_inc_u, lin_inc_u_1994 = pds_k_inc_u)
incumbency.dat.sub.1994$cdsu_inc_1994[is.na(incumbency.dat.sub.1994$cdsu_inc_1994)] <- incumbency.dat.sub.1994$csu_k_inc[is.na(incumbency.dat.sub.1994$cdsu_inc_1994)]
incumbency.dat.sub.1994$csu_k_inc <- NULL
incumbency.dat.sub.1994$year <- NULL
incumbency.dat.sub.1994 <- transmute(incumbency.dat.sub.1994, wkr_nummer = wkr_nummer, cdsu_inc_1994 = cdsu_inc_1994 + cdsu_inc_u_1994, spd_inc_1994 = spd_inc_1994 + spd_inc_u_1994, fdp_inc_1994 = fdp_inc_1994 + fdp_inc_u_1994, gru_inc_1994 = gru_inc_1994 + gru_inc_u_1994, lin_inc_1994 = lin_inc_1994 + lin_inc_u_1994)
incumbency.dat.sub.1994[is.na(incumbency.dat.sub.1994)] <- 0

# prepare 1998 data
incumbency.dat.sub.1998 <- filter(incumbency.dat.sub, year == 1998) %>% rename(cdsu_inc_1998 = cdu_k_inc, spd_inc_1998 = spd_k_inc, fdp_inc_1998 = fdp_k_inc, gru_inc_1998 = gru_k_inc, lin_inc_1998 = pds_k_inc,
                                                                               cdsu_inc_u_1998 = cdu_k_inc_u, spd_inc_u_1998 = spd_k_inc_u, fdp_inc_u_1998 = fdp_k_inc_u, gru_inc_u_1998 = gru_k_inc_u, lin_inc_u_1998 = pds_k_inc_u)
incumbency.dat.sub.1998$cdsu_inc_1998[is.na(incumbency.dat.sub.1998$cdsu_inc_1998)] <- incumbency.dat.sub.1998$csu_k_inc[is.na(incumbency.dat.sub.1998$cdsu_inc_1998)]
incumbency.dat.sub.1998$csu_k_inc <- NULL
incumbency.dat.sub.1998$year <- NULL
incumbency.dat.sub.1998 <- transmute(incumbency.dat.sub.1998, wkr_nummer = wkr_nummer, cdsu_inc_1998 = cdsu_inc_1998 + cdsu_inc_u_1998, spd_inc_1998 = spd_inc_1998 + spd_inc_u_1998, fdp_inc_1998 = fdp_inc_1998 + fdp_inc_u_1998, gru_inc_1998 = gru_inc_1998 + gru_inc_u_1998, lin_inc_1998 = lin_inc_1998 + lin_inc_u_1998)
incumbency.dat.sub.1998[is.na(incumbency.dat.sub.1998)] <- 0

# prepare 2002 data
incumbency.dat.sub.2002 <- filter(incumbency.dat.sub, year == 2002) %>% rename(cdsu_inc_2002 = cdu_k_inc, spd_inc_2002 = spd_k_inc, fdp_inc_2002 = fdp_k_inc, gru_inc_2002 = gru_k_inc, lin_inc_2002 = pds_k_inc,
                                                                               cdsu_inc_u_2002 = cdu_k_inc_u, spd_inc_u_2002 = spd_k_inc_u, fdp_inc_u_2002 = fdp_k_inc_u, gru_inc_u_2002 = gru_k_inc_u, lin_inc_u_2002 = pds_k_inc_u)
incumbency.dat.sub.2002$cdsu_inc_2002[is.na(incumbency.dat.sub.2002$cdsu_inc_2002)] <- incumbency.dat.sub.2002$csu_k_inc[is.na(incumbency.dat.sub.2002$cdsu_inc_2002)]
incumbency.dat.sub.2002$csu_k_inc <- NULL
incumbency.dat.sub.2002$year <- NULL
incumbency.dat.sub.2002 <- transmute(incumbency.dat.sub.2002, wkr_nummer = wkr_nummer, cdsu_inc_2002 = cdsu_inc_2002 + cdsu_inc_u_2002, spd_inc_2002 = spd_inc_2002 + spd_inc_u_2002, fdp_inc_2002 = fdp_inc_2002 + fdp_inc_u_2002, gru_inc_2002 = gru_inc_2002 + gru_inc_u_2002, lin_inc_2002 = lin_inc_2002 + lin_inc_u_2002)
incumbency.dat.sub.2002[is.na(incumbency.dat.sub.2002)] <- 0

# prepare 2005 data
incumbency.dat.sub.2005 <- filter(incumbency.dat.sub, year == 2005) %>% rename(cdsu_inc_2005 = cdu_k_inc, spd_inc_2005 = spd_k_inc, fdp_inc_2005 = fdp_k_inc, gru_inc_2005 = gru_k_inc, lin_inc_2005 = pds_k_inc,
                                                                               cdsu_inc_u_2005 = cdu_k_inc_u, spd_inc_u_2005 = spd_k_inc_u, fdp_inc_u_2005 = fdp_k_inc_u, gru_inc_u_2005 = gru_k_inc_u, lin_inc_u_2005 = pds_k_inc_u)
incumbency.dat.sub.2005$cdsu_inc_2005[is.na(incumbency.dat.sub.2005$cdsu_inc_2005)] <- incumbency.dat.sub.2005$csu_k_inc[is.na(incumbency.dat.sub.2005$cdsu_inc_2005)]
incumbency.dat.sub.2005$csu_k_inc <- NULL
incumbency.dat.sub.2005$year <- NULL
incumbency.dat.sub.2005 <- transmute(incumbency.dat.sub.2005, wkr_nummer = wkr_nummer, cdsu_inc_2005 = cdsu_inc_2005 + cdsu_inc_u_2005, spd_inc_2005 = spd_inc_2005 + spd_inc_u_2005, fdp_inc_2005 = fdp_inc_2005 + fdp_inc_u_2005, gru_inc_2005 = gru_inc_2005 + gru_inc_u_2005, lin_inc_2005 = lin_inc_2005 + lin_inc_u_2005)
incumbency.dat.sub.2005[is.na(incumbency.dat.sub.2005)] <- 0

# prepare 2009 data
incumbency.dat.sub.2009 <- filter(incumbency.dat.sub, year == 2009) %>% rename(cdsu_inc_2009 = cdu_k_inc, spd_inc_2009 = spd_k_inc, fdp_inc_2009 = fdp_k_inc, gru_inc_2009 = gru_k_inc, lin_inc_2009 = pds_k_inc,
                                                                               cdsu_inc_u_2009 = cdu_k_inc_u, spd_inc_u_2009 = spd_k_inc_u, fdp_inc_u_2009 = fdp_k_inc_u, gru_inc_u_2009 = gru_k_inc_u, lin_inc_u_2009 = pds_k_inc_u)
incumbency.dat.sub.2009$cdsu_inc_2009[is.na(incumbency.dat.sub.2009$cdsu_inc_2009)] <- incumbency.dat.sub.2009$csu_k_inc[is.na(incumbency.dat.sub.2009$cdsu_inc_2009)]
incumbency.dat.sub.2009$csu_k_inc <- NULL
incumbency.dat.sub.2009$year <- NULL
incumbency.dat.sub.2009 <- transmute(incumbency.dat.sub.2009, wkr_nummer = wkr_nummer, cdsu_inc_2009 = cdsu_inc_2009 + cdsu_inc_u_2009, spd_inc_2009 = spd_inc_2009 + spd_inc_u_2009, fdp_inc_2009 = fdp_inc_2009 + fdp_inc_u_2009, gru_inc_2009 = gru_inc_2009 + gru_inc_u_2009, lin_inc_2009 = lin_inc_2009 + lin_inc_u_2009)
incumbency.dat.sub.2009[is.na(incumbency.dat.sub.2009)] <- 0

# prepare 2013 data
incumbency.dat.sub.2013 <- filter(incumbency.dat.sub, year == 2013) %>% rename(cdsu_inc_2013 = cdu_k_inc, spd_inc_2013 = spd_k_inc, fdp_inc_2013 = fdp_k_inc, gru_inc_2013 = gru_k_inc, lin_inc_2013 = pds_k_inc,
                                                                               cdsu_inc_u_2013 = cdu_k_inc_u, spd_inc_u_2013 = spd_k_inc_u, fdp_inc_u_2013 = fdp_k_inc_u, gru_inc_u_2013 = gru_k_inc_u, lin_inc_u_2013 = pds_k_inc_u)
incumbency.dat.sub.2013$cdsu_inc_2013[is.na(incumbency.dat.sub.2013$cdsu_inc_2013)] <- incumbency.dat.sub.2013$csu_k_inc[is.na(incumbency.dat.sub.2013$cdsu_inc_2013)]
incumbency.dat.sub.2013$csu_k_inc <- NULL
incumbency.dat.sub.2013$year <- NULL
incumbency.dat.sub.2013 <- transmute(incumbency.dat.sub.2013, wkr_nummer = wkr_nummer, cdsu_inc_2013 = cdsu_inc_2013 + cdsu_inc_u_2013, spd_inc_2013 = spd_inc_2013 + spd_inc_u_2013, fdp_inc_2013 = fdp_inc_2013 + fdp_inc_u_2013, gru_inc_2013 = gru_inc_2013 + gru_inc_u_2013, lin_inc_2013 = lin_inc_2013 + lin_inc_u_2013)
incumbency.dat.sub.2013[is.na(incumbency.dat.sub.2013)] <- 0

# merge data
districts.df.merge <- merge(districts.df, incumbency.dat.sub.1994, by.x = "wkr_nr1994", by.y = "wkr_nummer", all.x = TRUE)
districts.df.merge <- merge(districts.df.merge, incumbency.dat.sub.1998, by.x = "wkr_nr1998", by.y = "wkr_nummer", all.x = TRUE)
districts.df.merge <- merge(districts.df.merge, incumbency.dat.sub.2002, by.x = "wkr_nr2002", by.y = "wkr_nummer", all.x = TRUE)
districts.df.merge <- merge(districts.df.merge, incumbency.dat.sub.2005, by.x = "wkr_nr2005", by.y = "wkr_nummer", all.x = TRUE)
districts.df.merge <- merge(districts.df.merge, incumbency.dat.sub.2009, by.x = "wkr_nr2009", by.y = "wkr_nummer", all.x = TRUE)
districts.df.merge <- merge(districts.df.merge, incumbency.dat.sub.2013, by.x = "wkr_nr2013", by.y = "wkr_nummer", all.x = TRUE)


# save data
write.dta(districts.df.merge, "prepared_data/data_districts_1994_2013.dta", convert.factors = "string", version=10)



