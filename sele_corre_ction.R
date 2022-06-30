
library(lubridate)
oncein3 <- F
oncein5 <- F
oncein6 <- F

##########################################################################
### open master file and prepare it
##########################################################################


### open master file with scraped and first-time-not-seen (called "lasteen", which unfortunately can lead to some confusion) dates

seemaster <- readRDS("generalised/masterfile_generalised.rds")
class(seemaster$date)
class(seemaster$lastseen)

### convert dates to correct format 
# https://epirhandbook.com/en/working-with-dates.html#messy-dates

seemaster$dateclean <- strptime(seemaster$date, "%a %b %d %H:%M:%S %Y")
seemaster$lastseenclean <- strptime(seemaster$lastseen, "%a %b %d %H:%M:%S %Y")
class(seemaster$dateclean)
class(seemaster$lastseenclean)
unique(floor_date(seemaster$dateclean, unit = "day"))

# cut to January 20 for consistency with original estimation, and transform into once-in-5
seemaster <- seemaster[seemaster$dateclean<strptime("Fri Jan 21 17:30:53 2022", "%a %b %d %H:%M:%S %Y"),]
seemaster$lastseenclean[seemaster$lastseenclean>strptime("Sat Jan 29 17:30:53 2022", "%a %b %d %H:%M:%S %Y"),] <- NA
str(seemaster)
max(seemaster$lastseenclean, na.rm = T)



# re-scale first-seen and first-not-seen as numbers starting with 0 at first scrape
seemaster$date_num <-     round(as.numeric(difftime(seemaster$dateclean,    min(seemaster$dateclean)    , units="days")),0)
seemaster$lastseen_num <- round(as.numeric(difftime(seemaster$lastseenclean,min(seemaster$dateclean, na.rm = T), units="days")),0)

correspondence_date_num <- data.frame( date= unique(floor_date(seemaster$dateclean, unit = "day")) , num=unique(floor(seemaster$date_num))  )

# choose scraping sequence for statistical simulations
if (oncein3==T) {
  # eliminate ads that appears and disapperas within a fictitious scraping interval
  seemaster <- seemaster[seemaster$date_num<=0|seemaster$date_num>=5&seemaster$lastseen_num<=0|seemaster$lastseen_num>=5,]
  seemaster <- seemaster[seemaster$date_num<=5|seemaster$date_num>=14&seemaster$lastseen_num<=5|seemaster$lastseen_num>=14,]
  seemaster <- seemaster[seemaster$date_num<=14|seemaster$date_num>=26&seemaster$lastseen_num<=14|seemaster$lastseen_num>=26,]
  seemaster <- seemaster[seemaster$date_num<=26|seemaster$date_num>=40&seemaster$lastseen_num<=26|seemaster$lastseen_num>=40,]
  seemaster <- seemaster[seemaster$date_num<=40|seemaster$date_num>=56&seemaster$lastseen_num<=40|seemaster$lastseen_num>=56,]
  seemaster <- seemaster[seemaster$date_num<=56|seemaster$date_num>=71&seemaster$lastseen_num<=56|seemaster$lastseen_num>=71,]
  seemaster <- seemaster[seemaster$date_num<=71|seemaster$date_num>=84&seemaster$lastseen_num<=71|seemaster$lastseen_num>=84,]
  seemaster <- seemaster[seemaster$date_num<=84|seemaster$date_num>=95&seemaster$lastseen_num<=84|seemaster$lastseen_num>=95,]
  seemaster <- seemaster[seemaster$date_num<=95|seemaster$date_num>=110&seemaster$lastseen_num<=95|seemaster$lastseen_num>=110,]
  seemaster <- seemaster[seemaster$date_num<=110|seemaster$date_num>=123&seemaster$lastseen_num<=110|seemaster$lastseen_num>=123,]
  seemaster <- seemaster[seemaster$date_num<=123|seemaster$date_num>=141&seemaster$lastseen_num<=123|seemaster$lastseen_num>=141,]
  seemaster <- seemaster[seemaster$date_num<=141|seemaster$date_num>=158&seemaster$lastseen_num<=141|seemaster$lastseen_num>=158,]
  seemaster <- seemaster[seemaster$date_num<=158|seemaster$date_num>=176&seemaster$lastseen_num<=158|seemaster$lastseen_num>=176,]
  seemaster <- seemaster[seemaster$date_num<=176|seemaster$date_num>=196&seemaster$lastseen_num<=176|seemaster$lastseen_num>=196,]
  #seemaster <- seemaster[seemaster$date_num<=196|seemaster$date_num>=210&seemaster$lastseen_num<=196|seemaster$lastseen_num>=210,]
  #seemaster <- seemaster[seemaster$date_num<=210|seemaster$date_num>=217&seemaster$lastseen_num<=210|seemaster$lastseen_num>=217,]
  seemaster <- seemaster[seemaster$date_num<=196|seemaster$date_num>=217&seemaster$lastseen_num<=196|seemaster$lastseen_num>=217,]
  # update date_num to new scraping intervals
  seemaster$date_num[seemaster$date_num>0&seemaster$date_num<5] <- 5
  seemaster$date_num[seemaster$date_num>5&seemaster$date_num<14] <- 14
  seemaster$date_num[seemaster$date_num>14&seemaster$date_num<26] <- 26
  seemaster$date_num[seemaster$date_num>26&seemaster$date_num<40] <- 40
  seemaster$date_num[seemaster$date_num>40&seemaster$date_num<56] <- 56
  seemaster$date_num[seemaster$date_num>56&seemaster$date_num<71] <- 71
  seemaster$date_num[seemaster$date_num>71&seemaster$date_num<84] <- 84
  seemaster$date_num[seemaster$date_num>84&seemaster$date_num<95] <- 95
  seemaster$date_num[seemaster$date_num>95&seemaster$date_num<110] <- 110
  seemaster$date_num[seemaster$date_num>110&seemaster$date_num<123] <- 123
  seemaster$date_num[seemaster$date_num>123&seemaster$date_num<141] <- 141
  seemaster$date_num[seemaster$date_num>141&seemaster$date_num<158] <- 158
  seemaster$date_num[seemaster$date_num>158&seemaster$date_num<176] <- 176
  seemaster$date_num[seemaster$date_num>176&seemaster$date_num<196] <- 196
  #seemaster$date_num[seemaster$date_num>196&seemaster$date_num<210] <- 210
  #seemaster$date_num[seemaster$date_num>210&seemaster$date_num<217] <- 217
  seemaster$date_num[seemaster$date_num>196&seemaster$date_num<217] <- 217
  # update lastseen_num to new scraping intervals
  seemaster$lastseen_num[seemaster$lastseen_num>=217] <- 225
  seemaster$lastseen_num[seemaster$lastseen_num>=196&seemaster$lastseen_num<217] <- 217
  #seemaster$lastseen_num[seemaster$lastseen_num>=210&seemaster$lastseen_num<217] <- 217
  #seemaster$lastseen_num[seemaster$lastseen_num>=196&seemaster$lastseen_num<210] <- 210
  seemaster$lastseen_num[seemaster$lastseen_num>=176&seemaster$lastseen_num<196] <- 196
  seemaster$lastseen_num[seemaster$lastseen_num>=158&seemaster$lastseen_num<176] <- 176
  seemaster$lastseen_num[seemaster$lastseen_num>=141&seemaster$lastseen_num<158] <- 158
  seemaster$lastseen_num[seemaster$lastseen_num>=123&seemaster$lastseen_num<141] <- 141
  seemaster$lastseen_num[seemaster$lastseen_num>=110&seemaster$lastseen_num<123] <- 123
  seemaster$lastseen_num[seemaster$lastseen_num>=95&seemaster$lastseen_num<110] <- 110
  seemaster$lastseen_num[seemaster$lastseen_num>=84&seemaster$lastseen_num<95] <- 95
  seemaster$lastseen_num[seemaster$lastseen_num>=71&seemaster$lastseen_num<84] <- 84
  seemaster$lastseen_num[seemaster$lastseen_num>=56&seemaster$lastseen_num<71] <- 71
  seemaster$lastseen_num[seemaster$lastseen_num>=40&seemaster$lastseen_num<56] <- 56
  seemaster$lastseen_num[seemaster$lastseen_num>=26&seemaster$lastseen_num<40] <- 40
  seemaster$lastseen_num[seemaster$lastseen_num>=14&seemaster$lastseen_num<26] <- 26
  seemaster$lastseen_num[seemaster$lastseen_num>=5&seemaster$lastseen_num<14] <- 14
  seemaster$lastseen_num[seemaster$lastseen_num>=0&seemaster$lastseen_num<5] <- 5
}

if (oncein5==T) {
  # eliminate ads that appears and disapperas within a fictitious scraping interval
  seemaster <- seemaster[seemaster$date_num<=0|seemaster$date_num>=11&seemaster$lastseen_num<=0|seemaster$lastseen_num>=11,]
  seemaster <- seemaster[seemaster$date_num<=11|seemaster$date_num>=32&seemaster$lastseen_num<=11|seemaster$lastseen_num>=32,]
  seemaster <- seemaster[seemaster$date_num<=32|seemaster$date_num>=56&seemaster$lastseen_num<=32|seemaster$lastseen_num>=56,]
  seemaster <- seemaster[seemaster$date_num<=56|seemaster$date_num>=81&seemaster$lastseen_num<=56|seemaster$lastseen_num>=81,]
  seemaster <- seemaster[seemaster$date_num<=81|seemaster$date_num>=99&seemaster$lastseen_num<=81|seemaster$lastseen_num>=99,]
  seemaster <- seemaster[seemaster$date_num<=99|seemaster$date_num>=123&seemaster$lastseen_num<=99|seemaster$lastseen_num>=123,]
  seemaster <- seemaster[seemaster$date_num<=123|seemaster$date_num>=153&seemaster$lastseen_num<=123|seemaster$lastseen_num>=153,]
  seemaster <- seemaster[seemaster$date_num<=153|seemaster$date_num>=179&seemaster$lastseen_num<=153|seemaster$lastseen_num>=179,]
  seemaster <- seemaster[seemaster$date_num<=179|seemaster$date_num>=217&seemaster$lastseen_num<=179|seemaster$lastseen_num>=217,]
  # update date_num to new scraping intervals
  seemaster$date_num[seemaster$date_num>0&seemaster$date_num<11] <- 11
  seemaster$date_num[seemaster$date_num>11&seemaster$date_num<32] <- 32
  seemaster$date_num[seemaster$date_num>32&seemaster$date_num<56] <- 56
  seemaster$date_num[seemaster$date_num>56&seemaster$date_num<81] <- 81
  seemaster$date_num[seemaster$date_num>81&seemaster$date_num<99] <- 99
  seemaster$date_num[seemaster$date_num>99&seemaster$date_num<123] <- 123
  seemaster$date_num[seemaster$date_num>123&seemaster$date_num<153] <- 153
  seemaster$date_num[seemaster$date_num>153&seemaster$date_num<179] <- 179
  seemaster$date_num[seemaster$date_num>179&seemaster$date_num<217] <- 217
  # update lastseen_num to new scraping intervals
  seemaster$lastseen_num[seemaster$lastseen_num>=217] <- 225
  seemaster$lastseen_num[seemaster$lastseen_num>=179&seemaster$lastseen_num<217] <- 217
  seemaster$lastseen_num[seemaster$lastseen_num>=153&seemaster$lastseen_num<179] <- 179
  seemaster$lastseen_num[seemaster$lastseen_num>=123&seemaster$lastseen_num<153] <- 153
  seemaster$lastseen_num[seemaster$lastseen_num>=99&seemaster$lastseen_num<123] <- 123
  seemaster$lastseen_num[seemaster$lastseen_num>=81&seemaster$lastseen_num<99] <- 99
  seemaster$lastseen_num[seemaster$lastseen_num>=56&seemaster$lastseen_num<81] <- 81
  seemaster$lastseen_num[seemaster$lastseen_num>=32&seemaster$lastseen_num<56] <- 56
  seemaster$lastseen_num[seemaster$lastseen_num>=11&seemaster$lastseen_num<32] <- 32
  seemaster$lastseen_num[seemaster$lastseen_num>=0&seemaster$lastseen_num<11] <- 11
}

if (oncein6==T) {
  # eliminate ads that appears and disapperas within a fictitious scraping interval
  seemaster <- seemaster[seemaster$date_num<=0|seemaster$date_num>=14&seemaster$lastseen_num<=0|seemaster$lastseen_num>=14,]
  seemaster <- seemaster[seemaster$date_num<=14|seemaster$date_num>=40&seemaster$lastseen_num<=14|seemaster$lastseen_num>=40,]
  seemaster <- seemaster[seemaster$date_num<=40|seemaster$date_num>=71&seemaster$lastseen_num<=40|seemaster$lastseen_num>=71,]
  seemaster <- seemaster[seemaster$date_num<=71|seemaster$date_num>=95&seemaster$lastseen_num<=71|seemaster$lastseen_num>=95,]
  seemaster <- seemaster[seemaster$date_num<=95|seemaster$date_num>=123&seemaster$lastseen_num<=95|seemaster$lastseen_num>=123,]
  seemaster <- seemaster[seemaster$date_num<=123|seemaster$date_num>=158&seemaster$lastseen_num<=123|seemaster$lastseen_num>=158,]
  seemaster <- seemaster[seemaster$date_num<=158|seemaster$date_num>=196&seemaster$lastseen_num<=158|seemaster$lastseen_num>=196,]
  seemaster <- seemaster[seemaster$date_num<=196|seemaster$date_num>=217&seemaster$lastseen_num<=196|seemaster$lastseen_num>=217,]
  # update date_num to new scraping intervals
  seemaster$date_num[seemaster$date_num>0&seemaster$date_num<14] <- 14
  seemaster$date_num[seemaster$date_num>14&seemaster$date_num<40] <- 40
  seemaster$date_num[seemaster$date_num>40&seemaster$date_num<71] <- 71
  seemaster$date_num[seemaster$date_num>71&seemaster$date_num<95] <- 95
  seemaster$date_num[seemaster$date_num>95&seemaster$date_num<123] <- 123
  seemaster$date_num[seemaster$date_num>123&seemaster$date_num<158] <- 158
  seemaster$date_num[seemaster$date_num>158&seemaster$date_num<196] <- 196
  seemaster$date_num[seemaster$date_num>196&seemaster$date_num<217] <- 217
  # update lastseen_num to new scraping intervals
  seemaster$lastseen_num[seemaster$lastseen_num>=217] <- 225
  seemaster$lastseen_num[seemaster$lastseen_num>=196&seemaster$lastseen_num<217] <- 217
  seemaster$lastseen_num[seemaster$lastseen_num>=158&seemaster$lastseen_num<196] <- 196
  seemaster$lastseen_num[seemaster$lastseen_num>=123&seemaster$lastseen_num<158] <- 158
  seemaster$lastseen_num[seemaster$lastseen_num>=95&seemaster$lastseen_num<123] <- 123
  seemaster$lastseen_num[seemaster$lastseen_num>=71&seemaster$lastseen_num<95] <- 95
  seemaster$lastseen_num[seemaster$lastseen_num>=40&seemaster$lastseen_num<71] <- 71
  seemaster$lastseen_num[seemaster$lastseen_num>=14&seemaster$lastseen_num<40] <- 40
  seemaster$lastseen_num[seemaster$lastseen_num>=0&seemaster$lastseen_num<14] <- 14
}





### generate a variable with the previous dates (last-not-seen and last-seen), step by step
dates_list <- unique(c(unique(seemaster$date_num), unique(seemaster$lastseen_num))) # generate list of ingestion dates
dates_list <- dates_list[order(dates_list)]
assign_previous <- function(i) { # generate function to extract a date from that list
  if (is.na(i)==F & i!=1) {dates_list[i-1]} else {NA}
}
assign_previous(2) # try out function
date_ordinal <- match(seemaster$date_num,dates_list) # identify the position in dates_list of the previous date, respect to the one cointained in $date
lastseen_ordinal <- match(seemaster$lastseen_num,dates_list) # identify the position in lastseen_list of the next $lastseen
seemaster$previous_date     <- apply(as.matrix(date_ordinal)    ,1,assign_previous) # assign the previous date to each row
seemaster$previous_lastseen <- apply(as.matrix(lastseen_ordinal),1,assign_previous) # assign the previous date to each row
max_date <- max(dates_list, na.rm = T) # generate an object equal to the number of days between the first and the last scrapings occurred

### re-assign dates with following structure:
# last-time-not-seen: 0
# first-time-seen: i
# time elapsed between first-time-seen and last-time-seen: h
# last-time-seen: 0
# first-time-not-seen: k

seemaster$i <- seemaster$date_num - seemaster$previous_date
seemaster$h <- seemaster$previous_lastseen - seemaster$date_num
seemaster$h[is.na(seemaster$h)==T] <- max_date - seemaster$date_num[is.na(seemaster$h)==T]
seemaster$k <- seemaster$lastseen_num - seemaster$previous_lastseen
seemaster$d <- seemaster$h + seemaster$i
mean(seemaster$h, na.rm = T)


##########################################################################
### set up log-likelihood functions and calculate overall hazard rate
##########################################################################

# save data table as survival_data
survival_data <- seemaster[is.na(seemaster$i)==F,]
dim(survival_data)
mean(survival_data$i)


### write functions to calculate the log-likelihood

# write three functions calculating the three elements of the log-likelihood function for a given observation j and hazard rate r
#r <- .99
#j <- 10000
calculate_loglik_start <- function(j,r) {
  -log(survival_data$i[j]) + log(sum(sapply(-survival_data$i[j]:0, function(x) {r^x})))
}
calculate_loglik_end <- function(j,r) {
  if (is.na(survival_data$k[j])==F) {log(sum(sapply(0:survival_data$k[j], function(x) {r^x})))} else {log(1-r)}
}
calculate_loglik_middle <- function(j,r) {
  (survival_data$h[j]-1)*log(r) + log(1-r)
}
calculate_loglik_start(10000, .99)
calculate_loglik_end(10000, .99)
calculate_loglik_middle(10000, .99)
survival_data[10000,]

# write function applying the three previous functions to the whole dataset and summing up the results, thus calculating the log-likelihood function

calculate_great_loglik <- function(survrate) {
  #survrate <- .88
  loglik_start_tot  <- sum(sapply(1:dim(survival_data)[1],calculate_loglik_start, r=survrate))
  loglik_middle_tot <- sum(sapply(1:dim(survival_data)[1],calculate_loglik_middle, r=survrate))
  loglik_end_tot    <- sum(sapply(1:dim(survival_data)[1],calculate_loglik_end, r=survrate))
  great_loglik <- loglik_start_tot + loglik_middle_tot + loglik_end_tot
  return(great_loglik)
}

calculate_great_loglik(survrate = .99)

### log-likelihood maximisation for all ads in the dataset

#lik_series <- sapply(((0:1000)/10000+.9),calculate_great_loglik)
#lik_plot <- data.frame(lik=lik_series, r=((0:1000)/10000+.9))
##lik_plot <- readRDS("lik_plot.rds")
#plot(lik_plot$r,lik_plot$lik)
#r_all <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
#saveRDS(lik_plot,"lik_plot.rds")



######################################################################
### generate table with the additional info from the scraping
######################################################################

# find the list of files with the data from the detailed scraping
tablelist <- list.files(path = "C:/Users/Gabriele.Marconi/Documents/Rprojects/scraper/generalised", pattern = "^scrape")

# open and append them
tableset <- lapply(paste0("generalised/",tablelist), readRDS)
length(tableset)
mytable <- do.call(rbind,tableset)
#View(mytable)

# generate a few clean variables
mytable$m2clean <- as.numeric(sapply(strsplit(mytable$m2,".", fixed = T), `[`, 1))
mytable$priceclean <- as.numeric(gsub("[^0-9]", "", mytable$price))
mytable$priceperm2 <- mytable$priceclean/mytable$m2clean
mytable$anneeclean <- as.numeric(mytable$annee)

### merge with scraping date information to explore relationships between time online and year of construction
prova <- merge(mytable, seemaster[,c("link", "h")])
plot(prova$anneeclean[prova$anneeclean>1900], prova$h[prova$anneeclean>1900])
summary(lm(h~anneeclean,data = prova[prova$anneeclean>1900,]))
summary(prova$anneeclean[prova$anneeclean>1900])
summary(prova$anneeclean)
mean(prova$h[prova$anneeclean<1990], na.rm = T)
mean(prova$h[prova$anneeclean>1989&prova$anneeclean<2000], na.rm = T)
mean(prova$h[prova$anneeclean>1999&prova$anneeclean<2010], na.rm = T)
mean(prova$h[prova$anneeclean>2009&prova$anneeclean<2020], na.rm = T)
mean(prova$h[prova$anneeclean>2019], na.rm = T)

### generate dummy variable on year of construction (new=1)
prova$annee_dummy <- as.numeric(prova$anneeclean>2019)
summary(prova$annee_dummy)
mean(prova$h[prova$annee_dummy==1], na.rm = T)
mean(prova$h[prova$annee_dummy==0], na.rm = T)

### log-likelihood maximisation for new apartments/houses

# merge master file with dummy on year on construction
prova2 <- merge(seemaster, prova[,c("link", "annee_dummy")])
table1row1 <- c(mean(prova2$i[prova2$annee_dummy==1], na.rm = T), mean(prova2$i[prova2$annee_dummy==0], na.rm = T))
table1row2 <- c(mean(prova2$k[prova2$annee_dummy==1], na.rm = T), mean(prova2$k[prova2$annee_dummy==0], na.rm = T) )
table1row3 <- c(mean(prova2$d[prova2$annee_dummy==1], na.rm = T), mean(prova2$d[prova2$annee_dummy==0], na.rm = T) )
table1 <- rbind(table1row1, table1row2, table1row3)
rownames(table1) <- c("s","r","d")
colnames(table1) <- c("new","old")
table1


# maximise the likelihood function, explore and store results
survival_data <- prova2[is.na(prova2$i)==F&prova2$annee_dummy==1&is.na(prova2$annee_dummy)==F,]
dim(survival_data)
lik_series <- sapply(((0:1000)/10000+.9),calculate_great_loglik)
lik_plot <- data.frame(lik=lik_series, r=((0:1000)/10000+.9))
#lik_plot <- readRDS("lik_plot_new.rds")
plot(lik_plot$r,lik_plot$lik)
r_new <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
saveRDS(lik_plot,"lik_plot_new.rds")
1-r_new

### log-likelihood maximisation for new apartments/houses

# maximise the likelihood function, explore and store results

survival_data <- prova2[is.na(prova2$i)==F&prova2$annee_dummy==0&is.na(prova2$annee_dummy)==F,]
dim(survival_data)
mean(survival_data$i, na.rm = T)
lik_series <- sapply(((0:1000)/10000+.9),calculate_great_loglik)
lik_plot <- data.frame(lik=lik_series, r=((0:1000)/10000+.9))
#lik_plot <- readRDS("lik_plot_old.rds")
plot(lik_plot$r,lik_plot$lik)
r_old <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
saveRDS(lik_plot,"lik_plot_old.rds")
1-r_old

######################################################################
### estimate proportion of ads for new apartments / houses
######################################################################

# proportion before correction for content-removal bias
prova2 <- prova2[is.na(prova2$i)==F & is.na(prova2$annee_dummy)==F,]
raw_new <- mean(prova2$annee_dummy, na.rm = T)
raw_new
sd(prova2$annee_dummy, na.rm = T)/sqrt(length(prova2$annee_dummy[is.na(prova2$annee_dummy)==F]))


### estimate weights for removing the bias

# write function to calculate weights
calculate_weight <- function(i,r) {
  (i) / sum(sapply(1:i, function(x) {r^x}))
}
calculate_weight(3,.99)

# apply the function and generate weigths
prova2$w <- NA
prova2$w[prova2$annee_dummy==1] <- sapply(prova2$i[prova2$annee_dummy==1],calculate_weight, r=r_new)
prova2$w[prova2$annee_dummy==0] <- sapply(prova2$i[prova2$annee_dummy==0],calculate_weight, r=r_old)
table(prova2$w[prova2$annee_dummy==1], useNA = "always")
table(prova2$w[prova2$annee_dummy==0], useNA = "always")
mean(prova2$w[prova2$annee_dummy==1], na.rm = T)
mean(prova2$w[prova2$annee_dummy==0], na.rm = T)

# proportion after correction for content-removal bias, and standard error of the difference between averages
corrected_new <- sum(prova2$annee_dummy*prova2$w, na.rm = T) / sum(prova2$w, na.rm = T)
corrected_new
sd(prova2$annee_dummy*(prova2$w - 1), na.rm = T) / sqrt(length(prova2$w[is.na(prova2$annee_dummy*(prova2$w - 1))==F]))


# -> the estimated proportion of new apartments decreases slightly (from .340 to .336) as a result of the correction. this was expected, because new constructions last longer on the website so they need a smaller upward adjustment when correcting for content-removal

# ratio of total number of ads after vs before correction
# all ads
sum(prova2$w, na.rm = T)/length(prova2$w[is.na(prova2$w)==F])
# new apts
sum(prova2$annee_dummy*prova2$w, na.rm = T)/length(prova2$w[is.na(prova2$w)==F&prova2$annee_dummy==1])
# old apts
sum((1-prova2$annee_dummy)*prova2$w, na.rm = T)/length(prova2$w[is.na(prova2$w)==F&prova2$annee_dummy==0])  

table2 <- data.frame(new=c(raw_new,corrected_new), old=c(1-raw_new,1-corrected_new))


######################################################################
### paper tables
######################################################################

table1
table2


######################################################################
### montecarlo
######################################################################

weekly <- T
monthly <- F


table2_montecarlo <- function() {
  list_new <- lapply(1:100,function (i) {data.frame(new=c(rep(1,rpois(1,33)),rep(0,rpois(1,67))),posted=i)})
  md <- do.call(rbind,list_new)
  md$removed <- as.numeric(NA)
  md$removed <- sapply(1:length(md$removed), function(i) {md$posted[i] + round(rexp(1, rate=(0.0237-md$new[i]*.0079)))})
  dim(md)
  
  if (weekly==T) {
    # eliminate ads that appears and disapperas within a fictitious scraping interval
    md <- md[md$posted<=0|md$posted>=7&md$removed<=0|md$removed>=7,]
    md <- md[md$posted<=7|md$posted>=14&md$removed<=7|md$removed>=14,]
    md <- md[md$posted<=14|md$posted>=21&md$removed<=14|md$removed>=21,]
    md <- md[md$posted<=21|md$posted>=28&md$removed<=21|md$removed>=28,]
    md <- md[md$posted<=28|md$posted>=35&md$removed<=28|md$removed>=35,]
    md <- md[md$posted<=35|md$posted>=42&md$removed<=35|md$removed>=42,]
    md <- md[md$posted<=42|md$posted>=49&md$removed<=42|md$removed>=49,]
    md <- md[md$posted<=49|md$posted>=56&md$removed<=49|md$removed>=56,]
    md <- md[md$posted<=56|md$posted>=63&md$removed<=56|md$removed>=63,]
    md <- md[md$posted<=63|md$posted>=70&md$removed<=63|md$removed>=70,]
    md <- md[md$posted<=70|md$posted>=77&md$removed<=70|md$removed>=77,]
    md <- md[md$posted<=77|md$posted>=84&md$removed<=77|md$removed>=84,]
    md <- md[md$posted<=84|md$posted>=91&md$removed<=84|md$removed>=91,]
    md <- md[md$posted<=91|md$posted>=98&md$removed<=91|md$removed>=98,]
    md <- md[md$posted<=98|md$posted>=105&md$removed<=98|md$removed>=105,]
    # update date_num to new scraping intervals
    md$posted[md$posted>0&md$posted<7] <- 7
    md$posted[md$posted>7&md$posted<14] <- 14
    md$posted[md$posted>14&md$posted<21] <- 21
    md$posted[md$posted>21&md$posted<28] <- 28
    md$posted[md$posted>28&md$posted<35] <- 35
    md$posted[md$posted>35&md$posted<42] <- 42
    md$posted[md$posted>42&md$posted<49] <- 49
    md$posted[md$posted>49&md$posted<56] <- 56
    md$posted[md$posted>56&md$posted<63] <- 63
    md$posted[md$posted>63&md$posted<70] <- 70
    md$posted[md$posted>70&md$posted<77] <- 77
    md$posted[md$posted>77&md$posted<84] <- 84
    md$posted[md$posted>84&md$posted<91] <- 91
    md$posted[md$posted>91&md$posted<98] <- 98
    md$posted[md$posted>98&md$posted<105] <- 105
    # update lastseen_num to new scraping intervals
    md$removed[md$removed>=98] <- 105
    md$removed[md$removed>=91&md$removed<98] <- 98
    md$removed[md$removed>=84&md$removed<91] <- 91
    md$removed[md$removed>=77&md$removed<84] <- 84
    md$removed[md$removed>=70&md$removed<77] <- 77
    md$removed[md$removed>=63&md$removed<70] <- 70
    md$removed[md$removed>=56&md$removed<63] <- 63
    md$removed[md$removed>=49&md$removed<56] <- 56
    md$removed[md$removed>=42&md$removed<49] <- 49
    md$removed[md$removed>=35&md$removed<42] <- 42
    md$removed[md$removed>=28&md$removed<35] <- 35
    md$removed[md$removed>=21&md$removed<28] <- 28
    md$removed[md$removed>=14&md$removed<21] <- 21
    md$removed[md$removed>=7&md$removed<14] <- 14
    md$removed[md$removed>=0&md$removed<7] <- 7
  }
  
  dim(md)
  md$removed[md$removed>105] <- NA
  seemaster <- md
  colnames(seemaster) <- c("annee_dummy","date_num", "lastseen_num")
  
  
  ### generate a variable with the previous dates (last-not-seen and last-seen), step by step
  dates_list <- unique(c(unique(seemaster$date_num), unique(seemaster$lastseen_num))) # generate list of ingestion dates
  dates_list <- dates_list[order(dates_list)]
  assign_previous <- function(i) { # generate function to extract a date from that list
    if (is.na(i)==F & i!=1) {dates_list[i-1]} else {NA}
  }
  assign_previous(2) # try out function
  date_ordinal <- match(seemaster$date_num,dates_list) # identify the position in dates_list of the previous date, respect to the one cointained in $date
  lastseen_ordinal <- match(seemaster$lastseen_num,dates_list) # identify the position in lastseen_list of the next $lastseen
  seemaster$previous_date     <- apply(as.matrix(date_ordinal)    ,1,assign_previous) # assign the previous date to each row
  seemaster$previous_lastseen <- apply(as.matrix(lastseen_ordinal),1,assign_previous) # assign the previous date to each row
  max_date <- max(dates_list, na.rm = T) # generate an object equal to the number of days between the first and the last scrapings occurred
  
  ### generate i, h and k
  
  seemaster$i <- seemaster$date_num - seemaster$previous_date
  seemaster$h <- seemaster$previous_lastseen - seemaster$date_num
  seemaster$h[is.na(seemaster$h)==T] <- max_date - seemaster$date_num[is.na(seemaster$h)==T]
  seemaster$k <- seemaster$lastseen_num - seemaster$previous_lastseen
  seemaster$d <- seemaster$h + seemaster$i
  mean(seemaster$i, na.rm = T)
  
  # table 1
  
  prova2 <- seemaster
  table1row1 <- c(mean(prova2$i[prova2$annee_dummy==1], na.rm = T), mean(prova2$i[prova2$annee_dummy==0], na.rm = T))
  table1row2 <- c(mean(prova2$k[prova2$annee_dummy==1], na.rm = T), mean(prova2$k[prova2$annee_dummy==0], na.rm = T) )
  table1row3 <- c(mean(prova2$d[prova2$annee_dummy==1], na.rm = T), mean(prova2$d[prova2$annee_dummy==0], na.rm = T) )
  table1 <- rbind(table1row1, table1row2, table1row3)
  rownames(table1) <- c("s","r","d")
  colnames(table1) <- c("new","old")
  print(table1)
  
  ### maximise the likelihood function for new and old apartments
  
  range_r <- ((0:40)/1000+.94)
  
  survival_data <- prova2[is.na(prova2$i)==F&prova2$annee_dummy==1&is.na(prova2$annee_dummy)==F,]
  dim(survival_data)
  lik_series <- sapply(range_r,calculate_great_loglik)
  lik_plot <- data.frame(lik=lik_series, r=range_r)
  #lik_plot <- readRDS("lik_plot_new.rds")
  plot(lik_plot$r,lik_plot$lik)
  r_new <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
  saveRDS(lik_plot,"lik_plot_new.rds")
  print(1-r_new)
  
  survival_data <- prova2[is.na(prova2$i)==F&prova2$annee_dummy==0&is.na(prova2$annee_dummy)==F,]
  dim(survival_data)
  mean(survival_data$i, na.rm = T)
  lik_series <- sapply(range_r,calculate_great_loglik)
  lik_plot <- data.frame(lik=lik_series, r=range_r)
  #lik_plot <- readRDS("lik_plot_old.rds")
  plot(lik_plot$r,lik_plot$lik)
  r_old <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
  saveRDS(lik_plot,"lik_plot_old.rds")
  print(1-r_old)
  
  
  ### compare weighted and un-weighted estimates
  
  # proportion before correction for content-removal bias
  prova2 <- prova2[is.na(prova2$i)==F & is.na(prova2$annee_dummy)==F,]
  raw_new <- mean(prova2$annee_dummy, na.rm = T)
  raw_new
  sd(prova2$annee_dummy, na.rm = T)/sqrt(length(prova2$annee_dummy[is.na(prova2$annee_dummy)==F]))
  
  
  # generate weigths
  prova2$w <- NA
  prova2$w[prova2$annee_dummy==1] <- sapply(prova2$i[prova2$annee_dummy==1],calculate_weight, r=r_new)
  prova2$w[prova2$annee_dummy==0] <- sapply(prova2$i[prova2$annee_dummy==0],calculate_weight, r=r_old)
  table(prova2$w[prova2$annee_dummy==1], useNA = "always")
  table(prova2$w[prova2$annee_dummy==0], useNA = "always")
  mean(prova2$w[prova2$annee_dummy==1], na.rm = T)
  mean(prova2$w[prova2$annee_dummy==0], na.rm = T)
  
  # proportion after correction for content-removal bias, and standard error of the difference between averages
  corrected_new <- sum(prova2$annee_dummy*prova2$w, na.rm = T) / sum(prova2$w, na.rm = T)
  corrected_new
  sd(prova2$annee_dummy*(prova2$w - 1), na.rm = T) / sqrt(length(prova2$w[is.na(prova2$annee_dummy*(prova2$w - 1))==F]))
  
  
  # table2
  
  table2 <- data.frame(new=c(raw_new,corrected_new), old=c(1-raw_new,1-corrected_new))
  print(table2)
  return(table2)
}

table2_montecarlo()
