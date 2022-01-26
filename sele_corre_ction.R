
library(lubridate)

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

# re-scale first-seen and first-not-seen as numbers starting with 0 at first scrape
seemaster$date_num <-     round(as.numeric(difftime(seemaster$dateclean,    min(seemaster$dateclean)    , units="days")),0)
seemaster$lastseen_num <- round(as.numeric(difftime(seemaster$lastseenclean,min(seemaster$dateclean, na.rm = T), units="days")),0)

### generate a variable with the previous dates (last-not-seen and last-seen), step by step
dates_list <- unique(seemaster$date_num) # generate list of ingestion dates
assign_previous <- function(i) { # generate function to extract a date from that list
  if (is.na(i)==F & i!=1) {dates_list[i-1]} else {NA}
}
assign_previous(2) # try out function
date_ordinal <- match(seemaster$date_num,dates_list) # identify the position in dates_list of the previous date, respect to the one cointained in $date
lastseen_ordinal <- match(seemaster$lastseen_num,dates_list) # identify the position in lastseen_list of the next $lastseen
seemaster$previous_date     <- apply(as.matrix(date_ordinal)    ,1,assign_previous) # assign the previous date to each row
seemaster$previous_lastseen <- apply(as.matrix(lastseen_ordinal),1,assign_previous) # assign the previous date to each row
max_date <- max(dates_list) # generate an object equal to the number of days between the first and the last scrapings occurred

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
  -log(1+survival_data$i[j]) + log(sum(sapply(-survival_data$i[j]:0, function(x) {r^x})))
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

lik_series <- sapply(((0:1000)/10000+.9),calculate_great_loglik)
lik_plot <- data.frame(lik=lik_series, r=((0:1000)/10000+.9))
#lik_plot <- readRDS("lik_plot.rds")
plot(lik_plot$r,lik_plot$lik)
r_all <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
saveRDS(lik_plot,"lik_plot.rds")



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

# maximise the likelihood function, explore and store results
survival_data <- prova2[is.na(prova2$i)==F&prova2$annee_dummy==1&is.na(prova2$annee_dummy)==F,]
dim(survival_data)
mean(survival_data$i, na.rm = T)
lik_series <- sapply(((0:1000)/10000+.9),calculate_great_loglik)
lik_plot <- data.frame(lik=lik_series, r=((0:1000)/10000+.9))
#lik_plot <- readRDS("lik_plot_new.rds")
plot(lik_plot$r,lik_plot$lik)
r_new <- lik_plot$r[lik_plot$lik==max(lik_plot$lik)]
saveRDS(lik_plot,"lik_plot_new.rds")

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


######################################################################
### estimate proportion of ads for new apartments / houses
######################################################################

# proportion before correction for content-removal bias
prova2 <- prova2[is.na(prova2$i)==F & is.na(prova2$annee_dummy)==F,]
mean(prova2$annee_dummy, na.rm = T)

### estimate weights for removing the bias

# write function to calculate weights
calculate_weight <- function(i,r) {
  (i+1) / sum(sapply(0:i, function(x) {r^x}))
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

# proportion after correction for content-removal bias
sum(prova2$annee_dummy*prova2$w, na.rm = T) / sum(prova2$w, na.rm = T)
# -> the estimated proportion of new apartments decreases slightly (from .340 to .336) as a result of the correction. this was expected, because new constructions last longer on the website so they need a smaller upward adjustment when correcting for content-removal

# ratio of total number of ads after vs before correction
# all ads
sum(prova2$w, na.rm = T)/length(prova2$w[is.na(prova2$w)==F])
# new apts
sum(prova2$annee_dummy*prova2$w, na.rm = T)/length(prova2$w[is.na(prova2$w)==F&prova2$annee_dummy==1])
# old apts
sum((1-prova2$annee_dummy)*prova2$w, na.rm = T)/length(prova2$w[is.na(prova2$w)==F&prova2$annee_dummy==0])  


######################################################################
### generate time series for total number of new apartments / houses
######################################################################


# plot before correcting for content removal
daily_plot <- as.data.frame(table(prova2$date_num))
plot(daily_plot$Var1, daily_plot$Freq)
str(daily_plot)

# generate total weighted count of ads for each date, and link to scraping date and previous date
ads_count <- sapply(prova2$date_num[duplicated(prova2$date_num)==F], function(t) {sum(prova2$w[prova2$date_num==t])})
scrape_counts <- as.data.frame(cbind(prova2$date_num[duplicated(prova2$date_num)==F],  prova2$previous_date[duplicated(prova2$date_num)==F], ads_count))
colnames(scrape_counts) <- c("date_num", "previous_date", "ads_count")

# generate virgin time dataset from 0 to max_date, and merge it with the data frame just created
ts_df <- data.frame(date_num=0:max_date)
ts_df <- merge(ts_df, scrape_counts, all.x = T)

# generate number of days without scrapers before a scraper comes at any time t
ts_df$daily_count <- ts_df$ads_count/(ts_df$date_num-ts_df$previous_date)

# write function to fill a uniform distribution of ads in the periods without scraping (based on the adjusted numbers scraped at time t)
fill_uniform <- function(t) {
  rep(ts_df$daily_count[ts_df$date_num==t], times=ts_df$date_num[ts_df$date_num==t]-ts_df$previous_date[ts_df$date_num==t])
}

# apply the function to each scraping date (except the first, 0)
daily_count_list <- lapply(ts_df$date_num[is.na(ts_df$previous_date)==F], fill_uniform)

# concatenate one NA value (for date 0) and the uniform distributions generated at the previous step, and store this vector as a new variable
ts_df$daily_count <- c(NA, do.call("c",daily_count_list))

plot(ts_df$date_num, ts_df$daily_count)



