

######################################################################
### functions
######################################################################

#survival_data <- prova2[is.na(prova2$s)==F&prova2$annee_dummy==1&is.na(prova2$annee_dummy)==F,]

calculate_hazard <- function(mydata, myrange) {
  #myrange <- (1:30)/1000
  #mydata <- survival_data
  
  # write function calculate_lollik for calculating the log likelihood for a given hazard rate p
  # input: a data table with the structure of the table modeldata; and an arbitrary hazard rate p
  # output: the estimated log likelihood
  # examples input:
  #mydata <- survival_data
  #p<-.01
  
  calculate_lollik <- function (lollik_data=mydata, p=myrange) {
    #lollik_data <- survival_data
    #p<-.01
    lollik_data$d_na <- is.na(lollik_data$d)
    lollik_data$d[is.na(lollik_data$d)] <- as.numeric(max(lollik_data$date_num) - lollik_data$date_num[is.na(lollik_data$d)])
    term1 <- -sum(log(lollik_data$s))
    term2 <- (log(1-p))*(sum(lollik_data$d))
    term3 <- length(lollik_data$d[lollik_data$d_na==F])*log(p)
    term4 <- length(lollik_data$d[lollik_data$d_na==T])*log(1-p)
    term5 <- sum(sapply(1:length(lollik_data$s), function(i) {log(sum(sapply(1:lollik_data$s[i], function(j) {(1-p)^j})))}))
    term6 <- sum(sapply(lollik_data$r[is.na(lollik_data$r)==F], function(myr) {log(sum(sapply(1:myr, function(k) {(1-p)^k})))}))
    lollik <- term1 + term2 + term3 + term4 + term5 + term6
    return(lollik)
  }
  
  # apply the function calculate_lollik in the range 0-0.02 and pick the hazard rate p that maximises the log-likelihood
  lollik_list <- do.call(rbind, lapply(myrange, calculate_lollik, lollik_data=mydata) )
  pstar <- myrange[match(as.numeric(max(lollik_list)), as.numeric(lollik_list))]
  return(pstar)
}
  

# write the same function, but based on tabulations

calculate_hazard_fast <- function(mydata, myrange) {
  #myrange <- (50:150)/10000
  #mydata <- survival_data
  
  # write function calculate_lollik for calculating the log likelihood for a given hazard rate p
  # input: a data table with the structure of the table modeldata; and an arbitrary hazard rate p
  # output: the estimated log likelihood
  # examples input:
  #mydata <- survival_data
  #p<-.01
  
  calculate_lollik <- function (p) {
    #p<-.01
    lollik_data <- mydata # NB this can easily be turned into a function argument
    lollik_data$d_na <- is.na(lollik_data$d)
    lollik_data$d[is.na(lollik_data$d)] <- as.numeric(max(lollik_data$date_num) - lollik_data$date_num[is.na(lollik_data$d)])
    table_temp <- as.data.frame(table(lollik_data$s))
    table_s <- data.frame(s=as.numeric(levels(table_temp$Var1)), ads=as.numeric(table_temp$Freq))
    table_temp <- as.data.frame(table(lollik_data$r))
    table_r <- data.frame(r=as.numeric(levels(table_temp$Var1)), ads=as.numeric(table_temp$Freq))
    table_temp <- as.data.frame(table(lollik_data$d))
    table_d <- data.frame(d=as.numeric(levels(table_temp$Var1)), ads=as.numeric(table_temp$Freq))
    d_na <- sum(lollik_data$d_na)
    term1 <- -sum(log(table_s$s)*table_s$ads) # done
    term2 <- (log(1-p))*(sum(table_d$d*table_d$ads)) # done
    term3 <- (sum(table_d$ads)-d_na)*log(p) #done
    term4 <- d_na*log(1-p) # done
    term5 <- sum(sapply(1:length(table_s$s), function(i) {table_s$ads[i]*log(sum(sapply(1:table_s$s[i], function(j) {(1-p)^j})))})) # done
    term6 <- sum(sapply(table_r$r, function(myr) {table_r$ads[table_r$r==myr]*log(sum(sapply(1:myr, function(k) {(1-p)^k})))})) # done
    
    lollik <- term1 + term2 + term3 + term4 + term5 + term6
    return(lollik)
  }
  
  # apply the function calculate_lollik in the range 0-0.02 and pick the hazard rate p that maximises the log-likelihood
  lollik_list <- do.call(rbind, lapply(myrange, calculate_lollik) )
  pstar <- myrange[match(as.numeric(max(lollik_list)), as.numeric(lollik_list))]
  return(pstar)
}



 
# write function to calculate weights

# calculate proportion of ads lost during a censoring interval of length s
calculate_w <- function(mydata, pc) {
  temp_term <- sapply((1:length(mydata$s)), function (i) {sum(sapply(1:mydata$s[i], function(j) {sum(sapply(j:mydata$s[i], function(q) {(1-pc)^(mydata$s[i]-q)}))}))})
  myoutput <- 1 / (1 - (pc * temp_term / mydata$s) )
  return(myoutput)
}

# calculate proportions of ads, wieghted and unweighted
calculate_w_fast <- function(mydata, pc) {
  #mydata<- prova2[prova2$annee_dummy==1,]
  #pc<-r_new
  table_s <- data.frame(s=as.numeric(unique(mydata$s)))
  temp_term <- sapply((1:length(table_s$s)), function (i) {sum(sapply(1:table_s$s[i], function(j) {sum(sapply(j:table_s$s[i], function(q) {(1-pc)^(table_s$s[i]-q)}))}))})
  table_s$myw <- 1 / (1 - (pc * temp_term / table_s$s) )
  temp <- merge(mydata, table_s, all.x = T, by="s")
  myoutput <- temp$myw
  return(myoutput)
}

#survival_data$w <- calculate_w(survival_data, .02)


######################################################################
### montecarlo
######################################################################

# myfrequency can be "daily", "weekly" or "monthly"

myfrequency <- "weekly"
montecarlo_results <- data.frame(new_raw=NA,new_corrected=NA, true_value=NA)

for (iteration in 1:1000) {
  print(paste0("iteration: ", iteration, " ", myfrequency))
  list_new <- lapply(1:100,function (i) {data.frame(new=c(rep(1,rpois(1,33)),rep(0,rpois(1,67))),posted=i)})
  md <- do.call(rbind,list_new)
  md$removed <- as.numeric(NA)
  md$removed <- sapply(1:length(md$removed), function(i) {md$posted[i] + round(rexp(1, rate=(0.0237-md$new[i]*.0079)))})
  real_thing <- mean(md$new)
  dim(md)
  print(paste0("real thing: ", real_thing))
  
  if (myfrequency=="weekly") {
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
    md$posted[md$posted>98&md$posted<100] <- 100
    # update lastseen_num to new scraping intervals
    md$removed[md$removed>105] <- NA
    md$removed[md$removed>100&md$removed<105] <- 105
    md$removed[md$removed>98&md$removed<100] <- 100
    md$removed[md$removed>91&md$removed<98] <- 98
    md$removed[md$removed>84&md$removed<91] <- 91
    md$removed[md$removed>77&md$removed<84] <- 84
    md$removed[md$removed>70&md$removed<77] <- 77
    md$removed[md$removed>63&md$removed<70] <- 70
    md$removed[md$removed>56&md$removed<63] <- 63
    md$removed[md$removed>49&md$removed<56] <- 56
    md$removed[md$removed>42&md$removed<49] <- 49
    md$removed[md$removed>35&md$removed<42] <- 42
    md$removed[md$removed>28&md$removed<35] <- 35
    md$removed[md$removed>21&md$removed<28] <- 28
    md$removed[md$removed>14&md$removed<21] <- 21
    md$removed[md$removed>7&md$removed<14] <- 14
    md$removed[md$removed>0&md$removed<7] <- 7
  }
  
  
  
  if (myfrequency=="monthly") {
    # eliminate ads that appears and disapperas within a fictitious scraping interval
    md <- md[md$posted<=0|md$posted>=30&md$removed<=0|md$removed>=30,]
    md <- md[md$posted<=30|md$posted>=60&md$removed<=30|md$removed>=60,]
    md <- md[md$posted<=60|md$posted>=90&md$removed<=60|md$removed>=90,]
    md <- md[md$posted<=90|md$posted>=105&md$removed<=90|md$removed>=105,]
    # update date_num to new scraping intervals
    md$posted[md$posted>0&md$posted<30] <- 30
    md$posted[md$posted>30&md$posted<60] <- 60
    md$posted[md$posted>60&md$posted<90] <- 90
    md$posted[md$posted>90&md$posted<100] <- 100
    # update lastseen_num to new scraping intervals
    md$removed[md$removed>105] <- NA
    md$removed[md$removed>100&md$removed<105] <- 105
    md$removed[md$removed>90&md$removed<100] <- 100
    md$removed[md$removed>60&md$removed<90] <- 90
    md$removed[md$removed>30&md$removed<60] <- 60
    md$removed[md$removed>0&md$removed<30] <- 30
    }
  
  
  dim(md)
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
  seemaster$previous_date[is.na(seemaster$previous_date)] <- 0
  seemaster$previous_lastseen[is.na(seemaster$lastseen_num)] <- NA
  max_date <- max(dates_list, na.rm = T) # generate an object equal to the number of days between the first and the last scrapings occurred
  
  ### generate s, d and r
  
  seemaster$s <- seemaster$date_num - seemaster$previous_date
  seemaster$d <- seemaster$previous_lastseen - seemaster$previous_date
  seemaster$r <- seemaster$lastseen_num - seemaster$previous_lastseen
  table(seemaster$r, useNA = "always")
  mean(seemaster$s, na.rm = T)
  
  # table 1
  
  prova2 <- seemaster
  table1row1 <- c(mean(prova2$s[prova2$annee_dummy==1], na.rm = T), mean(prova2$s[prova2$annee_dummy==0], na.rm = T))
  table1row2 <- c(mean(prova2$r[prova2$annee_dummy==1], na.rm = T), mean(prova2$r[prova2$annee_dummy==0], na.rm = T) )
  table1row3 <- c(mean(prova2$d[prova2$annee_dummy==1], na.rm = T), mean(prova2$d[prova2$annee_dummy==0], na.rm = T) )
  table1 <- rbind(table1row1, table1row2, table1row3)
  rownames(table1) <- c("s","r","d")
  colnames(table1) <- c("new","old")
  print(table1)
  
  ### maximise the likelihood function for new and old apartments
  
  range_r <- (50:150)/10000
  
  survival_data <- prova2[is.na(prova2$s)==F&prova2$annee_dummy==1&is.na(prova2$annee_dummy)==F,]
  dim(survival_data)
  r_new <- calculate_hazard_fast(survival_data, range_r)
  print(r_new)
  
  survival_data <- prova2[is.na(prova2$s)==F&prova2$annee_dummy==0&is.na(prova2$annee_dummy)==F,]
  r_old <- calculate_hazard_fast(survival_data, range_r)
  print(r_old)
  
  
  ### compare weighted and un-weighted estimates
  
  # proportion before correction for content-removal bias
  raw_new <- mean(prova2$annee_dummy, na.rm = T)
  raw_new
  sd(prova2$annee_dummy, na.rm = T)/sqrt(length(prova2$annee_dummy[is.na(prova2$annee_dummy)==F]))
  
  
  # generate weigths
  prova2$w <- NA
  prova2$w[prova2$annee_dummy==1] <- calculate_w_fast(prova2[prova2$annee_dummy==1,], r_new)
  prova2$w[prova2$annee_dummy==0] <- calculate_w_fast(prova2[prova2$annee_dummy==0,], r_old)
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
  rownames(table2) <- c("raw", "corrected")
  print(table2)
  
  # update montecarlo_results
  update_montecarlo <- data.frame(new_raw=raw_new, new_corrected=corrected_new, true_value=real_thing)
  montecarlo_results <- rbind(montecarlo_results, update_montecarlo)
}

if (myfrequency=="daily") {write.csv(montecarlo_results, "montecarlo_results_daily.csv")}
if (myfrequency=="weekly") {write.csv(montecarlo_results, "montecarlo_results_weekly.csv")}
if (myfrequency=="monthly") {write.csv(montecarlo_results, "montecarlo_results_monthly.csv")}

  
summary(montecarlo_results)  
str(montecarlo_results)

