
source("scraper.R")

### ingestion with first-stage deduplication

#load masterfile
masterfile <- read_rds("masterfile.rds")

#find number of pages
webpage <- paste0("https://www.immotop.lu/search/index1.html")
tempdata <- as.data.frame(read_lines(webpage, n_max=-1))
colnames(tempdata) <- "texto"
tempdata$itemsnr <- str_detect(tempdata$texto,"var pInfoCalc=.{1}") 
itemsnr <- tempdata$texto[tempdata$itemsnr==TRUE]
itemsnr <- substr(itemsnr,str_locate(itemsnr, "var pInfoCalc=.{1}")[2]+1,str_locate(itemsnr, "var pInfoCalc=.{1}")[2]+5)
numericitemsnr <- as.numeric(itemsnr)
numberofpages <- 1:ceiling(numericitemsnr/15)

# run scrapeyourass
templist <- lapply(numberofpages,scrapeyourass)
for (i in 1:(max(numberofpages)-min(numberofpages))) {
  if (i==1) {bindbindbind <- templist[[i]]} else {bindbindbind <- rbind(bindbindbind,templist[[i]])}
}
dim(bindbindbind)
dim(masterfile)

# match duplicates
index <- as.data.frame(match(masterfile$link,bindbindbind$link,nomatch=NA))
masterfile$lastseen[is.na(index)==FALSE] <- date()
index <- as.data.frame(match(bindbindbind$link,masterfile$link,nomatch=NA))
bindbindbind <- bindbindbind[is.na(index)==TRUE,]

# merge masterfile with new data and overwrite
masterfile <- rbind(masterfile,bindbindbind)
write_rds(masterfile,"masterfile.rds")

# run scrape me more
numberoflinks <- 1:dim(bindbindbind)[1]
#head(scrapememore(7))
templist <- lapply(numberoflinks,scrapememore)
youscrapedit <- do.call("rbind",templist)

#create indexscore
bindbindbind$indexscore <- 1
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(bindbindbind$cents!=TRUE,.2,0)
bindbindbind$chambre[is.na(bindbindbind$chambre)==TRUE] <- -1
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(bindbindbind$chambre!=3,.2,0)
bindbindbind$terrasse[is.na(bindbindbind$terrasse)==TRUE] <- -1
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(bindbindbind$terrasse!=1,.05,0)
bindbindbind$chauss[is.na(bindbindbind$chauss)==TRUE] <- -1
bindbindbind$ground[is.na(bindbindbind$ground)==TRUE] <- -1
bindbindbind$etage[is.na(bindbindbind$etage)==TRUE] <- -1
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(bindbindbind$chauss!=1&bindbindbind$ground!=1&bindbindbind$etage!=0,.1,0)
bindbindbind$garage[is.na(bindbindbind$garage)==TRUE] <- -1
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(bindbindbind$garage!=1,.05,0)
bindbindbind$annee[is.na(bindbindbind$annee)==TRUE] <- -1
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(bindbindbind$annee>1999,.1,0)
bindbindbind$price <- str_trim(bindbindbind$price)
bindbindbind$price[is.na(bindbindbind$price)==TRUE] <- "-9999999"
bindbindbind$indexscore <- bindbindbind$indexscore - ifelse(nchar(bindbindbind$price)>7,.2,0)



# compile, sort and save data
thatsit <- cbind(bindbindbind,youscrapedit)
myorder <- order(-thatsit$indexscore)
thatsit <- thatsit[myorder,]
myrdsfilename <- paste0("scrapedata_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".rds")
mycsvfilename <- paste0("scrapedata_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".csv")
mycsv2filename <- paste0("scrapedatacsv2_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".csv")
write_rds(thatsit,myrdsfilename)
write_csv(thatsit,mycsvfilename)
write_csv2(thatsit,mycsv2filename)