
source("scraper.R")

### ingestion with first-stage deduplication

#load masterfile_generalised
masterfile_generalised <- read_rds("generalised/masterfile_generalised.rds")

#find number of pages
webpage <- paste0("https://www.immotop.lu/search/index1.html")
tempdata <- as.data.frame(read_lines(webpage, n_max=-1))
colnames(tempdata) <- "texto"
tempdata$itemsnr <- str_detect(tempdata$texto,"var pInfoCalc=.{1}") 
itemsnr <- tempdata$texto[tempdata$itemsnr==TRUE]
itemsnr <- substr(itemsnr,str_locate(itemsnr, "var pInfoCalc=.{1}")[2]+1,str_locate(itemsnr, "var pInfoCalc=.{1}")[2]+5)
numericitemsnr <- as.numeric(itemsnr)
numberofpages <- 1:ceiling(numericitemsnr/15)

# run scrapeyourchin
templist <- lapply(numberofpages,scrapeyourchin)
for (i in 1:(max(numberofpages)-min(numberofpages))) {
  if (i==1) {bindbindbind <- templist[[i]]} else {bindbindbind <- rbind(bindbindbind,templist[[i]])}
}
dim(bindbindbind)
dim(masterfile_generalised)

# match duplicates
index <- as.data.frame(match(masterfile_generalised$link,bindbindbind$link,nomatch=NA))
masterfile_generalised$lastseen[is.na(index)==FALSE] <- date()
index <- as.data.frame(match(bindbindbind$link,masterfile_generalised$link,nomatch=NA))
bindbindbind <- bindbindbind[is.na(index)==TRUE,]

# merge masterfile_generalised with new data and overwrite
masterfile_generalised <- rbind(masterfile_generalised,bindbindbind)
dim(bindbindbind)
dim(masterfile_generalised)
write_rds(masterfile_generalised,"generalised/masterfile_generalised.rds")

# run scrape me more
numberoflinks <- 1:dim(bindbindbind)[1]
#head(scrapememore(7))
templist <- lapply(numberoflinks,scrapememore)
for (i in 1:length(templist)) {  if(dim(templist[[i]])[2]!=13) {print(i)}  }
youscrapedit <- do.call("rbind",templist)
#View(youscrapedit)


# compile and save scraped dataset
thatsit <- cbind(bindbindbind,youscrapedit)
myrdsfilename <- paste0("generalised/scrape_gen_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".rds")
write_rds(thatsit,myrdsfilename)

dim(thatsit)
thatsit$date[1]
sum(as.numeric(thatsit$emphyt), na.rm = T)
thatsit$link[thatsit$chauss==1&str_detect(thatsit$texto,"LUXEMOURG-")==T]
thatsit$link[thatsit$chambre==3&nchar(thatsit$price)==8&str_detect(thatsit$texto,"LUXEMOURG-")==T]


