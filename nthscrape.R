
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
dim(bindbindbind)
dim(masterfile)
write_rds(masterfile,"masterfile.rds")

# run scrape me more
numberoflinks <- 1:dim(bindbindbind)[1]
#head(scrapememore(7))
templist <- lapply(numberoflinks,scrapememore)
youscrapedit <- do.call("rbind",templist)
#View(youscrapedit)

# compile scraped dataset
thatsit <- cbind(bindbindbind,youscrapedit)

#create indexscore and sort by it
thatsit$indexscore <- 1
thatsit$indexscore <- thatsit$indexscore - ifelse(rowSums(is.na(thatsit))>1, .05, 0)
thatsit$indexscore <- thatsit$indexscore - ifelse(rowSums(is.na(thatsit))>3, .05, 0)
thatsit$indexscore <- thatsit$indexscore - ifelse(rowSums(is.na(thatsit))>4, .05, 0)
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$cents!=TRUE&thatsit$ville!=TRUE,.2,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$ville==TRUE,.05,0)
thatsit$chambre[is.na(thatsit$chambre)==TRUE] <- -1
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$chambre!=3&thatsit$chambre!=4&thatsit$chambre!=5&thatsit$chambre!=2&thatsit$chambre!=-1,.2,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$chambre==2,.1,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$chambre==-1,.05,0)
thatsit$terrasse[is.na(thatsit$terrasse)==TRUE] <- -1
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$terrasse!=1&thatsit$terrasse!=-1,.05,0)
thatsit$chauss[is.na(thatsit$chauss)==TRUE] <- -1
thatsit$ground[is.na(thatsit$ground)==TRUE] <- -1
thatsit$etage[is.na(thatsit$etage)==TRUE] <- -1
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$chauss!=1&thatsit$ground!=1&thatsit$etage!=0,.1,0)
thatsit$garage[is.na(thatsit$garage)==TRUE] <- -1
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$garage!=1&thatsit$garage!=-1,.05,0)
thatsit$annee[is.na(thatsit$annee)==TRUE] <- -1
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$annee<2000 & thatsit$annee!=-1,.1,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$annee<1990 & thatsit$annee!=-1,.05,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(thatsit$annee<1980 & thatsit$annee!=-1,.05,0)
thatsit$price <- str_trim(thatsit$price)
thatsit$price[is.na(thatsit$price)==TRUE] <- "-99999999"
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(str_trim(thatsit$price))!=7,.2,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(str_trim(thatsit$price))==7 & substr(str_trim(thatsit$price),1,1)==9,.1,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(str_trim(thatsit$price))==9 & substr(str_trim(thatsit$price),1,1)!=1,.2,0)
thatsit$m2[is.na(thatsit$m2)==TRUE] <- "-999"
extractfirst <- function(i) {
  i[1]
}
temp <- as.matrix(lapply(strsplit(thatsit$m2, '[.]'),extractfirst))
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(temp)==1|nchar(temp)==4|is.na(nchar(temp)),.1,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(temp)==2&str_detect(substr(temp,1,1),"([1-6])")==TRUE,.4,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(temp)==2&str_detect(substr(temp,1,1),"7")==TRUE,.2,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(temp)==2&str_detect(substr(temp,1,1),"8")==TRUE,.1,0)
thatsit$indexscore <- thatsit$indexscore - ifelse(nchar(temp)==2&str_detect(substr(temp,1,1),"9")==TRUE,.05,0)
myorder <- order(-thatsit$indexscore)
thatsit <- thatsit[myorder,]


# save data
myrdsfilename <- paste0("scrapedata_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".rds")
mycsvfilename <- paste0("scrapedata_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".csv")
write_rds(thatsit,myrdsfilename)
write_csv(thatsit,mycsvfilename)


