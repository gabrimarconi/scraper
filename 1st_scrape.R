
source("scraper.R")

### first ingestion ever 

#find number of pages
webpage <- webpage_i1
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

masterfile_generalised <- bindbindbind
masterfile_generalised$lastseen <- date()
write_rds(masterfile_generalised,"generalised/masterfile_generalised.rds")
