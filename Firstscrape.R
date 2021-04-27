
source("scraper.R")

#find number of pages
webpage <- paste0("https://www.immotop.lu/search/index1.html")
tempdata <- as.data.frame(read_lines(webpage, n_max=-1))
colnames(tempdata) <- "texto"
tempdata$itemsnr <- str_detect(tempdata$texto,"var pInfoCalc=.{1}") 
itemsnr <- tempdata$texto[tempdata$itemsnr==TRUE]
itemsnr <- substr(itemsnr,str_locate(itemsnr, "var pInfoCalc=.{1}")[2]+1,str_locate(itemsnr, "var pInfoCalc=.{1}")[2]+5)
numericitemsnr <- as.numeric(itemsnr)
numberofpages <- 1:ceiling(numericitemsnr/15)


templist <- lapply(numberofpages,scrapeyourass)

for (i in numberofpages) {
  if (i==1) {bindbindbind <- templist[[i]]} else {bindbindbind <- rbind(bindbindbind,templist[[i]])}
}

write_rds(bindbindbind,"masterfile.rds")
myrdsfilename <- paste0("firstscrape_",substr(date(),9,10),substr(date(),5,7),substr(date(),21,24),".rds")
write_rds(bindbindbind,myrdsfilename)

masterfile <- read_rds("masterfile.rds")
masterfile <- masterfile[,1:dim(masterfile)[2]-1]
masterfile$pagenr <- NA
masterfile$lastseen <- ""
write_rds(masterfile,"masterfile.rds")
