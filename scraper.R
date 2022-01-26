

#install.packages("rvest")
#install.packages("readr")
#install.packages("stringr")
library(rvest)
library(readr)
library(stringr)
# https://www.dataquest.io/blog/web-scraping-in-r-rvest/
# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=229765585183&utm_targetid=aud-390929969673:dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=9067749&gclid=Cj0KCQjw6-SDBhCMARIsAGbI7UgXMI_-EixQFrb99ZnfFw0cZjfCiXdHMRzAnsDTsMg6r-QpNmAFxGYaAq1dEALw_wcB

if (file.exists("generalised/failed_downloads.csv")==F) {
  failed_downloads <- data.frame(pagenr=0,date=date())
  write.csv(failed_downloads,"generalised/failed_downloads.csv", row.names = F)
}

#page <- read_html("https://dataquestio.github.io/web-scraping-pages/simple.html")
#str(page)
#html_nodes(page, "p")



###############################3

scrapeyourchin <- function(i) {
  #i <- 3
  
  webpage <- paste0("https://www.immotop.lu/search/index",i,".html")
  
  #set webpage
  page <- try(read_html(webpage))
  if (class(page)=="try-error") {page <- try(read_html(webpage))}
  if (class(page)=="try-error") {page <- try(read_html(webpage))}
  
  if (class(page)=="try-error") {
    clean_ads <- data.frame(cleanads="download_failed",maison=F,apt=F,vente=F)
    clean_price <- data.frame(html_text="0")
  } 
  if (class(page)!="try-error") {
    #ad title
    gross_ads <- html_nodes(page, ".title-anons")
    clean_ads <- as.data.frame(str_trim(html_text(gross_ads)))
    colnames(clean_ads) <- "cleanads"
    
    # apartments and houses on sale
    clean_ads$maison <- str_detect(clean_ads$cleanads,"Maison|Ferme|Villa|Bungalow|Chalet|Château|Propriété|Gros-oeuvre")
    clean_ads$apt <- str_detect(clean_ads$cleanads,"Appartement|Studio|Duplex|Triplex|Penthouse|Loft")
    clean_ads$vente <- str_detect(clean_ads$cleanads,"vendre")  
    
    #price
    gross_price <- html_nodes(page, ".price")
    clean_price <- as.data.frame(html_text(gross_price))
    gross_price <- html_nodes(page, "[class='price']")
    clean_price <- as.data.frame(html_text(gross_price))
  }

  
  # link
  tempobj <- try(as.data.frame(read_lines(webpage, n_max=-1)))
  if (class(tempobj)=="try-error") {tempobj <- try(as.data.frame(read_lines(webpage, n_max=-1)))}
  if (class(tempobj)=="try-error") {tempobj <- try(as.data.frame(read_lines(webpage, n_max=-1)))}
  
  if (class(tempobj)=="try-error") {
    allinks <- data.frame(link="download_failed")
  }
  if (class(tempobj)!="try-error") {
    colnames(tempobj) <- "texto"
    tempobj$titleanons <- str_detect(tempobj$texto,"title-anons")
    tempobj$weblink <- rbind(FALSE,as.data.frame(tempobj$titleanons[2:length(tempobj$titleanons)-1]))
    weblinks <- tempobj$texto[tempobj$weblink==TRUE]
    obj1 <- substr(weblinks,10,9999)
    obj2 <- str_split(obj1,">", n=2)
    onlyone <- function (i) {
      return(obj2[[i]][[1]])
    }
    allinks <- as.data.frame(sapply(1:length(obj2),onlyone))
    colnames(allinks) <- "link"
    allinks$link <- (substr(allinks$link,1,nchar(allinks$link)-1))
  }
  
  
  #pagenr
  pagenr <- i
  print(i)
  
  #merge
  dataset <- cbind(date(),clean_ads,clean_price,allinks,pagenr,"")
  colnames(dataset) <- c("date","texto","maison","apt","vente","price","link","pagenr","lastseen")
  if (dataset$link=="download_failed") {
    failed_downloads <- rbind(read.csv("generalised/failed_downloads.csv"),data.frame(pagenr=pagenr,date=date()))
    write.csv(failed_downloads,"generalised/failed_downloads.csv", row.names = F)
    }
  dataset <- dataset[(dataset$maison==TRUE|dataset$apt==TRUE) & dataset$vente==TRUE & str_detect(dataset$texto,"\\(DE\\)|\\(FR\\)|\\(BE\\)")==FALSE,]
  
  return(dataset)
}


scrapememore <- function(i) {
  #i <- 15
  
  # download page
  tempobj <- read_lines(bindbindbind$link[i], n_max=-1)
  tempdata <- as.data.frame(tempobj)
  colnames(tempdata) <- "texto"
  
  # download structured description
  tempdata$strucdescr <- str_detect(tempdata$texto,"<meta name=.{1}description.{1} content=") 
  strucdescr <- tempdata$texto[tempdata$strucdescr==TRUE]
  firstseen <- date()
  
  if (length(strucdescr)==0) {
    strucdescr <- NA
    jardin <- NA
    terrasse <- NA
    cave <- NA
    garage <- NA
    chauss <- NA
    ground <- NA
    etage <- NA
    m2 <- NA
    chambres <- NA
    annee <- NA
    emphyt <- NA
  } else {
    # generate variables
    jardin <- max(str_detect(strucdescr,"jardin"))
    terrasse <- max(str_detect(strucdescr,"terrasse"))
    cave <- max(str_detect(strucdescr,"cave"))
    garage <- max(str_detect(tempobj,">Garage<"))
    chauss <- max(str_detect(tempobj,"chauss|Chauss|CHAUSS",))
    ground <- max(str_detect(tempobj,"ground floor|groundfloor|ground-floor|Ground floor|Groundfloor|Ground-floor|the ground|sur terrain"))
    etage <- substr(strucdescr,str_locate(strucdescr, "tage ")[2]+1,str_locate(strucdescr, "tage ")[2]+1)
    m2 <- substr(strucdescr,str_locate(strucdescr, "Surface habitable")[2]+7,str_locate(strucdescr, "Surface habitable")[2]+12)
    chambres <- substr(strucdescr,str_locate(strucdescr, "Ch. ")[2]+1,str_locate(strucdescr, "Ch. ")[2]+1)
    annee <- substr(strucdescr,str_locate(strucdescr, "de construction ")[2]+1,str_locate(strucdescr, "de construction ")[2]+4)
    emphyt <- ifelse(max(str_detect(tempobj,"emphyt|enphyt|emphit|enphit|emfit|enfit|amphit|amphyt|amphit|anphit|amfit|anfit"))==1&max(str_detect(tempobj,"pas de bail|ien en pleine propri"))==0, 1, 0)
    }
  
  # compile and return output
  description <- as.data.frame(cbind(strucdescr,firstseen,emphyt,chambres,annee,chauss,m2,terrasse,garage,cave,jardin,etage,ground))
  return(description)
}


#https://www.immotop.lu/properties/used/apartment/duplex/2772270
#Bien en pleine propriété - pas de bail emphythéotique