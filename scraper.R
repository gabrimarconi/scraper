

#install.packages("rvest")
#install.packages("readr")
#install.packages("stringr")
library(rvest)
library(readr)
library(stringr)
# https://www.dataquest.io/blog/web-scraping-in-r-rvest/
# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=229765585183&utm_targetid=aud-390929969673:dsa-429603003980&utm_loc_interest_ms=&utm_loc_physical_ms=9067749&gclid=Cj0KCQjw6-SDBhCMARIsAGbI7UgXMI_-EixQFrb99ZnfFw0cZjfCiXdHMRzAnsDTsMg6r-QpNmAFxGYaAq1dEALw_wcB

#page <- read_html("https://dataquestio.github.io/web-scraping-pages/simple.html")
#str(page)
#html_nodes(page, "p")



###############################3

scrapeyourass <- function(i) {
  #i <- 1
  
  webpage <- paste0("https://www.immotop.lu/search/index",i,".html")
  
  #set webpage
  page <- read_html(webpage)
  
  #ad title
  gross_ads <- html_nodes(page, ".title-anons")
  clean_ads <- as.data.frame(html_text(gross_ads))
  colnames(clean_ads) <- "cleanads"

  # luxembourg-ville
  clean_ads$ville <- str_detect(clean_ads$cleanads,"LUXEMBOURG-")
  clean_ads$cents <- str_detect(clean_ads$cleanads,"LUXEMBOURG-CENTS")
  clean_ads$vente <- str_detect(clean_ads$cleanads,"vendre")  
  
  #price
  gross_price <- html_nodes(page, ".price")
  clean_price <- as.data.frame(html_text(gross_price))
  gross_price <- html_nodes(page, "[class='price']")
  clean_price <- as.data.frame(html_text(gross_price))
  
  # link
  tempobj <- as.data.frame(read_lines(webpage, n_max=-1))
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
  
  #pagenr
  pagenr <- i
  print(i)
  
  #merge
  dataset <- cbind(date(),clean_ads,clean_price,allinks,pagenr,"")
  colnames(dataset) <- c("date","texto","ville","cents","vente","price","link","pagenr","lastseen")
  dataset <- dataset[dataset$ville==TRUE & dataset$vente==TRUE,]
  
  return(dataset)
}


#View(scrapeyourass(15))

scrapememore <- function(i) {
  #i <- 15
  
  # download page
  tempobj <- read_lines(bindbindbind$link[i], n_max=-1)
  tempdata <- as.data.frame(tempobj)
  colnames(tempdata) <- "texto"
  
  # download structured description
  tempdata$strucdescr <- str_detect(tempdata$texto,"<meta name=.{1}description.{1} content=") 
  strucdescr <- tempdata$texto[tempdata$strucdescr==TRUE]
  
  if (length(strucdescr)==0) {
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
    }
  
  # compile and return output
  description <- as.data.frame(cbind(chambres,annee,chauss,m2,terrasse,garage,cave,jardin,etage,ground))
  return(description)
}

