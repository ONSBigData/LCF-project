#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code is used to process mySupermarket data in order to make it comparable with LCF Data (abbreviations etc.)
#
# date: November 2016
# author: ivyONS
############################################################

setwd("~/R/LCF")
library("ggplot2")
library("dplyr")
#Q3 <- read.csv('mysupermarket/2014Q3.csv', header = T, stringsAsFactors = F)
Q3f <- read.csv('mysupermarket/formatted2014Q3.csv', header = T, stringsAsFactors = F)
supcat <- read.csv('mysupermarket/supcats.csv', header = F, stringsAsFactors = F)
supcat <- supcat %>% mutate( coicop = as.numeric(gsub(pattern = '[.]', replacement = '', x=V3)), level= nchar(as.character(coicop)) )
                             #, included_subcat = (level==5)==(substr(coicop,1,4) %in% as.character(coicop)))

# there is 34 Product Families and each product appear many times (most about 90x - everyday in the quarter) 
hist(table(Q3$ProductTemplateID))
table(Q3$ProductFamily)

load("~/R/LCF/nn1dist_big2.RData")
shops <- c(asda = 140, tesco = 2190, waitrose = 2370, sainsbury = 1980)
# number of products in 4 large supermarkets per category
print (nn1dist_df %>% filter(Shop %in% shops) %>% group_by(Shop, cat) %>% summarise(n=n()) , n= 100)
# proportion of products bought in large supermarkets
print (nn1dist_df %>% group_by(cat) %>% summarise(prop = sum(Shop %in% shops)/n()) , n= 100)

# subset historical LCF data just to those shops and coicops
coicop_subset <- function(x, level4 = with(supcat, coicop[level==4]), level5= with(supcat, coicop[level==5])){
  (substr(x,1,4) %in% level4) | (as.character(x) %in% level5)
}

LCF_subset <- nn1dist_df %>%  filter(Shop %in% shops, coicop_subset(coicop))

# now we should get rid of duplicates in mysupermarket data 
Q3f_dupl <- Q3f %>% group_by(store, ProductFamily, MySupermarket_Product_Name, Quantity, ItemsInPack, Price, off_Price, Offer, Offer_Type, ProductTemplateID) %>% summarize(n = n())  

# have the price reflect the offer
Q3f_dupl <- Q3f_dupl %>% ungroup() %>% mutate(orig_price = Price, Price = ifelse(is.na(off_Price), orig_price, off_Price))

# extract offers from LCF
library("tm")

split_offer <- function(descr){
  corp <- Corpus(VectorSource(descr))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, content_transformer(gsub), pattern = "m\\s?buy\\b?", replacement = " mbuy ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "cpn\\b", replacement = " cpn ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = " m ?s\\b", replacement = " ms ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "x\\s?([0-9]+)", replacement = " x\\1 ")    
  
  ctrl <- list(wordLengths = c(1, Inf))
  dtm <- DocumentTermMatrix(corp, control=ctrl)
  mbuy_offers <- intersect(dimnames(dtm)$Terms, c("bogof", "mdeal", "ms", "mbuy", "buy", "deal"))
  disc_offers <- intersect(dimnames(dtm)$Terms, c("disc",  "cpn", "rcd", "hsv")) 
  res <- ifelse(rowSums(as.matrix(dtm[,mbuy_offers]))>0, 'MultiBuy', ifelse(rowSums(as.matrix(dtm[,disc_offers]))>0, 'Price Discount', NA))
  
  corp <- tm_map(corp, removeWords, c("bogof", "mdeal", "ms", "mbuy", "buy", "deal", "disc",  "cpn", "rcd", "hsv"))
  corp <- tm_map(corp, stripWhitespace)
  descr <- sapply(corp, function(x) x$content)

  list(res, descr)
}

LCF_offers <- split_offer(LCF_subset$EXPDESC)
LCF_subset$Offer_type <- LCF_offers[[1]]
LCF_subset$descr <- LCF_offers[[2]]

mysuper_shorthand <-  function(descr){
  corp <- Corpus(VectorSource(descr))
  corp <- tm_map(corp, content_transformer(tolower))
  # package amounts in brackets
  corp <- tm_map(corp, content_transformer(gsub), pattern = "\\(([0-9]+)x", replacement = " x\\1 ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "\\(([0-9]+) per pack(age)?", replacement = " x\\1 ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "\\.", replacement = "")      
  corp <- tm_map(corp, content_transformer(gsub), pattern = " \\(?[0-9]+[a-z]+\\)\\b", replacement = "")      
  
  
  #some  shortcuts  ... general
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(gsub), pattern = "fresh", replacement = " fh ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "frozen", replacement = " fz ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "tinned", replacement = " tin ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "packet", replacement = " pkt ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "cooked", replacement = " ckd ") 
  corp <- tm_map(corp, content_transformer(gsub), pattern = "uncooked", replacement = " uc ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "ready to eat", replacement = " r to e ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "ready to drink", replacement = " r to d ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "ready meal", replacement = " rm ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "organic", replacement = " org ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "regular", replacement = " reg ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "concetrated|concetrate", replacement = " conc ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "portion", replacement = " ptn ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "battered", replacement = " batd ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "breaded", replacement = " brdd ")
  corp <- tm_map(corp, content_transformer(gsub), pattern = "chilled", replacement = " chill ")   

  

  #some  shortcuts  ... bread
 

  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "sliced", replacement = " sl ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "white", replacement = " wht ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "brown", replacement = " brn ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "wholemeal", replacement = " wmeal ")   
  
  
  #some  shortcuts  ... milk
  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "whole", replacement = " whl ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "semi[-|/s]skimmed|semiskimmed", replacement = " semi ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "skimmed", replacement = " skm ")
  
  
  #some  shortcuts  ... other food items
  
  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "sandwich", replacement = " s/wich ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "mayonaisse", replacement = " mayo ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "chocolate", replacement = " choc ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "biscuit", replacement = " bisc ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "squash", replacement = " sqh ") 
  corp <- tm_map(corp, content_transformer(gsub), pattern = "vegetables|vegetable", replacement = " veg ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "juice", replacement = " jce ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "sauce", replacement = " sce ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "cauliflower", replacement = " cauli ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "brocolli", replacement = " broc ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "tomatoes", replacement = " toms ")
  
  
  
  
  #some  shortcuts  ... 
  
  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "premium range|premium", replacement = " pr ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "value range|value", replacement = " va ") 
  
  
  
  
  
  
  
  corp <- tm_map(corp, stripWhitespace)
  descr <- sapply(corp, function(x) x$content)
  descr
}

Q3f_dupl$descr <- mysuper_shorthand(Q3f_dupl$MySupermarket_Product_Name)
