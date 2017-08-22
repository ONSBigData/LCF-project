#
#

setwd("~/R/LCF")
library("ggplot2")
library("dplyr")
library('tm')


load("~/R/LCF/nn1dist_big2.RData")
LCF <- nn1dist_df
#LCF_offers <- split_offer(LCF$EXPDESC)
#LCF$Offer_type <- LCF_offers[[1]]
#LCF$descr <- LCF_offers[[2]]

#LCF_dupl <- LCF_subset %>% group_by(EXPDESC) %>% summarise(n = n())

### helper function to split description into descr, offer(2types), pkg (num in pack), info (e.g. 'fh', 'wv') 
split_EXPDESC <- function(descr){
  res <- data.frame(descr=descr)
  corp <- Corpus(VectorSource(descr))
  #corp <- Corpus(VectorSource(LCF_dupl$EXPDESC))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(tolower))
  # standardize some offer shortcuts
  corp <- tm_map(corp, content_transformer(gsub), pattern = "m\\s?buy\\b?", replacement = " mbuy ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = " m ?s\\b", replacement = " ms ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "cpn\\b", replacement = " cpn ")  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "disc\\b", replacement = " disc ")  
  
  # standardise package size information:
  corp <- tm_map(corp, content_transformer(gsub), pattern = "x\\s?([0-9]+)", replacement = " x\\1 ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "([0-9]+)\\s?x", replacement = " x\\1 ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "([0-9]+)\\s?pkt?", replacement = " x\\1 ")    

  # standardise some other inconsistencies:
  corp <- tm_map(corp, content_transformer(gsub), pattern = "r\\s?to\\s?e", replacement = " rtoe ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "r\\s?to\\s?d", replacement = " rtod ")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = "ski?m(med)?", replacement = "skm")  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "semi skm", replacement = "semi ")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = "^ ", replacement = "")    
  corp <- tm_map(corp, content_transformer(gsub), pattern = " not ", replacement = " not")  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "7 [uU]p", replacement = "7up")  
  corp <- tm_map(corp, content_transformer(gsub), pattern = "m  m", replacement = "mm") 
  
  # split the corpus into words:
  ctrl <- list(wordLengths = c(1, Inf))
  dtm <- DocumentTermMatrix(corp, control=ctrl)
  
  # extract offer info -> two offer types
  mbuy_offers <- intersect(dimnames(dtm)$Terms, c("bogof", "mdeal", "ms", "mbuy", "buy", "deal"))
  disc_offers <- intersect(dimnames(dtm)$Terms, c("disc",  "cpn", "rcd", "hsv")) 
  res$offer <- ifelse(rowSums(as.matrix(dtm[,mbuy_offers]))>0, 'MultiBuy', ifelse(rowSums(as.matrix(dtm[,disc_offers]))>0, 'Price Discount', NA))
  
  # extract package info -> number
  pkg_num <- intersect(dimnames(dtm)$Terms, paste('x',1:1000, sep=''))
  pkg_no <- sapply(pkg_num, function(x) as.numeric(substr(x,2, nchar(x))))
  res$pkg <- apply(as.matrix(dtm[,pkg_num]),1, function(x) prod(pmax(x*pkg_no, 1)) )  
  
  # additional info - shorthands
  keywords <- c('fh','fz','tin','ckd','uc', 'rtoe', 'rtod', 'rm', 'org', 'lc', 'reg', 'conc', 'ptn', 'batd', 'brdd', 'chill',
                'df', 'wv', 'up', 'cf', 'gf', 'mow', 'ns','pr', 'va', 'ta', 'ok') 
  add_info <- intersect(dimnames(dtm)$Terms, c(keywords, paste0('not', keywords)))
  res$info <- apply(as.matrix(dtm[ , add_info]), 1, function(x) paste(ifelse(x, paste(add_info, ''), ''), collapse='') ) 
  
  #remove extracted info from description
  corp <- tm_map(corp, removeWords, c(mbuy_offers, disc_offers, pkg_num, add_info))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(gsub), pattern = "^ ", replacement = "")    
  res$descr <- sapply(corp, function(x) x$content)
  
  res
}

LCF_res <- split_EXPDESC(LCF$EXPDESC)
LCF$offer <- NULL
LCF$descr <- NULL
LCF2 <- cbind(LCF_res, LCF) %>% filter(nchar(descr)>0)
LCF2$word1 <- sapply(LCF2$descr, function(x) gsub("([a-z]+).*","\\1",x))   # {temp<- strsplit(x," ")[[1]]; ifelse(temp[1]=='', temp[2], temp[1])})
LCF2_n <- LCF2 %>%  filter(as.numeric(FINCODE)<20000, is.na(MAFFUNIT)==F) %>%
  group_by(FINCODE, descr, info, offer, pkg, PAID1, MAFFQUAN) %>% summarize(n=n()) %>% ungroup()
summar <- LCF2_n %>% group_by(as.numeric(FINCODE)%/%100) %>% summarize(total= sum(n), uniq=n(), rare = sum(n==1)) 
# %>% group_by(coicop%/%100) %>% summarize(len = sum(m), word1max = max(m))


# quantify the repetitivness of products and the variability in weight info
LCF3 <- LCF2 %>% group_by(coicop, descr, info, offer, pkg, Shop, Paid1) %>% summarise(n=n(), dist_weight = n_distinct(MAFFQuan))
tt <- LCF3 %>% filter(n>1) %>% group_by(coicop) %>% summarise(total= sum(n), uniq = sum(n[dist_weight==1]), perc= uniq/total) 

LCF14 <- LCF2 %>% filter(coicop %/%100==141) %>% group_by(coicop, descr, offer, pkg, info, Shop, Paid1, MAFFQuan, word) %>% summarise(n=n()) %>% #filter(n>1) %>%
  group_by(coicop, descr, offer, pkg, info, Shop, Paid1, word) %>% summarise(n=sum(n), dist_weight = n_distinct(MAFFQuan))
tt14 <- LCF14 %>% group_by(coicop, word) %>% summarise(total= sum(n), uniq = sum(n[dist_weight==1]), perc= uniq/total)   

lookup_len <- LCF4 %>% filter(dist_weight<3) %>%  group_by(coicop, Shop) %>% summarise(n=n())        

word1uniquelength <- LCF2 %>% group_by(coicop, descr, offer, pkg, info, Shop, Paid1, word) %>% summarize(n=n()) %>%
  group_by(coicop, word) %>% summarize(total= sum(n), m=n()) %>% group_by(coicop%/%100) %>% summarize(len = sum(m), word1max = max(m))

#save(LCF2, file='LCF2.RData')
#load('LCF2.RData')

exportcsv <-  function(df = LCF2, subcat = '141**'){
  df<- df %>% filter(cat == subcat) %>% ungroup() %>% 
    mutate (EXPDESC = paste(descr, info, ifelse(pkg>1, paste0('x', pkg), ''), ifelse(is.na(offer)|offer!='Price Discount', '', 'disc'), ifelse(is.na(offer)|offer!= 'MultiBuy', '', 'mbuy')),
            key = paste(word1, Paid1))
  for (i in unique(df$coicop)){
    lookup <- df %>% filter(coicop==i) %>% group_by(key, EXPDESC, Shop, Paid1, MAFFQuan, MAFFUnit, word1) %>% 
      summarise(count = n() ) %>% ungroup() %>% arrange(word1, Paid1, EXPDESC) %>% select(-word1)
    print(lookup, n=30)
    write.csv(lookup, file=paste0('lookup/lookup ', i, '.csv'))
  }
}
                                                                      
exportcsv(subcat='141**')
