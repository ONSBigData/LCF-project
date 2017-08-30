#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This is a helper function to manipulate the text description of products in historic LCF data
#
# date: January 2017
# author: ivyONS
############################################################

library('tm')

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
  
  res <- res %>%  mutate (longdescr = paste(descr, info, ifelse(pkg>1, paste0('x', pkg), ''), 
                                            ifelse(is.na(offer)|offer!='Price Discount', '', 'disc'), ifelse(is.na(offer)|offer!= 'MultiBuy', '', 'mbuy')))
  corp <- Corpus(VectorSource(res$longdescr))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(gsub), pattern = "^ ", replacement = "")   
  corp <- tm_map(corp, content_transformer(gsub), pattern = " $", replacement = "")   
  res$longdescr <- sapply(corp, function(x) x$content)
  
  res
}