#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# helper functions for LCF visualisations and processing
#
# date: July 2016
# author: ivyONS
############################################################

library("dplyr")
library("tm")

#unit conversion - rough coeficients
convert_unit <- function(type, unit, amount){
  UNITS<-c("Eggs (record NUMBER of eggs)", "KiloGrams" ,"Ounces","Pounds","Grams",
           "Litres","Pints","MilliLitres", "Fluid ounces" ,"CentiLitres (eg for alcohol) ")
  KOEFS <- c(1, 1, .03,.5,.001, 1,.5,.001,.03,.01)
  UNIT_NEW <- c("Eggs", rep("KiloGrams",4), rep("Litres", 5) )
  names(KOEFS) <- UNITS
  names(UNIT_NEW) <- UNITS
  if (type=='amount')   amount * KOEFS[unit]
  else              UNIT_NEW[unit]
}

############################
## is it offer?
is_offer <- function(descr){
  corpus <- Corpus(VectorSource(descr))
  corp <- tm_map(corpus, removePunctuation)
  corp <- tm_map(corp, content_transformer(tolower))
  ctrl <- list(wordLengths = c(1, Inf))
  dtm <- DocumentTermMatrix(corp, control=ctrl)
  offers <- intersect(dimnames(dtm)$Terms, c("bogof", "mbuy", "disc", "mdeal",  "cpn", "rcd", "ms", "hsv"))
  rowSums(as.matrix(dtm[,offers]))>0
}

#############################
## amount bins - trying to create some represenattive categories for possible weights
a_bins <- function(amount){
  n <- round(log2(length(amount)))
  if (n >= length(unique(amount))) return(amount)
  else{
    cl<-kmeans(log(amount), n)$cluster
    val <- tapply(amount, cl, median)
    bins <- as.vector(val[cl])
    return(bins)
  }
}

#######################################
# naive nearest neighbor - run for each coicop separately as it is terribly slow (thats why we need Solr)!
library("tm")

# neighbor search: 1. descr 2. price 3. shop
nn1help <- function(ind, dtm, price, amount, shop, subset = T, details = F){
  if (subset==T){
    subind <- (price>=price[ind]/2)&(price<=price[ind]*2)
    subind[ind] <- FALSE 
    if (sum(subind)==0) subind <- (1:length(price))!=ind
    #print(subind)
  }else subind <- (1:length(price))!=ind
  
  score <- as.vector(dtm[ind,,drop=F]%*%t(dtm[subind,,drop=F]))/pmax(rowSums(dtm[subind,,drop=F]), sum(dtm[ind,]))
  #print(score)
  maxscore = max(score)
  neighbors <- score==maxscore        # best matching descriptions
  pricediff <- as.vector(abs(price[subind][neighbors]-price[ind]))
  closest <- pricediff == min(pricediff)         # best matching price
  if (sum(shop[subind][neighbors][closest]==shop[ind])>0) {        # are there products from the same shop?
    closest <- closest & (shop[subind][neighbors]==shop[ind])  }
  pool <- amount[subind][neighbors][closest]
  est <- modus(pool)  # taking median out of the nearest neighbors (if there are ties)
  if (details){
    res <- c(est=est, score=maxscore, pricediff= min(pricediff), shopmatch = sum(shop[subind][neighbors][closest]==shop[ind])>0, candid_num = length(pool), candid_match = sum(est==pool) )
    }else res <- est
  return(res)
}

# neighbor search: 1. price 2. shop 3. desc  (it is faster that nn1help)
nn2help <- function(ind, dtm, price, amount, shop, subset = T){
    subind <- (price>=price[ind]*.95)&(price<=price[ind]*1.05)
    subind[ind] <- FALSE 
    if (sum(subind)==0) return(NA)   # require very close match on price - otherwise don't estimate amount
    else { 
      score <- as.vector(dtm[ind,,drop=F]%*%t(dtm[subind,,drop=F]))/pmax(rowSums(dtm[subind,,drop=F]), sum(dtm[ind,]))
      neighbors <- score==max(score)        # best matching descriptions including shop token
  
      est <- median(amount[subind][neighbors])  # taking median  of the nearest neighbors (if there are ties)
}}


nn1 <- function(descr, price, amount, shop, type=2,  details = F){
  if (type==2) descr <- paste0(descr, shop, 'shop')
  corp <- Corpus(VectorSource(descr))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(tolower))
  ctrl <- list(wordLengths = c(1, Inf))
  dtm <- DocumentTermMatrix(corp, control=ctrl)
  dtm <-  as.matrix(dtm)>0
  #res <- amount
  #for (i in 1:length(price)){
  #  if (type==1)    res[i] <- nn1help(i, dtm=dtm, price=price, amount=amount, shop=shop, details=details)
  #  else res[i] <- nn2help(i, dtm=dtm, price=price, amount=amount, shop=shop)
  #  if (i%%500==1) print(i)
  #}
  if (type==1)  res <- sapply(1:length(price), nn1help, dtm=dtm, price=price, amount=amount, shop=shop, details=details)
  else res <- sapply(1:length(price), nn2help, dtm=dtm, price=price, amount=amount, shop=shop)
  #print(res) 
}

################### why isn't mode implemented by default????
modus <- function(x, na.rm = TRUE) {
  if(na.rm) x = x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}