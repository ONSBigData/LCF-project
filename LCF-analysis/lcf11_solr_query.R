#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code implements retrieves lookups from SOLR using a simple query reflecting the SOLR settings at that time.
#
# date: February 2017
# author: ivyONS
############################################################


library(solrium)
library("ggplot2")
library("dplyr")

#############
#clean data for SOLR
setwd("~/R/LCF")
load( file='foodies_train-test.RData')


#################
# helper function to connect to SOLR
solr_connect("http://192.168.0.135:8080/solr/select", errors = 'complete', verbose=F)  #older solr 5.5
#solr_connect("http://192.168.0.123:8080/solr/select", errors = 'complete', verbose=F) # new solr 6.0

create_q <- function(descr, paid = NA, coicop = NA, shop=NA){
  # finetuning the query options is essential for the estimator performance
  # see examples here: http://yonik.com/solr/query-syntax/
  words <- strsplit(descr, "\\s")[[1]]
  words <- words[nchar(words)>0]   # drop empty words
  q_descr <- paste0('(cleandescr:"',descr,'"^2)') # get points for full match in the same order
  for (i in 1:length(words))
    q_descr <-  paste0(q_descr, '(cleandescr:"', words[i], '"^', 4/(2+i),')') #get points for each matched word 
  
  if (is.na(paid)) q_paid='' else  q_paid <- paste0(#'(paid:', paid, ')', # get points for exact price match
                                                      '(paid:[', paid-1, ' TO ', paid+1,'])', # price within +-1
                                                      '(paid:[', floor(paid*.9), ' TO ', ceiling(paid*1.1),'])',  # price within +- 10% 
                                                      '(paid:[', floor(paid*.2), ' TO ', ceiling(paid*2),'])')  # price within +- 50% 
  
  if (is.na(coicop)) q_coicop=''  else  q_coicop = paste0('(+coicop:', coicop, ')') # require coicop if available

  if (is.na(shop))   q_shop=''    else  q_shop = paste0('(shop:"', shop, '"^0.8)')
  
  return(paste0(q_descr, q_paid, q_coicop, q_shop))
}

solr_weight <-  function(row, onlytop=T, rowmax = 12, useshop=F, usecoicop=F, proxy=F){
  q <- create_q(descr = row$cleandescr, paid = row$paid,  shop = ifelse(useshop, row$shop, NA), coicop = ifelse(usecoicop, row$coicop, NA))
  res <- solr_all(q=q, rows=rowmax, fl=c('coicop','quantity','count','score'))$search
  if (nrow(res)>0){
    if (proxy){  res_proxy <- which(res$quantity==row$quantity)
                 res_proxy <- ifelse(length(res_proxy)==0, NA, min(res_proxy))} 
    if (onlytop) res <- res %>% filter(score==max(score))    # use only the best matched products
    res<- res %>%  group_by(quantity) %>% summarise(s = sum(score*sqrt(count))) %>% 
                   ungroup() %>% arrange(-s)
    res<- res$quantity[1]
    if (proxy) res<- c(quantity_est=res, quantity_proxy=res_proxy)}
  else res <- NA
  return(res)
}

solr_coicop <-  function(row, onlytop=T, rowmax = 12, usepaid=F, useshop=F, proxy=F){
  q <- create_q(descr = row$cleandescr, paid = ifelse(usepaid, row$paid, NA),  shop = ifelse(useshop, row$shop, NA)) 
  res <- solr_all(q=q, rows=rowmax, fl=c('coicop','quantity','count','score'))$search
  if (nrow(res) > 0) {
    if (proxy){  res_proxy <- which(res$coicop==row$coicop)
                 res_proxy <- ifelse(length(res_proxy)==0, NA, min(res_proxy))} 
    if (onlytop) res <- res %>% filter(score==max(score))    # use only the best matched products
    res <- res %>% group_by(coicop) %>% summarise(s = sum(score*sqrt(count))) %>% 
                   ungroup() %>% arrange(-s)
    res<- res$coicop[1]
    if (proxy) res<- c(coicop_est = res, coicop_proxy=res_proxy)}
  else res <- NA
  return(res)
}

solr_both <-  function(row, onlytop=T, rowmax = 16, useshop=F, proxy=T, usecoicop=T){
  res <- solr_coicop(row, onlytop=onlytop, rowmax=rowmax, useshop=useshop, proxy=proxy, usepaid=F)
  row$coicop <- res[1]
  res_q <- solr_weight(row, onlytop=onlytop, rowmax=rowmax, useshop=useshop, proxy=proxy, usecoicop=usecoicop)
  return(c(res, res_q))
}


###############
#examples
if (F){
q <- paste0('(cleandescr:"semi    skm  up ")(paid: 90)')
# q <- paste0('(paid:[61 TO 64])')
q<-create_q(test_set$cleandescr[1], test_set$paid[1], test_set$coicop[1], test_set$shop[1])
res2 <-   solr_search(q=q, rows=10, wt='csv', fl=c('score'), qt = "edismax")

q<-create_q(test_set$cleandescr[1], test_set$paid[1], test_set$coicop[1])
solr_coicop(test_set[1,])
solr_weight(test_set[6,])

solr_est <- list()
system.time(for (i in 1:100) solr_est[[i]] <- solr_both(test_set[i,]))

test_set$solr_weight <- NA
system.time(for (i in 1:100) test_set$solr_weight[i] <- solr_weight(test_set[i,]))

test_set$solr_coicop <- NA
system.time(for (i in 1:100) test_set$solr_coicop[i] <- solr_coicop(test_set[i,]))
}
###########################################################
############ very inefficient !DO NOT RUN 
library(foreach)
library(doParallel)
cl <- makeCluster(7)
clusterCall(cl, function() {  library("dplyr")
                              library("solrium") 
  solr_connect("http://192.168.0.135:8080/solr/select", errors = 'complete', verbose=F)
  })
registerDoParallel(cl)

# little bit of benchmarking...  -> 90s without parallel and <60s parallelized per 1000rows => whole set about an hour 
if(F){
k=1000
solr_est <- list()
Sys.time()
system.time(for (i in (1:k)) solr_est[[i]] <- solr_both(test_set[i,]))
Sys.time()
system.time(  solr_est <- foreach (i = (1:k),  .combine=rbind) %dopar% solr_both(test_set[i,]) )
Sys.time()
system.time(  solr_est <- foreach (i = 1:k,  .combine=rbind) %do% solr_both(test_set[i,]) )
Sys.time()
}

## to be run in innovation lab
if (T){
  print('Time required for the first 1000 items (out of 66k test_set):')
  print(system.time(solr_est <- foreach (i = 1:1000) %dopar% try(solr_both(test_set[i,]))))
  print(Sys.time())
  solr_est <- foreach (i = 1:nrow(test_set)) %dopar% {
    try(solr_both(test_set[i,]))
  }
  print(Sys.time())
  save(solr_est, file='solr_est.RData')
}

if (F){
  print(Sys.time())
  for (i in 10000:30000) {
    if (i%%3000==0){print(i)
      print(Sys.time())
      save(solr_est, file='solr_est.RData')
    }
    solr_est[[i]] <- try(solr_both(test_set[i,]))
  }
}

