#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code implements first version of lookups from historic LCF data and evaluates the performance (proximity of correct amount in the list)
#
# date: November 2016
# author: ivyONS
############################################################


setwd("~/R/LCF")
source('scripts/lcf1_helper.R')
library("ggplot2")
library("dplyr")
library("tidyr")
library('tm')
#source('~/R/LCF/scripts/lcf5_history_lookup.R') # need function split_EXPDESC

#########################################################
LCF <- read.csv('orig_data/2015_Q2_to_2016_Q1_RawdryX.csv', header = T, stringsAsFactors = F)
LCF <- LCF[,c(8,11:12, 15:16, 29)]
LCF <- LCF %>%  mutate(FINCODE = as.numeric(FINCODE)) %>% filter(FINCODE<20000,  is.na(MAFFQUAN)==F)
LCF_res <- split_EXPDESC(LCF$EXPDESC)
LCF2 <- cbind(LCF_res, LCF) %>% filter(nchar(descr)>0, is.na(MAFFQUAN)==F)

LCF_n <- LCF2 %>% group_by(FINCODE, descr, info, offer, pkg, PAID1, MAFFQUAN) %>% 
  summarize(n=n(), shops = paste(sort(unique(SHOP)), collapse=', ') ) %>% ungroup()

summar <- LCF_n %>% group_by(as.numeric(FINCODE)%/%100) %>% summarize(total= sum(n), uniq=n(), rare = sum(n==1)) 
sublist <- LCF_n %>% mutate(word1 = sapply(descr, function(x) gsub("([a-z]+).*","\\1",x)),
                             EXPDESC = paste(descr, paste0(pkg, 'x'), info)) %>%
            arrange(FINCODE, word1, PAID1, EXPDESC, -n)

###########################################################################################
#performance evaluation (slow)

perf_eval <- function(LCF_n, k=10){
  coicops<- unique(LCF_n$FINCODE)
  LCF_n$word1 <- sapply(LCF_n$descr, function(x) gsub("([a-z]+).*","\\1",x))  
  LCF_n <- LCF_n %>% mutate(EXPDESC = paste(descr, paste0(pkg, 'x'), info))
  pe <- sapply(coicops, function(x)  {cat(which(coicops==x), x, sum(LCF_n$FINCODE==x))
    print (Sys.time())
    c(coicop=x, perf1(LCF_n %>% filter(FINCODE==x), k=k))})  
  as.data.frame(apply(pe,1,as.numeric))
}

perf1 <- function(sublist, k=10){
  sublist <- sublist %>% arrange(word1, PAID1, EXPDESC)
  multiquan <- sapply(-k:k, shift, v=sublist$MAFFQUAN)
  multin <- sapply(-k:k, shift, v=sublist$n) - matrix(t(c(rep(0,k), 1, rep(0,k))),ncol=2*k+1, nrow=nrow(sublist), byrow=T)
  multiquan <- multiquan * (multin>0)
  avail <- apply(multiquan == matrix(sublist$MAFFQUAN, ncol=2*k+1, nrow=nrow(sublist)), 1, max) 
  common <- sapply(1:nrow(sublist), function(x) modushelp(multiquan[x,], multin[x,], sublist$MAFFQUAN[x]))  
  c(n=sum(sublist$n), available= sum(avail*sublist$n), mostcommon=sum(common*sublist$n))
}

shift <- function(t, v, dummy=0){
  if (abs(t)>=length(v)) rep(dummy, length(v)) 
  else
    if (t>0) c( v[(t+1):(length(v))], rep(dummy, t))
  else c(rep(dummy, -t), v[1:(length(v)+t)])
}

modushelp <- function(val, n, goal){
  df <- data.frame(val, n) %>% group_by(val) %>% summarise(m=sum(n)) %>% ungroup() %>% arrange(-m)
  df <- df %>% filter(m == df$m[1], val == goal)
  nrow(df)>0
}

##############################
#performance visual
df <- rbind(pe5, pe10, pe15)
df$proximity <- rep(c(5,10,15), each = nrow(pe5))
df$coicop <- as.character(df$coicop)
ggplot(df %>% mutate(proximity = as.character(proximity))) +
  geom_point(aes(x=coicop, y=available/n, size=n, color = proximity))+
  geom_point(aes(x=coicop, y=mostcommon/n, size=n, color = proximity))

ggplot(df, aes(group=coicop)) +
  geom_line(aes(x=proximity, y=mostcommon/n, color = coicop))+
  geom_point(aes(x=proximity, y=mostcommon/n, size=n, color = coicop)) +
  guides(color='none')


##################################
#using train and test set from 2014&2015
load( file='foodies_train-test.RData')
both_sets <- rbind(cbind(test_set, test=T), cbind(train_set, test=F)) %>% 
  mutate(word1 =  gsub("([a-z]+).*","\\1",cleandescr))  %>% 
  arrange(coicop, word1, paid, cleandescr, count, quantity)

both_sets <-  both_sets %>% group_by(coicop) %>% 
  mutate(avail=perf1(quantity, k=16))


perf1 <- function(sublist, k=10){
  multiquan <- sapply(c(-k:-1, 1:k), shift, v=sublist) 
  avail <- apply(multiquan == matrix(sublist, ncol=2*k, nrow=length(sublist)), 1, function(x) {
    t<-which(x)-k
    min(ifelse(t>0, t, (-t+1)))}
    ) 
  return(avail)
}

shift <- function(t, v, dummy=0){
  if (abs(t)>=length(v)) rep(dummy, length(v)) 
  else
    if (t>0) c( v[(t+1):(length(v))], rep(dummy, t))
  else c(rep(dummy, -t), v[1:(length(v)+t)])
}

both_sets %>% filter(test) %>% group_by(coicop%/%100) %>% 
  summarise(first = sum(avail<2)/n(),
         inten=sum(avail<11)/n())

## using bot_sets to export lookup as one big file
both_sets$test <-NULL
names(both_sets)<- c('EXPDESC', 'CODING', 'Paid1', 'Quantity', 'Units', 'Shop', 'Multiplicity', '1st_word')
write.csv(both_sets, file=paste0('lookup/LCF2014-15_lookup.csv'), row.names = F)
