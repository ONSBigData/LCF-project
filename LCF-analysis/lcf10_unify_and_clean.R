#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code is used to clean and reprocess historic LCF data 2014&15
#
# date: January 2017
# author: ivyONS
############################################################


# LCF - data preprocessing tasks:
# load data from 2014 and 2015
# filter only relevant columns and items
# add column with clean up description
# save for the rest of the people (and Solr)

DATA_DIR <- "~/R/LCF/orig_data/"
SCRIPT_DIR <- "~/R/LCF/scripts/"
setwd("~/R/LCF")
source(paste0(SCRIPT_DIR, 'lcf9_split_EXPDESC.R'))

library("ggplot2")
library("dplyr")
library("tidyr")
library("readr")

foodies15 <- read_csv(paste0(DATA_DIR, "DEFdry_5Q1_42015.csv"))
foodies14 <- read_delim(paste0(DATA_DIR, "for_big_data_14_RawX.csv",  delim ="\t"))

foodies5 <- foodies15 %>% ungroup() %>% 
  mutate(description=ItemDesc, paid=as.integer(AmtPaid), coicop=as.integer(CCPNODOT), 
         quantity=as.integer(StndQuan), units=as.integer(StndUnit), shop=as.integer(Shop), qtr=as.integer(qtr)) %>%
  select(description, paid, coicop, quantity, units, shop, qtr)%>%
  filter(coicop <20000, !is.na(quantity))
foodies4 <- foodies14 %>% ungroup() %>% 
  mutate(description=EXPDESC, paid=as.integer(Paid1), coicop=as.integer(FinCode), 
         quantity=as.integer(MAFFQuan), units=as.integer(MAFFUnit),shop=as.integer(Shop), qtr=as.integer(0)) %>%
  select(description, paid, coicop, quantity, units, shop, qtr)%>%
  filter(coicop <20000, !is.na(quantity))

foodies <- rbind(foodies4,foodies5)
rm(foodies15, foodies14, foodies4, foodies5)

write_csv(foodies, path=paste0(DATA_DIR, "foodies14+15RawX.csv"))

#########################################################
#clean-up description - takes really long  cca 20min?
res <- list()
for (i in 1:75) {print(i)  # the whole thing doesn't fit into my RAM, so I do it in chunks 
  print(system.time(res[[i]] <- split_EXPDESC(foodies$description[1:10000+(i-1)*10000])))
  }
res[[76]] <- split_EXPDESC(foodies$description[750001:nrow(foodies)])
splitdescr<-as.data.frame(do.call('rbind', res))

save(splitdescr, file='splitdescr.RData')
foodies$cleandescr <- splitdescr$longdescr
foodies$offer<- splitdescr$offer
foodies$pkg <-splitdescr$pkg

test_set <- foodies %>% filter(qtr==4) %>% 
  group_by(cleandescr, coicop, paid, quantity, units, shop) %>%
  summarise(count = n()) %>% 
  ungroup() %>% arrange(coicop, paid, cleandescr)
train_set <- foodies %>% filter(qtr!=4) %>% 
  group_by(cleandescr, coicop, paid, quantity, units) %>%
  summarise( shop = paste(sort(unique(shop)), collapse=' '), count = n()) %>% 
  ungroup() %>% arrange(coicop, paid, cleandescr)

write_csv(test_set, path=paste0(DATA_DIR, "test_set.csv"))
write_csv(train_set, path=paste0(DATA_DIR, "train_set.csv"))
save(test_set, train_set, file="foodies_train-test.RData")
