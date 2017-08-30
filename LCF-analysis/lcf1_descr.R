#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code implements basic analysis, summary statistics of available historic LCF data
#
# date: July 2016
# author: ivyONS
############################################################

setwd("~/R/LCF")
source('scripts/lcf1_helper.R')
library("ggplot2")
library("dplyr")

#########################################################
# loading subset of data corresponding to coicop families: dairy & bread
# in terminal:
# cat nsmLCF.csv | grep ",1\.1\.4\." > 114dairy.csv
# cat nsmLCF.csv | grep ",1\.1\.1\." > 111bread.csv

if (T) { # if we can to load already prepared data in the file 'bread&dairy_15columns_csv.7z'
 dairy <- read.csv('114_dairy.csv', header = T, stringsAsFactors = F)
 bread <- read.csv('111_bread.csv', header = T, stringsAsFactors = F)
} else {
  dairy <- read.csv('114dairy.csv', header = F, stringsAsFactors = F)
  bread <- read.csv('111bread.csv', header = F, stringsAsFactors = F)
  dairy2 <- read.csv('114dairy2.csv', header = F, stringsAsFactors = F)
  bread2 <- read.csv('114bread2.csv', header = F, stringsAsFactors = F)

#########################################################
# some pre-processing - to be skiped if data obtained from 'bread&dairy_15columns.csv.7z'
  for (dtn in c('dairy', 'bread')){
    dt <- get(dtn)
    names(dt) <- c('id','descr','coicop2', 'descr2', 'coicop', 'price', 'amount', 'unit')
    print(table(dt$coicop))
    # convert units
    dt$amount2 <- convert_unit('amount', dt$unit, dt$amount) 
    dt$unit2 <- convert_unit('unit', dt$unit)
    # create bins for amounts separately for each coicop:
    dt <- dt %>% group_by(coicop) %>% mutate(bins = a_bins(amount2)) %>% ungroup()  
    #dt$bins <-  factor(dt$bins, levels = sort(unique(dt$bins)))
    print(paste(dtn,': Extracting offer status from description... '))
    dt$offer <- is_offer(dt$descr)
    assign(dtn, dt)
  }
  for (dtn in c('dairy2', 'bread2')){
    dt <- get(dtn)
    names(dt) <- c("ImpDiary","DiaryTyp","PAB_DONE","Transfer","Month","Day","PUR","EXPDESC","CODING","CodeDesc","FinCode","Paid1","Quantity","Units","MAFFQuan","MAFFUnit","UnitCost","UnitCos2","SoftChek","RangeLo","RangeHi","RatioLo","RatioHi","Portion","PortOth","ChildD","ToWhom","NtAmt1","Shop","Paid2","NtAmt2","EODay","EOPur","EOItems","EODESC","EOCODE","EOCodeDesc","EOFinCod","EOOutLt","EOSpnDay","EOSpnPur","FoodTyp","Outlet","OnOff","HotCold","TypeAlc","TypeBeer","TypeCide","TypeWine","TypeCham","TypeFort","TypeSpir","TypeSpmx","TypeLiqu","TypeLqmx","TypeCktl","TypeApop","NumDrink","DVNumeo","EOSpnCod","PayEOut","HomeDay","HomePur","HomeDesc","HomeCode","HoCodeDesc","HOFinCod","Quantity2","Units2","MAFFQuan2","MAFFUnit2","Source","RefDay","RefPur","RefDesc","RefCode","ReCodeDesc","ReFinCod","Refpaid","RefAmt","RefNet","YthDay","YthPur","FoodTyp2","Outlet2","HotCold2","YthCod","PayYouth")
    dt <- dt[,c(8,10:16, 28:29)]
    dt <- dt[!is.na(dt$Paid1),]
    names(dt) <- c('descr', 'descr2', 'coicop', 'price', 'amount', 'unit', 'amount3', 'unit3', 'price2', 'shop')
    dt$shop2 <- as.numeric(dt$shop %in%  c(20 ,  32  , 40  , 70 , 140 , 480, 1090, 1130, 1300 ,1420 ,1550 ,1980, 2190, 2370 ))*dt$shop   
    dt$amount2 <- convert_unit('amount', dt$unit, dt$amount) 
    dt$unit2 <- convert_unit('unit', dt$unit)
    # create bins for amounts separately for each coicop:
    dt <- dt %>% group_by(coicop) %>% mutate(bins = a_bins(amount2)) %>% ungroup()  
    #dt$bins <-  factor(dt$bins, levels = sort(unique(dt$bins)))
    print(paste(dtn,': Extracting offer status from description... '))
    dt$offer <- is_offer(dt$descr)
    assign(dtn, dt)
  }
}

#########################################################
#bunch of plots

ggplot(dairy, aes(y = price, x = amount3, color = unit)) +
  geom_jitter() +
  facet_wrap(~coicop, scales='free') +
  theme_bw() +scale_x_log10() +scale_y_log10()

ggplot(bread, aes(y = price, x = amount3, color = factor(descr))) +
  geom_point() +
  facet_wrap(~coicop, scales='free') +
  theme_bw() +scale_x_log10() +scale_y_log10()+
  theme(legend.position ='none')

ggplot(bread, aes(x=amount3)) +
  geom_histogram() +
  facet_wrap(~coicop, scales='free') +
  theme_bw() +scale_x_log10() 

ggplot(bread %>% filter(coicop==11121)
       , aes(y = amount2, x = bins, color = factor(bins))) +
  geom_point() +
  facet_wrap(~coicop, scales='free') +
  #geom_density2d(aes(alpha=..level..)) +
  theme_bw() +scale_x_log10() +scale_y_log10() #+ theme(legend.position ='none')

ggplot(bread #%>% filter(coicop==11121)
       , aes(y = price, x = as.numeric(bins), color = offer, fill = offer)) +
  geom_boxplot(aes(group = interaction(offer,bins))) + 
  geom_smooth(se=F, method='lm') +
  facet_wrap(~coicop, scales='free') +
  theme_bw() + scale_y_log10() #+ theme(legend.position ='none')

##############################################################
#### final nice plot:
big_coicop <- as.integer(union(names(which (table(bread$coicop)>100)), names(which (table(dairy$coicop)>100))))
ggplot(rbind(bread, dairy),# %>% filter(coicop %in% big_coicop)
        aes(y = price, x = bins, color = offer, fill = offer)) +
  geom_boxplot(aes(group = interaction(offer,as.factor(bins))),outlier.size=NA) + 
  geom_smooth(se=F, method='lm', alpha=.5) +
  facet_wrap(~coicop, scales='free', ncol=9) +
  ggtitle('Bread&Dairy products: Price for amount by COICOP & offer status') +
  theme_bw() + xlab('Amount category (KiloGrams or Litres, logscale)') + scale_x_log10() +
  ylab('Price (logscale)') + scale_y_log10() +
  theme(legend.justification=c(1,0), legend.position=c(.98,0), strip.background=element_rect(fill="white"))

#########################################################
####plot prices by shop
ggplot(dairy2 #%>% filter(coicop==11422)
       , aes(y = price, x = amount2, color = shop2==40, group=shop2==40)) +
  geom_jitter() +  geom_smooth(se=F, method=lm) +
  facet_wrap(~coicop, scales='free') +
  #geom_density2d(aes(alpha=..level..)) +
  theme_bw() +scale_x_log10() +scale_y_log10() #+ theme(legend.position ='none')

ggplot(bread2 #%>% filter(coicop==11422)
       , aes(y = price, x = amount2, color = factor(shop2), group=factor(shop2))) +
  geom_jitter() +  geom_smooth(se=F, method=lm) +
  facet_wrap(~coicop, scales='free') +
  #geom_density2d(aes(alpha=..level..)) +
  theme_bw() + scale_x_log10() + scale_y_log10() #+ theme(legend.position ='none')

##########################################################
#descritpion break down for yoghurts
yog <- dairy %>% filter(coicop==11441)

library("tm")
corpus <- Corpus(VectorSource(yog$descr))
corp <- tm_map(corpus, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
#corp <- tm_map(corp, removeWords, "/")

dtm <- DocumentTermMatrix(corp)
dtms <- removeSparseTerms(dtm,.999)
m <- as.matrix(dtms)
colSums(m)

###plot yogurts using dtm
library("reshape2")
yog_text <-  melt(m)
yog_text$amount <- yog$amount2[yog_text$Docs] 
yog_text$price <- yog$price[yog_text$Docs] 
yog_text$desc <- yog$desc[yog_text$Docs] 
yog_text$contains <- factor(yog_text$value>0)

ggplot(yog_text %>% filter(Terms %in% c("mbuy", "cpn", "rcd", "disc")), aes(y = price, x = amount, color = contains)) +
  geom_point() +
  facet_wrap(~Terms, scales='free') +
  theme_bw() +scale_x_log10() +scale_y_log10()

yog$sale <- rowSums(m[,c("mbuy", "cpn", "rcd", "disc")])>0
ggplot(yog, aes(y = price, x = amount2, color=sale)) +
  geom_point() + #geom_text(aes(label=descr),hjust=0, vjust=0) +
  geom_smooth(aes(group=sale), se=F, method = 'lm') +
  theme_bw() +scale_x_log10() +scale_y_log10()

##########################################################
# quantifying uniquesness-frequency of products
# match on coicop price weight (shop)
dat <- rbind(bread, dairy)

freq_dat <- dat %>% group_by(coicop, amount3, price, shop2) %>% summarize( tri_shop_n = n()) %>%
  group_by(coicop, amount3, price) %>% mutate( triple_n = sum(tri_shop_n))

freq_sum <- freq_dat %>% group_by(coicop) %>% summarize (coicop_n = sum(tri_shop_n), 
                               freq_shop = sum(tri_shop_n[tri_shop_n>1]), 
                               freq_triple = sum(tri_shop_n[triple_n>1])) 

# proprotion of products where there is another product with matching coicop*amount3*price(*shop2)
freq_dat %>% group_by(coicop%/%100 ) %>% summarize (coicop_n = sum(tri_shop_n), 
                                             freq_shop = sum(tri_shop_n[tri_shop_n>1])/coicop_n, 
                                             freq_triple = sum(tri_shop_n[triple_n>1])/coicop_n) 

# proportion of products where the coicop*price*shop identifies uniquelly the amount and at the same time there are at least 2 such products 
freq_dat %>% group_by(coicop, price, shop2) %>% mutate(unique_amount = n_distinct(amount3)) %>%
  group_by(coicop%/%100) %>% summarize(coicop_n = sum(tri_shop_n), 
                                 freq_shop = sum(tri_shop_n[(unique_amount==1)&(tri_shop_n>1)])/coicop_n)

