#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code implements nearest neighbor classification on the historic LCF data and evaluates the performance (test =  last quater data)
#
# date: August 2016
# author: ivyONS
############################################################

setwd("~/R/LCF")
source('scripts/lcf1_helper.R')
library("ggplot2")
library("dplyr")
library("tidyr")

#########################################################
dairy <- read.csv('orig_data/2015_Q2_to_2016_Q1_RawdryX.csv', header = T, stringsAsFactors = F)
#dairy <- read.csv('114_dairy.csv', header = T, stringsAsFactors = F)
#bread <- read.csv('111_bread.csv', header = T, stringsAsFactors = F)


nn1dist <- lapply(unique(dairy$coicop), function(x)  {print(x)
  with(dairy %>% filter(coicop==x), cbind(coicop, descr, price, amount3, shop, t(nn1(descr , price , amount3 , shop, type=1, details=T))))})  
nn1dist2 <- lapply(unique(bread$coicop), function(x)  {print(x)
  with(bread %>% filter(coicop==x), cbind(coicop, descr, price, amount3, shop, t(nn1(descr , price , amount3 , shop, type=1, details=T))))})  
nn1dist_df <- as.data.frame(do.call("rbind", c(nn1dist,nn1dist2)))
nn1dist_df$descr<-gsub('  ','',as.character(nn1dist_df$descr))
for (i in c(1, 3:9)) nn1dist_df[,i]<-as.numeric(as.character(nn1dist_df[,i]))
sum(is.na(nn1dist_df))

save(nn1dist_df, file='nn1dist.RData')
#########################################################

dat <- nn1dist_df %>% 
  mutate(residuals= amount3-est,
         cat = paste0('COICOP categories ', as.character(coicop %/% 100), '**'),
         coicop2 = as.character(coicop%%100),
         perc_error =  abs(residuals)/amount3*100) 
trim=55
error_quant <- dat %>%  group_by(cat, coicop2) %>% 
  mutate(perc_error = ifelse(is.na(perc_error), 10000,perc_error)) %>%
  summarise(l = quantile(perc_error, 0.25)[[1]],
            m = median(perc_error), 
            u = pmin(quantile(perc_error, 0.75)[[1]],trim) ,
            'Number_of_products' =n())%>% ungroup()

error_quant <- rbind (error_quant, dat %>%  group_by(cat) %>% 
                        mutate(perc_error = ifelse(is.na(perc_error), 10000,perc_error)) %>%
                        summarise(coicop2 = 'all',
                                  l = quantile(perc_error, 0.25)[[1]],
                                  m = median(perc_error), 
                                  u = pmin(quantile(perc_error, 0.75)[[1]],trim) ,
                                  'Number_of_products' =n())%>% ungroup())

## final plot for quartiles of percentual error
ggplot(error_quant,
       aes(x = coicop2, #interaction(Classifier, coicop2), 
           y = m, ymin = l, ymax = u)) +
  #geom_pointrange(position=position_dodge(width = 0.8)) + #coord_flip() +
  geom_errorbar(position=position_dodge(width = 0.8), aes(alpha=Number_of_products)) +
  geom_pointrange(position=position_dodge(width = 0.8), aes(ymin = m, ymax = m, size=Number_of_products)) +
  scale_size(range = c(0, 2)) + scale_alpha(range=c(.3,1)) +ylim(0, trim) +
  facet_grid(.~cat, scale='free_x', space = "free_x") + xlab('') +
  ylab('Percentual error at product quartiles (lower=better)')+
  ggtitle('Errors of simple estimators for weights of bread & dairy') +
  theme_bw() +  theme( legend.position='bottom', strip.background=element_rect(fill="white"))

###############################################
# description length
descr_len <- function(descr){
corp <- Corpus(VectorSource(descr))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
ctrl <- list(wordLengths = c(2, Inf))
dtm <- DocumentTermMatrix(corp, control=ctrl)
dtm <-  as.matrix(dtm)
return(rowSums(dtm))
}
dat <- dat %>% group_by(coicop) %>% mutate(descr_length = descr_len(EXPDESC))

#penalty for -log(score), pricediff/price and -shopmatch with weights:
penalty_weights <- c(1,1,1)
dat$penalty <- with(dat, as.vector(cbind(1-score, pricediff/Paid1, 1-shopmatch) %*% penalty_weights))

#sort by penalty, plot percentage of correctly estimated 
dat <- dat %>% ungroup() %>% arrange(penalty, -descr_length, Paid1)

perc_class <- cumsum(dat$residuals==0)/sum(dat$residuals==0)
accuracy <- cumsum(dat$residuals==0)/1:nrow(dat)
penalty_df <- rbind(penalty_df, 
  data.frame(ind=1:nrow(dat), perc_class=perc_class, cum_accu=accuracy, 
                         penalty_weights = paste(penalty_weights, collapse = ' ') ) )

plot(perc_class, type='l')
lines(accuracy, col='black')

gg_df <- penalty_df %>% gather(type, proportion, 2:3)
gg_df$type <- factor(gg_df$type, labels=c('cumsum - correctly estimated weights', 'accuracy so far'))

ggplot(gg_df, aes(x=ind, col=penalty_weights))+
  geom_line(aes(y=proportion, linetype=type)) +
  geom_vline(xintercept = 81409) +
  theme_bw() +  theme( legend.position='bottom')

penalty_df2 <- data.frame(ind=1:nrow(dat), weight_match = as.numeric(dat$residuals==0), perc_class=perc_class, cum_accu=accuracy, 
           penalty_weights = paste(penalty_weights, collapse = ' '), cat=dat$cat, coicop=dat$coicop ) 

gg_df <- penalty_df2 %>% gather(type, proportion, 2:3)
ggplot(gg_df, aes(x=ind, col=coicop2))+
  geom_point(aes(y=proportion)) +
  geom_vline(xintercept = 81409) +
  theme_bw() +  theme( legend.position='bottom') + guides(col = guide_legend(nrow = 1))

subs <- names(which (table(penalty_df2$coicop[1:81409])>2500))
penalty_df2$coicop2 <- as.character(penalty_df2$coicop)
penalty_df2$coicop2[!(penalty_df2$coicop %in% subs)] <- penalty_df2$cat[!(penalty_df2$coicop %in% subs)]

ggplot(penalty_df2, aes(x=ind))+
  geom_point(aes(y=weight_match,  col=coicop2)) +
  geom_smooth(aes(y=weight_match), se=F) +
  geom_vline(xintercept = 81409) +
  theme_bw() +  theme( legend.position='bottom') + guides(col = guide_legend(nrow = 1))

penalty_df3 <- data.frame(ind=1:nrow(dat), descr_match = as.numeric(dat$penalty==0), perc_class=perc_class, cum_accu=accuracy, 
                          penalty_weights = paste(penalty_weights, collapse = ' '), cat=dat$cat, coicop=dat$coicop ) 
penalty_df3 <- penalty_df3 %>% group_by(cat) %>% mutate(ind_cat = cumsum(coicop>0)/n())
interc <- penalty_df3 %>% group_by(cat) %>% summarize(ind_cat = sum(descr_match)/n())

ggplot(penalty_df3, aes(x=ind_cat, y=weight_match,  col=cat)) +
  geom_smooth(se=F, method='loess', span=.5) +
  geom_vline(data=interc, aes(xintercept = ind_cat, col=cat)) +
  ylim(0,1)+ylab('Proportion of correctly estimated weights')+
  xlab('Products sorted by the estimate confidence (perfect match up to the vertical line)')+
  theme_bw() +  theme( legend.position='bottom',  axis.text.x = element_blank()) + 
  guides(col = guide_legend(nrow = 1, title=' '))+
  ggtitle('Accuracy vs confidence of the weight prediction')

#################################################
# prediction confidence
#################################################

#foodies <- read.csv('orig_data/LCFinch.csv', header = T, stringsAsFactors = F)
foodies <- read.csv('orig_data/2015_Q2_to_2016_Q1_RawdryX.csv', header = T, stringsAsFactors = F)
food <- foodies[,c(8,11:12, 15:16, 19, 29)]
food <- food %>%  mutate(coicop = as.numeric(FINCODE),
                         cat = paste0(as.character(coicop %/% 100), '**'),
                         coicop2 = as.character(coicop%%100))
tapply(food$cat,is.na(food$MAFFUnit), table)
food <- food %>% filter(coicop<20000, is.na(MAFFUNIT)==F, is.na(SHOP)==F)
rm(foodies)
food <- food %>% group_by(coicop) %>% mutate( offer2 = is_offer(EXPDESC)) %>% ungroup()
hist(table(food$coicop))

####### very time consuming:
sort(table(food$coicop), decr=T)
coicops<- names(sort(table(food$coicop), decr=T))
nn1dist <- lapply(coicops, function(x)  {
               cat (c(which(coicops==x), x, sum(food$coicop==x))) 
               print (Sys.time())
  with(food %>% filter(coicop==x), cbind(coicop, EXPDESC, Paid1, MAFFQuan, Shop , cat , offer , t(nn1(EXPDESC , Paid1 , MAFFQuan, Shop , type=1, details=T))))})  

nn1dist_df <- as.data.frame(do.call("rbind", nn1dist))
#save(nn1dist, file='nn1dist_big.RData')
for (i in c(2, 6)) nn1dist_df[,i]<-gsub('  ','',as.character(nn1dist_df[,i]))
for (i in c(1, 3:5, 8:13)) nn1dist_df[,i]<-as.numeric(as.character(nn1dist_df[,i]))
nn1dist_df[,7]<-as.numeric(nn1dist_df[,7]==TRUE)
sum(is.na(nn1dist_df))
save(nn1dist_df, file='nn1dist_big2.RData')

###########################################

dat <- nn1dist_df %>% 
  mutate(residuals= MAFFQuan-est,
         coicop2 = as.character(coicop%%100%/%10),
         perc_error =  abs(residuals)/MAFFQuan*100) 
trim=150
error_quant <- dat %>%  group_by(cat, coicop2) %>% 
  mutate(perc_error = ifelse(is.na(perc_error), 10000,perc_error)) %>%
  summarise(l = quantile(perc_error, 0.25)[[1]],
            m = median(perc_error), 
            u = pmin(quantile(perc_error, 0.75)[[1]],trim) ,
            'Number_of_products' =n())%>% ungroup()

error_quant <- rbind (error_quant, dat %>%  group_by(cat) %>% 
                        mutate(perc_error = ifelse(is.na(perc_error), 10000,perc_error)) %>%
                        summarise(coicop2 = 'all',
                                  l = quantile(perc_error, 0.25)[[1]],
                                  m = median(perc_error), 
                                  u = pmin(quantile(perc_error, 0.75)[[1]],trim) ,
                                  'Number_of_products' =n())%>% ungroup())

## final plot for quartiles of percentual error
ggplot(error_quant %>% filter(coicop2=='all'),
       aes(x = coicop2, #interaction(Classifier, coicop2), 
           y = m, ymin = l, ymax = u)) +
  #geom_pointrange(position=position_dodge(width = 0.8)) + #coord_flip() +
  geom_errorbar(position=position_dodge(width = 0.8), aes(alpha=Number_of_products)) +
  geom_pointrange(position=position_dodge(width = 0.8), aes(ymin = m, ymax = m, size=Number_of_products)) +
  scale_size(range = c(0, 2)) + scale_alpha(range=c(.3,1)) +ylim(0, trim) +
  facet_grid(.~cat, scale='free_x', space = "free_x") + xlab('') +
  ylab('Percentual error at product quartiles (lower=better)')+
  ggtitle('Errors of simple estimators for weights of bread & dairy') +
  theme_bw() +  theme( legend.position='bottom', strip.background=element_rect(fill="white"))

dat$penalty <- with(dat, as.vector(cbind(1-score, pricediff/Paid1, 1-shopmatch) %*% penalty_weights))

ggplot(penalty_df3, aes(x=ind_cat, y=weight_match,  col=cat)) +
  geom_smooth(se=F)+#, method='loess', span=.5) +
  geom_vline(data=interc, aes(xintercept = ind_cat, col=cat)) +
  ylim(0,1)+ylab('Proportion of correctly estimated weights')+
  xlab('Products sorted by the estimate confidence (perfect match up to the vertical line)')+
  theme_bw() +  theme( legend.position='bottom',  axis.text.x = element_blank(),
                       strip.background=element_rect(fill="white")) + 
  facet_wrap(~cat)+
  guides(col = 'none')+
  ggtitle('Accuracy vs confidence of the weight prediction')


coicop_sum <- rbind(dat %>% filter(penalty==0) %>% group_by(cat, coicop) %>% 
  summarise( sd_quan = sd(MAFFQuan), sd_est = sd(est), 
             m_quan = mean(MAFFQuan), m_est=mean(est), 
             abs_err = mean(abs(MAFFQuan-est)),
             BR = (m_quan-m_est)/(sd_quan+.0001),
             abs_BR = abs_err/(sd_quan+.0001),
             n=n(), type='match'
             )  %>% ungroup(),
dat %>% group_by(cat, coicop) %>% 
  summarise( sd_quan = sd(MAFFQuan), sd_est = sd(est), 
             m_quan = mean(MAFFQuan), m_est=mean(est), 
             abs_err = mean(abs(MAFFQuan-est)),
             BR = (m_quan-m_est)/(sd_quan+.0001),
             abs_BR = abs_err/(sd_quan+.0001),
             n=n(), type='all'
  )  %>% ungroup())

  
ggplot(coicop_sum %>% filter(type=='match'), aes(x=n, y=BR, col=cat))+
  geom_point()+#aes(shape=type))+ geom_line(aes(group=coicop))+
  scale_x_log10()+ #scale_y_log10()+
  xlab('Number of items in Coicop')+ylab('Mean error per standart deviation in Coicop')+
  ggtitle("Bias Ratio illustration for weights' estimator (of matched items)") + 
  theme_minimal()

##################################################
#penalty for -log(score), pricediff/price and -shopmatch with weights:
penalty_weights <- c(1,1,1,1)
dat$penalty4 <-  with(dat, cbind(1-score, pricediff/Paid1, 1-shopmatch, 1-((candid_match>1)&(candid_match==candid_num))  )) %*% as.matrix(penalty_weights)
dat$penalty3 <-  with(dat, cbind(1-score, pricediff/Paid1, 1-shopmatch )) %*% as.matrix(penalty_weights[1:3])

#sort by penalty, plot percentage of correctly estimated 
dat <- dat %>% ungroup() %>% arrange(penalty3, penalty, -descr_length, candid_match, Paid1)

penalty_df3 <- data.frame(ind=1:nrow(dat), descr_match = as.numeric(dat$penalty==0), weight_match = as.numeric(dat$residuals==0), perc_class=perc_class, cum_accu=accuracy, 
                          penalty_weights = paste(penalty_weights, collapse = ' '), cat=dat$cat, coicop=dat$coicop,  descr_match3 = as.numeric(dat$penalty3==0) ) 
penalty_df3 <- penalty_df3 %>% group_by(cat) %>% mutate(ind_cat = cumsum(coicop>0)/n())
interc <- penalty_df3 %>% group_by(cat) %>% summarize(ind_cat = sum(descr_match)/n())
interc3 <- penalty_df3 %>% group_by(cat) %>% summarize(ind_cat = sum(descr_match3)/n())

ggplot(penalty_df3, aes(x=ind_cat, y=weight_match,  col=cat)) +
  geom_smooth(se=F, method='loess', span=.5) +
  geom_vline(data=interc, aes(xintercept = ind_cat, col=cat)) +
  geom_vline(data=interc3, aes(xintercept = ind_cat, col=cat)) +
  ylim(0,1)+ylab('Proportion of correctly estimated weights')+
  xlab('Products sorted by the estimate confidence (perfect match up to the vertical line)')+
  theme_bw() +  theme( legend.position='bottom',  axis.text.x = element_blank(),
                       strip.background=element_rect(fill="white")) + 
  facet_wrap(~cat)+
  guides(col = 'none')+
  ggtitle('Accuracy vs confidence of the weight prediction')

