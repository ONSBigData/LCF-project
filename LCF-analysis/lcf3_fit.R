#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# compare several models for amount estimation on subset of LCF data
#
# date: August 2016
# author: ivyONS
############################################################

# make sure the data loads consistently
foodies <- read.csv('orig_data/LCF.csv', header = T, stringsAsFactors = F) # problem with quotes in 10" pizza
foodies <- read.csv('orig_data/LCFinch.csv', header = T, stringsAsFactors = F)
foodies2 <- read.csv('orig_data/for_big_data_14_RawX.csv', sep = "\t", header = T, stringsAsFactors = F)
foodies3 <- read.csv('orig_data/nsmLCF.csv', header = T, stringsAsFactors = F)

#  find differencies in coicop:
min(which((foodies$FinCode!=foodies2$FinCode[1:nrow(foodies4)])&(foodies$FinCode!=foodies4$FinCode[31])))
min(which((foodies3$FinCode!=foodies2$FinCode)&(foodies3$FinCode!=foodies3$FinCode[31])))

###########################################
## outliers

out_dat <- dat[,c(1,2,3,4,5,6,7,8,16,17, 21) ]
out_dat1 <- cbind(out_dat[269901:404850,c(1:8,10:11)], out_dat[134951:269900,10:11], out_dat[1:134950, 10:11])
names(out_dat1)[9:14] <- c('lm_res', 'lm_perc', 'knn_res', 'knn_perc', 'knn2_res', 'knn2_perc')

ggplot(out_dat1, aes(x=knn_perc, y= lm_res, col= (lm_perc+knn_perc>1200))) +
  geom_jitter() +
  theme_bw() +theme( legend.position='none')

outie <- out_dat1 %>% filter(abs(knn_res)>1300)
outie2 <- out_dat1 %>% filter(lm_perc+knn_perc>1200)

ggplot(out_dat1  %>% filter(coicop==11422)
       , aes(x=amount3,  col= - knn_perc)) +
  geom_jitter(aes(y=amount3-knn_res)) + 
  scale_x_log10() +scale_y_log10() +
  xlab('Actual amount') + ylab('Predicted amount') +
  facet_wrap(~coicop, scale='free') +
  theme_bw() +theme( legend.position='none')

tmp<-out_dat1  %>% group_by(coicop, amount3, pred = amount3-knn_res) %>% summarize(n=n()) %>% ungroup() %>% arrange(n) 

ggplot(data=out_dat1 #%>% filter(coicop==11422)
       ,   aes(x= amount3, y=amount3-knn_res) ) +
  stat_density2d() +
  geom_jitter(data=tmp #%>% filter(coicop==11422)
              ,   aes(x= amount3, y=pred, size=n, col= -n))  +
  stat_ellipse(level=.95) +
  #stat_quantile(quantiles=c(.05,.95),formula=y~x)+
  #geom_smooth(method='lm', se=F) +
  scale_x_log10() +scale_y_log10() +
  xlab('Actual amount') + ylab('Predicted amount') +
  facet_wrap(~coicop, scale='free') +
  ggtitle('Yoghurts: predicted vs. recorded amount') +
  theme_bw() +theme( legend.position='none')


########################
#yogurts
yog <- dat %>% filter(coicop==11441) %>% mutate(amount=amount3, price_bins =a_bins(price), amount_bins = a_bins(amount3))
yog <-yog %>% filter(classifier=='knn1')

g1 <- ggplot(yog, aes(y = price, x = amount, color=offer)) +
  ggtitle('Yoghurts: scatter plot (with jitter)')+
  geom_jitter(size=1, height=.05, width=.05) + #geom_text(aes(label=descr),hjust=0, vjust=0) +
  geom_smooth(aes(group=offer), se=F, method = 'lm') +
  theme_bw() +scale_x_log10() + scale_y_log10() +
  theme( legend.position='bottom') #+scale_color_brewer(palette = 'Set1')

g3 <- ggplot(yog, aes(y=price, x= amount_bins, group=amount_bins)) +
  ggtitle('Yoghurts: price distribution per amount bins')+
  #geom_jitter(height = 0) +
  geom_violin(scale='count', fill = '#a6cee3', colour = '#1f78b4', alpha=.6, width=.7) + #draw_quantiles = c(0.25, 0.5, 0.75), width=1) +
  theme_bw() +scale_x_log10() + scale_y_log10() +
  theme( legend.position='bottom')

g4 <- ggplot(yog, aes(y=amount, x= price_bins, group=price_bins)) +
  ggtitle('Yoghurts: amount distribution per price bins')+
  #geom_jitter(height = 0) +
  geom_violin(scale='count',  fill = '#a6cee3', colour = '#1f78b4', alpha=.6,width=.8) + #draw_quantiles = c(0.25, 0.5, 0.75), width=1) +
  theme_bw() +scale_x_log10() + scale_y_log10() +
  theme( legend.position='bottom') + coord_flip() +theme( legend.position='none')

g2 <- ggplot()+
  ggtitle('Yoghurts: price per amount - simplified into bins')+
  geom_point(data=yog %>% group_by(amount_bins, price_bins) %>%summarize(nn=n()), aes(x=amount_bins, y= price_bins, size=nn))  +
  stat_density2d(data=yog, aes(y = price, x = amount, fill=-..level..), geom='polygon', alpha=.2, n=100)+
  geom_point(data=yog %>% group_by(amount_bins, price_bins) %>%summarize(nn=n()), aes(x=amount_bins, y= price_bins, size=nn))  +
  theme_bw() +scale_x_log10() + scale_y_log10() + scale_size_area(0,6)+
  theme( legend.position='none')

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)

for (i in 1:43){
  i=15
  tmp<- cbind(yog, word = m[,i]) %>% arrange(word) 
  g <- ggplot(tmp, aes(y = price, x = amount, color=factor(word==1, levels=c(T, F)))) +
  ggtitle(paste0('Yoghurts: scatter plot - ', colnames(m)[i]))+
  geom_jitter(size=1, height=.05, width=.02) + #geom_text(aes(label=descr),hjust=0, vjust=0) +
  theme_bw() +scale_x_log10() + scale_y_log10() +
  theme( legend.position='bottom') + guides(colour=guide_legend(title= colnames(m)[i])) #+scale_color_brewer(palette = 'Set1')
  print(g)
  g4=g
  }
