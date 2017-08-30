#############################################################
# LCF - Big Data project 2016/17  (amount imputation using historic data)
#
# This code implements evaluating and visualising of SOLR classification performance 
#
# date: February 2017
# author: ivyONS
############################################################

# evaluating and visualising solr classification performance
# run lcf11_solr_query first !!!

est<-as.data.frame(do.call('rbind', solr_est))
test_set$quan_proxy <- est$proxy
test_set$quan_est <- est$quantity
test_set$coicop_est <- est$coicop
test_set$coicop_separate <- est$V1 == test_set$coicop

test_set %>% group_by(coicop%/%100) %>% summarise(coicop_accu = sum(coicop_est, na.rm=T)/n(),
                                                  coicop_accu2 = sum(coicop_separate, na.rm=T)/n(),
                                                  quan_accu = sum(quan_est, na.rm=T)/n(),
                                                  quan_proxy = sum(quan_proxy<=10, na.rm=T)/n())

test_set %>% ungroup() %>% summarise(coicop_accu = sum(coicop_est, na.rm=T)/n(),
                                     coicop_accu2 = sum(coicop_separate, na.rm=T)/n(),
                                                  quan_accu = sum(quan_est, na.rm=T)/n(),
                                                  quan_proxy = sum(quan_proxy<=10, na.rm=T)/n())
test_set$proxy <- factor(ifelse(is.na(test_set$quan_proxy), 20, test_set$quan_proxy))
test_set$quan_proxy[is.na(test_set$quan_proxy)] <- 20

save(test_set, file='solr_test_set.RData')
test_set$coicop_group <- test_set$coicop%/%100

ggplot(test_set, aes(x=is.na(quan_proxy), fill=-quan_proxy, group=quan_proxy)) +
  geom_bar(position='stack')+
  theme_minimal() + facet_grid(.~coicop_group) +
  xlab('COICOP group') +
  theme(axis.text.x=element_blank())


