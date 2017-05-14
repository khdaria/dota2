
ggplot(pmp, aes(as.POSIXct(start_time,origin="1970-01-01"))) + geom_histogram()

matches$date2 <- as.POSIXct(matches$start_time,origin="1970-01-01")

matches$col <- ifelse(matches$match_id %in% c(same_seria$match_id, not_same_seria$match_id),1 ,0 )

ggplot(filter(matches, tier=="premium"), aes(date2, fill=factor(col))) + geom_histogram() + 
  ylab("Количество матчей") + scale_x_datetime(breaks = date_breaks("6 months"),
                                               labels = date_format("%m-%Y")) +
  xlab("Дата") + theme(panel.grid.major = element_line(colour="lightblue",lineend="round"),
                       panel.grid.minor = element_line(colour="lightgrey",lineend="butt"),
                       panel.background = element_rect(fill="white"), legend.position="None") + 
  scale_fill_manual(values=c("darkgrey", "lightblue"))
  



ggplot(pm5, aes(outcome, prev_adv)) + geom_boxplot() + facet_wrap(~same_seria)
ggplot(pm5, aes(prev_adv)) + geom_histogram() + facet_wrap(~same_seria)
ggplot(winrates, aes(winrate)) + geom_histogram(bins=20)+ facet_wrap(~year) + geom_vline(xintercept =0.5)

ggplot(pmp_bc, aes(log(aggr))) + geom_histogram()
ggplot(pmp_bc, aes(kills_per_min, aggr)) + geom_point()
ggplot(pmp_bc, aes(tower_damage, aggr)) + geom_point()



ggplot(not_same_seria, aes(radiant_win, aggr_dif)) + geom_boxplot()

ggplot(not_same_seria, aes(prev_adv, aggr_dif)) + geom_point()

ggplot(not_same_seria, aes(radiant_win, rad_aggr_adv)) + geom_boxplot() 
+ ylim(-25,25)


ggplot(same_seria, aes(prev_adv_abs)) + geom_histogram()
ggplot(same_seria, aes(rad_mmr_adv)) + geom_histogram()


g1 <- ggplot(not_same_seria, aes(prev_adv_dif)) + geom_histogram()
g2 <- ggplot(not_same_seria, aes(rad_mmr_adv)) + geom_histogram()
g3 <- ggplot(not_same_seria, aes(rad_kill_adv)) + geom_histogram()
g4 <- ggplot(not_same_seria, aes(rad_aggr_adv)) + geom_histogram()
g5 <- ggplot(not_same_seria, aes(adv_more_than_0_prop_dif)) + geom_histogram()


multiplot(g1,g2,g3,g4,g5,cols=2)

same_seria$cl <- as.factor(m_gold$cl)[match(same_seria$prev_match_id,m_gold$match_id)]
same_seria$cl <- ifelse(same_seria$cl ==3, "comeback", "anotcomeback")
same_seria$prev_outcome <- ifelse(same_seria$prev_outcome=="won", "a_won","lost")
fit_s <- glm(radiant_win ~ prev_adv_abs*prev_outcome+ rad_mmr_adv + cl, data=same_seria, family="binomial")
summary(fit_s)
exp(fit_s$coefficients)
car::vif(fit_s)

g1 <- ggplot(not_same_seria, aes(radiant_win,rad_aggr_adv)) + geom_boxplot() + xlab("TEAM A win") +
  ylab("Realization of Advantage") + theme(text=element_text(size=15))
g2 <- ggplot(not_same_seria, aes(radiant_win,scale(rad_kill_adv))) + geom_boxplot() + xlab("TEAM A win") +
  ylab("Aggressiveness") + theme(text=element_text(size=15))
multiplot(g1,g2,cols=2)





not_same_seria$cl <- as.factor(m_gold$cl)[match(not_same_seria$prev_match_id,m_gold$match_id)]

fit_ns <- glm(radiant_win ~ prev_adv_dif + rad_mmr_adv+ 
                 adv_more_than_0_prop_dif, data=not_same_seria,
              family="binomial")

fit_ns <- glm(radiant_win ~ rad_aggr_adv , data=not_same_seria,
              family="binomial")


fit_ns <- ctree(radiant_win ~ prev_adv_dif + rad_mmr_adv + rad_kill_adv + 
                adv_more_than_0_prop_dif, data=not_same_seria)
plot(fit_ns)

summary(fit_ns)
car::vif(fit_ns)
exp(fit_ns$coefficients)

prev_adv <- data.frame()
prev_mmr <- data.frame()

for(i in c(2014:2017)){
  fit_s <- glm(radiant_win ~ prev_adv + rad_mmr_adv, data=filter(same_seria,year==i),
               family="binomial")
  
  df_prev_adv <- data.frame(prev_adv = seq(-0.5,0.5, 0.01), rad_mmr_adv=rep(0,101))
  df_mmr_adv <- data.frame(rad_mmr_adv = seq(-3000,3000, 60), prev_adv=rep(0,101))
  df_prev_adv$pred <- predict(fit_s, df_prev_adv, type="response")
  df_mmr_adv$pred <- predict(fit_s, df_prev_adv, type="response")
  df_prev_adv$year=i
  df_mmr_adv$year=i
  prev_adv <- rbind(prev_adv, df_prev_adv)
  prev_mmr <- rbind(prev_mmr, df_mmr_adv)
  
}

ggplot(prev_adv, aes(prev_adv, pred, color=factor(year))) + geom_line()
ggplot(prev_mmr, aes(rad_mmr_adv, pred, color=factor(year))) + geom_line()


pred_s <-ifelse(predict(fit_s, same_seria, type="response") >0.5,"won","lost")
table(pred_s, same_seria$radiant_win)
table(pred_s, same_seria$radiant_win)/nrow(same_seria)

pred_ns <-ifelse(predict(fit_ns, not_same_seria, type="response") >0.5,"won","lost")
table(pred_ns, not_same_seria$radiant_win)
table(pred_ns, not_same_seria$radiant_win)/nrow(not_same_seria)



fit <- ctree(radiant_win ~ prev_adv + rad_mmr_adv + rad_kill_adv + 
             rad_aggr_adv + adv_more_than_0_prop   + duration, data=same_seria)

plot(fit)


fit <- ctree(radiant_win ~ prev_adv_dif + rad_mmr_adv + rad_kill_adv + 
                rad_aggr_adv  + adv_more_than_0_prop_dif, data=not_same_seria)


plot(fit)

fit <- lm(adv ~ prev_adv + mmr_winner_adv + aggr_more_winner + kills_more_winner + winner_rises , data=same_seria)
summary(fit)



fit <- glm(radiant_win ~ prev_adv + rad_mmr_adv + rad_kill_adv + 
             rad_aggr_adv + adv_more_than_0_prop   + duration, data=same_seria,
           family="binomial")

car::vif(fit)
summary(fit)




fit <- glm(radiant_win ~ prev_adv_dif + rad_mmr_adv + rad_kill_adv + 
             rad_aggr_adv  + adv_more_than_0_prop_dif, data=not_same_seria,
           family="binomial")



fit <- glm(radiant_win ~ prev_adv_dif + rad_mmr_adv + adv_more_than_0_prop_dif  +rad_aggr_adv , data=not_same_seria,
           family="binomial")


car::vif(fit)
summary(fit)
plot(fit)



pls <- post_match_players %>% group_by(match_id,team) %>% summarise(kda = mean(kda, na.rm=T), tower_damage=sum(tower_damage))
ggplot(pls, aes(tower_damage,kda))+geom_point()
ggplot(pls, aes(scale(kda)/scale(tower_damage)))+geom_histogram()

ggplot(pls, aes(tower_damage))+geom_histogram()
ggplot(pls, aes(kda))+geom_histogram()




########################################################


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
