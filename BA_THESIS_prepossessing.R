library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(party)

post_match_players <- read_csv("BA_THESIS_post_match_players.csv")

pmp <- filter(post_match_players, !is.na(total_xp))
pmp <- pmp %>% dplyr::select(isRadiant, radiant_win, start_time, match_id, series_id, series_type, total_gold) %>% 
  group_by(match_id, series_id, series_type,radiant_win,start_time) %>% 
  summarise(gold_rad=sum(total_gold[isRadiant==T]), gold_dire=sum(total_gold[isRadiant==F])) %>% mutate(rad_adv=gold_rad-gold_dire) %>%
  ungroup()


pm <- select(post_match_players, match_id, team, solo_competitive_rank,pings,isRadiant) %>% 
  filter(! match_id %in% c(904638208, 1113864861, 1035670463, 1035813284, 1053745264, 1053890714,
                           1054064700, 1152403518, 1152518001)) %>% 
  filter(!is.na(team)) %>% ungroup() %>% 
  group_by(match_id, team) %>% 
  summarise(mmr=mean(as.numeric(solo_competitive_rank), na.rm=T), 
            mmr_na=sum(is.na(solo_competitive_rank)), 
            ping = max(pings,na.rm=T), 
            ping_na = sum(is.na(pings)),
            isRadiant = unique(isRadiant))


pmp_bc <-post_match_players %>% group_by(match_id, isRadiant, radiant_win, start_time) %>% select(match_id, isRadiant, radiant_win,start_time,
                                                                                                  kills_per_min,gold_per_min, xp_per_min, hero_healing_per_min, 
                                                                                                  hero_damage_per_min,tower_damage,
                                                                                                  last_hits_per_min, kda) %>% summarise_each(funs(sum)) %>%
  mutate(outcome = ifelse(isRadiant & radiant_win | !isRadiant & !radiant_win, "won","lost"))
names(pmp_bc)[1]<-"prev_match_id"
pmp_bc$duration <- gold_adv_summary$duration[match(pmp_bc$prev_match_id, gold_adv_summary$prev_match_id)]


pmp_bc <- pmp_bc %>% ungroup() %>% mutate(kills =kda, aggr = log((tower_damage/duration)))
pmp_bc <- select(ungroup(pmp_bc), prev_match_id, aggr, kills,isRadiant)
pmp_bc$prev_match_id <- as.character(pmp_bc$prev_match_id)

pm <- left_join(pm, pmp)
pm <- pm %>% mutate(outcome = ifelse(isRadiant & radiant_win | !isRadiant & !radiant_win, "won","lost"))
pm$adv <- ifelse(pm$isRadiant==T, (pm$gold_rad-pm$gold_dire)/(pm$gold_rad+pm$gold_dire), (pm$gold_dire-pm$gold_rad)/(pm$gold_rad+pm$gold_dire))
pm$match_id <- as.character(pm$match_id)
pm <- filter(pm, adv !=1 & adv !=-1)
pm <- pm %>% group_by(team) %>% arrange(team,match_id) %>% mutate(prev_match_id = c(NA, match_id[-length(match_id)]),
                                                                  prev_start_time = c(NA, start_time[-length(match_id)]),
                                                                  prev_adv = c(NA, adv[-length(adv)]),
                                                                  prev_outcome = c(NA, outcome[-length(outcome)]),
                                                                  prev_series_id = c(NA, series_id[-length(series_id)]))


pm <- left_join(pm, pmp_bc)

pm$time_dif <- (pm$start_time - pm$prev_start_time)/3600
pm$year <- year(as.POSIXct(pm$start_time,origin="1970-01-01"))
winrates <- pm %>% group_by(team,year) %>% summarise(n=length(outcome),winrate=sum(outcome=="won")/length(outcome))%>% filter(n>=30)

pm5 <- filter(pm, time_dif <8 & !is.na(team))
pm5$sequence <- str_c(pm5$prev_outcome, " then ", pm5$outcome)
pm5$same_seria <- ifelse(pm5$series_id == pm5$prev_series_id,1,0)


mids <- pm5 %>% group_by(match_id) %>% tally
mids <- mids$match_id[mids$n==2]


write.csv(pm5, "BA_THESIS_preprocessed.csv", row.names = F)

pm5 <- read.csv("BA_THESIS_preprocessed.csv")


#################################################

gold_adv <- read.csv("BA_THESES_gold_adv.csv")


gold_adv <- gold_adv %>% group_by(match_id) %>% mutate(adv_prev = c(0, adv[-length(adv)])) %>% 
  mutate(adv_rises=ifelse(adv>adv_prev,1,0))

gold_adv$match_id <- as.character(gold_adv$match_id)
gold_adv_summary <- gold_adv %>% ungroup %>% group_by(match_id) %>% summarise(max_adv = max(adv)/unique(duration), min_adv = min(adv)/unique(duration), 
                                                                              avd_rises_min = sum(adv_rises, na.rm=T),
                                                                              avd_falls_min = sum(adv_rises==F, na.rm=T),
                                                                              avd_rises_prop = sum(adv_rises, na.rm=T)/length(adv),
                                                                              adv_more_than_0 = sum(adv > 0, na.rm=T),
                                                                              adv_less_than_0 = sum(adv < 0, na.rm=T),
                                                                              adv_more_than_0_prop = sum(adv > 0, na.rm=T)/length(adv),
                                                                              duration=unique(duration))

names(gold_adv_summary)[1] <- "prev_match_id"

gold_adv_summary_dire <- data.frame(prev_match_id_dire= gold_adv_summary$prev_match_id, 
                                    max_adv_dire=-gold_adv_summary$min_adv,
                                    min_adv_dire=-gold_adv_summary$max_adv, 
                                    avd_rises_min_dire = gold_adv_summary$avd_falls_min,
                                    avd_falls_min_dire = gold_adv_summary$avd_rises_min,
                                    avd_rises_prop_dire = 1-gold_adv_summary$avd_rises_prop,
                                    adv_more_than_0_dire = gold_adv_summary$adv_less_than_0,
                                    adv_less_than_0_dire = gold_adv_summary$adv_more_than_0,
                                    adv_more_than_0_prop_dire = 1- gold_adv_summary$adv_more_than_0_prop,
                                    duration_dire = gold_adv_summary$duration)


#########################################


## WINNER ADVANTAGE


same_seria <- filter(pm5, same_seria==1 & match_id %in% mids) %>% group_by(match_id) %>% 
  arrange(desc(match_id, outcome)) %>% mutate(mmr_lost = mmr[1], aggr_lost=aggr[1], aggr_won=aggr[2],
                                              kills_lost=kills[1], kills_won=kills[2]) %>%
  filter(outcome=="won") %>% select(match_id , radiant_win, mmr, mmr_lost, prev_match_id, 
                                  prev_adv, prev_outcome, year, adv, aggr_won, aggr_lost,kills_won,kills_lost)

same_seria$mmr_winner_adv <- same_seria$mmr - same_seria$mmr_lost
same_seria$aggr_more_winner <- same_seria$aggr_won - same_seria$aggr_lost
same_seria$kills_more_winner <- same_seria$kills_won - same_seria$kills_lost


same_seria$winner_rises_rad <- gold_adv_summary$adv_more_than_0_prop[match(same_seria$prev_match_id,gold_adv_summary$prev_match_id )]
same_seria$winner_rises_dire <- gold_adv_summary_dire$adv_more_than_0_prop[match(same_seria$prev_match_id,gold_adv_summary_dire$prev_match_id )]
same_seria$winner_rises <- ifelse(same_seria$radiant_win, same_seria$winner_rises_rad,same_seria$winner_rises_dire)
#same_seria <- na.omit(same_seria)

same_seria$prev_outcome<- as.factor(same_seria$prev_outcome)
same_seria <- filter(same_seria, !is.infinite(aggr_more_winner))

not_same_seria <-  filter(pm5, same_seria==0 & match_id %in% mids) %>% group_by(match_id) %>% 
  arrange(desc(match_id, outcome)) %>% mutate(mmr_lost = mmr[1], aggr_lost=aggr[1], aggr_won=aggr[2],
                                              kills_lost=kills[1], kills_won=kills[2], 
                                              prev_match_id_lost = prev_match_id[2],
                                      prev_adv_lost=prev_adv[1], 
                                      prev_outcome_lost= prev_outcome[1]) %>% filter(outcome=="won") %>%
  select(match_id , radiant_win, mmr, mmr_lost, prev_match_id, prev_adv, prev_outcome,
         prev_match_id_lost, prev_adv_lost, prev_outcome_lost, year, adv,aggr_lost,aggr_won, kills_lost, kills_won)




not_same_seria$winner_rises_rad <- gold_adv_summary$adv_more_than_0_prop[match(same_seria$prev_match_id,gold_adv_summary$prev_match_id )]
not_same_seria$winner_rises_dire <- gold_adv_summary_dire$adv_more_than_0_prop[match(same_seria$prev_match_id,gold_adv_summary_dire$prev_match_id )]
not_same_seria$winner_rises <- ifelse(same_seria$radiant_win, same_seria$winner_rises_rad,same_seria$winner_rises_dire)
same_seria <- na.omit(same_seria)

not_same_seria$mmr_winner_adv <- not_same_seria$mmr - not_same_seria$mmr_lost
not_same_seria$aggr_more_winner <- not_same_seria$aggr_won - not_same_seria$aggr_lost
not_same_seria$kills_more_winner <- not_same_seria$kills_won - not_same_seria$kills_lost







not_same_seria$radiant_win<- as.factor(not_same_seria$radiant_win)
not_same_seria$prev_outcome<- as.factor(not_same_seria$prev_outcome)

not_same_seria$prev_outcome_dire<- as.factor(not_same_seria$prev_outcome_dire)

not_same_seria$aggr_dif <- not_same_seria$aggr_rad-not_same_seria$aggr_dire
not_same_seria <- filter(not_same_seria, !is.infinite(aggr_dif))


######

same_seria <- filter(pm5, same_seria==1 & match_id %in% mids) %>% group_by(match_id) %>% 
 arrange(desc(isRadiant)) %>% mutate(mmr_dire = mmr[2], aggr_rad=aggr[1], aggr_dire=aggr[2], kills_rad=kills[1],
                                     kills_dire=kills[2]) %>%
 filter(isRadiant==1) %>% select(match_id , radiant_win, mmr, mmr_dire, prev_match_id, 
                                 prev_adv, prev_outcome, year, adv, aggr_rad, aggr_dire,
                                 kills_rad, kills_dire)


same_seria$rad_mmr_adv <- same_seria$mmr - same_seria$mmr_dire



same_seria$prev_adv_abs <- abs(same_seria$prev_adv)
same_seria$rad_kill_adv <- same_seria$kills_rad - same_seria$kills_dire
same_seria$rad_aggr_adv <- same_seria$aggr_rad - same_seria$aggr_dire

same_seria <- na.omit(same_seria)

same_seria$radiant_win <- as.factor(same_seria$radiant_win)

same_seria <- filter(same_seria, !is.infinite(rad_aggr_adv))
same_seria$prev_match_id<- as.character(same_seria$prev_match_id)
same_seria <- left_join(same_seria, gold_adv_summary, by="prev_match_id")
same_seria$prev_outcome<- as.factor(same_seria$prev_outcome)


not_same_seria <-  filter(pm5, same_seria==0 & match_id %in% mids) %>% group_by(match_id) %>% 
  arrange(desc(isRadiant)) %>% mutate(mmr_dire = mmr[2], prev_match_id_dire = prev_match_id[2],
                                      prev_adv_dire=prev_adv[2], 
                                      prev_outcome_dire= prev_outcome[2],
                                      aggr_rad=aggr[1], aggr_dire=aggr[2],
                                      kills_rad=kills[1],
                                      kills_dire=kills[2]) %>% filter(isRadiant==1) %>%
  select(match_id , radiant_win, mmr, mmr_dire, prev_match_id, prev_adv, prev_outcome,
         prev_match_id_dire, prev_adv_dire, prev_outcome_dire, year, adv,aggr_rad, aggr_dire,kills_rad,kills_dire)



not_same_seria$rad_mmr_adv <- not_same_seria$mmr - not_same_seria$mmr_dire
not_same_seria$rad_kill_adv <- not_same_seria$kills_rad - not_same_seria$kills_dire
not_same_seria$rad_aggr_adv <- not_same_seria$aggr_rad - not_same_seria$aggr_dire

not_same_seria <- na.omit(not_same_seria)

not_same_seria$radiant_win <- as.factor(not_same_seria$radiant_win)

not_same_seria <- filter(not_same_seria, !is.infinite(rad_aggr_adv))
not_same_seria$prev_match_id<- as.character(not_same_seria$prev_match_id)
not_same_seria$prev_match_id_dire<- as.factor(not_same_seria$prev_match_id_dire)


not_same_seria <- left_join(not_same_seria, gold_adv_summary, by="prev_match_id") %>% left_join(gold_adv_summary_dire, by="prev_match_id_dire")



for(i in c("prev_adv", "max_adv" ,"min_adv", "avd_rises_min",  "avd_falls_min" ,"avd_rises_prop", "adv_more_than_0",
           "adv_less_than_0", "adv_more_than_0_prop","duration")){
  not_same_seria[[str_c(i,"_dif")]] <- not_same_seria[[i]]-not_same_seria[[str_c(i,"_dire")]]
}


########################

same_seria <- select(same_seria, year, rad_mmr_adv, prev_adv, radiant_win, prev_outcome,prev_match_id) %>% na.omit


not_same_seria <- select(not_same_seria, radiant_win, prev_adv_dif, rad_mmr_adv, rad_kill_adv, rad_aggr_adv, adv_more_than_0_prop_dif)
for(i in 3:6){
  not_same_seria[,i] <- scale(not_same_seria[,i])
}
for(i in c(3:4,8)){
  same_seria[,i] <- scale(same_seria[,i])
}

##CLUSTERS 

m_gold <- data.frame()

for(i in unique(gold_adv$match_id)[1461:5118]){
  print(which(unique(gold_adv$match_id)==i))
  x <- filter(gold_adv, match_id==i) 
  x <-sapply(seq(0,100,5), FUN=ts_percs, vec=x$adv_reversed)
  m_gold <- rbind(m_gold, data.frame(match_id=i, perc=seq(0,100,5), adv=x))
}  

m_gold_x <- m_gold%>% group_by(match_id) %>% mutate(adv=scale(adv))

m_gold2 <- acast(match_id~perc, data=m_gold_x, value.var = "adv")
m_gold2 <- acast(match_id~perc, data=m_gold, value.var = "adv")


d <- dist(m_gold2, method = "euclidean") 
H.fit <- hclust(d, method="ward")

plot(H.fit) 
no_cl <- 
rect.hclust(H.fit, k=no_cl, border="red") 

groups <- cutree(H.fit, k=4) 
m_gold$cl <- groups[match(m_gold$match_id, rownames(m_gold2))]


png("gold_cl.png", res=300, height =1000, width=1000)
ggplot(m_gold, aes(perc, adv))+ geom_line(alpha=0.01, aes(group=match_id)) + 
  geom_smooth(se=F, color= "#2B3C8F") + facet_wrap(~cl) + geom_hline(yintercept = 0) +
  theme(text= element_text(size=12), panel.grid.major = element_line(colour="lightgrey",lineend="round"),
          panel.grid.minor = element_line(colour="lightgrey",lineend="butt"),
          panel.background = element_rect(fill="white"), legend.position="None") 
dev.off()