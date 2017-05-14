library(rjson)
library(dplyr)
library(tidyr)
library(purrr)

games <- data.frame()
sqlQuery <- paste("select  * from public_matches where start_time > 1483228800 AND duration > 900 AND avg_mmr > 5000 AND num_mmr > 9 limit 5000",
                  ";", sep = "")

gameList <- fromJSON(file = paste("https://api.opendota.com/api/explorer?sql=", URLencode(sqlQuery), sep=""))
gameListDF <- gameList$rows %>% lapply(as.data.frame) %>% plyr::rbind.fill()
games <- rbind(games, gameListDF)
write.csv(games, "LINEUP_matches_desc.csv")



matches <- list()
games_ids <- unique(games$match_id)

for(i in games_ids[491:length(games_ids)]){
  if(which(games_ids==i)%%100==0){
    print(which(games_ids==i))
    saveRDS(matches, "LINEUP_matches.RDS")
  }
  readJSON <- tryCatch(
    fromJSON(file=paste("https://api.opendota.com/api/matches/", i, sep = "")),
    error = function (e) {"error"}
  )
  if(!is.null(readJSON$players[[2]]$is_roaming)){
    print("yeah!")
  }
  matches[[length(matches)+1]] <- readJSON
  Sys.sleep(1)
  
}
saveRDS(matches, "LINEUP_matches.RDS")
matches <- readRDS( "LINEUP_matches.RDS")

#########################

matches_df <- data.frame()

for(i in 1:length(matches)){
  mmr <- unlist(map(matches[[i]]$players,"solo_competitive_rank"))
  lane <- unlist(map(matches[[i]]$players,"lane_role"))
  if(is.null(lane)){lane <- NA}
  gpm <- unlist(map(matches[[i]]$players,"gold_per_min"))
  roam <- unlist(map(matches[[i]]$players,"is_roaming"))
  if(is.null(roam)){roam <- NA}
  outcome <- rep(c(matches[[i]]$radiant_win, !matches[[i]]$radiant_win), each=5)
  hero <- unlist(map(matches[[i]]$players,"hero_id"))
  player <- unlist(map(matches[[i]]$players,"account_id"))
  match_id <- rep(matches[[i]]$match_id,times=10)
  side <- rep(c("radiant","dire"),each=5)
  
  matches_df <- rbind(matches_df, data.frame(match_id, side, outcome, player, hero,
                                             mmr, lane, roam, gpm))
}

write.csv(matches_df, "LINEUP_matches_desc_ext.csv")

mdf <- filter(matches_df, !match_id %in% c(2935706108,2936244900))
mdf<- na.omit(mdf)
mdf$player_id <- str_c(mdf$match_id, "_", mdf$player)
mdf$id <- str_c(mdf$match_id,mdf$side)

mdf_mids <- mdf %>% group_by(id) %>% arrange(desc(gpm)) %>% filter(lane==2) %>% top_n(1,gpm)
mdf_mids$pos <- 2

mdf <- filter(mdf, !player_id %in% mdf_mids$player_id) %>% 
  group_by(id) %>% arrange(desc(gpm)) %>% 
  mutate(pos=(1:(length(id)+1))[-2])
mdf <- rbind(mdf, mdf_mids)

mdf <- filter(mdf, !match_id %in% c(2989980602, 3037515709))

vcd::assoc(pos ~ roam, data=mdf, shade=T)

mdf2 <- reshape2::dcast(mdf, id ~ pos, value.var = "mmr")
names(mdf2)[2:6] <- c("pos1", "pos2", "pos3", "pos4", "pos5")
for(i in 2:6){
  mdf2[,i] <- as.numeric(mdf2[,i])
}
mdf2$match_id <- mdf$match_id[match(mdf2$id, mdf$id)]
mdf2$outcome <- mdf$outcome[match(mdf2$id, mdf$id)]

mdf2 <- mdf2 %>% group_by(match_id) %>% summarise(dif_pos1 = pos1[2] - pos1[1],
                                          dif_pos2 = pos2[2] - pos2[1],
                                          dif_pos3 = pos3[2] - pos3[1],
                                          dif_pos4 = pos4[2] - pos4[1],
                                          dif_pos5 = pos5[2] - pos5[1],
                                          carry_adv = pos1[2] - pos3[1],
                                          off_adv = pos3[2] - pos1[1],
                                          won = ifelse(outcome[2],"won","lost"))

mdf2$won <- as.factor(mdf2$won)

fit <- glm(won ~ .-match_id-carry_adv-off_adv,data=mdf2, family = "binomial")
summary(fit)
car::vif(fit)

fit <- glm(won ~ off_adv + carry_adv,data=mdf2, family = "binomial")
summary(fit)

df_test <- data.frame(dif_pos1 = rep(0, times=41),
                      dif_pos2 = seq(-2000,2000,100),
                      dif_pos3 = rep(0, times=41),
                      dif_pos4 = rep(0, times=41),
                      dif_pos5 = rep(0, times=41),
                      off_adv = rep(0, times=41),
                      carry_adv = rep(0, times=41),
                      match_id= rep(0, times=41))


df_test$pred <- predict(fit, df_test,type="response")

df_test_plot <- data.frame(dif=df_test$dif_pos2, pred=df_test$pred, pos=rep("mid",times=41))

df_test <- data.frame(dif_pos1 = rep(0, times=41),
                      dif_pos3 = seq(-2000,2000,100),
                      dif_pos2 = rep(0, times=41),
                      dif_pos4 = rep(0, times=41),
                      dif_pos5 = rep(0, times=41),
                      off_adv = rep(0, times=41),
                      carry_adv = rep(0, times=41),
                      match_id= rep(0, times=41))


df_test$pred <- predict(fit, df_test,type="response")
df_test_plot <- rbind(df_test_plot,
                      data.frame(dif=df_test$dif_pos3, pred=df_test$pred, pos=rep("off",times=41)))


df_test <- data.frame(dif_pos3 = rep(0, times=41),
                      dif_pos1 = seq(-2000,2000,100),
                      dif_pos2 = rep(0, times=41),
                      dif_pos4 = rep(0, times=41),
                      dif_pos5 = rep(0, times=41),
                      off_adv = rep(0, times=41),
                      carry_adv = rep(0, times=41),
                      match_id= rep(0, times=41))


df_test$pred <- predict(fit, df_test,type="response")
df_test_plot <- rbind(df_test_plot,
                      data.frame(dif=df_test$dif_pos1, pred=df_test$pred, pos=rep("carry",times=41)))



df_test <- data.frame(dif_pos1 = rep(0, times=41),
                      dif_pos4 = seq(-2000,2000,100),
                      dif_pos2 = rep(0, times=41),
                      dif_pos3 = rep(0, times=41),
                      dif_pos5 = rep(0, times=41),
                      off_adv = rep(0, times=41),
                      carry_adv = rep(0, times=41),
                      match_id= rep(0, times=41))


df_test$pred <- predict(fit, df_test,type="response")
df_test_plot <- rbind(df_test_plot,
                      data.frame(dif=df_test$dif_pos4, pred=df_test$pred, pos=rep("semi-sup",times=41)))



df_test <- data.frame(dif_pos3 = rep(0, times=41),
                      dif_pos5 = seq(-2000,2000,100),
                      dif_pos2 = rep(0, times=41),
                      dif_pos4 = rep(0, times=41),
                      dif_pos1 = rep(0, times=41),
                      off_adv = rep(0, times=41),
                      carry_adv = rep(0, times=41),
                      match_id= rep(0, times=41))


df_test$pred <- predict(fit, df_test,type="response")
df_test_plot <- rbind(df_test_plot,
                      data.frame(dif=df_test$dif_pos5, pred=df_test$pred, pos=rep("sup",times=41)))

ggplot(df_test_plot, aes(dif, pred,color=pos)) +geom_line(size=2) + scale_colour_brewer(palette = "Set2") +
  xlab("MMR Difference with the Same Position in an Enemy Team") +
  ylab("Probability of Victory")
