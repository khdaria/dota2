library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(rvest)


leag <- fromJSON("https://api.opendota.com/api/leagues")
leag <- filter(leag, tier=="premium")

matches <- fromJSON("https://api.opendota.com/api/proMatches")
for (i in 1:300){
  print(i)
  x <- fromJSON(str_c("https://api.opendota.com/api/proMatches?less_than_match_id=",matches$match_id[nrow(matches)], collapse=""))
  matches <- rbind(matches, x)
  Sys.sleep(1)
}

write.csv(matches, "BA_THESIS_list_of_pro_matches.csv", row.names = F)

ids <- unique(matches$match_id)
post_match_players <- data.frame()
gold_adv <- data.frame()
error_list <-c()

for (MATCH_ID in ids) {
  t1 <- Sys.time()
  print(which(ids ==MATCH_ID))
  
  readJSON <- tryCatch(
    rjson::fromJSON(file = paste("https://api.opendota.com/api/matches/", MATCH_ID, sep = "")),
    error = function (e) {"error"}
  )
  
  if(length(readJSON)>1){
    
    all_matches[[length(all_matches)+1]] <- readJSON
    if(length(readJSON$players)==10){
      
      one_match <- data.frame()
      if(!is.null(readJSON$players[[3]]$account_id)){
       for(j in 1:10){
          
          one_player <- data.frame(account_id = readJSON$players[[j]]$account_id)
          
          one_player$hero_id <- readJSON$players[[j]]$hero_id
          one_player$isRadiant <- readJSON$players[[j]]$isRadiant
          one_player$pings <- readJSON$players[[j]]$pings
          one_player$solo_competitive_rank <- readJSON$players[[j]]$solo_competitive_rank
          
          one_player$assists <- readJSON$players[[j]]$assists 
          one_player$kda <- readJSON$players[[j]]$kda
          one_player$kills <- readJSON$players[[j]]$kills
          one_player$deaths <- readJSON$players[[j]]$deaths
          one_player$total_xp <- readJSON$players[[j]]$total_xp
          one_player$total_gold <- readJSON$players[[j]]$total_gold
          one_player$lane <- readJSON$players[[j]]$lane
          one_player$lane_role <- readJSON$players[[j]]$lane_role
          one_player$lane_efficiency <- readJSON$players[[j]]$lane_efficiency
          
          one_player$obs_placed <- readJSON$players[[j]]$obs_placed
          one_player$observer_kills <- readJSON$players[[j]]$observer_kills
          one_player$sen_placed <- readJSON$players[[j]]$sen_placed
          one_player$sentry_kills <- readJSON$players[[j]]$sentry_kills
          one_player$hero_healing <- readJSON$players[[j]]$hero_healing
          one_player$hero_damage <- readJSON$players[[j]]$hero_damage
          one_player$camps_stacked <- readJSON$players[[j]]$camps_stacked
          
          
          
          if(!is.null(readJSON$players[[j]]$benchmarks$gold_per_min[[2]])){
            ns <- rep(names(readJSON$players[[j]]$benchmarks),each=2)
            ns[c(F,T)] <- str_c(ns[c(F,T)],"_pct")
            benchmarks <- as.numeric(unlist(readJSON$players[[j]]$benchmarks )) %>% t %>% as.data.frame()
            if(length(ns)== length(benchmarks)){
              names(benchmarks) <- ns
              one_player <- cbind(one_player, benchmarks) %>% as.data.frame()
            }else{
              benchmarks <- as.data.frame(t(unlist(readJSON$players[[j]]$benchmarks) ))
              names(benchmarks) <- str_replace_all(names(benchmarks),"\\.","_")
              one_player <- cbind(one_player, benchmarks) %>% as.data.frame()
            }
            
          }
          one_match <- plyr::rbind.fill(one_match, one_player)
        }
        
        
      
      one_match$match_id <- readJSON$match_id
      one_match$series_type <- readJSON$series_type
      one_match$series_id <- readJSON$series_id
      one_match$start_time <- readJSON$start_time
      one_match$radiant_win <- readJSON$radiant_win
      one_match$team <- NA
      if(!is.null(readJSON$radiant_team$team_id)){
        one_match$team[one_match$isRadiant==TRUE] <- readJSON$radiant_team$team_id
        if(!is.null(readJSON$dire_team$team_id)){
          one_match$team[one_match$isRadiant==F] <- readJSON$dire_team$team_id
        }
      }
      
      
      post_match_players <- plyr::rbind.fill(post_match_players, one_match)
      
      if(!is.null(readJSON$radiant_gold_adv)&length(readJSON$radiant_gold_adv)>0){
        x <- data.frame(adv =readJSON$radiant_gold_adv)
        if(length(readJSON$radiant_win)==1){
          if(readJSON$radiant_win){
            x$outcome=1
          }else{x$outcome=0}
          x$duration <- readJSON$duration
          x$match_id <- readJSON$match_id
          gold_adv <- rbind(gold_adv,x)
          
        }
        }
      }
      
    }
    
    
  }else{
    error_list <- c(error_list, MATCH_ID)
  }
  
  
  
  t2 <- Sys.time()
  if(t2-t1<1){
    Sys.sleep(1-(t2-t1))
  }

}

gold_adv <- distinct(gold_adv)
gold_adv$duration_min <- gold_adv$duration/60
gold_adv <- gold_adv %>% group_by(match_id) %>% mutate(min=(1:length(adv)))
gold_adv$adv_reversed <- ifelse(gold_adv$outcome==0, -gold_adv$adv, gold_adv$adv)

write.csv(post_match_players, "BA_THESIS_post_match_players.csv",row.names = F)
write.csv(gold_adv, "BA_THESES_gold_adv.csv",row.names = F)

mids <- unique(post_match_players$match_id)

page <- html("http://wiki.teamliquid.net/dota2/Leaderboards")
mmr_tab <- page %>% html_node(xpath ="//table") %>% html_table() 
mmr_tab <- mmr_tab[,c(2,4,5)]
mmr_tab$`Solo MMR` <- extract_numeric(mmr_tab$`Solo MMR`)
mmr_tab$link <- page %>% html_nodes(xpath ="//table//td[2]//a") %>% html_attr("href")
mmr_tab$account_id <- NA

for(i in 1:nrow(mmr_tab)){
  page <- html(str_c("http://wiki.teamliquid.net", mmr_tab$link[i]))
  lks <- page %>% html_nodes(xpath = "//a[@rel='nofollow noopener']") %>% html_attr("href") 
  lks <- lks[str_detect(lks,"dotabuff")&str_detect(lks,"player")] %>% str_sub(start = -15) %>% extract_numeric()
  if(length(lks)!=0) {mmr_tab$account_id[i] <- lks[length(lks)]}
}


write.csv(mmr_tab, "BA_THESIS_players_mmr.csv")

