library(tidyverse)
library(datavolley)
library(classInt)
library(cluster)
library(factoextra)
library(gridExtra)

## search through digs to find players not on the court (bros),
## loop back through to after to see how many sets they played, divide directly

'%!in%' <- function(x,y)!('%in%'(x,y))
start.time <- Sys.time()
dataFile = "~/Users/eantal/practice/"
#setwd(dataFile)

# add all .dvw files to wd first - then my.files will be a list of files to parse
my.files <- list.files(pattern = "*dvw", recursive = TRUE)
all_files <- as.data.frame(matrix(ncol=0,nrow=0))

#run read_dv code for all files in my.files - rbind them into "all_files"
for(q in 1:1500) {
  x <- read_dv(my.files[q], skill_evaluation_decode = "volleymetrics")
  x$plays$match_date <- x$meta$match$date
  all_files <- rbind(all_files, x$plays)
  print(q)
}

#rename total to p_master and remove non-plays
p_master <- subset(all_files, substring(code, 2, 2)!="P" & substring(code, 2, 2)!="p" 
                 & substring(code, 2, 2)!="z" & substring(code, 2, 2)!="c"
                 & substring(code, 2, 2)!="*" & substring(code, 2, 2)!="$")
#p_master <- p_master[,c(1,2,4,6,8,10,11,12,13,14,19,20,21,23,24,54,55,56,57,58,59,72,73,74,75,79,80,81,82,83)]
all_files <- NULL

#create touch_id for all rows - helps w/ ability to reorder properly if I screw something up down the road
p_master$id_p_master <- seq.int(nrow(p_master))
p_master <- p_master[order(p_master$id_p_master),] 

#create column for who the team making the contact is playing against
p_master$opponent <- NA
p_master$opponent <- ifelse(p_master$team == p_master$home_team, p_master$visiting_team, p_master$opponent)
p_master$opponent <- ifelse(p_master$team == p_master$visiting_team, p_master$home_team, p_master$opponent)

#create column as a count (for the denomenator for most equations)
p_master$count <- 1

#create cover as one of the skills
p_master$add_cover <- ifelse(p_master$skill=="Dig" & lag(p_master$skill, 2)=="Attack" & 
                             lag(p_master$skill, 1)=="Block" & lag(p_master$team, 2)==p_master$team, "Cover", p_master$skill)

#create skill-quality combination
p_master$skq <- paste(p_master$add_cover, p_master$evaluation_code)

#create Forward/Backward looking contacts that help power Input/Output Contacts
p_master$two_touch_ago <- ifelse(p_master$point_id == lag(p_master$point_id, 2), lag(p_master$skq, 2), NA)
p_master$one_touch_ago <- ifelse(p_master$point_id == lag(p_master$point_id, 1), lag(p_master$skq, 1), NA)
p_master$one_touch_future <- ifelse(p_master$point_id == lead(p_master$point_id, 1), lead(p_master$skq, 1), NA)
p_master$two_touch_future <- ifelse(p_master$point_id == lead(p_master$point_id, 2), lead(p_master$skq, 2), NA)
p_master$three_touch_future <- ifelse(p_master$point_id == lead(p_master$point_id, 3), lead(p_master$skq, 3), NA)

#create possession data
p_master$possession <- NA
p_master$possession <- ifelse(p_master$skill=="Serve", 1, p_master$possession)
p_master$possession <- ifelse(p_master$skill=="Reception", 2, p_master$possession)
x <- 0
repeat {
  x = x+1
  p_master$possession <- ifelse(p_master$skill!="Serve" & p_master$skill!="Reception" & lag(p_master$team,1)==p_master$team, lag(p_master$possession), p_master$possession)
  p_master$possession <- ifelse(p_master$skill!="Serve" & p_master$skill!="Reception" & lag(p_master$team,1)!=p_master$team, lag(p_master$possession) + 1, p_master$possession)
  if (x == 50){
    break
  }
}

#create rally - possession combination
p_master$rally_possession <- paste0(p_master$point_id, "-", p_master$possession)

#create contact count w/in each possession
p_master$contact <- NA
p_master$contact <- ifelse(p_master$skill=="Serve" | p_master$skill=="Reception", 1, p_master$contact)
x <- 0
repeat {
  x = x+1
  p_master$contact <- ifelse(p_master$skill!="Serve" & p_master$skill!="Reception" & lag(p_master$possession,1)==p_master$possession, lag(p_master$contact) + 1, p_master$contact)
  p_master$contact <- ifelse(p_master$skill!="Serve" & p_master$skill!="Reception" & lag(p_master$possession,1)!=p_master$possession, 1, p_master$contact)
  if (x == 10){
    break
  }
}
p_master$contact <- ifelse(p_master$contact == 5, 1, p_master$contact)
p_master$contact <- ifelse(p_master$contact == 6, 2, p_master$contact)
p_master$contact <- ifelse(p_master$contact == 7, 3, p_master$contact)


#create column to denote if there is a winning or losing contact & fill in other touches within same possession if poss is won/lost
winners <- c("Serve #", "Attack #", "Block #")
losers <- c("Serve =", "Reception =", "Set =", "Attack =", "Attack /", "Block =", "Dig =", "Freeball =", "Cover =")
p_master$possession_winner <- ifelse(p_master$skq %in% winners, 1, 0)
p_master$possession_winner <- ifelse(lead(p_master$skq,1) %in% winners & p_master$rally_possession==lead(p_master$rally_possession, 1), 1, p_master$possession_winner)
p_master$possession_winner <- ifelse(lead(p_master$skq,2) %in% winners & p_master$rally_possession==lead(p_master$rally_possession, 2), 1, p_master$possession_winner)
p_master$possession_winner <- ifelse(lead(p_master$skq,3) %in% winners & p_master$rally_possession==lead(p_master$rally_possession, 3), 1, p_master$possession_winner)

p_master$possession_loser <- ifelse(p_master$skq %in% losers, 1, 0)
p_master$possession_loser <- ifelse(lead(p_master$skq,1) %in% losers & p_master$rally_possession==lead(p_master$rally_possession, 1), 1, p_master$possession_loser)
p_master$possession_loser <- ifelse(lead(p_master$skq,2) %in% losers & p_master$rally_possession==lead(p_master$rally_possession, 2), 1, p_master$possession_loser)
p_master$possession_loser <- ifelse(lead(p_master$skq,3) %in% losers & p_master$rally_possession==lead(p_master$rally_possession, 3), 1, p_master$possession_loser)

#create column for winning/losing on NEXT possession
p_master$poss_win_next <- 0
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,1) &
                                 p_master$possession==lead(p_master$possession,1)-1 & p_master$team!=lead(p_master$team,1) & lead(p_master$skq,1) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,2) &
                                 p_master$possession==lead(p_master$possession,2)-1 & p_master$team!=lead(p_master$team,2) & lead(p_master$skq,2) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,3) &
                                 p_master$possession==lead(p_master$possession,3)-1 & p_master$team!=lead(p_master$team,3) & lead(p_master$skq,3) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,4) &
                                 p_master$possession==lead(p_master$possession,4)-1 & p_master$team!=lead(p_master$team,4) & lead(p_master$skq,4) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,5) &
                                 p_master$possession==lead(p_master$possession,5)-1 & p_master$team!=lead(p_master$team,5) & lead(p_master$skq,5) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,6) &
                                 p_master$possession==lead(p_master$possession,6)-1 & p_master$team!=lead(p_master$team,6) & lead(p_master$skq,6) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,7) &
                                 p_master$possession==lead(p_master$possession,7)-1 & p_master$team!=lead(p_master$team,7) & lead(p_master$skq,7) %in% losers, 1, p_master$poss_win_next)
p_master$poss_win_next <- ifelse(p_master$point_id==lead(p_master$point_id,8) &
                                 p_master$possession==lead(p_master$possession,8)-1 & p_master$team!=lead(p_master$team,8) & lead(p_master$skq,8) %in% losers, 1, p_master$poss_win_next)

p_master$poss_lost_next <- 0
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,1) &
                                  p_master$possession==lead(p_master$possession,1)-1 & p_master$team!=lead(p_master$team,1) & lead(p_master$skq,1) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,2) &
                                  p_master$possession==lead(p_master$possession,2)-1 & p_master$team!=lead(p_master$team,2) & lead(p_master$skq,2) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,3) &
                                  p_master$possession==lead(p_master$possession,3)-1 & p_master$team!=lead(p_master$team,3) & lead(p_master$skq,3) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,4) &
                                  p_master$possession==lead(p_master$possession,4)-1 & p_master$team!=lead(p_master$team,4) & lead(p_master$skq,4) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,5) &
                                  p_master$possession==lead(p_master$possession,5)-1 & p_master$team!=lead(p_master$team,5) & lead(p_master$skq,5) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,6) &
                                  p_master$possession==lead(p_master$possession,6)-1 & p_master$team!=lead(p_master$team,6) & lead(p_master$skq,6) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,7) &
                                  p_master$possession==lead(p_master$possession,7)-1 & p_master$team!=lead(p_master$team,7) & lead(p_master$skq,7) %in% winners, 1, p_master$poss_lost_next)
p_master$poss_lost_next <- ifelse(p_master$point_id==lead(p_master$point_id,8) &
                                  p_master$possession==lead(p_master$possession,8)-1 & p_master$team!=lead(p_master$team,8) & lead(p_master$skq,8) %in% winners, 1, p_master$poss_lost_next)

#create column for winning/losing on NEXT NEXT possession
p_master$poss_win_nextnext <- 0
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,1) &
                                     p_master$possession==lead(p_master$possession,1)-2 & p_master$team==lead(p_master$team,1) & lead(p_master$skq,1) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,2) &
                                     p_master$possession==lead(p_master$possession,2)-2 & p_master$team==lead(p_master$team,2) & lead(p_master$skq,2) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,3) &
                                     p_master$possession==lead(p_master$possession,3)-2 & p_master$team==lead(p_master$team,3) & lead(p_master$skq,3) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,4) &
                                     p_master$possession==lead(p_master$possession,4)-2 & p_master$team==lead(p_master$team,4) & lead(p_master$skq,4) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,5) &
                                     p_master$possession==lead(p_master$possession,5)-2 & p_master$team==lead(p_master$team,5) & lead(p_master$skq,5) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,6) &
                                     p_master$possession==lead(p_master$possession,6)-2 & p_master$team==lead(p_master$team,6) & lead(p_master$skq,6) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,7) &
                                     p_master$possession==lead(p_master$possession,7)-2 & p_master$team==lead(p_master$team,7) & lead(p_master$skq,7) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,8) &
                                     p_master$possession==lead(p_master$possession,8)-2 & p_master$team==lead(p_master$team,8) & lead(p_master$skq,8) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,9) &
                                     p_master$possession==lead(p_master$possession,9)-2 & p_master$team==lead(p_master$team,9) & lead(p_master$skq,9) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,10) &
                                     p_master$possession==lead(p_master$possession,10)-2 & p_master$team==lead(p_master$team,10) & lead(p_master$skq,10) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,11) &
                                     p_master$possession==lead(p_master$possession,11)-2 & p_master$team==lead(p_master$team,11) & lead(p_master$skq,11) %in% winners, 1, p_master$poss_win_nextnext)
p_master$poss_win_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,12) &
                                     p_master$possession==lead(p_master$possession,12)-2 & p_master$team==lead(p_master$team,12) & lead(p_master$skq,12) %in% winners, 1, p_master$poss_win_nextnext)

p_master$poss_lost_nextnext <- 0
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,1) &
                                      p_master$possession==lead(p_master$possession,1)-2 & p_master$team==lead(p_master$team,1) & lead(p_master$skq,1) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,2) &
                                      p_master$possession==lead(p_master$possession,2)-2 & p_master$team==lead(p_master$team,2) & lead(p_master$skq,2) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,3) &
                                      p_master$possession==lead(p_master$possession,3)-2 & p_master$team==lead(p_master$team,3) & lead(p_master$skq,3) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,4) &
                                      p_master$possession==lead(p_master$possession,4)-2 & p_master$team==lead(p_master$team,4) & lead(p_master$skq,4) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,5) &
                                      p_master$possession==lead(p_master$possession,5)-2 & p_master$team==lead(p_master$team,5) & lead(p_master$skq,5) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,6) &
                                      p_master$possession==lead(p_master$possession,6)-2 & p_master$team==lead(p_master$team,6) & lead(p_master$skq,6) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,7) &
                                      p_master$possession==lead(p_master$possession,7)-2 & p_master$team==lead(p_master$team,7) & lead(p_master$skq,7) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,8) &
                                      p_master$possession==lead(p_master$possession,8)-2 & p_master$team==lead(p_master$team,8) & lead(p_master$skq,8) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,9) &
                                      p_master$possession==lead(p_master$possession,9)-2 & p_master$team==lead(p_master$team,9) & lead(p_master$skq,9) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,10) &
                                      p_master$possession==lead(p_master$possession,10)-2 & p_master$team==lead(p_master$team,10) & lead(p_master$skq,10) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,11) &
                                      p_master$possession==lead(p_master$possession,11)-2 & p_master$team==lead(p_master$team,11) & lead(p_master$skq,11) %in% losers, 1, p_master$poss_lost_nextnext)
p_master$poss_lost_nextnext <- ifelse(p_master$point_id==lead(p_master$point_id,12) &
                                      p_master$possession==lead(p_master$possession,12)-2 & p_master$team==lead(p_master$team,12) & lead(p_master$skq,12) %in% losers, 1, p_master$poss_lost_nextnext)

#create efficiency w/in possession
p_master$poss_eff <- p_master$possession_winner-p_master$possession_loser
p_master$poss_eff_next <- p_master$poss_win_next-p_master$poss_lost_next
p_master$poss_eff_nextnext <- p_master$poss_win_nextnext-p_master$poss_lost_nextnext

#create rally winner & rally loser
p_master$rally_winner <- ifelse(p_master$team==p_master$point_won_by, 1, 0) 
p_master$rally_loser <- ifelse(p_master$team!=p_master$point_won_by, 1, 0)

#create efficiency w/in rally
p_master$rally_eff <- p_master$rally_winner

#create serving & home team RALLY efficiency
p_master$serveteam_rallyeff <- ifelse(p_master$serving_team==p_master$point_won_by, 1,0)
p_master$hometeam_rallyeff <- ifelse(p_master$home_team==p_master$point_won_by, 1, 0)

#create serving & home team POSSESSION efficiency
p_master$st_posswin <- NA
p_master$st_posslose <- NA
p_master$ht_posswin <- NA
p_master$ht_posslose <- NA

p_master$st_posswin <- ifelse(p_master$serving_team==p_master$team & p_master$possession_winner==1, 1, p_master$st_posswin)
p_master$ht_posswin <- ifelse(p_master$home_team==p_master$team & p_master$possession_winner==1, 1, p_master$ht_posswin)
p_master$st_posslose <- ifelse(p_master$serving_team==p_master$team & p_master$possession_loser==1, 1, p_master$st_posslose)
p_master$ht_posslose <- ifelse(p_master$home_team==p_master$team & p_master$possession_loser==1, 1, p_master$ht_posslose)

p_master$serveteam_posseff <- p_master$st_posswin-p_master$st_posslose
p_master$hometeam_posseff <- p_master$ht_posswin-p_master$ht_posslose

#is serving team touching the ball?
p_master$serveteam_touch <- ifelse(p_master$team==p_master$serving_team, "yes", "no")

#is home team touching the ball?
p_master$hometeamtouch <- ifelse(p_master$home_team==p_master$team, "yes", "no")

#create endzone+subzone for setting
p_master$endzone_full <- paste0(p_master$end_zone, p_master$end_subzone)

#create location where setter takes ball for R,D,Cover,Freeball codes
p_master$set_from <- NA
p_master$set_from <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Cover", "Freeball") & lead(p_master$team, 1)==p_master$team & substr(p_master$one_touch_future,1,3)=="Set", lead(p_master$end_zone,1), p_master$set_from)

#create what type of attack follows the first touch (attack code starts w/ V or not)
p_master$attack_type <- NA
p_master$attack_type <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Cover", "Freeball") & lead(p_master$team, 1)==p_master$team & lead(p_master$team,2)==p_master$team & lead(p_master$add_cover,1)=="Set", 
                             substr(lead(p_master$attack_code,2),1,1), p_master$attack_type)

p_master$in_or_oos <- NA
p_master$in_or_oos <- ifelse(p_master$attack_type=="V", "OOS", p_master$in_or_oos)
p_master$in_or_oos <- ifelse(p_master$attack_type %in% c("C", "X", "P"), "In Sys", p_master$in_or_oos)

p_master$attack_type <- NULL

#create unique set_id
p_master$set_id <- paste0(p_master$match_id, "- Set ", p_master$set_number)

#create if team won the set overall
p_master <- p_master[order(p_master$id_p_master),] 

p_master$who_won_set <- NA
p_master$who_lost_set <- NA
p_master$who_won_set <- ifelse(p_master$set_id!=lead(p_master$set_id,1) & p_master$team==p_master$point_won_by, p_master$team, p_master$who_won_set)
p_master$who_won_set <- ifelse(p_master$set_id!=lead(p_master$set_id,1) & p_master$team!=p_master$point_won_by, p_master$opponent, p_master$who_won_set)
p_master$who_lost_set <- ifelse(p_master$set_id!=lead(p_master$set_id,1) & p_master$team!=p_master$point_won_by, p_master$team, p_master$who_lost_set)
p_master$who_lost_set <- ifelse(p_master$set_id!=lead(p_master$set_id,1) & p_master$team==p_master$point_won_by, p_master$opponent, p_master$who_lost_set)

p_master$who_won_set <- ifelse(p_master$id_p_master==max(p_master$id_p_master) & p_master$team==p_master$point_won_by, p_master$team, p_master$who_won_set)
p_master$who_won_set <- ifelse(p_master$id_p_master==max(p_master$id_p_master) & p_master$team!=p_master$point_won_by, p_master$opponent, p_master$who_won_set)
p_master$who_lost_set <- ifelse(p_master$id_p_master==max(p_master$id_p_master) & p_master$team!=p_master$point_won_by, p_master$team, p_master$who_lost_set)
p_master$who_lost_set <- ifelse(p_master$id_p_master==max(p_master$id_p_master) & p_master$team==p_master$point_won_by, p_master$opponent, p_master$who_lost_set)

p_master <- fill(p_master, who_won_set, who_lost_set, .direction = c("up"))

p_master$teamwonset <- ifelse(p_master$team==p_master$who_won_set, 1, 0)
p_master$teamlostset <- ifelse(p_master$team==p_master$who_lost_set, 1, 0)

p_master$who_won_set <- NULL
p_master$who_lost_set <- NULL

p_master$passer_rating <- NA
p_master$passer_rating <- ifelse(p_master$skq=="Reception #", 3, p_master$passer_rating)
p_master$passer_rating <- ifelse(p_master$skq=="Reception +", 3, p_master$passer_rating)
p_master$passer_rating <- ifelse(p_master$skq=="Reception !", 2, p_master$passer_rating)
p_master$passer_rating <- ifelse(p_master$skq=="Reception -", 1, p_master$passer_rating)
p_master$passer_rating <- ifelse(p_master$skq=="Reception /", 1, p_master$passer_rating)
p_master$passer_rating <- ifelse(p_master$skq=="Reception =", 0, p_master$passer_rating)


# when working with lag/lead functions, always double check order of data
p_master <- p_master[order(p_master$id_p_master),] 

# create new function to read, if x IS NOT in group y...
'%!in%' <- function(x,y)!('%in%'(x,y))

# create 1st touch quality for input - these are built by looking at rally eff after each contact
# there was a pocket of positive touches, a cluster around 0, and a pocket of negative touches
pos_touch <- c("Reception #", "Reception +", "Dig #", "Freeball #", "Freeball +", "Cover #")
neutral_touch <- c("Dig +", "Dig !", "Freeball !", "Reception !", "Cover +", "Cover !")
neg_touch <- c("Dig -", "Freeball -", "Freeball /", "Reception -", "Reception /", "Cover -")

# build first touch quality - if contact is pos/neutral/neg, say so as well as the zone of the subsequent touch
p_master$first_touch_quality <- NA
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)==p_master$team & lead(p_master$skill,1)=="Set" &
                                       p_master$skq %in% pos_touch, paste0("Good ", p_master$add_cover, " into ", lead(p_master$end_zone,1)), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)==p_master$team & lead(p_master$skill,1)!="Set" &
                                       p_master$skq %in% pos_touch, paste0("Good ", p_master$add_cover, " into ", lead(p_master$start_zone,1)), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)==p_master$team & lead(p_master$skill,1)=="Set" &
                                       p_master$skq %in% neutral_touch, paste0("OK ", p_master$add_cover, " into ", lead(p_master$end_zone,1)), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)==p_master$team & lead(p_master$skill,1)!="Set" &
                                       p_master$skq %in% neutral_touch, paste0("OK ", p_master$add_cover, " into ", lead(p_master$start_zone,1)), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") &
                                       p_master$skq %in% neg_touch & !is.na(p_master$one_touch_future), paste0("Poor ", p_master$add_cover), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)==p_master$team & lead(p_master$skill,1)=="Set" &
                                       p_master$skq %in% neg_touch & !is.na(p_master$one_touch_future), paste0("Poor ", p_master$add_cover, " into ", lead(p_master$end_zone,1)), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)==p_master$team & lead(p_master$skill,1)!="Set" &
                                       p_master$skq %in% neg_touch & !is.na(p_master$one_touch_future), paste0("Poor ", p_master$add_cover, " into ", lead(p_master$start_zone,1)), p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)!=p_master$team & lead(p_master$skill,1)=="Attack" &
                                       p_master$skq %!in% c("Reception =", "Dig =", "Freeball =", "Cover ="), "Overpass into Attack", p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(p_master$team,1)!=p_master$team & lead(p_master$skill,1)!="Attack" &
                                       p_master$skq %!in% c("Reception =", "Dig =", "Freeball =", "Cover ="), "Overpass into Freeball", p_master$first_touch_quality)
p_master$first_touch_quality <- ifelse(p_master$skq %in% c("Reception =", "Dig =", "Freeball =", "Cover ="), paste0("Error on ", p_master$add_cover), p_master$first_touch_quality)


# create the 2nd touch situation labels
## we've worked backwards here, the output of setting must equal the input of attacking
p_master$second_touch_result <- NA
p_master$second_touch_result <- ifelse(p_master$skq=="Set #" & lead(p_master$skill=="Attack",1), paste0(lag(p_master$in_or_oos,1), " - ", lead(p_master$num_players)), p_master$second_touch_result)
p_master$second_touch_result <- ifelse(p_master$skq=="Set -", "Poor Set", p_master$second_touch_result)
p_master$second_touch_result <- ifelse(p_master$skq=="Set =", "Ball Handling Error", p_master$second_touch_result)

# create block touch situation labels - if a cover, add the "time" as well
p_master$block_touch_result <- NA
p_master$block_touch_result <- ifelse(p_master$skill == "Block" & lead(p_master$add_cover!="Cover"), lead(p_master$first_touch_quality,1), p_master$block_touch_result)
p_master$block_touch_result <- ifelse(p_master$skill == "Block" & lead(p_master$add_cover=="Cover"), paste0(lag(p_master$skill_subtype,1), " + ", ifelse(lead(p_master$video_time,1) - lag(p_master$video_time,1) < 2, lead(p_master$video_time,1) - lag(p_master$video_time,1),"2"), " sec to Cover"), p_master$block_touch_result)
p_master$block_touch_result <- ifelse(p_master$skq %in% c("Block =", "Block !"), "Block Tooled", p_master$block_touch_result)
p_master$block_touch_result <- ifelse(p_master$skq=="Block #", "Stuff Block", p_master$block_touch_result)

overpass <- c("Reception /", "Dig -", "Cover -", "Freeball /")

######## SIMPLIFY ############
p_master$input <- NA
p_master$output <- NA

#serve
p_master$input <- ifelse(p_master$skill=="Serve", "Pre-Serve", p_master$input)
p_master$output <- ifelse(p_master$skill=="Serve", lead(p_master$first_touch_quality,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Serve" & p_master$skq=="Serve #", "Service Ace", p_master$output)
p_master$output <- ifelse(p_master$skill=="Serve" & p_master$skq=="Serve =", "Service Error", p_master$output)

#reception
p_master$input <- ifelse(p_master$skill=="Reception", "Pre-Serve", p_master$input)
p_master$output <- ifelse(p_master$skill=="Reception", p_master$first_touch_quality, p_master$output)
p_master$output <- ifelse(p_master$skill=="Reception" & p_master$skq=="Reception =", "Reception Error", p_master$output)

#set
p_master$input <- ifelse(p_master$skill=="Set", lag(p_master$output,1), p_master$input)
p_master$output <- ifelse(p_master$skq=="Set #", p_master$second_touch_result, p_master$output)
p_master$output <- ifelse(p_master$skq=="Set -", "Poor Set", p_master$output)
p_master$output  <- ifelse(p_master$skq=="Set =", "Ball Handling Error", p_master$output)

#attack
p_master$input <- ifelse(p_master$skill=="Attack", lag(p_master$second_touch_result,1), p_master$input)
p_master$input <- ifelse(p_master$skill=="Attack" & p_master$one_touch_ago %in% overpass, "Overpass", p_master$input)
p_master$output <- ifelse(p_master$skill=="Attack" & lead(p_master$add_cover,1)=="Block", lead(p_master$block_touch_result,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & lead(p_master$add_cover,1)!="Block", lead(p_master$first_touch_quality,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & p_master$skq=="Attack #", "Kill", p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & p_master$skq=="Attack =", "Attack Error", p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & p_master$skq=="Attack /", "Attack Blocked", p_master$output)

#block
p_master$input <- ifelse(p_master$skill=="Block", lag(p_master$input,1), p_master$input)
p_master$output <- ifelse(p_master$skill=="Block", lead(p_master$first_touch_quality,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Block" & p_master$skq=="Block #", "Stuff Block", p_master$output)
p_master$output <- ifelse(p_master$skill=="Block" & (p_master$skq=="Block =" | p_master$skq=="Block !"), "Block Tooled", p_master$output)

#dig
p_master$input <- ifelse(p_master$add_cover=="Dig" & substr(lag(p_master$skq,1),1,5)!="Block", lag(p_master$input,1), p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & lag(p_master$skq,1)=="Block +", paste0("Block Slowdown + ", ifelse(p_master$video_time - lag(p_master$video_time,2) < 2, p_master$video_time - lag(p_master$video_time,2),"2"), " sec"), p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & (p_master$input=="Block Slowdown + 3 sec" | p_master$input=="Block Slowdown + 4 sec" | p_master$input=="Block Slowdown + 5 sec"), "Block Slowdown + 2 sec", p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & substr(lag(p_master$skq,1),1,5)=="Block" & lag(p_master$skq,1)!="Block +", "Unhelpful Block Touch", p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & lag(p_master$skq) %in% c("Block =", "Block !"), "Block Tooled", p_master$input)
p_master$output <- ifelse(p_master$add_cover=="Dig", p_master$first_touch_quality, p_master$output)
p_master$output <- ifelse(p_master$add_cover=="Dig" & p_master$skq=="Dig =", "Dig Error", p_master$output)

#cover
p_master$input <- ifelse(p_master$add_cover=="Cover", lag(p_master$block_touch_result,1), p_master$input)
p_master$output <- ifelse(p_master$add_cover=="Cover", p_master$first_touch_quality, p_master$output)

#freeball
p_master$input <- ifelse(p_master$skill=="Freeball", "Freeball Baseline", p_master$input)
p_master$output <- ifelse(p_master$skill=="Freeball", p_master$first_touch_quality, p_master$output)
p_master$output <- ifelse(p_master$skill=="Freeball" & p_master$skq=="Freeball =", "Freeball Error", p_master$output)

#set 2
p_master$input <- ifelse(p_master$skill=="Set", lag(p_master$output,1), p_master$input)
p_master$output <- ifelse(p_master$skq=="Set #", p_master$second_touch_result, p_master$output)
p_master$output <- ifelse(p_master$skq=="Set -", "Poor Set", p_master$output)
p_master$output  <- ifelse(p_master$skq=="Set =", "Ball Handling Error", p_master$output)

#attack 2
p_master$input <- ifelse(p_master$skill=="Attack", lag(p_master$second_touch_result,1), p_master$input)
p_master$input <- ifelse(p_master$skill=="Attack" & p_master$one_touch_ago %in% overpass, "Overpass", p_master$input)
p_master$output <- ifelse(p_master$skill=="Attack" & lead(p_master$add_cover,1)=="Block", lead(p_master$block_touch_result,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & lead(p_master$add_cover,1)!="Block", lead(p_master$first_touch_quality,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & p_master$skq=="Attack #", "Kill", p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & p_master$skq=="Attack =", "Attack Error", p_master$output)
p_master$output <- ifelse(p_master$skill=="Attack" & p_master$skq=="Attack /", "Attack Blocked", p_master$output)

#block 2
p_master$input <- ifelse(p_master$skill=="Block", lag(p_master$input,1), p_master$input)
p_master$output <- ifelse(p_master$skill=="Block", lead(p_master$first_touch_quality,1), p_master$output)
p_master$output <- ifelse(p_master$skill=="Block" & p_master$skq=="Block #", "Stuff Block", p_master$output)
p_master$output <- ifelse(p_master$skill=="Block" & (p_master$skq=="Block =" | p_master$skq=="Block !"), "Block Tooled", p_master$output)

#dig 2
p_master$input <- ifelse(p_master$add_cover=="Dig" & substr(lag(p_master$skq,1),1,5)!="Block", lag(p_master$input,1), p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & lag(p_master$skq,1)=="Block +", paste0("Block Slowdown + ", ifelse(p_master$video_time - lag(p_master$video_time,2) < 2, p_master$video_time - lag(p_master$video_time,2),"2"), " sec"), p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & (p_master$input=="Block Slowdown + 3 sec" | p_master$input=="Block Slowdown + 4 sec" | p_master$input=="Block Slowdown + 5 sec"), "Block Slowdown + 2 sec", p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & substr(lag(p_master$skq,1),1,5)=="Block" & lag(p_master$skq,1)!="Block +", "Unhelpful Block Touch", p_master$input)
p_master$input <- ifelse(p_master$add_cover=="Dig" & lag(p_master$skq) %in% c("Block =", "Block !"), "Block Tooled", p_master$input)
p_master$output <- ifelse(p_master$add_cover=="Dig", p_master$first_touch_quality, p_master$output)
p_master$output <- ifelse(p_master$add_cover=="Dig" & p_master$skq=="Dig =", "Dig Error", p_master$output)

#cover 2
p_master$input <- ifelse(p_master$add_cover=="Cover", lag(p_master$block_touch_result,1), p_master$input)
p_master$output <- ifelse(p_master$add_cover=="Cover", p_master$first_touch_quality, p_master$output)

#freeball 2
p_master$input <- ifelse(p_master$skill=="Freeball", "Freeball Baseline", p_master$input)
p_master$output <- ifelse(p_master$skill=="Freeball", p_master$first_touch_quality, p_master$output)
p_master$output <- ifelse(p_master$skill=="Freeball" & p_master$skq=="Freeball =", "Freeball Error", p_master$output)

p_master$input = ifelse(is.na(p_master$input),"na",p_master$input)
p_master$output = ifelse(is.na(p_master$output),"na", p_master$output)

write.csv(p_master, "p_master1.csv",row.names = FALSE)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#####
#added June 15, 2020
p_master$gender <- ifelse(grepl("Men's", p_master$team), "Men", "Women")