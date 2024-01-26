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
dataFile = "~/Gemini/dvw"
setwd(dataFile)

# add all .dvw files to wd first - then my.files will be a list of files to parse
my.files <- list.files(pattern = "*vsm", recursive = TRUE)
all_files <- as.data.frame(matrix(ncol=0,nrow=0))

#run read_dv code for all files in my.files - rbind them into "all_files"
for(q in 1:1500) {
  x <- read_dv(my.files[q], skill_evaluation_decode = "volleymetrics")
  x$plays$match_date <- x$meta$match$date
  all_files <- rbind(all_files, x$plays)
  print(q)
}

#rename total to master and remove non-plays
master <- subset(all_files, substring(code, 2, 2)!="P" & substring(code, 2, 2)!="p" 
                 & substring(code, 2, 2)!="z" & substring(code, 2, 2)!="c"
                 & substring(code, 2, 2)!="*" & substring(code, 2, 2)!="$")
#master <- master[,c(1,2,4,6,8,10,11,12,13,14,19,20,21,23,24,54,55,56,57,58,59,72,73,74,75,79,80,81,82,83)]
all_files <- NULL

#create touch_id for all rows - helps w/ ability to reorder properly if I screw something up down the road
master$id_master <- seq.int(nrow(master))
master <- master[order(master$id_master),] 

#create column for who the team making the contact is playing against
master$opponent <- NA
master$opponent <- ifelse(master$team == master$home_team, master$visiting_team, master$opponent)
master$opponent <- ifelse(master$team == master$visiting_team, master$home_team, master$opponent)

#create column as a count (for the denomenator for most equations)
master$count <- 1

#create cover as one of the skills
master$add_cover <- ifelse(master$skill=="Dig" & lag(master$skill, 2)=="Attack" & 
                             lag(master$skill, 1)=="Block" & lag(master$team, 2)==master$team, "Cover", master$skill)

#create skill-quality combination
master$skq <- paste(master$add_cover, master$evaluation_code)

#create Forward/Backward looking contacts that help power Input/Output Contacts
master$two_touch_ago <- ifelse(master$point_id == lag(master$point_id, 2), lag(master$skq, 2), NA)
master$one_touch_ago <- ifelse(master$point_id == lag(master$point_id, 1), lag(master$skq, 1), NA)
master$one_touch_future <- ifelse(master$point_id == lead(master$point_id, 1), lead(master$skq, 1), NA)
master$two_touch_future <- ifelse(master$point_id == lead(master$point_id, 2), lead(master$skq, 2), NA)
master$three_touch_future <- ifelse(master$point_id == lead(master$point_id, 3), lead(master$skq, 3), NA)

#create possession data
master$possession <- NA
master$possession <- ifelse(master$skill=="Serve", 1, master$possession)
master$possession <- ifelse(master$skill=="Reception", 2, master$possession)
x <- 0
repeat {
  x = x+1
  master$possession <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$team,1)==master$team, lag(master$possession), master$possession)
  master$possession <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$team,1)!=master$team, lag(master$possession) + 1, master$possession)
  if (x == 50){
    break
  }
}

#create rally - possession combination
master$rally_possession <- paste0(master$point_id, "-", master$possession)

#create contact count w/in each possession
master$contact <- NA
master$contact <- ifelse(master$skill=="Serve" | master$skill=="Reception", 1, master$contact)
x <- 0
repeat {
  x = x+1
  master$contact <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$possession,1)==master$possession, lag(master$contact) + 1, master$contact)
  master$contact <- ifelse(master$skill!="Serve" & master$skill!="Reception" & lag(master$possession,1)!=master$possession, 1, master$contact)
  if (x == 10){
    break
  }
}
master$contact <- ifelse(master$contact == 5, 1, master$contact)
master$contact <- ifelse(master$contact == 6, 2, master$contact)
master$contact <- ifelse(master$contact == 7, 3, master$contact)


#create column to denote if there is a winning or losing contact & fill in other touches within same possession if poss is won/lost
winners <- c("Serve #", "Attack #", "Block #")
losers <- c("Serve =", "Reception =", "Set =", "Attack =", "Attack /", "Block =", "Dig =", "Freeball =", "Cover =")
master$possession_winner <- ifelse(master$skq %in% winners, 1, 0)
master$possession_winner <- ifelse(lead(master$skq,1) %in% winners & master$rally_possession==lead(master$rally_possession, 1), 1, master$possession_winner)
master$possession_winner <- ifelse(lead(master$skq,2) %in% winners & master$rally_possession==lead(master$rally_possession, 2), 1, master$possession_winner)
master$possession_winner <- ifelse(lead(master$skq,3) %in% winners & master$rally_possession==lead(master$rally_possession, 3), 1, master$possession_winner)

master$possession_loser <- ifelse(master$skq %in% losers, 1, 0)
master$possession_loser <- ifelse(lead(master$skq,1) %in% losers & master$rally_possession==lead(master$rally_possession, 1), 1, master$possession_loser)
master$possession_loser <- ifelse(lead(master$skq,2) %in% losers & master$rally_possession==lead(master$rally_possession, 2), 1, master$possession_loser)
master$possession_loser <- ifelse(lead(master$skq,3) %in% losers & master$rally_possession==lead(master$rally_possession, 3), 1, master$possession_loser)

#create column for winning/losing on NEXT possession
master$poss_win_next <- 0
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,1) &
                                 master$possession==lead(master$possession,1)-1 & master$team!=lead(master$team,1) & lead(master$skq,1) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,2) &
                                 master$possession==lead(master$possession,2)-1 & master$team!=lead(master$team,2) & lead(master$skq,2) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,3) &
                                 master$possession==lead(master$possession,3)-1 & master$team!=lead(master$team,3) & lead(master$skq,3) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,4) &
                                 master$possession==lead(master$possession,4)-1 & master$team!=lead(master$team,4) & lead(master$skq,4) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,5) &
                                 master$possession==lead(master$possession,5)-1 & master$team!=lead(master$team,5) & lead(master$skq,5) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,6) &
                                 master$possession==lead(master$possession,6)-1 & master$team!=lead(master$team,6) & lead(master$skq,6) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,7) &
                                 master$possession==lead(master$possession,7)-1 & master$team!=lead(master$team,7) & lead(master$skq,7) %in% losers, 1, master$poss_win_next)
master$poss_win_next <- ifelse(master$point_id==lead(master$point_id,8) &
                                 master$possession==lead(master$possession,8)-1 & master$team!=lead(master$team,8) & lead(master$skq,8) %in% losers, 1, master$poss_win_next)

master$poss_lost_next <- 0
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,1) &
                                  master$possession==lead(master$possession,1)-1 & master$team!=lead(master$team,1) & lead(master$skq,1) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,2) &
                                  master$possession==lead(master$possession,2)-1 & master$team!=lead(master$team,2) & lead(master$skq,2) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,3) &
                                  master$possession==lead(master$possession,3)-1 & master$team!=lead(master$team,3) & lead(master$skq,3) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,4) &
                                  master$possession==lead(master$possession,4)-1 & master$team!=lead(master$team,4) & lead(master$skq,4) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,5) &
                                  master$possession==lead(master$possession,5)-1 & master$team!=lead(master$team,5) & lead(master$skq,5) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,6) &
                                  master$possession==lead(master$possession,6)-1 & master$team!=lead(master$team,6) & lead(master$skq,6) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,7) &
                                  master$possession==lead(master$possession,7)-1 & master$team!=lead(master$team,7) & lead(master$skq,7) %in% winners, 1, master$poss_lost_next)
master$poss_lost_next <- ifelse(master$point_id==lead(master$point_id,8) &
                                  master$possession==lead(master$possession,8)-1 & master$team!=lead(master$team,8) & lead(master$skq,8) %in% winners, 1, master$poss_lost_next)

#create column for winning/losing on NEXT NEXT possession
master$poss_win_nextnext <- 0
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,1) &
                                     master$possession==lead(master$possession,1)-2 & master$team==lead(master$team,1) & lead(master$skq,1) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,2) &
                                     master$possession==lead(master$possession,2)-2 & master$team==lead(master$team,2) & lead(master$skq,2) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,3) &
                                     master$possession==lead(master$possession,3)-2 & master$team==lead(master$team,3) & lead(master$skq,3) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,4) &
                                     master$possession==lead(master$possession,4)-2 & master$team==lead(master$team,4) & lead(master$skq,4) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,5) &
                                     master$possession==lead(master$possession,5)-2 & master$team==lead(master$team,5) & lead(master$skq,5) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,6) &
                                     master$possession==lead(master$possession,6)-2 & master$team==lead(master$team,6) & lead(master$skq,6) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,7) &
                                     master$possession==lead(master$possession,7)-2 & master$team==lead(master$team,7) & lead(master$skq,7) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,8) &
                                     master$possession==lead(master$possession,8)-2 & master$team==lead(master$team,8) & lead(master$skq,8) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,9) &
                                     master$possession==lead(master$possession,9)-2 & master$team==lead(master$team,9) & lead(master$skq,9) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,10) &
                                     master$possession==lead(master$possession,10)-2 & master$team==lead(master$team,10) & lead(master$skq,10) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,11) &
                                     master$possession==lead(master$possession,11)-2 & master$team==lead(master$team,11) & lead(master$skq,11) %in% winners, 1, master$poss_win_nextnext)
master$poss_win_nextnext <- ifelse(master$point_id==lead(master$point_id,12) &
                                     master$possession==lead(master$possession,12)-2 & master$team==lead(master$team,12) & lead(master$skq,12) %in% winners, 1, master$poss_win_nextnext)

master$poss_lost_nextnext <- 0
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,1) &
                                      master$possession==lead(master$possession,1)-2 & master$team==lead(master$team,1) & lead(master$skq,1) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,2) &
                                      master$possession==lead(master$possession,2)-2 & master$team==lead(master$team,2) & lead(master$skq,2) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,3) &
                                      master$possession==lead(master$possession,3)-2 & master$team==lead(master$team,3) & lead(master$skq,3) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,4) &
                                      master$possession==lead(master$possession,4)-2 & master$team==lead(master$team,4) & lead(master$skq,4) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,5) &
                                      master$possession==lead(master$possession,5)-2 & master$team==lead(master$team,5) & lead(master$skq,5) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,6) &
                                      master$possession==lead(master$possession,6)-2 & master$team==lead(master$team,6) & lead(master$skq,6) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,7) &
                                      master$possession==lead(master$possession,7)-2 & master$team==lead(master$team,7) & lead(master$skq,7) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,8) &
                                      master$possession==lead(master$possession,8)-2 & master$team==lead(master$team,8) & lead(master$skq,8) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,9) &
                                      master$possession==lead(master$possession,9)-2 & master$team==lead(master$team,9) & lead(master$skq,9) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,10) &
                                      master$possession==lead(master$possession,10)-2 & master$team==lead(master$team,10) & lead(master$skq,10) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,11) &
                                      master$possession==lead(master$possession,11)-2 & master$team==lead(master$team,11) & lead(master$skq,11) %in% losers, 1, master$poss_lost_nextnext)
master$poss_lost_nextnext <- ifelse(master$point_id==lead(master$point_id,12) &
                                      master$possession==lead(master$possession,12)-2 & master$team==lead(master$team,12) & lead(master$skq,12) %in% losers, 1, master$poss_lost_nextnext)

#create efficiency w/in possession
master$poss_eff <- master$possession_winner-master$possession_loser
master$poss_eff_next <- master$poss_win_next-master$poss_lost_next
master$poss_eff_nextnext <- master$poss_win_nextnext-master$poss_lost_nextnext

#create rally winner & rally loser
master$rally_winner <- ifelse(master$team==master$point_won_by, 1, 0) 
master$rally_loser <- ifelse(master$team!=master$point_won_by, 1, 0)

#create efficiency w/in rally
master$rally_eff <- master$rally_winner

#create serving & home team RALLY efficiency
master$serveteam_rallyeff <- ifelse(master$serving_team==master$point_won_by, 1,0)
master$hometeam_rallyeff <- ifelse(master$home_team==master$point_won_by, 1, 0)

#create serving & home team POSSESSION efficiency
master$st_posswin <- NA
master$st_posslose <- NA
master$ht_posswin <- NA
master$ht_posslose <- NA

master$st_posswin <- ifelse(master$serving_team==master$team & master$possession_winner==1, 1, master$st_posswin)
master$ht_posswin <- ifelse(master$home_team==master$team & master$possession_winner==1, 1, master$ht_posswin)
master$st_posslose <- ifelse(master$serving_team==master$team & master$possession_loser==1, 1, master$st_posslose)
master$ht_posslose <- ifelse(master$home_team==master$team & master$possession_loser==1, 1, master$ht_posslose)

master$serveteam_posseff <- master$st_posswin-master$st_posslose
master$hometeam_posseff <- master$ht_posswin-master$ht_posslose

#is serving team touching the ball?
master$serveteam_touch <- ifelse(master$team==master$serving_team, "yes", "no")

#is home team touching the ball?
master$hometeamtouch <- ifelse(master$home_team==master$team, "yes", "no")

#create endzone+subzone for setting
master$endzone_full <- paste0(master$end_zone, master$end_subzone)

#create location where setter takes ball for R,D,Cover,Freeball codes
master$set_from <- NA
master$set_from <- ifelse(master$add_cover %in% c("Reception", "Dig", "Cover", "Freeball") & lead(master$team, 1)==master$team & substr(master$one_touch_future,1,3)=="Set", lead(master$end_zone,1), master$set_from)

#create what type of attack follows the first touch (attack code starts w/ V or not)
master$attack_type <- NA
master$attack_type <- ifelse(master$add_cover %in% c("Reception", "Dig", "Cover", "Freeball") & lead(master$team, 1)==master$team & lead(master$team,2)==master$team & lead(master$add_cover,1)=="Set", 
                             substr(lead(master$attack_code,2),1,1), master$attack_type)

master$in_or_oos <- NA
master$in_or_oos <- ifelse(master$attack_type=="V", "OOS", master$in_or_oos)
master$in_or_oos <- ifelse(master$attack_type %in% c("C", "X", "P"), "In Sys", master$in_or_oos)

master$attack_type <- NULL

#create unique set_id
master$set_id <- paste0(master$match_id, "- Set ", master$set_number)

#create if team won the set overall
master <- master[order(master$id_master),] 

master$who_won_set <- NA
master$who_lost_set <- NA
master$who_won_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team==master$point_won_by, master$team, master$who_won_set)
master$who_won_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team!=master$point_won_by, master$opponent, master$who_won_set)
master$who_lost_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team!=master$point_won_by, master$team, master$who_lost_set)
master$who_lost_set <- ifelse(master$set_id!=lead(master$set_id,1) & master$team==master$point_won_by, master$opponent, master$who_lost_set)

master$who_won_set <- ifelse(master$id_master==max(master$id_master) & master$team==master$point_won_by, master$team, master$who_won_set)
master$who_won_set <- ifelse(master$id_master==max(master$id_master) & master$team!=master$point_won_by, master$opponent, master$who_won_set)
master$who_lost_set <- ifelse(master$id_master==max(master$id_master) & master$team!=master$point_won_by, master$team, master$who_lost_set)
master$who_lost_set <- ifelse(master$id_master==max(master$id_master) & master$team==master$point_won_by, master$opponent, master$who_lost_set)

master <- fill(master, who_won_set, who_lost_set, .direction = c("up"))

master$teamwonset <- ifelse(master$team==master$who_won_set, 1, 0)
master$teamlostset <- ifelse(master$team==master$who_lost_set, 1, 0)

master$who_won_set <- NULL
master$who_lost_set <- NULL

master$passer_rating <- NA
master$passer_rating <- ifelse(master$skq=="Reception #", 3, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception +", 3, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception !", 2, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception -", 1, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception /", 1, master$passer_rating)
master$passer_rating <- ifelse(master$skq=="Reception =", 0, master$passer_rating)


# when working with lag/lead functions, always double check order of data
master <- master[order(master$id_master),] 

# create new function to read, if x IS NOT in group y...
'%!in%' <- function(x,y)!('%in%'(x,y))

# create 1st touch quality for input - these are built by looking at rally eff after each contact
# there was a pocket of positive touches, a cluster around 0, and a pocket of negative touches
pos_touch <- c("Reception #", "Reception +", "Dig #", "Freeball #", "Freeball +", "Cover #")
neutral_touch <- c("Dig +", "Dig !", "Freeball !", "Reception !", "Cover +", "Cover !")
neg_touch <- c("Dig -", "Freeball -", "Freeball /", "Reception -", "Reception /", "Cover -")

# build first touch quality - if contact is pos/neutral/neg, say so as well as the zone of the subsequent touch
master$first_touch_quality <- NA
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)==master$team & lead(master$skill,1)=="Set" &
                                       master$skq %in% pos_touch, paste0("Good ", master$add_cover, " into ", lead(master$end_zone,1)), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)==master$team & lead(master$skill,1)!="Set" &
                                       master$skq %in% pos_touch, paste0("Good ", master$add_cover, " into ", lead(master$start_zone,1)), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)==master$team & lead(master$skill,1)=="Set" &
                                       master$skq %in% neutral_touch, paste0("OK ", master$add_cover, " into ", lead(master$end_zone,1)), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)==master$team & lead(master$skill,1)!="Set" &
                                       master$skq %in% neutral_touch, paste0("OK ", master$add_cover, " into ", lead(master$start_zone,1)), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") &
                                       master$skq %in% neg_touch & !is.na(master$one_touch_future), paste0("Poor ", master$add_cover), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)==master$team & lead(master$skill,1)=="Set" &
                                       master$skq %in% neg_touch & !is.na(master$one_touch_future), paste0("Poor ", master$add_cover, " into ", lead(master$end_zone,1)), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)==master$team & lead(master$skill,1)!="Set" &
                                       master$skq %in% neg_touch & !is.na(master$one_touch_future), paste0("Poor ", master$add_cover, " into ", lead(master$start_zone,1)), master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)!=master$team & lead(master$skill,1)=="Attack" &
                                       master$skq %!in% c("Reception =", "Dig =", "Freeball =", "Cover ="), "Overpass into Attack", master$first_touch_quality)
master$first_touch_quality <- ifelse(master$add_cover %in% c("Reception", "Dig", "Freeball", "Cover") & lead(master$team,1)!=master$team & lead(master$skill,1)!="Attack" &
                                       master$skq %!in% c("Reception =", "Dig =", "Freeball =", "Cover ="), "Overpass into Freeball", master$first_touch_quality)
master$first_touch_quality <- ifelse(master$skq %in% c("Reception =", "Dig =", "Freeball =", "Cover ="), paste0("Error on ", master$add_cover), master$first_touch_quality)


# create the 2nd touch situation labels
## we've worked backwards here, the output of setting must equal the input of attacking
master$second_touch_result <- NA
master$second_touch_result <- ifelse(master$skq=="Set #" & lead(master$skill=="Attack",1), paste0(lag(master$in_or_oos,1), " - ", lead(master$num_players)), master$second_touch_result)
master$second_touch_result <- ifelse(master$skq=="Set -", "Poor Set", master$second_touch_result)
master$second_touch_result <- ifelse(master$skq=="Set =", "Ball Handling Error", master$second_touch_result)

# create block touch situation labels - if a cover, add the "time" as well
master$block_touch_result <- NA
master$block_touch_result <- ifelse(master$skill == "Block" & lead(master$add_cover!="Cover"), lead(master$first_touch_quality,1), master$block_touch_result)
master$block_touch_result <- ifelse(master$skill == "Block" & lead(master$add_cover=="Cover"), paste0(lag(master$skill_subtype,1), " + ", ifelse(lead(master$video_time,1) - lag(master$video_time,1) < 2, lead(master$video_time,1) - lag(master$video_time,1),"2"), " sec to Cover"), master$block_touch_result)
master$block_touch_result <- ifelse(master$skq %in% c("Block =", "Block !"), "Block Tooled", master$block_touch_result)
master$block_touch_result <- ifelse(master$skq=="Block #", "Stuff Block", master$block_touch_result)

overpass <- c("Reception /", "Dig -", "Cover -", "Freeball /")

######## SIMPLIFY ############
master$input <- NA
master$output <- NA

#serve
master$input <- ifelse(master$skill=="Serve", "Pre-Serve", master$input)
master$output <- ifelse(master$skill=="Serve", lead(master$first_touch_quality,1), master$output)
master$output <- ifelse(master$skill=="Serve" & master$skq=="Serve #", "Service Ace", master$output)
master$output <- ifelse(master$skill=="Serve" & master$skq=="Serve =", "Service Error", master$output)

#reception
master$input <- ifelse(master$skill=="Reception", "Pre-Serve", master$input)
master$output <- ifelse(master$skill=="Reception", master$first_touch_quality, master$output)
master$output <- ifelse(master$skill=="Reception" & master$skq=="Reception =", "Reception Error", master$output)

#set
master$input <- ifelse(master$skill=="Set", lag(master$output,1), master$input)
master$output <- ifelse(master$skq=="Set #", master$second_touch_result, master$output)
master$output <- ifelse(master$skq=="Set -", "Poor Set", master$output)
master$output  <- ifelse(master$skq=="Set =", "Ball Handling Error", master$output)

#attack
master$input <- ifelse(master$skill=="Attack", lag(master$second_touch_result,1), master$input)
master$input <- ifelse(master$skill=="Attack" & master$one_touch_ago %in% overpass, "Overpass", master$input)
master$output <- ifelse(master$skill=="Attack" & lead(master$add_cover,1)=="Block", lead(master$block_touch_result,1), master$output)
master$output <- ifelse(master$skill=="Attack" & lead(master$add_cover,1)!="Block", lead(master$first_touch_quality,1), master$output)
master$output <- ifelse(master$skill=="Attack" & master$skq=="Attack #", "Kill", master$output)
master$output <- ifelse(master$skill=="Attack" & master$skq=="Attack =", "Attack Error", master$output)
master$output <- ifelse(master$skill=="Attack" & master$skq=="Attack /", "Attack Blocked", master$output)

#block
master$input <- ifelse(master$skill=="Block", lag(master$input,1), master$input)
master$output <- ifelse(master$skill=="Block", lead(master$first_touch_quality,1), master$output)
master$output <- ifelse(master$skill=="Block" & master$skq=="Block #", "Stuff Block", master$output)
master$output <- ifelse(master$skill=="Block" & (master$skq=="Block =" | master$skq=="Block !"), "Block Tooled", master$output)

#dig
master$input <- ifelse(master$add_cover=="Dig" & substr(lag(master$skq,1),1,5)!="Block", lag(master$input,1), master$input)
master$input <- ifelse(master$add_cover=="Dig" & lag(master$skq,1)=="Block +", paste0("Block Slowdown + ", ifelse(master$video_time - lag(master$video_time,2) < 2, master$video_time - lag(master$video_time,2),"2"), " sec"), master$input)
master$input <- ifelse(master$add_cover=="Dig" & (master$input=="Block Slowdown + 3 sec" | master$input=="Block Slowdown + 4 sec" | master$input=="Block Slowdown + 5 sec"), "Block Slowdown + 2 sec", master$input)
master$input <- ifelse(master$add_cover=="Dig" & substr(lag(master$skq,1),1,5)=="Block" & lag(master$skq,1)!="Block +", "Unhelpful Block Touch", master$input)
master$input <- ifelse(master$add_cover=="Dig" & lag(master$skq) %in% c("Block =", "Block !"), "Block Tooled", master$input)
master$output <- ifelse(master$add_cover=="Dig", master$first_touch_quality, master$output)
master$output <- ifelse(master$add_cover=="Dig" & master$skq=="Dig =", "Dig Error", master$output)

#cover
master$input <- ifelse(master$add_cover=="Cover", lag(master$block_touch_result,1), master$input)
master$output <- ifelse(master$add_cover=="Cover", master$first_touch_quality, master$output)

#freeball
master$input <- ifelse(master$skill=="Freeball", "Freeball Baseline", master$input)
master$output <- ifelse(master$skill=="Freeball", master$first_touch_quality, master$output)
master$output <- ifelse(master$skill=="Freeball" & master$skq=="Freeball =", "Freeball Error", master$output)

#set 2
master$input <- ifelse(master$skill=="Set", lag(master$output,1), master$input)
master$output <- ifelse(master$skq=="Set #", master$second_touch_result, master$output)
master$output <- ifelse(master$skq=="Set -", "Poor Set", master$output)
master$output  <- ifelse(master$skq=="Set =", "Ball Handling Error", master$output)

#attack 2
master$input <- ifelse(master$skill=="Attack", lag(master$second_touch_result,1), master$input)
master$input <- ifelse(master$skill=="Attack" & master$one_touch_ago %in% overpass, "Overpass", master$input)
master$output <- ifelse(master$skill=="Attack" & lead(master$add_cover,1)=="Block", lead(master$block_touch_result,1), master$output)
master$output <- ifelse(master$skill=="Attack" & lead(master$add_cover,1)!="Block", lead(master$first_touch_quality,1), master$output)
master$output <- ifelse(master$skill=="Attack" & master$skq=="Attack #", "Kill", master$output)
master$output <- ifelse(master$skill=="Attack" & master$skq=="Attack =", "Attack Error", master$output)
master$output <- ifelse(master$skill=="Attack" & master$skq=="Attack /", "Attack Blocked", master$output)

#block 2
master$input <- ifelse(master$skill=="Block", lag(master$input,1), master$input)
master$output <- ifelse(master$skill=="Block", lead(master$first_touch_quality,1), master$output)
master$output <- ifelse(master$skill=="Block" & master$skq=="Block #", "Stuff Block", master$output)
master$output <- ifelse(master$skill=="Block" & (master$skq=="Block =" | master$skq=="Block !"), "Block Tooled", master$output)

#dig 2
master$input <- ifelse(master$add_cover=="Dig" & substr(lag(master$skq,1),1,5)!="Block", lag(master$input,1), master$input)
master$input <- ifelse(master$add_cover=="Dig" & lag(master$skq,1)=="Block +", paste0("Block Slowdown + ", ifelse(master$video_time - lag(master$video_time,2) < 2, master$video_time - lag(master$video_time,2),"2"), " sec"), master$input)
master$input <- ifelse(master$add_cover=="Dig" & (master$input=="Block Slowdown + 3 sec" | master$input=="Block Slowdown + 4 sec" | master$input=="Block Slowdown + 5 sec"), "Block Slowdown + 2 sec", master$input)
master$input <- ifelse(master$add_cover=="Dig" & substr(lag(master$skq,1),1,5)=="Block" & lag(master$skq,1)!="Block +", "Unhelpful Block Touch", master$input)
master$input <- ifelse(master$add_cover=="Dig" & lag(master$skq) %in% c("Block =", "Block !"), "Block Tooled", master$input)
master$output <- ifelse(master$add_cover=="Dig", master$first_touch_quality, master$output)
master$output <- ifelse(master$add_cover=="Dig" & master$skq=="Dig =", "Dig Error", master$output)

#cover 2
master$input <- ifelse(master$add_cover=="Cover", lag(master$block_touch_result,1), master$input)
master$output <- ifelse(master$add_cover=="Cover", master$first_touch_quality, master$output)

#freeball 2
master$input <- ifelse(master$skill=="Freeball", "Freeball Baseline", master$input)
master$output <- ifelse(master$skill=="Freeball", master$first_touch_quality, master$output)
master$output <- ifelse(master$skill=="Freeball" & master$skq=="Freeball =", "Freeball Error", master$output)

master$input = ifelse(is.na(master$input),"na",master$input)
master$output = ifelse(is.na(master$output),"na", master$output)

write.csv(master, "master1.csv",row.names = FALSE)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#####
#added June 15, 2020
master$gender <- ifelse(grepl("Men's", master$team), "Men", "Women")