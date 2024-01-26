library(datavolley)
library(dplyr)

SEC <- c("University of Kentucky", "Mississippi State University", "University of Florida", 
         "University of Tennessee", "University of Mississippi", "University of Arkansas",
         "Louisiana State University", "University of Georgia", "Texas A&M University",
         "University of South Carolina", "Auburn University", "University of Alabama", "University of Missouri")
Big10 <- c("University of Wisconsin-Madison", "University of Nebraska-Lincoln", "Ohio State University",
           "Purdue University", "University of Minnesota", "Pennsylvania State University",
           "University of Illinois Urbana-Champaign", "University of Michigan", "University of Maryland",
           "Northwestern University", "Michigan State University", "Indiana University, Bloomington",
           "University of Iowa", "Rutgers University")
Big12 <- c("University of Texas at Austin", "Baylor University", "West Virginia University",
           "Iowa State University", "University of Kansas", "Texas Tech University",
           "Kansas State University", "University of Oklahoma", "Texas Christian University", "University of Cincinnati",
           "Brigham Young University", "University of Houston", "University of Central Florida")
Pac12 <- c("University of Washington", "University of California, Los Angeles", "University of Utah",
           "University of Oregon", "Stanford University", "Washington State University",
           "University of Southern California", "University of Arizona", "Arizona State University",
           "University of Colorado, Boulder", "Oregon State University", "University of California, Berkeley")
ACC <- c("University of Louisville", "University of Pittsburgh", "University of Miami",
         "Georgia Institute Of Technology", "Florida State University", "University of North Carolina",
         "North Carolina State University", "University of Notre Dame", "Duke University",
         "Syracuse University", "Wake Forest University", "Boston College",
         "Clemson University", "Virginia Tech", "University of Virginia")
A10 <- c("University of Dayton", "Loyola University Chicago", "Virginia Commonwealth University", "Davidson College",
         "Fordham University", "Saint Louis University", "Duquesne University", "George Mason University",
         "George Washington University", "University of Rhode Island")
BigEast <- c("Creighton University", "Marquette University", "St. John's University", "Xavier University",
             "Depaul University", "Villanova University", "Seton Hall University", "Butler University",
             "Georgetown University", "Providence College", "University of Connecticut")
CAA <- c("Towson University", "University of Delaware", "Campbell University", "Stony Brook University",
         "College of Charleston", "Northeastern University", "University of North Carolina Wilmington",
         "Hofstra University", "William & Mary", "North Carolina A&T State University", "Elon University", "Hampton University")
WCC <- c("Pepperdine University", "Loyola Marymount University", "University of San Diego", "University of the Pacific",
         "Saint Mary's College", "Santa Clara University", "University of San Francisco", "Portland University",
         "Gonzaga University")
OVC <- c("Eastern Illinois University", "Southeast Missouri State University", "Morehead State University",
         "Lindenwood University", "University of Southern Indiana", "Tennessee Technological University",
         "University of Arkansas, Little Rock", "Western Illinois University", "University of Tennessee at Martin",
         "Tennessee State University", "Southern Illinois University Edwardsville")
MAC <- c("University at Buffalo", "Ohio University", "Bowling Green State University", "University of Akron",
         "Kent State University", "Miami University (Ohio)", "Western Michigan University", "Ball State University",
         "University of Toledo", "Central Michigan University", "Northern Illinois University", "Eastern Michigan University")
MVC <- c("University of Northern Iowa", "Drake University", "University of Illinois at Chicago",
         "Southern Illinois University at Carbondale","Valparaiso University", "Illinois State University",
         "Missouri State University", "Belmont University", "Murray State University",
         "University of Evansville", "Indiana State University", "Bradley University")
SLand <- c("Southeastern Louisiana University", "Texas A&M University-Corpus Christi", "Houston Christian University",
           "McNeese State University", "Northwestern State University", "University of the Incarnate Word",
           "University of New Orleans", "Texas A&M University-Commerce", "Nicholls State University", "Lamar University")
WAC <- c("Stephen F. Austin State University", "Grand Canyon University", "University of Texas, Rio Grande Valley",
         "University of Texas at Arlington", "Utah Valley University", "Tarleton State University",
         "California Baptist University", "Abilene Christian University", "Utah Tech University",
         "Seattle University", "Southern Utah University")
AAC <- c("University of South Florida", "Florida Atlantic University", "East Carolina University", "University of Memphis",
         "Temple University", "University of North Carolina at Charlotte", "University of Alabama at Birmingham",
         "Southern Methodist University", "Wichita State University", "Rice University", "University of North Texas",
         "University of Tulsa", "University of Texas at San Antonio", "Tulane University")
AEast <- c("University of Maryland, Baltimore County", "University of New Hampshire", "Binghamton University",
           "Bryant University", "University at Albany", "New Jersey Institute of Technology")
ASUN <- c("Florida Gulf Coast University", "Lipscomb University", "Eastern Kentucky University", "Kennesaw State University",
          "University of North Alabama", "University of North Florida", "Stetson University", "Bellarmine University",
          "Jacksonville University", "Austin Peay State University", "University of Central Arkansas", "Queens University of Charlotte")
BigSky <- c("California State University, Sacramento", "University of Northern Colorado", "Montana State University-Bozeman",
            "Weber State University", "Portland State University", "University of Montana", "Eastern Washington University",
            "Idaho State University", "Northern Arizona University", "University of Idaho")
BigSouth <- c("High Point University", "Winthrop University", "Gardner-Webb University", "Presbyterian College",
              "University of South Carolina Upstate", "Radford University", "Charleston Southern University", "University of North Carolina Asheville")
BigWest <- c("University of California, Santa Barbara", "University of Hawaii", "California Polytechnic State University",
             "California State University, Long Beach", "University of California, San Diego", "University of California, Irvine",
             "University of California, Davis", "California State University, Bakersfield", "University of California, Riverside",
             "California State University, Northridge", "California State University, Fullerton")
CUSA <- c("Western Kentucky University", "New Mexico State University", "Middle Tennessee State University",
          "University of Texas at El Paso", "Liberty University", "Florida International University",
          "Louisiana Tech University", "Jacksonville State University", "Sam Houston State University")
Horizon <- c("Wright State University", "University of Wisconsin-Green Bay", "University of Wisconsin, Milwaukee",
             "Northern Kentucky University", "Oakland University", "Cleveland State University", "Purdue University Fort Wayne",
             "Youngstown State University", "Robert Morris University", "Indiana University-Purdue University at Indianapolis")
MAAC <- c("Fairfield University", "Quinnipiac University", "Marist College", "Niagara University", "Rider University",
          "Iona College", "Canisius College", "Manhattan College", "Siena College", "Saint Peter's University")
MEAC <- c("Coppin State University", "Howard University", "Delaware State University", "University of Maryland Eastern Shore",
          "Norfolk State University", "North Carolina Central University", "Morgan State University", "South Carolina State University")
MWest <- c("Utah State University", "Colorado State University", "University of Nevada, Las Vegas", "University of New Mexico",
           "California State University, Fresno", "Boise State University", "University of Wyoming", "U.S. Air Force Academy",
           "San Jose State University", "San Diego State University", "University of Nevada, Reno")
NEC <- c("Fairleigh Dickinson University", "Sacred Heart University", "Saint Francis University", "Long Island University",
         "Merrimack College", "Central Connecticut State University", "Stonehill College", "Le Moyne College")
SoCon <- c("The Citadel", "Mercer University", "Wofford College", "Samford University", "Furman University", "Western Carolina University",
           "University of Tennessee at Chattanooga", "East Tennessee State University", "University of North Carolina - Greensboro")
SWAC <- c("Alabama State University", "Prairie View A&M University", "Alabama A&M University", "Jackson State University",
          "Grambling State University", "Texas Southern University", "Bethune-Cookman University", "University of Arkansas, Pine Bluff",
          "Alcorn State University", "Mississippi Valley State University", "Southern University, Baton Rouge")
SunBelt <- c("Coastal Carolina University", "James Madison University", "Georgia Southern University", "Old Dominion University",
             "Marshall University", "Appalachian State University", "Georgia State University", "Texas State University",
             "Troy University", "University of South Alabama", "University of Louisiana", "The University of Southern Mississippi",
             "Arkansas State University", "University of Louisiana at Monroe")
Ivy <- c("Yale University", "Princeton University", "Brown University", "Harvard University", "Dartmouth College",
         "Cornell University", "University of Pennsylvania", "Columbia University")
Summit <- c("University of Missouri-Kansas City", "University of Nebraska Omaha", "University of South Dakota", "North Dakota State University",
            "South Dakota State University", "University of North Dakota", "University of St. Thomas (Minnesota)", "Oral Roberts University",
            "University of Denver")
Patriot <- c("Colgate University", "American University", "Bucknell University", "Loyola University Maryland", "Lehigh University",
             "Lafayette College", "U.S. Military Academy", "U.S. Naval Academy", "College of the Holy Cross")

conferences <- c("SEC", "BIG 10", "BIG 12", "PAC 12", "ACC", "A10", "BIG EAST",
                 "CAA", "WCC", "OVC", "MVC", "MAC", "SOUTHLAND", "WAC", "AAC",
                 "AMERICA EAST", "ASUN", "BIG SKY", "BIG SOUTH", "BIG WEST",
                 "C-USA", "HORIZON", "MAAC", "MEAC", "MOUNTAIN WEST", "NEC",
                 "SOCON", "SWAC", "SUN BELT", "IVY", "SUMMIT", "PATRIOT")

'%!in%' <- function(x,y)!('%in%'(x,y))


df <- master %>% 
  select(player_name, team, skill, skill_type, evaluation_code, attack_code, set_type, 
         start_zone, end_zone, end_subzone, skill_subtype, num_players_numeric, 
         custom_code, home_setter_position, visiting_setter_position, 
         start_coordinate_x, start_coordinate_y, mid_coordinate_x, mid_coordinate_y, end_coordinate_x, end_coordinate_y,
         two_touch_ago, one_touch_ago, one_touch_future, two_touch_future, three_touch_future)


df_pos_add <- df %>% 
  group_by(player_name, team) %>% 
  summarise(att = n(),
            atk_att = sum(skill == "Attack"),
            set_att = sum(skill == "Set"),
            srv_att = sum(skill == "Serve"),
            dig_att = sum(skill == "Dig"),
            pass_att = sum(skill == "Reception"),
            l_atk = sum(attack_code %in% c("X5", "V5", "X9")),
            r_atk = sum(attack_code %in% c("X6", "X4", "V6", "X3")),
            m_atk = sum(attack_code %in% c("X1", "X2", "XM", "XL", "CF", "CB", "X7")),
            s_atk = sum(attack_code == "PS")) %>% 
  mutate(atk_p = atk_att / att,
         set_p = set_att / att,
         srv_p = srv_att / att,
         dig_p = dig_att / att,
         pass_p = pass_att / att,
         l_atk_p = l_atk / atk_att,
         r_atk_p = r_atk / atk_att,
         m_atk_p = m_atk / atk_att,
         s_atk_p = s_atk / atk_att,
         position = case_when(
           set_p > 0.55 | set_att > 500 ~ "SETTER",
           atk_p < 0.25 & dig_p > 0.20 & pass_p > 0.1~ "DS/L",
           srv_p > 0.70 & atk_p < 0.03~ "SS",
           l_atk_p > 0.65 ~ "OH",
           r_atk_p > 0.65 ~ "OPP",
           m_atk_p > 0.65 ~ "MB",
           s_atk_p > 0.65 ~ "SETTER",
           r_atk_p > l_atk_p ~ "OPP",
           l_atk_p > r_atk_p ~ "OH"),
         conference = case_when(
           team %in% SEC ~ "SEC",
           team %in% Big10 ~ "BIG 10",
           team %in% Big12 ~ "BIG 12",
           team %in% Pac12 ~ "PAC 12",
           team %in% ACC ~ "ACC",
           team %in% A10 ~ "A10",
           team %in% BigEast ~ "BIG EAST",
           team %in% CAA ~ "CAA",
           team %in% WCC ~ "WCC",
           team %in% OVC ~ "OVC",
           team %in% MVC ~ "MVC",
           team %in% MAC ~ "MAC",
           team %in% SLand ~ "SOUTHLAND",
           team %in% WAC ~ "WAC",
           team %in% AAC ~ "AAC",
           team %in% AEast ~ "AMERICA EAST",
           team %in% ASUN ~ "ASUN",
           team %in% BigSky ~ "BIG SKY",
           team %in% BigSouth ~ "BIG SOUTH",
           team %in% BigWest ~ "BIG WEST",
           team %in% CUSA ~ "C-USA",
           team %in% Horizon ~ "HORIZON",
           team %in% MAAC ~ "MAAC",
           team %in% MEAC ~ "MEAC",
           team %in% MWest ~ "MOUNTAIN WEST",
           team %in% NEC ~ "NEC",
           team %in% SoCon ~ "SOCON",
           team %in% SWAC ~ "SWAC",
           team %in% SunBelt ~ "SUN BELT",
           team %in% Ivy ~ "IVY",
           team %in% Summit ~ "SUMMIT",
           team %in% Patriot ~ "PATRIOT",
           team %!in% c("SEC", "Big10", "Big12", "Pac12", "ACC",
                        "A10", "BigEast", "CAA", "WCC", "OVC",
                        "MVC", "MAC", "SLand", "WAC", "AAC", "AEast",
                        "ASUN", "BigSky", "BigSouth", "BigWest", "CUSA",
                        "Horizon", "MAAC", "MEAC", "MWest", "NEC", "SoCon",
                        "SWAC", "SunBelt", "Ivy", "Summit", "Patriot") ~ "Other"))

final <- full_join(df, df_pos_add)

#Serving Coefficients
s_PP <- 0.47
s_GP <- 0.45
s_MP <- 0.41
s_BP <- 0.3

#Passing Coefficients
p_PP <- 0.453
p_GP <- 0.446
p_MP <- 0.398
p_BP <- 0.315 

#Blocking Coefficients
b_PB = 1.0000000
b_GB = 0.5817308
b_MB = 0.4598733
b_BB = 0.1738182
b_BE = 0
INS_atk = c("X5", "X6", "X9", "X4", "XT", "XY", "CF", "CB", "X1", "X2", "X7")
OOS_atk = c("V5", "V6", "VP", "VD", "V0", "V8", "VR", "VB")

all_stats <- final %>% 
  group_by(player_name, team, position, conference) %>%
  summarise(
    ### ATTACKING ####
    atk_att = sum(skill == "Attack"),
    atk_kill = sum(skill == "Attack" & evaluation_code == "#"),
    atk_ue = sum(skill == "Attack" & evaluation_code == "="),
    atk_be = sum(skill == "Attack" & evaluation_code == "/"),
    atk_INS_att = sum(skill == "Attack" & attack_code %in% INS_atk),
    atk_INS_err = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "="),
    atk_INS_kill = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "#"),
    atk_INS_blkd = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "/"),
    atk_INS_recy = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "!"),
    atk_OOS_att = sum(skill == "Attack" & attack_code %in% OOS_atk),
    atk_OOS_err = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "="),
    atk_OOS_kill = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "#"),
    atk_OOS_blkd = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "/"),
    atk_OOS_recy = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "!"),
    atk_eff = (sum(skill == "Attack" & evaluation_code == "#") -
                 (sum(skill == "Attack" & evaluation_code == "=") + 
                    sum(skill == "Attack" & evaluation_code == "/"))) / 
      sum(skill == "Attack"),
    atk_kill_p = sum(skill == "Attack" & evaluation_code == "#") / sum(skill == "Attack"),
    atk_err_p = sum(skill == "Attack" & evaluation_code == "=") / sum(skill == "Attack"),
    atk_blk_p = sum(skill == "Attack" & evaluation_code == "/") / sum(skill == "Attack"),
    atk_INS_eff = (sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "#") -
                     (sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "=") + 
                        sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "/"))) /
      sum(skill == "Attack" & attack_code %in% INS_atk),
    atk_OOS_eff = (sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "#") -
                     (sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "=") +
                        sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "/"))) /
      sum(skill == "Attack" & attack_code %in% OOS_atk),
    atk_INS_kill_p = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "#") /
      sum(skill == "Attack" & attack_code %in% INS_atk),
    atk_OOS_kill_p = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "#") /
      sum(skill == "Attack" & attack_code %in% OOS_atk),
    atk_INS_err_p = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "=") /
      sum(skill == "Attack" & attack_code %in% INS_atk),
    atk_OOS_err_p = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "=") /
      sum(skill == "Attack" & attack_code %in% OOS_atk),
    atk_INS_blk_p = sum(skill == "Attack" & attack_code %in% INS_atk & evaluation_code == "/") /
      sum(skill == "Attack" & attack_code %in% INS_atk),
    atk_OOS_blk_p = sum(skill == "Attack" & attack_code %in% OOS_atk & evaluation_code == "/") /
      sum(skill == "Attack" & attack_code %in% OOS_atk),
    
    ### BLOCKING ###
    blk_att = sum(skill == "Block"),
    blk_INS_att = sum(skill == "Block" & lag(attack_code,1) %in% INS_atk),
    blk_OOS_att = sum(skill == "Block" & lag(attack_code,1) %in% OOS_atk),
    blk_stuf_p = sum(skill == "Block" & evaluation_code == "#") / sum(skill == "Block"),
    blk_GT_p = sum(skill == "Block" & evaluation_code == "+") / sum(skill == "Block"),
    blk_ret_p = sum(skill == "Block" & evaluation_code == "!") / sum(skill == "Block"),
    blk_BT_p = sum(skill == "Block" & evaluation_code == "-") / sum(skill == "Block"),
    blk_err_p = sum(skill == "Block" & evaluation_code == "=") / sum(skill == "Block"),
    #MBO_p = sum(skill == "Block" & evaluation_code == "/") / sum(skill == "Block"), ADD IF CLEANED MATCHES
    blk_GBT = sum(skill == "Block" & evaluation_code %in% c("#", "+", "!")) / sum(skill == "Block"),
    blk_INSABE = ((sum(skill == "Block" & evaluation_code == "#" & lag(attack_code,1) %in% INS_atk) * b_PB) + 
                    (sum(skill == "Block" & evaluation_code == "+" & lag(attack_code,1) %in% INS_atk) * b_GB) + 
                    (sum(skill == "Block" & evaluation_code == "!" & lag(attack_code,1) %in% INS_atk) * b_MB) + 
                    (sum(skill == "Block" & evaluation_code == "-" & lag(attack_code,1) %in% INS_atk) * b_BB)) / 
      sum(skill == "Block" & lag(attack_code,1) %in% INS_atk), # - sum(skill == "Block" & evaluation_code == "/" & lag(attack_code,1) %in% INS_atk)), ADD IF CLEANED MATCHES
    blk_OOSABE = ((sum(skill == "Block" & evaluation_code == "#" & lag(attack_code,1) %in% OOS_atk) * b_PB) + 
                    (sum(skill == "Block" & evaluation_code == "+" & lag(attack_code,1) %in% OOS_atk) * b_GB) + 
                    (sum(skill == "Block" & evaluation_code == "!" & lag(attack_code,1) %in% OOS_atk) * b_MB) + 
                    (sum(skill == "Block" & evaluation_code == "-" & lag(attack_code,1) %in% OOS_atk) * b_BB)) / 
      sum(skill == "Block" & lag(attack_code,1) %in% OOS_atk), # - sum(skill == "Block" & evaluation_code == "/" & lag(attack_code,1) %in% OOS_atk)), ADD IF CLEANED MATCHES
    
    ### SERVING ###
    srv_att = sum(skill == "Serve"),
    srv_ace = sum(skill == "Serve" & evaluation_code == "#"),
    srv_err = sum(skill == "Serve" & evaluation_code == "="),
    srv_opass = sum(skill == "Serve" & evaluation_code == "/"),
    srv_gs = sum(skill == "Serve" & evaluation_code == "+"),
    srv_ms = sum(skill == "Serve" & evaluation_code == "!"),
    srv_bs = sum(skill == "Serve" & evaluation_code == "-"),
    srv_ppass = sum(skill == "Serve" & one_touch_future %in% c("Reception #")),
    srv_gpass = sum(skill == "Serve" & one_touch_future %in% c("Reception +")),
    srv_mpass = sum(skill == "Serve" & one_touch_future %in% c("Reception !")),
    srv_bpass = sum(skill == "Serve" & one_touch_future %in% c("Reception -")),
    srv_FBSO_att = sum(skill == "Serve" & three_touch_future %in% c("Attack #")),
    srv_ace_p = sum(skill == "Serve" & evaluation_code == "#") / sum(skill == "Serve"),
    srv_err_p = sum(skill == "Serve" & evaluation_code == "=") / sum(skill == "Serve"),
    srv_KO_p = (sum(skill == "Serve" & evaluation_code == "+") + sum(skill == "Serve" & evaluation_code == "/")) /
      (sum(skill == "Serve") - sum(skill == "Serve" & evaluation_code == "=")),
    srv_in_p = 1 - (sum(skill == "Serve" & evaluation_code == "=") / sum(skill == "Serve")),
    srv_FBSO = (sum(skill == "Serve" & three_touch_future %in% c("Attack #"))) / sum(skill == "Serve"),
    srv_xFBSO = (sum(skill == "Serve" & evaluation_code == "=") + 
                   (sum(skill == "Serve" & one_touch_future %in% c("Reception #")) * s_PP) +
                   (sum(skill == "Serve" & one_touch_future %in% c("Reception +")) * s_GP) +
                   (sum(skill == "Serve" & one_touch_future %in% c("Reception !")) * s_MP) +
                   (sum(skill == "Serve" & one_touch_future %in% c("Reception -")) * s_BP)) /
      sum(skill == "Serve"),
    
    ### PASSING ###
    pass_att = sum(skill == "Reception"),
    pass_PP = sum(skill == "Reception" & evaluation_code == "#"),
    pass_GP = sum(skill == "Reception" & evaluation_code == "+"),
    pass_MP = sum(skill == "Reception" & evaluation_code == "!"),
    pass_BP = sum(skill == "Reception" & evaluation_code == "-"),
    pass_err = sum(skill == "Reception" & evaluation_code == "="),
    pass_opass = sum(skill == "Reception" & evaluation_code == "/"),
    pass_z1_PP = sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(1,9)),
    pass_z6_PP = sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(6,8)),
    pass_z5_PP = sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(5,7)),
    pass_z1_err = sum(skill == "Reception" & evaluation_code == "=" & start_zone %in% c(1,9)),
    pass_z6_err = sum(skill == "Reception" & evaluation_code == "=" & start_zone %in% c(6,8)),
    pass_z5_err = sum(skill == "Reception" & evaluation_code == "=" & start_zone %in% c(5,7)),
    pass_z1_opass = sum(skill == "Reception" & evaluation_code == "/" & start_zone %in% c(1,9)),
    pass_z6_opass = sum(skill == "Reception" & evaluation_code == "/" & start_zone %in% c(6,8)),
    pass_z5_opass = sum(skill == "Reception" & evaluation_code == "/" & start_zone %in% c(5,7)),
    pass_z1_GP = sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(1,9)),
    pass_z6_GP = sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(6,8)),
    pass_z5_GP = sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(5,7)),
    pass_z1_MP = sum(skill == "Reception" & evaluation_code == "!" & start_zone %in% c(1,9)),
    pass_z6_MP = sum(skill == "Reception" & evaluation_code == "!" & start_zone %in% c(6,8)),
    pass_z5_MP = sum(skill == "Reception" & evaluation_code == "!" & start_zone %in% c(5,7)),
    pass_z1_BP = sum(skill == "Reception" & evaluation_code == "-" & start_zone %in% c(1,9)),
    pass_z6_BP = sum(skill == "Reception" & evaluation_code == "-" & start_zone %in% c(6,8)),
    pass_z5_BP = sum(skill == "Reception" & evaluation_code == "-" & start_zone %in% c(5,7)),
    pass_z1_att = sum(skill == "Reception" & start_zone %in% c(1,9)),
    pass_z6_att = sum(skill == "Reception" & start_zone %in% c(6,8)),
    pass_z5_att = sum(skill == "Reception" & start_zone %in% c(5,7)),
    pass_FBSO_att = sum(skill == "Reception" & two_touch_future %in% c("Attack #")),
    pass_PP_p = sum(skill == "Reception" & evaluation_code == "#") / sum(skill == "Reception"),
    pass_GP_p = sum(skill == "Reception" & evaluation_code == "+") / sum(skill == "Reception"),
    pass_MP_p = sum(skill == "Reception" & evaluation_code == "!") / sum(skill == "Reception"),
    pass_BP_p = sum(skill == "Reception" & evaluation_code == "-") / sum(skill == "Reception"),
    pass_EOP_p = (sum(skill == "Reception" & evaluation_code == "/") + sum(skill == "Reception" & evaluation_code == "=")) /
      sum(skill == "Reception"),
    pass_err_p = sum(skill == "Reception" & evaluation_code == "=") / sum(skill == "Reception"),
    pass_opass_p = sum(skill == "Reception" & evaluation_code == "/") / sum(skill == "Reception"),
    pass_GPPP_p = (sum(skill == "Reception" & evaluation_code == "#") + sum(skill == "Reception" & evaluation_code == "+")) /
      sum(skill == "Reception"),
    pass_xFBSO = ((sum(skill == "Reception" & evaluation_code == "#") * p_PP) +
                    (sum(skill == "Reception" & evaluation_code == "+") * p_GP) +
                    (sum(skill == "Reception" & evaluation_code == "!") * p_MP) +
                    (sum(skill == "Reception" & evaluation_code == "-") * p_BP)) /
      sum(skill == "Reception"),
    pass_z1_PP_p = sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(1,9)) /
      sum(skill == "Reception" & start_zone %in% c(1,9)),
    pass_z1_err_p = sum(skill == "Reception" & evaluation_code == "=" & start_zone %in% c(1,9)) /
      sum(skill == "Reception" & start_zone %in% c(1,9)),
    pass_z1_GPPP_p = (sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(1,9)) +
                        sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(1,9))) / 
      sum(skill == "Reception" & start_zone %in% c(1,9)),
    pass_z1_xFBSO = ((sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(1,9)) * p_PP) + 
                       (sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(1,9)) * p_GP) +
                       (sum(skill == "Reception" & evaluation_code == "!" & start_zone %in% c(1,9)) * p_MP) +
                       (sum(skill == "Reception" & evaluation_code == "-" & start_zone %in% c(1,9)) * p_BP)) /
      sum(skill == "Reception" & start_zone %in% c(1,9)),
    pass_z6_PP_p = sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(6,8)) /
      sum(skill == "Reception" & start_zone %in% c(6,8)),
    pass_z6_err_p = sum(skill == "Reception" & evaluation_code == "=" & start_zone %in% c(6,8)) /
      sum(skill == "Reception" & start_zone %in% c(6,8)),
    pass_z6_GPPP_p = (sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(6,8)) +
                        sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(6,8))) / 
      sum(skill == "Reception" & start_zone %in% c(6,8)),
    pass_z6_xFBSO = ((sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(6,8)) * p_PP) + 
                       (sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(6,8)) * p_GP) +
                       (sum(skill == "Reception" & evaluation_code == "!" & start_zone %in% c(6,8)) * p_MP) +
                       (sum(skill == "Reception" & evaluation_code == "-" & start_zone %in% c(6,8)) * p_BP)) /
      sum(skill == "Reception" & start_zone %in% c(6,8)),
    pass_z5_PP_p = sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(5,7)) /
      sum(skill == "Reception" & start_zone %in% c(5,7)),
    pass_z5_err_p = sum(skill == "Reception" & evaluation_code == "=" & start_zone %in% c(5,7)) /
      sum(skill == "Reception" & start_zone %in% c(5,7)),
    pass_z5_GPPP_p = (sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(5,7)) +
                        sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(5,7))) / 
      sum(skill == "Reception" & start_zone %in% c(5,7)),
    pass_z5_xFBSO = ((sum(skill == "Reception" & evaluation_code == "#" & start_zone %in% c(5,7)) * p_PP) + 
                       (sum(skill == "Reception" & evaluation_code == "+" & start_zone %in% c(5,7)) * p_GP) +
                       (sum(skill == "Reception" & evaluation_code == "!" & start_zone %in% c(5,7)) * p_MP) +
                       (sum(skill == "Reception" & evaluation_code == "-" & start_zone %in% c(5,7)) * p_BP)) /
      sum(skill == "Reception" & start_zone %in% c(5,7)),
    pass_FBSO = sum(skill == "Reception" & two_touch_future %in% c("Attack #")) / sum(skill == "Reception"),
    
    ### DIGGING ###
    dig_att = sum(skill == "Dig"),
    dig_p = sum(skill == "Dig" & evaluation_code %in% c("#", "+", "!", "-","/")) / sum(skill == "Dig"),
    dig_poss_p = sum(skill == "Dig" & evaluation_code %in% c("#", "+", "!")) / sum(skill == "Dig"),
    dig_INS_d_p = sum(skill == "Dig" & evaluation_code %in% c("#", "+")) / sum(skill == "Dig"),
    dig_OOS_d_p = sum(skill == "Dig" & evaluation_code %in% c("-", "/")) / sum(skill == "Dig"),
    dig_err_p = sum(skill == "Dig" & evaluation_code %in% c("=")) / sum(skill == "Dig"),
    dig_OOS_atk_p = sum(skill_type == "High ball dig" & evaluation_code %in% c("#", "+", "!", "-","/")) / sum(skill == "Dig"),
    dig_INS_atk_p = sum(skill_type %in% c("Head ball dig", "Quick ball dig", "Super ball dig", "Half ball dig", "Slide ball dig") & 
                          evaluation_code %in% c("#", "+", "!", "-","/")) / sum(skill == "Dig"),
    
    ### SETTING ###
    set_att = sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_INS_att = sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #")),
    set_MeS_att = sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +")),
    set_OOS_att = sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -")),
    set_fset_p = sum(skill == "Set" & set_type %in% c("F") & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_bset_p = sum(skill == "Set" & set_type %in% c("B") & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_cset_p = sum(skill == "Set" & set_type %in% c("C") & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_pset_p = sum(skill == "Set" & set_type %in% c("P") & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_INS_att_p = sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_MeS_att_p = sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_OOS_att_p = sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +","Reception !","Reception -", "Dig #", "Dig +", "Dig -")),
    set_INS_kill_p = sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #") & one_touch_future %in% c("Attack #")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #")),
    set_MeS_kill_p = sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +") & one_touch_future %in% c("Attack #")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +")),
    set_OOS_kill_p = sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -") & one_touch_future %in% c("Attack #")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -")),
    set_INS_err_p = sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #") & one_touch_future %in% c("Attack =")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #")),
    set_MeS_err_p = sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +") & one_touch_future %in% c("Attack =")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +")),
    set_OOS_err_p = sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -") & one_touch_future %in% c("Attack =")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -")),
    set_INS_blk_p = sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #") & one_touch_future %in% c("Attack /")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #")),
    set_MeS_blk_p = sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +") & one_touch_future %in% c("Attack /")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +")),
    OOS_blk_p = sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -") & one_touch_future %in% c("Attack /")) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -")),
    set_INS_eff = ((sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #") & sum(one_touch_future %in% c("Attack #")))) - sum(one_touch_future %in% c("Attack =", "Attack /"))) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception #", "Reception +", "Dig #")),
    set_MeS_eff = ((sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +") & sum(one_touch_future %in% c("Attack #")))) - sum(one_touch_future %in% c("Attack =", "Attack /"))) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception !", "Dig +")),
    set_OOS_eff = ((sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -") & sum(one_touch_future %in% c("Attack #")))) - sum(one_touch_future %in% c("Attack =", "Attack /"))) / 
      sum(skill == "Set" & one_touch_ago %in% c("Reception -", "Dig -")),
    .groups = "drop")