library(shiny)
library(shinydashboard)
library(tidyr)
library(fontawesome)
library(dplyr)
library(ggplot2)
library(datavolley)
library(gt)
library(bslib)
library(RColorBrewer)
library(ovlytics)
library(shinyjs)

#final <- load(file="final.rda")
#all_stats <- load(file="all_stats.rda")

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

jsCode <- "
shinyjs.getStorageItem = function(params) {
var key = params[0];
var value = localStorage.getItem(key);
Shiny.onInputChange('jsStorageItem', value);
}

shinyjs.setStorageItem = function(params) {
var key = params[0];
var value = params[1];
localStorage.setItem(key, value);
}
"

#######------------------------------------------------------------------#######

# Define UI for application
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "TAMU VOLLEYBALL"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "home", icon = icon(name = "home-user", class = NULL, lib="font-awesome")),
      menuItem("TEAM STATISTICS", tabName = "statistics", icon = icon(name = "chart-simple", class = NULL, lib="font-awesome")),
      menuItem("PLAYER ANALYSIS", tabName = "analysis", icon = icon(name = "person", class = NULL, lib="font-awesome")),
      menuItem("SCOUTING", tabName = "scouting", icon = icon(name = "chalkboard", class = NULL, lib="font-awesome")),
      menuItem("PLAYER PROFILES", tabName = "profiles", icon = icon(name = "address-card", class = NULL, lib="font-awesome"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("getStorageItem", "setStorageItem")),
    tabItems(
      # Home tab content
      tabItem(tabName = "home",
              h2("Home tab content")
    ),
    
    # Statistics tab content
      tabItem(tabName = "statistics",
            fluidRow(
              box(
                width = "400px", solidHeader = TRUE, status = "primary",
                selectInput("statsConference", "Select Conference:", sort(conferences)),
                selectInput("statsTeam", "Select Team", choices = NULL))
            ),
            fluidRow(
              tabBox(
                title = "TEAM STATS", id = "teamStats", height = "300px", width = "400px",
                tabPanel("OVERALL",
                         tableOutput(outputId = "teamOverallTable")),
                tabPanel("ATTACK",
                         tableOutput(outputId = "teamAttackTable")),
                tabPanel("BLOCK",
                         tableOutput(outputId = "teamBlockTable")),
                tabPanel("PASS",
                         tableOutput(outputId = "teamPassTable")),
                tabPanel("SERVE",
                         tableOutput(outputId = "teamServeTable")),
                tabPanel("DIG",
                         tableOutput(outputId = "teamDigTable")),
                tabPanel("SET",
                         tableOutput(outputId = "teamSetTable")),
              )
            )
    ), # End tabItem statistics
    
    # Player Analysis tab content
    tabItem(tabName = "analysis",
            fluidRow(
              box(
                width = "400px", solidHeader = TRUE, status = "primary",
                selectInput("playerConference", "Select Conference:", sort(conferences)),
                selectInput("playerTeam", "Select Team", choices = NULL),
                selectInput("playerPlayer", "Select Player", choices = NULL))
            ),
            fluidRow(
              tabBox(
                title = "PLAYER ANALYSIS", id = "playerStats", height = "700px", width = "300px",
                tabPanel("ATTACK",
                        tableOutput(outputId = "playerAttackTable"),
                        box(width = 3, plotOutput(outputId = "playerAttackAllPlot")),
                        box(width = 3, plotOutput(outputId = "playerAttackKillPlot")),
                        box(width = 3, plotOutput(outputId = "playerAttackBlockPlot")),
                        box(width = 3, plotOutput(outputId = "playerAttackErrorPlot"))),
                tabPanel("BLOCK",
                        tableOutput(outputId = "playerBlockTable"),
                        box(width = 3, plotOutput(outputId = "playerAllBlockPlot")),
                        box(width = 3, plotOutput(outputId = "playerStuffBlockPlot")),
                        box(width = 3, plotOutput(outputId = "playerGTBlockPlot")),
                        box(width = 3, plotOutput(outputId = "playerToolBlockPlot"))),
                tabPanel("PASS",
                         tableOutput(outputId = "playerPassTable"),
                         box(width = 3, plotOutput(outputId = "playerAllPassPlot")),
                         box(width = 3, plotOutput(outputId = "playerINSPassPlot")),
                         box(width = 3, plotOutput(outputId = "playerOOSPassPlot")),
                         box(width = 3, plotOutput(outputId = "playerEOPPassPlot"))),
                tabPanel("SERVE",
                         tableOutput(outputId = "playerServeTable"),
                         box(width = 3, plotOutput(outputId = "playerAllServePlot")),
                         box(width = 3, plotOutput(outputId = "playerAceServePlot")),
                         box(width = 3, plotOutput(outputId = "playerINSServePlot")),
                         box(width = 3, plotOutput(outputId = "playerOOSOPServePlot")),
                         box(width = 3, plotOutput(outputId = "playerErrServePlot"))),
                tabPanel("DIG",
                         tableOutput(outputId = "playerDigTable"),
                         box(width = 3, plotOutput(outputId = "playerAllDigPlot")),
                         box(width = 3, plotOutput(outputId = "playerINSDigPlot")),
                         box(width = 3, plotOutput(outputId = "playerOOSDigPlot")),
                         box(width = 3, plotOutput(outputId = "playerErrDigPlot"))),
                tabPanel("SET",
                         tableOutput(outputId = "playerSetTable"),
                         box(plotOutput(outputId = "playerAllSetPlot")),
                         box(plotOutput(outputId = "playerKillSetPlot")),
                         box(plotOutput(outputId = "playerBlockSetPlot")),
                         box(plotOutput(outputId = "playerErrSetPlot")))
              )
            )
    ),
    
    # Scouting tab content
      tabItem(tabName = "scouting",
            h2("Scouting tab content")
    ), # End tabItem scouting
    
    # Player Profile tab content
      tabItem(tabName = "profiles",
            fluidRow(
              box(width = "400px", solidHeader = TRUE, status = "primary",
                  selectInput("conference", "Select Conference:", sort(conferences)),
                  selectInput("team", "Select Team", choices = NULL),
                  selectInput("player", "Select Player", choices = NULL))
            ),
            fluidRow(
              tabBox(
                title = "CRAFT", id = "craft", height = "300px", width = "400px",
                tabPanel("Attack",
                         tableOutput(outputId = "attackTable"),
                         tableOutput(outputId = "attackGoalsTable")),
                tabPanel("Block",
                         tableOutput(outputId = "blockTable"),
                         tableOutput(outputId = "blockGoalsTable")),
                tabPanel("Pass",
                         tableOutput(outputId = "passTable"),
                         tableOutput(outputId = "passGoalsTable")),
                tabPanel("Serve",
                         tableOutput(outputId = "serveTable"),
                         tableOutput(outputId = "serveGoalsTable")),
                tabPanel("Dig",
                         tableOutput(outputId = "digTable"),
                         tableOutput(outputId = "digGoalsTable")),
                tabPanel("Set",
                         tableOutput(outputId = "setTable"),
                         tableOutput(outputId = "setGoalsTable")),
              )
            
              ),
            fluidRow(
              tabBox(
                title = "MIND", id = "mind", height = "300px", width = "400px",
                tabPanel("STRENGTH",
                         textInput(inputId = "mindStrength", label = "Strength", width = "100%", placeholder = "Enter 1 strength"),
                         verbatimTextOutput("displayStrengthText")),
                tabPanel("OPPORTUNITY",
                         textInput(inputId = "mindOpportunity", label = "Opportunity", width = "100%", placeholder = "Enter 1 opportunity"),
                         verbatimTextOutput("displayOpportunityText")),
                tabPanel("NOTES",
                         textInput(inputId = "mindNotes", label = "Mind Notes", width = "100%", placeholder = "Add Notes here"),
                         verbatimTextOutput("displayMindNotesText"))
              )
              
            ),
            fluidRow(
              tabBox(
                title = "BODY", id = "body", height = "300px", width = "400px",
                tabPanel("VERTICAL",
                         tableOutput(outputId = "vertRecentTable"),
                         tableOutput(outputId = "vertChangeTable"),
                         tableOutput(outputId = "vertGoalsTable")),
                tabPanel("PEAK PWR / WT",
                         tableOutput(outputId = "pwrRecentTable"),
                         tableOutput(outputId = "pwrChangeTable"),
                         tableOutput(outputId = "pwrGoalsTable")),
                tabPanel("NOTES",
                         textInput(inputId = "bodyNotes", label = "Body Notes", width = "100%", placeholder = "Add Notes here"),
                         verbatimTextOutput("displayBodyNotes"))
              )
              
            ),
            fluidRow(
              tabBox(
                title = "VOLLEYMETRICS WATCH LIST", id = "VM", height = "300px", width = "400px",
                tabPanel("What to watch",
                         textInput(inputId = "VM1", label = "1", width = "100%", placeholder = "What to watch?"),
                         verbatimTextOutput("displayVM1"),
                         textInput(inputId = "VM2", label = "2", width = "100%", placeholder = "What to watch?"),
                         verbatimTextOutput("displayVM2"),
                         textInput(inputId = "VMNotes", label = "Volleymetrics Notes", width = "100%", placeholder = "Add Notes here"),
                         verbatimTextOutput("displayVMNotes"))
              )
              
            ),
            fluidRow(
              tabBox(
                title = "MANTRA", id = "mantra", height = "300px", width = "400px"))
            
            ) # End tabItem profiles
    ) # End tabItems
) # End dashboardBody
) # End dashboardPage


# Define server logic
server <- function(input, output, session) {
  
#####------------------------ BEGINNING STATISTICS ------------------------#####
  observe({
    teams_in_selected_conf <- sort(unique(all_stats$team[all_stats$conference == input$statsConference]))
    updateSelectInput(session, "statsTeam", choices = teams_in_selected_conf)
  })
  
  output$teamOverallTable <- render_gt({
    team_filtered_stats <- all_stats %>%
      filter(team == input$statsTeam) %>%
      select(player_name, position, atk_att, atk_eff, atk_kill_p, atk_err_p, atk_blk_p, atk_INS_eff, atk_OOS_eff,
             srv_att, srv_xFBSO, srv_in_p, pass_att, pass_xFBSO, pass_GPPP_p, pass_EOP_p,
             blk_att, blk_GBT, blk_stuf_p, blk_INSABE, blk_OOSABE, dig_att, dig_INS_d_p, dig_poss_p) %>%
      gt() %>%
      cols_align("center") %>%
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) %>%
      opt_stylize(style = 6, color = "gray") %>%
      opt_table_font(font = system_fonts("industrial")) %>%
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) %>%
      tab_header(title = "Overall") %>%
      fmt_percent(columns = c(atk_eff, atk_kill_p, atk_err_p, atk_blk_p, atk_INS_eff, atk_OOS_eff,
                              srv_xFBSO, srv_in_p, pass_xFBSO, pass_GPPP_p, pass_EOP_p,
                              blk_GBT, blk_stuf_p, blk_INSABE, blk_OOSABE, dig_INS_d_p, dig_poss_p), decimals = 1) %>%
      cols_label(
        player_name = "Name",
        position = "Position",
        atk_att = "Atk Att",
        atk_eff = "Atk Eff",
        atk_kill_p = "INS Kill %",
        atk_blk_p = "INS Blk %",
        atk_err_p = "INS Err %",
        atk_INS_eff = "INS Eff",
        atk_OOS_eff = "OOS Eff",
        srv_att = "Srv Att",
        srv_xFBSO = "Srv xFBSO",
        srv_in_p = "Srv In %",
        pass_att = "Pass Att",
        pass_xFBSO = "Pass xFBSO",
        pass_GPPP_p = "Pass GP %",
        pass_EOP_p = "Pass EOP %",
        blk_att = "Blk Att",
        blk_GBT = "GT %",
        blk_stuf_p = "Stuf %",
        blk_INSABE = "INS A.B.E.",
        blk_OOSABE = "OOS A.B.E.",
        dig_att = "Dig Att",
        dig_INS_d_p = "INSAtk D %",
        dig_poss_p = "GD %")
      
  })
  
  output$teamAttackTable <- render_gt({
    team_filtered_stats <- all_stats %>% 
      filter(team == input$statsTeam) %>%
      filter(atk_att > 15) %>% 
      select(player_name, atk_INS_att, atk_INS_kill_p, atk_INS_blk_p, atk_INS_err_p, atk_INS_eff,
             atk_OOS_att, atk_OOS_kill_p, atk_OOS_blk_p, atk_OOS_err_p, atk_OOS_eff) %>%
      gt() %>% 
      cols_align("center") %>% 
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) %>%
      opt_stylize(style = 6, color = "gray") %>%
      opt_table_font(font = system_fonts("industrial")) %>%
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) %>%
      tab_header(title = "Attacking") %>%
      fmt_percent(columns = c(atk_INS_kill_p, atk_INS_blk_p, atk_INS_err_p, atk_INS_eff,
                              atk_OOS_kill_p, atk_OOS_blk_p, atk_OOS_err_p, atk_OOS_eff), decimals = 1) %>%
      cols_label(
        player_name = "Name",
        atk_INS_att = "INS Att",
        atk_INS_kill_p = "INS Kill %",
        atk_INS_blk_p = "INS Blk %",
        atk_INS_err_p = "INS Err %",
        atk_INS_eff = "INS Eff",
        atk_OOS_att = "OOS Att",
        atk_OOS_kill_p = "OOS Kill %",
        atk_OOS_blk_p = "OOS Blk %",
        atk_OOS_err_p = "OOS Err %",
        atk_OOS_eff = "OOS Eff")
  })
  
  output$teamBlockTable <- render_gt({
    team_filtered_stats <- all_stats %>% 
      filter(team == input$statsTeam) %>% 
      filter(blk_att > 15) %>% 
      select(player_name, blk_GBT, blk_INSABE, blk_OOSABE) %>%
      gt() %>% 
      cols_label(
        player_name = "Name",
        blk_GBT = "GT %",
        blk_INSABE = "INS A.B.E.",
        blk_OOSABE = "OOS A.B.E.") %>% 
      cols_align("center") %>% 
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) %>%
      opt_stylize(style = 6, color = "gray") %>%
      opt_table_font(font = system_fonts("industrial")) %>%
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) %>%
      tab_header(title = "Blocking") |>
      fmt_percent(columns = c(blk_GBT, blk_INSABE, blk_OOSABE), decimals = 1)
  })
  
  output$teamPassTable <- render_gt({
    team_filtered_stats <- all_stats %>% 
      filter(team == input$statsTeam) %>% 
      filter(pass_att > 15) %>% 
      select(player_name, pass_PP_p, pass_GP_p, pass_MP_p, pass_BP_p, pass_EOP_p) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Passing") |>
      fmt_percent(columns = c(pass_PP_p, pass_GP_p, pass_MP_p, pass_BP_p, pass_EOP_p), decimals = 1) |>
      cols_label(
        player_name = "Name",
        pass_PP_p = "PP %",
        pass_GP_p = "GP %",
        pass_MP_p = "MP %",
        pass_BP_p = "BP %",
        pass_EOP_p = "E&OP %")
    
  })
  
  output$teamServeTable <- render_gt({
    team_filtered_stats <- all_stats %>% 
      filter(team == input$statsTeam) %>% 
      filter(srv_att > 15) %>%
      select(player_name, srv_KO_p, srv_err_p) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Serving") |>
      fmt_percent(columns = c(srv_KO_p, srv_err_p), decimals = 1) |>
      cols_label(
        player_name = "Name",
        srv_KO_p = "KO %",
        srv_err_p = "Err %")
  })
  
  output$teamDigTable <- render_gt({
    team_filtered_stats <- all_stats %>% 
      filter(team == input$statsTeam) %>% 
      filter(dig_att > 15) %>% 
      select(player_name, dig_p, dig_poss_p) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Digging") |>
      fmt_percent(columns = c(dig_p, dig_poss_p), decimals = 1) |>
      cols_label(
        player_name = "Name",
        dig_p = "Dig %",
        dig_poss_p = "Poss %")
  })
  
  output$teamSetTable <- render_gt({
    team_filtered_stats <- all_stats %>% 
      filter(team == input$statsTeam) %>% 
      filter(set_INS_att > 30) %>%
      select(player_name, set_INS_att_p, set_MeS_att_p, set_OOS_att_p, set_INS_kill_p, set_INS_eff,
             set_MeS_kill_p, set_MeS_eff, set_OOS_kill_p, set_OOS_eff) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Setting") |>
      fmt_percent(columns = c(set_INS_att_p, set_MeS_att_p, set_OOS_att_p, set_INS_kill_p, set_INS_eff,
                              set_MeS_kill_p, set_MeS_eff, set_OOS_kill_p, set_OOS_eff), decimals = 1) |>
      cols_label(
        player_name = "Name",
        set_INS_att_p = "INS %",
        set_MeS_att_p = "MeS %",
        set_OOS_att_p = "OOS %",
        set_INS_kill_p = "INS Set Kill %",
        set_INS_eff = "INS Set Eff",
        set_MeS_kill_p = "MeS Set Kill %",
        set_MeS_eff = "MeS Set Eff",
        set_OOS_kill_p = "OOS Set Kill %",
        set_OOS_eff = "OOS Set Eff")
  })
#####--------------------------- END STATISTICS ---------------------------#####
  
##### -------------------- BEGINNGING PLAYER ANALYSIS ---------------------#####
  observe({
    teams_in_selected_conf <- sort(unique(all_stats$team[all_stats$conference == input$playerConference]))
    updateSelectInput(session, "playerTeam", choices = teams_in_selected_conf)
  })
  
  # Filter players based on selected team
  observe({
    players_in_selected_team <- sort(unique(all_stats$player_name[all_stats$team == input$playerTeam]))
    updateSelectInput(session, "playerPlayer", choices = players_in_selected_team)
  })
  
  # Create Attack Table
  output$playerAttackTable <-render_gt({
    filtered_atk_stats <- all_stats %>% 
                filter(team == input$playerTeam & player_name == input$playerPlayer) %>% 
    select(player_name, atk_INS_att, atk_INS_kill_p, atk_INS_blk_p, atk_INS_err_p, atk_INS_eff,
           atk_OOS_att, atk_OOS_kill_p, atk_OOS_blk_p, atk_OOS_err_p, atk_OOS_eff) %>% 
    gt() %>% 
    cols_align("center") %>% 
    opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) %>% 
    opt_stylize(style = 6, color = "gray") %>% 
    opt_table_font(font = system_fonts("industrial")) %>% 
    #data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) %>% 
    tab_header(title = "Attacking") %>% 
    fmt_percent(columns = c(atk_INS_kill_p, atk_INS_blk_p, atk_INS_err_p, atk_INS_eff,
                            atk_OOS_kill_p, atk_OOS_blk_p, atk_OOS_err_p, atk_OOS_eff), decimals = 1) %>% 
    cols_label(
      player_name = "Name",
      atk_INS_att = "INS Att",
      atk_INS_kill_p = "INS Kill %",
      atk_INS_blk_p = "INS Blk %",
      atk_INS_err_p = "INS Err %",
      atk_INS_eff = "INS Eff",
      atk_OOS_att = "OOS Att",
      atk_OOS_kill_p = "OOS Kill %",
      atk_OOS_blk_p = "OOS Blk %",
      atk_OOS_err_p = "OOS Err %",
      atk_OOS_eff = "OOS Eff")
})
  
  # Create Attack Plots
  create_attack_plot <- function(skill, eval_code, plot_title) {
    atk_filtered_data <- reactive({
      if (is.null(eval_code)) {
        final %>%
          filter(team == input$playerTeam &
                 player_name == input$playerPlayer &
                 skill == skill)
      } else {
        final %>%
          filter(team == input$playerTeam &
                 player_name == input$playerPlayer &
                 skill == skill &
                 evaluation_code == eval_code)
      }
      
    })
    
    atk_output_plot <- renderPlot({
      atk_hx <- ov_heatmap_kde(atk_filtered_data() %>%
                             select(end_coordinate_x, end_coordinate_y),
                           resolution = "coordinates", court = "upper")
      
      ggplot(atk_hx, aes(x, y, fill = density)) +
        scale_fill_distiller(palette = "Spectral", guide = "none") +
        geom_raster() +
        ggcourt(labels = NULL, court = "upper") +
        ggtitle(plot_title) +
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))
    })
    
    return(atk_output_plot)
  }
  
  output$playerAttackAllPlot <- create_attack_plot("Attack", NULL, "All Attacks")
  output$playerAttackKillPlot <- create_attack_plot("Attack", "#", "Kills")
  output$playerAttackBlockPlot <- create_attack_plot("Attack", "/", "Blocked")
  output$playerAttackErrorPlot <- create_attack_plot("Attack", "=", "Error")
  
  # Create Block Plots
  create_block_plot <- function(skill, eval_code, plot_title) {
    blk_filtered_data <- reactive({
      if (is.null(eval_code)) {
        final %>%
          filter(team == input$playerTeam &
                   player_name == input$playerPlayer &
                   skill == skill)
      } else {
        final %>%
          filter(team == input$playerTeam &
                   player_name == input$playerPlayer &
                   skill == skill &
                   evaluation_code == eval_code)
      }
      
    })
    
    blk_output_plot <- renderPlot({
      blk_hx <- ov_heatmap_kde(blk_filtered_data() %>%
                                 select(end_coordinate_x, end_coordinate_y),
                               resolution = "coordinates", court = "upper")
      
      ggplot(blk_hx, aes(x, y, fill = density)) +
        scale_fill_distiller(palette = "Spectral", guide = "none") +
        geom_raster() +
        ggcourt(labels = NULL, court = "upper") +
        ggtitle(plot_title) +
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))
    })
    
    return(blk_output_plot)
  }
  output$playerAllBlockPlot <- create_block_plot("Block", NULL, "All Touches")
  output$playerStuffBlockPlot <- create_block_plot("Block", "#", "Stuff Blocks")
  output$playerGTBlockPlot <- create_block_plot("Block", c("+","!"), "Good Touch")
  output$playerToolBlockPlot <- create_block_plot("Block", "=", "Tools")
  
  # Create Pass Plots
  create_pass_plot <- function(skill, eval_code, plot_title) {
    pass_filtered_data <- reactive({
      if (is.null(eval_code)) {
        final %>%
          filter(team == input$playerTeam &
                   player_name == input$playerPlayer &
                   skill == skill)
      } else {
        final %>%
          filter(team == input$playerTeam &
                   player_name == input$playerPlayer &
                   skill == skill &
                   evaluation_code == eval_code)
      }
      
    })
    
    pass_output_plot <- renderPlot({
      pass_hx <- ov_heatmap_kde(pass_filtered_data() %>%
                                 select(end_coordinate_x, end_coordinate_y),
                               resolution = "coordinates", court = "upper")
      
      ggplot(pass_hx, aes(x, y, fill = density)) +
        scale_fill_distiller(palette = "Spectral", guide = "none") +
        geom_raster() +
        ggcourt(labels = NULL, court = "upper") +
        ggtitle(plot_title) +
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))
    })
    
    return(pass_output_plot)
  }
  
  output$playerAllPassPlot <- create_pass_plot("Reception", NULL, "All Passes")
  output$playerINSPassPlot <- create_pass_plot("Reception", c("#","+"), "INS Pass")
  output$playerOOSPassPlot <- create_pass_plot("Reception", c("!","-"), "OOS Pass")
  output$playerEOPPassPlot <- create_pass_plot("Reception", c("=","/"), "Error")
  
  # Create Serve Plots
  create_serve_plot <- function(skill, eval_code, plot_title) {
    serve_filtered_data <- reactive({
      if (is.null(eval_code)) {
        final %>%
          filter(team == input$playerTeam &
                   player_name == input$playerPlayer &
                   skill == skill)
      } else {
        final %>%
          filter(team == input$playerTeam &
                   player_name == input$playerPlayer &
                   skill == skill &
                   evaluation_code == eval_code)
      }
      
    })
    
    serve_output_plot <- renderPlot({
      serve_hx <- ov_heatmap_kde(serve_filtered_data() %>%
                                  select(end_coordinate_x, end_coordinate_y),
                                resolution = "coordinates", court = "upper")
      
      ggplot(serve_hx, aes(x, y, fill = density)) +
        scale_fill_distiller(palette = "Spectral", guide = "none") +
        geom_raster() +
        ggcourt(labels = NULL, court = "upper") +
        ggtitle(plot_title) +
        theme(plot.title = element_text(size = 25, face = "bold", hjust = 0.5))
    })
    
    return(serve_output_plot)
  }
  output$playerAllServePlot <- create_serve_plot("Reception", NULL, "All Serves")
  output$playerAceServePlot <- create_serve_plot("Reception", "#", "Aces")
  output$playerINSServePlot <- create_serve_plot("Reception", c("-","!"), "INS Pass Serves")
  output$playerOOSOPServePlot <- create_serve_plot("Reception", c("+","/"), "OOS Pass Serves")
  output$playerErrServePlot <- create_serve_plot("Reception", "=", "Error")
  
##### ----------------------- END PLAYER ANALYSIS -------------------------#####
  
#####------------------------- BEGINNING PROFILES -------------------------#####
  observe({
    teams_in_selected_conf <- sort(unique(all_stats$team[all_stats$conference == input$conference]))
    updateSelectInput(session, "team", choices = teams_in_selected_conf)
  })
  
  # Filter players based on selected team
  observe({
    players_in_selected_team <- sort(unique(all_stats$player_name[all_stats$team == input$team]))
    updateSelectInput(session, "player", choices = players_in_selected_team)
  })

  
  # Store text_data for profiles
  text_data <- reactiveValues()

  # Update text_data when player_name changes - mindStrength
  observe({
    player <- input$player
    if (!is.null(player) && player != "" && !is.na(player)) {
      if (is.null(text_data[[player]])) {
        # Check to see if data is stored in browser local storage
        stored_data <- js$getStorageItem(c(player))
        if (!is.null(stored_data)) {
          text_data[[player]] <- stored_data
        } else {
          text_data[[player]] <- ""
        }
      }
    }
  })

  # Update stored text when user enters new text - mindStrength
  observeEvent(input$mindStrength, {
    player <- input$player
    if (!is.null(player) && player != "" && !is.na(player)) {
      text_data[[player]] <- input$mindStrength
      # Store data in local browser storage
      js$setStorageItem(c(player, input$mindStrength))
    }
  })

  # Display text for the selected player_name - mindStrength
  output$displayStrengthText <- renderText({
    player <- input$player
    if (!is.null(player) && player != "" && !is.na(player)) {
      text_data[[player]]
    }
  })
  
  # Create craftTable based on selected player's position
  output$attackTable <- render_gt({
    filtered_stats <- all_stats %>%
      filter(team == input$team, player_name == input$player) %>% 
      select(player_name, atk_INS_att, atk_INS_kill_p, atk_INS_blk_p, atk_INS_err_p, atk_INS_eff,
             atk_OOS_att, atk_OOS_kill_p, atk_OOS_blk_p, atk_OOS_err_p, atk_OOS_eff) %>% 
      gt() %>% 
      cols_align("center") %>% 
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) %>% 
      opt_stylize(style = 6, color = "gray") %>% 
      opt_table_font(font = system_fonts("industrial")) %>% 
      #data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) %>% 
      tab_header(title = "Attacking") %>% 
      fmt_percent(columns = c(atk_INS_kill_p, atk_INS_blk_p, atk_INS_err_p, atk_INS_eff,
                              atk_OOS_kill_p, atk_OOS_blk_p, atk_OOS_err_p, atk_OOS_eff), decimals = 1) %>% 
      cols_label(
        player_name = "Name",
        atk_INS_att = "INS Att",
        atk_INS_kill_p = "INS Kill %",
        atk_INS_blk_p = "INS Blk %",
        atk_INS_err_p = "INS Err %",
        atk_INS_eff = "INS Eff",
        atk_OOS_att = "OOS Att",
        atk_OOS_kill_p = "OOS Kill %",
        atk_OOS_blk_p = "OOS Blk %",
        atk_OOS_err_p = "OOS Err %",
        atk_OOS_eff = "OOS Eff")
  })
  
  output$blockTable <- render_gt({
    filtered_stats <- all_stats %>%
      filter(team == input$team, player_name == input$player) %>% 
      select(player_name, blk_GBT, blk_INSABE, blk_OOSABE) %>% 
      gt() %>% 
      cols_label(
        player_name = "Name",
        blk_GBT = "GT %",
        blk_INSABE = "INS A.B.E.",
        blk_OOSABE = "OOS A.B.E.") %>% 
      cols_align("center") %>% 
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) %>%
      opt_stylize(style = 6, color = "gray") %>%
      opt_table_font(font = system_fonts("industrial")) %>%
      # data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) %>%
      tab_header(title = "Blocking") |>
      fmt_percent(columns = c(blk_GBT, blk_INSABE, blk_OOSABE), decimals = 1)
  })
  
  output$passTable <- render_gt({
    filtered_stats <- all_stats %>%
      filter(team == input$team, player_name == input$player) %>% 
      select(player_name, pass_PP_p, pass_GP_p, pass_MP_p, pass_BP_p, pass_EOP_p) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      #data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Passing") |>
      fmt_percent(columns = c(pass_PP_p, pass_GP_p, pass_MP_p, pass_BP_p, pass_EOP_p), decimals = 1) |>
      cols_label(
        player_name = "Name",
        pass_PP_p = "PP %",
        pass_GP_p = "GP %",
        pass_MP_p = "MP %",
        pass_BP_p = "BP %",
        pass_EOP_p = "E&OP %")
  })
  
  output$serveTable <- render_gt({
    filtered_stats <- all_stats %>%
      filter(team == input$team, player_name == input$player) %>% 
      select(player_name, srv_KO_p, srv_err_p) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      #data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Serving") |>
      fmt_percent(columns = c(srv_KO_p, srv_err_p), decimals = 1) |>
      cols_label(
        player_name = "Name",
        srv_KO_p = "KO %",
        srv_err_p = "Err %")
  })
  
  output$digTable <- render_gt({
    filtered_stats <- all_stats %>%
      filter(team == input$team, player_name == input$player) %>% 
      select(player_name, dig_p, dig_poss_p) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      #data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Digging") |>
      fmt_percent(columns = c(dig_p, dig_poss_p), decimals = 1) |>
      cols_label(
        player_name = "Name",
        dig_p = "Dig %",
        dig_poss_p = "Poss %")
  })
  
  output$setTable <- render_gt({
    filtered_stats <- all_stats %>%
      filter(team == input$team, player_name == input$player) %>% 
      select(player_name, set_INS_att_p, set_MeS_att_p, set_OOS_att_p, set_INS_kill_p, set_INS_eff,
             set_MeS_kill_p, set_MeS_eff, set_OOS_kill_p, set_OOS_eff) %>% 
      gt() %>% 
      cols_align("center") |>
      opt_interactive(use_pagination_info = FALSE, use_compact_mode = TRUE) |>
      opt_stylize(style = 6, color = "gray") |>
      opt_table_font(font = system_fonts("industrial")) |>
      #data_color(method = "numeric", reverse = TRUE, palette = brewer.pal(11, name="Spectral")) |>
      tab_header(title = "Setting") |>
      fmt_percent(columns = c(set_INS_att_p, set_MeS_att_p, set_OOS_att_p, set_INS_kill_p, set_INS_eff,
                              set_MeS_kill_p, set_MeS_eff, set_OOS_kill_p, set_OOS_eff), decimals = 1) |>
      cols_label(
        player_name = "Name",
        set_INS_att_p = "INS %",
        set_MeS_att_p = "MeS %",
        set_OOS_att_p = "OOS %",
        set_INS_kill_p = "INS Set Kill %",
        set_INS_eff = "INS Set Eff",
        set_MeS_kill_p = "MeS Set Kill %",
        set_MeS_eff = "MeS Set Eff",
        set_OOS_kill_p = "OOS Set Kill %",
        set_OOS_eff = "OOS Set Eff")
  })
#####--------------------------- END PROFILES -----------------------------#####
  
} # End server
  

# Run the application 
shinyApp(ui, server)
