library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(reactable)
library(scales)
library(shinycssloaders)
library(rsconnect)

#print(getwd())
data_root <- ""


# enables table to work on dark mode
smart_table_theme <- reactableTheme(
  color = "var(--bs-body-color)",
  backgroundColor = "var(--bs-body-bg)",
  borderColor = "var(--bs-border-color)",
  stripedColor = "var(--bs-tertiary-bg)",
  highlightColor = "var(--bs-primary-bg-subtle)",
  cellPadding = "8px 12px",
  searchInputStyle = list(
    backgroundColor = "var(--bs-body-bg)",
    color = "var(--bs-body-color)",
    borderColor = "var(--bs-border-color)"
  ),
  selectStyle = list(
    backgroundColor = "var(--bs-body-bg)",
    color = "var(--bs-body-color)",
    borderColor = "var(--bs-border-color)"
  ),
  paginationStyle = list(
    backgroundColor = "var(--bs-body-bg)",
    color = "var(--bs-body-color)",
    borderColor = "var(--bs-border-color)"
  ),
  pageButtonHoverStyle = list(
    backgroundColor = "var(--bs-tertiary-bg)"
  ),
  inputStyle = list(
    backgroundColor = "var(--bs-body-bg)",
    color = "var(--bs-body-color)",
    borderColor = "var(--bs-border-color)"
  )
)

ui <- page_navbar(
  id = "main_tabs",
  title = "Division 1 CBB RAPM",
  fillable = c("Single-Season RAPM", "Multi-Year RAPM"),
  theme = bs_theme(bootswatch = "minty"),
  
  
  # tab 1 - home/about - explain rapm and give some math?
  nav_panel(
    title = "Home",
    layout_column_wrap(
      width = 1/2,
      #style = "margin-bottom: 30px;",
      actionButton(
        "go_single", 
        "Single-Season RAPM", 
        icon = icon("chart-simple"),
        class = "btn-lg btn-success", 
        style = "width: 100%; padding: 20px;"
      ),
      actionButton(
        "go_multi", 
        "Multi-Year RAPM", 
        icon = icon("diagram-project"),
        class = "btn-lg btn-info",
        style = "width: 100%; padding: 20px;"
      )
    ),
    card(
      full_screen = TRUE,
      fill= FALSE,
      card_header(tags$strong("What is RAPM?")),
      markdown("**Regularized Adjusted Plus-Minus (RAPM)** is one of the most widely used metrics for evaluating basketball impact. 
                      It is a form of Adjusted Plus-Minus (APM), which measures player impact on a team's point differential per 100 possessions. 
                      Unlike standard box-score stats, RAPM accounts for the quality of teammates and opponents on the floor.
                      Further technical details behind how RAPM is calculated are discussed in ‘The Math Behind RAPM’ below."),
                      
                      
     markdown("RAPM is popular and readily available for the NBA (some NBA RAPM resources are linked further down on this page), 
                               but it is much less accessible for college basketball—particularly for the women's game. 
                               Reasons for this gap include the sheer number of college basketball teams and players, 
                               and the messy, less available play-by-play data compared to the resources the NBA provides 
                               to work with its play-by-play data.")
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "The Math Behind RAPM",
        withMathJax(),
        markdown("To calculate APM, we transform raw play-by-play data into an N by M*2 matrix (X) and a vector of size N (y), 
        where N is the number of possessions, and M is the number of players in our data. 
        The only values in our X matrix are 0 and 1, where 0 means a player is off the floor and 1 means they were on the floor for that possession. 
        We multiply M by 2 to get an offensive coefficient and a defensive coefficient for each player. 
        The y vector represents the points scored on the corresponding possession. 
        Row i in the X matrix corresponds to element i in the y vector. "),
        
        markdown("While the matrix itself is extremely large, each possession will only have 10 non-zero elements. 
        For example, for North Carolina’s final offensive possession in their 71-68 win over Duke on February 7th, 2026, the following columns of the X matrix would be 1: 
        O_Seth Trimble, O_Derek Dixon, O_Caleb Wilson, O_Henri Veesaar, O_Jarin Stevenson, D_Cameron Boozer, D_Maliq Brown, D_Dame Sarr, D_Caleb Foster, and D_Isaiah Evans. 
        For that row, the rest of the columns would be 0. 
        The corresponding element in the y vector would be 3."),
        
        markdown("To make the metric more intuitive, I changed the defensive column values from 1 to -1. 
        This will make both a positive ORAPM and DRAPM good and a negative value bad. 
        From here, we can use a linear regression model to solve for the coefficients, and then multiply the coefficients by 100 since we are interested in the impact per 100 possessions. 
                 The closed-form solution for an ordinary least squares (OLS) regression is as follows: "),
        "$$\\hat{\\beta}_{OLS} = (X^T X)^{-1} X^T y$$",
        
        markdown("The ‘R’ in RAPM stands for regularization. 
        One problem with APM is multicollinearity, or many of our predictors are correlated with each other. 
        The reason for this is obvious: players on the same team play many of their minutes with the same teammates. 
        This gives us noisy and inaccurate results, because APM numbers for lesser players might be inflated due to having their minutes tied to star players. 
        To fix this, we use regularization — we introduce a penalty that pushes coefficients closer to 0. 
        This helps us better identify which players are the true drivers of a certain lineup’s success. 
        We introduce a parameter lambda, which is our penalty.
        The ridge regression closed-form solution is defined as:"),
        "$$\\hat{\\beta}_{ridge} = (X^T X + \\lambda I)^{-1} X^T y$$",
        
        markdown("Our RAPM metric starts with the assumption that all players have the same level of impact. 
                 This is obviously untrue, and we can seek to change this assumption to improve our estimate of RAPM. 
                 RAPM takes many possessions to stabilize, so introducing a prior assumption can help our RAPM estimate converge faster and be closer to the true impact of a player. 
                 The prior I used for my creation of RAPM was Basketball Reference’s BPM. 
                 There are multiple ways to incorporate this prior into the calculation, but the way I did it was to append M*2 rows to the X matrix and the y vector. 
                 Each of these appended rows had just one column that was 1 (O_Player  or D_Player, each player had a prior for their ORAPM and DRAPM separately). 
                 The corresponding element in y was a player’s OBPM/DBPM, scaled by a value that depended on the number of minutes played he/she played. 
                 If a player barely played, we want their prior to be very close to 0, since we don’t have a good guess on how impactful they are. "),
        
        markdown("Both the raw RAPM values and BPM-Prior RAPM values for each player are available in this app.")
      ),
      accordion_panel(
        "Limitations & Caveats",
        markdown("A limitation of RAPM is that it requires a ton of possessions to stabilize, preferably multiple seasons. 
                 This is particularly limiting in college; in addition to teams playing significantly fewer (shorter) games than NBA teams, many of the best players only play one or two seasons. 
                 This leads to potentially noisy and unreliable results, and there is a fair argument that the majority of college players will never play enough possessions for the RAPM estimate to ever be reliable enough. 
                 In this app, I’m displaying single-season raw RAPM, even though I’m voicing my concerns over the potentially unreliable nature of single-season RAPM in general, and how this problem can be even worse in college."),
        
        markdown("Despite my concerns, I think there is some value that can be taken from even the raw RAPM data. 
                 For the highest-minute players, RAPM seems to correctly identify the best players in the sport. 
                 Zach Edey, Cooper Flagg, Cameron Boozer, Caitlin Clark, Paige Bueckers, and Sarah Strong all rank very highly in single-season RAPM data, as expected. 
                 While it’s best used to evaluate these high-minute players, I think it’s worth exploring its accuracy for lower-minute, higher-RAPM players. 
                 In 2023-24, Arizona’s MBB team was led by 3rd-team All-American Caleb Love, two other future NBA players in Pelle Larsson and Keshad Johnson, and productive college players in Kylan Boswell and Oumar Ballo, earning a two-seed in the West Region. 
                 Despite not starting a single game and playing the 6th most minutes on the team, reserve guard Jaden Bradley had the highest RAPM on the team. 
                 Two years later, he was named the Big 12 Player of the Year. Could it be a coincidence? For sure. 
                 However, I do think it’s worth looking at as an initial starting point for player evaluation when considering transfer portal options and potential future star-leaps."),
        
        markdown("As discussed previously, single-season RAPM can be extremely noisy. 
        Results may be unreliable, and this is especially true for teams that have lost star players for significant amounts of time. 
                 In 2025-26, Caleb Wilson, JT Toppin, and Darryn Peterson all missed sizeable chunks of the season with various injuries. 
                 The raw RAPM numbers for these players are likely lower than they should be, and it’s important to keep in mind that estimates for their teammates are less accurate."),
        
        markdown("While multi-year RAPM is more accurate and reliable, in the college case, we also have to remember that the careers for college basketball’s best players are much different from those in the NBA. 
        Some players, like the aforementioned Flagg and Boozer, play just one season, while others, like Braden Smith and Kam Jones, star on quality teams for lengthy college careers. 
        Other stars have just one or two star-level impact years due to the combination of a late-breakout and an early-declare, such as Jeremy Fears Jr. In the NBA, this isn’t much of a problem. 
        Comparing Shai Gilgeous-Alexander, Nikola Jokic, and Giannis Antetokounmpo is more straightforward because all three have thousands of minutes logged as a superstar-level player, which isn’t the case in college. 
                 This is important to keep in mind when comparing the college RAPM values for these players.")
      ),
      accordion_panel(
        "Further RAPM Details",
        tags$a("Adjusted Plus-Minus, Explained: The Stat That Drives Modern Basketball", href = "https://www.roycewebb.com/p/adjusted-plus-minus-explained", target = "_blank"),
        tags$br(),
        tags$br(),
        tags$a("Adjusted Plus-Minus (APM)", href = "https://www.nbastuffer.com/analytics101/adjusted-plus-minus/", target = "_blank"),
        tags$br(),
        tags$br(),
        tags$a("Regularized Adjusted Plus Minus (xRAPM)", href = "https://www.nbastuffer.com/analytics101/regularized-adjusted-plus-minus-rapm/", target = "_blank"),
        tags$br(),
        tags$br(),
        tags$a("Regularized Adjusted Plus/Minus (RAPM)", href = "https://basketballstat.home.blog/2019/08/14/regularized-adjusted-plus-minus-rapm/", target = "_blank")
        
      ),
      accordion_panel(
        "Other RAPM Resources",
        tags$h5("NBA"),
        tags$a("xRAPM - Jeremias Engelmann", href = "https://xrapm.com/table_pages/xRAPM.html", target = "_blank"),
        tags$br(),
        tags$br(),
        tags$a("nbarapm.com", href = "https://www.nbarapm.com/", target = "_blank"),
        tags$br(),
        tags$br(),
        tags$h5("College"),
        tags$a("Hoop Explorer", href = "https://hoop-explorer.com/PlayerLeaderboard?gender=Men&tier=High&year=2025%2F26&", target = "_blank"),
      ),
      accordion_panel(
        "Notes",
        markdown("- In some seasons, players share the same name, such as Madison Greene (Ohio State and Vanderbilt) and Kobe Johnson (St. Louis and UCLA). Currently, these players are excluded from the dataset. "),
        
        markdown("- For BPM-Prior RAPM, name mismatches occur between the NCAA website and Basketball Reference. I have identified several, but may have missed others. 
                 Unmatched players are assigned a prior of 0. As I find more discrepancies, I will fix them."),
        
        tags$p(
          "For any questions or bugs, you can email me ",
          tags$a(
            "here", 
            href = "mailto:shanetyler2005@gmail.com?subject=CBB RAPM"
          ),
          "."
        )
      ),
      accordion_panel(
        "Data Sources",
        tags$a("College Basketball Reference", href = "https://www.sports-reference.com/cbb/", target = "_blank"),
        tags$br(),
        tags$br(),
        tags$a("ncaa.org", href = "https://stats.ncaa.org/", target = "_blank")
      )
    )
  ),
  
  # tab 2 -  single season rapm
  nav_panel(
    title = "Single-Season RAPM",

    layout_sidebar(
      fill = TRUE,
      sidebar = sidebar(
                        radioButtons("sport_single", "Sport:", choices = c("Men's", "Women's"), selected = "Men's", inline = TRUE),
                        selectInput("season_single", "Season:", choices = c("23-24", "24-25", "25-26"), multiple = FALSE, selected = "23-24"),
                        
                        
                        pickerInput(
                          inputId = "team_single", 
                          label = "Team: ", 
                          choices = NULL,         # do this in the server
                          multiple = TRUE,
                          options = pickerOptions(
                            actionsBox = TRUE,
                            selectAllText = "All",
                            deselectAllText = "None",
                            liveSearch = TRUE,
                            noneSelectedText = "All teams selected"
                          )
                        ),
                        prettySwitch(
                          "bayesian_toggle_single",
                          label = "Bayesian",
                          value = FALSE,
                          fill = TRUE,
                          status = "primary"
                        ),
                        
                        actionButton("simulate_btn_single", "Load Data", style = "color: white; background-color: #0072B2; border-color: #005b96;")
                      
                        ),
      withSpinner(
        reactableOutput("raw_table"),
        type = 6,              # There are 8 different spinner types
        color = "#0072B2"      
      )
    )
  ),
  
  # multi year rapm
  nav_panel(
    title = "Multi-Year RAPM",
    fill = TRUE,
    layout_sidebar(
      sidebar = sidebar(
                        radioButtons("sport_multi", "Sport:", choices = c("Men's", "Women's"), selected = "Men's", inline = TRUE),
                        
                        prettySwitch(
                          "bayesian_toggle",
                          label = "Bayesian",
                          value = FALSE,
                          fill = TRUE,
                          status = "primary"
                        ),
                        
                        actionButton("simulate_btn_multi", "Load Data", style = "color: white; background-color: #0072B2; border-color: #005b96;")
      ),
      withSpinner(
        reactableOutput("multi_table"),
        type = 6,              # There are 8 different spinner types
        color = "#0072B2"      
      )
    )
  ),
  
  nav_spacer(), # pushes everything to the right
  
  nav_item(
    input_dark_mode(id = "dark_mode")
  ),
  
  # uncomment if i want my name in the top right
 # nav_item(
 #   tags$span(
 #     "Created by: Shane Faberman", 
  #    style = "margin-right: 9px; vertical-align: middle; color: #666;"
 #   )
#
)

server <- function(input, output, session) {
  
  ### Buttons on home page
  
  # go to single season rapm
  observeEvent(input$go_single, {
    nav_select("main_tabs", selected = "Single-Season RAPM")
  })
  
  
  # go to multi year rapm
  observeEvent(input$go_multi, {
    nav_select("main_tabs", selected = "Multi-Year RAPM")
  })
  
  
  ### Single Season RAPM
  
  ## team filters
  
  observe({
    if (input$sport_single == "Men's") {
      sport_path = "MBB/"
    }
    else {
      sport_path = "WBB/"
    }
    # Load the data briefly just to get the unique team names
    temp_df <- read_csv(paste0(data_root, sport_path,input$season_single, "/no_prior_rapm.csv")) 
    team_list <- sort(unique(temp_df$team))
    
    updatePickerInput(session, "team_single", choices = team_list)
  })
  
  # event triggers on button click
  # get correct dataframe
  filtered_data <- eventReactive(input$simulate_btn_single, {
    
    if (input$sport_single == "Men's") {
      sport_path = "MBB/"
    }
    else {
      sport_path = "WBB/"
    }
    
    if (input$bayesian_toggle_single) {
      csv_type = "/prior_rapm.csv" 
    } 
    else {
      csv_type = "/no_prior_rapm.csv"
    }
    
      
      # full path for data
      path <- paste0(data_root, sport_path, input$season_single,csv_type)
      
      
      # load and filter data
      df <- read_csv(path) %>% 
        rename(
          Player = player,
          `Total RAPM` = Total_RAPM,
          Team = `team`, # change later
          id = `...1`
        )
      
      exclude <- df %>% 
        count(Player) %>% 
        filter(n > 1) %>% 
        pull(Player)
      
      df <- df %>% filter(!(Player %in% exclude))
        
      
      global_bounds <- list(
        total_max = max(df$`Total RAPM`, na.rm = TRUE),
        total_min = min(df$`Total RAPM`, na.rm = TRUE),
        o_max = max(df$ORAPM, na.rm = TRUE),
        o_min = min(df$ORAPM, na.rm = TRUE),
        d_max = max(df$DRAPM, na.rm = TRUE),
        d_min = min(df$DRAPM, na.rm = TRUE)
      )

      
      # filter teams
      if (!is.null(input$team_single)) {
        df2 <- df %>% filter(Team %in% input$team_single) %>% relocate(Player, ORAPM, DRAPM)
        #print(names(df2))
      } else(
        df2 <- df %>% relocate(Player, ORAPM, DRAPM)
      )
  
      return(list(data=df2, bounds=global_bounds, non_filtered_data=df))
  })
    
    
  ### Table on Raw RAPM page
    
    output$raw_table <- renderReactable({
      
      req(filtered_data())
      df <- filtered_data()$data
      bounds <- filtered_data()$bounds
      df2 <- filtered_data()$non_filtered_data
      
      total_rapm <- col_numeric(
        palette = c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
        domain = c(bounds$total_min, bounds$total_max) 
      )
      
      drapm <- col_numeric(
        palette = c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
        domain = c(bounds$d_min, bounds$d_max) 
      )
      
      orapm <- col_numeric(
        palette = c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
        domain = c(bounds$o_min, bounds$o_max) 
      )
      
      
      reactable(df,
                searchable = TRUE,
                theme = smart_table_theme,
                showPageSizeOptions = TRUE,
                highlight = TRUE,
                showPageInfo = FALSE,
                striped = TRUE,
                paginationType = "jump",
                defaultSortOrder = "desc",
                defaultSorted = c("Total RAPM"),
                columns = list(
                  ORAPM = colDef(filterable = FALSE,
                                 style = function(value) {
                                   # make color visible on background
                                   color <- if (between(value, quantile(df2$`ORAPM`, .025), quantile(df2$`ORAPM`, .9975))) {
                                     "#1A1A1A" # dark on light
                                   } else {
                                     "#FFFFFF" # light on dark
                                   }
                                   list(background = orapm(value), color = color)
                                 }),
                  DRAPM = colDef(filterable = FALSE,
                                 style = function(value) {
                                   
                                   color <- if (between(value, quantile(df2$`DRAPM`, .025), quantile(df2$`DRAPM`, .9975))) {
                                     "#1A1A1A" # dark on light
                                   } else {
                                     "#FFFFFF" # light on dark
                                   }
                                   
                                   list(background = drapm(value), color = color)
                                 }),
                  `Total RAPM` = colDef(filterable = FALSE,
                                        style = function(value) {
                                          
                                          color <- if (between(value, quantile(df2$`Total RAPM`, .025), quantile(df2$`Total RAPM`, .9975))) {
                                            "#1A1A1A" # dark on light
                                          } else {
                                            "#FFFFFF" # light on dark
                                          }                                         
                                          
                                          list(background = total_rapm(value), color = color)
                                        }),
                  Player = colDef(defaultSortOrder = "asc",
                                  cell = function(value, index) {
                                    Team <- df$Team[index]
                                    div(
                                      div(style = list(fontWeight = 600), value),
                                      div(class = "text-muted", 
                                          style = list(fontSize = "0.75rem"),
                                          Team)
                                    )
                                  }),
                  Team = colDef(show = FALSE),
                  id = colDef(show = FALSE)
                ),
                defaultPageSize = 25,
                pageSizeOptions = c(10, 25, 50, 100))
      
    })
  
    
    
    
 
    ### Multi-Year RAPM
    
    # event triggers on button click
    # get correct dataframe
    filtered_data_multi <- eventReactive(input$simulate_btn_multi, {
      
      if (input$sport_multi == "Men's") {
        sport_path = "MBB/"
      }
      else {
        sport_path = "WBB/"
      }
      
      if (input$bayesian_toggle) {
        csv_type = "/multi_season_prior_rapm.csv" 
      } 
      else {
        csv_type = "/multi_season_no_prior_rapm.csv"
      }
      
      
      # full path for data
      path <- paste0(data_root, sport_path, csv_type)
      
      
      # load and filter data
      df <- read_csv(path) %>% 
        rename(
          Player = player,
          `Total RAPM` = Total_RAPM,
          id = `...1`
        )
      
      exclude <- df %>% 
        count(Player) %>% 
        filter(n > 1) %>% 
        pull(Player)
      
      df <- df %>% filter(!(Player %in% exclude)) %>% relocate(Player, ORAPM, DRAPM)
      
      
      global_bounds <- list(
        total_max = max(df$`Total RAPM`, na.rm = TRUE),
        total_min = min(df$`Total RAPM`, na.rm = TRUE),
        o_max = max(df$ORAPM, na.rm = TRUE),
        o_min = min(df$ORAPM, na.rm = TRUE),
        d_max = max(df$DRAPM, na.rm = TRUE),
        d_min = min(df$DRAPM, na.rm = TRUE)
      )
      

      
      return(list(data=df, bounds=global_bounds))
    })
    
    ### Table on Multi-Year RAPM page
    
    output$multi_table <- renderReactable({
      
      req(filtered_data_multi())
      df <- filtered_data_multi()$data
      bounds <- filtered_data_multi()$bounds
      
      total_rapm <- col_numeric(
        palette = c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
        domain = c(bounds$total_min, bounds$total_max) 
      )
      
      drapm <- col_numeric(
        palette = c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
        domain = c(bounds$d_min, bounds$d_max) 
      )
      
      orapm <- col_numeric(
        palette = c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
        domain = c(bounds$o_min, bounds$o_max) 
      )
      
      
      reactable(df,
                searchable = TRUE,
                theme = smart_table_theme,
                showPageSizeOptions = TRUE,
                highlight = TRUE,
                showPageInfo = FALSE,
                striped = TRUE,
                paginationType = "jump",
                defaultSortOrder = "desc",
                defaultSorted = c("Total RAPM"),
                columns = list(
                  ORAPM = colDef(filterable = FALSE,
                                 style = function(value) {
                                   # make color visible on background
                                   color <- if (between(value, quantile(df$`ORAPM`, .025), quantile(df$`ORAPM`, .9975))) {
                                     "#1A1A1A" # dark on light
                                   } else {
                                     "#FFFFFF" # light on dark
                                   }
                                   list(background = orapm(value), color = color)
                                 }),
                  DRAPM = colDef(filterable = FALSE,
                                 style = function(value) {
                                   
                                   color <- if (between(value, quantile(df$`DRAPM`, .025), quantile(df$`DRAPM`, .9975))) {
                                     "#1A1A1A" # dark on light
                                   } else {
                                     "#FFFFFF" # light on dark
                                   }
                                   
                                   list(background = drapm(value), color = color)
                                 }),
                  `Total RAPM` = colDef(filterable = FALSE,
                                        style = function(value) {
                                          
                                          color <- if (between(value, quantile(df$`Total RAPM`, .025), quantile(df$`Total RAPM`, .9975))) {
                                            "#1A1A1A" # dark on light
                                          } else {
                                            "#FFFFFF" # light on dark
                                          }                                         
                                          
                                          list(background = total_rapm(value), color = color)
                                        }),
                  Player = colDef(defaultSortOrder = "asc",
                                  cell = function(value) {
                                    div(
                                      div(style = list(fontWeight = 600), value))
                                  }),
                  id = colDef(show = FALSE)
                ),
                defaultPageSize = 25,
                pageSizeOptions = c(10, 25, 50, 100))
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

#df <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/WBB/23-24/no_prior_rapm.csv") %>% 
#  relocate(player)

