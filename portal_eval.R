library(tidyverse)
library(gt)
library(cbbplotR)
library(gtExtras)
library(stringi)

portal_2025 <- read_csv("/Users/shanefaberman/transfer_portal/players_full_2025.csv") %>% 
  mutate(player_name = paste0(player.firstName, " ",player.lastName)) %>% 
  select(player_name, player_transfer_rating = player.transferRating, pos = player.position, 
         player.transfer.source.institution) %>% 
  distinct(player_name,.keep_all = T) %>% 
  arrange(-player_transfer_rating) %>% 
  mutate(rank_transfer = row_number(-player_transfer_rating))

portal_2024 <- read_csv("/Users/shanefaberman/transfer_portal/players_full_2024.csv") %>% 
  mutate(player_name = paste0(player.firstName, " ",player.lastName)) %>% 
  select(player_name, player_transfer_rating = player.transferRating, pos = player.position, 
         player.transfer.source.institution) %>% 
  distinct(player_name,.keep_all = T) %>% 
  arrange(-player_transfer_rating) %>% 
  mutate(rank_transfer = row_number(-player_transfer_rating))

prior_rapm_24 <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/MBB/23-24/prior_rapm.csv") %>% 
  select(player, Total_RAPM)

players_to_exclude_24 <- prior_rapm_24 %>% 
  count(player) %>% 
  filter(n > 1) %>% 
  pull(player)

portal_2024_2 <- portal_2024 %>% 
  left_join(prior_rapm_24 %>% filter(!(player %in% players_to_exclude_24)), join_by(player_name == player)) %>% 
  mutate(rank_RAPM_24 = row_number(-Total_RAPM)) %>% 
  select(player_name,
         rank_transfer_24 = rank_transfer,
         Total_RAPM_24 = Total_RAPM, 
         rank_RAPM_24)
  

# get rid of all periods (like v.j.)
portal_2025$player_name <- portal_2025$player_name %>%  str_remove_all("\\.")

# get rid of jr, III, IV, etc.
suffix_regex <- "\\s+([jJ][rR]|[iI]{2,3}|[iI][vV])\\.?$" # get rid of Jr, III, IV, etc
portal_2025$player_name <- portal_2025$player_name %>%
  str_remove(suffix_regex) %>%
  str_trim()

# get rid of accents on player names
portal_2025$player_name <- stri_trans_general(portal_2025$player_name, "Latin-ASCII")

no_prior_rapm_25 <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/MBB/24-25/no_prior_rapm.csv") %>% 
  select(player, Total_RAPM)

prior_rapm_25 <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/MBB/24-25/prior_rapm.csv") %>% 
  select(player, Total_RAPM)

players_to_exclude <- no_prior_rapm_25 %>% 
  count(player) %>% 
  filter(n > 1) %>% 
  pull(player)


players_to_exclude_prior <- prior_rapm_25 %>% 
  count(player) %>% 
  filter(n > 1) %>% 
  pull(player)

portal_2025_2 <- portal_2025 %>% 
  mutate(player_name = ifelse(player_name == "Rob Wright", "Robert Wright", player_name)) %>% 
  left_join(no_prior_rapm_25 %>% filter(!(player %in% players_to_exclude)), join_by(player_name == player)) %>% 
  mutate(rank_rapm = row_number(-Total_RAPM))

portal_2025_3 <- portal_2025 %>% 
  mutate(player_name = ifelse(player_name == "Rob Wright", "Robert Wright", player_name)) %>% 
  left_join(prior_rapm_25 %>% filter(!(player %in% players_to_exclude)), join_by(player_name == player)) %>% 
  mutate(rank_rapm = row_number(-Total_RAPM)) %>% 
  mutate(rank_change = rank_transfer - rank_rapm)

rapm_rank_domain <- c(min(as.numeric(portal_2025_3$rank_change), na.rm = TRUE), 
                               max(as.numeric(portal_2025_3$rank_change), na.rm = TRUE))

# visually better for negative values, so it can't be blue
rapm_rank_domain_negative <- c(min(as.numeric(portal_2025_3$rank_change), na.rm = TRUE), 
                      -min(as.numeric(portal_2025_3$rank_change), na.rm = TRUE))

good_portal <- tibble(
  Player = c("Henri Veesaar", "Pryce Sandfort", "Bennett Stirtz", "Dillon Mitchell", "Donovan Atwell",
             "Ja'Kobi Gillespie", "Duke Miles", "Bryce Lindsay", "Aday Mara"),
  `Transferred From` = c("Arizona", "Iowa", "Drake", "Cincinnati", "UNC Greensboro",
                         "Maryland", "Oklahoma", "James Madison", "UCLA"),
  `Transferred To` = c("North Carolina", "Nebraska", "Iowa", "St. John's", "Texas Tech",
                       "Tennessee", "Vanderbilt", "Villanova", "Michigan"),
  `247 Transfer Portal Rank` = c("36", "187", "12", "42", "170", "16", "202", "277", "49"),
  `Bayesian RAPM Rank` = c("1", "23", "3", "10", "98", "4", "78", "174", "27"),
  `Rank Difference` = c(35, 164, 9, 32, 72, 12, 124, 103, 22),
  `Result` = c("All-ACC 2nd team", "All-B10 1st team", "All-B10 2nd team", "All-BE 3rd team","All-B12 HM",
               "All-SEC 1st team", "16th in PPG, 5th in APG, 1st in SPG in SEC",
               "3rd in 3PTFG/G in BE", "B10 DPOY, NCAA All-Tournament Team")
)

tbl <- good_portal %>% 
  mutate(
    `247 Transfer Portal Rank` = as.numeric(`247 Transfer Portal Rank`),
    `Bayesian RAPM Rank` = as.numeric(`Bayesian RAPM Rank`)
  ) %>% 
  gt_cbb_teams(`Transferred From`, `Transferred From`, logo_color = 'normal') %>% 
  gt_cbb_teams(`Transferred To`, `Transferred To`, logo_color = 'normal') %>% 
  gt(id = "good portal") %>% 
  gt_theme_guardian() %>% 
  fmt_markdown(`Transferred From`) %>% 
  fmt_markdown(`Transferred To`) %>% 
  tab_header(
    title = md("**Correctly Identified Underrated 2025-26 Transfers**")
  ) %>% 
  tab_options( heading.align = "center",
              table.width = pct(90),
              table.font.names = "Times New Roman") %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_borders(sides = "right", color = "#D3D3D3", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>% 
  tab_options(
    data_row.padding = px(10), # Adds vertical space between players
    column_labels.padding = px(20) # Adds space around the column titles
  ) %>% 
  data_color(
    columns = c(`Rank Difference`),
    fn = scales::col_numeric(
      palette =c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"), # Light to Dark Blue
      domain = rapm_rank_domain
    )
  ) %>% 
  fmt_number(
    columns = c(`Rank Difference`),
    decimals = 0,     
    force_sign = TRUE  
  )
tbl

gtsave(tbl, "~/Downloads/good_portal.png")

bad_portal <- tibble(
  Player = c("Ian Jackson", "Joson Sanon", "Simeon Wilcher", "AJ Storr", "Pop Isaacs", "Jalil Bethea", "Kennard Davis", "Wesley Yates III"),
  `Transferred From` = c("North Carolina", "Arizona St.", "St. John's", "Kansas", "Creighton", "Miami (FL)", "Southern Illinois", 
                         "USC"),
  `Transferred To` = c("St John's", "St John's", "Texas", "Ole Miss", "Texas A&M", "Alabama", "BYU",
                       "Washington"),
  `247 Transfer Portal Rank` = c("7", "38", "94", "47", "39", "52", "41", "33"),
  `Bayesian RAPM Rank` = c("639", "284", "464", "1041", "256", "1251", "836", "112"),
  `Rank Difference` = c(-632, -246, -370, -994, -217, -1199, -795, -79),
  `Result` = c("18 MPG, 49.6% eFG%", "21 MPG, 42.9 eFG%", "19 MPG, 45.6% eFG%", "Benched mid-season", "Started 8/33 games","Played just 8 MPG, 37.5 FG%",
               "3PT% dropped 5.5%", "45.6% eFG%, 0.82 AST/TOV")
)

tbl_bad <- bad_portal %>% 
  mutate(
    `247 Transfer Portal Rank` = as.numeric(`247 Transfer Portal Rank`),
    `Bayesian RAPM Rank` = as.numeric(`Bayesian RAPM Rank`)
  ) %>% 
  gt_cbb_teams(`Transferred From`, `Transferred From`, logo_color = 'normal') %>% 
  gt_cbb_teams(`Transferred To`, `Transferred To`, logo_color = 'normal') %>% 
  gt(id = "good portal") %>% 
  gt_theme_guardian() %>% 
  fmt_markdown(`Transferred From`) %>% 
  fmt_markdown(`Transferred To`) %>% 
  tab_header(
    title = md("**Correctly Identified Overrated 2025-26 Transfers**")
  ) %>% 
  tab_options( heading.align = "center",
               table.width = pct(90),
               table.font.names = "Times New Roman") %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = cell_borders(sides = "right", color = "#D3D3D3", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) %>% 
  tab_options(
    data_row.padding = px(10), # Adds vertical space between players
    column_labels.padding = px(20) # Adds space around the column titles
  ) %>% 
  data_color(
    columns = c(`Rank Difference`),
    fn = scales::col_numeric(
      palette =c("#b22222", "#ff9999", "transparent", "#99ccff", "#6395EE"),
      domain = rapm_rank_domain_negative
    )
  ) %>% 
  fmt_number(
    columns = c(`Rank Difference`),
    decimals = 0,     
    force_sign = TRUE  
  )
tbl_bad

gtsave(tbl_bad, "~/Downloads/bad_portal.png")


prior_rapm_26 <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/MBB/25-26/prior_rapm.csv")

portal_2025_4 <- portal_2025_3 %>% 
  left_join(prior_rapm_26 %>% select(player, Total_RAPM_26 = Total_RAPM), join_by(player_name == player)) %>% 
  mutate(rapm_change = Total_RAPM_26 - Total_RAPM)

players_to_exclude_26 <- prior_rapm_26 %>% 
  count(player) %>% 
  filter(n > 1) %>% 
  pull(player)





portal_2026 <- read_csv("/Users/shanefaberman/Downloads/portal_2026_4-15.csv") %>% 
  mutate(player_name = paste0(FirstName, " ", LastName)) %>% 
  select(player_name, Position, `Origin Name`, Rating, `Destination Name`)

# get rid of all periods (like v.j.)
portal_2026$player_name <- portal_2026$player_name %>%  str_remove_all("\\.")

# get rid of jr, III, IV, etc.
suffix_regex <- "\\s+([jJ][rR]|[iI]{2,3}|[iI][vV])\\.?$" # get rid of Jr, III, IV, etc
portal_2026$player_name <- portal_2026$player_name %>%
  str_remove(suffix_regex) %>%
  str_trim()

# get rid of accents on player names
portal_2026$player_name <- stri_trans_general(portal_2026$player_name, "Latin-ASCII")

rapm_multi <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/MBB/multi_season_prior_rapm.csv") %>% 
  select(player, Total_RAPM)


players_to_exclude_multi <- rapm_multi %>% 
  count(player) %>% 
  filter(n > 1) %>% 
  pull(player)


portal_2026_2 <- portal_2026 %>% 
  mutate(player_name = ifelse(player_name == "Rob Wright", "Robert Wright", player_name)) %>% 
  left_join(rapm_multi %>% filter(!(player %in% players_to_exclude_multi)), join_by(player_name == player)) %>% 
  mutate(rank_rapm_bayesian = row_number(-Total_RAPM))


rapm_multi_raw <- read_csv("/Users/shanefaberman/cbb-rapm-dashboard/MBB/multi_season_no_prior_rapm.csv") %>% 
  select(player, Total_RAPM_Raw = Total_RAPM)


players_to_exclude_multi_raw <- rapm_multi_raw %>% 
  count(player) %>% 
  filter(n > 1) %>% 
  pull(player)


portal_2026_3 <- portal_2026_2 %>% 
  #mutate(player_name = ifelse(player_name == "Rob Wright", "Robert Wright", player_name)) %>% 
  left_join(rapm_multi_raw %>% filter(!(player %in% players_to_exclude_multi_raw)), join_by(player_name == player)) %>% 
  left_join(prior_rapm_26 %>% filter(!(player %in% players_to_exclude_26)) %>%  select(player, Total_RAPM_26 = Total_RAPM), join_by(player_name == player)) %>% 
  mutate(rank_rapm_raw = row_number(-Total_RAPM_Raw),
         rank_247 = row_number(-Rating)) %>% 
  select(Player = player_name, 
         Position,
         `Origin School` = `Origin Name`,
         `Destination School` = `Destination Name`,
         `Multi-Year Bayesian RAPM` = Total_RAPM,
         `2026 Bayesian RAPM` = Total_RAPM_26,
         `247 Transfer Portal Rank` = rank_247) %>% 
  arrange(-`Multi-Year Bayesian RAPM`)

portal_26_tbl <- portal_2026_3 %>% 
  mutate(`Multi-Year Bayesian RAPM` = round(`Multi-Year Bayesian RAPM`, digits = 2),
         `2026 Bayesian RAPM` = round(`2026 Bayesian RAPM`, digits = 2),
         `Destination School` = ifelse(is.na(`Destination School`), "", `Destination School`),
         `247 Transfer Portal Rank` = ifelse(`Player` == "Jasper Floyd", "Out of Elgibility", `247 Transfer Portal Rank`)) %>% 
  mutate(Position = ifelse(Position == "CG", "G", Position)) %>% 
  head(15) %>% 
  gt_cbb_teams(`Origin School`, `Origin School`, logo_color = 'normal') %>% 
  gt(id = "2026 portal") %>% 
  gt_theme_guardian() %>% 
  fmt_markdown(`Origin School`) %>% 
  tab_header(
    title = md("**Top 2026-27 Players in the Portal as of April 15th, 2026**")
  ) %>% 
  tab_options( heading.align = "center",
               table.width = pct(90),
               table.font.names = "Times New Roman") %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = cell_borders(sides = "right", color = "#D3D3D3", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>% 
  tab_options(
    data_row.padding = px(10), # Adds vertical space between players
    column_labels.padding = px(20) # Adds space around the column titles
  ) %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `Multi-Year Bayesian RAPM`) 
  )
portal_26_tbl

gtsave(portal_26_tbl, "~/Downloads/portal_26.png")


# lewis idea of correlation between n and n-1 rapm and 247 portal rank
combined <- portal_2025_2 %>% 
    filter(!is.na(rank_transfer)) %>% 
    inner_join(portal_2024_2 %>% filter(!is.na(rank_transfer_24)), join_by(player_name)) %>% 
  drop_na()

# predict next seasons portal rank
cor(combined$rank_transfer, combined$rank_transfer_24)
cor(combined$rank_transfer, combined$rank_RAPM_24)
cor(combined$rank_transfer, combined$Total_RAPM_24)

# predict next seasons RAPM/rank rapm
cor(combined$Total_RAPM, combined$Total_RAPM_24)
cor(combined$Total_RAPM, combined$rank_transfer_24)
cor(combined$rank_rapm, combined$rank_RAPM_24)

# cor and RAPM
cor(combined$rank_transfer, combined$Total_RAPM)
cor(combined$rank_transfer_24, combined$Total_RAPM_24)
