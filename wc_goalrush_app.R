# Required packages
require(data.table)
require(tidyverse)
require(stringr)
require(formattable)
require(googledrive)
require(googlesheets4)
require(gargle)
require(reactable)
require(htmltools)
require(htmlwidgets)

# system git IDs
system('git config user.email "richardplank@hotmail.com"')
system('git config user.name "richardplank"')

load(file = "lib/entries")

# set entry masking on (1) or off
masking <- 0
runs <- 40
runs <- 1
sleepytime <- if_else(runs == 1, 1, 120)

# De-authorize public sheet
gs4_deauth()

# sheet URLs
goals_sheet_url <- "https://docs.google.com/spreadsheets/d/1YYGvhWviudeDMu4l6dW-VqgGIOYFeefHHo5FQeVK36U/edit?gid=0#gid=0"

# Continuous deployment looping protocol (active)
for(i in 1:runs){
  message(paste("Updating at", Sys.time()))

goals_raw <- read_sheet(goals_sheet_url, sheet = "fixtures")
team_status <- read_sheet(goals_sheet_url, sheet = "status")
screen_name <- read_sheet(goals_sheet_url, sheet = "entries", range = "A:B")

# get last entered and next up fixtures
last_fix_df <- goals_raw |> 
  filter(!is.na(goals1)) |> 
  slice_max(order_by = gmnum) |> 
  mutate(
    last_fix = paste0(team1, " ", goals1, " - ", goals2, " ", team2)
  )
last_fix <- last_fix_df$last_fix

next_fix_df <- goals_raw |> 
  filter(is.na(goals1)) |> 
  slice_min(order_by = gmnum) |> 
  mutate(
    next_fix = paste0(team1, " v ", team2)
  )
next_fix <- next_fix_df$next_fix
next_fix_num <- next_fix_df$gmnum

if(nrow(last_fix_df) == 0) last_fix <- "Kick Off Soon!"
if(nrow(next_fix_df) == 0) next_fix <- "Tournament Complete"


goals_t1 <- goals_raw |> 
  # keep games played
  filter(!is.na(goals1)) |> 
  mutate(
    TEAM = team1,
    gm_gf = goals1,
    gm_ga = goals2
  ) |> 
  select(gmnum, TEAM, gm_gf, gm_ga)

goals_t2 <- goals_raw |> 
  # keep games played
  filter(!is.na(goals2)) |> 
  mutate(
    TEAM = team2,
    gm_gf = goals2,
    gm_ga = goals1,
  ) |> 
  select(gmnum, TEAM, gm_gf, gm_ga)


team_goals <- bind_rows(goals_t1, goals_t2) |> 
  # get total goals scored in all games
  mutate(ALLGOALS = sum(gm_gf)) |> 
  arrange(TEAM, gmnum) |> 
  group_by(TEAM) |> 
  mutate(
    GOALS_FOR = cumsum(gm_gf),
    GOALS_AGAINST = cumsum(gm_ga),
    GAMES = row_number()
  ) |> 
  slice_max(order_by = gmnum) |> 
  ungroup() |> 
  # merge on status
  right_join(team_status, by = "TEAM") |> 
  mutate(
    GOALS_FOR = if_else(is.na(GOALS_FOR), 0, GOALS_FOR),
    GOALS_AGAINST = if_else(is.na(GOALS_AGAINST), 0, GOALS_AGAINST),
    GAMES = if_else(is.na(GAMES), 0, GAMES),
    STATUS = case_when(
      str_detect(status, "Grp") == TRUE ~ paste0(status, substr(GROUP, 6, 7), ", ", as.character(GAMES), " games"),
      TRUE ~ paste0(status, ", ", as.character(GAMES), " games")
    )
  ) |> 
  select(TEAM, TEAMCD, POTN, GAMES, GOALS_FOR, GOALS_AGAINST, STATUS)

ALLGOALS = sum(team_goals$GOALS_FOR)
TOTGAMES = sum(team_goals$GAMES) / 2
GAMES_LEFT = 104 - TOTGAMES
PROJ_GOALS = if_else(TOTGAMES == 0, 0, round((ALLGOALS / TOTGAMES) * 104))

### Calculate scores

scores <- entries |> 
  left_join(team_goals, by = "TEAM") |> 
  mutate(
    POINTS = case_when(
      POTCD %in% c("P1A", "P2A", "P3A", "P4A", "P5A", "P6A",
                   "P7A", "P8A", "P9A", "P10A", "P11A", "P12A") ~ GOALS_FOR * JKMULT,
      POTCD %in% c("P1D", "P2D", "P3D", "P4D", "P5D", "P6D") ~ 0 - GOALS_AGAINST
    ),
    TOTDIFF = TOTGUESS - PROJ_GOALS,
    TDIFFRANK = abs(TOTDIFF)
  )

totscores <- scores |> 
  arrange(NAME, POTCD) |> 
  group_by(NAME) |> 
  mutate(
    TOTPOINTS = cumsum(POINTS),
    TOT_GPA = cumsum(if_else(POTCD %in% c("P1A", "P2A", "P3A", "P4A", "P5A", "P6A",
                              "P7A", "P8A", "P9A", "P10A", "P11A", "P12A"), GAMES, 0)),
    TOT_GPD = cumsum(if_else(POTCD %in% c("P1D", "P2D", "P3D", "P4D", "P5D", "P6D"), GAMES, 0)),
    TOT_GM_PLD = paste0(TOT_GPA, "/", TOT_GPD)
  ) |> 
  slice(n()) |> 
  ungroup() |> 
  # set details to reflect total score
  mutate(
    POTCD = "TOTAL",
    TEAM = "Total",
    TEAMCD = "TOT"
  ) |> 
  select(-GAMES, -starts_with("GOALS"), -STATUS, -POINTS) |> 
  rename(POINTS = TOTPOINTS)

tot_gm_pld <- totscores |> 
  select(NAME, TOT_GM_PLD)

scores_all <- bind_rows(scores, 
                        totscores |> 
                          select(-TOT_GPA, -TOT_GPD, -TOT_GM_PLD))

player_points <- scores_all |> 
  pivot_wider(
    id_cols = c("NAME", "TOTGUESS", "TOTDIFF", "TDIFFRANK"),
    values_from = POINTS,
    names_from = POTCD
  )

player_teams <- scores_all |> 
  # drop TOTAL from getting team names
  filter(TEAMCD != "TOT") |> 
  mutate(
    TCD = case_when(
      JKMULT == 1 ~ TEAMCD,
      JKMULT > 1 ~ paste0(TEAMCD, "*")
    ),
    SMK = case_when(
      toupper(substr(STATUS, 1, 3)) == "OUT" ~ " | ",
      TRUE ~ " : "
    )
  ) |> 
  pivot_wider(
    id_cols = c("NAME"),
    values_from = c(TCD, SMK),
    names_from = POTCD
  )

player_rows <- player_points |> 
  left_join(screen_name, by = "NAME") |> 
  left_join(player_teams, by = "NAME") |> 
  left_join(tot_gm_pld, by = "NAME") |> 
  arrange(desc(TOTAL), TDIFFRANK) |>
  mutate(
    TOT_GD_RANK = case_when(
      SCREEN_NAME == "AI" ~ as.numeric(NA),
      TRUE ~ (10000 * TOTAL) - TDIFFRANK
    ),
    POS = min_rank(desc(TOT_GD_RANK)),
    POSNAME = if_else(SCREEN_NAME == "AI", 
                      paste0("- ", SCREEN_NAME), 
                      paste0(POS, " ", SCREEN_NAME)),
    Pot01 = paste0(TCD_P1A, SMK_P1A, P1A),
    Pot02 = paste0(TCD_P2A, SMK_P2A, P2A),
    Pot03 = paste0(substr(TCD_P3A, 1, 3), SMK_P3A, P3A, substr(TCD_P3A, 4, 4)),
    Pot04 = paste0(substr(TCD_P4A, 1, 3), SMK_P4A, P4A, substr(TCD_P4A, 4, 4)),
    Pot05 = paste0(substr(TCD_P5A, 1, 3), SMK_P5A, P5A, substr(TCD_P5A, 4, 4)),
    Pot06 = paste0(substr(TCD_P6A, 1, 3), SMK_P6A, P6A, substr(TCD_P6A, 4, 4)),
    Pot07 = paste0(substr(TCD_P7A, 1, 3), SMK_P7A, P7A, substr(TCD_P7A, 4, 4)),
    Pot08 = paste0(substr(TCD_P8A, 1, 3), SMK_P8A, P8A, substr(TCD_P8A, 4, 4)),
    Pot09 = paste0(substr(TCD_P9A, 1, 3), SMK_P9A, P9A, substr(TCD_P9A, 4, 4)),
    Pot10 = paste0(substr(TCD_P10A, 1, 3), SMK_P10A, P10A, substr(TCD_P10A, 4, 4)),
    Pot11 = paste0(substr(TCD_P11A, 1, 3), SMK_P11A, P11A, substr(TCD_P11A, 4, 4)),
    Pot12 = paste0(substr(TCD_P12A, 1, 3), SMK_P12A, P12A, substr(TCD_P12A, 4, 4)),
    DPot01 = paste0(TCD_P1D, SMK_P1D, P1D),
    DPot02 = paste0(TCD_P2D, SMK_P2D, P2D),
    DPot03 = paste0(TCD_P3D, SMK_P3D, P3D),
    DPot04 = paste0(TCD_P4D, SMK_P4D, P4D),
    DPot05 = paste0(TCD_P5D, SMK_P5D, P5D),
    DPot06 = paste0(TCD_P6D, SMK_P6D, P6D)
  ) |> 
  select(POSNAME, TOTAL, TOT_GM_PLD, starts_with("Pot"), starts_with("DPot"), TOTGUESS, TOTDIFF)


## team status output

pots_for_list     <- paste0("P", 1:12, "A") # Generates P1A, P2A... P12A
pots_against_list <- paste0("P", 1:6, "D")  # Generates P1D, P2D... P6D

team_summary <- scores_all |>
  filter(POTCD != "TOTAL" & NAME != "AI") |> 
  group_by(TEAM) |>
  summarise(
    PACNT = coalesce(sum(POTCD %in% pots_for_list, na.rm = TRUE), 0),
    
    # Count rows where POTCD matches any value in P1D - P6D
    PDCNT = coalesce(sum(POTCD %in% pots_against_list, na.rm = TRUE), 0),
    
    # Count rows where JKMULT is strictly greater than 1
    JKCNT = coalesce(sum(JKMULT > 1, na.rm = TRUE), 0),
    
    .groups = "drop"
  ) |>
  # merge goals and status on
  right_join(team_goals, by = "TEAM") |> 
  mutate(
    PACNT = if_else(is.na(PACNT), 0, PACNT),
    PDCNT = if_else(is.na(PDCNT), 0, PDCNT),
    GOALS = case_when(
      POTN <= 6 ~ paste0(GOALS_FOR, " - ", GOALS_AGAINST),
      TRUE ~ as.character(GOALS_FOR)
    ),
    PCKD = case_when(
      POTN <= 6 ~ paste0(PACNT, " / ", PDCNT),
      TRUE ~ as.character(PACNT)
    ),
    JKCNT = case_when(
      POTN > 2 & JKCNT == 0 ~ as.character(NA),
      POTN > 2 ~ as.character(JKCNT),
      TRUE ~ "-"
    ),
    PCKA = if_else(is.na(PACNT) | PACNT == 0, as.numeric(NA), PACNT),
    PCKD = if_else(is.na(PDCNT) | PDCNT == 0, as.numeric(NA), PDCNT),
    GLSA = case_when(
      GAMES > 0 ~ GOALS_FOR,
      TRUE ~ as.numeric(NA)
    ),
    GLSD = case_when(
      POTN <= 6 & GAMES > 0 ~ 0 - GOALS_AGAINST, 
      TRUE ~ as.numeric(NA)
    )
  ) |>
  arrange(POTN, desc(PACNT), desc(JKCNT), desc(GOALS_FOR), desc(PDCNT), GOALS_AGAINST) |> 
  #select(POTN, TEAM, PCKD, GOALS, JKCNT, STATUS)
  select(POTN, TEAMCD, PCKA, GLSA, JKCNT, PCKD, GLSD, STATUS)



## MASKING DATA FOR PREVIEW
if (masking == 1) {
  player_rows <- player_rows |>
    filter(POSNAME != "- AI") |> 
    mutate(
      TOTAL = 0,
      across(matches("^Pot\\d+$|^DPot\\d+$"), ~ "XXX : 0"),
      TOTGUESS = "nnn",
      TOTDIFF = "nnn",
      Name_Only = str_trim(str_remove(POSNAME, "^\\d+\\s+"))
    ) |>
    arrange(Name_Only) |>
    mutate(
      temprank = case_when(
        Name_Only == "- AI" ~ as.numeric(NA),
        TRUE ~ row_number()
      ),
      POS = min_rank(temprank),
      POSNAME = if_else(is.na(temprank), 
                        "- AI", 
                        paste0(POS, " ", Name_Only))
    ) |>
    arrange(temprank) |> 
    select(-Name_Only, -temprank, -POS) 
    
  
  team_summary <- team_summary |>
    mutate(
      PCKA = as.numeric(NA),
      GLSA = as.numeric(NA),
      JKCNT = case_when(
        POTN > 2 ~ as.character(NA),
        TRUE ~ "-"
      ),
      PCKD = as.numeric(NA),
      GLSD = as.numeric(NA)
    ) |> 
    arrange(POTN, TEAMCD)
}



# Define the page palette
my_theme <- reactableTheme(
  backgroundColor = "#1e1f21",       # Dark charcoal background
  borderColor = "#f0f0f0",           # Off-white lines/borders
  stripedColor = "#2a2b2d",          # Slightly lighter dark for rows
  headerStyle = list(
    backgroundColor = "#1e1f21",
    color = "#ffdc55",               # Mustard yellow for column headers
    borderBottom = "2px solid #f0f0f0"
  ),
  cellStyle = list(
    color = "#f0f0f0",               # Off-white text for body
    borderBottom = "1px solid #f0f0f0"
  )
)

create_scenario_table <- function(player_rows) {
  pot_cols <- grep("^Pot\\d{2}", names(player_rows), value = TRUE)
  dpot_cols <- grep("^DPot\\d{2}", names(player_rows), value = TRUE)
  
  pot_defs <- lapply(c(pot_cols, dpot_cols), function(x) {
    short_name <- regmatches(x, regexpr("\\d{2}$", x))
    colDef(
      name = short_name, 
      minWidth = 62,       
      align = "center"
    )
  })
  names(pot_defs) <- c(pot_cols, dpot_cols)
  
  reactable(
    player_rows,
    pagination = FALSE,
    compact = TRUE,
    fullWidth = FALSE,
    theme = my_theme,
    style = list(minWidth = "1250px"), 
    columnGroups = list(
      colGroup(
        name = paste0("POTS FOR >>>", strrep("\u3000", 50), "<<< POTS FOR"), 
        columns = pot_cols
      ),
      colGroup(name = paste0("POTS AGAINST >>>", strrep("\u3000", 10), "<<< POTS AGAINST"), columns = dpot_cols)
    ),
    defaultColDef = colDef(
      align = "center",
      style = function(value, index, name) {
        styles <- list(fontSize = "11px", color = "#f0f0f0", fontWeight = "normal", opacity = 1)
        
        # Keep the yellow right dividers on Pot12 and DPot06
        if (!is.null(name) && (name == "Pot12" || name == "DPot06")) {
          styles$borderRight <- "1px solid #ffdc55"
        }
        
        # 1. Check if this is the AI row (matches ' AI' at the end of POSNAME)
        is_ai_row <- grepl("\\sAI$", player_rows$POSNAME[index])
        
        if (is_ai_row) {
          styles$opacity <- 0.4  # Fade the entire row out slightly
        }
        
        if (is_character(value)) {
          # 2. Yellow/Gold-color and bold any value containing the asterisk (*)
          if (grepl("\\*", value)) {
            styles$fontWeight <- "bold"
            
            if (is_ai_row) {
              # Faded/muted yellow for the AI row specifically
              # #bfa440 is a muted/darker gold, or you could use #ffdc5580 for 50% transparency
              styles$color <- "#bfa440" 
            } else {
              # Normal bright yellow for non-AI rows
              styles$color <- "#ffdc55"
            }
          }
          
          # 3. Extra fade out for team cells containing the pipe (|) (eliminated teams)
          if (grepl("\\|", value)) {
            styles$opacity <- if_else(is_ai_row, 0.2, 0.4) 
          }
        }
        styles
      }
    ),
    columns = c(
      list(
        POSNAME = colDef(
          name = "RANK / PLAYER", 
          width = 140, 
          align = "left",
          style = function(value) {
            styles <- list(paddingLeft = "10px", fontWeight = "500", whiteSpace = "nowrap")
            if (grepl("\\sAI$", value)) {
              styles$opacity <- 0.4
            }
            styles
          }
        ),
        TOTAL = colDef(
          name = "TOT", 
          width = 45, 
          style = function(value, index) {
            styles <- list(background = "#ffdc55", color = "#252628", fontWeight = "bold")
            if (grepl("\\sAI$", player_rows$POSNAME[index])) {
              styles$background <- "#444547"
              styles$color <- "#a0a0a0"
              styles$fontWeight <- "normal"
            }
            styles
          }
        ),
        TOT_GM_PLD = colDef(name = "GP", width = 50),
        TOTGUESS = colDef(name = "TG", width = 50),
        TOTDIFF = colDef(name = "GD", width = 50)
      ),
      pot_defs
    )
  )
}

create_team_summary_table <- function(team_summary) {
  reactable(
    team_summary,
    pagination = FALSE,
    compact = TRUE,
    fullWidth = FALSE,
    theme = my_theme,
    style = list(maxWidth = "500px"),
    columnGroups = list(
      colGroup(name = "GOALS FOR", columns = c("PCKA", "GLSA", "JKCNT")),
      colGroup(name = "GOALS AGAINST", columns = c("PCKD", "GLSD"))
    ),
    defaultColDef = colDef(
      align = "center",
      style = htmlwidgets::JS("
        function(rowInfo, column, tableState) {
          const index = rowInfo.index;
          const baseStyle = { fontSize: '10px', borderBottom: 'none' };
          
          if (index > 0) {
            if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
              baseStyle.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
            } else {
              baseStyle.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
            }
          }
          
          if (rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out')) {
            baseStyle.opacity = 0.4;
          }
          
          return baseStyle;
        }
      ")
    ),
    
    columns = list(
      POTN = colDef(
        name = "POT", 
        width = 40,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            if (index === 0 || tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
              return { 
                fontWeight: 'bold',
                borderBottom: 'none',
                borderTop: index > 0 ? '1px solid rgba(255, 255, 255, 0.4)' : 'none'
              }
            }
            return { 
              color: 'transparent',
              borderTop: 'none',
              borderBottom: 'none'
            }
          }
        ")
      ),
      TEAMCD = colDef(
        name = "TEAM", 
        align = "left", 
        width = 50,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const style = { fontSize: '10px', paddingLeft: '10px', fontWeight: '500', borderBottom: 'none', borderRight: '1px solid #ffdc55' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            if (rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out')) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      ),
      PCKA = colDef(
        name = "PICKS", 
        width = 50,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const style = { fontSize: '10px', borderBottom: 'none' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            if (rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out')) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      ),
      GLSA = colDef(
        name = "GF", 
        width = 30,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const potGF = rowInfo.row['POTN'];
            const valGF = rowInfo.row['GLSA'];
            const isOut = rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out');
            const style = { fontSize: '10px', borderBottom: 'none' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            const valuesGF = tableState.pageRows
              .filter(function(r) { return r['POTN'] === potGF; })
              .map(function(r) { return r['GLSA']; });
              
            const maxGF = Math.max.apply(null, valuesGF);
            
            if (valGF === maxGF && valuesGF.some(function(v) { return v > 0; }) && !isOut) { 
              style.backgroundColor = '#ffdc55';
              style.fontWeight = 'bold';
              style.color = '#333';
            }
            
            if (isOut) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      ),
      JKCNT = colDef(
        name = "JOKERS", 
        width = 60,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const isOut = rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out');
            const jokerColor = isOut ? '#bfa440' : '#ffdc55';
            const style = { color: jokerColor, fontWeight: 'bold', borderBottom: 'none', borderRight: '1px solid #ffdc55' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            if (isOut) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      ),
      PCKD = colDef(
        name = "PICKS", 
        width = 50,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const style = { fontSize: '10px', borderBottom: 'none' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            if (rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out')) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      ),
      GLSD = colDef(
        name = "GA",
        width = 30,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const potGLSD = rowInfo.row['POTN'];
            const valGLSD = rowInfo.row['GLSD'];
            const isOut = rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out');
            const style = { fontSize: '10px', borderBottom: 'none', borderRight: '1px solid #ffdc55' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            // Extract and clean values: Filter out null, undefined, or empty string data 
            const valuesGLSD = tableState.pageRows
              .filter(function(r) { return r['POTN'] === potGLSD; })
              .map(function(r) { return r['GLSD']; })
              .filter(function(v) { return v !== null && v !== undefined && v !== ''; });
              
            // Safely find the maximum negative value
            if (valuesGLSD.length > 0) {
              const maxGLSD = Math.max.apply(null, valuesGLSD);
              
              if (valGLSD === maxGLSD && !isOut) { 
                style.backgroundColor = '#ffdc55';
                style.fontWeight = 'bold';
                style.color = '#333';
              }
            }
            
            if (isOut) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      ),
      STATUS = colDef(
        name = "STATUS", 
        width = 150,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const style = { fontSize: '10px', borderBottom: 'none' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            if (rowInfo.row['STATUS'] && rowInfo.row['STATUS'].includes('Out')) {
              style.opacity = 0.4;
            }
            return style;
          }
        ")
      )
    )
  )
}

# Shared CSS injections for the global navigation system
shared_styles <- HTML("
  body { 
    font-family: 'Montserrat', sans-serif; 
    background-color: #1e1f21; 
    color: #f0f0f0; 
    padding: 20px; 
    margin: 0;
  }
  .page-container { max-width: 100%; margin: 0 auto; }
  h1 { 
    text-align: center; color: #ffdc55; font-weight: 800; 
    text-transform: uppercase; letter-spacing: 1px; font-size: 2em; margin-bottom: 5px;
  }
  .update-time { text-align: center; font-size: 0.9em; opacity: 0.8; margin-bottom: 15px; }
  
  /* Navigation Bar Styles */
  .nav-bar { text-align: center; margin-bottom: 30px; }
  .nav-btn {
    display: inline-block; padding: 10px 20px; margin: 0 8px;
    background-color: #2a2b2d; color: #f0f0f0; font-weight: 700;
    text-transform: uppercase; text-decoration: none; font-size: 0.85em;
    letter-spacing: 1px; border: 1px solid #ffdc55; border-radius: 4px;
    transition: all 0.2s ease-in-out;
  }
  .nav-btn:hover { background-color: #ffdc55; color: #1e1f21; cursor: pointer; }
  .nav-btn.active { background-color: #ffdc55; color: #1e1f21; border: 1px solid #ffdc55; }

  .table-section { display: inline-block; text-align: left; max-width: 100%; margin-bottom: 20px; }
  h2 { 
    color: #ffdc55; border-left: 5px solid #ffdc55; padding-left: 15px; 
    text-transform: uppercase; font-size: 1.5em; margin-top: 0; margin-bottom: 15px; font-weight: 800; display: block;
  }
  .rt-container { width: 100% !important; overflow-x: auto !important; display: block !important; -webkit-overflow-scrolling: touch; }
  .rt-table { width: auto !important; flex: none !important; }
  .rt-th, .rt-header-content { color: #ffdc55 !important; font-size: 11px !important; text-align: center !important; justify-content: center !important; }
  .rt-column-group-header { background-color: #252628 !important; border-bottom: 1px solid #444 !important; display: flex !important; }
  .rt-column-group-header-content { font-size: 11px !important; text-transform: uppercase; letter-spacing: 1px; color: #ffdc55 !important; display: inline-block !important; margin-right: auto !important; padding-left: 12px !important; }
  .rt-td { white-space: nowrap !important; padding: 6px 4px !important; }
  .footnote-container { margin-top: 40px; padding: 20px 10px; border-top: 1px solid #333; text-align: center; }
  .footnote-line { color: #ffdc55; font-size: 0.8em; font-weight: 600; letter-spacing: 1px; margin-bottom: 8px; opacity: 0.9; }
  .footnote-line:last-child { margin-bottom: 0; }
  @media (max-width: 600px) {
    body { padding: 10px; } h1 { font-size: 1.6em; } h2 { font-size: 1.3em; padding-left: 10px; }
    .update-time { font-size: 0.8em; } .footnote-line { font-size: 0.75em; }
  }
")

# Build standalone page 1 (live.html / Dashboard)
page1 <- tags$html(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700;800&display=swap", rel="stylesheet"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, shrink-to-fit=no"),
    tags$meta(`http-equiv` = "refresh", content = "30"),
    tags$title("World Cup Goal Rush"),
    tags$style(shared_styles),
    
    tags$style(HTML("
      .nav-bar { margin-bottom: 15px; }
      .nav-btn {
        padding: 5px 12px !important;
        font-size: 0.72em !important;
        margin: 0 3px !important;
      }
      @media (max-width: 480px) {
        .nav-btn {
          padding: 4px 8px !important;
          font-size: 0.68em !important;
        }
      }
    "))
  ),
  tags$body(
    tags$div(class = "page-container",
             tags$h1("WC26 Goal Rush"),
             tags$p(class = "update-time", style = "margin: 2px 0 12px 0; font-weight: 500;",
                    paste("Last Updated:", format(Sys.time(), "%H:%M %d %b %Y"))),
             
             # Links to move between the two dashboards
             tags$div(class = "nav-bar",
                      tags$a(href = "live.html", class = "nav-btn active", "Standings"),
                      tags$a(href = "fixtures.html", class = "nav-btn", "Fixtures & Selections")),
             
             tags$p(class = "fixture-status-line", style = "text-align: center; font-size: 0.85em; opacity: 0.85; margin: 2px 0; font-weight: 500;",
                    tags$span(style = "color: #ffdc55; font-weight: bold;", "Latest Score: "), last_fix),
             tags$p(class = "fixture-status-line", style = "text-align: center; font-size: 0.85em; opacity: 0.85; margin: 2px 0; font-weight: 500; margin-bottom: 25px;",
                    tags$span(style = "color: #ffdc55; font-weight: bold;", "Next Up: "), next_fix),
             
             tags$div(style = "width: 100%; overflow: visible;",
                      tags$div(class = "table-section",
                               tags$h2("Standings"),
                               create_scenario_table(player_rows)))),
    
    tags$div(class = "footnote-container",
             tags$div(class = "footnote-line", "* = SELECTED JOKER"),
             tags$div(class = "footnote-line", "GP = GAMES PLAYED (ATT/DEF)"),
             tags$div(class = "footnote-line", "TG = TOTAL PREDICTED GOALS"),
             tags$div(class = "footnote-line", paste0("GD = DIFFERENCE VS PROJECTED ACTUAL TOTAL (", PROJ_GOALS, ")")),
             tags$div(class = "footnote-line", style = "opacity: 0.45; font-weight: normal; font-size: 0.75em; margin-top: 12px;",
                      "| = COUNTRY ELIMINATED FROM WORLD CUP")),
    
    tags$div(style = "width: 100%; overflow: visible; margin-top: 30px;",
             tags$div(class = "table-section",
                      tags$h2("Team Summary"),
                      create_team_summary_table(team_summary))),
    
    tags$div(class = "footnote-container",
             tags$div(class = "footnote-line", "GOLD SQUARE = BEST PICK(S) IN POT"))
  )
)

htmltools::save_html(page1, file = "live.html", libdir = "lib")


#### CREATE 2nd HTML TO SHOW FIXTURES AND SELECTORS
# 1. Clean and prepare the entries data

prepared_entries <- entries |>
  left_join(screen_name, by = "NAME") |> 
  mutate(DISPLAY_NAME = if_else(JKMULT > 1, paste0("*", SCREEN_NAME, "*"), SCREEN_NAME)) |>
  mutate(POT_TYPE = if_else(str_detect(POTCD, "A$"), "A", "D")) |>
  arrange(TEAM, POT_TYPE, SCREEN_NAME) |>
  group_by(TEAM, POT_TYPE) |>
  summarise(
    # Changed from a comma to an HTML line break for clean vertical stacking
    ENTRANTS_LIST = paste(DISPLAY_NAME, collapse = "<br>"),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = POT_TYPE, 
    values_from = ENTRANTS_LIST,
    names_prefix = "PICKED_"
  )

# 2. Build on the goals_raw structure using left_joins
goals_with_entrants <- goals_raw |>
  left_join(prepared_entries, by = c("team1" = "TEAM")) |>
  rename(TEAM1A = PICKED_A, TEAM1D = PICKED_D) |>
  left_join(prepared_entries, by = c("team2" = "TEAM")) |>
  rename(TEAM2A = PICKED_A, TEAM2D = PICKED_D) |>
  mutate(across(c(TEAM1A, TEAM1D, TEAM2A, TEAM2D), ~ replace_na(.x, ""))) |> 
  left_join(team_goals, by = c("team1" = "TEAM")) |> 
  rename(TEAM1CD = TEAMCD) |>
  left_join(team_goals, by = c("team2" = "TEAM")) |> 
  rename(TEAM2CD = TEAMCD) |> 
  mutate(
    FIXTURE = case_when(
      is.na(goals1) | is.na(goals2) ~ paste0(TEAM1CD, " v ", TEAM2CD),
      TRUE ~ paste0(TEAM1CD, " ", goals1, "-", goals2, " ", TEAM2CD),
    ),
    keep_fix = case_when(
      next_fix_num <= 3 & gmnum <= 5 ~ "Y",
      gmnum >= next_fix_num - 2 & gmnum <= next_fix_num + 3 ~ "Y",
      TRUE ~ "N"
    )
  ) |> 
  filter(keep_fix == "Y") |> 
  select(TEAM1D, TEAM1A, FIXTURE, TEAM2A, TEAM2D)

## MASKING DATA FOR PREVIEW
if (masking == 1) {
  goals_with_entrants <- goals_with_entrants |>
    mutate(
      TEAM1D = case_when(
        FIXTURE %in% c("MEX v SAF", "USA v PAR") ~ "Player Z",
        TRUE ~ as.character(NA)
      ),
      TEAM1A = case_when(
        FIXTURE %in% c("MEX v SAF") ~ "Player Y",
        FIXTURE %in% c("KOR v CZE") ~ "*Player Y*",
        FIXTURE %in% c("CAN v BOS", "USA v PAR") ~ "Player X",
        TRUE ~ as.character(NA)
      ),
      TEAM2A = case_when(
        FIXTURE %in% c("MEX v SAF", "KOR v CZE") ~ "Player X",
        FIXTURE %in% c("CAN v BOS") ~ "Player Y</br>*Player Z*",
        TRUE ~ as.character(NA)
      ),
      TEAM2D = case_when(
        FIXTURE %in% c("USA v PAR", "QAT v SWI") ~ "Player Y",
        TRUE ~ as.character(NA)
      )
    ) 
}


# Functional component for rendering the new fixtures table inside reactable
create_fixtures_table <- function(goals_with_entrants) {
  
  # Custom R cell renderer to cleanly parse out asterisks, colorize Jokers, and handle line breaks
  joker_html_renderer <- function(value) {
    if (value == "" || is.na(value)) return("")
    
    # Use regex to find text inside asterisks, replace with bold gold styling tags
    formatted_value <- gsub("\\*([^*]+)\\*", "<b style='color: #ffdc55; font-weight: bold;'>\\1</b>", value)
    htmltools::HTML(formatted_value)
  }
  
  reactable(
    goals_with_entrants,
    pagination = FALSE,
    compact = TRUE,
    fullWidth = TRUE,             # Changed to TRUE to scale gracefully on responsive viewports
    theme = my_theme,
    style = list(maxWidth = "100%", minWidth = "600px"), # Reduced min-width drastically for mobile accessibility
    columnGroups = list(
      colGroup(name = "<<<", columns = c("TEAM1D", "TEAM1A")),
      colGroup(name = ">>>", columns = c("TEAM2A", "TEAM2D"))
    ),
    defaultColDef = colDef(
      align = "left",
      html = TRUE,                # Enables rendering of custom <br> updates and HTML tags
      cell = joker_html_renderer, # Injects our styling logic across the entry lists
      style = list(fontSize = "10px", color = "#f0f0f0", padding = "2px 1px")
    ),
    columns = list(
      # Scaled widths down for mobile screens while prioritizing vertically stacked lists
      TEAM1D = colDef(name = "DEFENCE", width = 80, align = "center"),
      TEAM1A = colDef(
        name = "ATTACK", 
        width = 80, 
        align = "center",
        style = list(fontSize = "10px", color = "#f0f0f0", borderRight = "1px solid #ffdc55", padding = "2px 1px")
      ),
      FIXTURE = colDef(
        name = "MATCH", 
        width = 90, 
        align = "center",
        html = FALSE, # Standard text parsing for raw code structures
        cell = function(value) value, 
        style = list(fontWeight = "bold", color = "#ffdc55", background = "#252628", borderRight = "1px solid #ffdc55", fontSize = "10px")
      ),
      TEAM2A = colDef(name = "ATTACK", width = 80, align = "center"),
      TEAM2D = colDef(name = "DEFENCE", width = 80, align = "center")
    )
  )
}

# Build standalone page 2 (fixtures.html)
page2 <- tags$html(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700;800&display=swap", rel="stylesheet"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, shrink-to-fit=no"),
    tags$meta(`http-equiv` = "refresh", content = "30"),
    tags$title("World Cup Goal Rush - Fixtures"),
    tags$style(shared_styles),
    
    # SAFE OVERRIDE: This applies specifically to the buttons without touching shared_styles
    tags$style(HTML("
      .nav-bar { margin-bottom: 15px; }
      .nav-btn {
        padding: 5px 12px !important;
        font-size: 0.72em !important;
        margin: 0 3px !important;
      }
      @media (max-width: 480px) {
        .nav-btn {
          padding: 4px 8px !important;
          font-size: 0.68em !important;
        }
      }
    "))
  ),
  tags$body(
    tags$div(class = "page-container",
             tags$h1("WC26 Goal Rush"),
             tags$p(class = "update-time", style = "margin: 2px 0 12px 0; font-weight: 500;",
                    paste("Last Updated:", format(Sys.time(), "%H:%M %d %b %Y"))),
             
             # Active links configuration for Navigation mapping
             tags$div(class = "nav-bar",
                      tags$a(href = "live.html", class = "nav-btn", "Back to Standings"),
                      tags$a(href = "fixtures.html", class = "nav-btn active", "Fixtures & Selections")),
             
             tags$div(style = "width: 100%; overflow: visible;",
                      tags$div(class = "table-section",
                               tags$h2("Fixtures & Selections"),
                               create_fixtures_table(goals_with_entrants)))),
    
    tags$div(class = "footnote-container",
             tags$div(class = "footnote-line", style = "color: #ffdc55;", "GOLD = JOKER"))
  )
)

htmltools::save_html(page2, file = "fixtures.html", libdir = "lib")




  try({
    # Staging all newly saved layout modules
    system("git add .")

    # Execution block for auto commits safely ignoring empty tree states
    system('git commit -m "Auto-update scores" --no-verify')

    # Final operational execution sequence pushing data models into origin pipeline
    system("git push origin main --quiet")
  }, silent = FALSE)

  message(paste("Successfully updated at", Sys.time()))

  # check the sheet every 90 seconds
  Sys.sleep(sleepytime)
}
