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

# De-authorize public sheet
gs4_deauth()

# sheet URLs
goals_sheet_url <- "https://docs.google.com/spreadsheets/d/1YYGvhWviudeDMu4l6dW-VqgGIOYFeefHHo5FQeVK36U/edit?gid=0#gid=0"

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
      str_detect(status, "GRP") == TRUE ~ paste0(status, substr(GROUP, 6, 7), ", ", as.character(GAMES), " games"),
      TRUE ~ paste0(status, ", ", as.character(GAMES), " games")
    )
  ) |> 
  select(TEAM, TEAMCD, POTN, GAMES, GOALS_FOR, GOALS_AGAINST, STATUS)

ALLGOALS = sum(team_goals$GOALS_FOR)

### Calculate scores

scores <- entries |> 
  left_join(team_goals, by = "TEAM") |> 
  mutate(
    POINTS = case_when(
      POTCD %in% c("P1A", "P2A", "P3A", "P4A", "P5A", "P6A",
                   "P7A", "P8A", "P9A", "P10A", "P11A", "P12A") ~ GOALS_FOR * JKMULT,
      POTCD %in% c("P1D", "P2D", "P3D", "P4D", "P5D", "P6D") ~ 0 - GOALS_AGAINST
    ),
    TOTDIFF = TOTGUESS - ALLGOALS,
    TDIFFRANK = abs(TOTDIFF)
  )

totscores <- scores |> 
  arrange(NAME, POTCD) |> 
  group_by(NAME) |> 
  mutate(
    TOTPOINTS = cumsum(POINTS)
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


scores_all <- bind_rows(scores, totscores)

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
  arrange(desc(TOTAL), TDIFFRANK) |>
  mutate(
    TOT_GD_RANK = (10000 * TOTAL) - TDIFFRANK,
    POS = min_rank(desc(TOT_GD_RANK)),
    POSNAME = paste0(POS, " ", SCREEN_NAME),
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
  select(POSNAME, TOTAL, starts_with("Pot"), starts_with("DPot"), TOTGUESS, TOTDIFF)
  


## team status output

pots_for_list     <- paste0("P", 1:12, "A") # Generates P1A, P2A... P12A
pots_against_list <- paste0("P", 1:6, "D")  # Generates P1D, P2D... P6D

team_summary <- scores_all |>
  filter(POTCD != "TOTAL") |> 
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
player_rows <- player_rows |> 
  mutate(
    TOTAL = 0,
    across(matches("^Pot\\d+$|^DPot\\d+$"), ~ "XXX : 0"),
    TOTGUESS = "nnn",
    TOTDIFF = "nnn",
    Name_Only = str_trim(str_remove(POSNAME, "^\\d+\\s+"))
  ) |> 
  arrange(Name_Only) |> 
  mutate(
    POSNAME = paste(row_number(), Name_Only)
  ) |> 
  select(-Name_Only)

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
  )



# Define the page palette
my_theme <- reactableTheme(
  backgroundColor = "#1e1f21",       # Dark charcoal background
  borderColor = "#f0f0f0",           # Off-white lines/borders
  stripedColor = "#2a2b2d",         # Slightly lighter dark for rows
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
      minWidth = 62,       # Changed from width=62 to minWidth=45
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
    # Reduced from 1250px to 1050px so it fits much better into mobile viewports
    style = list(minWidth = "1250px"), 
    columnGroups = list(
      colGroup(
        # \u3000 is a native invisible character. No HTML wrappers needed!
        name = paste0("POTS FOR >>>", strrep("\u3000", 50), "<<< POTS FOR"), 
        columns = pot_cols
      ),
      colGroup(name = paste0("POTS AGAINST >>>", strrep("\u3000", 10), "<<< POTS AGAINST"), columns = dpot_cols)
    ),
    defaultColDef = colDef(
      align = "center",
      style = function(value) {
        # Default fallback styles
        styles <- list(fontSize = "11px", color = "#f0f0f0", fontWeight = "normal", opacity = 1)
        
        if (is.character(value)) {
          # 1. Gold-color and bold any value containing the asterisk (*)
          if (grepl("\\*", value)) {
            styles$color <- "#ffdc55"
            styles$fontWeight <- "bold"
          }
          
          # 2. Fade out any cells containing the pipe (|)
          if (grepl("\\|", value)) {
            styles$opacity <- 0.4 
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
          style = list(paddingLeft = "10px", fontWeight = "500", whiteSpace = "nowrap")
        ),
        TOTAL = colDef(
          name = "TOT", 
          width = 45, 
          style = list(background = "#ffdc55", color = "#252628", fontWeight = "bold")
        ),
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
      # 1. APPLY THE INTERNAL DASHED LINE TO EVERY COLUMN BY DEFAULT
      style = htmlwidgets::JS("
        function(rowInfo, column, tableState) {
          const index = rowInfo.index;
          
          // Clear default theme border styles completely
          const baseStyle = { fontSize: '10px', borderBottom: 'none' };
          
          if (index > 0) {
            // If it's a new POT block, draw a solid white line all the way across
            if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
              baseStyle.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
            } else {
              // Otherwise, draw the light dashed internal line
              baseStyle.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
            }
          }
          return baseStyle;
        }
      ")
    ),
    
    columns = list(
      POTN = colDef(
        name = "POT", 
        width = 40,
        # 2. OVERRIDE POT COLUMN TO WIPE OUT INTERNAL LINES COMPLETELY
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            
            if (index === 0 || tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
              return { 
                fontWeight: 'bold',
                borderBottom: 'none',
                // Keep the solid boundary line at the start of a block
                borderTop: index > 0 ? '1px solid rgba(255, 255, 255, 0.4)' : 'none'
              }
            }
            
            // Wipe out internal text and ALL internal lines (dashed & solid) inside POT column
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
        # We need to mix the base border style with the custom alignment padding
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const style = { fontSize: '10px', paddingLeft: '10px', fontWeight: '500', borderBottom: 'none' };
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            return style;
          }
        ")
      ),
      PCKA = colDef(name = "PICKS", width = 50),
      GLSA = colDef(
        name = "GF", 
        width = 30,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const potGF = rowInfo.row['POTN'];
            const valGF = rowInfo.row['GLSA'];
            
            const style = { fontSize: '10px', borderBottom: 'none' };
            
            // Handle cross-column border calculations
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            // Highlighting Logic
            const valuesGF = tableState.pageRows
              .filter(function(r) { return r['POTN'] === potGF; })
              .map(function(r) { return r['GLSA']; });
              
            const maxGF = Math.max.apply(null, valuesGF);
            
            if (valGF === maxGF && valuesGF.some(function(v) { return v > 0; })) { 
              style.backgroundColor = '#ffdc55';
              style.fontWeight = 'bold';
              style.color = '#333';
            }
            return style;
          }
        ")
      ),
      JKCNT = colDef(
        name = "JOKERS", 
        width = 60,
        # Re-attaching the yellow color while letting defaultColDef handle the lines
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const style = { color: '#ffdc55', fontWeight: 'bold', borderBottom: 'none' };
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            return style;
          }
        ")
      ),
      PCKD = colDef(name = "PICKS", width = 50),
      GLSD = colDef(
        name = "GA",
        width = 30,
        style = htmlwidgets::JS("
          function(rowInfo, column, tableState) {
            const index = rowInfo.index;
            const potGLSD = rowInfo.row['POTN'];
            const valGLSD = rowInfo.row['GLSD'];
            
            const style = { fontSize: '10px', borderBottom: 'none' };
            
            if (index > 0) {
              if (tableState.pageRows[index - 1]['POTN'] !== rowInfo.row['POTN']) {
                style.borderTop = '1px solid rgba(255, 255, 255, 0.4)';
              } else {
                style.borderTop = '1px dashed rgba(255, 255, 255, 0.15)';
              }
            }
            
            const valuesGLSD = tableState.pageRows
              .filter(function(r) { return r['POTN'] === potGLSD; })
              .map(function(r) { return r['GLSD']; });
              
            const maxGLSD = Math.max.apply(null, valuesGLSD);
            
            if (valGLSD === maxGLSD) { 
              style.backgroundColor = '#ffdc55';
              style.fontWeight = 'bold';
              style.color = '#333';
            }
            return style;
          }
        ")
      ),
      STATUS = colDef(name = "STATUS", width = 150)
    )
  )
}

page <- tags$html(
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700;800&display=swap", rel="stylesheet"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, shrink-to-fit=no"),
    tags$title("World Cup Goal Rush"),
    tags$style(HTML("
      /* Core Global Page Setup */
      body { 
        font-family: 'Montserrat', sans-serif; 
        background-color: #1e1f21; 
        color: #f0f0f0; 
        padding: 20px; 
        margin: 0;
      }
      
      .page-container {
        max-width: 100%;
        margin: 0 auto;
      }
      
      h1 { 
        text-align: center; 
        color: #ffdc55; 
        font-weight: 800; 
        text-transform: uppercase;
        letter-spacing: 1px;
        font-size: 2em;
        margin-bottom: 5px;
      }
      
      .update-time {
        text-align: center; 
        font-size: 0.9em; 
        opacity: 0.8;
        margin-bottom: 30px;
      }

      .table-section {
        display: inline-block;
        text-align: left;
        max-width: 100%;
        margin-bottom: 20px;
      }

      h2 { 
        color: #ffdc55; 
        border-left: 5px solid #ffdc55; 
        padding-left: 15px; 
        text-transform: uppercase; 
        font-size: 1.5em; 
        margin-top: 0;
        margin-bottom: 15px;
        font-weight: 800;
        display: block;
      }

      .rt-container {
        width: 100% !important;
        overflow-x: auto !important;
        display: block !important;
        -webkit-overflow-scrolling: touch;
      }

      .rt-table {
        width: auto !important;
        flex: none !important;
      }

      .rt-th, .rt-header-content {
        color: #ffdc55 !important;
        font-size: 11px !important;
        text-align: center !important;
        justify-content: center !important;
      }

      /* THE FLEXBOX OVERRIDE TRICK */
      .rt-column-group-header {
        background-color: #252628 !important;
        border-bottom: 1px solid #444 !important;
        display: flex !important;
      }

      /* Bulletproof override: forces all extra space to the right */
      .rt-column-group-header-content {
        font-size: 11px !important;
        text-transform: uppercase;
        letter-spacing: 1px;
        color: #ffdc55 !important;
        
        display: inline-block !important;
        margin-right: auto !important; /* Shoves everything to the left */
        padding-left: 12px !important;  /* Controls the indent edge */
      }

      .rt-td {
        white-space: nowrap !important;
        padding: 6px 4px !important;
      }

      /* Updated Footnote Container Layout */
      .footnote-container {
        margin-top: 40px; 
        padding: 20px 10px; 
        border-top: 1px solid #333;
        text-align: center;
      }

      .footnote-line {
        color: #ffdc55; 
        font-size: 0.8em; 
        font-weight: 600; 
        letter-spacing: 1px;
        margin-bottom: 8px; /* Gap between the stacked lines */
        opacity: 0.9;
      }
      
      .footnote-line:last-child {
        margin-bottom: 0;
      }

      @media (max-width: 600px) {
        body { padding: 10px; }
        h1 { font-size: 1.6em; }
        h2 { font-size: 1.3em; padding-left: 10px; }
        .update-time { font-size: 0.8em; }
        .footnote-line { font-size: 0.75em; }
      }
    "))
  ),
  tags$body(
    tags$div(class = "page-container",
             
             tags$h1("WC26 Goal Rush"),
             tags$p(class = "update-time",
                    style = "text-align: center; font-size: 0.85em; opacity: 0.85; margin: 2px 0; font-weight: 500;",
                    paste("Last Updated:", format(Sys.time(), "%H:%M %d %b %Y"))),
             tags$p(class = "fixture-status-line",
                    style = "text-align: center; font-size: 0.85em; opacity: 0.85; margin: 2px 0; font-weight: 500;",
                    tags$span(style = "color: #ffdc55; font-weight: bold;", "Last In: "),
                    last_fix
             ),
             
             tags$p(class = "fixture-status-line",
                    style = "text-align: center; font-size: 0.85em; opacity: 0.85; margin: 2px 0; font-weight: 500; margin-bottom: 25px;",
                    tags$span(style = "color: #ffdc55; font-weight: bold;", "Next Up: "),
                    next_fix
             ),
             
             tags$div(style = "width: 100%; overflow: visible;",
                      tags$div(class = "table-section",
                               tags$h2("Standings"),
                               create_scenario_table(player_rows)
                      )
             ),
             
             # Cleanly Stacked Footnotes Box
             tags$div(class = "footnote-container",
                      tags$div(class = "footnote-line", "* = SELECTED JOKER"),
                      tags$div(class = "footnote-line", "TG = TOTAL PREDICTED GOALS"),
                      tags$div(class = "footnote-line", "GD = DIFFERENCE VS ACTUAL TOTAL"),
                      
                      # The new faded country footnote
                      tags$div(class = "footnote-line", 
                               style = "opacity: 0.45; font-weight: normal; font-size: 0.75em; margin-top: 12px;",
                               "| = COUNTRY ELIMINATED FROM WORLD CUP")
             ),
             
             # NEW: Your secondary summary table block
             tags$div(style = "width: 100%; overflow: visible; margin-top: 30px;",
                      tags$div(class = "table-section",
                               tags$h2("Team Summary"),
                               create_team_summary_table(team_summary)
                      )
             )
    )
  )
)

htmltools::save_html(page, file = "live.html", libdir = "lib")






#while(TRUE){
for(i in 1:1){
  message(paste("Updating at", Sys.time()))
  
  
  # try({
  #   # The "." tells Git to look at EVERYTHING in the folder (html and lib)
  #   system("git add .")
  #   
  #   # Commit only if there are changes (avoids errors if nothing changed)
  #   system('git commit -m "Auto-update scores" --no-verify')
  #   
  #   # Push using our authenticated remote
  #   system("git push origin main --quiet")
  # }, silent = FALSE)
  # 
  # message(paste("Successfully updated at", Sys.time()))
  # 
  # # check the sheet every 60 seconds
  # Sys.sleep(90)
  
}

