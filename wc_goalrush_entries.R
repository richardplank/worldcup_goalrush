# Required packages
require(data.table)
require(tidyverse)
require(stringr)
require(formattable)
require(googledrive)
require(googlesheets4)

# De-authorize public sheet
gs4_deauth()

# sheet URLs
entry_sheet_url <- "https://docs.google.com/spreadsheets/d/15Bcq2OOLA2A8otEMWntKETEZLN3eDDheBYIyGVPF5qg/edit?gid=0#gid=0"

entries <- read_sheet(entry_sheet_url) |> 
  pivot_longer(
    cols = c("P1A", "P1D", "P2A", "P2D", "P3A", "P3D", "P4A", "P4D", "P5A", "P5D", "P6A", "P6D",
             "P7A", "P8A", "P9A", "P10A", "P11A", "P12A"),
    names_to = "POTCD",
    values_to = "TEAM"
  ) |> 
  # flag joker
  mutate(
    JKMULT = case_when(
      POTCD == paste0("P",substr(JKPOT, 5, 5),"A") ~ case_when(
        JKPOT %in% c("Pot 3", "Pot 4") ~ 2,
        JKPOT %in% c("Pot 5", "Pot 6") ~ 3,
        JKPOT %in% c("Pot 7", "Pot 8") ~ 4,
        JKPOT %in% c("Pot 9", "Pot 10") ~ 5,
        JKPOT %in% c("Pot 11", "Pot 12") ~ 6
      ),
      TRUE ~ 1
    )
  )

save(entries, file = "lib/entries")
