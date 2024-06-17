library(googlesheets4) # gs4_auth()
library(here)
library(lubridate)
library(readxl)
library(RefManageR)
library(tidyverse)
library(vitae)

# import ------------------------------------------------------------------
path <- here("CV", "CVcontent.xlsx")
d <- list()
d$job <- read_excel(path, sheet = "Job")
d$edu <- read_excel(path, sheet = "Education")
d$ski <- read_excel(path, sheet = "Skills")

langs <- c("eng", "ger")

# format ------------------------------------------------------------------
for (item in names(d)) {
  temp <- list()

  for (LANG in langs) {
    NOTLANG <- langs[langs != LANG]
    LOCALE <- c(eng = "en_US.UTF-8", ger = "de_DE.UTF-8")[[LANG]]

    temp[[LANG]] <- d[[item]] %>%
      select(-contains(NOTLANG)) %>%
      rename_all( ~ str_remove(., "\\_.*"))

    hasdate <- all(c("from", "to") %in% names(temp[[LANG]]))

    if (hasdate) {
      temp[[LANG]] <- temp[[LANG]] %>%
        mutate(across(from:to,
                      ~ str_c(
                        month(., TRUE, TRUE, locale = LOCALE), " ", year(.)
                      ))) %>%
        mutate(
          dates = case_when(
            is.na(to) & LANG == "ger" ~ str_c("Seit ", from),
            is.na(to) & LANG == "eng" ~ str_c("Since ", from),
            TRUE ~ str_c(from, " - ", to)
          ),
          .keep = "unused",
          .before = "details"
        )
    }
  }
  d[[item]] <- temp
}


# # Other Skills ------------------------------------------------------------
# for (LANG in langs) {
#   d$ski[[LANG]] <- d$ski[[LANG]] %>% 
#     group_by(name) %>% 
#     summarise(content = str_c(content, collapse = ", ")) %>% 
#     ungroup() %>% 
#     mutate(content = content %>%
#              str_replace_all(c(
#                "\\#" = "\\\\#",
#                "\\&" = "\\\\&",
#                "\\_" = "\\\\_"
#              ))) %>% 
#     mutate(cat = str_c("\\cvskill {", name,"} \n{", content,"} \n")) %>% 
#     pull(cat) %>% 
#     str_c(collapse = "\n")
#   }
# 
# 
# # publications ------------------------------------------------------------
# d$pub <- here("CV", "publications.bib") %>% 
#   vitae::bibliography_entries() %>%
#   select(-publisher, -`publisher-place`, -genre, -`title-short`) %>% 
#   arrange(desc(issued), id)
# 
# 
# # workshops ----------------------------------------------------------------
# token <- readRDS(".secrets/gs4_token.rds")
# gs4_auth(token = token)
# 
# sheet_url <- "https://docs.google.com/spreadsheets/d/1wSK6RiqaAWFqxaAd8VlXA4v0LQib0CevTopTAMFHzgs/edit?usp=sharing"
# 
# d$workshops <- read_sheet(sheet_url) %>%
#   select(
#     Title,
#     Location = Label_Location,
#     Duration = Label_h,
#     Time = Label_Time
#   ) 
# 
# # Keep maximum of 50 rows in workshops table
# d$workshops <- bind_rows(
#   d$workshops %>% slice(1:45), # first 43 rows
#   as_tibble(t(rep("...", ncol(d$workshops))), .name_repair = ~ names(d$workshops)), # A '...' row
#   d$workshops %>% slice((nrow(d$workshops) - 5):nrow(d$workshops)) # Last 6 rows
# )
# 
# d$workshops <- d$workshops %>% 
#   transmute(
#     cat = str_c(
#       "\\cvhonor\n",
#       "{", Title, "  }\n",
#       "{", Location, "}\n",
#       "{", Duration, "}\n",
#       "{", Time, "  }"
#     )
#   ) %>% 
#   pull(cat) %>% 
#   str_c(collapse = "\n\n")
# 
# 
# 
