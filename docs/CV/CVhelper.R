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


# Other Skills ------------------------------------------------------------
for (LANG in langs) {
  d$ski[[LANG]] <- d$ski[[LANG]] %>% 
    group_by(name) %>% 
    summarise(content = str_c(content, collapse = ", ")) %>% 
    ungroup() %>% 
    mutate(content = content %>%
             str_replace_all(c(
               "\\#" = "\\\\#",
               "\\&" = "\\\\&",
               "\\_" = "\\\\_"
             ))) %>% 
    mutate(cat = str_c("\\cvskill {", name,"} \n{", content," } \n")) %>% 
    pull(cat) %>% 
    str_c(collapse = "\n")
  }


# publications ------------------------------------------------------------
d$pub <- here("CV", "publications.bib") %>% 
  vitae::bibliography_entries() %>%
  select(-publisher, -`publisher-place`, -genre, -`title-short`) %>% 
  arrange(desc(issued), id)


# workshops ----------------------------------------------------------------
d$workshops <- read_excel(here("src", "workshops.xlsx")) %>% 
  mutate(Title = Title %>% 
           str_replace_all(c("experimental" = "exp.",
                             "experimentellen" = "exp.",
                             "Naturwissenschaften" = "Naturwiss."))
  ) %>% 
  transmute(
    cat = str_c(
      "\\cvhonor\n",
      "{", Title, "  }\n",
      "{", Location, "}\n",
      "{", Duration, "}\n",
      "{", Time, "  }"
    )
  ) %>% 
  pull(cat) %>% 
  str_c(collapse = "\n\n")
