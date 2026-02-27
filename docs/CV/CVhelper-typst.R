# ---- CVhelper-typst.R (mit Google-Sheets-Import für Workshops) ----------------
# Läuft VOR dem Rendern der .qmd und bereitet alles vor.
# WICHTIG: Dein manuell formatierter 'dates'-String bleibt unberührt.
# Nur falls 'dates' fehlt, wird er minimal aus from/to gebaut (Fallback).

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(here)
library(tibble)
library(googlesheets4)

# -------------------- Import aus Excel --------------------
xlsx_path <- here("CV", "CVcontent.xlsx")

raw <- list(
  job = read_excel(xlsx_path, sheet = "Job"),
  edu = read_excel(xlsx_path, sheet = "Education"),
  ski = read_excel(xlsx_path, sheet = "Skills")
)

langs <- c("eng", "ger")

# -------------------- Helpers --------------------
# Spalten *_eng / *_ger in je eine Sprach-Variante splitten,
# Spaltennamen ohne Suffix zurücklassen.
split_lang <- function(df, langs = c("eng", "ger")) {
  out <- list()
  for (LANG in langs) {
    NOTLANG <- setdiff(langs, LANG)
    out[[LANG]] <- df %>%
      select(-matches(paste0("(_", paste(NOTLANG, collapse = "|_"), ")$"))) %>%
      rename_with(~ sub("_(eng|ger)$", "", .x))
  }
  out
}

# Fallback: Nur wenn 'dates' fehlt, aus from/to einen simplen String bauen.
# Keine fancy Formatierung, kein Locale – rein defensiv, damit resume_entry() nicht crasht.
ensure_dates <- function(df, lang = c("eng", "ger")) {
  lang <- match.arg(lang)
  if ("dates" %in% names(df) || !all(c("from", "to") %in% names(df))) {
    return(df)
  }
  # from/to als Date versuchen; wenn nicht möglich, als Character übernehmen
  as_date_safe <- function(x) suppressWarnings(as.Date(x))
  fmt <- function(d) {
    d <- as_date_safe(d)
    ifelse(is.na(d), NA_character_, format(d, "%Y-%m"))
  }
  df %>%
    mutate(
      dates = case_when(
        is.na(.data$to) & lang == "ger" ~ paste0("Seit ", fmt(.data$from)),
        is.na(.data$to) & lang == "eng" ~ paste0("Since ", fmt(.data$from)),
        TRUE ~ paste(fmt(.data$from), "-", fmt(.data$to))
      )
    )
}

# Wide-Format für Details-Spalten (detail1, detail2, …)
make_wide <- function(df, group_vars, detail_col = "details") {
  if (!detail_col %in% names(df)) {
    # minimaler Fallback-Name
    detail_col <- dplyr::first(c("details", "detail", "content")[
      c("details", "detail", "content") %in% names(df)
    ])
    if (is.na(detail_col)) {
      stop(
        "Keine Detail-Spalte gefunden (erwartet: 'details', 'detail' oder 'content')."
      )
    }
  }
  df %>%
    group_by(across(any_of(group_vars))) %>% # robust, falls z. B. 'dates' mal fehlt
    mutate(detail_id = paste0("detail", row_number())) %>%
    ungroup() %>%
    pivot_wider(names_from = detail_id, values_from = all_of(detail_col))
}

# -------------------- Sprach-Splits --------------------
d <- list(
  job = split_lang(raw$job, langs),
  edu = split_lang(raw$edu, langs),
  ski = split_lang(raw$ski, langs)
)

# -------------------- CV-Objekt aufbauen --------------------
cv <- list()

for (LANG in langs) {
  job_lang <- ensure_dates(d$job[[LANG]], lang = LANG)
  edu_lang <- ensure_dates(d$edu[[LANG]], lang = LANG)

  cv[[LANG]] <- list(
    job_wide = make_wide(
      job_lang,
      group_vars = c("role", "company", "loc", "dates"),
      detail_col = "details"
    ),
    edu_wide = make_wide(
      edu_lang,
      group_vars = c("degree", "uni", "loc", "dates"),
      detail_col = "details"
    ),
    skills_flat = d$ski[[LANG]] %>%
      group_by(name) %>%
      summarise(content = paste(content, collapse = ", "), .groups = "drop") %>%
      transmute(
        title = name,
        location = "",
        date = "",
        description = content
      )
  )
}

# -------------------- Publications-Pfad --------------------
cv$pub_path <- here("CV", "publications.bib")

# -------------------- Workshops aus Google Sheets --------------------
library(googlesheets4)

token <- readRDS(here::here(".secrets", "gs4_token.rds"))
gs4_auth(token = token)

sheet_url <- "https://docs.google.com/spreadsheets/d/1wSK6RiqaAWFqxaAd8VlXA4v0LQib0CevTopTAMFHzgs/edit?usp=sharing"

ws <- read_sheet(sheet_url, sheet = "Main")

cv$workshops <- ws %>%
  transmute(
    title = Title,
    location = coalesce(Label_Location, ""),
    date = coalesce(Label_Time, "")
  )

# Gruppierung pro Jahr (nur für Darstellung)
if (nrow(cv$workshops) > 0) {
  cv$workshops_wide <- cv$workshops %>%
    mutate(
      year = substr(date, 1, 4),
      detail = paste0(date, " — ", title, " — ", location)
    ) %>%
    group_by(year) %>%
    mutate(detail_id = paste0("detail", row_number())) %>%
    ungroup() %>%
    select(year, detail_id, detail) %>%
    tidyr::pivot_wider(names_from = detail_id, values_from = detail) %>%
    transmute(
      title = year,
      location = "",
      date = "",
      description = "",
      dplyr::across(starts_with("detail"))
    )
} else {
  cv$workshops_wide <- tibble::tibble(
    title = character(),
    location = character(),
    date = character()
  )
}


# -------------------- Emit-Funktionen für die .qmd --------------------
cv_emit_experience <- function(lang = "eng") {
  df <- cv[[lang]]$job_wide
  detail_cols <- grep("^detail\\d+$", names(df), value = TRUE)
  typstcv::resume_entry(
    df,
    title = "role",
    location = "loc",
    date = "dates",
    description = "company",
    details = detail_cols
  )
}

cv_emit_education <- function(lang = "eng") {
  df <- cv[[lang]]$edu_wide
  detail_cols <- grep("^detail\\d+$", names(df), value = TRUE)
  typstcv::resume_entry(
    df,
    title = "degree",
    location = "loc",
    date = "dates",
    description = "uni",
    details = detail_cols
  )
}

cv_emit_skills <- function(lang = "eng") {
  df <- cv[[lang]]$skills_flat
  typstcv::resume_entry(
    df,
    title = "title",
    location = "location",
    date = "date",
    description = "description"
  )
}

cv_emit_workshops <- function() {
  df <- cv$workshops_wide
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }
  detail_cols <- grep("^detail\\d+$", names(df), value = TRUE)
  typstcv::resume_entry(
    df,
    title = "title",
    location = "location",
    date = "date",
    description = "description",
    details = detail_cols
  )
}

# -------------------- Persistieren --------------------
saveRDS(cv, here("CV", "CV_ready.rds"))
