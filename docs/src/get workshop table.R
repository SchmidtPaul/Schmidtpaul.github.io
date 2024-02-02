pacman::p_load(formattable, googlesheets4, gtExtras, here, htmltools, kableExtra, tidyverse)


# get table ---------------------------------------------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1wSK6RiqaAWFqxaAd8VlXA4v0LQib0CevTopTAMFHzgs/edit?usp=sharing"
# gs4_auth()
raw <- read_sheet(sheet_url, sheet = "Main")

workshops <- raw %>% 
  transmute(
    Time = Label_Time,
    "Workshop Title" = Title,
    Lang = Language,
    Plat = Platform,
    Location = Label_Location,
    Duration = as.integer(h),
    ID = ID
  ) 


# Evaluations -------------------------------------------------------------
IDs_with_eval <- list.files(here::here("src", "eval"), ".pdf$") %>% 
  str_remove(".pdf$") %>% 
  str_remove("eval_")

workshops <- workshops %>% 
  mutate(
    Eval = case_when(
      ID %in% IDs_with_eval ~ str_c('<a href="https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/src/eval/eval_', ID,'.pdf" target="_blank">view</a>'),
      .default = ""
    )
  ) %>% 
  select(-ID)


# Flags -------------------------------------------------------------------
flag_urls <- c(Ger = "https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/img/flag_ger.png?raw=true",
               Eng = "https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/img/flag_usa.png?raw=true")

imgs_flag <- unname(flag_urls[workshops$Lang])
workshops$Lang <- ""


# Zoom --------------------------------------------------------------------
workshops <- workshops %>% 
  mutate(Location = str_replace(Location, "zoom", "<img src='https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/img/logo_zoom.png?raw=true' style='width:30px; height:6px;'>")) 


# Platform ----------------------------------------------------------------
platform_urls <- c(R = "https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/img/logo_rstudio.png?raw=true",
                   Python = "https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/img/logo_python.png?raw=true",
                   SAS = "https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/img/logo_sas.png?raw=true")

imgs_platform <- unname(platform_urls[workshops$Plat])
workshops$Plat <- ""


# Duration Bar ------------------------------------------------------------
workshops <- workshops %>%
  mutate(
    Dur_lab = str_c("> ", Duration, "h</span>"),
    Dur_val = str_remove(Duration, "h") %>% as.integer(),
  ) %>% 
  mutate(
    Duration = pmin(25, Dur_val) %>% 
      color_bar("lightgrey")(.) %>% 
      str_replace_all(">\\d+</span>", Dur_lab)
  ) %>% 
  mutate(Duration = case_when(
    Dur_val > 24 ~ str_replace(Duration, "lightgrey", "darkgrey"),
    .default = Duration
  )) %>% 
  select(-starts_with("Dur_"))


workshops <- workshops %>%
  kbl(align = c("r", "l", "r", "r", "r", "l"), escape = F) %>%
  kable_minimal(
    full_width = T,
    font_size = 10,
    bootstrap_options = c("hover", "condensed")
  ) %>%
  column_spec(1, width = "4em") %>%
  column_spec(2, width = "26em") %>% 
  column_spec(3, width = "2em",
              image = spec_image(imgs_flag, 50, 50)) %>%
  column_spec(4, width = "2em",
              image = spec_image(imgs_platform, 50, 50)) %>% 
  column_spec(5, width = "17em") %>%
  column_spec(6, width = "4em") %>% 
  column_spec(7, width = "2em")

# gt instead of kable -----------------------------------------------------

# # gt ----------------------------------------------------------------------
# workshops <- raw %>%
#   gt() %>% 
#   tab_options(table_body.hlines.color = "transparent")
# 
# 
# # Language ----------------------------------------------------------------
# workshops <- workshops %>%
#   text_transform(
#     locations = cells_body(columns = Lang),
#     fn = function(x) {
#       x %>%
#         str_replace("Eng", str_c(local_image(
#           filename = here::here("img", "flag_usa.png"),
#           height = 15
#         ))) %>%
#         str_replace("Ger", str_c(local_image(
#           filename = here::here("img", "flag_ger.png"),
#           height = 15
#         )))
#     }
#   )
# 
# 
# # Platform ----------------------------------------------------------------
# workshops <- workshops %>%
#   text_transform(
#     locations = cells_body(columns = Plat),
#     fn = function(x) {
#       x %>%
#         str_replace("R", str_c(local_image(
#           filename = here::here("img", "logo_rstudio.png"),
#           height = 15
#         ))) %>%
#         str_replace("Python", str_c(local_image(
#           filename = here::here("img", "logo_python.png"),
#           height = 15
#         ))) %>% 
#         str_replace("SAS", str_c(local_image(
#           filename = here::here("img", "logo_sas.png"),
#           height = 15
#         )))
#     }
#   )
# 
# # Zoom logo ---------------------------------------------------------------
# workshops <- workshops %>%
#   text_transform(
#     locations = cells_body(columns = Location),
#     fn = function(x) {
#       x %>%
#         str_replace("zoom", str_c(local_image(
#           filename = here::here("img", "logo_zoom.png"),
#           height = 10
#         )))
#     }
#   )
