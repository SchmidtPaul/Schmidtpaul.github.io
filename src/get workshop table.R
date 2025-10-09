pacman::p_load(
  fontawesome,
  formattable,
  googlesheets4,
  gt,
  here,
  htmltools,
  tidyverse
)

token <- readRDS(".secrets/gs4_token.rds")
gs4_auth(token = token)

# get table ---------------------------------------------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1wSK6RiqaAWFqxaAd8VlXA4v0LQib0CevTopTAMFHzgs/edit?usp=sharing"
raw <- read_sheet(sheet_url, sheet = "Main")

workshops <- raw %>%
  transmute(
    Time = Label_Time,
    Title = Title,
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
    Title = case_when(
      ID %in% IDs_with_eval ~
        str_c(
          Title,
          ' <a href="https://github.com/SchmidtPaul/Schmidtpaul.github.io/blob/main/src/eval/eval_',
          ID,
          '.pdf" title="view evaluation" target="_blank" style="color: #00923f;"> ',
          as.character(fa("ranking-star")),
          '</a>'
        ),
      .default = Title
    )
  ) %>%
  select(-ID)

# Transform data to image filenames BEFORE creating GT table
workshops <- workshops %>%
  mutate(
    # Transform language codes to image filenames
    Lang_img = case_when(
      Lang == "Ger" ~ "flag_ger.png",
      Lang == "Eng" ~ "flag_usa.png",
      TRUE ~ Lang
    ),
    # Transform platform codes to image filenames
    Plat_img = case_when(
      Plat == "R" ~ "logo_rstudio.png",
      Plat == "Python" ~ "logo_python.png",
      Plat == "SAS" ~ "logo_sas.png",
      TRUE ~ Plat
    ),
    # Store duration for bars
    Duration_orig = Duration
  ) %>%
  # Replace original columns with image filename columns
  select(-Lang, -Plat) %>%
  rename(Lang = Lang_img, Plat = Plat_img)

# Create GT table
workshops_gt <- workshops %>%
  gt() %>%

  # Basic table options
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(3),
    column_labels.padding = px(6),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    column_labels.font.size = px(11),
    row.striping.include_table_body = FALSE,
    table_body.hlines.style = "hidden",
    table_body.vlines.style = "hidden",
    table.width = pct(100),
    row.striping.background_color = "transparent",
    table_body.hlines.color = "transparent",
    table_body.vlines.color = "transparent",
    table_body.border.top.color = "transparent",
    table_body.border.bottom.color = "transparent"
  ) %>%

  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body()
  ) %>%

  # Add gray line under column headers
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "#dee2e6",
      weight = px(1),
      style = "solid"
    ),
    locations = cells_column_labels()
  ) %>%

  # Column labels
  cols_label(
    Time = "Time",
    Title = "Workshop Title",
    Lang = "Lang",
    Plat = "Plat",
    Location = "Location",
    Duration = "Duration"
  ) %>%

  # Column widths
  cols_width(
    Time ~ px(70),
    Title ~ px(350),
    Lang ~ px(50),
    Plat ~ px(50),
    Location ~ px(280),
    Duration ~ px(90)
  ) %>%

  # Column alignment
  cols_align(
    align = "left",
    columns = c(Time, Title, Location)
  ) %>%
  cols_align(
    align = "center",
    columns = c(Lang, Plat, Duration)
  ) %>%

  # FORMAT IMAGES using fmt_image - the proper GT way!
  fmt_image(
    columns = Lang,
    path = "img",
    height = 15,
    width = 20
  ) %>%

  fmt_image(
    columns = Plat,
    path = "img",
    height = 20,
    width = 20
  ) %>%

  # Transform location column - replace zoom with local image
  text_transform(
    locations = cells_body(columns = Location),
    fn = function(x) {
      str_replace_all(
        x,
        "zoom",
        '<img src="img/logo_zoom.png" style="width:30px; height:6px; vertical-align: middle;">'
      )
    }
  ) %>%

  # Create duration bars using background styling
  text_transform(
    locations = cells_body(columns = Duration),
    fn = function(x) {
      duration_vals <- workshops$Duration_orig
      map2_chr(
        duration_vals,
        duration_vals,
        ~ {
          # Calculate bar width percentage (max 24h = 100%)
          bar_width <- pmin(100, (.x / 24) * 100)

          # Choose color based on duration
          bar_color <- if (.x > 24) "#a9a9a9" else "#d3d3d3"

          # Create HTML with background gradient
          glue::glue(
            '<div style="background: linear-gradient(to right, {bar_color} {bar_width}%, transparent {bar_width}%); padding: 2px 4px; text-align: center; min-height: 20px; display: flex; align-items: center; justify-content: center;">{.x}h</div>'
          )
        }
      )
    }
  ) %>%

  # Enable HTML rendering for columns that need it
  fmt_markdown(columns = c(Title, Location, Duration)) %>%

  # Remove helper column
  cols_hide(columns = Duration_orig) %>%

  cols_move(
    columns = c(Lang, Plat),
    after = Title
  )

# Return the table
workshops_gt
