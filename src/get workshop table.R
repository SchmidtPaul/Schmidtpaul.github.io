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
    Org = Firma,
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

# Transform data - use iconify shortcodes for both language and platform
workshops <- workshops %>%
  mutate(
    # Transform language codes to flag images with tooltips
    Lang_iconify = case_when(
      Lang == "Ger" ~ '<img src="img/flag_ger.png" title="German" style="width:22px; vertical-align: middle;">',
      Lang == "Eng" ~ '<img src="img/flag_usa.png" title="English" style="width:22px; vertical-align: middle;">',
      TRUE ~ Lang
    ),
    # Transform platform codes to icons with tooltips and links
    Plat_fa = case_when(
      Plat == "R" ~
        '<a title="R" href="https://www.r-project.org/">{{< fa brands r-project size=xl >}}</a>',
      Plat == "Python" ~
        '<a title="Python" href="https://www.python.org/">{{< fa brands python size=xl >}}</a>',
      Plat == "SAS" ~ '<a title="SAS" href="https://www.sas.com/"><img src="img/logo_sas.png" style="width:20px; height:20px; vertical-align: middle; filter: hue-rotate(-90deg);"></a>',
      TRUE ~ Plat
    ),
    # Transform org/firma to icon with tooltip and link
    Org_icon = case_when(
      Org == "BioMath" ~ '<a title="BioMath" href="https://www.biomath.de/"><img src="img/logo_biomath.png" style="width:20px; height:20px; vertical-align: middle;"></a>',
      Org == "Paul" ~ '<img src="img/logo_ps.svg" title="Freelance" style="width:24px; height:24px; vertical-align: middle;">',
      Org == "Hohenheim" ~ '<a title="Uni Hohenheim" href="https://www.uni-hohenheim.de/"><img src="img/logo_hohenheim.svg" style="width:20px; height:20px; vertical-align: middle; "></a>',
      TRUE ~ Org
    ),
    # Store duration for bars
    Duration_orig = Duration
  ) %>%
  # Replace original columns with transformed versions
  select(-Lang, -Plat, -Org) %>%
  rename(Lang = Lang_iconify, Plat = Plat_fa, Org = Org_icon)

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
    Org = "Orga",
    Location = "Location",
    Duration = "Duration"
  ) %>%

  # Reorder columns: Time, Title, Lang, Plat, Org, Location, Duration
  cols_move(
    columns = c(Lang, Plat, Org),
    after = Title
  ) %>%

  # Column widths
  cols_width(
    Time ~ px(70),
    Title ~ px(350),
    Lang ~ px(50),
    Plat ~ px(50),
    Org ~ px(50),
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
    columns = c(Lang, Plat, Org, Duration)
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
  fmt_markdown(columns = c(Title, Lang, Plat, Org, Location, Duration)) %>%

  # Replace NA with empty string
  sub_missing(columns = everything(), missing_text = "") %>%

  # Remove helper column
  cols_hide(columns = Duration_orig)
