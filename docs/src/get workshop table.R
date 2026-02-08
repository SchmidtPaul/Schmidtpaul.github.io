pacman::p_load(
  fontawesome,
  formattable,
  gt,
  here,
  htmltools,
  tidyverse,
  yaml
)

# get table from YAML -----------------------------------------------------
yaml_path <- "C:/GitHub/workshop-management/workshops.yml"
raw_yaml <- yaml::read_yaml(yaml_path)

raw <- map_dfr(raw_yaml$workshops, function(ws) {
  tibble(
    ID = ws$id %||% NA_character_,
    Title = ws$title %||% NA_character_,
    Language = ws$language %||% NA_character_,
    Platform = ws$platform %||% NA_character_,
    client = ws$client %||% NA_character_,
    location = ws$location %||% NA_character_,
    date_from = ws$date_from %||% NA_character_,
    hours = ws$hours %||% NA_real_
  )
})

# Derive label fields (previously computed in Google Sheets)
workshops <- raw %>%
  transmute(
    Time = case_when(
      !is.na(date_from) ~ format(as.Date(date_from), "%Y %b"),
      .default = NA_character_
    ),
    Title = Title,
    Lang = Language,
    Plat = Platform,
    Location = case_when(
      !is.na(client) & !is.na(location) & location != client ~
        paste(client, "via", location),
      !is.na(client) ~ client,
      .default = NA_character_
    ),
    Duration = as.integer(hours),
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
    # Transform language codes to iconify flag shortcodes (square flags)
    Lang_iconify = case_when(
      Lang == "Ger" ~ '{{< iconify circle-flags:de size=xl >}}',
      Lang == "Eng" ~ '{{< iconify circle-flags:us size=xl >}}',
      TRUE ~ Lang
    ),
    # Transform platform codes to Font Awesome shortcodes with links and larger size
    Plat_fa = case_when(
      Plat == "R" ~
        '[{{< fa brands r-project size=xl >}}](https://www.r-project.org/)',
      Plat == "Python" ~
        '[{{< fa brands python size=xl >}}](https://www.python.org/)',
      Plat == "SAS" ~ '[{{< fa chart-simple size=xl >}}](https://www.sas.com/)',
      TRUE ~ Plat
    ),
    # Store duration for bars
    Duration_orig = Duration
  ) %>%
  # Replace original columns with transformed versions
  select(-Lang, -Plat) %>%
  rename(Lang = Lang_iconify, Plat = Plat_fa)

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

  # Reorder columns: Time, Title, Lang, Plat, Location, Duration
  cols_move(
    columns = c(Lang, Plat),
    after = Title
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
  fmt_markdown(columns = c(Title, Lang, Plat, Location, Duration)) %>%

  # Remove helper column
  cols_hide(columns = Duration_orig)
