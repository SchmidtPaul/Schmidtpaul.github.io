library(ggrepel)
library(here)
library(readxl)
library(tidyverse)

dat <- read_excel(here("src", "workshops.xlsx")) %>% 
  filter(!str_detect(Time, "‑")) %>% 
  mutate(
    year = as.integer(str_sub(Time, 5, 8)),
    dur = as.integer(str_remove(Duration, "h"))
  )

pdat <- dat %>% 
  group_by(year) %>% 
  summarise(
    sum = sum(dur)
  ) %>%
  ungroup()

ggplot(data = pdat) +
  aes(y = sum, x = year) +
  geom_line(size = 1) +
  scale_y_continuous(
    name = "Total hours of workshops",
    limits = c(0, NA),
    breaks = scales::breaks_width(50),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    name = NULL
  ) +
  theme_minimal()