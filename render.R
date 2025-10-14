quarto::quarto_render("CVger.qmd")
# shell.exec(here::here("CVger.pdf"))

quarto::quarto_render("CVeng.qmd")
# shell.exec(here::here("CVeng.pdf"))

quarto::quarto_render("index.qmd")
# shell.exec(here::here("docs", "index.html"))
