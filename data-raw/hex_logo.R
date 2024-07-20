library(hexSticker)

logo_image <- fs::path("data-raw", "meteoland_logo_image.png")
sticker(
  logo_image,
  package = "meteoland", p_size = 18, p_y = 1.55, p_color = "#D4AA00",
  s_x = 0.98, s_y = .70, s_width = .76,
  filename = fs::path("data-raw", "meteoland.png"),
  #   url = "emf.creaf.cat", u_size = 6, u_color = "#BFD77A", u_y = .2, u_x = 1.2,
  h_fill = "#6C5d53", h_color = "#D4AA00"
)
