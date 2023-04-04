### define colors
palette <- c(
  "#5C946E",
  "#F7B538",
  "#9E2B25",
  "#F2F7FF",
  "#082A75"
)

### build plot
model <- fHMM::dax_model_3t
data <- model$data$data
decoding <- model$decoding
library("ggplot2")
p <- ggplot(
    data = data.frame(x = 1:length(data), y = data, state = decoding), 
    aes(x = x, y = data, color = factor(state))
  ) +
  geom_point(size = 0.1) +
  theme_void() +
  theme(legend.position = "none") +
  scale_color_manual(values = palette[1:3]) +
  xlim(50, 3800)
plot(p)

### build sticker
library("hexSticker")
sticker_file <- sticker(
  ### image
  subplot = p,
  s_x = 1,
  s_y = 0.75,
  s_width = 1.75,
  s_height = 0.95,
  ### package name
  package = "fHMM",
  p_x = 1,
  p_y = 1.4,
  p_color = palette[5],
  p_family = "Aller_Rg",
  p_fontface = "plain",
  p_size = 30,
  ### sticker
  h_size = 1.2,
  h_fill = palette[4],
  h_color = palette[5],
  spotlight = FALSE,
  l_x = 0.9,
  l_y = 1.4,
  l_width = 2,
  l_height = 1,
  l_alpha = 0.8,
  white_around_sticker = FALSE,
  ### URL
  url = "loelschlaeger.de/fHMM",
  u_x = 1,
  u_y = 0.1,
  u_color = palette[5],
  u_family = "Aller_Rg",
  u_size = 6.8,
  u_angle = 30,
  ### save file
  filename = "sticker/fHMM_sticker.png",
  asp = 1,
  dpi = 300
)
plot(sticker_file)

