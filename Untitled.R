library(ggplot2)
library(magick)

# Set parameters for word cloud
min.freq <- 10
max.words <- 200
width <- 1000
height <- 1000
colors <- brewer.pal(8, "Dark2")

# Create a list of words for each time point
words_list <- list(
  t1 = c("apple", "banana", "orange", "kiwi", "mango"),
  t2 = c("car", "bus", "train", "bike", "plane"),
  t3 = c("book", "pen", "pencil", "paper", "notebook")
)

# Loop through each time point and create a word cloud
for (i in seq_along(words_list)) {
  words <- words_list[[i]]
  freq <- sample(20:100, length(words), replace = TRUE)
  df <- data.frame(words, freq)
  
  # Create word cloud
  p <- ggplot(df, aes(x = 0, y = 0, label = words, size = freq)) +
    geom_text(color = sample(colors, length(words), replace = TRUE), fontface = "bold") +
    scale_size(range = c(10, 60), breaks = seq(0, max(freq), by = 20), name = "Frequency") +
    labs(title = paste0("Word Cloud ", i), x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 24))
  ggsave(paste0("wordcloud_", i, ".png"), p, width = 8, height = 8, dpi = 300)
  
  # Convert to square shape
  img <- image_read(paste0("wordcloud_", i, ".png"))
  img <- image_resize(img, "1000x1000^")
  img <- image_extent(img, 1000, 1000, "+0+0")
  image_write(img, path = paste0("wordcloud_", i, "_square.png"))
}