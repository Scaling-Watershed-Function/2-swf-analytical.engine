gc()

librarian::shelf(dplyr, tidytext, tidyverse,
                 widyr,igraph, ggraph, plotly,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm,scales, ggrepel, ggplotify)


# Local Import Path
assets_pubs <- "../1-swf-knowledge.base/assets/data/raw" 
t_df <- as_tibble(read_csv(paste(assets_pubs,"230330_scaling_hbgc_zotero.csv",sep='/'),show_col_types = FALSE))
t_df

# For our analyses below, we won't need DOIs, PMIDs or arXiv IDs. We will focus
# on Title, Abstract, Authors, Journal, Year. To keep column names formatting
# simple, I use lowercase and spaces are added with (_) when needed.

# Since research rabbit pulls information from multiple databases, in some cases
# is possible that the abstracts for the papers are not retrieved. In such cases,
# the value returned would be NA (which is read as a character string and not as
# the typical NA). It could also happen that the abstracts could be partially
# retrieved. To filter these cases out, we will add another column to the data
# frame to count the character length of each abstract, and remove those that are
# less than 20 characters long. Finally, we will add a sequential id and make it
# a factor for visualization purposes.

t_df_c <- t_df %>% 
  distinct() %>% 
  select(Title,`Abstract Note`,Author,`Publication Title`,`Publication Year`) %>% 
  rename(title = Title,
         abstract = `Abstract Note`,
         authors = Author,
         journal = `Publication Title`,
         year = `Publication Year`) %>% 
  mutate(abstract_lenght = nchar(abstract)) %>% 
  filter(abstract_lenght > 20)%>% 
  mutate(id = seq(from =1, to= nrow(.),by=1)) %>% 
  mutate(id = factor(id))

t_df_c

# In this case we went from 104 publication items, to 96 (i.e. 8 items with no
# abstract retrieved.)

# To make sure all words are processed correctly, we need to do some additional 
# cleaning on the text data. That includes unnesting each publication component 
# (pub_comp) into tokens (i.e. single words), performing the cleaning tasks 
# (i.e. singularizing,removing punctuations and digits, removing stop words (a,
# an, by, ...)), and finally putting the tokens back together (i.e. nesting)

# How to unnest and nest text data in using tidytext? check this post: 
# https://stackoverflow.com/questions/49118539/opposite-of-unnest-tokens-in-r

# Choosing analysis level

# You can choose the publication component (pub_comp) you want to focus your 
# analysis on. In this case, our options are title or abstract


# Preparing the data

pub_comp = "title"

pub_dat<- dplyr::select(t_df_c, id, authors, year, journal, all_of(pub_comp)) %>%
  rename(pub_comp_words = all_of(pub_comp)) %>% 
  unnest_tokens(output = word, input = pub_comp_words, drop = FALSE) %>% 
  mutate(word = str_to_lower(word),
         word = singularize(word)) %>%  
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  anti_join(stop_words, by = "word") %>% 
  nest(data = word) %>% 
  mutate(!!paste0(pub_comp) := map_chr(map(data, unlist), paste, collapse = " ")) %>% 
  select(-c(pub_comp_words,data))

# Tokenization

# Our first step to analyze the selected publication component, is to break it 
# into individual words (or tokens) and store the tokens in a new data frame for 
# (pub_tokens). There are several packages you could use to tokenize a piece of 
# text, here we will use the tidytext package for most of our analysis. 

# What the following chunk of code does is: 1) call the pub_dat dataset, 2) break the 
# chunk of (nested) text into tokens (output = word) by using the function unnest_tokens(),
# 3) eliminating duplicated words with distinct(), 4) grouping the tokens (word) by years,
# 5) calculating the frequency of a given token in each year -count(), and adding a new
# column with the numnber of characters -nchar for each token, so we can filter monosyllabes.

pub_tokens <- filter(pub_dat) %>%
  unnest_tokens(output = word, input = pub_comp, drop = FALSE) %>%
  distinct() %>%
  group_by(year) %>%
  count(word, sort = TRUE)%>%
  rename(word_freq = n) %>% 
  summarise(tot_freq = sum(word_freq)) %>%
  left_join(
    .,
    pub_dat %>%
  unnest_tokens(output = word, input = pub_comp, drop = FALSE) %>%
  distinct() %>%
  group_by(year, word) %>%
  count(sort = TRUE) %>%
  rename(word_freq = n) %>% 
  rowwise() %>% dplyr::summarise(max_freq = max(word_freq),groups = word,word_freq) %>% 
  mutate(length = nchar(word)) %>%
  filter(length > 2) %>%
  group_by(year) %>%
  mutate(
    n_rel = word_freq / sum(word_freq),
    csm_rel = cumsum(n_rel),
    pst_rel = (word_freq-1) + csm_rel)%>%
    ungroup(),
  by = "year"
  )
#source: https://chat.openai.com/auth/login?next=/chat/f817b62b-ac51-4121-bad6-f6f0fd8c7d90 


res_plot <- 0.8
depth <- res_plot * nrow(pub_tokens)
low_lim <- min(pub_tokens$year)
upp_lim <- max(pub_tokens$year)
set.seed(27)
word_spacing = pub_tokens$pst_rel + 0.8

p <- pub_tokens[1:depth,] %>% 
  ggplot(aes(x = year, y = pst_rel, color = -year, size = word_freq, label = word)) +
  # geom_point(data = . %>% filter(word_freq < 2), size = 0.5) +
  # geom_jitter(data = . %>% filter(word_freq > 1),height = 0.1, width = 0) +
  # scale_size(range = c(1, 10), name = "Frequency") +
  geom_text(aes(size = word_freq),check_overlap = TRUE, hjust = 0)+
  scale_radius(range = c(3,6))+
  theme_classic() +
  theme(legend.position = "none")+
  scale_y_log10()

# Display the plot
p

#source: https://ggplot2.tidyverse.org/reference/position_jitter.html

# Conceptual maps from n-grams

gram_l = 2
breath = 150
time_window = 2008


n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')

pub_ngrams <- pub_dat %>%
  ungroup() %>%
  unnest_tokens(n_gram, pub_comp, token = "ngrams", n = gram_l) %>%
  separate(n_gram, columns, sep = " ", remove = FALSE) %>%
  # group_by(year) %>% 
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  # count(n_gram, sort = TRUE) %>% 
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(pub_ngrams)

# Create the graph using igraph
ngram_graph <- pub_ngrams %>%
  filter(rank < breath) %>%
  # filter(year > time_window) %>% 
  graph_from_data_frame()

# Convert the graph to a ggplot object using ggraph
ggplot_graph <- ggraph(ngram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "sienna3") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), nudge_x = 0.1, nudge_y = 0.1, size = 2.5) +
  ggtitle("Interactive Network Graph using plotly and ggraph") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size = FALSE)

# Convert the ggplot object to an interactive plot using ggplotly
plotly_graph <- ggplotly(ggplot_graph, tooltip = c("name", "degree"))

# Add plotly click interactions
plotly_graph <- plotly_graph %>% 
  event_register("plotly_click")

# Display the interactive plot
plotly_graph

# This code generate a plotly version but with no links


# Create the graph using visNetworks

library(visNetwork)

# Create a data frame for nodes
node_df <- data.frame(id = V(ngram_graph)$name, 
                      size = 10, 
                      label = V(ngram_graph)$name,
                      title = paste("Rank:", V(ngram_graph)$rank, "<br>",
                                    "Year:", V(ngram_graph)$year))

# Create a data frame for edges
edge_df <- data.frame(from = as.character(get.edgelist(ngram_graph)[,1]), 
                      to = as.character(get.edgelist(ngram_graph)[,2]))

# Create a visNetwork object
visNetwork(nodes = node_df, edges = edge_df, 
           width = "100%", height = "600px") %>%
  
  # Add physics layout and stabilization
  visPhysics(stabilization = TRUE) %>%
  
  # Add labels for nodes
  visNodes(label = "label", title = "title", font = list(size = 15)) %>%
  
  # Customize edges
  visEdges(arrows = "to") %>%
  
  # Add a tooltip
  visInteraction(hover = TRUE,
                 navigationButtons = TRUE) 

# source: https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html


