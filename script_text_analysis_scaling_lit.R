require(librarian)
librarian::shelf(plyr, tidytext, tidyverse,
                 widyr,igraph, ggraph,
                 wordcloud, reshape2, graphlayouts,
                 pluralize, quanteda, qgraph, cowplot, readr,
                 ggwordcloud,tm)



# Local Import Path
assets_pubs <- "../1-swf-knowledge.base/assets/pubs" 
t_df <- as_tibble(read_csv(paste(assets_pubs,"scaling_example_background.csv",sep='/'),show_col_types = FALSE))


# Cleaning the data set
pub_dat <- t_df %>%
  select(Title, Abstract,Authors,Journal,Year) %>% 
  rename(title = Title,
         abstract = Abstract,
         authors = Authors,
         journal = Journal,
         year = Year) %>% 
  mutate(id = seq(from =1, to= nrow(t_df),by=1)) %>% 
  mutate(abstract=removeNumbers(abstract)) %>%
  mutate(title = removeNumbers(title)) %>% 
  mutate(abstract=gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                              collapse = '|'), '', abstract)) %>% 
  mutate(title=gsub(paste0('\\b',tm::stopwords("english"), '\\b', 
                              collapse = '|'), '', title)) %>% 
  mutate(id = factor(id))

# Global removal of stopwords: 
#https://stackoverflow.com/questions/64361808/r-remove-stopwords-from-text-without-tokenizing-transforming-data-into-list


# Titles

titl_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = title, drop = FALSE)%>%
  anti_join(stop_words, by = "word")%>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>% 
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

summary(titl_tokens)

# Using ggwordclouds:
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

p <- ggplot(filter(titl_tokens), 
            aes(label = word, 
                size = n, 
                color = as.factor(year))) +
  geom_text_wordcloud(area_corr_power = 1) +
  scale_radius(range = c(0, 20),
               limits = c(0, NA))
p

abst_tokens <- pub_dat %>% 
  ungroup() %>% 
  unnest_tokens(output = word, input = abstract, drop = FALSE)%>%
  anti_join(stop_words, by = "word")%>%
  filter(str_detect(word,"[:alpha:]"))%>%
  rowwise() %>% mutate(word = if_else(word!="data",singularize(word),"data")) %>%
  distinct() %>% 
  group_by(year) %>% 
  count(word, sort = FALSE) %>% 
  mutate(length = nchar(word)) 

summary(abst_tokens)

  p <- ggplot(filter(abst_tokens,n>1), 
              aes(label = word, 
                  size = n, 
                  color = as.factor(year))) +
    geom_text_wordcloud(area_corr_power = 1) +
    scale_radius(range = c(0, 20),
                 limits = c(0, NA)) 
  p

# n-grams
  
#Define the length of the gram
  
gram_l = 2
n_gram <- paste(gram_l,"gram",sep='-')

a <- seq(1:gram_l)
b <- rep("word",times=gram_l)
columns <- paste(b,a,sep = '')


abs_ngrams <- pub_dat %>%
  ungroup() %>%
  filter(str_detect(abstract,"[:alpha:]")) %>%
  unnest_tokens(n_gram, abstract, token = "ngrams", n = gram_l) %>%
  separate(n_gram, columns, sep = " ") %>%
  count(across(all_of(columns), ~.x), sort = TRUE) %>%
  mutate(rank = row_number(),
         total = sum(n),
         t_freq = n/total)
head(abs_ngrams)

n_gram_res <- 0.02

ngram_graph <- abs_ngrams %>%
  filter(rank < n_gram_res*nrow(abs_ngrams)) %>%
  graph_from_data_frame()
ngram_graph

set.seed(2017)

l <- layout_with_fr(ngram_graph)
e <- get.edgelist(ngram_graph,names=FALSE)
m <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(ngram_graph))
deg <- degree(ngram_graph,mode="all")
fsize <- degree(ngram_graph, mode= "all")

#png(filename=paste("assets/NetworkAnalysis_words_",Sys.Date(),".png", sep = ""), res = 100)

plot(ngram_graph,
     layout=m, 
     edge.arrow.size =.05,
     vertex.color = "pink", 
     vertex.size =500,
     vertex.frame.color="deeppink",
     vertex.label.color="black", 
     vertex.label.cex=fsize/2,
     vertex.label.dist=0.6,
     edge.curve = 0.75,
     edge.color="skyblue",
     edge.label.family="Ariahttp://127.0.0.1:35229/graphics/plot_zoom_png?width=1536&height=898l", 
     rescale=F, 
     axes = FALSE, 
     ylim = c(-100,100), 
     xlim = c(-130,130),
     asp =0)


# 4-grams

ab_tetragrams <- pub_dat%>%
  ungroup() %>%
  filter(str_detect(abstract,"[:alpha:]"))%>%
  unnest_tokens(tetragram, abstract, token = "ngrams", n = 4) %>% 
  separate(tetragram,c("word1", "word2","word3","word4"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  filter(!word4 %in% stop_words$word) %>% 
  count(word1, word2, word3,word4,sort = TRUE) #%>%
  # mutate(rank = row_number(),
  #        total=sum(n),
  #        t_freq = n/total)
head(ab_tetragrams)


ab_tetragrams %>% 
  filter(rank < 31) %>% 
  unite(tetragram, word1, word2, word3, word4,sep = " ") %>% 
  ggplot(aes(t_freq, fct_reorder(tetragram, t_freq), fill = t_freq)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Frequency", y = NULL)

tetragram_graph <- ab_tetragrams %>%
  filter(rank < 101) %>%
  graph_from_data_frame()
tetragram_graph

set.seed(2017)

l <- layout_with_fr(tetragram_graph)
e <- get.edgelist(tetragram_graph,names=FALSE)
m <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(tetragram_graph))
deg <- degree(tetragram_graph,mode="all")
fsize <- degree(tetragram_graph, mode= "all")

#png(filename=paste("assets/NetworkAnalysis_words_",Sys.Date(),".png", sep = ""), res = 100)

plot(tetragram_graph,
     layout=m, 
     edge.arrow.size =.05,
     vertex.color = "pink", 
     vertex.size =500,
     vertex.frame.color="deeppink",
     vertex.label.color="black", 
     vertex.label.cex=fsize/2,
     vertex.label.dist=0.6,
     edge.curve = 0.75,
     edge.color="skyblue",
     edge.label.family="Ariahttp://127.0.0.1:35229/graphics/plot_zoom_png?width=1536&height=898l", 
     rescale=F, 
     axes = FALSE, 
     ylim = c(-90,120), 
     xlim = c(-130,130),
     asp =0)