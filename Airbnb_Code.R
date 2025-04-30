#######################################
##### Data loading and processing #####
#######################################

# Load packages
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(tibble)
#install.packages("mongolite") # Uncomment if installation is needed
library(mongolite)
#install.packages("scales") # Uncomment if installation is needed
library(scales)
#install.packages("widyr") # Uncomment if installation is needed
library(widyr)
#install.packages("ggraph") # Uncomment if installation is needed
library(ggraph)
#install.packages("igraph") # Uncomment if installation is needed
library(igraph)
#install.packages("topicmodels") # Uncomment if installation is needed
library(topicmodels)


# Connect to mongodb 
connection_string <- 'mongodb+srv://aditya:Hult1234@cluster0.dclr6.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
airbnb <- airbnb_collection$find()

# Filter data to include only properties in the United States
airbnb <- airbnb %>%
  filter(str_ends(address$street, "United States"))

# Add listing ID to each obseravtion
airbnb <- airbnb %>%
  rowid_to_column("listing_id") # Added to provide a unique identifier

#############################
##### Text Tokenization #####
#############################

# Change the "description" column name to "text" in airbnb dataset
colnames(airbnb)[6] <- "text"

# Tokenize the 'text' column and remove stop words
airbnb_token <- airbnb %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

# Create frequency histogram of most common words
freq_hist <-airbnb_token %>%
  count(word, sort=TRUE) %>%
  filter(n>500) %>% # shows only those words with a frequency of 500 or more
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(freq_hist)

# Create tidy format of property_type data 
apartment <- airbnb %>%
  filter(property_type== "Apartment")

tidy_apartment <- apartment %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

house <- airbnb %>%
  filter(property_type== "House")

tidy_house <- house %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


condominium <- airbnb %>%
  filter(property_type== "Condominium")

tidy_condominium <- condominium %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


# Check frequency of words
frequency <- bind_rows(mutate(tidy_apartment, author="Apartment"),
                       mutate(tidy_house, author= "House"),
                       mutate(tidy_condominium, author="Condominium")) %>% 
                        mutate(word=str_extract(word, "[a-z']+")) %>%
                        count(author, word) %>%
                        group_by(author) %>%
                        mutate(proportion = n/sum(n))%>%
                        select(-n) %>%
                        spread(author, proportion) %>%
                        pivot_longer(cols = c(House, Condominium), names_to = "author", values_to = "proportion")

# Plot correlograms
ggplot(frequency, aes(x=proportion, y=`Apartment`, 
                      color = abs(`Apartment`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Apartment", x=NULL)

###############################
##### Correlation testing #####
###############################

# Correlation test
cor.test(data=frequency[frequency$author == "House",],
         ~proportion + `Apartment`)

cor.test(data=frequency[frequency$author == "Condominium",],
         ~proportion + `Apartment`)

#############################
###### N-grams analysis #####
#############################

# Create dataframe with count of 5-word sentences
airbnb_pentagrams <- airbnb %>%
  unnest_tokens(pentagram, text, token = "ngrams", n=5) %>% 
  count(pentagram, sort = TRUE)

# Filter top 20 most frequent pentagrams
top_pentagrams <- airbnb_pentagrams %>%
  slice_max(n, n = 20)

# Plot pentagram analysis
ggplot(top_pentagrams, aes(x = reorder(pentagram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flips the axes
  labs(
    title = "Top 20 Most Frequent 5-Word Sentences (Pentagrams)",
    x = "Pentagram",
    y = "Frequency"
  ) +
  theme_minimal()

# Create dataframe with count of 10-word sentences
airbnb_decagrams <- airbnb %>%
  unnest_tokens(decagram, text, token = "ngrams", n=10) %>% 
  count(decagram, sort = TRUE)

# Filter top 15 most frequent decagrams
top_decagrams <- airbnb_decagrams %>%
  filter(!is.na(decagram)) %>%
  slice_max(n, n = 15)

# Plot decagram analysis
ggplot(top_decagrams, aes(x = reorder(decagram, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +  # Flips the axes from readibility
  labs(
    title = "Top 15 Most Frequent 10-Word Sentences (Decagrams)",
    x = "Decagram",
    y = "Frequency"
  ) +
  theme_minimal()

################################################################
##### Comparing sentiments across different property types #####
################################################################

# Check sentiment analysis for apartment category
apartment_afinn <- tidy_apartment %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN", category = "Apartment")

apartment_bing_and_nrc <- bind_rows(
  tidy_apartment %>%
    inner_join(get_sentiments("nrc")) %>%
    mutate(method = "Bing et al."),
  tidy_apartment %>%
    inner_join(get_sentiments("bing") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")
) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         category = "Apartment")

# Check sentiment analysis for house category
house_afinn <- tidy_house %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN", category = "House")

house_bing_and_nrc <- bind_rows(
  tidy_house %>%
    inner_join(get_sentiments("nrc")) %>%
    mutate(method = "Bing et al."),
  tidy_house %>%
    inner_join(get_sentiments("bing") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")
) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         category = "House")

# Check sentiment analysis for condominium category
condominium_afinn <- tidy_condominium %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN", category = "Condominium")

condominium_bing_and_nrc <- bind_rows(
  tidy_condominium %>%
    inner_join(get_sentiments("nrc")) %>%
    mutate(method = "Bing et al."),
  tidy_condominium %>%
    inner_join(get_sentiments("bing") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")
) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         category = "Condominium")

# Combine sentiment analyses
combined_sentiment <- bind_rows(
  apartment_afinn, apartment_bing_and_nrc,
  house_afinn, house_bing_and_nrc,
  condominium_afinn, condominium_bing_and_nrc
)

# Plot sentiment analysis
ggplot(combined_sentiment, aes(x = method, y = sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~category, ncol = 1, scales = "free_y") +
  labs(
    title = "Sentiment Analysis by Property Type and Method",
    x = "Sentiment Method",
    y = "Net Sentiment Score"
  ) +
  theme_minimal()

################################################
###Pairwise correlations between words #########
################################################

# Create pairwise correlation dataframe
word_cors <- airbnb_token %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, space, sort=TRUE)

# Create bar chart to show correlated words
word_cors %>%
  filter(item1 %in% c("street", "access", "resort", "neighborhood")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

# Create a correlation network
word_cors %>%
  filter(correlation >.85) %>% # Includes results with correlation coefficient above 0.85
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

####################################
### LDA Analysis with Price ########
####################################

# Filter relevant listings and clean price
airbnb_lda_ready <- airbnb %>%
  filter(!is.na(text), !is.na(price)) %>%
  mutate(price = as.numeric(gsub("[$,]", "", price))) %>%
  filter(property_type %in% c("Apartment", "House", "Condominium"))

# Calculate average price by property type
avg_price_by_type <- airbnb_lda_ready %>%
  group_by(property_type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))

print(avg_price_by_type)

# Tokenize text and remove stop words
word_counts <- airbnb_lda_ready %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(listing_id, word, sort = TRUE) %>%
  ungroup()

# Create document-term matrix
airbnb_dtm <- word_counts %>%
  cast_dtm(listing_id, word, n)

# Run LDA with 3 topics
airbnb_lda <- LDA(airbnb_dtm, k = 3, control = list(seed = 123))

# Extract and visualize top words per topic (beta)
airbnb_topics <- tidy(airbnb_lda, matrix = "beta")

top_terms <- airbnb_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic", x = NULL, y = "Probability")

# Get dominant topic for each listing
airbnb_gamma <- tidy(airbnb_lda, matrix = "gamma")

dominant_topic <- airbnb_gamma %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

# Join with price info
price_by_topic <- dominant_topic %>%
  mutate(document = as.integer(document)) %>%
  left_join(airbnb_lda_ready %>% select(listing_id, price),
            by = c("document" = "listing_id"))

# Plot average price per topic
price_by_topic %>%
  group_by(topic) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(topic), y = avg_price, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average Price by Dominant Topic", 
       x = "Topic", 
       y = "Average Nightly Price (USD)")
