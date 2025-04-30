# Load packages
library(shiny)
library(tidytext)
library(dplyr)
library(stringr)
library(mongolite)
library(ggplot2)
library(scales)
library(widyr)
library(ggraph)
library(igraph)
library(tidyr)
library(DT)
library(plotly)

# Create UI features
ui <- fluidPage(
  titlePanel("Airbnb Text Analysis Dashboard"),
  fluidRow(
    column(4,
           selectInput("sentiment_method", "Sentiment Method:", 
                       choices = c("AFINN", "Bing", "NRC"))
    ),
    column(4,
           sliderInput("min_count", "Min Word Count:", min = 100, max = 1000, value = 500)
    ),
    column(4,
           sliderInput("ngram_n", "N-Gram Size:", min = 2, max = 10, value = 5),
           sliderInput("cor_threshold", "Correlation Threshold:", min = 0.1, max = 1.0, value = 0.7, step = 0.05)
    )
  ),
  fluidRow(
    column(6, plotlyOutput("freqPlot")),
    column(6, plotlyOutput("ngramPlot"))
  ),
  fluidRow(
    column(4, plotlyOutput("sentimentPlot")),
    column(4, plotlyOutput("ldaTopTermsPlot")),
    column(4, plotlyOutput("avgPriceByTopicPlot"))
  ),
  fluidRow(
    column(12, plotOutput("correlationNetwork"))
  )
)

# Create server inputs and outputs
server <- function(input, output, session) {
  connection_string <- 'mongodb+srv://aditya:Hult1234@cluster0.dclr6.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
  airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
  airbnb <- airbnb_collection$find()
  airbnb <- airbnb %>%
    rowid_to_column("listing_id") %>%
    filter(str_ends(address$street, "United States")) %>%
    filter(!is.na(description) & description != "")
  colnames(airbnb)[5] <- "text"
  
  airbnb <- airbnb %>%
    mutate(price = as.numeric(gsub("[$,]", "", price)))
  
  airbnb_token <- airbnb %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  tidy_tokens <- function(df) {
    df %>% unnest_tokens(word, text) %>% anti_join(stop_words)
  }
  tidy_apartment <- tidy_tokens(filter(airbnb, property_type == "Apartment"))
  tidy_house <- tidy_tokens(filter(airbnb, property_type == "House"))
  tidy_condominium <- tidy_tokens(filter(airbnb, property_type == "Condominium"))
  
  output$freqPlot <- renderPlotly({
    data <- airbnb_token %>%
      count(word, sort = TRUE) %>%
      filter(n >= input$min_count) %>%
      mutate(word = reorder(word, n))
    gg <- ggplot(data, aes(word, n, text = paste("Word:", word, "Count:", n))) +
      geom_col(fill = "skyblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Word Frequency", x = NULL, y = "Count")
    ggplotly(gg, tooltip = "text")
  })
  
  output$ngramPlot <- renderPlotly({
    col_name <- paste0("ngram_", input$ngram_n)
    airbnb_ngrams <- airbnb %>%
      unnest_tokens(output = !!col_name, input = text, token = "ngrams", n = input$ngram_n) %>%
      filter(!is.na(!!sym(col_name)), !!sym(col_name) != "NA") %>%
      count(!!sym(col_name), sort = TRUE) %>%
      slice_max(n, n = 20)
    gg <- ggplot(airbnb_ngrams, aes(x = reorder(!!sym(col_name), n), y = n, text = !!sym(col_name))) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Top 20", input$ngram_n, "-grams"), x = "Phrase", y = "Count")
    ggplotly(gg, tooltip = "text")
  })
  
  sentiment_data <- reactive({
    method <- input$sentiment_method
    get_sentiment <- function(tidy_df, method) {
      if (method == "AFINN") {
        tidy_df %>%
          inner_join(get_sentiments("afinn")) %>%
          summarise(sentiment = sum(value)) %>%
          mutate(method = "AFINN")
      } else {
        lexicon <- if (method == "Bing") "bing" else "nrc"
        tidy_df %>%
          inner_join(get_sentiments(lexicon)) %>%
          count(sentiment) %>%
          pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
          mutate(sentiment = positive - negative,
                 method = method)
      }
    }
    bind_rows(
      get_sentiment(tidy_apartment, method = method) %>% mutate(category = "Apartment"),
      get_sentiment(tidy_house, method = method) %>% mutate(category = "House"),
      get_sentiment(tidy_condominium, method = method) %>% mutate(category = "Condominium")
    )
  })
  
  output$sentimentPlot <- renderPlotly({
    gg <- ggplot(sentiment_data(), aes(x = category, y = sentiment, fill = category, text = paste("Category:", category, "Sentiment:", sentiment))) +
      geom_col(show.legend = FALSE) +
      theme_minimal() +
      labs(title = paste(input$sentiment_method, "Sentiment by Property Type"), y = "Net Sentiment")
    ggplotly(gg, tooltip = "text")
  })
  
  output$ldaTopTermsPlot <- renderPlotly({
    lda_ready <- airbnb %>%
      filter(!is.na(text), !is.na(price), property_type %in% c("Apartment", "House", "Condominium"))
    
    word_counts <- lda_ready %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(listing_id, word, sort = TRUE) %>%
      ungroup()
    
    dtm <- word_counts %>%
      cast_dtm(listing_id, word, n)
    
    lda_model <- LDA(dtm, k = 3, control = list(seed = 123))
    
    top_terms <- tidy(lda_model, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(5, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      mutate(term = reorder_within(term, beta, topic))
    
    gg <- ggplot(top_terms, aes(term, beta, fill = factor(topic), text = paste("Term:", term, "Beta:", round(beta, 4)))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~topic, scales = "free", ncol = 1) +
      coord_flip() +
      scale_x_reordered() +
      labs(title = "Top Terms per Topic", x = NULL, y = "Probability") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
  })
  
  output$avgPriceByTopicPlot <- renderPlotly({
    lda_ready <- airbnb %>%
      filter(!is.na(text), !is.na(price), property_type %in% c("Apartment", "House", "Condominium"))
    
    word_counts <- lda_ready %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(listing_id, word, sort = TRUE) %>%
      ungroup()
    
    dtm <- word_counts %>%
      cast_dtm(listing_id, word, n)
    
    lda_model <- LDA(dtm, k = 3, control = list(seed = 123))
    
    gamma <- tidy(lda_model, matrix = "gamma")
    
    dominant_topic <- gamma %>%
      group_by(document) %>%
      slice_max(gamma, n = 1) %>%
      ungroup()
    
    price_by_topic <- dominant_topic %>%
      mutate(document = as.integer(document)) %>%
      left_join(lda_ready %>% select(listing_id, price), by = c("document" = "listing_id")) %>%
      group_by(topic) %>%
      summarise(avg_price = mean(price, na.rm = TRUE))
    
    gg <- ggplot(price_by_topic, aes(x = factor(topic), y = avg_price, fill = factor(topic), text = paste("Topic:", topic, "Avg Price:", round(avg_price, 2)))) +
      geom_col(show.legend = FALSE) +
      labs(title = "Average Price by Dominant Topic", x = "Topic", y = "Average Nightly Price (USD)") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
  })
  
  output$correlationNetwork <- renderPlot({
    word_cors <- airbnb_token %>%
      group_by(word) %>%
      filter(n() >= 5) %>%
      pairwise_cor(word, listing_id, sort = TRUE)
    
    graph_data <- word_cors %>%
      filter(correlation > input$cor_threshold) %>%
      graph_from_data_frame()
    
    ggraph(graph_data, layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
      geom_node_point(color = "lightgreen", size = 6) +
      geom_node_text(aes(label = name), repel = TRUE) +
      theme_void()
  })
}

# Run app
shinyApp(ui = ui, server = server)
