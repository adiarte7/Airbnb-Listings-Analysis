# Airbnb-Listings-Analysis

An interactive R Shiny dashboard is created that performs advanced text analysis on Airbnb listings in the United States, aimed at uncovering business insights related to pricing, sentiment, and listing language patterns.

## Overview

This project explores over 5,000 Airbnb listings by applying natural language processing (NLP) techniques including:

- **Tokenization & Frequency Analysis**
- **Sentiment Analysis (AFINN, Bing, NRC lexicons)**
- **N-Gram Modeling (2- to 10-grams)**
- **Word Correlation Networks**
- **Topic Modeling (LDA)**
- **Text-Driven Revenue Analysis**

All these insights are integrated into an interactive Shiny dashboard where users can manipulate input parameters to extract dynamic insights.

## Key Features

- **Sentiment Comparison**: Analyze net positivity across property types using 3 lexicons.
- **N-Gram Analysis**: Explore common phrases in listing descriptions.
- **Topic Modeling (LDA)**: Identify latent themes in descriptions and relate them to average listing price.
- **Word Correlation Network**: Visualize semantically linked word clusters and uncover multilingual trends.
- **Dynamic Visualization**: Powered by `ggplot2`, `plotly`, and `ggraph` for interactivity.

## Dataset

Pulled from the MongoDB Atlas sample database:

```
Database: sample_airbnb  
Collection: listingsAndReviews 
```

## Author

Aditya Arte  
MBA + Business Analytics Dual Degree Candidate  
Hult International Business School  
[LinkedIn Profile](https://www.linkedin.com/in/aditya-arte/)
