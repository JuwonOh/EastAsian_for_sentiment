# EastAsian_for_sentiment

This repository is code and data intended to represent cross-national relations through various sentiment analyzes.

## Using data

This is an analysis of the data of US media companies in China, Korea and Japan. The press used was CNN, Fox News, New York Times, Washington Post, and Wall streat jounal. The article runs from January 1, 2019 to June 31, 2019. The code that crawled the material is on my github.

## Sentiment analysis method

사용한 sentiment analysis 방법은 VADER sentiment analysis와 supervised learning입니다.

- VADER sentiment analysis

VADER Sentiment Analysis (Valence Aware Dictionary and sEntiment Reasoner) is a lexicon and rule-based sentiment analysis tool. The entire article was divided into sentences, and each country or major person was considered a sentence corresponding to each country. The emotion analyzer used VADER Sentiment Analysis based on the emotion dictionary. The VADER Sentiment Analysis is very simple but shows similar feelings in articles written by other media about the same time and the same country.
However, VADER sentiment analysis has a limitation of lexicon-based sentiment analysis. We also use lexicon based on social media. These algorithms are more limited in their use for news data, so we compared the results with sentiment analysis using supervised learning.

- Supervised learning

For supervised learning, we have labeled about 10% of all articles according to their importance. By learning this material, we created a model to classify the sentiment of other articles. After preprocessing the text in the article, to solve the imbalanced data problem using the Synthetic Minority Over-sampling Technique (SMOTE). To classify each document, we found the best model using several classification algorithms provided by the scikit-learn package. The parameters of each model were set via grid search.
