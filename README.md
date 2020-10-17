---
title: "Introduction to tidytext"
author: "Tenzin"
date: "10/17/2020"
---

```{r}
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books
```

```{r}
## # A tibble: 73,422 x 4
##    text                    book                 line chapter
##    <chr>                   <fct>               <int>   <int>
##  1 "SENSE AND SENSIBILITY" Sense & Sensibility     1       0
##  2 ""                      Sense & Sensibility     2       0
##  3 "by Jane Austen"        Sense & Sensibility     3       0
##  4 ""                      Sense & Sensibility     4       0
##  5 "(1811)"                Sense & Sensibility     5       0
##  6 ""                      Sense & Sensibility     6       0
##  7 ""                      Sense & Sensibility     7       0
##  8 ""                      Sense & Sensibility     8       0
##  9 ""                      Sense & Sensibility     9       0
## 10 "CHAPTER 1"             Sense & Sensibility    10       1
## # … with 73,412 more rows
```

To work with this as a tidy dataset, we need to restructure it as one-token-per-row format. The unnest_tokens function is a way to convert a dataframe with a text column to be one-token-per-row:

```{r}
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books
```

```{r}
## # A tibble: 725,055 x 4
##    book                 line chapter word       
##    <fct>               <int>   <int> <chr>      
##  1 Sense & Sensibility     1       0 sense      
##  2 Sense & Sensibility     1       0 and        
##  3 Sense & Sensibility     1       0 sensibility
##  4 Sense & Sensibility     3       0 by         
##  5 Sense & Sensibility     3       0 jane       
##  6 Sense & Sensibility     3       0 austen     
##  7 Sense & Sensibility     5       0 1811       
##  8 Sense & Sensibility    10       1 chapter    
##  9 Sense & Sensibility    10       1 1          
## 10 Sense & Sensibility    13       1 the        
## # … with 725,045 more rows
```

This function uses the tokenizers package to separate each line into words. The default tokenizing is for words, but other options include characters, ngrams, sentences, lines, paragraphs, or separation around a regex pattern.

Now that the data is in one-word-per-row format, we can manipulate it with tidy tools like dplyr. We can remove stop words (accessible in a tidy form with the function get_stopwords()) with an anti_join.

We can also use count to find the most common words in all the books as a whole.

Sentiment analysis can be done as an inner join. Sentiment lexicons are available via the get_sentiments() function. Let’s look at the words with a positive score from the lexicon of Bing Liu and collaborators. What are the most common positive words in Emma?

```{r}
positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_books %>%
  filter(book == "Emma") %>%
  semi_join(positive) %>%
  count(word, sort = TRUE)
```

Or instead we could examine how sentiment changes during each novel. Let’s find a sentiment score for each word using the same lexicon, then count the number of positive and negative words in defined sections of each novel.

```{r}
library(tidyr)
bing <- get_sentiments("bing")

janeaustensentiment <- tidy_books %>%
  inner_join(bing) %>%
  count(book, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
```

Now we can plot these sentiment scores across the plot trajectory of each novel.

```{r}
library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
```


## Most common positive and negative words

One advantage of having the data frame with both sentiment and word is that we can analyze word counts that contribute to each sentiment.

```{r}
bing_word_counts <- tidy_books %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts
```

This can be shown visually, and we can pipe straight into ggplot2 because of the way we are consistently using tools built for handling tidy data frames.

```{r}
bing_word_counts %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")
```

This lets us spot an anomaly in the sentiment analysis; the word “miss” is coded as negative but it is used as a title for young, unmarried women in Jane Austen’s works. If it were appropriate for our purposes, we could easily add “miss” to a custom stop-words list using bind_rows.

## Looking at units beyond just words

Lots of useful work can be done by tokenizing at the word level, but sometimes it is useful or necessary to look at different units of text. For example, some sentiment analysis algorithms look beyond only unigrams (i.e. single words) to try to understand the sentiment of a sentence as a whole. These algorithms try to understand that

I am not having a good day.

is a sad sentence, not a happy one, because of negation. The Stanford CoreNLP tools and the sentimentr R package are examples of such sentiment analysis algorithms. For these, we may want to tokenize text into sentences.

```{r}
PandP_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")
```

Let’s look at just one.

```{r}
PandP_sentences$sentence[2]
```

The sentence tokenizing does seem to have a bit of trouble with UTF-8 encoded text, especially with sections of dialogue; it does much better with punctuation in ASCII.

Another option in unnest_tokens is to split into tokens using a regex pattern. We could use this, for example, to split the text of Jane Austen’s novels into a data frame by chapter.

```{r}
austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())
```

We have recovered the correct number of chapters in each novel (plus an “extra” row for each novel title). In this data frame, each row corresponds to one chapter.

Near the beginning of this vignette, we used a similar regex to find where all the chapters were in Austen’s novels for a tidy data frame organized by one-word-per-row. We can use tidy text analysis to ask questions such as what are the most negative chapters in each of Jane Austen’s novels? First, let’s get the list of negative words from the Bing lexicon. Second, let’s make a dataframe of how many words are in each chapter so we can normalize for the length of chapters. Then, let’s find the number of negative words in each chapter and divide by the total words in each chapter. Which chapter has the highest proportion of negative words?

```{r}
bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1)
```

