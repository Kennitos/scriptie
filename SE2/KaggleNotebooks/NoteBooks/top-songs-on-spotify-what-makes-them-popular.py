---
title: "Analysis of Top Songs of 2017 on Spotify"
author: "Cihan Oklap"
output: 
 html_document:
  toc: true
  code_folding: hide
---

# Introduction

In this kernel, I'll take a look at the audio features of the tracks in **Spotify's Top Songs of 2017** playlist and try to highlight the common patterns behind the audio features of these songs. 

# Read In and Explore the Data

First off, let’s import our data and get an idea of what we’re working with.

```{r read, echo=TRUE, message=FALSE, warning=FALSE}

library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(formattable)
library(wordcloud)
library(RWeka)
library(qdap)
library(tm)

spotify_data <- read_csv('../input/top-tracks-of-2017/featuresdf.csv')
daily_spotify <- read_csv('../input/spotifys-worldwide-daily-song-ranking/data.csv')
```

```{r structure, echo=TRUE, message=FALSE, warning=FALSE}
glimpse(spotify_data)
```

```{r summary, echo=TRUE}
summary(spotify_data)
```

I understand that the data is a tidy one and doesn't contain any NA's. However, for the simplicity, I'll convert `duration_ms` variable in seconds rather than miliseconds.

```{r , echo=TRUE, message = FALSE}
spotify_data$duration_ms <- round(spotify_data$duration_ms / 1000)
colnames(spotify_data)[15] <- "duration"
```

Now, I can start the analysis.

# Data Analysis

## Artists dominating the Top List

Let's determine the artists who have more than one song on the Top 100 Songs List  

```{r , echo=TRUE, message = FALSE}
top_artists <- spotify_data %>%
    group_by(artists)  %>%
    summarise(n_apperance = n()) %>%
    filter(n_apperance > 1) %>%
    arrange(desc(n_apperance))

top_artists$artists <- factor(top_artists$artists, levels = top_artists$artists[order(top_artists$n_apperance)]) # in order to visualise the list in descending order 

ggplot(top_artists, aes(x = artists, y = n_apperance)) +
    geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) + 
    labs(title = "Top Artists of 2017", x = "Artists", y = "Number of Apperance on the Top 100") +
    theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
    geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
    coord_flip()
```

So, It seems like **Ed Sheeran** and **The Chainsmokers** rocked the 2017 with 4 different songs on the Top list, while **Drake**  and **Martin Garrix** are following them with 3 different songs.

## Top Artists by the Total Playing Time

As **Fernando Lasso** suggested on the comments, I also would like to list the top artists by total playing time. Since both of the data sources don't contain such information, I'll use the `Streams` variable of `daily spotify` and `duration` variable of `spotify_data`. 

```{r , echo=TRUE, message = FALSE}
us_daily_spotify <- daily_spotify %>%
    filter(Region == "us") %>%
    group_by(`Track Name`) %>%
    summarise(total_streams = sum(Streams))

names(us_daily_spotify)[1] <- paste("name") # in order to make the joining easier

top_by_playtime <- spotify_data %>%
    left_join(us_daily_spotify, by = "name") %>%
    select(name, artists, duration, total_streams) %>%
    mutate(total_time = duration * total_streams / 60000) # in order to convert seconds into hours

top20_by_playtime <-  top_by_playtime %>%
    group_by(artists)  %>%
    summarise(n_time = sum(total_time)) %>%
    arrange(desc(n_time)) %>%
    top_n(20)

top20_by_playtime$artists <- factor(top20_by_playtime$artists, levels = top20_by_playtime$artists [order(top20_by_playtime$n_time)]) # in order to visualise the list in descending order

ggplot(top20_by_playtime, aes(x=artists, y=n_time, color=artists)) +
    geom_point(size=3) + 
    geom_segment(aes(x=artists,xend=artists, y=0, yend=n_time)) +
    labs(title = "Top Artists of 2017 in US by Playing time", x='',y='') +
    theme_bw() +
    theme(legend.position = 'none', plot.title = element_text(size=17,hjust = -0.7, face = "bold"), axis.title.y = element_text(face = "bold"), axis.title.x = element_text(angle = 120)) +
    coord_flip()

```

Now **Post Malone** just started to feel like a rockstar! If we take the playing time into consideration, **Ed Sheeran** still has the top spot. But **Post Malone** and **Kendrick Lamar** leapt forward now.

### How did Ed Sheeran's Songs perform in 2017 on daily basis?

Since 2017 was an unforgettable year for **Ed Sheeran**, I'd like to see how his songs perform in US on each day of 2017.

```{r , echo=TRUE, message = FALSE}
ed_sheeran_daily <- daily_spotify %>%
    filter(Region == "us", Artist == "Ed Sheeran", Position <= 100)

formatted_ed <- ed_sheeran_daily %>%
    group_by(`Track Name`) %>%
    summarise(n_daily = n()) %>%
    arrange(desc(n_daily))

formattable(formatted_ed)
```

**Ed Sheeran** had 19 different songs displayed on the Top 100 Songs List in US at least for one day. Since it might be confusing to plot all of that, I'll only take the ones which **stayed on the Top 100 List for at least 20 different days in 2017**.

```{r , echo=TRUE, message = FALSE}

ed_20 <- ed_sheeran_daily %>%
    group_by(`Track Name`) %>%
    summarise(n_daily = n()) %>%
    filter(n_daily >= 20) %>%
    select(`Track Name`)

ed_20 <- ed_20 %>% collect %>% .[["Track Name"]] # in order to turn that tibble into a list

ed_daily_plot <- ed_sheeran_daily %>%
    filter(`Track Name` %in% ed_20) %>%
    ggplot(aes(x = Date, y = Position, col = `Track Name`)) + 
    geom_point(alpha = 0.7, size = 3) +
    scale_y_reverse(breaks = seq(0,100,10)) +
    scale_x_date() +
    ggtitle("Ed Sheeran on Top 100 Daily List in US") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    theme(legend.title=element_blank())

ed_daily_plot
```

Wow, there are lots of different comments about this graph!

* First, we can see why it's not surprising that *Shape of You* is the best on the Top Songs List in 2017. 
* Secondly, March was the month of **Ed Sheeran** which is probably the result of the his last album release, which is on *March 3, 2017*.
* Thirdly, the late rise of **Perfect** is probably because of its video clip, which is released on *November 3* and **Perfect Duet** is an example of its popularity.
* And lastly, except the songs above, **Castle on the Hill** and **Galway Girl** can be considered to be stayed longer than the other songs on the album.

## Correlation between variables

In order to understand the correlation between variables, I'll use `corrplot` function, which is one of the base data visualization functions.

```{r , echo=TRUE, message = FALSE}

library(corrplot)
spotify_data_num <- spotify_data[,-(1:3)]
mtCor <- cor(spotify_data_num)
corrplot(mtCor, method = "ellipse", type = "upper", tl.srt = 45)
```

* It seems like `energy` and `loudness` are highly positively correlated.
* Also, `valence` is positively correlated with `danceability` and `energy`. Considering happy songs make people energetic and want to dance, the correlation make a lot sense. 
*Interestingly, `speechiness` and `loudness` are negatively correlated with each other. 


## Common features of Top Songs

Let's figure out the variables that are common among the Top Songs List.

### Common Keys

Let's determine the most common keys among Top 100 Songs.

Rather than using the numeric quantities, I'll convert values of `keys` into their original symbols.  

```{r , echo=TRUE, message = FALSE}
spotify_data$key <- as.character(spotify_data$key)
spotify_data$key <- revalue(spotify_data$key, c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))

song_keys <- spotify_data %>%
    group_by(key) %>%
    summarise(n_key = n()) %>%
    arrange(desc(n_key))

song_keys$key <- factor(song_keys$key, levels = song_keys$key[order(song_keys$n_key)]) # in order to visualise the keys in descending order

ggplot(song_keys, aes(x = reorder(key,-n_key), y = n_key, fill = reorder(key,-n_key))) +
    geom_bar(stat = "identity") +
    labs(title = "Distribution of the Keys of Top Songs", x = "Keys", y = "Count of Keys on the Top 100") +
    geom_text(aes(label=n_key), position = position_stack(vjust = 0.8)) +
    theme_bw() +
    theme(plot.title = element_text(size=15,face = "bold"), axis.title = element_text(size=12)) +
    theme(legend.position="none")
```

So, it seems like the most common key among top tracks is **C♯,D♭**; while **D♯,E♭** is the least preferred in the Top Songs list.

### Density Plots of Correlated Variables

We've already determined that `energy`,`valence` and `danceability` are positively correlated; but this time, let's see how these variables are distributed over 100 songs.

```{r , echo=TRUE, message = FALSE}

correlated_density <- ggplot(spotify_data) +
    geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
    geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
    geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
    scale_x_continuous(name = "Energy, Valence and Danceability") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot of Energy, Valence and Danceability") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12)) +
    theme(legend.title=element_blank()) +
    scale_fill_brewer(palette="Accent")

correlated_density
```

As it can be seen on the graph, since these variables are positively correlated and have limited between **(0,1)**, the distribution of these variables look similar to each other.

### The More Loud the More Popular

After we acknowledge the density of `energy` variable, we can guess the density of `loudness` must be high, with the help of Correlation Table above. Let's see how loud the Top 100 Songs of 2017 are.

```{r , echo = TRUE, message = FALSE}
loudness_density <- ggplot(spotify_data) +
    geom_density(aes(loudness, fill ="loudness")) + 
    scale_x_continuous(name = "Loudness") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot of Loudness") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
            text = element_text(size = 12)) +
    theme(legend.title=element_blank()) +
    scale_fill_brewer(palette="Paired")

print(loudness_density)

```

As we've guessed, the Top 100 Songs are mostly loud.

## Understanding the Most Important Factor

In order to understand what makes Number 1 song better than the Number 100, I'll add a "standings" column to the dataset, and look into the features that differentiates the best of the top songs lists from the rest of the list.

```{r , echo=TRUE}
library(rpart)
library(rpart.plot)
spotify_data_num$standing <- c(1:100)
tree_model <- rpart(standing ~ ., data = spotify_data_num)
rpart.plot(tree_model, box.palette = "GnBu")
```

That means, the songs of which the key values are less than 3.5 **("C","C♯,D♭","D","D♯,E♭"**), `duration` is more than **204** seconds and `valence` is more than **0.68** have highest chance to be around the top of the Top 100 list; while the songs of which the key values are more than 3.5, `valence` is less than **0.73**, `duration` is less than **244** seconds and `speechiness` is less than **0.05** have the highest chance to be around the bottom of the Top 100 list.

# Text Mining

Although I've learned a lot with data analysis, I also like to dive into text mining to reach more interesting findings.


## Most Featured Artists of Top Songs 2017 in US

Collaborations in songs are getting popular and popular every day. Features make a song powerful, richer and get more attention from different audiences. That's why, I'd like to see who are the most popular artists on the ‘feat’ songs with a good-looking word cloud. I'll take the songs which entered the Top 100 Songs List on any day of 2017 in US.

First, I'll prepare the functions I need for cleaning and preprocessing the text.

```{r , echo=TRUE, message = FALSE, warnings = FALSE}

qdap_clean <- function(x) { 
    x <- replace_abbreviation(x) 
    x <- replace_contraction(x) 
    x <- replace_number(x) 
    x <- replace_ordinal(x) 
    x <- tolower(x) 

    return(x) 

    }



tm_clean <- function(corpus) {
    corpus <- tm_map(corpus, content_transformer(strip), char.keep="$")
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords,
                     c(stopwords("en"), "with", "feat", "ty"))
    return(corpus)

    }



tokenizer <- function(x) 
    NGramTokenizer(x, Weka_control(min = 2, max = 3))

    

us_top100_titles <- daily_spotify %>%
    filter(Region == "us", Position <= 100) %>%
    select(`Track Name`) %>%
    filter(grepl('feat|with', `Track Name`))

us_top100_titles <- us_top100_titles[!duplicated(us_top100_titles$`Track Name`),]

us_top100_titles <- qdap_clean(us_top100_titles)

us_top100_corp <- VCorpus(VectorSource(us_top100_titles))

us_top100_corp_tm <- tm_clean(us_top100_corp)



us_top100_tdm <- TermDocumentMatrix(us_top100_corp_tm, control = list(tokenize = tokenizer))

us_top100_tdm_m <- as.matrix(us_top100_tdm)



us_top100_freq <- rowSums(us_top100_tdm_m)

wordcloud(names(us_top100_freq),us_top100_freq, min.freq = 2, max.words = 100, scale = c(3,.3), colors = c("grey80","darkgoldenrod1","tomato"))

```



Seems like **Ty Dolla $ign**, **Lil Wayne** and **Lil Yachty** are the kings of the 'feat' songs. That was kind of predictable to see rappers on the top :)





***This was my first analysis and I'm still trying to make it better. That's why, please feel free to give any feedback!***