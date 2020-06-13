

# Install packages, only if it is not installed
library(shiny)
library(shinyjs)
library(tidyverse)
library(tm)
library(SnowballC)
library(dplyr)

# Import data
billboard <- read.csv("billboard_lyrics_1964-2015.csv")

# Checking missing/NA values and converting columns Rank, year and source from numeric to factor
sapply(billboard, function(x) sum(is.na(x))) #There are 374 NA values found in the dataset

# Tab 1
tab1source <- billboard[c(1,3)]
rankMatrix = data.frame(matrix(ncol = 3, nrow = 0))
ranks <- unique(billboard$Rank)
for (r in ranks) {
  artists <- tab1source[tab1source$Rank == r,] 
  unique_artists <- unique(artists$Artist)
  for (artist in unique_artists) {
    hits <- NROW(artists[artists$Artist == artist,])
    rankMatrix <- rbind(rankMatrix, data.frame(r, artist, hits))
  }
}

# Tab 2a output
tab2source <- na.omit(billboard[c("Lyrics", "Year")])
tab2source$Lyrics <- iconv(tab2source$Lyrics, "UTF-8", "UTF-8",sub='')
corp <- Corpus(VectorSource(tab2source$Lyrics))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(corp)
freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df <- data.frame(word=names(freq), freq=freq)

#Tab 2b output
yearwiselyrics <- function(year) {
  data2b <- billboard[billboard$Year == year, 4:5]
  data2b$Lyrics <- iconv(data2b$Lyrics, "UTF-8", "UTF-8", sub='')
  corp2 <- Corpus(VectorSource(data2b$Lyrics))
  corp2 <- tm_map(corp2, stripWhitespace)
  corp2 <- tm_map(corp2, removeWords, stopwords("english"))
  tdm2 <- TermDocumentMatrix(corp2)
  freq_words2 <- sort(rowSums(as.matrix(tdm2)), decreasing = TRUE)
  freq_words2.df <- data.frame(word=names(freq_words2), freq=freq_words2)
  return(freq_words2.df)
}

# Tab 3a output
tab3source <- na.omit(billboard[c("Song", "Year")])
corp2 <- Corpus(VectorSource(tab3source$Song))
tdm1 <- TermDocumentMatrix(corp2, control = list(stripWhitespace = TRUE,
                                                 stopwords = TRUE))
freq2 <- sort(rowSums(as.matrix(tdm1)), decreasing = TRUE)
freq2.df <- data.frame(word=names(freq2), freq = freq2)

#Tab 3b output
yearwisetitles <- function(year) {
  data3b <- billboard[billboard$Year == year, 2:4]
  data3b$Lyrics <- iconv(data3b$Song, "UTF-8", "UTF-8", sub='')
  corp3 <- Corpus(VectorSource(data3b$Song))
  corp3 <- tm_map(corp3, stripWhitespace)
  corp3 <- tm_map(corp3, removeWords, stopwords("english"))
  tdm3 <- TermDocumentMatrix(corp3)
  freq_words3 <- sort(rowSums(as.matrix(tdm3)), decreasing = TRUE)
  freq_words3.df <- data.frame(word=names(freq_words3), freq=freq_words3)
  return(freq_words3.df)
}

#Tab 4a output
yearwiseartists <- function(year) {
  tab4source <- billboard[billboard$Year == year, 3]
  df <- sort(table(tab4source), decreasing = TRUE)
  df <- as.data.frame(df)
  colnames(df) <- c("artist", "hits")
  return(df)
}

#Tab 4b output
pop_artists <- sort(table(billboard$Artist), decreasing = TRUE)
pop_artists <- as.data.frame(pop_artists)
colnames(pop_artists) <- c("artist","hits")
pop_artists <- filter(pop_artists, pop_artists$hits >= 12)

popartistsfn <- function(artist) {
  src <- billboard[billboard$Artist == artist, 3:4]
  src <- src %>% group_by(Year) %>% tally()
  dest <- data.frame(year=min(billboard$Year):max(billboard$Year), n=0)
  dest$n <- src$n[match(dest$year, src$Year)]
  dest$n[is.na(dest$n)] <- 0
  return(dest)
  }

# UI
ui <- fluidPage(
  useShinyjs(),
  navbarPage("",
             tabPanel("Qn 1: Rank Analysis of Artists", 
                      div(id = "Sidebar", 
                          sidebarPanel(selectInput(inputId = "rank", label = "Select the Rank", 
                                                   choices = c(min(billboard$Rank):max(billboard$Rank)))
                          )),
                      mainPanel(plotOutput("rankChart"))
             ),
             
             tabPanel("Qn 2a: Analysis of frequent words in Song Lyrics",
                      mainPanel(plotOutput("lyrics"))
             ),
             
             tabPanel("Qn 2b: Yearwise analysis of frequent words in Song Lyrics",
                      div(id = "Sidebar2", 
                          sidebarPanel(selectInput(inputId = "year11", label = "Select Year 1", 
                                                   choices = c(min(billboard$Year):max(billboard$Year))),
                                       selectInput(inputId = "year12", label = "Select Year 2", 
                                                   choices = c(min(billboard$Year):max(billboard$Year)))
                          )),
                      mainPanel(plotOutput("yearwiseLyrics1"),
                                plotOutput("yearwiseLyrics2"))
             ),
             
             tabPanel("Qn 3a: Analysis of frequent words in Song Titles",
                      mainPanel(plotOutput("titles"))
             ),
             
             tabPanel("Qn 3b: Yearwise Analysis of frequent words in Song Titles",
                      div(id = "Sidebar3", 
                          sidebarPanel(selectInput(inputId = "year21", label = "Select Year 1", 
                                                   choices = c(min(billboard$Year):max(billboard$Year))),
                                       selectInput(inputId = "year22", label = "Select Year 2", 
                                                   choices = c(min(billboard$Year):max(billboard$Year)))
                          )),
                      mainPanel(plotOutput("yearwisetitles1"),
                                plotOutput("yearwisetitles2"))
             ),
             
             tabPanel("Qn 4a: Artists with maximum hits yearwise",
                      div(id = "Sidebar4",
                          sidebarPanel(selectInput(inputId = "year3", label = "Select the Year",
                                                   choices = c(min(billboard$Year):max(billboard$Year)))
                          )),
                      mainPanel(plotOutput("yearwiseartists"))
             ),
             tabPanel("Qn 4b: Analysis of popular artists(atleast 12 hits)",
                      div(id = "Sidebar5",
                          sidebarPanel(selectInput(inputId = "artist", label = "Select the popular artist(artist with atleast 12 hits)",
                                                   choices = pop_artists$artist)
                          )),
                      mainPanel(plotOutput("popartists"))
             )
  )
)

# Server
server <- function(input, output) {
  #Tab 1 Output
  output$rankChart <- renderPlot({
    ggplot(rankMatrix[rankMatrix$r == input$rank,2:3], aes(reorder(artist, -hits), hits)) +
      geom_bar(stat="identity") + xlab("Artists") + ylab("Number of Hits") +
      ggtitle("Bar chart for Artists Vs Number of hits based on Rank") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  #Tab 2a output
  output$lyrics <- renderPlot({
    ggplot(freq.df[1:20,], aes(reorder(word, -freq),freq)) + 
      ggtitle("Bar chart showing Most frequent Words in Song Lyrics Vs their frequency") +
      geom_bar(stat = "identity") + xlab("Words in Song lyrics") + ylab("Frequency")
  })
  
  #Tab 2b output
  output$yearwiseLyrics1 <- renderPlot({
    ggplot(yearwiselyrics(input$year11)[1:20,], aes(reorder(word, -freq), freq)) +
      ggtitle(paste("Words in Song titles Vs Frequency for the year",input$year11)) +
      geom_bar(stat="identity") + xlab("Words in Song titles") + ylab("Frequency")
  })
  output$yearwiseLyrics2 <- renderPlot({
    ggplot(yearwiselyrics(input$year12)[1:20,], aes(reorder(word, -freq), freq)) +
      ggtitle(paste("Words in Song titles Vs Frequency based for the year",input$year12)) +
      geom_bar(stat="identity") + xlab("Words in Song titles") + ylab("Frequency")
  })
  
  #Tab 3a output
  output$titles <- renderPlot({
    ggplot(freq2.df[1:20,], aes(reorder(word, -freq), freq)) +
      ggtitle("Bar chart showing Most frequent Words in Song titles Vs their frequency") +
      geom_bar(stat = "identity") + xlab("Words in Song Titles") + ylab("Frequency")
  })
  
  #Tab 3b output
  output$yearwisetitles1 <- renderPlot({
    ggplot(yearwisetitles(input$year21)[1:20,], aes(reorder(word, -freq), freq)) +
      ggtitle(paste("Most Frequent Words in Song Titles for year",input$year21)) +
      geom_bar(stat = "identity") + xlab("Words in Song Titles") + ylab("Frequency")
  })
  output$yearwisetitles2 <- renderPlot({
    ggplot(yearwisetitles(input$year22)[1:20,], aes(reorder(word, -freq), freq)) +
      ggtitle(paste("Most Frequent Words in Song Titles for year",input$year22)) +
      geom_bar(stat = "identity") + xlab("Words in Song Titles") + ylab("Frequency")
  })
  
  #Tab 4a output
  output$yearwiseartists <- renderPlot({
    ggplot(yearwiseartists(input$year3)[1:15,], aes(artist, hits)) +
      ggtitle("Analysis of Artists Vs Number of hits for every year") +
      geom_bar(stat="identity") + xlab("Artist") + ylab("Number of Hits") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  #Tab 4b output
  output$popartists <- renderPlot({
    ggplot(popartistsfn(input$artist), aes(year, n)) + 
      ggtitle("Cumulative Analysis of Popular artists (artists with atleast 12 hits in 1965-2015) and their hits ") +
      geom_bar(stat="identity") + xlab("Year") + ylab("Number of hits")
  })
  
}

# Shiny App object
shinyApp(ui = ui, server = server)
