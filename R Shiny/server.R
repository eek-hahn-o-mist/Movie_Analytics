library(ggvis)
library(dplyr)
library(shiny)
library(data.table)

###Data the movie data frame and get the data in format needed
movie_df <- read.csv('join_movie_df.csv')
drops <- c("X","X.1","X.2","Unnamed..0")
movie_df<-movie_df[ , !(names(movie_df) %in% drops)]
movie_df<- subset(movie_df, !is.na(movie_df$meta_score))
movie_df$score_diff<- abs(movie_df$meta_score - (movie_df$mov_imdb_rating*10)) #create variable for score difference
movie_df <- movie_df[!duplicated(movie_df$mov_name),] #remove duplicates
movie_df$ID <- 1:nrow(movie_df)
colnames(movie_df)[25:39] <- c("Meta Critic Score","Movie Budget","Director","Domestic Box Office","Genres","Number of IMBD Reviews",
                               "IMBD Rating","Title","Oscar","WorldWide Box Office","Year","Inflation Adjusted Domestic Box Office",
                               "Inflation Adjusted WorldWide Box Office","Inflation Adjusted Budget","Critic and Fan Difference")
movie_df$`Inflation Adjusted WorldWide Box Office`<- movie_df$`Inflation Adjusted WorldWide Box Office`/1e6
movie_df$`Inflation Adjusted Budget`<- movie_df$`Inflation Adjusted Budget`/1e6
movie_df$`Inflation Adjusted Domestic Box Office`<- movie_df$`Inflation Adjusted Domestic Box Office`/1e6
movie_df$`WorldWide Box Office`<- movie_df$`WorldWide Box Office`/1e6
movie_df$`Domestic Box Office`<- movie_df$`Domestic Box Office`/1e6
movie_df$`Movie Budget`<- movie_df$`Movie Budget`/1e6
movie_df$`Number of IMBD Reviews`<- movie_df$`Number of IMBD Reviews`/1e6

function(input, output, session) {
  # Filter the movies, returning a data frame
  movies <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    reviews <- input$reviews
    oscars <- input$oscars
    minyear <- input$year[1]
    maxyear <- input$year[2]
    minboxoffice <- input$boxoffice[1]
    maxboxoffice <- input$boxoffice[2]
    
    # Apply filters
    m <- movie_df %>%
      filter(
        #movie_df$mov_imdb_num >= reviews,
        #movie_df$mov_oscar >= oscars,
        movie_df$Year >= minyear,
        movie_df$Year <= maxyear,
        movie_df$`Inflation Adjusted WorldWide Box Office` >= minboxoffice,
        movie_df$`Inflation Adjusted WorldWide Box Office` <= maxboxoffice
      )
    
    
    # Optional: filter by genre
    if (input$genre != "All") {
      m <- data.table(m)
      m <- m[Genres %like% input$genre]
    }
    
    # Optional: filter by director
    if (!is.null(input$director) && input$director != "") {
      m <- data.table(m)
      m <- m[Director %like% input$director]
    }
    
    # Optional: filter by movie
    if (!is.null(input$title) && input$title != "") {
      m <- data.table(m)
      m <- m[Title %like% input$title]
    }
    
    
    m <- as.data.frame(m)
    
  })
    
  # Function for generating tooltip text
  
  
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    
    
    # Pick out the movie with this ID
    all_movies <- isolate(movies())
    movie <- all_movies[all_movies$ID == x$ID, ]
    
    paste0("<b>", movie$Title, "</b><br>",
           movie$Year, "<br>",
           "Infl Adj. WW Box Office $", format(movie$`Inflation Adjusted WorldWide Box Office`*1e6, big.mark = ",", scientific = FALSE), "<br>",
           "WW Box Office $", format(movie$`WorldWide Box Office`*1e6, big.mark = ",", scientific = FALSE)
    )
    
    
    
  }
  
  
  ###Create a slider to limit numerical view based on the data loaded
  vis <- reactive({
    
    xvar <- prop("x", as.symbol(input$scatter_x))
    yvar <- prop("y", as.symbol(input$scatter_y))
    
    movies%>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~Oscar, key := ~ID) %>%
      add_tooltip(movie_tooltip, "hover") %>%
      add_axis("x", title = input$scatter_x,format="####") %>%
      add_axis("y", title = input$scatter_y,title_offset = 50) %>%
      add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("orange", "#aaa")) %>%
      set_options(width = 700, height = 700)
  })
    

  
  
  vis %>% bind_shiny("plot1")
  
  
  output$n_movies <- renderText({ nrow(movies()) })

}
