library(ggvis)
library(shiny)
# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

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

fluidPage(
  titlePanel("Movie explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h5("Change Variables Below"),
             selectInput("scatter_x", "x-axis variable",names(movie_df)[c(25:26,28,30:31,33:39)], selected = "Year"),
             selectInput("scatter_y", "y-axis variable",names(movie_df)[c(25:26,28,30:31,33:39)], selected = "Inflation Adjusted WorldWide Box Office"),
             h6("Note all varible numbers are in millions")
           ),
           wellPanel(
             h4("Filter"),
             sliderInput("year", "Year released", 1960, 2017, value = c(1960, 2017),sep = ""),
             sliderInput("boxoffice", "Inflation Adjusted Dollars at Box Office (millions)",
                         0, 3500, c(0, 3500), step = 1),
             selectInput("genre", "Genre (a movie can have multiple genres)",
                         c("All", 'Action', "Adventure", "Animation", "Biography", "Comedy",
                           "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                           "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                           "Short", "Sport", "Thriller", "War", "Western")
             ),
             textInput("director", "Director name contains (e.g., Miyazaki)"),
             textInput("title", "Find your favorite movie (note this is case sensitive)")
           )
  ),
    column(9,
           ggvisOutput("plot1"),
           wellPanel(
             span("Number of movies selected:",
                  textOutput("n_movies")
             )
           )
    )
  )
)
