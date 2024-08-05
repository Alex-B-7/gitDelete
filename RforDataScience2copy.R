library(ggplot2)
library(gridExtra)
library(tidyverse)
library(nycflights13)
fDataScience <- function(){
  
  plots <- list()
  
  fChapter1 <- function(){
    cat(paste0("Hello fChapter1...Running...", Sys.time(), '\n'))
    #broswer()
    p <- ggplot(data = mpg) +
      geom_point(mapping = aes(x = displ, y = hwy, color = class))
    plots[[1]] <<- p
    cat(paste0("Goodbye fChapter1...Complete...", Sys.time(), '\n'))
  } # color coded gg plot
  
  fChapter2 <- function(){
    cat(paste0("Hello fChapter2...Running...", Sys.time(), '\n'))
    #browser()
    y <- seq(1, 10, length.out = 5)
    print(y)
    cat(paste0("Goodbye fChapter2...Complete...", Sys.time(), '\n'))
  } # using sequence function
  
  fChapter3 <- function(){
    cat(paste0("Hello fChapter3...Running...", Sys.time(), '\n'))
    #browser()
    batting <- as_tibble(Lahman::Batting)
    
    batters <- batting %>%
      group_by(playerID) %>%
      summarize(
        ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
        ab = sum(AB, na.rm = TRUE)
      )
    
    batters_filtered <- batters %>%
      filter(ab > 100)
    p <- ggplot(batters_filtered, aes(x = ab, y = ba)) +
      geom_point()
      
    
    plots[[2]] <<- p
    
    cat(paste0("Goodbye fChapter3...Complete...", Sys.time(), '\n'))
  } # batting average plot
  
  fChapter4 <- function(){
    cat(paste0("Hello fChapter4...Running...", Sys.time(), '\n'))
    #browser()
    library(dplyr)
    
    data <- mtcars
    
    processed_data <- data %>%
      filter(mpg > 20) %>%
      mutate(speed_factor = mpg / cyl)
             
      print(head(processed_data))
             
    cat(paste0("Goodbye fChapter4...Complete...", Sys.time(), '\n'))
  } # mtcars
  
  fChapter5 <- function(){
    cat(paste0("Hello fChapter5...Running...", Sys.time(), '\n'))
    #browser()
    smaller <- diamonds %>%
      filter(carat < 3)
    p <- ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
      geom_freqpoly(binwidth = 0.1)
    plots[[3]] <<- p
    cat(paste0("Goodbye fChapter5...Complete...", Sys.time(), '\n'))
  } # overlaying histograms
  
  fChapter6 <- function(){
    cat(paste0("Hello fChapter6...Running...", Sys.time(), '\n'))
    #browser()
    print(getwd())
    cat(paste0("Goodbye fChapter6...Complete...", Sys.time(), '\n'))
  } # view working directory
  
  fChapter7 <- function(){
    cat(paste0("Hello fChapter7...Running...", Sys.time(), '\n'))
    #browser()
    my_tibble <- tibble(
      x = 1:5,
      y = 1,
      z = x ^ 2 + y
    )
    print(my_tibble)
    cat(paste0("Goodbye fChapter7...Complete...", Sys.time(), '\n'))
  } # tibble
  
  fChapter8 <- function(){
    cat(paste0("Hello fChapter8...Running...", Sys.time(), '\n'))
    #browser()
    library(readr)
    
    data <- read_csv("cards.csv")
    
    print(head(data))
    cat(paste0("Goodbye fChapter8...Complete...", Sys.time(), '\n'))
  } # import data with readr
  
  fChapter9 <- function(){
    cat(paste0("Hello fChapter9...Running...", Sys.time(), '\n'))
    #browser()
    library(tidyr)
    library(dplyr)
    
    table1 <- tibble(
      country = c("Afghanistan", "Brazil", "China"),
      `1999` = c(745, 37737, 212258),
      `2000` = c(2666, 80488, 213766)
    )
    
    table2 <- tibble(
      country = c("Afghanistan", "Brazil", "China"),
      `1999` = c(19987071, 172006362, 1272915272),
      `2000` = c(20595360, 174504898, 1280428583)
    )
    
    tidy1 <- table1 %>%
      gather(`1999`, `2000`, key = "year", value = "cases")
    
    tidy2 <- table2 %>%
      gather(`1999`, `2000`, key = "year", value = "population")
    
    tidy_data <- left_join(tidy1, tidy2, by = c("country", "year"))
    
    print(tidy_data)
    
    cat(paste0("Goodbye fChapter9...Complete...", Sys.time(), '\n'))
  } # tidying data with tidyr
  
  fChapter10 <- function(){
    cat(paste0("Hello fChapter10...Running...", Sys.time(), '\n'))
    #browser()
    x <- tribble(
      ~key, ~val_x,
      1, "x1",
      2, "x2",
      3, "x3",
      4, "x4",
      5, "x5"
    )
    y <- tribble(
      ~key, ~val_y,
      1, "y1",
      2, "y2",
      4, "y3",
      5, "y5",
      6, "y6"
    )
    
    joinxy <- x %>%
                inner_join(y, by = "key")
    
    print(joinxy)
    cat(paste0("Goodbye fChapter10...Complete...", Sys.time(), '\n'))
  } # inner join 2 tibbles
  
  fChapter11 <- function(){
    cat(paste0("Hello fChapter11...Running...", Sys.time(), '\n'))
    #browser()
    name <- "Hadley"
    time_of_day <- "morning"
    birthday <- FALSE
        stringex <- str_c(
             "Good ", time_of_day, " ", name,
             if (birthday) " and HAPPY BIRTHDAY",
             "."
         )
     print(stringex)   
    cat(paste0("Goodbye fChapter11...Complete...", Sys.time(), '\n'))
  } # strings with stringr
  
  fChapter12 <- function(){
    cat(paste0("Hello fChapter12...Running...", Sys.time(), '\n'))
    #browser()
    
    x1 <- c("Dec", "Apr", "Jan", "Mar")
    
    month_levels <- c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    )
    
    y1 <- factor(x1, levels = month_levels)
    print(sort(y1))
    cat(paste0("Goodbye fChapter12...Complete...", Sys.time(), '\n'))
  } # creating a factor
  
  fChapter13 <- function(){
    cat(paste0("Hello fChapter13...Running...", Sys.time(), '\n'))
    #browser()
    
    dtflights <- flights %>%
      select(year, month, day, hour, minute) %>%
      mutate(
        departure = make_datetime(year, month, day, hour, minute)
      )
    
    print(head(dtflights))
    cat(paste0("Goodbye fChapter13...Complete...", Sys.time(), '\n')) 
  } # dates and times
  
  fChapter14 <- function(){
    cat(paste0("Hello fChapter14...Running...", Sys.time(), '\n'))
    #browser()
    pipemtcars <- mtcars %>% 
                    select(-am) %>% 
                    filter(mpg < 20) %>% 
                    mutate(mpg_times_10 = mpg * 10) %>% 
                    arrange(cyl,mpg)
    print(head(pipemtcars))
    
    cat(paste0("Goodbye fChapter14...Complete...", Sys.time(), '\n')) 
  } # Pipe %>%
  
  fChapter15 <- function(){
    cat(paste0("Hello fChapter15...Running...", Sys.time(), '\n'))
    #browser()
    has_name <- function(x) {
      nms <- names(x)
      if (is.null(nms)) {
        rep(FALSE, length(x))
      } else {
        !is.na(nms) & nms != ""
      }
    }
    
    x1 <- c(a = 1, b = 2, 3)
    x2 <- c(1, 2, 3)
    x3 <- c(a = 1, b = 2, c = NA, d = 4)
    
    cat("has_name(x1):", has_name(x1), "\n")
    cat("has_name(x2):", has_name(x2), "\n")
    cat("has_name(x3):", has_name(x3), "\n")
    
    
    cat(paste0("Goodbye fChapter15...Complete...", Sys.time(), '\n'))
  } # functions
  
  fChapter16 <- function(){
    cat(paste0("Hello fChapter16...Running...", Sys.time(), '\n'))
    #browser()
    x <- sample(20, 100, replace = TRUE)
    y <- x > 10
    print(sum(y))
    print(mean(y))
    
    cat(paste0("Goodbye fChapter16...Complete...", Sys.time(), '\n'))
  } # vectors
  
  fChapter17 <- function(){
    cat(paste0("Hello fChapter17...Running...", Sys.time(), '\n'))
    #browser()
    
    means <- c(0, 1, 2)
    
    out <- vector("list", length(means))
    for (i in seq_along(means)) {
      n <- sample(100, 1)
      out[[i]] <- rnorm(n, means[[i]])
    }

    print(str(unlist(out)))
    cat(paste0("Goodbye fChapter17...Complete...", Sys.time(), '\n'))
    
  } # iteration with purrr
  
  fChapter18 <- function(){
    cat(paste0("Hello fChapter18...Running...", Sys.time(), '\n'))
    #browser()
    seq_range <- function(x, n, pretty = FALSE) {
      if (pretty) {
        return(pretty(x, n))
      } else {
        return(seq(min(x), max(x), length.out = n))
      }
    }
    
    seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)
    
    x1 <- rcauchy(100)
    seq_range(x1, n = 5)
    
    x2 <- c(0, 1)
    seq_range(x2, n = 5)
    
    grid <- expand.grid(x1 = seq_range(x1, n = 5), x2 = seq_range(x2, n = 5))
    grid$pred <- runif(nrow(grid))  # Randomly generated for the example
    grid$model <- sample(c('Model1', 'Model2'), nrow(grid), replace = TRUE)  # Randomly assigned for the example
    
    p <- ggplot(grid, aes(x1, x2)) +
      geom_tile(aes(fill = pred)) +
      facet_wrap(~ model)
    
    plots[[4]] <<- p
    
    cat(paste0("Goodbye fChapter18...Complete...", Sys.time(), '\n'))
  } # model basics
  
  fChapter19 <- function(){
    cat(paste0("Hello fChapter19...Running...", Sys.time(), '\n'))
    #browser()
    diamonds2 <- diamonds %>%
      filter(carat <= 2.5) %>%
      mutate(lprice = log2(price), lcarat = log2(carat))
    
    p <- ggplot(diamonds2, aes(lcarat, lprice)) +
      geom_hex(bins = 50)
    
    plots[[5]] <<- p
    
    cat(paste0("Goodbye fChapter19...Complete...", Sys.time(), '\n'))
  } # model building
  
  fChapter20 <- function(){
    cat(paste0("Hello fChapter20...Running...", Sys.time(), '\n'))
    #browser()
    library(gapminder)
    gapminder
    
    p <- gapminder %>%
      ggplot(aes(year, lifeExp, group = country)) +
      geom_line(alpha = 1/3)
    
    plots[[6]] <<- p
    
    cat(paste0("Goodbye fChapter20...Complete...", Sys.time(), '\n'))
  } # many models purrr and broom
  
  fChapter21 <- function(){
    cat(paste0("Hello fChapter21...Running...", Sys.time(), '\n'))
    #browser()
    print("Markdown Chapter")
    cat(paste0("Goodbye fChapter21...Complete...", Sys.time(), '\n'))
  } # markdown
  
  fChapter22 <- function(){
    cat(paste0("Hello fChapter22...Running...", Sys.time(), '\n'))
    #browser()
    p <- ggplot(mpg, aes(displ, hwy)) +
          geom_point(aes(color = class)) +
          geom_smooth(se = FALSE) +
          labs(
            title = paste(
              "Fuel efficiency generally decreases with",
              "engine size"
            ),
            subtitle = paste(
              "Two seaters (sports cars) are an exception",
              "because of their light weight"
            ),
            caption = "Data from fueleconomy.gov"
          )
    
    plots[[7]] <<- p
    
    cat(paste0("Goodbye fChapter22...Complete...", Sys.time(), '\n'))
  } # graphics ggplot2
  
  fChapter23 <- function(){
    cat(paste0("Hello fChapter23...Running...", Sys.time(), '\n'))
    #broswer()
    print("Markdown Formats"
    )
    cat(paste0("Goodbye fChapter23...Complete...", Sys.time(), '\n'))
  } # markdown formats
  
  fChapter24 <- function(){
    cat(paste0("Hello fChapter24...Running...", Sys.time(), '\n'))
    #broswer()
    print("Markdown Workflow"
    )
    cat(paste0("Goodbye fChapter24...Complete...", Sys.time(), '\n'))
  } # markdown workflow
  
  cat('Hello World\n')
  fChapter1()
  fChapter2()
  fChapter3()
  fChapter4()
  fChapter5()
  fChapter6()
  fChapter7()
  fChapter8()
  fChapter9()
  fChapter10()
  fChapter11()
  fChapter12()
  fChapter13()
  fChapter14()
  fChapter15()
  fChapter16()
  fChapter17()
  fChapter18()
  fChapter19()
  fChapter20()
  fChapter21()
  fChapter22()
  fChapter23()
  fChapter24()
  
  # Remove any NULL plots (in case there are fewer than 9 plots generated)
  plots <- plots[!sapply(plots, is.null)]
  
  # Arrange and display all plots in a grid (3 by 3)
  grid.arrange(grobs = plots, ncol = 3)
}

cat("===========================================================\n") 
cat(paste0( 'RforDataScience.R Script Running ... ', Sys.time(), "\n.")) 
#browser() 
fDataScience() 
#browser() 
cat(paste0( 'RforDataScience.R Script Complete. ', Sys.time(), "\n.")) 
cat("===========================================================\n")




