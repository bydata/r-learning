library(tidyverse)

# tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
# tuesdata <- tidytuesdayR::tt_load(2020, week = 4)
# spotify_songs <- tuesdata$spotify_songs

a <- "Allianz"
a
print(paste("Ich arbeite bei der", a))

b <- 1
typeof(b)

c <- 1L
typeof(c)

typeof(b + c)

# Fehler
a + b

d <- list(a, b, c)
d

# Vector
e <- c(1, 2, 3, 4, 10, 20, 30, 40, 50)
g <- 1:100
h <- seq(2, 1000, 2)
h

f <- list(a, b, c, e, g, h)
f

class(f)

class(mtcars)
typeof(mtcars)
# Struktur des Objekts
str(mtcars)

mtcars2 <- mtcars
mtcars2$model <- names(mtcars)
as.list(mtcars2)

b <- 10
rep(a, b)
