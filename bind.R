
one <- data.frame(num = 1:3, name = c("apple", "ball", "cat"))

two <- data.frame(num = 4:5, name = c("dog", "elephant"))

three <- data.frame(species = c("plants", "non-living", "animal", "animal", "animal"))

# You can supply data frames as arguments:
four <- bind_rows(one, two, three)


five <- data.frame(sex = 1:2)

bind_cols(four, three)


library(dplyr)


bind_cols(one, two)


bind_rows(two, three)

