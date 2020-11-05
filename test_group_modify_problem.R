library("tidyverse")

# create tibble
df <- tibble(
  name=letters[1:10],
  csize=c("L","S","S","L","L","S","L","S","L","S"),
  v1=rnorm(10),
  v2=rnorm(10),
  v3=rnorm(10)
  )

# introduce some missing data
df$v1[3] <- NA
df$v1[6] <- NA
df$v1[7] <- NA
df$v3[2] <- NA

# these are the cols I want to impute
df_names <- c("v1","v2","v3")

# this is the grouping variable (has to be stored as a string, since it is an input to the function)
groupvar <- "csize"

df_imp1 <- df %>% group_by(.dots = groupvar) %>%
  mutate(across(all_of(df_names), ~if_else(is.na(.x), mean(.x, na.rm = TRUE), .x)))

df_imp2 <- df %>% group_by(csize) %>% mutate(across(v1:v3, ~ replace_na(., mean(., na.rm = T))))

# now I want to replace the NAs with column means, restricted to their group
# the following line works, but the problem is that it removes the name column, and reorders the rows...
df_imp <- df %>% group_by(.dots=groupvar) %>% select(df_names) %>% group_modify( ~{replace_na(.x,as.list(colMeans(.x, na.rm=TRUE)))})

# I could rebuild the tibble afterwards, but it would be neater/safer if I can direct the group_modify function to certain columns. AND retain the ordering, if possible.