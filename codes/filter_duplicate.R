library(dplyr)

filter_duplicate <- function(data, var_dup, var_keep = NULL)
{
  var_dup  <- as.character(var_dup)
  var_keep <- as.character(var_keep)
  
  data %>%
    group_by(across(all_of(var_dup))) %>%
    mutate(n_dup = n()) %>%
    filter(n_dup > 1) %>%
    arrange(across(all_of(var_dup))) %>%
    select(all_of(var_dup), n_dup, all_of(var_keep)) %>%
    ungroup()
}


filter_duplicate(trial, c("trt", "age"), "death") %>% head(10)

#   trt      age n_dup death
#   <chr>  <dbl> <int> <int>
# 1 Drug A    31     5     0
# 2 Drug A    31     5     0
# 3 Drug A    31     5     0
# 4 Drug A    31     5     1
# 5 Drug A    31     5     1
# 6 Drug A    34     3     1
# 7 Drug A    34     3     0
# 8 Drug A    34     3     1
# 9 Drug A    36     3     0
# 10 Drug A    36     3     1