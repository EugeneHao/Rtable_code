require(dplyr)
require(tidyr)
require(rlang)
require(gtsummary)

count_by_group <- function(data, var_for_count, group, names_sep = "_", names_prefix = "") 
{
  var_for_count <- as.character(var_for_count)
  group         <- as.character(group)
  
  data %>%
    count(across(all_of(c(var_for_count, group))), name = "n") %>%
    pivot_wider(
      names_from   = all_of(group),
      values_from  = n,
      values_fill  = 0,
      names_sep    = names_sep,
      names_prefix = names_prefix
    )
}

count_by_group(trial, c("death", "grade"), c("trt", "stage"))

#    death grade `Drug A_T1` `Drug A_T2` `Drug A_T3` `Drug A_T4` `Drug B_T1` `Drug B_T2`
#   <int> <fct>       <int>       <int>       <int>       <int>       <int>       <int>
# 1     0 I               4           6           6           3           4           5
# 2     0 II              9           2           2           3           5           4
# 3     0 III             3           6           1           1           4           4
# 4     1 I               4           2           5           5           5           5
# 5     1 II              5           6           3           2           4           5
# 6     1 III             3           3           5           9           3           6