require(dplyr)
require(tidyr)
require(rlang)
require(gtsummary)

check_events_censoring_by_group <- function(data, var_to_check, needfilter = FALSE, 
                                            groupname = NULL, groupcateg_list = NULL, 
                                            combine = FALSE, combine_list = NULL, desire_order = NULL, 
                                            comb_n_prop = FALSE) {
  
  # Step1: filter the data 
  if(needfilter == TRUE) {
    subdata <- data %>% filter(!!sym(groupname) %in% groupcateg_list)
  } else {
    subdata <- data
  }
  
  # Step 2: count the category 
  tab <- subdata %>%
    count(!!sym(var_to_check)) %>%
    mutate(prop = round(n / sum(n) * 100, 1))
  
  total_n <- sum(tab$n)
  tab0 <- tab
  
  # Step 3: if user specifies the combine 
  if (combine && !is.null(combine_list)) {
    for (new_cat in names(combine_list)) {
      included_cats <- combine_list[[new_cat]]
      if (is.list(included_cats)) {
        # support include / exclude 
        if (!is.null(included_cats$include)) {
          cats_to_combine <- included_cats$include
        } else if (!is.null(included_cats$exclude)) {
          cats_to_combine <- setdiff(tab0[[var_to_check]], included_cats$exclude)
        } else {
          stop("combine_list must specify include= or exclude=")
        }
      } else {
        cats_to_combine <- included_cats
      }
      
      add_row_df <- tibble(
        !!sym(var_to_check) := new_cat,
        n = sum(tab0$n[tab0[[var_to_check]] %in% cats_to_combine]),
        prop = round(sum(tab$n[tab0[[var_to_check]] %in% cats_to_combine]) / total_n * 100, 1)
      )
      tab <- bind_rows(tab, add_row_df)
    }
  }
  
  # Step 4: optional: combine n and prop into a single string
  if (comb_n_prop) {
    tab <- tab %>%
      mutate(
        n_prop = sprintf("%d (%.1f%%)", n, prop)
      ) %>%
      select(!!sym(var_to_check), n_prop)
  }
  
  # Step 5: return the table:   
  if(!is.null(desire_order)) {
    tab %>% arrange(factor(!!sym(var_to_check), levels = desired_order))
  } else {
    tab
  }
}

# head(trial)
# 
# trt      age marker stage grade response death ttdeath
# <chr>  <dbl>  <dbl> <fct> <fct>    <int> <int>   <dbl>
# 1 Drug A    23  0.16  T1    II           0     0    24  
# 2 Drug B     9  1.11  T2    I            1     0    24  
# 3 Drug A    31  0.277 T1    II           0     0    24  
# 4 Drug A    NA  2.07  T3    III          1     1    17.6
# 5 Drug A    51  2.77  T4    III          1     1    16.4
# 6 Drug B    39  0.613 T4    I            0     1    15.6

check_events_censoring_by_group(
  data = trial, var_to_check = "stage", needfilter = TRUE, 
  groupname = "grade", groupcateg_list = c("I", "II"), 
  combine = TRUE,
  combine_list = list(
    "FirstTwo" = c("T1", "T2"),
    "NotT1" = list(exclude = c("T1"))
  ), comb_n_prop = T
)

# stage    n_prop    
# <chr>    <chr>     
# 1 T1       40 (29.4%)
# 2 T2       35 (25.7%)
# 3 T3       29 (21.3%)
# 4 T4       32 (23.5%)
# 5 FirstTwo 75 (55.1%)
# 6 NotT1    96 (70.6%)