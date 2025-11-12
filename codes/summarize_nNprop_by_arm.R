require(dplyr)
require(tidyr)
require(rlang)
require(gtsummary)
require(DT)

nNprop_by_arm <- function(dat, arm_var, group_var, condition_str, useDT = TRUE)
{
  condition <- parse_expr(condition_str)
  
  tab_arm <- dat %>% 
    group_by(!!sym(group_var), !!sym(arm_var)) %>%
    summarise(
      n_total = n(),
      n_count = sum(with(cur_data(), !!condition), na.rm = TRUE),  
      .groups = "drop"
    ) %>%
    mutate(prop = sprintf("%d / %d (%.1f%%)", n_count, n_total, 100 * n_count / n_total)) %>%
    select(!!sym(group_var), !!sym(arm_var), prop) %>%
    pivot_wider(names_from = !!sym(arm_var), values_from = prop)
  
  tab_group_overall <- dat %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n_total = n(),
      n_count = sum(with(cur_data(), !!condition), na.rm = TRUE),  
      .groups = "drop"
    ) %>%
    mutate(Overall = sprintf("%d / %d (%.1f%%)", n_count, n_total, 100 * n_count / n_total)) %>% 
    dplyr::select(-n_total, -n_count)
  
  tab <- left_join(tab_arm, tab_group_overall, by = group_var)
  
  tab_all <- dat %>%
    summarise(
      n_total = n(),
      n_count = sum(with(cur_data(), !!condition), na.rm = TRUE),  
      .groups = "drop"
    )
  
  tab_all_arms <- dat %>%
    group_by(!!sym(arm_var)) %>%
    summarise(
      n_total = n(),
      n_count = sum(with(cur_data(), !!condition), na.rm = TRUE),  
      .groups = "drop"
    ) %>%
    mutate(prop = sprintf("%d / %d (%.1f%%)", n_count, n_total, 100 * n_count / n_total)) %>%
    select(!!sym(arm_var), prop) %>%
    pivot_wider(names_from = !!sym(arm_var), values_from = prop)
  
  tab_all <- tab_all %>%
    mutate(Overall = sprintf("%d / %d (%.1f%%)", n_count, n_total, 100*n_count/n_total)) %>%
    bind_cols(tab_all_arms) %>%
    mutate(!!sym(group_var) := "All") %>%
    select(!!sym(group_var), everything()) %>% 
    dplyr::select(-n_total, -n_count)
  
  final_tab <- bind_rows(tab, tab_all)
  
  if(useDT == TRUE)
  {
    final_tab %>%
      datatable(
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          autoWidth = TRUE
        )
      ) %>% return()
  } else {
    return(final_tab)
  }
}

# head(trial)

#   trt      age marker stage grade response death ttdeath
#   <chr>  <dbl>  <dbl> <fct> <fct>    <int> <int>   <dbl>
#   1 Drug A    23  0.16  T1    II           0     0    24  
#   2 Drug B     9  1.11  T2    I            1     0    24  
#   3 Drug A    31  0.277 T1    II           0     0    24  
#   4 Drug A    NA  2.07  T3    III          1     1    17.6
#   5 Drug A    51  2.77  T4    III          1     1    16.4
#   6 Drug B    39  0.613 T4    I            0     1    15.6

nNprop_by_arm(trial, "trt", "grade", "death == 1", useDT = FALSE)

#   grade `Drug A`        `Drug B`         Overall          
#   <chr> <chr>           <chr>            <chr>            
#   1 I     16 / 35 (45.7%) 17 / 33 (51.5%)  33 / 68 (48.5%)  
#   2 II    16 / 32 (50.0%) 20 / 36 (55.6%)  36 / 68 (52.9%)  
#   3 III   20 / 31 (64.5%) 23 / 33 (69.7%)  43 / 64 (67.2%)  
#   4 All   52 / 98 (53.1%) 60 / 102 (58.8%) 112 / 200 (56.0%)
