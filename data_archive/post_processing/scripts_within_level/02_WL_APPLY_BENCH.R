output_dir <- "./data_archive/post_processing/intermediate_output/within_level"
output_combined <- "./data_archive/post_processing/intermediate_output/within_level/combined"
gorica_files <- "./data_archive/simulations/conditions_data/output"

final_conditions_wl <- add_bound_support(final_conditions_wl, rules = rules_bench, bound_object = bound_list_wl)

final_conditions_wl <- final_conditions_wl %>% 
  mutate(
    supp_set1_h1_adj = case_when(
      supp_set1_h1 == 1 & bound_supp_set1 == 0 ~ 1,
      TRUE ~ 0),
    supp_set1_hc_adj = case_when(
      supp_set1_hc == 1 & bound_supp_set1 == 0 ~ 1,
      TRUE ~ 0
    ),
    supp_set2_h1_adj = case_when(
      supp_set2_h1 == 1 & bound_supp_set2 == 0 ~ 1,
      TRUE ~ 0
    ),
    supp_set2_hc_adj = case_when(
      supp_set2_hc == 1 & bound_supp_set2 == 0 ~ 1,
      TRUE ~ 0
    ),
    supp_set2_h0_adj = case_when(
      supp_set2_h0 == 1 & bound_supp_set2 == 1 ~ 1,
      TRUE ~ 0
    ),
    supp_set3_ha1_adj = case_when(
      supp_set3_ha1 == 1 & bound_supp_set3 == 0 ~ 1,
      TRUE ~ 0
    ),
    supp_set3_ha1_adj_2015 = case_when(
      supp_set3_ha1 == 1 & bound_supp_set3 == 1 ~ 1,
      TRUE ~ 0
    ),
    supp_set3_ha1c_adj = case_when(
      supp_set3_ha1c == 1 & bound_supp_set3 == 0 ~ 1,
      TRUE ~ 0
    ),
    supp_set4_ha2_adj = case_when(
      supp_set4_ha2 == 1 & bound_supp_set4 == 0 ~ 1,
      TRUE ~ 0
    ),
    supp_set4_ha2c_adj = case_when(
      supp_set4_ha2c == 1 & bound_supp_set4 == 0 ~ 1,
      TRUE ~ 0
    )
  )
