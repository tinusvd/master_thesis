final_conditions <- add_bound_support(final_data = final_conditions, rules = rules_bench, bound_object = bound_list)

final_conditions <- final_conditions %>% 
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