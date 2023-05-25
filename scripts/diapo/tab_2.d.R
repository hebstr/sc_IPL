source("scripts/_setup.R")

width <- 600

title <- ""

tab_2.gts <-
list(uv = tab_reg(d.1, method = coxph),
     mv = tab_reg(ref = d.1)) %>%
  map(~ tab_format(.) %>% 
        modify_table_body(~ .x %>%
          mutate(label = case_when(variable == "sympt_type"
                                   ~ glue("{level} symptoms — ref: {reference_level}"),
                                   .default = label)))) %>%
  tbl_merge(tab_spanner = glue("**{c('Univariable', 'Multivariable')} analysis**"))

tab_2.d <- gt_template(tab_2.gts, title)

#tab_data(tab_2.gts); tab_data(tab_2)

output(tab_2.d)

