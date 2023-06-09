source("scripts/_setup.R")

width <- 600

title <- 
glue("**Table 2.** Univariable and Multivariable Cox Regression Model for the Risk
     of Acute Coronary Syndrome Recurrence in the Year Following the Incident Episode, 
     Between {min(year(d.1$date_index))} and {max(year(d.2$date_episode))}.")

tab_2.gts <-
list(uv = tab_reg(d.1, method = coxph),
     mv = tab_reg(ref = d.1)) %>%
  map(~ tab_format(.) %>%
        spec_label("sympt_type", glue("{.$table_body$level} symptoms"))) %>% 
  tbl_merge(tab_spanner = glue("**{c('Univariable', 'Multivariable')} analysis**"))

tab_2 <- gt_template(tab_2.gts)

#tab_data(tab_2.gts); tab_data(tab_2)

output(tab_2)

