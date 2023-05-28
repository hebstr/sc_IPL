source("scripts/_setup.R")

width <- 850

title <-
glue("**Table S1.** Characteristics of the Incident Episode Among Patients Who Had
     a Recurrence in the Following Year, and of the Recurrent Episode, by Sex.")


#tab_inter_brut <-
#  opts$tab$input$base[-(1:2)] %>%
#  map(~ glm(eval(sym(.x)) ~ sexe * as.factor(id_episode),
#            d %>% filter(rec == 1 & tt_rec <= opts$set$suivi_jr),
#            family = binomial) %>%
#        tidy(exponentiate = TRUE, conf.int = T) %>% 
#        mutate(variable = .x) %>%
#        slice_tail(n = 1)) %>% 
#  bind_rows()

tab_inter.gts <-
filter(d, rec == 1 & tt_rec <= opts$set$suivi_jr) %>%
  select(id_episode, opts$tab$input$base) %>%
  tbl_strata(strata = id_episode, 
             .header = "**{c('Incident', 'Recurrent')} episode**",
             .tbl_fun = ~ .x %>%
               tbl_summary(by = sexe,
                           statistic = list(all_continuous() ~ "{mean}±{sd}",
                                            all_categorical() ~ "{n} ({p})"),
                           digits = list(all_continuous() ~ 1,
                                         all_categorical() ~ c(0,1)),
                           missing = "no") %>% 
               add_p(test = list(all_continuous() ~ "t.test",
                                 all_categorical() ~ "chisq.test.no.correct"),
                     test.args = all_tests("t.test") ~ list(var.equal = TRUE),
                     pvalue_fun = opts$set$p_format) %>%
               add_stat_label(label = list(all_continuous() ~ "mean±SD",
                                           all_categorical() ~ "n (%)")) %>%
               tab_format())

#    modify_table_body(~ .x %>% 
#                        left_join(tab_inter_brut, by = "variable") %>% 
#                        mutate(p.value.y = ifelse(row_type == "level" & var_type != "dichotomous", NA, p.value.y)) %>% 
#                        rename(p.value = p.value.y)) %>%
#      modify_header(p.value ~ "**p interaction<br>(sexe x episode)**")

tab_inter <- gt_template(tab_inter.gts)

#tab_data(tab_s.gts); tab_data(tab_s)

output(tab_inter)



