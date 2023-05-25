source("scripts/_setup.R")

labelled::var_label(d.1) <-
list(fdr = "≥1 Cardiovascular risk factor",
     maj_comp = "≥1 Major complication")

width <- 570

title <- ""

tab_1.gts <-
d.1[c("sexe", opts$vargrp$fdr)] %>% 
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
  add_stat_label(label = all_continuous() ~ "mean±SD") %>% 
  tab_format() %>% 
  modify_header(label ~ "",
                stat_0 ~ "**(n={N})**",
                c(stat_1, stat_2) ~ "**(n={n}, {style_percent(p)}%)**",
                p.value ~ "")

tab_1.d <- gt_template(tab_1.gts, title)

#tab_data(tab_1.gts); tab_data(tab_1)

output(tab_1.d)
