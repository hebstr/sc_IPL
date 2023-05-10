source("scripts/_setup.R")

width <- 570

title <-
glue("**Table 1.** Characteristics at Baseline of Patients Who Had a First Acute Coronary
     Syndrome Between {min(year(d.1$date_index))} and {max(year(d.1$date_index))}, by Sex.")

tab_1.gts <-
d.1[opts$tab$input$base] %>% 
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
  add_overall(col_label = "**Total<br>(n={N})**") %>%
  add_stat_label(label = all_continuous() ~ "mean±SD") %>% 
  tab_format()

tab_1 <- gt_template(tab_1.gts, title)

#tab_data(tab_1.gts); tab_data(tab_1)

output(tab_1)
