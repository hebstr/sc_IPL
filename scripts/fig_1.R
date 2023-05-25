source("scripts/_setup.R")

fig_1_test <-
opts$model$uv %>%
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%
  merge_estimate_ci()

d.1$sexe <- fct_rev(d.1$sexe)
fig_1_plot <- cuminc(Surv(tt_rec_csr, as.factor(rec_csr)) ~ sexe, d.1)

fig_1_tab <-
fig_1_plot %>%
  tidy_cuminc(times = 0:opts$set$suivi_jr) %>%
  select(time, strata, estimate, contains(c("conf", "n.", "event"))) %>%
  group_by(strata) %>% 
  slice_tail(n = 1)

fig_1_tab.plot <-
fig_1_tab %>%
  mutate(num.all = cum.event + n.censor,
         N = glue("{cum.event}/{num.all}")) %>%
  merge_estimate_ci(IR, multi = 100) %>% 
  select(strata, N, IR) %>% 
  rename(" " = strata,
         "No. of Events/\nTotal No. at Risk" = N,
         !!paste0("IR at ", opts$fig$date_point, "\n% ", opts$set$ci$label) := IR) %>% 
  ggtexttable(rows = NULL,
              theme = ttheme(base_style = "blank",
                             base_size = 7,
                             padding = unit(c(2,2), "mm"))) %>% 
  table_cell_font(row = 2:tab_nrow(.), column = 1,
                  face = "bold", size = 7)

fig_1 <-
fig_1_plot %>% 
  ggcuminc(theme = theme_cuminc(margin_title = margin(45,0, opts$fig$mg_bottom_title, 0))) +
  labs(caption = glue("{opts$fig$title}<br>
                      <span style='font-size:7pt'> {opts$fig$note_1} {opts$fig$note_suppl} {opts$fig$abb}</span>")) +
  scale_color_manual(values = c(opts$set$color$front_dark, opts$set$color$ref)) +
  scale_ggsurvfit(x_scales = list(name = "Months Since the Incident Episode",
                                  breaks = seq(0, opts$set$suivi_jr, by = (365/12) * opts$fig$x_break),
                                  label = seq(0, opts$set$suivi_an * 12, by = opts$fig$x_break),
                                  limits = c(opts$set$suivi_jr * -0.01, opts$set$suivi_jr * 1.01)),
                  y_scales = list(name = "Cumulative Incidence (%)",
                                  breaks = seq(0, max(fig_1_tab$estimate) * 1.2, by = opts$fig$y_break),
                                  limits = c(0, max(fig_1_tab$estimate) * 1.15),
                                  label = label_percent(accuracy = 1, suffix = ""))) +
  add_risktable(risktable_stats = "n.risk",
                stats_label = list(n.risk = "No. at Risk"),
                risktable_group = "risktable_stats",
                size = 2.5,
                family = opts$set$font$alpha,
                theme = theme_risktable(margin_table = margin(-37,0,45,0))) +
  geom_text(data = fig_1_test,
            aes(label = glue(opts$fig$stat),
                x = 0, y = max(fig_1_tab$estimate) * 1.05),
            size = 2.5, hjust = 0, family = opts$set$font$alpha) +
  geom_text(data = fig_1_tab,
            aes(label = strata,
                x = time, y = estimate * c(1.05, 0.93),
                color = strata),
            size = 2.5, hjust = 1, family = opts$set$font$alpha) +
  annotation_custom(ggplotGrob(fig_1_tab.plot),
                    xmax = opts$set$suivi_jr * 0.8*2,
                    ymax = max(fig_1_tab$estimate) * 0.25*2)

output(fig_1)
