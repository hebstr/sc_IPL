##########################################################################################################################-
# 1 - DEFINITION DES PARAMETRES
##########################################################################################################################-

library(tidyverse)
library(rlang)
library(glue)
library(gt)
library(gtsummary)
library(broom.helpers)
library(ggpubr)
library(gtable)
library(ggtext)
library(scales)
library(survival)
library(ggsurvfit)
library(tidycmprsk)
library(webshot)

rm(list = ls())
load("~/dfp_excl7_09-17/dfp_excl7_09-17.RData")
setwd("~/dfp_excl7_09-17")
source("scripts/_fonctions.R")

opts_set(suivi_an = 1, fdr = FALSE)

opts_vargrp <-
list(fdr = c("fdr", "HTA", "dyslip", "diab", "obesity", "tabac_actif"),
     maj_comp = c("maj_comp", "sympt_arret", "sympt_oap", "sympt_choc"),
     surv = c("tt_rec_csr", "rec_csr"),
     strata = c("lieu_recueil", "acs_type"))

opts_abb <-
abbr(CI ~ "confidence interval",
     N ~ "number of observations",
     IR ~ "incidence rate",
     BMI ~ "body mass index",
     ACS ~ "acute coronary syndrome",
     NSTEMI ~ "non-ST-elevation myocardial infarction",
     STEMI ~ "ST-elevation myocardial infarction")

opts_tab <-
  opts_tab(base = c("sexe", "age_cont","sympt_type", opts_vargrp$maj_comp, "acs_type", "pontage_pdt2", "angiop_pdt2"),
           uv = c("sexe", "age_incr", "lieu_recueil", "sympt_type", opts_vargrp$maj_comp, "acs_type", "pontage_pdt2",
                  "pontage_sortie", "angiop_pdt2", "angiop_sortie"),
           vargrp = opts_vargrp$maj_comp,
           abb = c(ACS, STEMI, NSTEMI),
           abb_fdr = BMI,
           note = list(p = "P-values were bolded if they were less than or equal to 0.05.",
                       vargrp = "At least one occurrence.",
                       strata = expr(glue("Multivariable Cox regression model stratified
                                          on {str_label(d, opts$vargrp$strata)}.")),
                       ajust = "Adjusted for all factors displayed in the multivariable analysis."))

opts_fig <-
tibble(date_point = c("1 Year", paste(c(2:5), "Years")),
       x_break = c(1, 2, 4, 6, 6),
       y_break = c(0.01, rep(0.02, 4)),
       mg_bottom_title = c(-35, rep(-28, 4)),
       width = 6.95, height = 4.10,
       title = glue("**Figure 1.** Cumulative Incidence of a First Recurrent Acute Coronary Syndrome
                  {date_point} After the Incident Episode."),
       note_1 = glue("The time to events was estimated by the Kaplan-Meier method. Data were censored
                     at {str_to_lower(date_point)} after the first event.No data were censored
                     before the date of point."),
       note_suppl = c(glue("Three events (one in {str_to_lower(opts$set$lab$sex_f)}, two in {str_to_lower(opts$set$lab$sex_m)})
                         occured at the date of point, which is why the number at risk remained unequal to the total
                         number at risk minus the number of events at this time."), rep("", 4)),
       abb = str_abb(with(opts_abb, c(CI, IR))),
       stat = "Hazard ratio for recurrence {opts$set$ci$label}: {select(fig_1_test, last_col())}
              Log rank p={round(p.value, 3)}") %>%
  .[opts$set$suivi_an,]

opts <-
append(opts,
       list(vargrp = opts_vargrp,
            tab = opts_tab,
            fig = opts_fig,
            abb = opts_abb))

##########################################################################################################################-
# 2 - PREPARATION DE LA BASE AVANT RECODAGE ----
##########################################################################################################################-

d <-
tibble(dfp2) %>%
  mutate(date_naiss =
           make_date(y = ddn_a.1,
                     m = ddn_m.1,
                     d = ddn_j.1),
         date_index =
           make_date(y = date_episode_a.1,
                     m = date_episode_m.1,
                     d = date_episode_j.1),
         age_index = time_length(date_index - date_naiss, unit = "year"),
         date_rec =
           make_date(y = date_episode_a.2,
                     m = date_episode_m.2,
                     d = date_episode_j.2),
         rec = ifelse(!is.na(date_rec), 1, 0),
         tt_rec =
           ifelse(rec == 1,
                  time_length(date_rec - date_index, unit = "days"),
                  time_length(max(date_rec, na.rm = TRUE) - date_index, unit = "days")),
         tt_rec_csr = squish(tt_rec, c(0, opts$set$suivi_jr)),
         rec_csr = ifelse(!is.na(date_rec) & tt_rec <= opts$set$suivi_jr, 1, 0)) %>% 
  modify_if(is.character, as.factor) %>% 
  filter(as.integer(age_index) < 74,
         year(date_index) <= max(year(date_index)) - opts$set$suivi_an,
         survie_episode2.1 == "vivant")

d <-
list(select(d, sexe.1, ends_with(".1"), date_naiss:length(d)),
     select(d, sexe.1, ends_with(".2"), -sexe.2, date_naiss:length(d)) %>% drop_na(id_episode.2)) %>%
  map_df(~ .x %>%
           rename_with(~ str_replace_all(., ".[:digit:]$", "")) %>%
           bind_rows())

##########################################################################################################################-
# 3. RECODAGE DE LA BASE ----
##########################################################################################################################-

.ecg <-
list(st = c("sus_st_onde_q", "onde_q", "sus_st", "bbg"),
     nst = c("sous_st_onde_t_neg", "onde_q_sous_st_onde_t_neg", "normal"))

.type <-
list(douleur = c("precordiale de repos", "precordiale d'effort", "precordiale SAI", "crescendo"),
     duree = c(">= 20 min", "crescendo"))

d <-
d %>%
  modify_if(is.factor, ~ case_when(. %in% c("oui", "present") ~ opts$set$lab$yes,
                                   . %in% c("non", "absent") ~ opts$set$lab$no,
                                   .default = .)) %>%
  mutate(sexe =
           case_when(sexe == "feminin" ~ opts$set$lab$sex_f,
                     .default = opts$set$lab$sex_m,
                     .ptype = factor(levels = c(opts$set$lab$sex_m, opts$set$lab$sex_f))),
         age_cont =
           ifelse(id_episode == 1,
                  age_index,
                  time_length(date_rec - date_naiss, unit = "year")),
         age_cat =
           cut(age_cont,
               c(35,45,55,65,74),
               labels = c("35-44","45-54","55-64","65-74"),
               right = FALSE),
         age_incr = as.numeric(cut(age_cont, breaks = 5*(7:15), right = FALSE)),
         bmi = poids / (taille / 100)^2) %>% 
  binr(tropo, bio_tropo_dose >= bio_tropo_seuil * 2) %>% 
  binr(obesity, as.integer(bmi) >= 30) %>%
  binr(tabac_actif, tabac == "fumeur actuel") %>% 
  binr(fdr, opts_vargrp$fdr[-1], auto_bin = TRUE) %>%
  binr(maj_comp, opts_vargrp$maj_comp[-1], auto_bin = TRUE) %>%
  mutate(acs_type =
           case_when(ecg %in% .ecg$st & tropo == opts$set$lab$yes ~ "STEMI",
                     ecg %in% .ecg$nst & tropo == opts$set$lab$yes ~ "NSTEMI",
                     ecg %in% .ecg$nst & tropo == opts$set$lab$no ~ "Unstable angina",
                     if_any(c(ecg, tropo), ~ is.na(.)) ~ NA,
                     .default = "Other",
                     .ptype = factor(levels = c("STEMI", "NSTEMI", "Unstable angina", "Other"))),
         sympt_type =
           case_when(sympt_douleur_type %in% .type$douleur & sympt_douleur_duree %in% .type$duree
                     ~ "Typical",
                     sympt_douleur_type == "non precordiale" |
                       sympt_douleur_type %in% .type$douleur & !sympt_douleur_duree %in% .type$duree |
                       sympt_douleur_type == "absente" & autres_signes == opts$set$lab$yes |
                       if_all(c(sympt_douleur_type, sympt_douleur_duree), ~ is.na(.)) & autres_signes == opts$set$lab$yes |
                       sympt_douleur_type == "absente" & autres_signes %in% c(opts$set$lab$no, NA)
                     ~ "Non-typical",
                     .ptype = factor(levels = c("Typical", "Non-typical")))) %>% 
  modify_if(is.character, as.factor)

labelled::var_label(d) <- 
list(id_episode = "Episode",
     sexe = "Sex",
     age_cont = "Age, years",
     age_incr = "Age (5-year increments)",
     lieu_recueil = "Center",
     pontage_coro = "Previous coronary bypass",
     angiop_coro = "Previous angioplasty",
     fdr = "Cardiovascular risk factors",
     HTA = "Hypertension",
     dyslip = "Dyslipidemia",
     diab = "Diabetes",
     obesity = "BMI ≥30",
     tabac_actif = "Current smoker",
     sympt_type = "Symptoms",
     maj_comp = "Major complications",
     sympt_arret = "Resuscitated cardiac arrest",
     sympt_oap = "Acute pulmonary oedema",
     sympt_choc = "Cardiogenic shock",
     acs_type = "ACS type",
     pontage_pdt2 = "Coronary bypass (acute phase)",
     angiop_pdt2 = "Angioplasty (acute phase)",
     pontage_sortie = "Coronary bypass (hospital discharge)",
     angiop_sortie = "Angioplasty (hospital discharge)")

##########################################################################################################################-
# 4. COMPLETION DE OPTS
##########################################################################################################################-

set_data <- \(x) {

y <- enexpr(x)
  
if (is_true(opts$set$fdr)) x <-
filter(x, year(date_index) %in% c(opts$set$fdr_start : max(year(date_index))))

assign(as_string(y), x, envir = .GlobalEnv)

x <- split(x, ~ id_episode)
x_rec_csr <- filter(x[[2]], tt_rec <= opts$set$suivi_jr)

check_pop <-
knitr::kable(
  tribble(~ ., ~ .,
          "follow-up", paste(opts$set$suivi_an, "an(s)"),
          "inclusion index", str_c(range(year(x[[1]]$date_index)), collapse = " à "),
          "date index max", max(year(x[[1]]$date_index)),
          "date recidive max", max(year(x[[2]]$date_episode)),
          "age index max", round(max(x[[1]]$age_index), 3),
          "age recidive max", round(max(x[[2]]$age_cont), 3),
          "n study pop", list(nrow(x[[1]])),
          "n rec sur follow-up", list(nrow(x_rec_csr))))

assign(paste0(y, ".1"), x[[1]], envir = .GlobalEnv)
assign(paste0(y, ".2"), x_rec_csr, envir = .GlobalEnv)

input_mv <-
  opts$tab$input$uv[!opts$tab$input$uv %in% opts$vargrp$strata] %>%
  map_dfr(~ tidy(coxph(reformulate(.x, opts$tab$model_obj$surv), x[[1]])))

input_mv <-
  unique(filter(input_mv, p.value <= 0.05)) %>% 
  mutate(term = str_extract(term, ".+(?=[:upper:][:lower:])|.+(?=$)"))

opts_tab_model <-
list(uv = coxph(reformulate("sexe",
                            opts_tab$model_obj$surv), x[[1]]),
     mv = coxph(reformulate(c(unique(c(opts$tab$input$uv[1], input_mv$term)), opts$tab$model_obj$strata),
                            opts$tab$model_obj$surv), x[[1]]))

    note_mv <- \(model) {
    
    pct_na <- style_percent(length(model$na.action)/(model$n + length(model$na.action)), symbol = TRUE, digits = 1)
    x <- glue("{model$nevent} events included on {model$n} total observations,
            {length(model$na.action)} ({pct_na}) observations were deleted for missing.")
    assign(".opts_note_mv", x, envir = .GlobalEnv)
    
    }
  
note_mv(opts_tab_model$mv)
assign("opts", append(opts, list(model = opts_tab_model), after = 3), envir = .GlobalEnv)

print(list(check_pop,
           glue("\n", "multivariable model:\n{.opts_note_mv}", "\n")))

}

set_data(d)
rm(dfp2); rm(list = ls()[str_starts(ls(), "opts_")])
