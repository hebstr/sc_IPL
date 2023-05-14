opts_set <- \(suivi_an = 1, suivi_jr = suivi_an * 365,
              fdr = FALSE, fdr_start = 2014,
              fdr_suffix = "_fdr", output_suffix = "",
              sex_m = "Men", sex_f = "Women",
              yes = "Yes", no = "No", na = "Missing data",
              int_sep = ":", out_sep = ";", ci_sep = ";",
              ci_lim = "[", ci_level = "95", ci_term = "%CI",
              p_format = ~ style_pvalue(.x, digits = 3), p_seuil = 0.05,
              font_alpha = "Bahnschrift", font_num = "calibri",
              color_ref = "#999999", color_back_light = "white",
              color_back_dark = "#292929", color_front_light = "#e1f6ff",
              color_front_dark = "#0099CC", sep_gap = FALSE) {

if (is_true(fdr)) output_suffix <- paste0(output_suffix, fdr_suffix)

lab <-
list(lab = list(sex_m = sex_m, sex_f = sex_f,
                yes = yes, no = no, na = na))

sep <- list(int = int_sep, out = out_sep, ci = ci_sep) %>% map(~ paste(., ""))
if (is_true(sep_gap)) sep <- map(sep, ~ paste("", .))
sep <- list(sep = sep)

if (ci_lim == "[") { ci_low <- "[" ; ci_high <- "]" }
else { ci_low <- "(" ; ci_high <- ")" }

ci <-
list(ci = list(low = ci_low,
               high = ci_high,
               label = paste0(ci_low, ci_level, ci_term, ci_high),
               data = paste0(ci_low, "{conf.low}", ci_sep, " {conf.high}", ci_high)))

font <-
list(font = list(alpha = font_alpha,
                 num = font_num))

color <-
list(color = list(ref = color_ref,
                  back_light = color_back_light,
                  back_dark = color_back_dark,
                  front_light = color_front_light,
                  front_dark = color_front_dark))

opts <- tibble(suivi_an, suivi_jr, fdr, fdr_start, fdr_suffix, output_suffix)

assign("opts", list(set = c(opts, p_format = p_format, p_seuil = p_seuil, lab, sep, ci, font, color)), envir = .GlobalEnv)

}

#-------------------------------------------------------------------------------------------------------------

opts_tab <- \(base, uv, vargrp = NULL, surv, strata = NULL, abb, abb_fdr = NULL, note,
              var_fdr = opts_vargrp$fdr, before = "sympt_type") {
  
    add_vec <- \(to, var, before) {
    
    x <- setdiff(c(to, var), var)
    append(x, var, after = match(before, x) - 1)
    
    }
  
    group <- \(...)
    list(labels = list(...) %>% map(~ .[1]) %>% unlist,
         levels = list(...) %>% map(~ .[-1]) %>% unlist)
    
    model_obj <- \(surv, strata) {
      
    obj <- \(x, y) paste0(x, "(", str_c(y, collapse = ","), ")")
    
    list(surv = obj("Surv", opts_vargrp$surv),
         strata = obj("strata", opts_vargrp$strata))
    
    }
  
abb <- enexpr(abb)
abb_fdr <- enexpr(abb_fdr)
with_abb <- expr(with(opts_abb, !!abb))
with_abb_fdr <- expr(with(opts_abb, c(!!abb_fdr, !!abb)))

if (is_false(opts$set$fdr))
list(input = list(base = base, uv = uv),
     vargrp = group(vargrp),
     model_obj = model_obj(surv, strata),
     abb = eval(with_abb),
     note = note)

else
list(input = (list(base = add_vec(to = base, var = var_fdr, before = before),
                   uv = add_vec(to = uv, var = var_fdr, before = before))),
     vargrp = group(vargrp, var_fdr),
     model_obj = model_obj(surv, strata),
     abb = eval(with_abb_fdr),
     note = note)
  
}

#--------------------------------------------------------------------------------------------------------------

abbr <- \(...) {
  
e <- env("~" = \(x, y) paste0(enexpr(x), opts$set$sep$int, y))

abb <-
  list(...) %>%
  map(~ eval(enexpr(.), e)) %>%
  setNames(str_extract(., "[:alnum:]+(?=)"))

}

#----------------------------------------------------------------------------------------------------------------

str_abb <- \(...)
paste0(str_c(c(...), collapse = opts$set$sep$out), ".")

#----------------------------------------------------------------------------------------------------------------

str_label <- \(data, ...)
c(...) %>%
  map_chr(~ with(data, eval(sym(.))) %>% labelled::var_label()) %>% 
  str_flatten_comma(" and ")

#----------------------------------------------------------------------------------------------------------------

gt_template <- \(x, title, note_1 = NULL, ...) {

x <-
as_gt(x) %>%
  tab_header(md(title)) %>%
  opt_align_table_header(align = "left") %>%
  opt_table_font(font = opts$set$font$alpha) %>%
  tab_options(table.width = px(width),
              table.font.size = px(12),
              table.font.color = opts$set$color$back_dark,
              table.background.color = opts$set$color$front_light,
              heading.background.color = opts$set$color$back_light,
              heading.title.font.size = pct(95),
              heading.border.bottom.style = "none",
              table.border.top.style = "none",
              table.border.bottom.style = "none", 
              column_labels.border.top.style = "none",
              column_labels.border.bottom.width = px(1),
              column_labels.border.bottom.color = opts$set$color$back_dark,
              column_labels.background.color = opts$set$color$back_light,
              table_body.border.top.width = px(1),
              table_body.border.top.color = opts$set$color$back_dark,
              table_body.border.bottom.width = px(1),
              table_body.border.bottom.color = opts$set$color$back_dark,
              table.border.bottom.width = px(1),
              table.border.bottom.color = opts$set$color$back_dark,
              table_body.hlines.style = "none",
              container.padding.x = px(10),
              heading.padding = px(10),
              data_row.padding = px(3),
              data_row.padding.horizontal = px(5),
              row.striping.include_table_body = TRUE,
              row.striping.background_color = opts$set$color$back_light,
              footnotes.marks = "standard",
              footnotes.background.color = opts$set$color$back_light,
              footnotes.padding = px(1),
              footnotes.font.size = pct(80), ...) %>% 
  tab_style(style = cell_text(align = "justify"),
            locations = list(cells_title(), cells_footnotes())) %>%
  tab_style(style = cell_text(size = px(11)),
            locations = cells_body(columns = grep("p.v", names(x[[1]])))) %>% 
  tab_style(style = cell_text(font = opts$set$font$num),
            locations = cells_body(columns = map(c("stat", "p.v", "estim"),
                                                 ~ grep(., names(x[[1]]))) %>% unlist))

if(sum(grep("coef", names(x[[1]]))) >= 1)
x %>%
    tab_footnote(c(eval(opts$tab$note$strata), .opts_note_mv, opts$tab$note$p,
                   str_abb(.ref, .estim, .estim_ajust, opts$abb$CI, opts$tab$abb))) %>%
    tab_footnote(opts$tab$note$ajust,
                 cells_column_labels(p.value_2))

else
x %>% 
  tab_footnote(c(opts$tab$note$p, str_abb(.n, .SD, opts$tab$abb))) %>% 
  tab_footnote(opts$tab$note$vargrp,
               cells_body(columns = label, rows = variable %in% opts$tab$vargrp$labels))

}

#----------------------------------------------------------------------------------------------------------------

theme_cuminc <- \(margin_title = margin(0,0,0,0), ...)
theme_classic() %+replace%
  theme(line = element_line(linewidth = 0.3),
        text = element_text(family = opts$set$font$alpha),
        axis.title = element_text(face = "bold",
                                  size = 9),
        axis.title.x = element_text(vjust = -1),
        axis.title.y.left = element_text(vjust = 1),
        axis.text = element_text(color = opts$set$color$back_dark,
                                 size = 8),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(0,0,0,0),
        plot.caption = element_textbox(size = 8,
                                       width = unit(1, "npc"),
                                       margin = margin_title),
        plot.caption.position = "plot",
        ...)

#---------------------------------------------------------------------------------------------------------------

theme_risktable <- \(margin_table = margin(0,0,0,0), ...)
list(theme_risktable_default(),
     theme(text = element_text(family = opts$set$font$alpha),
           plot.title = element_text(size = 7.5,
                                     face = "bold",
                                     margin = margin(0,0,0,0)),
           plot.title.position = "plot",
           plot.margin = margin_table,
           panel.background = element_blank(),
           plot.background = element_blank(),
           axis.text.y = element_text(size = 7),
           ...))

#-------------------------------------------------------------------------------------------------------------

tab_reg <- \(x = NULL, ref, ...) {

exponentiate <- TRUE
pvalue_fun <- opts$set$p_format

if (is.data.frame(x)) { 
  
x <- x[c(opts$vargrp$surv, opts$tab$input$uv)]
tbl_uvregression(x, y = !!expr(Surv(!!!syms(opts$vargrp$surv))),
                 exponentiate = exponentiate, pvalue_fun = pvalue_fun,
                 show_single_row = -categorical(x, opts$tab$input$uv),
                 hide_n = TRUE, ...)

} else
  tbl_regression(x = opts$model$mv,
                 exponentiate = exponentiate, pvalue_fun = pvalue_fun,
                 show_single_row = -categorical(ref, opts$tab$input$mv), ...)

}

#----------------------------------------------------------------------------------------------------------------

tab_data <- \(x) {
  
class <- class(x)[1]
y <- substitute(x)

assign_tab <- \(name, ext, obj)
assign(paste0(name, ext), obj, envir = .GlobalEnv)

if (class != "gt_tbl") {
  
    body_style <- list(assign_tab(y, "_body", x$table_body),
                       assign_tab(y, "_style", x$table_styling$header))
    
    if (class == "tbl_summary")
        assign_tab(y, "_meta", x$meta_data %>% unnest(cols = "df_stats", names_repair = "unique"))
    
    if (class == "tbl_uvregression")
        list(assign_tab(y, "_meta", x$meta_data),
             assign_tab(y, "_data", x$inputs$data %>% filter(.[[2]] != 0)))
    
    if (class %in% c("tbl_regression", "tbl_merge", "tbl_strata"))
        list(body_style)
  
} else assign_tab(y, ".gt_style", x$`_options`)
  
}

#----------------------------------------------------------------------------------------------------------------

binr <- \(x, var, condition, auto_bin = FALSE) {
  
if (is_true(auto_bin))
mutate(x, {{ var }} := ifelse(if_any({{ condition }}, ~ . == opts$set$lab$yes), opts$set$lab$yes, opts$set$lab$no))

else
mutate(x, {{ var }} := ifelse({{ condition }}, opts$set$lab$yes, opts$set$lab$no))
  
}

#----------------------------------------------------------------------------------------------------------------

str_num <- \(nrep, ...) {
  
suffix <- paste0("_", seq(nrep))
map(list(...), ~ paste0(., suffix)) %>% unlist
  
}

#----------------------------------------------------------------------------------------------------------------

categorical <- \(data, ...) {
  
level <- c(...) %>% map_int(~ with(data, eval(sym(.))) %>% nlevels())
c(...)[level > 2]

}

#----------------------------------------------------------------------------------------------------------------

merge_estimate_ci <- \(x, name = estimate_ci, multi = 1)
mutate(x, across(c(estimate, contains("conf")), ~ format(round(. * multi, 2), nsmall = 2)),
       {{ name }} := glue("{estimate} ", opts$set$ci$data))

#----------------------------------------------------------------------------------------------------------------

tab_format <- \(x, hide_n = TRUE) {

x <-
x %>% modify_header(label ~ "**Characteristics**", p.value ~ "**p**") %>%
      bold_p(t = opts$set$p_seuil)
  
if (str_detect(class(x)[1], "reg")) {

    x <-  
    x %>% modify_table_body(~ .x %>%
            mutate(estim_cap =
                     ifelse(class(x)[1] == "tbl_regression", glue("a{coefficients_label}"), coefficients_label),
                   estim_label =
                     ifelse(coefficients_label == "OR", "odds ratio", "hazard ratio"))) %>%
          modify_header(estimate ~ glue("**{unique(.$table_body$estim_cap)} {opts$set$ci$label}**")) %>% 
          modify_table_styling(columns = estimate, rows = !is.na(ci),
                               cols_merge_pattern = paste("{estimate}", opts$set$ci$data)) %>% 
          modify_table_styling(columns = ci, hide = TRUE)
    
    estim_cap <- "unique(x$table_body$coefficients_label)"
    estim_label <- "unique(x$table_body$estim_label)"
    
    assign(".estim", glue("{", estim_cap, "}: ", "{", estim_label, "}"), envir = .GlobalEnv)
    assign(".estim_ajust", glue("a{", estim_cap, "}: ", "adjusted {", estim_label, "}"), envir = .GlobalEnv)
    assign(".ref", abbr(ref ~ "reference for dichotomous variables"), envir = .GlobalEnv)
    
    levels <- model_list_terms_levels(opts$model$mv) %>% .[c("variable", "reference_level")] %>% distinct()
    
    x <-  
    x %>% modify_table_body(~ .x %>%
            left_join(levels, by = "variable") %>% 
            mutate(level = str_extract(term, "[:upper:].+"),
                   label = case_when(var_type == "dichotomous" & !reference_level %in% c("No", NA)
                                     ~ glue("{level} — ref: {reference_level}"),
                                     var_type == "dichotomous" & reference_level %in% c("No", NA)
                                     ~ glue("{label} — ref: {opts$set$lab$no}"),
                                     .default = label)))
    
    if (!hide_n) {
      
        if (length(unique(x$table_body$N_event)) == 1) x <-
            x %>% modify_table_body(~ .x %>%
                    mutate(n_event = ifelse(n_event == N_event & var_type == "continuous", NA, n_event))) %>%
                  modify_header(n_event ~ glue("**n (N={unique(.$table_body$N_event)})**"))
          
        else x <-
            x %>% modify_table_body(~ .x %>%
                    mutate(n_event = ifelse(n_event == N_event, NA, n_event),
                           N_event_uv = ifelse(var_type == "categorical" & header_row == FALSE, NA, N_event))) %>%
                  modify_header(n_event ~ "**n**", N_event_uv ~ "**N**") %>%
                  modify_table_body(~ .x %>% relocate(N_event_uv, .after = n_event))
        
    }

x <- x %>% modify_footnote(everything() ~ NA, abbreviation = TRUE)
  
} else {
  x <-
  x %>% modify_table_body(~ .x %>%
          mutate(across(contains("stat_"), ~ ifelse(str_starts(., "0"), "—", .)))) %>%
        modify_header(c(stat_1, stat_2) ~ "**{level}<br>(n={n}, {style_percent(p)}%)**") %>%
        modify_footnote(everything() ~ NA)
  
  assign(".n", abbr(n ~ "number of events"), envir = .GlobalEnv)
  assign(".SD", abbr(SD ~ "standard deviation"), envir = .GlobalEnv)
  
}

if (!is.null(opts$tab$vargrp)) x <-
    x %>% modify_table_body(~ .x %>%
            mutate(row_type = ifelse(variable %in% opts$tab$vargrp$levels, "level", row_type))) %>%
          modify_table_styling(columns = label, rows = row_type == "level", text_format = "indent2")

}

#---------------------------------------------------------------------------------------------------------------

output <- \(x, dir = "output", ...) {

if(!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
  
y <- paste0(enexpr(x), opts$set$output_suffix)
if (is.ggplot(x)) y <- paste0(y, "_", opts$set$suivi_an, "a")

path <- paste0(dir, "/", y)
.html <- paste0(path, ".html")
.svg <- paste0(path, ".svg")
.png <- paste0(path, ".png")

message(paste0("~/", dir, "/", y, ".png", "\n"))

if (str_detect(class(x)[1], "tbl")) {
    
    if (class(x)[1] != "gt_tbl") x <- as_gt(x)
    gtsave(x, file = .html)
    browseURL(.html)
    webshot::webshot(.html, file = .png, vwidth = width + width*0.0625, vheight = 1, zoom = 3)

} else {
  
    if (is.ggplot(x)) {
        
        plot <- htmltools::capturePlot(x, .svg, svg, width = opts$fig$width, height = opts$fig$height, ...)
        browseURL(plot)
        ggsave(.png, width = opts$fig$width, height = opts$fig$height, ...)
    
    }

}}
