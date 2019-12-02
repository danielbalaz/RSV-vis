library(magrittr)
library(ggplot2)


# paths ----

path_out <- file.path(here::here(), "..", "rsv_trans_model-master", "outputs")
path_soln <- file.path(path_out, "soln")
path_inter <- file.path(path_out, "inter")


# data -----

palette.outcomes <- c("#8080FF", "#FF8080", "#666666", "#00FF00", "#FF0000")
palette.protection <- c("#BBBBBB", "#FF0000", "#FF8000", "#FFFF00", "#00FF00", "#0000FF")

inter_code <- c("PAL_VHR_S",
                "MAB_VHR_S", "MAB_HR_S", "MAB_HR_S+", "MAB_ALL_S", "MAB_ALL_S+",
                "MAT_S", "MAT_A",
                "VAC_INF_S", "VAC_INF_A",
                "VAC_2_4_S", "VAC_5_9_S", "VAC_5_14_S", "VAC_75_S", "VAC_65_S")

inter_displ <- c("PAL-VHR",
                 "MAB-VHR", "MAB-HR-S", "MAB-HR-S+", "MAB-ALL-S", "MAB-ALL-S+",
                 "MAT-S", "MAT-A",
                 "VAC-INF-S", "VAC-INF-A",
                 "VAC-2-4", "VAC-5-9", "VAC-5-14", "VAC-75", "VAC-65")

no_intervention <- "NONE"

outcome_code <- c("symp", "hosp", "death", "gp", "bd")

outcome_label_long <- c("Symptomatic cases",
                        "Hospital admissions",
                        "Deaths",
                        "GP consultations",
                        "Bed days")

outcome_label_short <- c("Symp",
                         "Hosp",
                         "Death",
                         "GP",
                         "Bed days")

age_grp <- c("<1 mo", "1 mo", "2 mo", "3 mo",
             "4 mo", "5 mo", "6 mo", "7 mo",
             "8 mo", "9 mo", "10 mo", "11 mo",
             "1 yr", "2 yr", "3 yr", "4 yr",
             "5-9yrs", "10-14 yrs",
             "15-24 yrs", "25-34 yrs", "35-44 yrs", "44-54 yrs", "55-64yrs",
             "65-74yrs", "75yrs+")


# functions ----

strat_2 <- function(age) {
  val <- c("<1 yr", "1-4 yrs", "5-14 yrs", "15-64 yrs", "65+ yrs")
  value <- purrr::map_chr(
    age,
    ~switch(
      .x,
      "<1 mo" =, "1 mo" =, "2 mo" =, "3 mo" =,
      "4 mo" =, "5 mo" =, "6 mo" =, "7 mo" =,
      "8 mo" =, "9 mo" =, "10 mo" =, "11 mo" = val[1],
      "1 yr" =, "2 yr" =, "3 yr" =, "4 yr" = val[2],
      "5-9yrs" =, "10-14 yrs" = val[3],
      "15-24 yrs" =, "25-34 yrs" =, "35-44 yrs" =, "44-54 yrs" =, "55-64yrs" = val[4],
      "65-74yrs" =, "75yrs+" = val[5],
      NA_character_
    )
  )
  factor(value, levels = val)
}


strat_1_chr <- function(age) {
  val <- c("0-1mo", "2-3mo", "4-5mo", "6-7mo", "8-9mo", "10-11mo",
           "1yr", "2yr", "3yr", "4yr",
           "5-9yrs", "10-14yrs", "15-24yrs", "25-44yrs", "45-64yrs", "65+yrs")
  value <- purrr::map_chr(
    age,
    ~switch(
      .x,
      "<1 mo" =, "1 mo" = val[1], "2 mo" =, "3 mo" = val[2],
      "4 mo" =, "5 mo" = val[3], "6 mo" =, "7 mo" = val[4],
      "8 mo" =, "9 mo" = val[5], "10 mo" =, "11 mo" = val[6],
      "1 yr" = val[7], "2 yr" = val[8], "3 yr" = val[9], "4 yr" = val[10],
      "5-9yrs" = val[11], "10-14 yrs" = val[12],
      "15-24 yrs" = val[13], "25-34 yrs" =, "35-44 yrs" = val[14],
      "44-54 yrs" =, "55-64yrs" = val[15],
      "65-74yrs" =, "75yrs+" = val[16],
      NA_character_
    )
  )
  factor(value, levels = val)
}


strat_1 <- function(age) {
  val <- c("0-1mo", "2-3mo", "4-5mo", "6-7mo", "8-9mo", "10-11mo",
           "1yr", "2yr", "3yr", "4yr",
           "5-9yrs", "10-14yrs", "15-24yrs", "25-44yrs", "45-64yrs", "65+yrs")
  value <- purrr::map_chr(
    age,
    ~switch(
      as.character(.x),
      "0" =, "1" = val[1], "2" =, "3" = val[2],
      "4" =, "5" = val[3], "6" =, "7" = val[4],
      "8" =, "9" = val[5], "10" =, "11" = val[6],
      "12" = val[7], "13" = val[8], "14" = val[9], "15" = val[10],
      "16" = val[11], "17" = val[12],
      "18" = val[13], "19" =, "20" = val[14],
      "21" =, "22" = val[15],
      "23" =, "24" = val[16],
      NA_character_
    )
  )
  factor(value, levels = val)
}

read_epid <- function(path = path_inter, interventions = FALSE, direct = FALSE) {
  
  if (interventions) {
    inter_name <- inter_code
  } else {
    inter_name <- no_intervention
  }
  
  
  if (direct) {d <- "_d"} else {d <- ""}
  
  
  fnames <- purrr::map(
    inter_name,
    ~file.path(path, .x, paste0(outcome_code, d, ".csv"))
  )
  
  .inter <- purrr::map(
    fnames,
    ~purrr::map_dfr(
      .x,
      ~readr::read_csv(
        .x,
        col_types = list(seed = readr::col_integer())
      ),
      .id = "variable"
    ) %>%
      dplyr::mutate(variable = outcome_code[as.integer(variable)],
                    seed = factor(seed, levels = unique(seed)))
  )
  
  if (!interventions) {
    .inter <- .inter[[1]]
  }
  
  .inter
}

repair_doses <- function(path = path_inter, dir_inter) {
  # read a file unsuitable to read by readr::read_csv
  f <- file(description = file.path(path, dir_inter, "no_doses.csv"),
            open = "r")
  lns <- readLines(con = f)
  close(f)
  
  # the number of columns in data
  N <- max(purrr::map_int(lns[-1], ~length(gregexpr(",", .x, fixed = TRUE)[[1]])))
  
  # construct correct header
  doses_head <- paste0("vaccine,seed,", paste(seq_len(N), collapse = ","))
  
  # add vaccine information to the data
  doses_data <- paste(c("Palivizumab", "other"), lns[-1], sep = ",")
  
  # replace the first line (header)
  lns <- c(doses_head, doses_data)

  # create a file readable by readr::read_csv()
  f <- file(description = file.path(path, dir_inter, "no_doses_.csv"),
            open = "w")
  writeLines(lns, f)
  close(f)
  
  invisible(N)
}

# intervention doses
read_doses <- function(path = path_inter) {
  inter_code %>%
    purrr::map_dfr(
      ~readr::read_csv(
        file.path(path, .x, "no_doses_.csv"),
        col_types = list(seed = readr::col_integer())
      ),
      .id = "inter"
    ) %>%
    dplyr::mutate(inter = factor(inter_displ[as.integer(inter)], levels = inter_displ),
                  vaccine = factor(vaccine, levels = unique(vaccine)),
                  seed = factor(seed, levels = unique(seed))) %>%
    tidyr::gather(key = "step", value = "dose",
                  -inter, -vaccine, -seed) %>%
    dplyr::mutate(step = as.integer(step)) %>%
    dplyr::arrange(inter, seed, vaccine, step)
}

sum_age <- function(.tbl) {
  .tbl %>%
    tidyr::gather(key = "age", value = "rate", -variable, -seed) %>%
    dplyr::group_by(variable, seed) %>%
    dplyr::summarise(rate = sum(rate))
}


eval_inter_1 <- function(.tbl_base, .tbl_inter, mode = "") {
  mode_avail <- c("proportion", "diff")
  
  .tbl <- dplyr::full_join(
    sum_age(.tbl_base),
    sum_age(.tbl_inter),
    by = c("variable", "seed")
  )
  
  if (mode == mode_avail[1]) {
    .tbl <- dplyr::mutate(.tbl, rate = (rate.x - rate.y) / rate.x)
  } else if (mode == mode_avail[2]) {
    .tbl <- dplyr::mutate(.tbl, rate = rate.x - rate.y)
  } else {
    stop(sprintf("'mode' must be one of: %s", paste(mode_avail, collapse = ", ")))
  }
  
  .tbl %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(rate = median(rate))
}

eval_inter_indir_1 <- function(.tbl_base, .tbl_inter, .tbl_inter_dir, f.strat) {
  .inter <- dplyr::full_join(
    tidyr::gather(.tbl_inter,     key = "age", value = "inter",  -variable, -seed),
    tidyr::gather(.tbl_inter_dir, key = "age", value = "interD", -variable, -seed),
    by = c("variable", "seed", "age")
  ) %>%
    dplyr::mutate(diff = interD - inter)
  
  .base <- .tbl_base %>%
    tidyr::gather(key = "age", value = "base", -variable, -seed) %>%
    dplyr::group_by(variable, seed) %>%
    dplyr::summarise(base = sum(base))
    
  .all <- dplyr::full_join(
      .inter, .base,
      by = c("variable", "seed")
    ) %>%
    dplyr::mutate(rate = diff / base,
                  age_strat = f.strat(age)) %>%
    dplyr::group_by(variable, seed, age_strat) %>%
    dplyr::summarise(rate = sum(rate))
  
  .select_seed <- .all %>%
    dplyr::group_by(variable, seed) %>%
    dplyr::summarise(total = sum(rate)) %>%
    dplyr::arrange(variable, total) %>%
    dplyr::filter(total == total[ceiling(length(total)/2)]) %>%
    dplyr::select(-total)
  
  dplyr::right_join(.all, .select_seed,
                    by = c("variable", "seed")) %>%
    dplyr::ungroup()
}

wrap_up <- function(.tbl) {
  .tbl %>%
    dplyr::mutate(
      inter = factor(inter_displ[as.integer(inter)],
                     levels = inter_displ),
      variable =
        factor(variable,
               levels = outcome_code,
               labels = outcome_label_short)
    ) %>%
    dplyr::arrange(inter, variable)
}

eval_inter <- function(.tbl_base, .tbl_inter, mode = "") {
  .tbl_inter %>%
    purrr::map_dfr(~eval_inter_1(.tbl_base, .x, mode = mode),
                   .id = "inter") %>%
    wrap_up()
}

eval_inter_indir <- function(.tbl_base, .tbl_inter, .tbl_inter_dir, f.strat) {
  purrr::map2_dfr(.tbl_inter,
                  .tbl_inter_dir,
                  ~eval_inter_indir_1(.tbl_base, .x, .y, f.strat),
                  .id = "inter") %>%
    dplyr::select(-seed) %>%
    wrap_up()
}

combine_effects <- function(.tbl_dir, .tbl_indir) {
  .tbl_indir %<>%
    dplyr::mutate(protection = paste0("Indir. prot. (", age_strat, ")")) %>%
    dplyr::select(-age_strat)
  
  .tbl_dir$protection = "Direct prot."
  
  dplyr::bind_rows(.tbl_dir, .tbl_indir) %>%
    dplyr::mutate(protection = factor(protection, levels = unique(protection)),
                  rate = 100 * rate) %>%
    dplyr::arrange(inter, variable, protection)
}

get_doses <- function(.tbl, last_steps = 52L) {
  .tbl %>%
    dplyr::filter(seed == levels(seed)[1], step > max(step) - last_steps) %>%
    dplyr::group_by(inter) %>%
    dplyr::summarise(dose = sum(dose))
}

cases_averted <- function(.tbl_inter, .tbl_doses) {
  dplyr::full_join(.tbl_inter, .tbl_doses) %>%
    dplyr::mutate(cases_averted = rate / dose * 1000)
}

calc_outcome_prop <- function(.tbl_base, .tbl_inter) {
  eval_inter(.tbl_base, .tbl_inter, mode = "proportion")
}

calc_outcome_cases <- function(.tbl_base, .tbl_inter, .tbl_doses) {
  cases_averted(
    eval_inter(.tbl_base, .tbl_inter, mode = "diff"),
    get_doses(.tbl_doses)
  )
}

calc_protection <- function(.tbl_base, .tbl_inter, .tbl_inter_dir, f.strat = strat_2) {
  direct_prop <- eval_inter(.tbl_base, .tbl_inter_dir, mode = "proportion")
  indir <- eval_inter_indir(.tbl_base, .tbl_inter, .tbl_inter_dir, f.strat)
  combine_effects(direct_prop, indir)
}

eval_inter_1.wrong <- function(.tbl_base, .tbl_inter, mode = "") {
  mode_avail <- c("proportion", "diff")
  
  .tbl <- dplyr::full_join(
    sum_age(.tbl_base),
    sum_age(.tbl_inter),
    by = c("variable", "seed")
  )
  
  if (mode == mode_avail[1]) {
    .tbl <- dplyr::mutate(.tbl, rate = (rate.x - rate.y) / rate.x)
  } else if (mode == mode_avail[2]) {
    .tbl <- dplyr::mutate(.tbl, rate = rate.x - rate.y)
  } else {
    stop(sprintf("'mode' must be one of: %s", paste(mode_avail, collapse = ", ")))
  }
  
  .tbl %>%
    dplyr::arrange(variable, rate) %>% 
    dplyr::group_by(variable) %>%
    dplyr::summarise(rate = rate[5])
}

# mode = c("proportion", "cases")
# type = c("points", "bars", "bars_freescale")
plot_outcomes <- function(.tbl, mode, type) {
  opt.mode <- c("proportion", "cases")
  opt.type <- c("points", "bars", "bars_freescale")
  
  stopifnot(mode %in% opt.mode, type %in% opt.type)
  
  col_gridlines <- "#CCCCCC"
  col_border <- "#444444"
  col_transparent <- "#FFFFFF00"
  
  N_inter <- nlevels(.tbl$inter)
  
  
  if (mode == opt.mode[1]) {
    .tbl %<>% dplyr::mutate(y = rate * 100)
    
    lab_y <- "Proportion of annual cases averted (%)"
    
  } else {
    .tbl %<>% dplyr::rename(y = cases_averted)
    
    lab_y <- "Number of cases averted per 1,000 courses"
  }
  
  
  p <- ggplot(.tbl) +
    coord_flip() +
    labs(x = "Programmes", y = lab_y)
  
  if (type == opt.type[1]) {
    
    N_var <- nlevels(.tbl$variable)
    N_break <- floor((N_var + 1L) / 2L)
    levels_var <- levels(.tbl$variable)
    levels_inter <- levels(.tbl$inter)
    
    p <- p +
      geom_hline(yintercept = 0, color = col_gridlines) +
      geom_vline(xintercept = seq_len(N_inter - 1) * N_var + .5,
                 color = col_gridlines) +
      geom_point(
        aes(
          interaction(variable, inter), y,
          color = variable),
        pch = 4, stroke = 1.5, size = 1) +
      scale_x_discrete(labels = levels_inter,
                       breaks = paste(levels_var[N_break],
                                      levels_inter,
                                      sep = ".")) +
      scale_color_manual(values = palette.outcomes) +
      guides(color = guide_legend(reverse = TRUE))
    
  } else {
    
    N_var <- 1L
    
    p <- p +
      geom_col(
        aes(inter, y, fill = variable),
        position = "dodge") +
      geom_vline(xintercept = seq_len(N_inter - 1) * N_var + .5,
                 color = col_gridlines) +
      scale_fill_manual(values = palette.outcomes)
    
    if (type == opt.type[2]) {
      p <- p +
        guides(fill = guide_legend(reverse = TRUE))
    }
    
  }
  
  if (type %in% opt.type[1:2]) {
    p <- p +
      theme(legend.title = element_blank(),
            panel.border = element_rect(color = col_border, fill = col_transparent),
            panel.background = element_blank(),
            panel.grid.major.y = element_blank())
  } else {
    p <- p +
      theme(legend.title = element_blank(),
            panel.border = element_rect(color = col_border, fill = col_transparent),
            panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.spacing.x = unit(0.6, "lines"),
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_text(size = 8)) +
      facet_grid(cols = vars(variable), scales = "free_x")
  }
  
  
  return(p)
}


plot_protection <- function(.tbl, outcome,
                            custom_palette = FALSE, legend.position = "bottom",
                            ...) {
  stopifnot(outcome %in% c("Symp", "Hosp", "Death", "GP", "Bed days"))
  
  str <- switch(outcome,
                "Symp" = "symptomatic cases",
                "Hosp" = "hospital admissions",
                "Death" = "deaths",
                "GP" = "GP consultations",
                "Bed days" = "bed days")
  
  p <- .tbl %>%
    dplyr::filter(variable == outcome) %>%
    dplyr::mutate(rate = ifelse(rate < 0, 0, rate)) %>% 
    ggplot(aes(inter, rate, fill = protection)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    coord_flip() +
    labs(y = sprintf("Proportion of annual %s averted (%%)", str), x = "") +
    theme(legend.title = element_blank(),
          legend.position = legend.position,
          ...)
  
  if (!custom_palette) {
    p <- p + scale_fill_manual(values = palette.protection)
  }
  
  return(p)
}


