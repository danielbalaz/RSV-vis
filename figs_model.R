# after accidentally pressing CTR+SHIT+S
stop("HERE IN ORDER TO PREVENT FROM RUNNING ACCIDENTALLY")


# functions ----

source("figs-lib.R")


# load ----

# . fig 1 ----

pos <- readr::read_csv(file = file.path(path_soln, "pos_sample_Z-norm.csv"),
                       col_types = list("Age group" = readr::col_integer(),
                                        "t" = readr::col_integer()))

# . fig 1d ----

inci <- readr::read_csv(file = file.path(path_soln, "inci_sample_Z.csv"),
                        col_types = list("t" = readr::col_integer()))

# . fig 1e ----

outcome_none <- read_epid(path_inter)


# preprocess ----

pos1 <- pos %>%
  dplyr::select(`Age group`, t, `0.5`) %>%
  dplyr::rename(age = `Age group`, rate = `0.5`)


# Fig 1a) ----

weekly <- pos1 %>%
  dplyr::group_by(t) %>%
  dplyr::summarise(rate = sum(rate)) %>%
  dplyr::mutate(date = as.Date("2010-07-01") + t * 7) %>%
  tsibble::as_tibble()


fig_1a <- ggplot(weekly, aes(date, rate)) +
  geom_line(color = "#C80000") +
  labs(x = "Week", y = "Weekly number of\nRSV-positive samples") +
  scale_x_date(date_breaks = function(x) {as.Date(paste0(2010:2017, "-07-01"))},
               labels = scales::date_format("%b'%y")) +
  theme_bw()

fig_1a

ggsave(plot = fig_1a, path = "fig", filename = "Fig_1a.png",
       width = 6, height = 2)


# Fig 1b) 1c) ----


annual <- pos1 %>%
  dplyr::mutate(age_aggr = strat_1(age = age)) %>%
  dplyr::group_by(age_aggr) %>%
  dplyr::summarise(rate = mean(rate))
  

ggplot(annual, aes(age_aggr, rate)) +
  geom_col(fill = "#C80000") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "rate of RSV-positive samples\nin unknown units")

annual_1 <- pos1 %>%
  dplyr::filter(age < 16) %>%
  dplyr::mutate(age_aggr = strat_1(age = age)) %>%
  dplyr::group_by(age_aggr) %>%
  dplyr::summarise(rate = mean(rate))

fig_1b <- ggplot(annual_1, aes(age_aggr, rate)) +
  geom_col(fill = "#C80000") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "rate of RSV-positive samples\nin unknown units")

fig_1b

ggsave(plot = fig_1b, path = "fig", filename = "Fig_1b.png",
       width = 5, height = 3)

annual_2 <- pos1 %>%
  dplyr::filter(age > 15) %>%
  dplyr::mutate(age_aggr = strat_1(age = age)) %>%
  dplyr::group_by(age_aggr) %>%
  dplyr::summarise(rate = mean(rate))

fig_1c <- ggplot(annual_2, aes(age_aggr, rate)) +
  geom_col(fill = "#C80000") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "rate of RSV-positive samples\nin unknown units")

fig_1c

ggsave(plot = fig_1c, path = "fig", filename = "Fig_1c.png",
       width = 5, height = 3)


# Fig 1d) ----



# Fig 1e) ----

outcomes_aggr <- outcome_none %>%
  dplyr::mutate(
    variable =
      factor(variable,
             levels = outcome_code,
             labels = outcome_label_long)
  ) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise_if(is.numeric, mean) %>%
  tidyr::gather(key = "age", value = "value", -variable) %>%
  dplyr::mutate(
    age_aggr = strat_2(age = age)
  )


outcomes_age <- outcomes_aggr %>%
  dplyr::group_by(variable, age_aggr) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::mutate(value_scaled = value / sum(value) * 100) %>%
  dplyr::ungroup()

fig_1e <- ggplot(outcomes_age, aes(variable, value_scaled, fill = age_aggr)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  labs(x = "", y = "Proportion of annual incidence attributable to each age group (%)") +
  scale_y_continuous(breaks = 0:5*20) +
  paletteer::scale_fill_paletteer_d(package = "NineteenEightyR", palette = "malibu") +
  coord_flip() +
  theme(legend.title = element_blank())

fig_1e

ggsave(plot = fig_1e, path = "fig", filename = "Fig_1e.png",
       width = 7, height = 3)



