# after accidentally pressing CTR+SHIT+S
stop("HERE IN ORDER TO PREVENT FROM RUNNING ACCIDENTALLY")


# functions ----

source("figs-lib.R")


# load ----

# . fig 2 ----

outcome_none <- read_epid(path_inter)

outcome_inter <- read_epid(path_inter, interventions = TRUE)

outcome_inter_dir <- read_epid(path_inter, interventions = TRUE, direct = TRUE)


# . fig 2b ----

purrr::walk(
  c("NONE", inter_code),
  ~repair_doses(path = path_inter, dir_inter = .x)
)

doses_inter <- read_doses()


# Fig 2a) ----

inter_proportion <- calc_outcome_prop(outcome_none, outcome_inter)


# . plots ----

fig_2a.1 <- plot_outcomes(inter_proportion, mode = "proportion", type = "points")
fig_2a.2 <- plot_outcomes(inter_proportion, mode = "proportion", type = "bars")
fig_2a.3 <- plot_outcomes(inter_proportion, mode = "proportion", type = "bars_freescale")


fig_2a.1
fig_2a.2
fig_2a.3


# . save ----

ggsave(plot = fig_2a.1, path = "fig", filename = "Fig_2a-1.png",
       width = 5, height = 5)

ggsave(plot = fig_2a.2, path = "fig", filename = "Fig_2a-2.png",
       width = 5, height = 5)

ggsave(plot = fig_2a.3, path = "fig", filename = "Fig_2a-3.png",
       width = 8, height = 4)


# Fig 2b) ----

inter_cases <- calc_outcome_cases(outcome_none, outcome_inter, doses_inter)


# . plot ----


fig_2b.1 <- plot_outcomes(inter_cases, mode = "cases", type = "points")
fig_2b.2 <- plot_outcomes(inter_cases, mode = "cases", type = "bars")
fig_2b.3 <- plot_outcomes(inter_cases, mode = "cases", type = "bars_freescale")


fig_2b.1
fig_2b.2
fig_2b.3



# . save ----

ggsave(plot = fig_2b.1, path = "fig", filename = "Fig_2b-1.png",
       width = 5, height = 5)

ggsave(plot = fig_2b.2, path = "fig", filename = "Fig_2b-2.png",
       width = 5, height = 5)

ggsave(plot = fig_2b.3, path = "fig", filename = "Fig_2b-3.png",
       width = 8, height = 4)


# Fig 2c) ----

inter_protection <- calc_protection(outcome_none, outcome_inter, outcome_inter_dir)


fig_prot_symp <- plot_protection(inter_protection, "Symp")
fig_prot_hosp <- plot_protection(inter_protection, "Hosp")
fig_prot_death <- plot_protection(inter_protection, "Death")
fig_prot_GP <- plot_protection(inter_protection, "GP")
fig_prot_BD <- plot_protection(inter_protection, "Bed days")


fig_prot_symp
fig_prot_hosp
fig_prot_death
fig_prot_GP
fig_prot_BD


# . save ----

ggsave(plot = fig_prot_symp, path = "fig", filename = "Fig_prot-symp.png",
       width = 5, height = 5)

ggsave(plot = fig_prot_hosp, path = "fig", filename = "Fig_prot-hosp.png",
       width = 5, height = 5)

ggsave(plot = fig_prot_death, path = "fig", filename = "Fig_prot-death.png",
       width = 5, height = 5)

ggsave(plot = fig_prot_GP, path = "fig", filename = "Fig_prot-GP.png",
       width = 5, height = 5)

ggsave(plot = fig_prot_BD, path = "fig", filename = "Fig_prot-BD.png",
       width = 5, height = 5)




