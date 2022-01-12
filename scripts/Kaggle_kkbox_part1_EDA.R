library(tidyverse)
library(reticulate)
library(forcats)
library(lubridate)
library(labelled)
library(gtsummary)
library(ggpubr)
library(RColorBrewer)
library(lemon)
library(paletteer)
library(dbplot)
library(survival)
library(survminer)
library(bshazard)
library(magick)
library(cowplot)
library(rms)

#### Data_import ####
# Data files from Kaggle KKBox competition were preliminary downloaded.
# Files for transactions were combined into one.
# Files for user logs were combined into one.
# ~ 1% of users data for whom were present in members, transactions and user logs files were sampled.
# Their IDs (msno) are given in msno_sample.csv file in data directory.
# It is assumed that you will prepare trans_sample.csv with records for these IDs,
# as well as ulogs_sample.csv and members_sample.csv

# All newly created variables are described in Kaggle_KKBox_survival.html file

trans <- read_csv("data/trans_sample.csv", 
                  col_types = cols(transaction_date = col_date(),
                                   membership_expire_date = col_date(),
                                   is_cancel = col_logical(),
                                   is_auto_renew = col_logical(),
                                   payment_method_id = col_factor()))
trans <- distinct(trans) %>%
  mutate(payment_method_id = fct_infreq(payment_method_id)) %>%
  arrange(msno, transaction_date)

members <- read_csv("data/members_sample.csv",
                    col_types = cols(registration_init_time = col_date(),
                                     city = col_factor(),
                                     gender = col_factor(),
                                     registered_via = col_factor()))
members <- members %>%
  mutate(city = fct_infreq(city),
         registered_via = fct_infreq(registered_via),
         gender = fct_explicit_na(factor(gender, c("female", "male"), 
                                         c("Female", "Male")),
                                  "Unknown"),
         year_reg = factor(year(registration_init_time)),
         month_reg = factor(month(registration_init_time),
                            labels = month.abb))

ulogs <- read_csv("data/ulogs_sample.csv",
                  col_types = cols(date = col_date("%Y%m%d")))
ulogs$num_tot <- pmap_dbl(ulogs[, paste0("num_", c(25,50,75,985,100))], sum)
ulogs <- ulogs %>%
  mutate(av_times_played = num_tot / num_unq,
         num_repeats = num_tot - num_unq, 
         sh_25 = num_25 / num_tot,
         sh_50 = num_50 / num_tot,
         sh_75 = num_75 / num_tot,
         sh_985 = num_985 / num_tot,
         sh_100 = num_100 / num_tot,
         av_pct_played = (num_25*0.125 + num_50*0.375 + num_75*0.625 + num_985*0.8675 + num_100*0.9925)/num_tot*100)

var_label(trans) <- list(payment_method_id = "Payment method",
                         payment_plan_days = "Payment plan days",
                         plan_list_price = "Plan list price",
                         actual_amount_paid = "Actual amount paid",
                         is_auto_renew = "Auto-renew",
                         is_cancel = "Cancellation")

var_label(members) <- list(city = "City",
                           bd = "Age",
                           gender = "Gender",
                           registered_via = "Registration method",
                           year_reg = "Year of registration at the service",
                           month_reg = "Month of registration at the service")

var_label(ulogs) <- list(num_25 = "Number of songs played less than 25% of the song length",
                         num_50 = "Number of songs played between 25% to 50% of the song length", 
                         num_75 = "Number of songs played between 50% to 75% of the song length", 
                         num_985 = "Number of songs played between 75% to 98.5% of the song length",  
                         num_100 = "Number of songs played over 98.5% of the song length",
                         num_tot = "Total number of songs played",
                         num_unq = "Number of unique songs played",
                         av_times_played = "Average number of times of listening to one song",
                         num_repeats = "Number of repeats",
                         sh_25 = "Share of songs played less than 25% of the song length",
                         sh_50 = "Share of songs played between 25% to 50% of the song length", 
                         sh_75 = "Share of songs played between 50% to 75% of the song length", 
                         sh_985 = "Share of songs played between 75% to 98.5% of the song length",  
                         sh_100 = "Share of songs played over 98.5% of the song length",
                         av_pct_played = "Average % of one song's length played",
                         total_secs = "Total seconds played")

#### Transactions ####

trans <- trans %>%
  arrange(msno, transaction_date) %>%
  group_by(msno) %>%
  mutate(time_bw_exp_trans = as.numeric(membership_expire_date - transaction_date, 'days'),
         time_bw_exp_nexttrans = ifelse(is.na(lead(transaction_date)),
                                        as.numeric(as.Date("2017-03-31") - membership_expire_date, 'days'),
                                        as.numeric(lead(transaction_date) - membership_expire_date, 'days')),
         time_bw_trans = as.numeric(lead(transaction_date) - transaction_date, 'days'),
         trans_id = 1:n()) %>%
  ungroup()

trans_df <- trans %>%
  arrange(msno, transaction_date) %>%
  mutate(membership_expire_date_cor = as.Date(ifelse(membership_expire_date < transaction_date, 
                                                     transaction_date, membership_expire_date),
                                              "1970-01-01")) %>%
  group_by(msno) %>%
  mutate(time_bw_exp_trans = as.numeric(membership_expire_date_cor - transaction_date, 'days'),
         time_bw_exp_nexttrans = ifelse(is.na(lead(transaction_date)),
                                        as.numeric(as.Date("2017-03-31") - membership_expire_date_cor, 'days'),
                                        as.numeric(lead(transaction_date) - membership_expire_date_cor, 'days'))) %>%
  ungroup()

# number of transactions and time between them

trans_stat <- trans %>%
  group_by(msno) %>%
  summarise(n = n(),
            sum_cancel = sum(is_cancel),
            sum_auto = sum(is_auto_renew),
  ) %>%
  ungroup()

trans_freq_max <- trans %>%
  filter(!is.na(time_bw_trans)) %>%
  group_by(time_bw_trans) %>%
  tally() %>%
  arrange(-n)

p1 <- trans_stat %>%
  filter(n < quantile(trans_stat$n, 0.99)) %>%
  ggplot() +
  aes(x = n) +
  geom_histogram(binwidth = 1, fill = "#4682B4") +
  geom_text(x = 15, y = 4000, label = sprintf("Mean (SD): %.1f (%.1f)", mean(trans_stat$n),
                                              sd(trans_stat$n)),
            hjust = 0, color = "grey20", size = 3.5) +
  geom_text(x = 15, y = 3700, label = sprintf("Median (Q1, Q3): %.0f (%.0f, %.0f)", 
                                              median(trans_stat$n), quantile(trans_stat$n, 0.25),
                                              quantile(trans_stat$n, 0.75)),
            hjust = 0, color = "grey20", size = 3.5) +
  geom_text(x = 15, y = 3400, label = sprintf("Min-Max: %.0f-%.0f", min(trans_stat$n),
                                              max(trans_stat$n)),
            hjust = 0, color = "grey20", size = 3.5) +
  labs(x = "", y = "", title = "Number of records per ID in transactions\n(for values < 99th percentile)") +
  scale_x_continuous(breaks = seq(0, 30, 5), expand = c(0,0),
                     limits = c(0, 30)) +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0, 8000, 1000), labels = c(0, paste0(seq(1,8,1), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p2 <- trans %>%
  filter(time_bw_trans < quantile(trans$time_bw_trans, 0.99, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = time_bw_trans) +
  geom_histogram(binwidth = 5, fill = "#4682B4") +
  geom_text(x = 90, y = 140000, label = sprintf("Mean (SD): %.1f (%.1f)", mean(trans$time_bw_trans, na.rm = TRUE),
                                                sd(trans$time_bw_trans, na.rm = TRUE)),
            hjust = 0, color = "grey20", size = 3.5) +
  geom_text(x = 90, y = 130000, label = sprintf("Median (Q1, Q3): %.0f (%.0f, %.0f)",
                                                median(trans$time_bw_trans, na.rm = TRUE), 
                                                quantile(trans$time_bw_trans, 0.25, na.rm = TRUE),
                                                quantile(trans$time_bw_trans, 0.75, na.rm = TRUE)),
            hjust = 0, color = "grey20", size = 3.5) +
  geom_text(x = 90, y = 120000, label = sprintf("Min-Max: %.0f-%.0f", min(trans$time_bw_trans, na.rm = TRUE),
                                                max(trans$time_bw_trans, na.rm = TRUE)),
            hjust = 0, color = "grey20", size = 3.5) +
  labs(x = "", y = "", title = "Time between transactions\n(for values < 99th percentile)") +
  scale_x_continuous(breaks = seq(0, 210, 30), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 150000),
                     breaks = seq(0, 150000, 50000), 
                     labels = c(0, paste0(seq(50,150,50), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

ggarrange(p1, p2, nrow = 1, ncol = 2)


# payment method 

plt_df <- trans_df %>%
  group_by(payment_method_id) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.0f%%", pct),
         lbl = ifelse(lbl == "0%", "<1%", lbl))

trans_df %>%
  ggplot() +
  aes(x = payment_method_id) +
  geom_bar(fill = "#2D559E") +
  geom_text(aes(x = payment_method_id, y = n - 250, label = lbl),
            plt_df %>% filter(pct > 4), angle = 90, hjust = 1, vjust = 0.5,
            color = "white", fontface = "bold") +
  labs(title = "Payment method", y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,100000,20000),
                     labels = c(0, paste0(seq(20,100,20), "K")),
                     limits = c(0, 100000)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_blank())


# payment_plan_days

plt_df <- trans_df %>%
  mutate(payment_plan_days = fct_infreq(factor(payment_plan_days))) %>%
  group_by(payment_plan_days) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.0f%%", pct)) %>% 
  arrange(-n)

trans_df %>%
  ggplot() +
  aes(x = fct_infreq(factor(payment_plan_days))) +
  geom_bar(fill = "#2D559E") +
  geom_text(aes(x = payment_plan_days, y = n - 250, label = lbl),
            plt_df %>% filter(pct > 5), angle = 90, hjust = 1, vjust = 0.5,
            color = "white", fontface = "bold") +
  geom_text(aes(x = payment_plan_days, y = n, label = lbl),
            plt_df %>% filter(pct > 3 & pct < 80), angle = 90, hjust = 0, vjust = 0.5,
            color = "grey20", fontface = "bold") +
  labs(title = "Payment plan days", y = "", x = "Unique values of payment plan days") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,175000,25000),
                     labels = c(0, paste0(seq(25,175,25), "K")),
                     limits = c(0, 175000)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_blank())

# plan_actual_price

ggplot(trans_df %>% mutate(is_cancel = factor(is_cancel, labels = c("is_cancel=0", "is_cancel=1")))) +
  aes(x = plan_list_price, y = actual_amount_paid) +
  geom_point(aes(color = is_cancel), shape = "circle", size = 2, alpha = 0.5) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "Plan list price", y = "Actual amount paid") +
  facet_rep_wrap(~ is_cancel, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none")


# churn

trans_df <- trans_df %>%
  mutate(churn = as.numeric(time_bw_exp_nexttrans > 30))

subscr_periods <- function(churn_seq) {
  cumsum(ifelse(is.na(lag(churn_seq)), 1, lag(churn_seq) == 1))
}

# sequences of churn indicators by msno
trans_periods <- trans_df %>%
  group_by(msno) %>% 
  group_modify(~ subscr_periods(.x$churn) %>%
                 tibble::enframe(name = "trans_id",
                                 value = "msno_subscr_id"))

# joining each transaction with id for sequence of churn indicators

trans_df <- trans_df %>%
  left_join(trans_periods, by = c("msno", "trans_id"))

trans_df <- trans_df %>%
  mutate(subscr_id = group_indices(trans_df, msno, msno_subscr_id)) 

mode <- function(x){
  if (sum(is.na(x)) == length(x)) {
    NA
  } else {
    which.max(tabulate(x))
  }
}

# combining transactions in one sequence into periods
trans_periods <- trans_df %>%
  group_by(msno) %>%
  mutate(time_from_first_trans = as.numeric(transaction_date - transaction_date[1], 'days'),
         time_from_prev_trans = replace_na(as.numeric(transaction_date - lag(transaction_date), 'days'), 0),
         time_from_prev_exp = replace_na(as.numeric(transaction_date - lag(membership_expire_date_cor), 'days'), 0)) %>%
  ungroup() %>%
  group_by(subscr_id) %>%
  summarise(msno = unique(msno),
            msno_subscr_id = unique(msno_subscr_id),
            churn = max(churn),
            start_date = transaction_date[1],
            end_date = pmin(as.Date("2017-03-31"), membership_expire_date_cor[n()]),
            n_trans = n(),
            n_cancel = sum(is_cancel),
            num_auto_renew = sum(is_auto_renew),
            first_payment_method_id = payment_method_id[1],
            first_actual_amount_paid = actual_amount_paid[1],
            first_plan_list_price = plan_list_price[1],
            mode_payment_method_id = mode(as.numeric(as.character(payment_method_id))),
            sum_actual_amount_paid = sum(actual_amount_paid),
            churned_before = as.numeric(msno_subscr_id > 1),
            churned_before_num = msno_subscr_id - 1,
            time_from_first_trans = time_from_first_trans[1],
            time_from_prev_trans = time_from_prev_trans[1],
            time_from_prev_exp = time_from_prev_exp[1]) %>%
  ungroup() %>%
  mutate(time = as.numeric(end_date - start_date, 'days') + 1)

trans_periods <- trans_periods %>%
  mutate(mode_payment_method_id = fct_lump_min(factor(mode_payment_method_id, as.numeric(levels(trans_df$payment_method_id))),
                                               min = 30, other_level = "999"),
         first_payment_method_id = fct_lump_min(factor(first_payment_method_id, as.numeric(levels(trans_df$payment_method_id))),
                                                min = 30, other_level = "999"))

members <- members %>%
  left_join(trans_df %>%
              group_by(msno) %>%
              summarise(transaction_date_first = min(transaction_date),
                        transaction_date_last = max(transaction_date),
                        membership_expire_date_last = min(membership_expire_date_cor[n()], as.Date("2017-03-31")),
                        churned_num = sum(churn == 1),
                        churned_last = as.numeric(churn[n()] == 1),
                        churned_ever = as.numeric(churned_num > 0),
                        time_obs = as.numeric(membership_expire_date_last - transaction_date_first, 'days'),
                        n_trans_tot = n()),
            by = "msno") %>%
  left_join(trans_periods %>% group_by(msno) %>% summarise(n_subscr = n()) %>% ungroup(),
            by = "msno") %>%
  mutate(time_tot = as.numeric(membership_expire_date_last - registration_init_time, 'days') + 1)

# number of churns

plt_df <- members %>%
  group_by(churned_num) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.0f%%", pct),
         lbl = ifelse(lbl == "0%", "<1%", lbl))

p1 <- members %>%
  ggplot() +
  aes(x = churned_num) +
  geom_bar(fill = "#2D559E") +
  geom_text(aes(x = churned_num, y = n - 500, label = lbl), 
            plt_df %>% filter(pct > 2.5), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = churned_num, y = n + 500, label = lbl), 
            plt_df %>% filter(pct < 2.5), 
            color = "grey20", fontface = "bold") +
  labs(title = "Number of churns by one user", y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,10000,2000),
                     labels = c(0, paste0(seq(2,10,2), "K")),
                     limits = c(0, 10000)) +
  scale_x_continuous(expand = c(0,0), breaks = plt_df$churned_num) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

plt_df <- members %>%
  group_by(n_subscr) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.0f%%", pct),
         lbl = ifelse(lbl == "0%", "<1%", lbl))

p2 <- members %>%
  ggplot() +
  aes(x = n_subscr) +
  geom_bar(fill = "#2D559E") +
  geom_text(aes(x = n_subscr, y = n - 750, label = lbl), 
            plt_df %>% filter(pct > 3.5), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = n_subscr, y = n + 750, label = lbl), 
            plt_df %>% filter(pct <= 3.5), 
            color = "grey20", fontface = "bold") +
  labs(title = "Number of subscription periods by user", y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,15000,2500),
                     labels = c(0, paste0(seq(2.5,15,2.5), "K")),
                     limits = c(0, 15000)) +
  scale_x_continuous(expand = c(0,0), breaks = plt_df$n_subscr) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

ggarrange(p2, p1, nrow = 1, ncol = 2)


#### Descriptive survival analysis ####

# all

srvfit <- survfit(Surv(time, churn) ~ 1, trans_periods)
med_surv <- surv_median(srvfit)
srvhaz <- bshazard(Surv(time, churn) ~ 1, trans_periods, verbose = FALSE)

p1 <- ggsurvplot(srvfit, size = 1.2, palette = "#006666", conf.int = FALSE,
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership (= survival probability)",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "none", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  geom_text(x = 800, y = 0.9, 
            label = sprintf("Median membership time\n(= median survival time):\n%.d days (%.0f months)", med_surv$median, med_surv$median/30.4375),
            hjust = 1, vjust = 1) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p2 <- ggplot() +
  geom_line(aes(x = srvhaz$time, y = srvhaz$hazard), color = "#D82632", size = 1.2) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2), limits = c(0,820)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.0125)) +
  labs(x = "Time from the first transaction, months", y = "Hazard rate", 
       title = "Churn hazard rate") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

ggarrange(p1, p2, nrow = 2, ncol = 1)


# by number of churns

trans_periods <- trans_periods %>%
  mutate(churned_before_numG = factor(churned_before_num, 0:6,
                                      labels = c(0:3, "4-6", "4-6", "4-6")))

srvfit <- survfit(Surv(time, churn) ~ churned_before_numG, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_d("ggsci::light_blue_material")[seq(2,10,2)], 
                 size = 1.2, conf.int = FALSE, 
                 legend.labs = levels(trans_periods$churned_before_numG),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\nnumber of previous churns",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = c(0.9,0.8),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

plt_df <- NULL

for (lvl in levels(trans_periods$churned_before_numG)) {
  srvhaz <- bshazard(Surv(time, churn) ~ 1, 
                     trans_periods %>% filter(churned_before_numG == lvl), 
                     verbose = FALSE)
  plt_df <- rbind(plt_df,
                  tibble(x = srvhaz$time,
                         y = srvhaz$hazard,
                         lvl = lvl))
}

p2 <- ggplot() +
  geom_line(aes(x = x, y = y, color = lvl), plt_df, size = 1.2) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2), limits = c(0,820)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.0125)) +
  scale_color_manual(values = paletteer_d("ggsci::light_blue_material")[seq(2,10,2)]) +
  labs(x = "Time from the first transaction, months", y = "Hazard rate", 
       title = "Churn hazard rate by\nnumber of previous churns", color = "") +
  theme_classic() +
  theme(legend.position = c(0.9,0.8),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

ggarrange(p1, p2, nrow = 2, ncol = 1)

# year snd month of subscription 

trans_periods <- trans_periods %>%
  mutate(year_period = factor(year(start_date)),
         month_period = factor(month(start_date), labels = month.abb))

plt_df <- trans_periods %>%
  group_by(year_period) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.1f%%", pct))

trans_periods %>%
  ggplot() +
  aes(x = year_period) +
  geom_bar(aes(fill = year_period)) +
  geom_text(aes(x = year_period, y = n - 500, label = lbl), 
            plt_df %>% filter(year_period != "2015"), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = year_period, y = n - 500, label = lbl), 
            plt_df %>% filter(year_period == "2015"), 
            color = "grey20", fontface = "bold") +
  labs(title = "Year of subscription", y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 14000, 2000),
                     labels = c(0, paste0(seq(2,14,2), "K")),
                     limits = c(0, 14000)) +
  scale_fill_manual(values = paletteer_c("ggthemes::Blue", 3)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

srvfit <- survfit(Surv(time, churn) ~ year_period, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_c("ggthemes::Blue", 3), 
                 size = 1.2, conf.int = FALSE,
                 legend.labs = levels(trans_periods$year_period),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\nyear of subscription",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))


plt_df <- trans_periods %>%
  group_by(month_period) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.1f%%", pct))

trans_periods %>%
  ggplot() +
  aes(x = month_period) +
  geom_bar(aes(fill = month_period)) +
  geom_text(aes(x = month_period, y = n - 250, label = lbl), 
            plt_df, 
            color = "white", fontface = "bold") +
  labs(title = "Month of membership start", y = "", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6000, 2000),
                     labels = c(0, paste0(seq(2,6,2), "K")),
                     limits = c(0, 6350)) +
  scale_fill_manual(values = paletteer_c("grDevices::Set 2", 12)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

srvfit <- survfit(Surv(time, churn) ~ month_period, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_c("grDevices::Set 2", 12), 
                 size = 1.2, conf.int = FALSE,
                 legend.labs = levels(trans_periods$month_period),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\nmonth of subscription",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  guides(color = guide_legend(nrow = 6)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


#### Members' characteristics ####

# city

plt_df <- members %>%
  group_by(city) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.0f%%", pct),
         lbl = ifelse(lbl == "0%", "<1%", lbl))

members %>%
  ggplot() +
  aes(x = city) +
  geom_bar(aes(fill = city)) +
  geom_text(aes(x = city, y = n - 250, label = lbl), 
            plt_df %>% filter(pct > 2), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = city, y = n + 300, label = lbl), 
            plt_df %>% filter(pct < 2), 
            color = "grey20", fontface = "bold") +
  labs(title = "City", y = "Number of unique users", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,10000,2000),
                     labels = c(0, paste0(seq(2,10,2), "K"))) +
  scale_fill_manual(values = c(paletteer_d("ggthemes::Tableau_10")[1:8],
                               rep("grey50", length(levels(members$city))-8))) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

trans_periods <- trans_periods %>%
  left_join(members %>% select(msno, city, bd, gender, registered_via, registration_init_time,
                               year_reg, month_reg), by = "msno")

trans_periods <- trans_periods %>%
  mutate(time_from_reg = as.numeric(start_date - registration_init_time, 'days'),
         cityG = fct_lump_n(city, 8, other_level = "Other"))

srvfit <- survfit(Surv(time, churn) ~ cityG, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_d("ggthemes::Tableau_10")[1:length(levels(trans_periods$cityG))], 
                 size = 1.2, conf.int = FALSE, 
                 legend.labs = levels(trans_periods$cityG),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by user's city",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = "right",
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

# age

members <- members %>%
  mutate(bd_group = cut(bd, c(min(members$bd)-1, 10, 61, max(members$bd)+1), 
                        c("< 10", "10-60", "> 60"), right = FALSE))

var_label(members) <- list(bd = "Age", bd_group = "Age group")

tbl_summary(
  as.data.frame(select(members, bd, bd_group)),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 2), rep(0, 5)))) %>%
  bold_labels()


members %>%
  filter(bd_group == "10-60") %>%
  ggplot() +
  aes(x = bd) +
  geom_histogram(binwidth = 2, fill = "#4682B4") +
  labs(x = "Age", y = "Number of unique users", title = "Age (for age in [10, 60])") +
  scale_x_continuous(breaks = seq(10,70,10)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1000,250),
                     limits = c(0, 1000)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

trans_periods <- trans_periods %>%
  mutate(bdG = cut(bd, c(min(members$bd)-1, 10, max(members$bd)+1), 
                   c("< 10", ">= 10"), right = FALSE))

srvfit <- survfit(Surv(time, churn) ~ bdG, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_d("ggthemes::Jewel_Bright")[1:length(levels(trans_periods$cityG))], 
                 size = 1.2, conf.int = FALSE, 
                 legend.labs = levels(trans_periods$bdG),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by user's age",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


# gender

plt_df <- members %>%
  group_by(gender) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.1f%%", pct))

ggplot(members) +
  aes(x = gender, fill = gender) +
  geom_bar(width = 0.6) +
  geom_text(aes(x = gender, y = n - 500, label = lbl), plt_df, 
            color = "white", fontface = "bold") +
  scale_fill_manual(values = paletteer_d("ggthemes::Superfishel_Stone")[c(3,1,5)]) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,10000,2000),
                     limits = c(0, 10000), labels = c(0, paste0(seq(2,10,2), "K"))) +
  ggthemes::theme_tufte() +
  labs(x = "", y = "Number of unique users", title = "Gender") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

srvfit <- survfit(Surv(time, churn) ~ gender, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_d("ggthemes::Superfishel_Stone")[c(3,1,5)], 
                 size = 1.2, conf.int = FALSE,
                 legend.labs = levels(trans_periods$gender),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by user's gender",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


# registration method

plt_df <- members %>%
  group_by(registered_via) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.1f%%", pct))

members %>%
  ggplot() +
  aes(x = registered_via) +
  geom_bar(aes(fill = registered_via)) +
  geom_text(aes(x = registered_via, y = n - 200, label = lbl), 
            plt_df %>% filter(pct > 1), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = registered_via, y = n + 200, label = lbl), 
            plt_df %>% filter(pct < 1), 
            color = "grey20", fontface = "bold") +
  labs(title = "Registration method", y = "Number of unique users", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6000, 1000),
                     labels = c(0, paste0(seq(1,6,1), "K"))) +
  scale_fill_manual(values = paletteer_d("ggthemes::Tableau_10")[1:5]) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

srvfit <- survfit(Surv(time, churn) ~ registered_via, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_d("ggthemes::Tableau_10")[1:5], 
                 size = 1.2, conf.int = FALSE, 
                 legend.labs = levels(trans_periods$registered_via),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\nregistration method",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


# registration date

reg_days <- members %>%
  mutate(registration_init_time = floor_date(registration_init_time, "month")) %>%
  group_by(registration_init_time) %>%
  tally()

ggplot(reg_days) +
  aes(x = registration_init_time, y = n) +
  geom_line(size = 0.5, colour = "#112446") +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  # scale_y_continuous(limits = c(3000, 6000),
  #                    labels = c(paste0(seq(3,6,1), "K"))) +
  labs(title = "Number of registrations by month", x = "", y = "") +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5))


plt_df <- members %>%
  group_by(year_reg) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.1f%%", pct))

members %>%
  ggplot() +
  aes(x = year_reg) +
  geom_bar(aes(fill = year_reg)) +
  geom_text(aes(x = year_reg, y = n - 200, label = lbl), 
            plt_df %>% filter(pct > 1.9), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = year_reg, y = n + 200, label = lbl), 
            plt_df %>% filter(pct < 1.9), 
            color = "grey20", fontface = "bold") +
  labs(title = "Year of registration at the service", y = "Number of unique users", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6000, 1000),
                     labels = c(0, paste0(seq(1,6,1), "K"))) +
  scale_fill_manual(values = paletteer_c("ggthemes::Blue", 14)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())


trans_periods <- trans_periods %>%
  mutate(year_regG = fct_collapse(year_reg, 
                                  "2004-2009" = as.character(2004:2009),
                                  "2010-2011" = as.character(2010:2011),
                                  "2012-2013" = as.character(2012:2013)))

srvfit <- survfit(Surv(time, churn) ~ year_regG, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_c("ggthemes::Blue", length(levels(trans_periods$year_regG))), 
                 size = 1.2, conf.int = FALSE,
                 legend.labs = levels(trans_periods$year_regG),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\nuser's year of registration",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


# time_from_registration

trans_periods <- trans_periods %>%
  mutate(time_from_regG = cut(time_from_reg, c(0,0.1, 31, 366, 366*3, 365*5, 366*15),
                              c("0", "1-30 days", "31 days-1year", "1-3 years", "3-5 years", ">5 years"),
                              right = FALSE))

var_label(trans_periods) <- list(time_from_reg = "Time from user's registration at the service to subscription start, days",
                                 time_from_regG = "Time from user's registration at the service to subscription start")

tbl_summary(
  as.data.frame(select(trans_periods, time_from_reg, time_from_regG)),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 2), rep(0, 5)))) %>%
  bold_labels()

srvfit <- survfit(Surv(time, churn) ~ time_from_regG, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_c("ggthemes::Brown", length(levels(trans_periods$time_from_regG))), 
                 size = 1.2, conf.int = FALSE,
                 legend.labs = levels(trans_periods$time_from_regG),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\ntime from registration",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  guides(color = guide_legend(nrow = 6)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.85),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


# registration month

plt_df <- members %>%
  group_by(month_reg) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)*100,
         lbl = sprintf("%.1f%%", pct))

members %>%
  ggplot() +
  aes(x = month_reg) +
  geom_bar(aes(fill = month_reg)) +
  geom_text(aes(x = month_reg, y = n - 100, label = lbl), 
            plt_df %>% filter(pct > 1.9), 
            color = "white", fontface = "bold") +
  geom_text(aes(x = month_reg, y = n + 200, label = lbl), 
            plt_df %>% filter(pct < 1.9), 
            color = "grey20", fontface = "bold") +
  labs(title = "Month of registration at the service", y = "Number of unique users", x = "") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 6000, 500),
                     limits = c(0, 2000)) +
  scale_fill_manual(values = paletteer_c("grDevices::Set 2", 12)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank())

srvfit <- survfit(Surv(time, churn) ~ month_reg, trans_periods)

p1 <- ggsurvplot(srvfit, 
                 palette = paletteer_c("grDevices::Set 2", 12), 
                 size = 1.2, conf.int = FALSE,
                 legend.labs = levels(trans_periods$month_reg),
                 legend.title = "",
                 ggtheme = theme_minimal() + theme(plot.title = element_text(face = "bold")),
                 title = "Probability of active membership by\nuser's month of registration",
                 xlab = "Time from the first transaction, months",
                 ylab = "Probability of active membership",
                 legend = "bottom", censor = FALSE)

p1 <- p1$plot +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 820, 30.4375*2),
                     labels = seq(0, 820/30.4375, 2),
                     limits = c(0, 820)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  guides(color = guide_legend(nrow = 6)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))

p1


#### User logs ####

ulogs_days <- ulogs %>%
  group_by(date) %>%
  tally()

var_label(ulogs_days) <- list(n = "Number of records per day in user logs")

tbl_summary(
  as.data.frame(select(ulogs_days, n)),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 2), rep(0, 5)))) %>%
  bold_labels()

# logs per day

ggplot(ulogs_days) +
  aes(x = date, y = n) +
  geom_line(size = 0.5, colour = "#112446") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m", 
               limits = as.Date(c('2015-01-01','2017-03-31'))) +
  scale_y_continuous(limits = c(3000, 6000),
                     labels = c(paste0(seq(3,6,1), "K"))) +
  labs(title = "Number of records per day in user logs", x = "", y = "") +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5))

# merge logs with subscription periods 

trans_periods_wide <- trans_periods %>%
  select(msno, msno_subscr_id, start_date, end_date) %>%
  as.data.frame() %>%
  reshape(v.names = c("start_date", "end_date"), timevar = "msno_subscr_id", 
          idvar = "msno", direction = "wide")

ulogs <- ulogs %>%
  left_join(trans_periods_wide, by = "msno")

ulogs <- ulogs %>%
  mutate(msno_subscr_period = case_when(date < start_date.1 ~ 0,
                                        date <= end_date.1 ~ 1,
                                        date < start_date.2 ~ 1.5,
                                        date <= end_date.2 ~ 2,
                                        date < start_date.3 ~ 2.5,
                                        date <= end_date.3 ~ 3,
                                        date < start_date.4 ~ 3.5,
                                        date <= end_date.4 ~ 4,
                                        date < start_date.5 ~ 4.5,
                                        date <= end_date.5 ~ 5,
                                        date < start_date.6 ~ 5.5,
                                        date <= end_date.6 ~ 6,
                                        date < start_date.7 ~ 6.5,
                                        date <= end_date.7 ~ 7,
                                        TRUE ~ 7.5))

ulogs <- ulogs %>%
  mutate(in_subscr = msno_subscr_period %in% 1:7)

ulogs_periods <- ulogs %>%
  filter(in_subscr) %>%
  rename(msno_subscr_id = msno_subscr_period) %>%
  left_join(trans_periods %>% select(msno, msno_subscr_id, subscr_id, start_date, end_date, churn, time,
                                     city, cityG, bd, bdG, gender, registered_via,
                                     registration_init_time, year_reg, year_regG, month_reg,
                                     churned_before, churned_before_num), 
            by = c("msno", "msno_subscr_id"))

trans_periods <- trans_periods %>%
  mutate(subscr_got_logs = subscr_id %in% ulogs_periods$subscr_id)

members <- members %>%
  left_join(trans_periods %>% 
              select(msno, subscr_got_logs) %>%
              group_by(msno) %>%
              summarise(n_subscr_logs = sum(subscr_got_logs),
                        n_subscr_nologs = sum(!subscr_got_logs)) %>%
              ungroup(),
            by = "msno") %>%
  mutate(all_subscr_got_logs = n_subscr_logs == n_subscr)

# records per id

ulogs_stat <- ulogs %>%
  group_by(msno) %>%
  summarise(n = sum(in_subscr))

var_label(ulogs_stat) <- list(n = "Number of records in user logs per ID")

tbl_summary(
  as.data.frame(select(ulogs_stat, n)),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 2), rep(0, 5)))) %>%
  bold_labels()

# breaks bw logs

ulogs_breaks <- ulogs_periods %>%
  filter(!is.na(time_from_prev_date)) %>%
  group_by(msno) %>%
  summarise(mean_break = mean(time_from_prev_date),
            median_break = median(time_from_prev_date)) %>%
  ungroup()

var_label(ulogs_breaks) <- list(mean_break = "Mean break between logs for one ID, days",
                                median_break = "Median break between logs for one ID, days")

tbl_summary(
  as.data.frame(select(ulogs_breaks, mean_break, median_break)),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 2), rep(0, 5)))) %>%
  bold_labels()

# sampled users activity

msno_sample_act <- members %>%
  select(msno, n_subscr, n_subscr_logs) %>%
  group_by(n_subscr, n_subscr_logs) %>%
  filter(row_number() == 1)

msno_sample_act <- trans_periods %>%
  select(msno, subscr_id, start_date, end_date) %>%
  filter(msno %in% msno_sample_act$msno) %>%
  left_join(ulogs_periods %>% select(subscr_id, date, num_tot, churn), by = "subscr_id") %>%
  mutate(msno_id = factor(msno, unique(msno_sample_act$msno), 1:length(unique(msno_sample_act$msno))))

ggplot() +
  geom_segment(aes(x = date, xend = date + 1, y = msno_id, yend = msno_id, color = num_tot), 
               msno_sample_act %>% filter(!is.na(num_tot)), size = 8) +
  scale_color_gradient(low = brewer.pal(9, "Reds")[1], high = brewer.pal(9, "Reds")[9],
                       breaks = seq(0, 250, 50), 
                       labels = seq(0, 250, 50)) +
  geom_point(aes(x = start_date, y = msno_id, shape = "Start of subscr."), msno_sample_act,
             color = "#35978F", size = 1) +
  geom_point(aes(x = end_date, y = msno_id, shape = "End of subscr."), msno_sample_act,
             color = "#FDAE61", size = 1) +
  scale_shape_manual(values = c(19, 15), breaks = c("Start of subscr.", "End of subscr.")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%Y-%m",
               expand = c(0.01,0)) +
  labs(x = "", y = "User", color = "Tot.num.of songs", title = "Sampled users' daily activity", shape = "") +
  guides(shape = guide_legend(override.aes = list(color = c("#35978F", "#FDAE61"),
                                                  size = 3))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5))


# number of songs

p1 <- ulogs_periods %>%
  filter(num_25 < quantile(ulogs_periods$num_25, 0.99)) %>%
  ggplot() +
  aes(x = num_25) +
  geom_histogram(binwidth = 2, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_25, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 1500000, 300000),
                     labels = c(0, paste0(seq(300,1500,300), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p2 <- ulogs_periods %>%
  filter(num_50 < quantile(ulogs_periods$num_50, 0.99)) %>%
  ggplot() +
  aes(x = num_50) +
  geom_histogram(binwidth = 1, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_50, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 16, 1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1600000,400000),
                     labels = c(0, paste0(seq(400,1600,400), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p2a <- ulogs_periods %>%
  filter(num_75 < quantile(ulogs_periods$num_75, 0.99)) %>%
  ggplot() +
  aes(x = num_75) +
  geom_histogram(binwidth = 1, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_75, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 16, 1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,2000000,500000),
                     labels = c(0, paste0(seq(500,2000,500), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p3 <- 
  ulogs_periods %>%
  filter(num_985 < quantile(ulogs_periods$num_985, 0.99)) %>%
  ggplot() +
  aes(x = num_985) +
  geom_histogram(binwidth = 1, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_985, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 16, 1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,2000000,500000),
                     labels = c(0, paste0(seq(500,2000,500), "K")),
                     limits = c(0, 2000000)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p4 <- ulogs_periods %>%
  filter(num_100 < quantile(ulogs_periods$num_100, 0.99)) %>%
  ggplot() +
  aes(x = num_100) +
  geom_histogram(binwidth = 5, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_100, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 180, 20)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1400000,300000),
                     labels = c(0, paste0(seq(300,1400,300), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p4a <- ulogs_periods %>%
  ggplot() +
  aes(x = av_pct_played) +
  geom_histogram(binwidth = 1, fill = "#4682B4") +
  labs(x = "", y = "", title = var_label(ulogs_periods)$av_pct_played) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,600000,200000),
                     labels = c(0, paste0(seq(200,600,200), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p5 <- ulogs_periods %>%
  filter(num_tot < quantile(ulogs_periods$num_tot, 0.99)) %>%
  ggplot() +
  aes(x = num_tot) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_tot, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0,200,20)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,500000,100000),
                     labels = c(0, paste0(seq(100,500,100), "K")),
                     limits = c(0, 500000)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p6 <- ulogs_periods %>%
  filter(num_unq < quantile(ulogs_periods$num_unq, 0.99)) %>%
  ggplot() +
  aes(x = num_unq) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_unq, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0,160,20)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1000000,200000),
                     labels = c(0, paste0(seq(200,1000,200), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p7 <- ulogs_periods %>%
  filter(num_repeats < quantile(ulogs_periods$num_repeats, 0.99)) %>%
  ggplot() +
  aes(x = num_repeats) +
  geom_histogram(binwidth = 5, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$num_repeats, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0,120,20)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,2000000,500000),
                     labels = c(0, paste0(seq(500,2000,500), "K")),
                     limits = c(0, 2000000)) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p8 <- ulogs_periods %>%
  filter(av_times_played < quantile(ulogs_periods$av_times_played, 0.99)) %>%
  ggplot() +
  aes(x = av_times_played) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "#4682B4") +
  labs(x = "", y = "", title = paste0(var_label(ulogs_periods)$av_times_played, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(1,13,1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,3000000,1000000),
                     labels = c(0, paste0(seq(1,3,1), "KK"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

ggarrange(p1, p2, p2a, p3, p4, p4a, p5, p6, p7, p8, ncol = 2, nrow = 5)


# share of song's length listened

ggplot(ulogs_periods) + 
  stat_ecdf(aes(x = sh_100, color = "98.5-100%"), size = 1) + 
  stat_ecdf(aes(x = sh_985, color = "75-98.5%"), size = 1) + 
  stat_ecdf(aes(x = sh_75, color = "50-75%"), size = 1) + 
  stat_ecdf(aes(x = sh_50, color = "25-50%"), size = 1) + 
  stat_ecdf(aes(x = sh_25, color = "0-25%"), size = 1) +
  scale_color_viridis_d(end = 0.9, direction = -1) + 
  scale_x_continuous(expand = c(0.01,0),
                     breaks = seq(0, 1, 0.2),
                     labels = c(0, paste0("<= ", seq(0.2,1,0.2))),
                     sec.axis = sec_axis(trans = ~ .,
                                         name = "For 1-CDF: share of this length songs out of the total number of songs played",
                                         breaks = seq(0, 1, 0.2),
                                         labels = c(paste0(">= ", seq(0,0.8,0.2)), 1))) +
  scale_y_continuous(expand = c(0.01, 0), 
                     breaks = seq(0, 1, 0.2),
                     labels = scales::percent_format(accuracy = 1),
                     sec.axis = sec_axis(trans = ~ 1 - .,
                                         name = "1-CDF",
                                         breaks = seq(0, 1, 0.2),
                                         labels = scales::percent_format(accuracy = 1))) +
  labs(x = "For CDF: share of this length songs out of the total number of songs played",
       y = "CDF", color = "% of song length played",
       title = "Cumulative distribution function for shares of\nthe number of songs played by length") +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "grey90", size = 0.2),
        panel.grid.minor = element_line(color = "grey90", size = 0.2))


# average % of length played

ggplot(ulogs_periods, aes(y = av_pct_played)) +
  ggdist::stat_halfeye(adjust = 0.4, justification = -0.07, .width = 0, 
                       point_colour = NA, alpha = 0.8,
                       fill = "#4682B4") +
  geom_boxplot(width = 0.1, alpha = 0.5, outlier.colour = NA,
               show.legend = FALSE, fill = "#4682B4", color = "#4682B4") +
  coord_flip() +
  scale_y_continuous(breaks = seq(10,100,10),
                     expand = c(0.01,0),
                     limits = c(10, 100)) +
  scale_x_continuous(breaks = seq(0.06, 1.06, 0.2),
                     labels = seq(0,1,0.2),
                     expand = c(0.01, 0),
                     limits = c(-0.1, 1.1)) +
  labs(y = var_label(ulogs_periods)$av_pct_played, x = "Density",
       title = "Average % of one song's length played") +
  ggthemes::theme_tufte()


# total_secs imputing

ulogs_periods <- ulogs_periods %>%
  mutate(total_secs_na = case_when(total_secs < 0 | total_secs > 60*60*24 ~ NA_real_, 
                                   TRUE ~ total_secs),
         total_hrs_na = total_secs_na/3600,
         av_secs_real_na = total_secs / (num_25/0.125 + num_50/0.375 + num_75/0.625 + num_985/0.8675 + num_100/0.9925))

ulogs_periods <- ulogs_periods %>%
  mutate(total_secs_imp = ifelse(is.na(total_secs_na),
                                 min(round(median(ulogs_periods$av_secs_real_na, na.rm = TRUE),0)*av_pct_played/100,86400),
                                 total_secs_na),
         total_hrs_imp = total_secs_imp/3600,
         av_secs_real = total_secs_imp / (num_25/0.125 + num_50/0.375 + num_75/0.625 + num_985/0.8675 + num_100/0.9925),
         av_secs_played = total_secs_imp / num_tot,
         av_mins_real = av_secs_real/60,
         av_mins_played = av_secs_played/60)

var_label(ulogs_periods) <- list(total_secs_na = "Total seconds played",
                                 total_secs_imp = "Total seconds played (imputed)",
                                 total_hrs_na = "Total hours played",
                                 total_hrs_imp = "Total hours played (imputed)",
                                 av_secs_real = "Average real length of a song, seconds",
                                 av_secs_played = "Average time spent on listening to one song, seconds",
                                 av_mins_real = "Average real length of a song, minutes",
                                 av_mins_played = "Average time spent on listening to one song, minutes")

tbl_summary(
  as.data.frame(select(ulogs_periods, av_mins_played, total_hrs_na, total_hrs_imp)),
  type = all_continuous() ~ "continuous2",
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 7)))) %>%
  bold_labels()

p1 <- ulogs_periods %>%
  filter(av_mins_played < quantile(ulogs_periods$av_mins_played, 0.99, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = av_mins_played) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "#4682B4") +
  labs(x = "Minutes", y = "", title = paste0(var_label(ulogs_periods)$av_mins_played, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1500000,500000), 
                     labels = c(0, paste0(seq(500,1500,500), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

p2 <- ulogs_periods %>%
  filter(total_hrs_imp < quantile(ulogs_periods$total_hrs_imp, 0.99, na.rm = TRUE)) %>%
  ggplot() +
  aes(x = total_hrs_imp) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "#4682B4") +
  labs(x = "Hours", y = "", title = paste0(var_label(ulogs_periods)$total_hrs_imp, "\n(observations with values less than 99th percentile)")) +
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1500000,500000), 
                     labels = c(0, paste0(seq(500,1500,500), "K"))) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12))

ggarrange(p1,p2, nrow = 1, ncol = 2)


p1 <- dbplot_raster(ulogs_periods,
                    x = num_tot, y = total_hrs_imp) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,24,3), limits = c(0,24)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,6000,1000)) +
  scale_fill_viridis_c() +
  labs(x = "Total number of songs played", y = "Total hours played",
       title = "Total hours played vs. total numer of songs",
       fill = "Number of obs.") +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        legend.key.width = unit(1,"cm"))

p1


# activity during subscription

ulogs_last <- ulogs_periods %>%
  group_by(subscr_id) %>%
  summarise(days_active = n(),
            mean_num_unq = sum(num_unq),
            mean_num_tot = sum(num_tot),
            mean_av_pct_played = sum(av_pct_played),
            mean_total_secs_imp = sum(total_secs_imp),
            mean_total_hrs_imp = sum(total_hrs_imp),
            mean_av_secs_played = sum(av_secs_played),
            mean_av_mins_played = sum(av_mins_played),
            mean_sh_25 = sum(sh_25),
            mean_sh_985 = sum(sh_985),
            mean_sh_100 = sum(sh_100),
            last30d = as.numeric(end_date - date, 'days') < 30,
            last14d = as.numeric(end_date - date, 'days') < 14,
            last7d = as.numeric(end_date - date, 'days') < 7,
            days_active30 = sum(last30d),
            mean_num_unq30 = sum(num_unq[last30d]),
            mean_num_tot30 = sum(num_tot[last30d]),
            mean_av_pct_played30 = sum(av_pct_played[last30d]),
            mean_total_secs_imp30 = sum(total_secs_imp[last30d]),
            mean_total_hrs_imp30 = sum(total_hrs_imp[last30d]),
            mean_av_secs_played30 = sum(av_secs_played[last30d]),
            mean_av_mins_played30 = sum(av_mins_played[last30d]),
            mean_sh_2530 = sum(sh_25[last30d]),
            mean_sh_98530 = sum(sh_985[last30d]),
            mean_sh_10030 = sum(sh_100[last30d]),
            days_active14 = sum(last14d),
            mean_num_unq14 = sum(num_unq[last14d]),
            mean_num_tot14 = sum(num_tot[last14d]),
            mean_av_pct_played14 = sum(av_pct_played[last14d]),
            mean_total_secs_imp14 = sum(total_secs_imp[last14d]),
            mean_total_hrs_imp14 = sum(total_hrs_imp[last14d]),
            mean_av_secs_played14 = sum(av_secs_played[last14d]),
            mean_av_mins_played14 = sum(av_mins_played[last14d]),
            mean_sh_2514 = sum(sh_25[last14d]),
            mean_sh_98514 = sum(sh_985[last14d]),
            mean_sh_10014 = sum(sh_100[last14d]),
            days_active7 = sum(last7d),
            mean_num_unq7 = sum(num_unq[last7d]),
            mean_num_tot7 = sum(num_tot[last7d]),
            mean_av_pct_played7 = sum(av_pct_played[last7d]),
            mean_total_secs_imp7 = sum(total_secs_imp[last7d]),
            mean_total_hrs_imp7 = sum(total_hrs_imp[last7d]),
            mean_av_secs_played7 = sum(av_secs_played[last7d]),
            mean_av_mins_played7 = sum(av_mins_played[last7d]),
            mean_sh_257 = sum(sh_25[last7d]),
            mean_sh_9857 = sum(sh_985[last7d]),
            mean_sh_1007 = sum(sh_100[last7d])) %>%
  select(-last30d, -last14d, -last7d) %>%
  distinct()

trans_periods <- trans_periods %>%
  left_join(ulogs_last, by = "subscr_id") 

trans_periods <- trans_periods %>%
  mutate(days_active = replace_na(days_active, 0),
         pct_active = replace_na(days_active / time * 100, 0),
         tot_num_tot = replace_na(mean_num_tot,0),
         tot_total_secs_imp = replace_na(mean_total_secs_imp,0),
         mean_sh_25_act = replace_na(mean_sh_25/days_active, 0),
         mean_sh_25 = replace_na(mean_sh_25/time, 0),
         mean_sh_985_act = replace_na(mean_sh_985/days_active, 0),
         mean_sh_985 = replace_na(mean_sh_985/time, 0),
         mean_sh_100_act = replace_na(mean_sh_100/days_active, 0),
         mean_sh_100 = replace_na(mean_sh_100/time, 0),
         mean_num_unq_act = replace_na(mean_num_unq/days_active, 0),
         mean_num_unq = replace_na(mean_num_unq/time, 0),
         mean_num_tot_act = replace_na(mean_num_tot/days_active, 0),
         mean_num_tot = replace_na(mean_num_tot/time, 0),
         mean_av_pct_played_act = replace_na(mean_av_pct_played/days_active, 0),
         mean_av_pct_played = replace_na(mean_av_pct_played/time, 0),
         mean_total_secs_imp_act = replace_na(mean_total_secs_imp/days_active, 0),
         mean_total_secs_imp = replace_na(mean_total_secs_imp/time, 0),
         mean_total_hrs_imp_act = replace_na(mean_total_hrs_imp/days_active, 0),
         mean_total_hrs_imp = replace_na(mean_total_hrs_imp/time, 0),
         mean_av_secs_played_act = replace_na(mean_av_secs_played/days_active, 0),
         mean_av_secs_played = replace_na(mean_av_secs_played/time, 0),
         mean_av_mins_played_act = replace_na(mean_av_mins_played/days_active, 0),
         mean_av_mins_played = replace_na(mean_av_mins_played/time, 0),
         days_active30 = replace_na(days_active30, 0),
         pct_active30 = replace_na(days_active30 / 30 * 100, 0),
         tot_num_tot30 = replace_na(mean_num_tot30,0),
         tot_total_secs_imp30 = replace_na(mean_total_secs_imp30, 0),
         mean_sh_25_act30 = replace_na(mean_sh_2530/days_active, 0),
         mean_sh_2530 = replace_na(mean_sh_2530/time, 0),
         mean_sh_985_act30 = replace_na(mean_sh_98530/days_active, 0),
         mean_sh_98530 = replace_na(mean_sh_98530/time, 0),
         mean_sh_100_act30 = replace_na(mean_sh_10030/days_active, 0),
         mean_sh_10030 = replace_na(mean_sh_10030/time, 0),
         mean_num_unq_act30 = replace_na(mean_num_unq30/days_active30, 0),
         mean_num_unq30 = replace_na(mean_num_unq30/30, 0),
         mean_num_tot_act30 = replace_na(mean_num_tot30/days_active30, 0),
         mean_num_tot30 = replace_na(mean_num_tot30/30, 0),
         mean_av_pct_played_act30 = replace_na(mean_av_pct_played30/days_active30, 0),
         mean_av_pct_played30 = replace_na(mean_av_pct_played30/30, 0),
         mean_total_secs_imp_act30 = replace_na(mean_total_secs_imp30/days_active30, 0),
         mean_total_secs_imp30 = replace_na(mean_total_secs_imp30/30, 0),
         mean_total_hrs_imp_act30 = replace_na(mean_total_hrs_imp30/days_active30, 0),
         mean_total_hrs_imp30 = replace_na(mean_total_hrs_imp30/30, 0),
         mean_av_secs_played_act30 = replace_na(mean_av_secs_played30/days_active30, 0),
         mean_av_secs_played30 = replace_na(mean_av_secs_played30/30, 0),
         mean_av_mins_played_act30 = replace_na(mean_av_mins_played30/days_active30, 0),
         mean_av_mins_played30 = replace_na(mean_av_mins_played30/30, 0),
         days_active14 = replace_na(days_active14, 0),
         pct_active14 = replace_na(days_active14 / 14 * 100, 0),
         tot_num_tot14 = replace_na(mean_num_tot14,0),
         tot_total_secs_imp14 = replace_na(mean_total_secs_imp14, 0),
         mean_sh_25_act14 = replace_na(mean_sh_2514/days_active, 0),
         mean_sh_2514 = replace_na(mean_sh_2514/time, 0),
         mean_sh_985_act14 = replace_na(mean_sh_98514/days_active, 0),
         mean_sh_98514 = replace_na(mean_sh_98514/time, 0),
         mean_sh_100_act14 = replace_na(mean_sh_10014/days_active, 0),
         mean_sh_10014 = replace_na(mean_sh_10014/time, 0),
         mean_num_unq_act14 = replace_na(mean_num_unq14/days_active14, 0),
         mean_num_unq14 = replace_na(mean_num_unq14/14, 0),
         mean_num_tot_act14 = replace_na(mean_num_tot14/days_active14, 0),
         mean_num_tot14 = replace_na(mean_num_tot14/14, 0),
         mean_av_pct_played_act14 = replace_na(mean_av_pct_played14/days_active14, 0),
         mean_av_pct_played14 = replace_na(mean_av_pct_played14/14, 0),
         mean_total_secs_imp_act14 = replace_na(mean_total_secs_imp14/days_active14, 0),
         mean_total_secs_imp14 = replace_na(mean_total_secs_imp14/14, 0),
         mean_total_hrs_imp_act14 = replace_na(mean_total_hrs_imp14/days_active14, 0),
         mean_total_hrs_imp14 = replace_na(mean_total_hrs_imp14/14, 0),
         mean_av_secs_played_act14 = replace_na(mean_av_secs_played14/days_active14, 0),
         mean_av_secs_played14 = replace_na(mean_av_secs_played14/14, 0),
         mean_av_mins_played_act14 = replace_na(mean_av_mins_played14/days_active14, 0),
         mean_av_mins_played14 = replace_na(mean_av_mins_played14/14, 0),
         days_active7 = replace_na(days_active7, 0),
         pct_active7 = replace_na(days_active7 / 7 * 100, 0),
         tot_num_tot7 = replace_na(mean_num_tot7,0),
         tot_total_secs_imp7 = replace_na(mean_total_secs_imp7, 0),
         mean_sh_25_act7 = replace_na(mean_sh_257/days_active, 0),
         mean_sh_257 = replace_na(mean_sh_257/time, 0),
         mean_sh_985_act7 = replace_na(mean_sh_9857/days_active, 0),
         mean_sh_9857 = replace_na(mean_sh_9857/time, 0),
         mean_sh_100_act7 = replace_na(mean_sh_1007/days_active, 0),
         mean_sh_1007 = replace_na(mean_sh_1007/time, 0),
         mean_num_unq_act7 = replace_na(mean_num_unq7/days_active7, 0),
         mean_num_unq7 = replace_na(mean_num_unq7/7, 0),
         mean_num_tot_act7 = replace_na(mean_num_tot7/days_active7, 0),
         mean_num_tot7 = replace_na(mean_num_tot7/7, 0),
         mean_av_pct_played_act7 = replace_na(mean_av_pct_played7/days_active7, 0),
         mean_av_pct_played7 = replace_na(mean_av_pct_played7/7, 0),
         mean_total_secs_imp_act7 = replace_na(mean_total_secs_imp7/days_active7, 0),
         mean_total_secs_imp7 = replace_na(mean_total_secs_imp7/7, 0),
         mean_total_hrs_imp_act7 = replace_na(mean_total_hrs_imp7/days_active7, 0),
         mean_total_hrs_imp7 = replace_na(mean_total_hrs_imp7/7, 0),
         mean_av_secs_played_act7 = replace_na(mean_av_secs_played7/days_active7, 0),
         mean_av_secs_played7 = replace_na(mean_av_secs_played7/7, 0),
         mean_av_mins_played_act7 = replace_na(mean_av_mins_played7/days_active7, 0),
         mean_av_mins_played7 = replace_na(mean_av_mins_played7/7, 0))

trans_lags <- trans_periods %>%
  arrange(msno, msno_subscr_id) %>%
  group_by(msno) %>%
  filter(n() > 1) %>%
  transmute(subscr_id = unique(subscr_id),
            
            lag_time = replace_na(lag(time), 0),
            prev_time = cumsum(replace_na(lag(time), 0)),
            
            lag_days_active = replace_na(lag(days_active), 0),
            prev_days_active = cumsum(replace_na(lag(days_active), 0)),
            
            lag_pct_active = replace_na(lag(pct_active), 0),
            prev_pct_active = replace_na(prev_days_active/prev_time, 0),
            
            lag_tot_num_tot = replace_na(lag(tot_num_tot), 0),
            prev_tot_num_tot = cumsum(replace_na(lag(tot_num_tot), 0)),
            
            lag_tot_total_secs_imp = replace_na(lag(tot_total_secs_imp), 0),
            prev_tot_total_secs_imp = cumsum(replace_na(lag(tot_total_secs_imp), 0)),
            
            lag_mean_num_tot = replace_na(lag_tot_num_tot/lag_time, 0),
            prev_mean_num_tot = replace_na(prev_tot_num_tot/prev_time, 0),
            
            lag_mean_num_tot_act = replace_na(lag_tot_num_tot/lag_days_active, 0),
            prev_mean_num_tot_act = replace_na(prev_tot_num_tot/prev_days_active, 0),
            
            lag_mean_total_secs_imp = replace_na(lag_tot_total_secs_imp/lag_time, 0),
            prev_mean_total_secs_imp = replace_na(prev_tot_total_secs_imp/prev_time, 0),
            
            lag_mean_total_secs_imp_act = replace_na(lag_tot_total_secs_imp/lag_days_active, 0),
            prev_mean_total_secs_imp_act = replace_na(prev_tot_total_secs_imp/prev_days_active, 0),
            
            lag_mean_av_pct_played = replace_na(lag(mean_av_pct_played), 0),
            prev_mean_av_pct_played = replace_na(cumsum(replace_na(lag(mean_av_pct_played), 0))/(n()-1)),
            
            lag_mean_av_pct_played_act = replace_na(lag(mean_av_pct_played_act), 0),
            prev_mean_av_pct_played_act = replace_na(cumsum(replace_na(lag(mean_av_pct_played_act), 0))/(n()-1)),
            
            lag_mean_num_unq = replace_na(lag(mean_num_unq), 0),
            prev_mean_num_unq = replace_na(cumsum(replace_na(lag(mean_num_unq), 0))/(n()-1)),
            
            lag_mean_num_unq_act = replace_na(lag(mean_num_unq_act), 0),
            prev_mean_num_unq_act = replace_na(cumsum(replace_na(lag(mean_num_unq_act), 0))/(n()-1)),
            
            lag_mode_payment_id = factor(replace_na(lag(as.numeric(as.character(mode_payment_method_id))), 0),
                                         c(0, as.numeric(levels(trans_df$payment_method_id)))),
            prev_mode_payment_id = factor(replace_na(mode(as.numeric(as.character(mode_payment_method_id[-n()]))), 0),
                                          c(0, as.numeric(levels(trans_df$payment_method_id)), 999)),
            
            lag_sum_actual_amount_paid = replace_na(lag(sum_actual_amount_paid), 0),
            prev_sum_actual_amount_paid = cumsum(replace_na(lag(sum_actual_amount_paid), 0)),
            
            lag_mean_actual_amount_paid = replace_na(lag_sum_actual_amount_paid/lag_time, 0),
            prev_mean_actual_amount_paid = replace_na(prev_sum_actual_amount_paid/prev_time, 0),
            
            lag_num_cancel = replace_na(lag(n_cancel), 0),
            prev_num_cancel = cumsum(replace_na(lag(n_cancel), 0))) %>%
  ungroup()

trans_periods <- trans_periods %>%
  left_join(trans_lags, by = c("msno", "subscr_id")) %>%
  mutate_at(vars(contains("lag_"), dplyr::starts_with("prev_")), ~ replace_na(., 0))

trans_periods <- trans_periods %>%
  mutate(lag_mode_payment_id = fct_lump_min(lag_mode_payment_id, min = 30, other_level = "999"),
         prev_mode_payment_id = fct_lump_min(lag_mode_payment_id, min = 30, other_level = "999"))

var_label(trans_periods) <- list(time = "Subscription duration, days",
                                 pct_active = "%of active days in subscription",
                                 pct_active30 = "%of active days in the last 30 days of subscription",
                                 pct_active14 = "%of active days in the last 14 days of subscription",
                                 pct_active7 = "%of active days in the last 7 days of subscription")

tbl_summary(
  as.data.frame(trans_periods %>%
                  select(churn, time, contains("pct_active")) %>%
                  mutate(churn = factor(churn, labels = c("Churn = 0", "Churn = 1")))),
  by = "churn", 
  type = list(all_continuous() ~ "continuous2",
              pct_active7 ~ "continuous2"),
  statistic = list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})",  
                                        "{min}-{max}")),
  digits = list(all_continuous() ~ c(rep(1, 5), rep(0, 2)),
                time ~ c(rep(1, 2), rep(0, 5)))) %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n}") %>%
  add_overall(col_label = "**Total**<br>N = {N}", last = TRUE) %>%
  modify_footnote(update = everything() ~ NA) %>%
  bold_labels()


p1 <- trans_periods %>% 
  mutate(churn = factor(churn, labels = c("Churn = 0", "Churn = 1"))) %>%
  ggplot() +
  aes(x = pct_active, fill = churn) +
  geom_density(alpha = 0.4, color = "white") +
  labs(x = "", y = "Density", title = "% of active days in subscription", fill = "") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggthemes::theme_tufte() +
  theme(legend.position = c(0.5, 0.9),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10))

p2 <- trans_periods %>% 
  mutate(churn = factor(churn, labels = c("Churn = 0", "Churn = 1"))) %>%
  ggplot() +
  aes(x = pct_active30, fill = churn) +
  geom_density(alpha = 0.4, color = "white") +
  labs(x = "", y = "Density", title = "% of active days in the last 30 days of subscr.", fill = "") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggthemes::theme_tufte() +
  theme(legend.position = c(0.5, 0.9),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10))

p3 <- trans_periods %>% 
  mutate(churn = factor(churn, labels = c("Churn = 0", "Churn = 1"))) %>%
  ggplot() +
  aes(x = pct_active14, fill = churn) +
  geom_density(alpha = 0.4, color = "white") +
  labs(x = "", y = "Density", title = "% of active days in the last 14 days of subscr.", fill = "") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggthemes::theme_tufte() +
  theme(legend.position = c(0.5, 0.9),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10))

p4 <- trans_periods %>% 
  mutate(churn = factor(churn, labels = c("Churn = 0", "Churn = 1"))) %>%
  ggplot() +
  aes(x = pct_active7, fill = churn) +
  geom_density(alpha = 0.4, color = "white") +
  labs(x = "", y = "Density", title = "% of active days in the last 7 days of subscr.", fill = "") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggthemes::theme_tufte() +
  theme(legend.position = c(0.5, 0.9),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 10))

ggarrange(p1,p2,p3,p4, nrow = 2, ncol = 2, common.legend = TRUE)