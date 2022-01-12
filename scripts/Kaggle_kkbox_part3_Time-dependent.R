#### Data for models with time-dependent covariates ####

trans_df_date <- trans_df %>%
  group_by(subscr_id, transaction_date) %>%
  summarise(date = unique(transaction_date),
            actual_amount_paid = sum(actual_amount_paid),
            is_cancel = sum(is_cancel), 
            payment_method_id = payment_method_id[n()],
            plan_list_price = max(plan_list_price)) %>%
  ungroup()

ulogs_trans_periods <- ulogs_periods %>%
  select(date, subscr_id, num_tot, num_unq, total_secs_imp, av_pct_played) %>%
  mutate(file = "ulogs") %>%
  full_join(trans_periods %>%
              select(subscr_id, msno, msno_subscr_id, churn, time, start_date, end_date,
                     churned_before_num, city, bdG, gender, registered_via, month_period,
                     first_payment_method_id, first_plan_list_price,
                     time_from_reg, time_from_prev_exp, prev_time,
                     prev_mean_num_tot, prev_mean_total_secs_imp,
                     prev_mean_num_unq, prev_mean_av_pct_played_act,
                     prev_mean_actual_amount_paid, prev_num_cancel,
                     prev_mode_payment_id),
            by = "subscr_id") %>%
  full_join(trans_df_date %>% transmute(subscr_id, 
                                        date = as.Date(ifelse(subscr_id %in% ulogs_periods$subscr_id,
                                                              transaction_date, 
                                                              NA), "1970-01-01"),
                                        transaction_date, actual_amount_paid, plan_list_price,
                                        is_cancel, payment_method_id),
            by = c("subscr_id", "date"))

ulogs_trans_periods <- ulogs_trans_periods %>%
  mutate(date = ifelse(is.na(date), transaction_date, date)) %>%
  mutate_at(vars(num_tot, num_unq, total_secs_imp, av_pct_played,
                 actual_amount_paid, plan_list_price, is_cancel), ~ replace_na(., 0)) %>%
  group_by(subscr_id) %>%
  mutate_at(vars(msno, msno_subscr_id, churn, time, start_date, end_date,
                 churned_before_num, city, bdG, gender, registered_via, month_period,
                 first_payment_method_id, first_plan_list_price,
                 time_from_reg, time_from_prev_exp, prev_time,
                 prev_mean_num_tot, prev_mean_total_secs_imp,
                 prev_mean_num_unq, prev_mean_av_pct_played_act,
                 prev_mean_actual_amount_paid, prev_num_cancel,
                 prev_mode_payment_id),
            ~ unique(.[!is.na(.)])) %>%
  mutate(churn = ifelse(row_number() != n(), 0, churn))%>%
  ungroup()

ulogs_trans_periods <- ulogs_trans_periods %>%
  mutate(date = as.Date(date, "1970-01-01")) %>%
  arrange(subscr_id, date)

ulogs_trans_periods <- suppressWarnings(
  ulogs_trans_periods %>%
    group_by(subscr_id) %>%
    mutate(time_start = replace_na(as.numeric(date - date[1], 'days'), 0),
           time_stop = ifelse(is.na(lead(time_start)),
                              ifelse(is.na(date),
                                     time,
                                     as.numeric(end_date - date[1], 'days') + 1),
                              lead(time_start)),
           time_stop = ifelse(time_stop == 0, 1, time_stop),
           time_int = time_stop - time_start,
           cum_pct_active = cumsum(!is.na(file))/time_stop*100,
           cum_mean_num_tot = cumsum(num_tot)/time_stop,
           cum_mean_num_unq = cumsum(num_unq)/time_stop,
           cum_mean_total_secs_imp = cumsum(total_secs_imp)/time_stop,
           mean_av_pct_played_act = replace_na(cumsum(av_pct_played)/cumsum(!is.na(file)), 0),
           cum_mean_actual_amount_paid = cumsum(actual_amount_paid)/time_stop,
           cum_num_cancel = cumsum(is_cancel),
           last_payment_method_id = zoo::na.locf(payment_method_id),
           last_plan_list_price = zoo::na.locf(plan_list_price)) %>%
    ungroup())

# get indices for test set and sampled IDs (saved from python chunks)

train_set <- read.csv("X_train_index.csv")
sample_X_test_id <- read.csv("sample_X_test_id.csv")

# train and test for models with time-dependent covariates

df_features <- ulogs_trans_periods %>%
  transmute(subscr_id, time_start, time_stop, churn,
            churned_before_num, 
            cityG = fct_lump_min(city, 30, other_level = "19-20"), bdG, gender,
            registered_via, month_period,
            time_from_reg, time_from_prev_exp, prev_time, 
            cum_pct_active, cum_mean_num_tot, cum_mean_total_secs_imp,
            cum_mean_num_unq, mean_av_pct_played_act, cum_mean_actual_amount_paid,
            cum_num_cancel, last_payment_method_id, last_plan_list_price)

df_train <- df_features %>%
  filter(subscr_id %in% trans_periods$subscr_id[train_set$ind + 1])

df_test <- df_features %>%
  filter(! subscr_id %in% trans_periods$subscr_id[train_set$ind + 1])

sample_X_subscr_id <- trans_periods$subscr_id[sample_X_test_id$ID + 1]

sample_test <- df_features %>%
  filter(subscr_id %in% trans_periods$subscr_id[sample_X_test_id$ID + 1])

features_baseline <- c("churned_before_num", 
                       "cityG", "bdG", "gender",
                       "registered_via", "month_period",
                       "time_from_reg", "time_from_prev_exp", "prev_time")
features_timedep <- c("cum_pct_active", "cum_mean_num_tot", "cum_mean_total_secs_imp",
                      "cum_mean_num_unq", "mean_av_pct_played_act", "cum_mean_actual_amount_paid",
                      "cum_num_cancel", "last_payment_method_id", "last_plan_list_price")  


# Function to predict survival after Cox PH

survest_ind <- function(estimator, id_period) {
  intervals <- df_test %>%
    filter(subscr_id == id_period) %>%
    select(time_start, time_stop, churn)
  
  covs <- data.frame(df_test %>% filter(subscr_id == id_period) %>% select(all_of(features_baseline)) %>% unique())
  covs[, features_timedep] <- 0
  covs$last_payment_method_id <- trans_periods$first_payment_method_id[trans_periods$subscr_id == id_period]
  covs$last_plan_list_price <- trans_periods$first_plan_list_price[trans_periods$subscr_id == id_period]
  
  newdata <- data.frame(covs[1, ], intervals, row.names = NULL)
  out <- survfit(estimator, newdata = newdata)
  
  survests <- list(baseline = tibble(time = out$time, 
                                     surv = out$surv[,1]))
  
  covs <- df_test %>%
    filter(subscr_id == id_period)
  
  newdata <- data.frame(covs %>% select(-subscr_id), row.names = NULL)
  out <- survfit(estimator, newdata = newdata)
  
  survests[["complete"]] <- out$surv
  
  survests
}

# Function for plotting updated and not updated survival curves at #num points

survest_plots <- function(idt, idsurv, num=10) {
  times <- df_test %>%
    filter(subscr_id == idt) %>%
    select(time_stop) %>%
    pull()
  
  if (length(times) >= num) {
    times_select <- seq_along(times)
    times_select <- unique(c(seq(1, max(times_select), floor(max(times_select)/(num-1))), max(times_select)))
    times_select <- times[times_select]
  } else {
    times_select <- times
  }
  
  times_select <- times_select[order(times_select)]
  
  times_lab <- c("Baseline", times_select)
  
  colors <- paletteer_c("grDevices::Zissou 1", 1 + length(times_select))
  
  dfp <- tibble(time = as.numeric(idsurv$baseline$time),
                surv = as.numeric(idsurv$baseline$surv), 
                lbl = sprintf("If not updated after %s%s", times_lab[1], ""))
  p <- ggplot(dfp) +
    geom_line(aes(x = time, y = surv, linetype = lbl),
              size = 1, color = colors[1]) +
    scale_y_continuous(breaks = seq(0,1,0.2),
                       labels = scales::percent_format(accuracy = 1),
                       expand = c(0.02,0),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 810, 90), expand = c(0.02,0)) +
    labs(x = "Days", y = "Exp. survival probability") +
    theme_classic() 
  plots <- list(p)
  
  surv_upd <- dfp$surv[1]
  
  for (i in seq_along(times_select)) {
    dfp <- tibble(time = as.numeric(idsurv$baseline$time[idsurv$baseline$time >= times_select[i]]),
                  surv = as.numeric(idsurv$complete[idsurv$baseline$time >= times_select[i], 
                                                    which(times == times_select[i])]),
                  lbl = sprintf("If not updated after %s%s", times_lab[i+1], "d."))
    surv_upd <- c(surv_upd, dfp$surv[1])
    
    p <- ggplot(dfp) +
      geom_line(aes(x = time, y = surv, linetype = lbl),
                size = 1, color = colors[i + 1]) +
      scale_y_continuous(breaks = seq(0,1,0.2), 
                         labels = scales::percent_format(accuracy = 1),
                         expand = c(0.02,0), limits = c(0, 1)) +
      scale_x_continuous(breaks = seq(0, 810, 90), expand = c(0.02,0),
                         limits = c(0, 821)) +
      labs(x = "Days", y = "Exp. survival probability", linetype = "") +
      theme_classic()
    
    plots[[i+1]] <- p
  }
  
  dfp <- tibble(time = c(0, times_select),
                surv = surv_upd)
  
  for (i in 1:nrow(dfp)) {
    plots[[i]] <- plots[[i]] +
      geom_line(aes(x = time, y = surv, linetype = "Updated"), dfp,
                size = 0.5, color = "grey20", show.legend = TRUE) +
      geom_point(aes(x = time, y = surv), 
                 dfp[i,], shape = 21,
                 size = 3, color = "black", fill = colors[i],
                 show.legend = FALSE) +
      scale_linetype_manual(values = c("solid", "dotted")) +
      labs(linetype = "") +
      guides(linetype = guide_legend(override.aes = list(color = c(colors[i], "grey50")))) +
      theme(legend.position = c(0.7,0.9),
            legend.background = element_blank())
    
    plots[[i]]$layers <- plots[[i]]$layers[c(2,1,3)]
    
  }
  
  plots
}

#### Cox with time-dependent covariates ####

cox_td <- coxph(Surv(time = time_start, time2 = time_stop, event = churn) ~ ., 
                df_train %>% select(-subscr_id), x = TRUE)

cox_td_sum <- summary(cox_td)

# Plot gifs for updated and not updated survival curves for sampled test periods
# + one additional for illustration

for (idt in c(sample_X_subscr_id, 16739)) {
  
  test <- survest_ind(cox_td, idt)
  test_plots <- survest_plots(idt, test)
  
  dir_out <- file.path(tempdir(), paste0("surv_plots/", idt))
  dir.create(dir_out, recursive = TRUE)
  
  for (i in 1:length(test_plots)) {
    
    fp <- file.path(dir_out, paste0("id", idt, "_", 100+i, ".png"))
    
    ggsave(plot = test_plots[[i]], 
           filename = fp, width = 5, height = 3, dpi = 300,
           device = "png")
  }
  
  imgs <- list.files(dir_out, full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  img_list <- lapply(img_list, image_scale, "x600")
  
  img_joined <- image_join(img_list)
  
  img_animated <- image_animate(img_joined, fps = 1)
  
  image_write(image = img_animated,
              path = paste0("test_plots_", idt, ".gif"))
}  

# Model evaluation

cox_td_conc_train <- cox_td_sum$concordance[1]
cox_td_conc_test <- concordance(cox_td, newdata = data.frame(df_test))
cox_td_conc_test <- cox_td_conc_test$concordance


cox_td_pred <- survfit(cox_td, newdata = df_test, type = "aalen",
                       id = df_test$subscr_id)

cox_td_pred_df <- data.frame(time = cox_td_pred$time,
                             prob = cox_td_pred$surv)
cox_td_pred_df$id <- cox_td_pred_df$time - replace_na(lag(cox_td_pred_df$time),3)
cox_td_pred_df$id <- cumsum(cox_td_pred_df$id <= 0)

cox_td_pred_list <- split(cox_td_pred_df, cox_td_pred_df$id)
cox_td_pred_list <- setNames(cox_td_pred_list, NULL)

cox_td_pred_list_ibs720 <- lapply(cox_td_pred_list,
                                  function(x) {
                                    x <- x[x$time <= 720, ]
                                    x <- x[,-c(1,3)]
                                    c(1, x)
                                  })

cox_td_for_ibs720 <- list(survival.probs = cox_td_pred_list_ibs720,
                          survival.times = 0:720,
                          survival.tau = pmin(taus, 720))

data <- data.frame(df_test)
Survobj = Surv(data$time_start, data$time_stop, data$churn)
cox_ibs_td_pred <- sbrier_ltrc(obj = Survobj, id = data$subscr_id,
                               pred = cox_td_for_ibs720, type = "IBS")


# Conditional survival probability after Cox PH (example)

newdata <- df_test %>%
  filter(subscr_id == 16739)
newdata <- data.frame(newdata %>% select(-subscr_id), row.names = NULL)
out <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 0)
out30 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 30)
out60 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 60)
out90 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 90)
out180 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 180)
out270 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 270)
out360 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 360)
out450 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 450)
out540 <- survfit(cox_td, newdata = newdata, se.fit = F, start.time = 540)

ggplot() +
  geom_line(aes(x = c(0, out$time), y = c(1, out$surv[,714]), color = "0"), size = 1) +
  geom_line(aes(x = c(30, out30$time), y = c(1, out30$surv[,714]), color = "30d"), size = 1) +
  geom_line(aes(x = c(60, out60$time), y = c(1, out60$surv[,714]), color = "60d"), size = 1) +
  geom_line(aes(x = c(90, out90$time), y = c(1, out90$surv[,714]), color = "90d"), size = 1) +
  geom_line(aes(x = c(180, out180$time), y = c(1, out180$surv[,714]), color = "180d"), size = 1) +
  geom_line(aes(x = c(270, out270$time), y = c(1, out270$surv[,714]), color = "270d"), size = 1) +
  geom_line(aes(x = c(360, out360$time), y = c(1, out360$surv[,714]), color = "360d"), size = 1) +
  geom_line(aes(x = c(450, out450$time), y = c(1, out450$surv[,714]), color = "450d"), size = 1) +
  geom_line(aes(x = c(540, out540$time), y = c(1, out540$surv[,714]), color = "540d"), size = 1) +
  scale_color_manual(values = setNames(paletteer_d("ggsci::deep_orange_material")[10:2],
                                       c("0", paste0(c(30,60,90,180,270,360,450,540), "d")))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1),
                     breaks = seq(0,1,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0,810,90), expand = c(0,0)) +
  labs(x = "Days", y = "Conditional survival probability",
       title = "Predicted conditional survival probability", color = "") +
  theme_classic() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 12),
        panel.grid.major = element_line(size = .2, colour = "grey90"),
        panel.grid.minor = element_line(size = .2, colour = "grey90"))


# Variable importance in Cox PH with time-dependent covariates

fml = as.formula(paste0("Surv(time = time_start, time2 = time_stop, event = churn) ~ ",
                        paste(features_baseline, collapse = " + "),
                        " + ",
                        paste(features_timedep, collapse = " + ")))

cox_td_rms <- cph(fml, df_train, x=TRUE, y=TRUE)
cox_td_anova_rms <- anova(cox_td_rms)
plot(cox_td_anova_rms, sort = "ascending")


#### RSF with time-dependent covariates ####

fml = as.formula(paste0("Surv(time = time_start, time2 = time_stop, event = churn, type = 'counting') ~ ",
                        paste(features_baseline, collapse = " + "),
                        " + ",
                        paste(features_timedep, collapse = " + ")))

rrf_fit <- ltrcrrf(formula = fml, ntree = 10, mtry = 6,
                   data = data.frame(df_train), 
                   id = subscr_id)
# test
rrf_pred <- predictProb(object = rrf_fit, 
                        newdata = data.frame(df_test),
                        newdata.id = subscr_id,
                        time.eval = 0:720)
data <- data.frame(df_test)
Survobj = Surv(data$time_start, data$time_stop, data$churn)
rrf_pred_ibs <- sbrier_ltrc(obj = Survobj, id = data$subscr_id, 
                            pred = rrf_pred, type = "IBS")
rrf_pred_bs <- sbrier_ltrc(obj = Survobj, id = data$subscr_id, 
                           pred = rrf_pred, type = "BS")

rrf_pred821 <- predictProb(object = rrf_fit, 
                           newdata = data.frame(df_test),
                           newdata.id = subscr_id,
                           time.eval = 0:821)
rrf_pred_prob <- tibble(subscr_id = rep(X_test_subscr_id, each = 822),
                        time_stop = rep(0:821, times = length(X_test_subscr_id)),
                        pred_prob = as.numeric(rrf_pred821$survival.probs))
rrf_pred_prob <- rrf_pred_prob %>%
  right_join(df_test %>% select(subscr_id, time_stop), by = c("subscr_id", "time_stop"))
data <- data.frame(df_test)
rrf_conc <- concordancefit(Surv(data$time_start, data$time_stop, data$churn), 
                           x = rrf_pred_prob %>% pull(pred_prob))

# train
rrf_pred821_tr <- predictProb(object = rrf_fit, 
                              newdata = data.frame(df_train),
                              newdata.id = subscr_id,
                              time.eval = 0:821)
rrf_pred_tr_prob <- tibble(subscr_id = rep(unique(df_train$subscr_id), each = 822),
                           time_stop = rep(0:821, times = length(unique(df_train$subscr_id))),
                           pred_prob = as.numeric(rrf_pred821_tr$survival.probs))
rrf_pred_tr_prob <- rrf_pred_tr_prob %>%
  right_join(df_train %>% select(subscr_id, time_stop), by = c("subscr_id", "time_stop"))
data <- data.frame(df_train)
rrf_conc_tr <- concordancefit(Surv(data$time_start, data$time_stop, data$churn), 
                              x = rrf_pred_tr_prob %>% pull(pred_prob))

rrf_pred720_tr <- rrf_pred821_tr[2:4]
rrf_pred720_tr$survival.probs <- rrf_pred720_tr$survival.probs[1:721,]
rrf_pred720_tr$survival.times <- rrf_pred720_tr$survival.times[1:721]

data <- data.frame(df_train)
Survobj = Surv(data$time_start, data$time_stop, data$churn)
rrf_pred_bs_tr <- sbrier_ltrc(obj = Survobj, id = data$subscr_id, 
                              pred = rrf_pred720_tr, type = "BS")