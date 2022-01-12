#### Data for models with time-independent variables ####

X <- trans_periods %>%
  transmute(churned_before_num, 
            cityG = fct_lump_min(city, 30, other_level = "19-20"), 
            bdG, gender, registered_via, month_period,
            time_from_reg, time_from_prev_exp, 
            first_payment_method_id, first_plan_list_price,
            prev_time, prev_pct_active = 100*prev_pct_active, 
            prev_mean_num_tot, prev_mean_total_secs_imp,
            prev_mean_num_unq, prev_mean_av_pct_played_act,
            prev_mean_actual_amount_paid, prev_num_cancel,
            prev_mode_payment_id)

X_base <- X %>% select(-contains("prev_"))

Y_clmns <- trans_periods %>%
  select(churn, time)

sample_X_test_id <- read.csv("data/sample_X_test_id.csv") # file with randomly sampled 3 churned and 3 non-churned users from test sample

cat_columns <- map_dfc(X, ~ !is.numeric(.x)) %>% t()
num_columns <- map_dfc(X, ~is.numeric(.x)) %>% t()
cat_columns <- names(X)[cat_columns[,1]]
num_columns <- names(X)[num_columns[,1]]

cat_base_columns <- map_dfc(X_base, ~ !is.numeric(.x)) %>% t()
num_base_columns <- map_dfc(X_base, ~is.numeric(.x)) %>% t()
cat_base_columns <- names(X_base)[cat_base_columns[,1]]
num_base_columns <- names(X_base)[num_base_columns[,1]]


#### Results, time-independent variables (metrics obtained in python chunks) ####

# C-index

metrics <- list(CoxPH_base = py$cox_base_results,
                CoxPH = py$cox_results,
                RSF_base = py$rsf_base_results,
                RSF = py$rsf_results)

cindex <- lapply(metrics, function(x) x[1:2,])
cindex <- lapply(cindex, function(x) {x[,2] <- as.numeric(x[,2]) 
x})
cindex <- do.call(dplyr::bind_rows, cindex)

cindex$model <- rep(c("Cox PH baseline", "Cox PH prev", "RSF baseline", "RSF prev"), each=2)
cindex <- rbind(cindex[,c(1:2,4)] %>% rename(value=Train) %>% mutate(set="Train"), 
                cindex[,c(1,3:4)] %>% rename(value=Test) %>% mutate(set="Test"))
cindex$Metric <- gsub("Harrel", "Harrell", cindex$Metric)

ggplot() +
  geom_bar(aes(x=Metric, y=value, fill=model), cindex,
           stat = "identity", 
           position = position_dodge(0.8), color = "white") +
  geom_text(aes(x=Metric, y=y, label=lbl, group=model), 
            cindex %>% mutate(y = value + 0.03, lbl = round(value,2)),
            position = position_dodge(0.8), stat = "identity",
            size = 3, fontface = "bold") +
  facet_wrap(~ set) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  scale_fill_manual(values = paletteer_d("ggthemes::Tableau_20")[c(2,1,4,3)]) +
  labs(x="", y="", fill="", title="C-index", color="") +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.line = element_line(size = .2, color = "grey20"),
        axis.text.x = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill = NA))

# IBS

ibs <- unlist(lapply(metrics, function(x) x[3,3]))
ibs <- tibble(model = c("Cox PH baseline", "Cox PH prev", "RSF baseline", "RSF prev"),
              value = ibs)

ggplot(ibs) +
  geom_bar(aes(x=model, y=value, fill=model), stat = "identity", width = 0.6) +
  geom_text(aes(x=model, y=value-0.01, label=round(value,3)),
            size = 4, color = "grey10", fontface = "bold") +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.16),
                     breaks = seq(0,0.15,0.05)) +
  scale_fill_manual(values = paletteer_d("ggthemes::Tableau_20")[c(2,1,4,3)]) +
  labs(x="", y="", title="IBS, test sample") +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold"),
        axis.line = element_line(size = .2, color = "grey20"),
        axis.text.y = element_text(size = 12, hjust = 0,
                                   color = "grey10", face = "bold",
                                   margin = margin(l = 10, r = -100)),
        axis.ticks.y = element_blank())

# BS 

  # metrics obtained in python chunks

bs <- list(CoxPH_base = py$bs_base_test,
           CoxPH = py$bs_test,
           RSF_base = py$rsf_bs_base_test,
           RSF = py$rsf_bs_test)
bs <- lapply(bs, 
             function(x) {
               x[,2] <- as.numeric(gsub(",",".", x[,2]))
               x[,3] <- as.numeric(gsub(",",".", x[,3]))
               x})
bs <- do.call(dplyr::bind_rows, bs)

bs$model <- rep(c("Cox PH baseline", "Cox PH prev", "RSF baseline", "RSF prev"), each=8)
bs <- rbind(bs[,c(1:2,4)] %>% rename(value=Train) %>% mutate(set="Train"), 
            bs[,c(1,3:4)] %>% rename(value=Test) %>% mutate(set="Test"))

ggplot() +
  geom_line(aes(x=Time, y=value, color=model), bs, size=1.2) +
  geom_point(aes(x=Time, y=value, color=model), bs, size=3) +
  facet_rep_wrap(~ set, repeat.tick.labels = TRUE) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.2)) +
  scale_x_continuous(breaks = seq(0,720,180)) +
  scale_color_manual(values = paletteer_d("ggthemes::Tableau_20")[c(2,1,4,3)]) +
  labs(x="Time (days)", y="Time-dependent Brier score", color="", title="Brier score") +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold"),
        axis.line = element_line(size = .2, color = "grey20"),
        strip.background = element_rect(fill = NA))


# Predicted survival plots (with graphs saved from python chunks)

p1 <- ggdraw() + draw_image("cox_base_surv.png", scale = 1)
p2 <- ggdraw() + draw_image("cox_surv.png", scale = 1)
p3 <- ggdraw() + draw_image("rsf_surv.png", scale = 1)
p4 <- ggdraw() + draw_image("rsf_surv.png", scale = 1)
ggarrange(p1,p2,p3,p4,nrow = 2, ncol = 2)


# Variable importance, Cox

df_feat <- trans_periods %>%
  transmute(subscr_id, churn, time,
            churned_before_num, 
            cityG = fct_lump_min(city, 30, other_level = "19-20"), 
            bdG, gender, registered_via, month_period,
            time_from_reg, time_from_prev_exp, 
            first_payment_method_id, first_plan_list_price,
            prev_time, prev_pct_active = 100*prev_pct_active, 
            prev_mean_num_tot, prev_mean_total_secs_imp,
            prev_mean_num_unq, prev_mean_av_pct_played_act,
            prev_mean_actual_amount_paid, prev_num_cancel,
            prev_mode_payment_id)

df_feat_base <- df_feat %>% select(-contains("prev_"))

feat_base <- names(df_feat_base)[-c(1:3)]
feat_prev <- names(df_feat)[-c(1:3)]

train_set <- tibble(ind = py$X_train.index.tolist()) # indices of rows that were placed into train sample in python chunk
sample_X_test_id <- read.csv("data/sample_X_test_id.csv")

df_base_tr <- df_feat_base %>%
  filter(subscr_id %in% trans_periods$subscr_id[train_set$ind + 1])

df_base_tst <- df_feat_base %>%
  filter(! subscr_id %in% trans_periods$subscr_id[train_set$ind + 1])

df_tr <- df_feat %>%
  filter(subscr_id %in% trans_periods$subscr_id[train_set$ind + 1])

df_tst <- df_feat %>%
  filter(! subscr_id %in% trans_periods$subscr_id[train_set$ind + 1])

sample_X_subscr_id <- trans_periods$subscr_id[sample_X_test_id$ID + 1]

samp_base_test <- df_feat_base %>%
  filter(subscr_id %in% trans_periods$subscr_id[sample_X_test_id$ID + 1])
samp_test <- df_feat %>%
  filter(subscr_id %in% trans_periods$subscr_id[sample_X_test_id$ID + 1])

fml_base = as.formula(paste0("Surv(time = time, event = churn) ~ ",
                             paste(feat_base, collapse = " + ")))
fml_prev = as.formula(paste0("Surv(time = time, event = churn) ~ ",
                             paste(feat_prev, collapse = " + ")))


cox_base_rms <- cph(fml_base, df_base_tr, x=TRUE, y=TRUE)
cox_base <- coxph(fml_base, df_base_tr)
cox_base_anova_rms <- anova(cox_base_rms)
png("cox_base_imp.png", width = 500, height = 400)
plot(cox_base_anova_rms, sort = "ascending")
dev.off()

cox_rms <- cph(fml_prev, df_tr, x=TRUE, y=TRUE)
cox <- coxph(fml_prev, df_tr)
cox_anova_rms <- anova(cox_rms)
png("cox_imp.png", width = 500, height = 400)
plot(cox_anova_rms, sort = "ascending")
dev.off()

p1 <- ggdraw() + draw_image("cox_base_imp.png", scale = 1)
p2 <- ggdraw() + draw_image("cox_imp.png", scale = 1)
ggarrange(p1,p2,nrow = 1, ncol = 2, 
          labels = c("Cox PH baseline, variable importance",
                     "Cox PH prev, variable importance"), hjust = -0.25)

# Variable importance, RSF, plots saved from python chunks

p1 <- ggdraw() + draw_image("rsf_base_imp.png", scale = 1)
p2 <- ggdraw() + draw_image("rsf_imp.png", scale = 1)
ggarrange(p1,p2,nrow = 1, ncol = 2)

# Cox PH coefficients -> HRs

feature_group <- function(x, feat) {
  for (i in 1:length(x)) {
    for (j in feat) {
      if (grepl(j, x[i])) {
        x[i] <- j
      }
    }
  }
  x
}

cox_base_coef <- summary(cox_base)
cox_base_coef <- tibble(Variable = row.names(cox_base_coef$coefficients),
                        group = feature_group(Variable, feat_base),
                        HR = cox_base_coef$coefficients[,2],
                        se = cox_base_coef$coefficients[,3]) %>%
  arrange(HR, group)

p1 <- ggplot(cox_base_coef, aes(x=Variable, y=HR, color=group)) + 
  geom_hline(yintercept = 1) +
  geom_pointrange(aes(ymin=HR*exp(-1.96*se), ymax=HR*exp(1.96*se))) +
  scale_x_discrete(limits = cox_base_coef$Variable) +
  scale_color_manual(values = rev(paletteer_d("ggthemes::Tableau_10")[1:9])) +
  coord_flip() +
  labs(y = "Hazard Ratio [95% CI]", x = "", title = "Cox PH baseline, hazard ratios") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.line = element_line(size = .2, color = "grey20"),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size=.2, color="grey90"),
        panel.grid.minor.x = element_line(size=.2, color="grey90"),
        panel.grid.major.y = element_line(size=.2, color="grey90", linetype="dotted"))

cox_coef <- summary(cox)
cox_coef <- tibble(Variable = row.names(cox_coef$coefficients),
                   group = feature_group(Variable, feat_prev),
                   HR = cox_coef$coefficients[,2],
                   se = cox_coef$coefficients[,3]) %>%
  arrange(HR, group)

p2 <- ggplot(cox_coef, aes(x=Variable, y=HR, color=group)) + 
  geom_hline(yintercept = 1) +
  geom_pointrange(aes(ymin=HR*exp(-1.96*se), ymax=HR*exp(1.96*se))) +
  scale_x_discrete(limits = cox_coef$Variable) +
  scale_color_manual(values = paletteer_d("ggthemes::Classic_20")[1:19]) +
  coord_flip() +
  labs(y = "Hazard Ratio [95% CI]", x = "", title = "Cox PH prev, hazard ratios") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.line = element_line(size = .2, color = "grey20"),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(size=.2, color="grey90"),
        panel.grid.minor.x = element_line(size=.2, color="grey90"),
        panel.grid.major.y = element_line(size=.2, color="grey90", linetype="dotted"))

ggarrange(p1,p2,nrow = 1,ncol = 2)