library(xgboost)
library(lubridate)
library(magrittr)
library(tidyverse)

set.seed(0)

#---------------------------
cat("Preprocessing historical transactions...\n")

htrans <- read_csv("../input/historical_transactions.csv") %>% 
  rename(card = card_id)

sum_htrans_id <- htrans %>%
  group_by(card) %>%
  summarise_at(vars(ends_with("_id")), n_distinct, na.rm = TRUE) 

ohe_htrans <- htrans %>%
  select(authorized_flag, starts_with("category")) %>% 
  mutate_all(factor) %>% 
  model.matrix.lm(~ . - 1, ., na.action = NULL) %>% 
  as_tibble()

fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
sum_htrans <- htrans %>%
  select(-authorized_flag, -starts_with("category"), -ends_with("_id")) %>% 
  add_count(card) %>%
  group_by(card) %>%
  mutate(date_diff = as.integer(diff(range(purchase_date))),
         prop = n() / sum(n)) %>% 
  ungroup() %>% 
  mutate(year = year(purchase_date),
         month = month(purchase_date),
         day = day(purchase_date),
         hour = hour(purchase_date),
         month_diff = as.integer(ymd("2018-12-01") - date(purchase_date)) / 30 + month_lag) %>% 
  select(-purchase_date) %>% 
  bind_cols(ohe_htrans) %>% 
  group_by(card) %>%
  summarise_all(fn) %>% 
  left_join(sum_htrans_id)

rm(htrans, sum_htrans_id, ohe_htrans); invisible(gc())
 
#---------------------------
cat("Preprocessing new transactions...\n")

ntrans <- read_csv("../input/new_merchant_transactions.csv") %>% 
  left_join(read_csv("../input/merchants.csv"),
            by = "merchant_id", suffix = c("", "_y")) %>%
  select(-authorized_flag) %>% 
  rename(card = card_id)

sum_ntrans_id <- ntrans %>%
  group_by(card) %>%
  summarise_at(vars(contains("_id")), n_distinct, na.rm = TRUE) 

ohe_ntrans <- ntrans %>%
  select(starts_with("category"), starts_with("most_recent")) %>% 
  mutate_all(factor) %>% 
  model.matrix.lm(~ . - 1, ., na.action = NULL) %>% 
  as_tibble()

fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
sum_ntrans <- ntrans %>%
  select(-starts_with("category"), -starts_with("most_recent"), -contains("_id")) %>% 
  add_count(card) %>%
  group_by(card) %>%
  mutate(date_diff = as.integer(diff(range(purchase_date))),
         prop = n() / sum(n)) %>% 
  ungroup() %>% 
  mutate(year = year(purchase_date),
         month = month(purchase_date),
         day = day(purchase_date),
         hour = hour(purchase_date),
         month_diff = as.integer(ymd("2018-12-01") - date(purchase_date)) / 30 + month_lag) %>% 
  select(-purchase_date) %>% 
  bind_cols(ohe_ntrans) %>% 
  group_by(card) %>%
  summarise_all(fn) %>% 
  left_join(sum_ntrans_id)

rm(ntrans, sum_ntrans_id, ohe_ntrans, fn); invisible(gc())

#---------------------------
cat("Joining datasets...\n")

tr <- read_csv("../input/train.csv") 
te <- read_csv("../input/test.csv")

tri <- 1:nrow(tr)
y <- tr$target

tr_te <- tr %>% 
  select(-target) %>% 
  bind_rows(te) %>%
  rename(card = card_id) %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         date_diff = as.integer(ymd("2018-02-01") - first_active_month)) %>% 
  select(-first_active_month) %>% 
  left_join(sum_htrans, by = "card") %>% 
  left_join(sum_ntrans, by = "card") %>% 
  select(-card) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  select_if(~ n_distinct(.x) > 1) %>% 
  data.matrix()

rm(tr, te, sum_htrans, sum_ntrans); invisible(gc())

#---------------------------
cat("Preparing data...\n")

val <- caret::createDataPartition(y, p = 0.2, list = FALSE)
dtrain <- xgb.DMatrix(data = tr_te[tri, ][-val, ], label = y[-val])
dval <- xgb.DMatrix(data = tr_te[tri, ][val, ], label = y[val])
dtest <- xgb.DMatrix(data = tr_te[-tri, ])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")
p <- list(objective = "reg:linear",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 4,
          eta = 0.02,
          max_depth = 7,
          min_child_weight = 100,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.8,
          colsample_bylevel = 0.8,
          alpha = 0,
          lambda = 0.1)

set.seed(0)
m_xgb <- xgb.train(p, dtrain, 2000, list(val = dval), print_every_n = 100, early_stopping_rounds = 200)

xgb.importance(cols, model = m_xgb) %>% 
  xgb.ggplot.importance(top_n = 20) + theme_minimal()

#---------------------------
read_csv("../input/sample_submission.csv") %>%  
  mutate(target = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_elo_", round(m_xgb$best_score, 5), ".csv"))