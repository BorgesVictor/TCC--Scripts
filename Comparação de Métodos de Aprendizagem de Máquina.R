rm(list = ls())

# Comparação entre metodos de aprendizagem de maquina :

# Bibliotecas

library(tidyverse)
library(tidymodels)
library(poissonreg)
library(skimr)
library(GGally)
library(corrplot)
library(readxl)
library(vip)
library(conflicted)
library(iml)
library(dplyr)
library(magrittr)
library(glmnet)
library(rpart)
library(ranger)
library(kernlab)
library(nnet)
library(xgboost)
library(kknn)
library(ggplot2)

tidymodels_prefer()
ggplot2::theme_set(theme_bw())
options(warn = -1)

set.seed(1914)

setwd("C:/Users/VictorBorges/Desktop/TCC & PIBIC/TCC - PIBIC 2022-2023/Banco para analise")


BancoRegressaoOficial <- read_excel(
  "BancoRegressaoOficial.xlsx",
  col_types = c(
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)


# criar as dummy para depois entrar como varias colunas de 1 e 0 e aplicar
# os modelos


head(df)

df = BancoRegressaoOficial
#df<-df[c(-4,-5,-10)]
df <- na.omit(df)
df <- df[c(-15, -16, -18)]
# aparecendo um "i" antes e incomoda !!

names(df)
colnames(df)[13] <- "IDHEDUCACAO"

# Olhada rapida no banco de dados

summary(df)
skim(df)

# alterando a variavel resposta para ser factor :


df %<>%  mutate(
  Tempo = as.factor(Tempo),
  SEXO = as.factor(SEXO),
  RACACOR = as.factor(RACACOR),
  FAIXA_ETARIA = as.factor(FAIXA_ETARIA),
  CLITRATCAT = as.factor(CLITRATCAT),
  SOLIDOHEMATO = as.factor(SOLIDOHEMATO),
  RZNTR =  as.factor(RZNTR),
  TIPOHISTCATEG = as.factor(TIPOHISTCATEG),
  REGIÃOCATEG = as.factor(REGIÃOCATEG),
  PRITRATH = as.factor(PRITRATH),
)

head(df)


# Modelagem

# O conjunto de dados original foi inicialmente particionado
# em um conjunto de treinamento, com 90% das observações
# (n=30) e um conjunto de teste (n=4), com 10% das observações.
# É importante ressaltar o número pequeno de observações
# disponíveis para ajuste dos modelos.

set.seed(13)
df_split = initial_split(df, prop = .75)
df_trn = training(df_split)
df_tst = testing(df_split)
dim(df_trn)

dim(df_tst)


df_recipe = recipe(Tempo ~ ., data = df_trn) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
  step_zv(all_numeric(),-all_outcomes()) %>% # remove variáveis numéricas que têm variância 0
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") # remove preditores que tenham alta correlação com algum outro preditor
#step_nzv(all_predictors(), freq_cut = 90/10)

# Validação cruzada para tunning de hiperparâmetros

# Para a otimização dos hiperparâmetros dos modelos de
# aprendizagem de máquina,o conjunto de treinamento foi
# utilizado em um esquema de validação cruzada com 10 folds
# repetido 1 vezes.

df_vfolds = vfold_cv(df_trn, v = 10, repeats = 1)


# Definindo o modelo


lm_fit = logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet", standardize = FALSE)

rt_fit = decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("rpart")


rf_fit = rand_forest(mtry = tune(),
                     min_n = tune(),
                     trees = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")


svm_fit1 = svm_linear(cost = tune(), margin = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")


svm_fit2 = svm_rbf(cost = tune(),
                   rbf_sigma = tune(),
                   margin = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")


svm_fit3 = svm_poly(cost = tune(), margin = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")


mlp_fit = mlp(hidden_units = tune(),
              penalty = tune(),
              epochs = tune()) %>%
  set_mode("classification") %>%
  set_engine("nnet")

xgb_fit = boost_tree(
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  sample_size = tune(),
  trees = tune()
) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

# fazendo o usal distância euclidiana e kernal e vai escolher o melhor

knn_fit = nearest_neighbor(
  neighbors = tune(),
  weight_func = tune(),
  dist_power = tune()
) %>%
  set_mode("classification") %>%
  set_engine("kknn")


# neste aqui estou fazendo kernel knn:
#
# kknn_fit = nearest_neighbor(neighbors = tune()) %>%
#   set_mode("classification") %>%
#   set_engine("kknn")


# -------------------------------------------------------------------------



wf = workflow_set(
  preproc = list(df_recipe),
  models = list(
    elasticnet = lm_fit,
    reg_tree = rt_fit,
    random_forest = rf_fit,
    linear_svm = svm_fit1,
    poly_svm = svm_fit3,
    kernel_svm = svm_fit2,
    mlp = mlp_fit,
    xg_boost = xgb_fit,
    knn = knn_fit
  )
) %>%
  mutate(wflow_id = gsub("(recipe_)", "", wflow_id))

grid_ctrl = control_grid(
  save_pred = TRUE,
  parallel_over = "resamples",
  save_workflow = TRUE
)

grid_results = wf %>%
  workflow_map(resamples = df_vfolds,
               grid = 10,
               control = grid_ctrl)

grid_results


# ---> Duvida :

# Estou com duvida em como aplicar as demais metricas dentro do
# autoplot (AUC e F1), pois logo em seguida ele faz um comparativo
# com as melhores metricas e eu não sei como aplicar as minhas para
# diferenciar desta forma e ter essa comparação


# -------------------------------------------------------------------------

autoplot(grid_results, metric = "accuracy") +
  labs (title = "Performance de acordo com um esquema de 10-fold cross-validation", x = "Ranking", y = "AUC")
scale_colour_discrete(
  breaks = c(
    "rand_forest",
    "nearest_neighbor",
    "svm_poly",
    "svm_linear",
    "poisson_reg",
    "boost_tree",
    "svm_rbf",
    "decision_tree",
    "mlp"
  ),
  labels = c(
    "Random forest",
    "K-NN",
    "Kernel SVM (polinomial)",
    "SVM",
    "Rede elÃ¡stica Poisson",
    "Boosted trees",
    "Kernel SVM (RBF)",
    "Regression tree",
    "MLP"
  )
) +
  guides(colour = guide_legend(title = "Modelo"), shape = "none")


# Os modelos ajustados com os melhores conjuntos de hiperparametros
# são apresentados na figura a seguir.
# O modelo que apresentou o melhor desempenho foi o **Random forest**.


autoplot(
  grid_results,
  rank_metric = "accuracy",
  metric = "accuracy",
  select_best = TRUE
) +
  labs(title = "Melhor resultado em um esquema de 10-fold cross-validation", x = "Ranking", y = "Acurácia") +
  scale_colour_discrete(
    breaks = c(
      "logistic_reg",
      "decision_tree",
      "rand_forest",
      "svm_linear",
      "svm_poly",
      "svm_rbf",
      "mlp",
      "boost_tree",
      "nearest_neighbor"
    ),
    labels = c(
      "Regressão logística",
      "Decision tree",
      "Random forest",
      "Linear SVM",
      "Polynomial SVM",
      "RBF SVM",
      "MLP",
      "XGBoost",
      "KNN"
    )
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(title = "Modelo"), shape = "none")






# -------------------------------------------------------------------------
best_results_elnet = grid_results %>%
  extract_workflow_set_result("elasticnet") %>%
  select_best(metric = "accuracy")
best_results_rt = grid_results %>%
  extract_workflow_set_result("reg_tree") %>%
  select_best(metric = "accuracy")
best_results_rf = grid_results %>%
  extract_workflow_set_result("random_forest") %>%
  select_best(metric = "accuracy")
best_results_lsvm = grid_results %>%
  extract_workflow_set_result("linear_svm") %>%
  select_best(metric = "accuracy")
best_results_psvm = grid_results %>%
  extract_workflow_set_result("poly_svm") %>%
  select_best(metric = "accuracy")
best_results_ksvm = grid_results %>%
  extract_workflow_set_result("kernel_svm") %>%
  select_best(metric = "accuracy")
best_results_mlp = grid_results %>%
  extract_workflow_set_result("mlp") %>%
  select_best(metric = "accuracy")
best_results_xgb = grid_results %>%
  extract_workflow_set_result("xg_boost") %>%
  select_best(metric = "accuracy")
best_results_knn = grid_results %>%
  extract_workflow_set_result("knn") %>%
  select_best(metric = "accuracy")

best_results_elnet
best_results_rt
best_results_rf
best_results_lsvm
best_results_psvm
best_results_ksvm
best_results_mlp
best_results_xgb
best_results_knn

# -----------------------------------------------------------------------

test_results_elnet = grid_results %>%
  extract_workflow("elasticnet") %>%
  finalize_workflow(best_results_elnet) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_rt = grid_results %>%
  extract_workflow("reg_tree") %>%
  finalize_workflow(best_results_rt) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_rf = grid_results %>%
  extract_workflow("random_forest") %>%
  finalize_workflow(best_results_rf) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_lsvm = grid_results %>%
  extract_workflow("linear_svm") %>%
  finalize_workflow(best_results_lsvm) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_psvm = grid_results %>%
  extract_workflow("poly_svm") %>%
  finalize_workflow(best_results_psvm) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_ksvm = grid_results %>%
  extract_workflow("kernel_svm") %>%
  finalize_workflow(best_results_ksvm) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_mlp = grid_results %>%
  extract_workflow("mlp") %>%
  finalize_workflow(best_results_mlp) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_xgb = grid_results %>%
  extract_workflow("xg_boost") %>%
  finalize_workflow(best_results_xgb) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )
test_results_knn = grid_results %>%
  extract_workflow("knn") %>%
  finalize_workflow(best_results_knn) %>%
  last_fit(
    split = df_split,
    metrics = metric_set(recall, precision, f_meas,
                         accuracy, kap,
                         roc_auc, sens, spec)
  )




# -------------------------------------------------------------------------

# Não rodar, faltou colocou o kernel knn, na hora de rodar o codigo,
# foquei em colocar adiconar a modifcação para conseguir
# obter as variaveis mais importantes
# porém para o senhor conseguir rodar os graficos, a simulação está
# feita


cbind(
  collect_metrics(test_results_elnet)$.estimate,
  collect_metrics(test_results_rf)$.estimate,
  collect_metrics(test_results_rt)$.estimate,
  collect_metrics(test_results_lsvm)$.estimate,
  collect_metrics(test_results_psvm)$.estimate,
  collect_metrics(test_results_ksvm)$.estimate,
  collect_metrics(test_results_mlp)$.estimate,
  collect_metrics(test_results_xgb)$.estimate,
  collect_metrics(test_results_knn)$.estimate
)


# -------------------------------------------------------------------------

rf_pred <- test_results_rf %>%
  collect_predictions()


# -------------------------------------------------------------------------

rf_pred %>%
  conf_mat(Tempo, .pred_class)


# -------------------------------------------------------------------------
rf_pred %>%
  conf_mat(Tempo, .pred_class) %>%
  autoplot()


# -------------------------------------------------------------------------

table(rf_pred$Tempo)
table(rf_pred$.pred_class)


rf_pred %>%
  conf_mat(Tempo, .pred_class) %>%
  autoplot(type = "heatmap")

# -------------------------------------------------------------------------

rf_pred %>%
  roc_curve(Tempo, .pred_0) %>%
  autoplot()

# -------------------------------------------------------------------------

# é necessario colocar dentro do set_engine o importance para o modelo
# rf ?

test_results_rf %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip(exclude = "CLITRATCAT_X6")

test_results_rf$splits

test_results_rf %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip() %>%
  subset(variable != "CLITRATCAT_X6")





test_results_rf %>%
  pluck(".workflow", 1)


# vai ser para salvar tudo :

save(file = "C:/Users/VictorBorges/Desktop/TCC & PIBIC/TCC - PIBIC 2022-2023/Compdemetodostcc.RData")

# trazer de volta o que salvou

load(file = "C:/Users/VictorBorges/Desktop/TCC & PIBIC/TCC - PIBIC 2022-2023/Compdemetodostcc.RData")



# ------------------------------------------------------------------------


test_results_rf %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip::vip(data = ., exclude = "CLITRATCAT_X6")
