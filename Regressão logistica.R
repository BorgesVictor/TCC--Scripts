rm(list = ls())

library(readxl)
library(sampling)
library(caret)
library(dplyr)
library(magrittr)


# Modelo Geral (Realizado de forma manual e analisando o p-valor):

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


df = BancoRegressaoOficial
#df<-df[c(-4,-5,-10)]
df <- na.omit(df)

# aparecendo um "i" antes e incomoda !!

names(df)
colnames(df)[13] <- "IDHEDUCACAO"


# -------------------------------------------------------------------------

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



# --> Separar 10% do conjunto dos dados para teste, para eu poder ter uma matrix de confusão e curva roc em um conjunto independente.

# --> Amostragem estratificada pela variavel

# resposta e por estado (isso para pegar todos os estados e todas classes)



# -------------------------------------------------------------------------


# Etapa 1 : Separar o banco para treinamento e teste :

sample <-
  sample(c(TRUE, FALSE),
         nrow(df),
         replace = TRUE,
         prob = c(0.7, 0.3))
train  <- df[sample,]
test   <- df[!sample,]

# modelo geral

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar o maior p-valor:

train$RACACOR[which(train$RACACOR[] == 2)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor rznt 4:

train$RZNTR[which(train$RZNTR[] == 4)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor raça cor 5:


train$RACACOR[which(train$RACACOR[] == 5)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar o maior p-valor tipo histologico 3:


train$TIPOHISTCATEG[which(train$TIPOHISTCATEG[] == 3)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor rznt 4:

train$RZNTR[which(train$RZNTR[] == 6)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor raça cor 4:


train$RACACOR[which(train$RACACOR[] == 4)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor raça cor :


fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar o maior p-valor rznt 5:

train$RZNTR[which(train$RZNTR[] == 5)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar despesa

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor região 4:

train$REGIÃOCATEG[which(train$REGIÃOCATEG[] == 4)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar longevidade

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar prita 9

train$PRITRATH[which(train$PRITRATH[] == 9)] = 2

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor rznt 3:

train$RZNTR[which(train$RZNTR[] == 3)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tira renda id

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar o maior p-valor rznt 8:

train$RZNTR[which(train$RZNTR[] == 8)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor rznt 8:

train$CLITRATCAT[which(train$CLITRATCAT[] == 4)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar região 2



train$REGIÃOCATEG[which(train$REGIÃOCATEG[] == 2)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar região 3



train$REGIÃOCATEG[which(train$REGIÃOCATEG[] == 3)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar rznt 7



train$RZNTR[which(train$RZNTR[] == 7)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# tirar solido

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

(fit)

# tirar pritath 7

train$PRITRATH[which(train$PRITRATH[] == 7)] = 2

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# tirar o maior p-valor 3 clitra:

train$CLITRATCAT[which(train$CLITRATCAT[] == 3)] = 1

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# modelo final

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) +
    IDHEDUCACAO + PIBPERC +
    NMEDICOSPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)


# -------------------------------------------------------------------------



library(pROC)
roc1 = plot.roc(fit$y, fitted(fit))
plot(
  roc1,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grud = c(0.1, 0.2),
  grid.col = c("green", "red"),
  max.auc.polygon = TRUE,
  auc.polygon.col = "lightgreen",
  print.thres = TRUE,
  xlab = "Especificidade",
  ylab = "Sensibilidade"
)

optimal_lr.eta = function(x) {
  no = which.max(x$sensitivities + x$specificities)[1]
  result = x$thresholds[no]
  result
}

cutpoint <- optimal_lr.eta(roc1)

library(caret)

# Matriz de confusão :

y_chapeu <-
  as.numeric(na.omit(ifelse(fit$fitted.values > cutpoint, "1", "0")))
y <- as.numeric(fit$y)
confusionMatrix(as.factor(y_chapeu), as.factor(y), positive = "1")

round(prop.table(table(train$Tempo)), 2)

exp(fit$coefficients)

# -------------------------------------------------------------------------

# Modelo final

fit$deviance
# neste caso eu estou pegando o valor deviance que seria o residuo do meu modelo

qchisq(0.95, fit$df.residual)
s = summary(fit)
#___________________________________________________
s$deviance
#______________________________________________
s$dispersion
#_______________________________________

# A distÃ¢ncia da minha funÃ§Ã£o de (log-verossimilhaÃ§a), sendo uma com os valores
# do banco e a outra com valores estimados.
# mais adequada para cÃ¡lculos computacionais
# e permite que modelos possam ser comparados aditivamente,
# ao invÃ©s de multiplicativamente.
# Ã© o estimador de mÃ¡xima verossimilhanÃ§a de Î¸.

desvio = s$deviance / s$dispersion
q.quadr = qchisq(0.95, s$df.residual)
q.quadr
desvio < q.quadr



# -------------------------------------------------------------------------



# Nesta etapa foi feito o modelo geral de novo só que com metodo de seleção
# setpwise, porém os resultados do modelo geral serão considerados os que estão
# no codigo acima

# Start 2 : Fase

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

# aparecendo um "i" antes e incomoda !!

names(df)
colnames(df)[13] <- "IDHEDUCACAO"

# Olhada rapida no banco de dados

summary(df)


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

# -------------------------------------------------------------------------



# --> Separar 10% do conjunto dos dados para teste, para eu poder ter uma matrix de confus?o e curva roc em um conjunto independente.

# --> Amostragem estratificada pela variavel

# resposta e por estado (isso para pegar todos os estados e todas classes)



# -------------------------------------------------------------------------


# Etapa 1 : Separar o banco para treinamento e teste :

sample <-
  sample(c(TRUE, FALSE),
         nrow(df),
         replace = TRUE,
         prob = c(0.7, 0.3))
train  <- df[sample,]
test   <- df[!sample,]

table(test$REGIÃOCATEG)
table(test$Tempo)


round(prop.table(table(train$Tempo)), 2)

# -------------------------------------------------------------------------



# Valida??o da Variavel Númerica com a Vari?vel resposta (Binaria)

# lm(df$DTDIAG_DTINITRACAT~df$IDHEDUCACAO)
# plot(df$IDHEDUCACAO,df$DTDIAG_DTINITRACAT)
# plot(df$IDHM,df$DTDIAG_DTINITRACAT)
# plot(df$PIB,df$DTDIAG_DTINITRACAT)



# Todos nomes : -------------------------------------------------------------------------

# Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
#   as.factor(INSTRUC) + as.factor(EXDIAG) + as.factor(CLITRATCAT) +
#   as.factor(LOCTUDETCAT) + as.factor(SOLIDOHEMATO) +
#   as.factor(DESLOCAMENTOTRATAMENTO) + as.factor(REGI?OCATEG) +
#   IDHM + IDHMRenda + IDHEDUCACAO + IDHMLongevidade + PIB +
#   NMEDICOS + DESPESAANUAL + ESTIMATIVAPOP


# -------------------------------------------------------------------------




# -------------------------------------------------------------------------



# Adi??o das Variaveis do modelo:

fit = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = train,
  family = binomial(link = logit)
)

summary(fit)

# Fazendo o mesmo caso para gerar o banco de dados do test :

fit_teste = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = test,
  family = binomial(link = logit)
)

summary(fit)



# -------------------------------------------------------------------------

# Banco com as variaveis dummy usando o banco test:

test_dummy = data.frame(Tempo = test$Tempo, as.matrix(model.matrix(fit_teste)[, -1]))
# -------------------------------------------------------------------------



# Biblioteca para usar o caret :

library(MASS)

# Modelo de regress?o rodado com o metodo setpwise

fit.step = stepAIC(fit, k = log(nrow(df)))
summary(fit.step)


df2 = data.frame(Tempo = train$Tempo, as.matrix(model.matrix(fit)[, -1]))

# Rodar o modelo agora com o banco com as vari?veis dummy criadas :

fit.step = stepAIC(fit, k = log(nrow(df2)))
summary(fit.step)

# Agora avaliar o modelo gerado :

library(pROC)
roc1 = plot.roc(fit.step$y, fitted(fit.step))
plot(
  roc1,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grud = c(0.1, 0.2),
  grid.col = c("green", "red"),
  max.auc.polygon = TRUE,
  auc.polygon.col = "lightgreen",
  print.thres = TRUE,
  xlab = "Especificidade",
  ylab = "Sensibilidade"
)

optimal_lr.eta = function(x) {
  no = which.max(x$sensitivities + x$specificities)[1]
  result = x$thresholds[no]
  result
}

cutpoint <- optimal_lr.eta(roc1)

library(caret)

# Matriz de confus?o :

y_chapeu <-
  as.numeric(na.omit(ifelse(
    fit.step$fitted.values > cutpoint, "1", "0"
  )))
y <- as.numeric(fit.step$y)
confusionMatrix(as.factor(y_chapeu), as.factor(y), positive = "1")


# Utilizando o banco de teste :


Teste <- predict(fit.step, test, type = "response")
Teste



# Balanceamento das vari?veis para conseguir ter um banco mais
# estruturado :


df20 = df2[df2$Tempo == 0, ]
df21 = df2[df2$Tempo == 1, ]


n_maioria_necessario <- round(0.3 * nrow(df21))

dim(df20)
dim(df21)
table(df2$Tempo)

? sample

# -------------------------------------------------------------------------
downsmp = sample(1:nrow(df20), n_maioria_necessario, replace = FALSE)

df3 = rbind(df20[downsmp, ], df21)
dim(df3)
2 * dim(df21)

# Confirmar para saber se est?o balanceadas :

round(prop.table(table(df3$Tempo)), 2)

# Pegar os nomes do df3 :

fmla3 = paste0(names(df3)[ncol(df3[1])], "~", paste0(names(df3[, -ncol(df3[1])]), collapse = "+"))

# -------------------------------------------------------------------------

fitdownsp = glm(fmla3, data = df3, family = binomial(link = logit))
summary(fitdownsp)

# rodar o modelo balanceado :
# Obs (demorou muito para conseguir rodar)

fit.step.downsp = stepAIC(fitdownsp, k = log(nrow(df3)))
summary(fit.step.downsp)

# avaliar o modelo rodado :

library(pROC)
roc1 = plot.roc(fit.step.downsp$y, fitted(fit.step.downsp))
plot(
  roc1,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grud = c(0.1, 0.2),
  grid.col = c("green", "red"),
  max.auc.polygon = TRUE,
  auc.polygon.col = "lightgreen",
  print.thres = TRUE,
  xlab = "Especificidade",
  ylab = "Sensibilidade"
)

optimal_lr.eta = function(x) {
  no = which.max(x$sensitivities + x$specificities)[1]
  result = x$thresholds[no]
  result
}

cutpoint <- optimal_lr.eta(roc1)

library(caret)
y_chapeu <-
  as.numeric(na.omit(ifelse(
    fit.step.downsp$fitted.values > cutpoint, "1", "0"
  )))
y <- as.numeric(fit.step.downsp$y)
confusionMatrix(as.factor(y_chapeu), as.factor(y), positive = "1")

# Razão de chance
exp(fit.step.downsp$coefficients)
#exp(cbind(OR=coef(modelo), confint(modelo)))


# Refazendo a amostragem agora com up:



n_minoria_necessario <- round(0.28 * nrow(df20))


upsmp = sample(1:nrow(df21), n_minoria_necessario, replace = TRUE)

df4 = rbind(df20, df21[upsmp, ])
dim(df4)
2 * dim(df20)

# Confirmar para saber se est?o balanceadas :

round(prop.table(table(df4$Tempo)), 2)


# -------------------------------------------------------------------------

fitupsmp = glm(fmla3, data = df4, family = binomial(link = logit))
summary(fitupsmp)

fit.step.upsmp = stepAIC(fitupsmp, k = log(nrow(df4)))
summary(fit.step.upsmp)

library(pROC)
roc1 = plot.roc(fit.step.upsmp$y, fitted(fit.step.upsmp))
plot(
  roc1,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grud = c(0.1, 0.2),
  grid.col = c("green", "red"),
  max.auc.polygon = TRUE,
  auc.polygon.col = "lightgreen",
  print.thres = TRUE,
  xlab = "Especificidade",
  ylab = "Sensibilidade"
)

optimal_lr.eta = function(x) {
  no = which.max(x$sensitivities + x$specificities)[1]
  result = x$thresholds[no]
  result
}

cutpoint <- optimal_lr.eta(roc1)

library(caret)
y_chapeu <-
  as.numeric(na.omit(ifelse(
    fit.step.upsmp$fitted.values > cutpoint, "1", "0"
  )))
y <- as.numeric(fit.step.upsmp$y)
confusionMatrix(as.factor(y_chapeu), as.factor(y), positive = "1")

# Raz?o chance
exp(fit.step.upsmp$coefficients)
#exp(cbind(OR=coef(modelo), confint(modelo)))

# Teste bondade de ajuste :


# Modelo final

fit.step$deviance
# neste caso eu estou pegando o valor deviance que seria o residuo do meu modelo

qchisq(0.95, fit.step$df.residual)
s = summary(fit.step)
#___________________________________________________
s$deviance
#______________________________________________
s$dispersion
#_______________________________________

# A distância da minha função de (log-verossimilhaça), sendo uma com os valores
# do banco e a outra com valores estimados.
# mais adequada para cálculos computacionais
# e permite que modelos possam ser comparados aditivamente,
# ao invés de multiplicativamente.
# é o estimador de máxima verossimilhança de θ.

desvio = s$deviance / s$dispersion
q.quadr = qchisq(0.95, s$df.residual)
q.quadr
desvio < q.quadr



# -------------------------------------------------------------------------

library(ResourceSelection)

hoslem.test(fit.step$y, fitted(fit.step), g = 10)
? hoslem.test


# Modelo final com Down

fit.step.downsp$deviance
# neste caso eu estou pegando o valor deviance que seria o residuo do meu modelo

qchisq(0.95, fit.step.downsp$df.residual)
s = summary(fit.step.downsp)
#___________________________________________________
s$deviance
#______________________________________________
s$dispersion
#_______________________________________

# A distância da minha função de (log-verossimilhaça), sendo uma com os valores
# do banco e a outra com valores estimados.
# mais adequada para cálculos computacionais
# e permite que modelos possam ser comparados aditivamente,
# ao invés de multiplicativamente.
# é o estimador de máxima verossimilhança de θ.

desvio = s$deviance / s$dispersion
q.quadr = qchisq(0.95, s$df.residual)
q.quadr
desvio < q.quadr


# Modelo final com UPSMP
# Teste de bondade de ajuste :

fit.step.upsmp$deviance
# neste caso eu estou pegando o valor deviance que seria o residuo do meu modelo

qchisq(0.95, fit.step.upsmp$df.residual)
s = summary(fit.step.upsmp)
#___________________________________________________
s$deviance
#______________________________________________
s$dispersion
#_______________________________________

# A distância da minha função de (log-verossimilhaça), sendo uma com os valores
# do banco e a outra com valores estimados.
# mais adequada para cálculos computacionais
# e permite que modelos possam ser comparados aditivamente,
# ao invés de multiplicativamente.
# é o estimador de máxima verossimilhança de θ.

desvio = s$deviance / s$dispersion
q.quadr = qchisq(0.95, s$df.residual)
q.quadr
desvio < q.quadr


# -----------------------------------------------------------------------

# A partir daqui é pego o resultado do modelo geral sendo realizado de forma
# manual e os demais modelos (dowsamplig e upsamplig)

# Realizar o teste hosmer e lemeshow :

library(ResourceSelection)

# A hip?tese nula H0 do qui-quadrado (p=0,05) deste teste ? a de que
# as propor??es observadas e esperadas s?o as mesmas ao longo da amostra.
# Abaixo segue a estrutura do teste, sendo que o modelo apresenta
# dificuldade de ajuste em fun??o de que rejeita a hip?tese nula
# a p=0,05.


# Normal :

hoslem.test(fit$y, fitted(fit), g = 10)


# Pseudo R^2:

require(modEvA)
RsqGLM(m1)


as.factor(SEXO)2
as.factor(FAIXA_ETARIA)2
as.factor(FAIXA_ETARIA)3
as.factor(FAIXA_ETARIA)4
as.factor(CLITRATCAT)2
as.factor(CLITRATCAT)5
as.factor(CLITRATCAT)6
as.factor(TIPOHISTCATEG)2
as.factor(TIPOHISTCATEG)4
as.factor(TIPOHISTCATEG)5
as.factor(TIPOHISTCATEG)6
as.factor(TIPOHISTCATEG)7
as.factor(TIPOHISTCATEG)8
as.factor(TIPOHISTCATEG)9
as.factor(TIPOHISTCATEG)10
as.factor(TIPOHISTCATEG)11
as.factor(TIPOHISTCATEG)12
as.factor(TIPOHISTCATEG)13
as.factor(TIPOHISTCATEG)14
as.factor(TIPOHISTCATEG)15
as.factor(RZNTR)2
as.factor(REGI ? OCATEG)5
as.factor(PRITRATH)3
as.factor(PRITRATH)4
as.factor(PRITRATH)5
as.factor(PRITRATH)6
as.factor(PRITRATH)8
IDHEDUCACAO
PIBPERC
NMEDICOSPERC

# Predi??o dos modelos com banco de teste :


fit$data$PRITRATH %>% as.factor() %>% levels()
test_agrupar$PRITRATH %>% as.factor () %>% levels()
test$CLITRATCAT %>% levels()
Fit$data$clittratcat

test_agrupar <- test

test_agrupar$FAIXA_ETARIA[which(test_agrupar$FAIXA_ETARIA[] == 5)] = 1
test_agrupar$CLITRATCAT[which(test_agrupar$CLITRATCAT[] == 4)] = 1
test_agrupar$CLITRATCAT[which(test_agrupar$CLITRATCAT[] == 3)] = 1
test_agrupar$TIPOHISTCATEG[which(test_agrupar$TIPOHISTCATEG[] == 3)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 3)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 4)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 5)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 6)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 7)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 8)] = 1
test_agrupar$REGI ? OCATEG[which(test_agrupar$REGI ? OCATEG[] == 2)] = 1
test_agrupar$REGI ? OCATEG[which(test_agrupar$REGI ? OCATEG[] == 3)] = 1
test_agrupar$REGI ? OCATEG[which(test_agrupar$REGI ? OCATEG[] == 4)] = 1
test_agrupar$PRITRATH[which(test_agrupar$PRITRATH[] == 7)] = 2
test_agrupar$PRITRATH[which(test_agrupar$PRITRATH[] == 9)] = 2



summary(fit)
names(test)
Newdatatestnormal <-
  test_agrupar %>% dplyr::select(
    SEXO,
    FAIXA_ETARIA,
    CLITRATCAT,
    TIPOHISTCATEG,
    RZNTR,
    REGI ? OCATEG,
    PRITRATH,
    IDHEDUCACAO,
    PIBPERC,
    NMEDICOSPERC
  )

Teste_normal <-
  predict(fit, Newdatatestnormal, interval = "prediction")

fit$data$SEXO



Classification_teste_normal <- ifelse(Teste_normal >= 0.1524926, 1, 0)
table(Classification_teste_normal)
table(test$Tempo)




# Downsample :


hoslem.test(fit.step.downsp$y, fitted(fit.step.downsp), g = 10)

# Pseudo R^2:

require(modEvA)
RsqGLM(m1)


# Predi??o dos modelos com banco de teste :

summary(fit.step.downsp)

as.factor.FAIXA_ETARIA.4
as.factor.CLITRATCAT.2
as.factor.TIPOHISTCATEG.4
as.factor.TIPOHISTCATEG.6
as.factor.TIPOHISTCATEG.7
as.factor.TIPOHISTCATEG.8
as.factor.TIPOHISTCATEG.9
as.factor.TIPOHISTCATEG.10
as.factor.TIPOHISTCATEG.11
as.factor.TIPOHISTCATEG.12
as.factor.TIPOHISTCATEG.13
as.factor.TIPOHISTCATEG.14
as.factor.TIPOHISTCATEG.15
as.factor.REGI ? OCATEG.5
as.factor.PRITRATH.3

Newdatatestdown <-
  test_dummy %>% dplyr::select(
    as.factor.FAIXA_ETARIA.4,
    as.factor.CLITRATCAT.2,
    as.factor.TIPOHISTCATEG.4,
    as.factor.TIPOHISTCATEG.6,
    as.factor.TIPOHISTCATEG.7,
    as.factor.TIPOHISTCATEG.8,
    as.factor.TIPOHISTCATEG.9,
    as.factor.TIPOHISTCATEG.10,
    as.factor.TIPOHISTCATEG.11,
    as.factor.TIPOHISTCATEG.12,
    as.factor.TIPOHISTCATEG.13,
    as.factor.TIPOHISTCATEG.14,
    as.factor.TIPOHISTCATEG.15,
    as.factor.REGI ? OCATEG.5,
    as.factor.PRITRATH.3
  )


Teste_down <-
  predict(fit.step.downsp, Newdatatestdown, interval = "prediction")

Classification_teste_down <- ifelse(Teste_down >= 0.7348578
                                    , 1, 0)
table(Classification_teste_down)
table(test_dummy$Tempo)

# Upsample:


hoslem.test(fit.step.upsmp$y, fitted(fit.step.upsmp), g = 10)

# Pseudo R^2:

require(modEvA)
RsqGLM(m1)


# Predi??o dos modelos com banco de teste :


summary(fit.step.upsmp)

Newdatatestupsamp <- test_dummy %>% dplyr::select(
  as.factor.SEXO.2,
  as.factor.FAIXA_ETARIA.4,
  as.factor.CLITRATCAT.2,
  
  as.factor.CLITRATCAT.5,
  as.factor.CLITRATCAT.6,
  
  as.factor.TIPOHISTCATEG.4,
  
  as.factor.TIPOHISTCATEG.6,
  as.factor.TIPOHISTCATEG.8,
  as.factor.TIPOHISTCATEG.9,
  as.factor.TIPOHISTCATEG.10,
  as.factor.TIPOHISTCATEG.11,
  as.factor.TIPOHISTCATEG.12,
  as.factor.TIPOHISTCATEG.13,
  as.factor.TIPOHISTCATEG.14,
  as.factor.TIPOHISTCATEG.15,
  as.factor.RZNTR.2,
  as.factor.REGIÃOCATEG.5,
  as.factor.PRITRATH.3,
  as.factor.PRITRATH.5,
  as.factor.PRITRATH.6,
  as.factor.PRITRATH.8,
  IDHEDUCACAO,
  PIBPERC,
  NMEDICOSPERC
)



Teste_upsamp <-
  predict(fit.step.upsmp, Newdatatestupsamp, interval = "prediction")

Classification_teste_upsamp <- ifelse(Teste_upsamp >= 0.2767278, 1, 0)
table(Classification_teste_upsamp)
table(test_dummy$Tempo)


# -------------------------------------------------------------------------

# METODO PARA TESTAR SMOTE

rm(list = ls())

library(readxl)
library(sampling)
library(caret)
library(dplyr)
library(magrittr)
library(performanceEstimation)



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

# aparecendo um "i" antes e incomoda !!

names(df)
colnames(df)[13] <- "IDHEDUCACAO"

# Olhada rapida no banco de dados

summary(df)


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

# -------------------------------------------------------------------------



# --> Separar 10% do conjunto dos dados para teste, para eu poder ter uma matrix de confus?o e curva roc em um conjunto independente.

# --> Amostragem estratificada pela variavel

# resposta e por estado (isso para pegar todos os estados e todas classes)



# -------------------------------------------------------------------------


# Etapa 1 : Separar o banco para treinamento e teste :

sample <-
  sample(c(TRUE, FALSE),
         nrow(df),
         replace = TRUE,
         prob = c(0.7, 0.3))
train  <- df[sample,]
test   <- df[!sample,]

table(test$REGIÃOCATEG)
table(test$Tempo)


round(prop.table(table(train$Tempo)), 2)


# VOU UTILIZAR O BANCO DE TRAINAMENTO PARA FAZER O METODO SMOTE:

# O smote é uma técnica de oversampling que gera novos exemplos sintéticos da classe
# minoritária a partir de exemplos existentes.
# Esses novos exemplos são gerados utilizando interpolação
# entre exemplos da classe minoritária próximos uns dos outros.

# O argumento Tempo ~ . especifica a fórmula para o modelo que
# será usado na geração dos novos exemplos sintéticos.
# Nesse caso, a classe minoritária é representada pela variável
# Tempo e o ~ . significa que todas as outras variáveis no conjunto
# de dados de treinamento serão usadas para gerar novos exemplos
# sintéticos.

# Os argumentos perc.over = 10 e perc.under = 1.65 especificam
# os parâmetros para a geração de exemplos sintéticos.

# (A geração de exemplos sintéticos é uma técnica de oversampling
# usada para lidar com desequilíbrios
# de classe em conjuntos de dados de treinamento.
# Em resumo, essa técnica cria novos exemplos da classe
# minoritária a partir de exemplos existentes, utilizando
# interpolação entre exemplos próximos uns dos outros.)

# perc.over define a porcentagem de aumento que será aplicado
# na classe minoritária, enquanto perc.under define a
# porcentagem de redução que será aplicada na classe majoritária
# antes da geração de novos exemplos sintéticos.
# Nesse caso, a classe minoritária será aumentada
# em 10% e a classe majoritária será reduzida em 1.65%.

prop.table(table(train$Tempo))

new_df <- smote(Tempo ~ ., train, perc.over = 10, perc.under = 1.65)

help(smote)


help("smote")


round(prop.table(table(new_df$Tempo)), 2)

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)


# -------------------------------------------------------------------------

# seleção

new_df$RZNTR[which(new_df$RZNTR[] == 5)] = 1

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)


new_df$RZNTR[which(new_df$RZNTR[] == 4)] = 1

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)

new_df$RZNTR[which(new_df$RZNTR[] == 3)] = 1

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)


new_df$CLITRATCAT[which(new_df$CLITRATCAT[] == 4)] = 1

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)

new_df$RACACOR[which(new_df$RACACOR[] == 4)] = 1

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)

new_df$RZNTR[which(new_df$RZNTR[] == 6)] = 1

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)

new_df$PRITRATH[which(new_df$PRITRATH[] == 7)] = 2

fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC +
    NMEDICOSPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)


fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)


new_df$TIPOHISTCATEG[which(new_df$TIPOHISTCATEG[] == 3)] = 1


fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)

new_df$RACACOR[which(new_df$RACACOR[] == 3)] = 1


fit_smote = glm(
  Tempo ~ as.factor(SEXO) + as.factor(RACACOR) + as.factor(FAIXA_ETARIA) +
    as.factor(CLITRATCAT) + as.factor(SOLIDOHEMATO) + as.factor(TIPOHISTCATEG) +
    as.factor(RZNTR) + as.factor(REGIÃOCATEG)  + as.factor(PRITRATH) + IDHMRenda +
    IDHEDUCACAO + IDHMLongevidade + PIBPERC + DESPESANUALPERC,
  data = new_df,
  family = binomial(link = logit)
)

summary(fit_smote)


# -------------------------------------------------------------------------


library(pROC)
roc1 = plot.roc(fit_smote$y, fitted(fit_smote))
plot(
  roc1,
  print.auc = TRUE,
  auc.polygon = TRUE,
  grud = c(0.1, 0.2),
  grid.col = c("green", "red"),
  max.auc.polygon = TRUE,
  auc.polygon.col = "lightgreen",
  print.thres = TRUE,
  xlab = "Especificidade",
  ylab = "Sensibilidade"
)

optimal_lr.eta = function(x) {
  no = which.max(x$sensitivities + x$specificities)[1]
  result = x$thresholds[no]
  result
}

cutpoint <- optimal_lr.eta(roc1)

library(caret)

# Matriz de confus?o :

y_chapeu <-
  as.numeric(na.omit(ifelse(
    fit_smote$fitted.values > cutpoint, "1", "0"
  )))
y <- as.numeric(fit_smote$y)
confusionMatrix(as.factor(y_chapeu), as.factor(y), positive = "1")

table(y_chapeu)
table(y)


# -------------------------------------------------------------------------


# regularizar o banco de dados :


test_agrupar <- test

test$CLITRATCAT %>% levels()

test_agrupar$RACACOR %>% levels()

summary(fit_smote)



test_agrupar$RACACOR[which(test_agrupar$RACACOR[] == 3)] = 1
test_agrupar$RACACOR[which(test_agrupar$RACACOR[] == 4)] = 1
test_agrupar$FAIXA_ETARIA[which(test_agrupar$FAIXA_ETARIA[] == 5)] = 1
test_agrupar$CLITRATCAT[which(test_agrupar$CLITRATCAT[] == 4)] = 1
test_agrupar$TIPOHISTCATEG[which(test_agrupar$TIPOHISTCATEG[] == 3)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 3)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 4)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 5)] = 1
test_agrupar$RZNTR[which(test_agrupar$RZNTR[] == 6)] = 1
test_agrupar$PRITRATH[which(test_agrupar$PRITRATH[] == 7)] = 2




summary(fit_smote)
names(test)
Newdatatestnormal <-
  test_agrupar %>% dplyr::select(
    SEXO,
    RACACOR,
    FAIXA_ETARIA,
    CLITRATCAT,
    SOLIDOHEMATO,
    TIPOHISTCATEG,
    RZNTR,
    REGIÃOCATEG,
    PRITRATH,
    IDHMRenda,
    IDHEDUCACAO,
    IDHMLongevidade,
    PIBPERC,
    DESPESANUALPERC
  )

Teste_normal <-
  predict(fit_smote, Newdatatestnormal, interval = "prediction")

fit$data$SEXO



Classification_teste_normal <- ifelse(Teste_normal >= 0.3969766, 1, 0)
table(Classification_teste_normal)
table(test$Tempo)

table(Classification_teste_normal, test$Tempo)

# -------------------------------------------------------------------------


# Raz?o chance
exp(fit_smote$coefficients)
