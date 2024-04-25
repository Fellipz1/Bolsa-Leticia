# PEG 8000, diferença entre 2%, 3% e 6%

###*******************************************************************###
### Pacotes
library(ggpubr)

###*******************************************************************###
## Planejamento e resultados

# dados de ganho de massa
espessura_poros2 <- c(7.583, 9.6)
espessura_poros3 <- c(12.83, 8.52)
espessura_poros6 <- c(10.2, 7.06)

a <- 3 # numero de tratamentos (niveis)
n <- length(espessura_poros2) # numero de replicas
N <- a*n # numero de experimentos

# ordem de condução dos ensaios
ordem <- sample(1:N, size = N, replace = F)

# tratamentos/niveis
diferencas <- c(rep("E_P2", n),
                rep("E_P3",n),
                rep("E_P6",n))

# transformando em fator(tipo de variavel do R)
diferencas <- as.factor(diferencas)

# vetor de resultados
delta <- c(espessura_poros2, espessura_poros3, espessura_poros6)

# Planejamento totalmente aleatorizado
planejamento <- data.frame(ordem, diferencas, delta)

###************************************************************###
## Graficos

ggboxplot(planejamento,
          x = "diferencas",
          y = "delta",
          color = "diferencas",
          add = "jitter") + theme_bw ()

ggline(planejamento,
       x = "diferencas",
       y = "delta",
       color = "coral3",
       add = c("mean_ci", "jitter")) + theme_bw ()

###************************************************************###
## ANOVA via comando aov

# ANOVA
res.anova <- aov(delta ~ diferencas, data = planejamento)
summary(res.anova)

# F critico
f_tab <- qf(0.05, df1 = a-1, df2 = N-a, lower.tail = F)
f_tab

# Ajuste do modelo
lml <- lm(delta ~ diferencas, data = planejamento)
summary(lml)

###************************************************************###
## Regiao Critica

# sequencia para o eixo horizontal
x <- seq(from = 0, to = 6, length = 500)

# atribuindo valores da distribuição F
y <- df(x,a-1,a*n-a)

# plotando a fdp F
plot(x, y, type = "l", ylab = "densidade")

# valor F de interesse
f <- qf(0.05, a-1, a*n-1, lower.tail = F)

# destacando a probabilidade à direita do valor de interesse
x1 <- seq(f, 10, length=500)
y1 <- df(x1, a-1, a*n-a)
polygon(c(f,x1,10),c(0,y1,0),col="red")

###************************************************************###
## Pressuposicoes

# normalidade
shapiro.test(res.anova$residuals)

# homocedasticidade
bartlett.test(delta ~ diferencas, data = planejamento)

# graficos de residuos
par(mfrow=c(2,2))
plot(res.anova)
par(mfrow=c(1,1))

###************************************************************###
## Teste para comparacoes multiplas

# Teste de Tukey-HSD
TukeyHSD(res.anova)

plot(TukeyHSD(res.anova))

###************************************************************###
## Poder do teste 

# variancia entre os tratamentos
var_rev <- summary(res.anova)[[1]][1,3]

# variancia dentro dos tratamentos (erro experimental)
var_erro <- summary(res.anova)[[1]][2,3]

# poder do teste 
power.anova.test(groups = a,
                 n = n,
                 between.var = var_rev,
                 within.var = var_erro)