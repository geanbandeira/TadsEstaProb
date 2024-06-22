library(tidyverse)
library(psych)
library(skimr)
library(ggplot2)
library(readr)
library(dplyr)
library(MASS)
library(stats)

dados <- read.csv(file.choose(), sep = ";")

skim(dados)

#Analise Estatistica descritiva de todos dados
summary(dados)

##############
# Contando a quantidade de estabelecimento de cada unidade federativa
contagem <- table(dados$CO_UF)
# Resultado
print(contagem)
# Grafico
barplot(contagem, main="Frequência dos Códigos UF", xlab="Código UF", ylab="Frequência", col="blue")
#R: Existem mais Estabelecimentos de Saúde nas unidades federativas 35, 31 e 33.

########
# Contando a quantidade de ocorrências de cada tipo de gestão
contagem_gestao <- table(dados$TP_GESTAO)
# Resultado
print(contagem_gestao)
# Gráfico
barplot(contagem_gestao, 
        main="Frequência dos Tempo de Gestão", 
        xlab="Tempo de Gestão", 
        ylab="Frequência", 
        col="blue", 
        border="black")
#R: Curto Prazo (M - Mensal): Duração: até 1 mês.Curto Prazo (E - Exato): Duração: até 1 ano.
#Prazo (D - Duradouro): Duração: de 1 a 3 anos. Longo Prazo (S - Sustentável): Duração: mais de 3 anos.

######

contagem_gestao <- table(dados$CO_ATIVIDADE)
print(contagem_gestao)
barplot(contagem_gestao, 
        main = "Contagem de Atividades em Estabelecimentos de Saúde",
        xlab = "Atividade",
        ylab = "Quantidade",
        col = "skyblue",
        ylim = c(0, max(contagem_gestao) * 1.2),
        names.arg = c("Emergência", 
                      "Exame", 
                      "Cirurgia", 
                      "Consulta", 
                      "Terapia"),
        las = 2
)

#R: A atividade mais recorrente nos estabelecimentos de saúde é a consulta..

########

dados <- data.frame(
  ST_CENTRO_CIRURGICO = sample(c(0, 1), 530333, replace = TRUE)  # Exemplo com 100 observações aleatórias de 0s e 1s
)

# Contagem de 0s e 1s na variável ST_CENTRO_CIRURGICO
contagem_ctcirurgico <- table(dados$ST_CENTRO_CIRURGICO)

# Exibindo a contagem
print(contagem_ctcirurgico)

# Calculando a probabilidade de ter um centro cirúrgico 
probabilidade <- contagem_ctcirurgico[1] / sum(contagem_ctcirurgico)

# Exibindo a probabilidade
print(paste("A probabilidade de um estabelecimento de saúde ter um centro cirúrgico é:", probabilidade))

###########

dados <- data.frame(
  CO_AMBULATORIAL_SUS = sample(c(0, 1), 530333, replace = TRUE)  # Exemplo com 100 observações aleatórias de 0s e 1s
)

# Contagem de serviços ambulatoriais cobertos pelo SUS (valor 1)
contagem_sus <- sum(dados$CO_AMBULATORIAL_SUS == 1)

# Total de serviços ambulatoriais
total_servicos <- nrow(dados)

# Calculando a porcentagem
porcentagem_sus <- (contagem_sus / total_servicos) * 100

print(paste("A porcentagem de serviços ambulatoriais cobertos pelo SUS é:", porcentagem_sus, "%"))

###########

set.seed(123)  # Para reproduzir os mesmos resultados
dados <- data.frame(
  ST_ATEND_AMBULATORIAL = rnorm(530333, mean = 50, sd = 10),  # Exemplo de atendimentos ambulatoriais
  TP_GESTAO = sample(c("M", "E", "D", "S"), 530333, replace = TRUE)  # Exemplo de tempo de gestão
)

# Definindo os grupos com base no tempo de gestão
grupo_maior <- dados[dados$TP_GESTAO %in% c("D", "S"), "ST_ATEND_AMBULATORIAL"]
grupo_menor <- dados[dados$TP_GESTAO %in% c("M", "E"), "ST_ATEND_AMBULATORIAL"]

# Teste t de Student para duas amostras independentes
teste_t <- t.test(grupo_maior, grupo_menor, alternative = "greater", conf.level = 0.95)

# Resultado 
print(teste_t)

# Conclusão do teste
if (teste_t$p.value < 0.05) {
  print("Podemos rejeitar a hipótese nula.")
  print("Há evidências estatísticas de que a média de atendimentos ambulatoriais é superior em estabelecimentos com tempo de gestão maior.")
} else {
  print("Não podemos rejeitar a hipótese nula.")
  print("Não há evidências suficientes para afirmar que a média de atendimentos ambulatoriais é superior em estabelecimentos com tempo de gestão maior.")
}

##########


# Teste para verificar se a média de pacientes atendidos por estabelecimento é significativamente diferente entre as unidades federativas
t.test(dados$NU_CNPJ ~ dados$CO_ESFERA_ADMINISTRATIVA)


contingency_table <- table(dados$TP_GESTAO, dados$ST_CENTRO_CIRURGICO)

# Realizar o teste qui-quadrado de independência
chi_test <- chisq.test(contingency_table)
chi_test

# Gráficos
ggplot(dados, aes(x = TP_GESTAO, fill = ST_CENTRO_CIRURGICO)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição do Tipo de Gestão por Presença de Centro Cirúrgico",
       x = "Tipo de Gestão",
       y = "Contagem")

##############

# Pergunta 3: Diferença na presença de centro obstétrico entre diferentes esferas administrativas
tabeladecont3 <- table(dados$CO_ESFERA_ADMINISTRATIVA, dados$ST_CENTRO_OBSTETRICO)

# Realizar o teste qui-quadrado de independência
chi_test_3 <- chisq.test(tabeladecont3)

# Mostrar os resultados do teste
chi_test_3



# Pergunta 4: Associação entre tipo de unidade e tipo de gestão
tabeladecont4 <- table(dados$TP_UNIDADE, dados$TP_GESTAO)

# Realizar o teste qui-quadrado de independência
chi_test_4 <- chisq.test(tabeladecont4)

# Resultados do teste
chi_test_4

# Gráficos 
ggplot(dados, aes(x = as.factor(CO_ESFERA_ADMINISTRATIVA), fill = ST_CENTRO_OBSTETRICO)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Centro Obstétrico por Esfera Administrativa",
       x = "Esfera Administrativa",
       y = "Contagem")

# Gráfico 
ggplot(dados, aes(x = TP_UNIDADE, fill = TP_GESTAO)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição do Tipo de Unidade por Tipo de Gestão",
       x = "Tipo de Unidade",
       y = "Contagem")

###########


# Pergunta 5: Associação entre presença de centro obstétrico e turno de atendimento

contingency_table_5 <- table(dados$ST_CENTRO_OBSTETRICO, dados$CO_TURNO_ATENDIMENTO)

# Teste qui-quadrado de independência
chi_test_5 <- chisq.test(contingency_table_5)

# Resultados do teste
chi_test_5

# Gráficos
ggplot(dados, aes(x = as.factor(CO_TURNO_ATENDIMENTO), fill = ST_CENTRO_OBSTETRICO)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Centro Obstétrico por Turno de Atendimento",
       x = "Turno de Atendimento",
       y = "Contagem")

##########




# Pergunta 6: Associação entre atividade principal e natureza jurídica

tabeladecont6 <- table(dados$CO_ATIVIDADE, dados$CO_NATUREZA_JUR)

# Teste qui-quadrado de independência
chi_test_6 <- chisq.test(tabeladecont6)

# Resultados do teste
chi_test_6


ggplot(dados, aes(x = as.factor(CO_ATIVIDADE), fill = as.factor(CO_NATUREZA_JUR))) +
  geom_bar(position = "dodge", width = 0.9) +  # Ajuste a largura das barras
  labs(title = "Distribuição de Atividade Principal por Natureza Jurídica",
       x = "Atividade Principal",
       y = "Contagem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 12))  # Mudei o ângulo para 45 graus

################


# Pergunta 7: Relação entre latitude e presença de serviço de apoio

# Realizar um teste t para comparar as latitudes médias
t_test_7 <- t.test(dados$NU_LATITUDE ~ dados$ST_SERVICO_APOIO, dados = data_7)

# Mostrar os resultados do teste t
t_test_7


# Gráficos 
ggplot(dados, aes(x = NU_LATITUDE, fill = as.factor(ST_SERVICO_APOIO))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribuição de Latitude por Presença de Serviço de Apoio",
       x = "Latitude",
       y = "Densidade")

######### 


# Pergunta 9: Normalidade da distribuição das longitudes
data_9 <- dados %>% dplyr::select(NU_LONGITUDE) %>% na.omit()

# Verificar o tamanho da amostra
n <- nrow(data_9)

set.seed(123) # Para reprodutibilidade
if (n > 5000) {
  data_9 <- data_9 %>% sample_n(5000)
}

# Realizar o teste de Shapiro-Wilk
shapiro_test_9 <- shapiro.test(data_9$NU_LONGITUDE)

# Mostrar os resultados do teste de Shapiro-Wilk
print(shapiro_test_9)

# Histograma e Q-Q plot para verificar a normalidade
ggplot(data_9, aes(x = NU_LONGITUDE)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma e Curva de Densidade de NU_LONGITUDE",
       x = "Longitude",
       y = "Densidade")

# Q-Q plot
ggplot(data_9, aes(sample = NU_LONGITUDE)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot de NU_LONGITUDE",
       x = "Quantis Teóricos",
       y = "Quantis Amostrais")


