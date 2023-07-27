# 1. Setup ---------------------------------------------------------------------

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Set Working Directory - Define o diretório de trabalho.
# Insira o caminho entre aspas e com duas barras \\ como no caminho abaixo
# Caso tenha aberto pelo Rproject, este passo não é necessário.

# setwd("C:caminho_do_diretório\\arquivo")

# Get Working Directory - ver em qual diretório está trabalhando
getwd()

# Verificando arquivos e pastas no diretório
dir()

#Pacotes

# Rotina no R: instalação e carregamento de pacotes
install.packages("pacman")
library(pacman)

# Instalar e carregar a biblioteca ggplot2 (caso ainda não esteja instalada)
install.packages("ggplot2")
library(ggplot2)

install.packages("knitr")
install.packages("kableExtra")

# Simplificação da rotina com o pacote "pacman"
# p_load: função que instala e carrega pacotes

p_load(tidyverse,ggplot2,rio,psych,descr,patchwork,sjPlot)


# 2. Importar base de dados ENEM 2011 ----------------------------------------------------

# Enem 2011 br
sample_enem2011_br <- import("C:/Users/11842581/Downloads/materiaisss/microdados_enem_2011/DADOS/MICRODADOS_ENEM_2011.csv")

#2.1 Importar base de dados ENEM 2014

# Enem 2014 br 
sample_enem2014_br <- import("C:/Users/11842581/Downloads/materiaisss/microdados_enem_2014/DADOS/MICRODADOS_ENEM_2014.csv")


# 3. Manuseio dos dados de 2011 --------------------------------------------------------

# Novo objeto para manuseio (recomendado para evitar ficar carregando originais)
mydata2011 <- sample_enem2011_br

# Verificando nome das variáveis
names(mydata2011)

# Estrutura dos dados
str(mydata2011)


# 3.1 Manuseio dos dados de 2014 --------------------------------------------------------

# Novo objeto para manuseio (recomendado para evitar ficar carregando originais)
mydata2014 <- sample_enem2014_br

# Verificando nome das variáveis
names(mydata2014)

# Estrutura dos dados
str(mydata2014)


# 4 Seleção das variáveis Enem 2011
mydata2011 <- subset(mydata2011,
                     select = c(TP_COR_RACA, TP_SEXO, TP_FAIXA_ETARIA,
                                TP_DEPENDENCIA_ADM_ESC,
                                TP_ESCOLA,
                                Q001, Q002, Q003, Q004)
)

# Renomear variáveis ENEM 2011 - RENAME

mydata2011 <- rename(.data = mydata2011,
                     
                     "raca"=TP_COR_RACA,
                     "sexo"=TP_SEXO,
                     "faixaEtaria"=TP_FAIXA_ETARIA,
                     "depAdminEsc"=TP_DEPENDENCIA_ADM_ESC,
                     
                     "pessoasResid" = Q001,
                     "educPai" = Q002,
                     "educMae" = Q003,
                     "rendFam" = Q004,
)

# Verificando nome das variáveis
names(mydata2011)

# Verificar o nome das variáveis após a renomeação
names(mydata2011)

# Modificar variáveis (MUTATE)
mydata2011 <- mutate(.data = mydata2011,
                     raca2 = case_when(
                       raca == 0 ~ "ND",
                       raca == 1 ~ "Branca",
                       raca == 4 ~ "Branca",
                       raca %in% c(2,3,5) ~ "PPI",
                       raca == 6 ~ "NA"
                     ),
                     raca = case_when(
                       raca == 0 ~ "ND",
                       raca == 1 ~ "Branca",
                       raca == 2 ~ "Preta",
                       raca == 3 ~ "Parda",
                       raca == 4 ~ "Amarela",
                       raca == 5 ~ "Indígena",
                       raca == 6 ~ "NA"
                     ),
                     faixaEtaria = case_when(
                       faixaEtaria %in% c(1:8) ~ "< 23",
                       faixaEtaria %in% c(9:11) ~ "24-30",
                       faixaEtaria %in% c(12:15) ~ "31-50",
                       faixaEtaria %in% c(15:20) ~ "> 50"
                     ),
                     educMae = case_when(
                       educMae == 'A' ~ 0,
                       educMae == 'B' ~ 2,
                       educMae == 'C' ~ 5,
                       educMae == 'D' ~ 9,
                       educMae == 'E' ~ 12,
                       educMae == 'F' ~ 16,
                       educMae == 'G' ~ 17,
                       educMae == 'H' ~ 21,
                       educMae == 'I' ~ 99
                     ),
                     educPai = case_when(
                       educPai == 'A' ~ 0,
                       educPai == 'B' ~ 2,
                       educPai == 'C' ~ 5,
                       educPai == 'D' ~ 9,
                       educPai == 'E' ~ 12,
                       educPai == 'F' ~ 16,
                       educPai == 'G' ~ 17,
                       educPai == 'H' ~ 21,
                       educPai == 'I' ~ 99
                     ),
                     rendFam = case_when(
                       rendFam == "A" ~ 0,
                       rendFam == "B" ~ 545,
                       rendFam == "C" ~ 817,
                       rendFam == "D" ~ 1090,
                       rendFam == "E" ~ 2725,
                       rendFam == "F" ~ 3815,
                       rendFam == "G" ~ 5450,
                       rendFam == "H" ~ 6540,
                       rendFam == "I" ~ 8175,
                       rendFam == "J" ~ 16350,
                       rendFam == "K" ~ 30000
                     ),
                     depAdminEsc = factor(depAdminEsc,
                                          levels = 1:4,
                                          labels = c('Federal', 'Estadual', 'Municipal', 'Privada'),
                                          ordered = TRUE
                     ),
                     tp_Escola = case_when(
                       TP_ESCOLA == 2 ~ "Publica",
                       TP_ESCOLA == 3 ~ "Privada"
                     )
)


mydata2011 <- mutate(.data = mydata2011,
                     
                     educMae = ifelse(educMae > 17,NA,educMae),
                     
                     rendFamP = (rendFam/pessoasResid),
                     
                     raca = ifelse(raca == "NA",NA,raca),
                     raca2 = ifelse(raca2 %in% c('NA','ND'),NA,raca2)
)

# Estrutura dos dados (pós manuseio)
str(mydata2011)

# Salvando dados manuseados ----
export(x = mydata2011,file = "C:/Users/11842581/Downloads/materiaisss/microdados_enem_2011/DADOS/MICRODADOSENEM11.csv")


# 4.1 Seleção das variáveis Enem 2014
mydata2014 <- subset(mydata2014,
                     select = c(TP_COR_RACA, TP_SEXO, TP_FAIXA_ETARIA,
                                TP_DEPENDENCIA_ADM_ESC,
                                TP_ESCOLA,
                                Q001, Q002, Q003, Q004)
)

# Renomear variáveis ENEM 2014 - RENAME

mydata2014 <- rename(.data = mydata2014,
                     
                     "raca"=TP_COR_RACA,
                     "sexo"=TP_SEXO,
                     "faixaEtaria"=TP_FAIXA_ETARIA,
                     "depAdminEsc"=TP_DEPENDENCIA_ADM_ESC,
                     
                     "educPai" = Q001,
                     "educMae" = Q002,
                     "rendFam" = Q003,
                     "pessoasResid" = Q004,
)

# Verificando nome das variáveis
names(mydata2014)

# Verificar o nome das variáveis após a renomeação
names(mydata2014)

# Modificar variáveis (MUTATE)
mydata2014 <- mutate(.data = mydata2014,
                     raca2 = case_when(
                       raca == 0 ~ "ND",
                       raca == 1 ~ "Branca",
                       raca == 4 ~ "Branca",
                       raca %in% c(2,3,5) ~ "PPI",
                       raca == 6 ~ "NA"
                     ),
                     raca = case_when(
                       raca == 0 ~ "ND",
                       raca == 1 ~ "Branca",
                       raca == 2 ~ "Preta",
                       raca == 3 ~ "Parda",
                       raca == 4 ~ "Amarela",
                       raca == 5 ~ "Indígena",
                       raca == 6 ~ "NA"
                     ),
                     faixaEtaria = case_when(
                       faixaEtaria %in% c(1:8) ~ "< 23",
                       faixaEtaria %in% c(9:11) ~ "24-30",
                       faixaEtaria %in% c(12:15) ~ "31-50",
                       faixaEtaria %in% c(15:20) ~ "> 50"
                     ),
                     educMae = case_when(
                       educMae == 'A' ~ 0,
                       educMae == 'B' ~ 2,
                       educMae == 'C' ~ 5,
                       educMae == 'D' ~ 9,
                       educMae == 'E' ~ 12,
                       educMae == 'F' ~ 16,
                       educMae == 'G' ~ 17,
                       educMae == 'H' ~ 21,
                       educMae == 'I' ~ 99
                     ),
                     educPai = case_when(
                       educPai == 'A' ~ 0,
                       educPai == 'B' ~ 2,
                       educPai == 'C' ~ 5,
                       educPai == 'D' ~ 9,
                       educPai == 'E' ~ 12,
                       educPai == 'F' ~ 16,
                       educPai == 'G' ~ 17,
                       educPai == 'H' ~ 21,
                       educPai == 'I' ~ 99
                     ),                   
                     rendFam = case_when(
                       rendFam == "A" ~ 0,
                       rendFam == "B" ~ 724,
                       rendFam == "C" ~ 1086,
                       rendFam == "D" ~ 1448,
                       rendFam == "E" ~ 1810,
                       rendFam == "F" ~ 2172,
                       rendFam == "G" ~ 2896,
                       rendFam == "H" ~ 3620,
                       rendFam == "I" ~ 4344,
                       rendFam == "J" ~ 5068,
                       rendFam == "K" ~ 5792,
                       rendFam == "L" ~ 6516,
                       rendFam == "M" ~ 7240,
                       rendFam == "N" ~ 8688,
                       rendFam == "O" ~ 10860,
                       rendFam == "P" ~ 14480,
                       rendFam == "Q" ~ 20000
                     ),
                    depAdminEsc = factor(depAdminEsc,
                     levels = 1:4,
                     labels = c('Federal', 'Estadual', 'Municipal', 'Privada'),
                     ordered = TRUE
                    ),
                    tp_Escola = case_when(
                      TP_ESCOLA == 1 ~ "Publica",
                      TP_ESCOLA == 2 ~ "Privada"
                      )
)

mydata2014 <- mutate(.data = mydata2014,
                     
                     educMae = ifelse(educMae > 17,NA,educMae),
                     
                     rendFamP = (rendFam/pessoasResid),
                     
                     raca = ifelse(raca == "NA",NA,raca),
                     raca2 = ifelse(raca2 %in% c('NA','ND'),NA,raca2)
)

# Estrutura dos dados (pós manuseio)
str(mydata2014)

# Salvando dados manuseados ----
export(x = mydata2014,file = "C:/Users/11842581/Downloads/materiaisss/microdados_enem_2011/DADOS/MICRODADOSENEM14.csv")



# 5. Harmonizar os dados do Enem de 2011 e 2014 ------------------------------------------------------------------

# Verificando se as colunas são iguais em ambos os bancos de dados
identical(names(mydata2011), names(mydata2014))

# Identificar as colunas que estão presentes em mydata2011, mas não em mydata2014
setdiff(names(mydata2011), names(mydata2014))

# Identificar as colunas que estão presentes em mydata2014, mas não em mydata2011
setdiff(names(mydata2014), names(mydata2011))

# Juntar os bancos de dados usando todas as colunas (as colunas devem ser iguais em ambos)
combined_data <- bind_rows(mydata2011, mydata2014)
head(combined_data)

# Verificando a estrutura do novo banco de dados combinado
str(combined_data)

# Salvando o novo banco de dados combinado em um arquivo CSV
export(x = combined_data, file = "C:/Users/11842581/Downloads/EnemDadosCombinados.csv")



# 6. Análises ------------------------------------------------------------------

# I Parte: Exploração dos dados
str(combined_data)
summary(combined_data)

# Teste 1: raca

# Criar a tabela de frequência da variável "raca"
tabela_raca <- table(combined_data$raca)

# Mostrar a tabela de frequência
print(tabela_raca)

# Ordenar a tabela de frequência da variável "raca" de forma decrescente
tabela_raca_ordenada <- sort(tabela_raca, decreasing = TRUE)

# Mostrar a tabela de frequência ordenada
print(tabela_raca_ordenada)

# Criar a tabela de frequência da variável "raca"
tabela_raca <- table(combined_data$raca)

# Converter a tabela de frequência em um data frame
df_tabela_raca <- as.data.frame(tabela_raca)

# Renomear as colunas do data frame
colnames(df_tabela_raca) <- c("Raça", "Frequência")

# Ordenar o data frame pela frequência de forma decrescente
df_tabela_raca <- df_tabela_raca[order(-df_tabela_raca$Frequência), ]

# Plotar o gráfico de barras
ggplot(data = df_tabela_raca, aes(x = Raça, y = Frequência, fill = Raça)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição da Variável Raça",
       x = "Raça",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1 RESULTADO: SAIRAM OS DADOS COMBINADOS DO ENEM DE 2011 E 2014 
# (PROBLEMA: PRECISO DOS DADOS SEPARADOS)




# Teste 2: raca

# Carregue o pacote dplyr se ainda não estiver carregado
library(dplyr)

# Verificar a estrutura do conjunto de dados mydata2011
str(mydata2011)

# Verificar a estrutura do conjunto de dados mydata2014
str(mydata2014)

# Adicionar a coluna "ano" aos conjuntos de dados mydata2011 e mydata2014
mydata2011$ano <- 2011
mydata2014$ano <- 2014

# Criar a tabela de frequência para o ano 2011
df_raca_2011 <- mydata2011 %>%
  count(raca) %>%
  rename(freq_2011 = n)

# Criar a tabela de frequência para o ano 2014
df_raca_2014 <- mydata2014 %>%
  count(raca) %>%
  rename(freq_2014 = n)

# Exibir o conteúdo das tabelas de frequência
print(df_raca_2011)
print(df_raca_2014)

# Carregue o pacote knitr
install.packages("knitr")
library(knitr)

# Tabela para o ano 2011
tabela_2011 <- df_raca_2011
kable(tabela_2011, format = "markdown")

# Tabela para o ano 2014
tabela_2014 <- df_raca_2014
kable(tabela_2014, format = "markdown")

# Tabela para o ano 2011
tabela_2011 <- df_raca_2011
View(tabela_2011)

# Tabela para o ano 2014
tabela_2014 <- df_raca_2014
View(tabela_2014)

# Gráfico de barras para o ano de 2011
ggplot(data = df_raca_2011, aes(x = raca, y = freq_2011)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Distribuição das Raças - Ano 2011",
       x = "Raça",
       y = "Frequência")

# Gráfico de barras para o ano de 2014
ggplot(data = df_raca_2014, aes(x = raca, y = freq_2014)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Distribuição das Raças - Ano 2014",
       x = "Raça",
       y = "Frequência")

# Gráfico de pizza para o ano de 2011
ggplot(data = df_raca_2011, aes(x = "", y = freq_2011, fill = raca)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição das Raças - Ano 2011",
       x = NULL,
       y = NULL) +
  theme_void()

# Gráfico de pizza para o ano de 2014
ggplot(data = df_raca_2014, aes(x = "", y = freq_2014, fill = raca)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribuição das Raças - Ano 2014",
       x = NULL,
       y = NULL) +
  theme_void()

# RESULTADO 2: Funcionou. Resultou em duas tabelas diferentes para os dois anos do Enem.


# Teste 3: Escolas públicas

# Filtrar os dados para obter apenas as inscrições de alunos de escolas públicas para cada ano separadamente
inscricoes_2011_publicas <- mydata2011 %>%
  filter(tp_Escola == "Publica")

inscricoes_2014_publicas <- mydata2014 %>%
  filter(tp_Escola == "Publica")

# Verificar os nomes das colunas do conjunto de dados mydata2011
names(mydata2011)

# Verificar os nomes das colunas do conjunto de dados mydata2014
names(mydata2014)

# Criar a tabela de frequência para o ano 2011 (alunos de escolas públicas)
tabela_inscricoes_2011 <- mydata2011 %>%
  filter(tp_Escola == "Publica") %>%
  count(ano, name = "Num_Inscricoes_2011")

# Criar a tabela de frequência para o ano 2014 (alunos de escolas públicas)
tabela_inscricoes_2014 <- mydata2014 %>%
  filter(tp_Escola == "Publica") %>%
  count(ano, name = "Num_Inscricoes_2014")

# Exibir as tabelas de frequência
print(tabela_inscricoes_2011)
print(tabela_inscricoes_2014)

# Tabela para o ano 2011
tabela_2011 <- tabela_inscricoes_2011
View(tabela_2011)

# Tabela para o ano 2014
tabela_2014 <- tabela_inscricoes_2014
View(tabela_2014)

# RESULTADOS: FUNCIONOU.


# Teste 3: inscrições totais

cat("Total de inscritos no ENEM em 2011:", total_inscritos_2011, "\n")
cat("Total de inscritos no ENEM em 2014:", total_inscritos_2014, "\n")

# Resultado: funcionou.


# Teste 4: escola pública e raça 

# ENEM 2011

# Verificar as colunas do dataframe mydata2011
print(colnames(mydata2011))

# Criar a tabela cruzada entre escola pública (tp_Escola) e raça (raca)
tabela_cruzada_2011 <- table(mydata2011$raca, mydata2011$tp_Escola, useNA = "ifany")

# Visualizar a tabela cruzada
print(tabela_cruzada_2011)

# Tabela para o ano 2011
tabela_cruzada_2011 <- table(mydata2011$raca, mydata2011$tp_escola_publica, useNA = "ifany")

# Visualizar a tabela cruzada
View(tabela_cruzada_2011)

# ENEM 2014

# Verificar as colunas do dataframe mydata2014
print(colnames(mydata2014))

# Criar a tabela cruzada entre escola pública (tp_Escola) e raça (raca)
tabela_cruzada_2014 <- table(mydata2014$raca, mydata2014$tp_Escola, useNA = "ifany")

# Visualizar a tabela cruzada
print(tabela_cruzada_2014)

# Tabela para o ano 2014
tabela_cruzada_2014 <- table(mydata2014$raca, mydata2011$tp_escola_publica, useNA = "ifany")

# Visualizar a tabela cruzada
View(tabela_cruzada_2014)

# RESULTADO: FUNCIONOU


# Teste 5: Inscrições de acordo com depAdemEsc

# Filtrar as inscrições no Enem entre os anos de 2011 e 2014
inscricoes_2011_2014 <- mydata2011[mydata2011$ano >= 2011 & mydata2011$ano <= 2014, ]

# Contagem de inscrições por ano
contagem_por_ano <- table(inscricoes_2011_2014$ano)

# Visualizar a contagem por ano
print(contagem_por_ano)

# Criar uma tabela cruzada para visualizar a distribuição por ano e por dependência administrativa da escola
tabela_cruzada <- table(inscricoes_2011_2014$ano, inscricoes_2011_2014$depAdminEsc)

# Visualizar a tabela cruzada
print(tabela_cruzada)

# RESULTADO: SAIU APENAS OS DADOS DE 2011; MOTIVO: DESCONHECIDO


# Teste 6: Inscrições no Enem por faixa etária

# Filtrar as inscrições no Enem apenas para o ano de 2011
inscricoes_2011 <- mydata2011[mydata2011$ano == 2011, ]

# Criar a tabela cruzada para visualizar a distribuição por faixa etária
tabela_cruzada_2011 <- table(inscricoes_2011$faixaEtaria)

# Visualizar a tabela cruzada para o ano de 2011
print(tabela_cruzada_2011)


# Filtrar as inscrições no Enem apenas para o ano de 2014
inscricoes_2014 <- mydata2011[mydata2011$ano == 2014, ]

# Criar a tabela cruzada para visualizar a distribuição por faixa etária
tabela_cruzada_2014 <- table(inscricoes_2014$faixaEtaria)

# Visualizar a tabela cruzada para o ano de 2014
print(tabela_cruzada_2014)

unique(mydata2011$ano)
unique(mydata2014$ano)

# Filtrar as inscrições no Enem apenas para o ano de 2014 (usando mydata2014 ou o conjunto de dados correto)
inscricoes_2014 <- mydata2014[mydata2014$ano == 2014, ]

# Criar a tabela cruzada para visualizar a distribuição por faixa etária para o ano de 2014
tabela_cruzada_2014 <- table(inscricoes_2014$faixaEtaria)

# Visualizar a tabela cruzada para o ano de 2014
print(tabela_cruzada_2014)

# Tabela para o ano 2011
View(tabela_cruzada_2011)

# Tabela para o ano 2014
View(tabela_cruzada_2014)

# Resultado: funcionou perfeitamente.


# Teste 7: Inscrição no enem de acordo com a escolaridade da mãe

# ENEM 2011
# Filtrar as inscrições no Enem apenas para o ano de 2011
inscricoes_2011 <- mydata2011[mydata2011$ano == 2011, ]

# Criar a tabela cruzada para visualizar a distribuição por escolaridade da mãe em 2011
tabela_cruzada_mae_2011 <- table(inscricoes_2011$educMae)

# Visualizar a tabela cruzada para o ano de 2011
print(tabela_cruzada_mae_2011)

# Tabela para o ano 2011
View(tabela_cruzada_mae_2011)

#ENEM 2014
# Filtrar as inscrições no Enem apenas para o ano de 2014
inscricoes_2014 <- mydata2014[mydata2014$ano == 2014, ]

# Criar a tabela cruzada para visualizar a distribuição por escolaridade da mãe em 2014
tabela_cruzada_mae_2014 <- table(inscricoes_2014$educMae)

# Visualizar a tabela cruzada para o ano de 2014
print(tabela_cruzada_mae_2014)

# Tabela para o ano 2014
View(tabela_cruzada_mae_2014)

# Resultado: FUNCIONOU


# Teste 8: Inscrição no enem de acordo com a escolaridade do pai

# ENEM 2011
# Filtrar as inscrições no Enem apenas para o ano de 2011
inscricoes_2011 <- mydata2011[mydata2011$ano == 2011, ]

# Criar a tabela cruzada para visualizar a distribuição por escolaridade do pai em 2011
tabela_cruzada_pai_2011 <- table(inscricoes_2011$educPai)

# Visualizar a tabela cruzada para o ano de 2011
print(tabela_cruzada_pai_2011)

# Tabela para o ano 2011
View(tabela_cruzada_pai_2011)

# ENEM 2014
# Filtrar as inscrições no Enem apenas para o ano de 2014
inscricoes_2014 <- mydata2014[mydata2014$ano == 2014, ]

# Criar a tabela cruzada para visualizar a distribuição por escolaridade do pai em 2014
tabela_cruzada_pai_2014 <- table(inscricoes_2014$educPai)

# Visualizar a tabela cruzada para o ano de 2014
print(tabela_cruzada_pai_2014)

# Tabela para o ano 2014
View(tabela_cruzada_pai_2014)

# Resultado: FUNCIONOU


# Teste 9: Variável RendFamP

# Tabela de frequência para o ano 2011
tabela_renda_2011 <- mydata2011 %>%
  filter(ano == 2011) %>%
  count(rendFamP, name = "Freq_2011")

# Tabela de frequência para o ano 2014
tabela_renda_2014 <- mydata2014 %>%
  filter(ano == 2014) %>%
  count(rendFamP, name = "Freq_2014")

print(tabela_renda_2011)
print(tabela_renda_2014)

# Tabela para o ano de 2011
View(tabela_renda_2011)

# Tabela para o ano de 2014
View(tabela_renda_2014)

# Tabela para o ano 2011
tabela_renda_2011 <- mydata2011 %>%
  filter(ano == 2011) %>%
  count(rendFamP, name = "Freq_2011")

# Tabela para o ano 2014
tabela_renda_2014 <- mydata2014 %>%
  filter(ano == 2014) %>%
  count(rendFamP, name = "Freq_2014")

# Exibir as tabelas formatadas em markdown
kable(tabela_renda_2011, format = "markdown", col.names = c("Renda Familiar", "Frequência 2011"))
kable(tabela_renda_2014, format = "markdown", col.names = c("Renda Familiar", "Frequência 2014"))

# Definir os intervalos desejados e rótulos
intervalos <- c(0, 600, 900, 1100, 2800, 3900, 5000, 7500, 20000, Inf)
rotulos <- c("0-600", "600-900", "900-1100", "1100-2800", "2800-3900", "3900-5000", "5000-7500", "7500-20000", ">20000")

# Agrupar a variável de renda nos intervalos desejados para o ano de 2011
mydata2011$rendFamP_grupo <- cut(mydata2011$rendFamP, breaks = intervalos, labels = rotulos, right = FALSE)

# Agrupar a variável de renda nos intervalos desejados para o ano de 2014
mydata2014$rendFamP_grupo <- cut(mydata2014$rendFamP, breaks = intervalos, labels = rotulos, right = FALSE)

# Tabela de frequência para o ano de 2011 com valores agrupados
tabela_renda_2011_grupo <- table(mydata2011$rendFamP_grupo)

# Tabela de frequência para o ano de 2014 com valores agrupados
tabela_renda_2014_grupo <- table(mydata2014$rendFamP_grupo)

# Exibir as tabelas de frequência formatadas em markdown
print(kable(tabela_renda_2011_grupo, format = "markdown", col.names = c("Renda Familiar", "Frequência 2011")))
print(kable(tabela_renda_2014_grupo, format = "markdown", col.names = c("Renda Familiar", "Frequência 2014")))

# RESULTADO: FUNCIONOU



