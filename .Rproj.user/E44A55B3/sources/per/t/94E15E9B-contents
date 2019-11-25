# Trabalho da terceira unidade da matéria de Probabilidade (IMD0033)
# Autor: André de Oliveira Trigueiro
# Docente: Ismenia Blavatsky de Magalhães

# -----------------------------------------------------------------#

# Carregar pacotes
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------------------------------------#

# Importando os dados a serem utilizados. Ambos foram obtidos no site http://dados.ufrn.br/

dados_docentes <- read.csv("docentes.csv", sep = ";", encoding = "UTF-8")

dados_avaliacoes_docentes <-read.csv("avaliacaodocencia.csv", sep = ";", encoding = "UTF-8")

# Existem 85.138 alaviações de doscentes e 2370 docentes cadastrados

# -----------------------------------------------------------------#

# Olhando os dados

head(dados_avaliacoes_docentes)
str(dados_avaliacoes_docentes)

# Avaliando o máximo de turmas que algum docente deu

qnt_docentes <- count(dados_avaliacoes_docentes$id_docente)
qnt_docentes <- arrange(qnt_docentes, freq)

# Top6 docentes que deram mais aulas

top6 <- tail(qnt_docentes$x)

# Descobrindo nome dos docentes

professores_top6 <- dados_avaliacoes_docentes %>%
  select(id_docente, nome_docente) %>%
  filter(id_docente %in% top6) %>%
  group_keys(nome_docente)

professores_top6  

# Ranking de professores que tem mais avaliações:
# 1 - 5757049 - ADALA NAYANA DE SOUSA MATA
# 2 - 5756259 - CLAUDIA NUNES OLIVEIRA
# 3 - 5756244 - FRANCISCO PIGNATARO LIMA
# 4 - 22757   - JANE CRISTINA MEDEIROS
# 5 - 5756273 - LILIANE PEREIRA BRAGA
# 6 - 22755   - SEBASTIAO PACHECO DUQUE NETO

# Busca pelos dados da docente no dataframe dos docentes

professores <- dados_docentes %>%
  filter(nome %in% professores_top6$nome_docente)

avaliacao_professores <- dados_avaliacoes_docentes %>%
  filter(nome_docente %in% professores_top6$nome_docente)

# Checkando alguns dados dos docentes que parecem redundantes, visto que este dataset aparentemente é apenas de docentes ativos

count(dados_docentes$vinculo)

# No caso, todos os docentes que estão no dataset estão ativos.

# -----------------------------------------------------------------#

# Avaliando quantos docentes estão nos dois datasets, no caso, 2191 docentes

professores_avaliacao <- dados_avaliacoes_docentes %>%
  select(nome_docente) %>%
  group_keys(nome_docente)

match_professores <- dados_docentes %>%
  select(nome) %>%
  filter(nome %in% professores_avaliacao$nome_docente)

# -----------------------------------------------------------------#

# Agregando dados dos dois datasets, para ter uma visão mais simplificada dos dados


# Nota-se que, antes de aplicar esta função, o dataset dados_avaliacoes_docentes tem 85.138 objetos, 
# após aplicar este filtro, o número baixa para 61.564 objetos, ou seja, aproximadamente 24.000 avaliações de docentes que nao estao mais ativos

dados_agregados_avaliacao <- dados_avaliacoes_docentes %>%
  select(id_docente, nome_docente, qtd_discentes, postura_profissional_media, postura_profissional_DP, atuacao_profissional_media, atuacao_profissional_DP, autoavaliacao_aluno_DP, autoavaliacao_aluno_media) %>%
  group_by(nome_docente) %>%
  filter(nome_docente %in% match_professores$nome)

# Aplicando o filtro para saber quantos docentes que foram avaliados ainda estão ativos

dados_agregados_docentes <- dados_docentes %>%
  select(nome, sexo, formacao, tipo_jornada_trabalho, categoria, classe_funcional, id_unidade_lotacao, lotacao, admissao) %>%
  filter(nome %in% match_professores$nome)

# No dataset, temos 2370 docentes ativos, quando aplicamos, o numero de docentes com avaliação desce para 2191
# ou seja, 179 a menos. Provavelmente, estes docentes ingressaram neste semestre e ainda não possuem nenhuma avaliação.
# Vamos conferir isto:

# -----------------------------------------------------------------#

# Pegando a diferença entre os datasets
diff <- setdiff(dados_docentes$nome, dados_agregados_docentes$nome)

# Selecionando um dos docentes que nao esta nas avaliações
nome_exemplo1 <- diff[1]

dados_docentes %>%
  filter(nome == nome_exemplo1)

# Saida do RStudio: Encontrou o docente GILDETE AZEVEDO

dados_avaliacoes_docentes %>%
  filter(nome == nome_exemplo1)

# Saida do RStudio: Error: object 'nome' not found

###############################

# Selecionando outro docente 
nome_exemplo2 <- diff[10]

dados_docentes %>%
  filter(nome == nome_exemplo2)

# Saida do RStudio: Encontrou o docente ANA PAULA COSTA CAMARA

dados_avaliacoes_docentes %>%
  filter(nome == nome_exemplo1)
# Saida do RStudio: Error: object 'nome' not found

# Note que as buscas foram executadas nos datasets originais, e não nos modificados.

# -----------------------------------------------------------------#

