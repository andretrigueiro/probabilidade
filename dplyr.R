# Dplyr, tidyr, ggplot2 ---------------------------------------------------

# Instalar pacotes --------------------------------------------------------
install.packages(c("dplyr", "tidyr", "ggplot2"))

# Carregar pacotes --------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Funções dplyr -----------------------------------------------------------

# Operador '%>%' 
iris %>% subset(Sepal.Length > 5)
iris2 <- iris %>% subset(Sepal.Length > 5)

# Manipulação de dataframes ----
# select() - seleciona colunas
mtcars %>% 
  select(mpg, cyl, wt)

mtcars %>% 
  select(1, 2, 3)

mtcars %>% 
  select(-mpg, -cyl, -wt)

mtcars %>% 
  select(mpg:hp)

mtcars %>% 
  select(-(mpg:hp))

mtcars %>% 
  select(contains("m"))

mtcars %>% 
  select(starts_with("mp"))

mtcars %>% 
  select(ends_with("p"), starts_with("m"))

mtcars %>% 
  select(matches("m|d"))

# filter() - Filtra dataframes (equivalente ao subset)
mtcars %>% 
  select(mpg, wt, cyl) %>% 
  filter(mpg >= 20 & wt >= 3)
# OU
mtcars %>%
  select(mpg, wt, cyl) %>% 
  filter(mpg >= 20, wt >= 3) # a vírgula funciona como o operador lógico "&"

mtcars %>% 
  select(mpg, wt, cyl) %>% 
  filter(mpg >= 20 | wt >= 3)

# arrange() - Ordena dataframes com base em uma ou mais colunas
mtcars %>% 
  select(mpg, cyl) %>% 
  filter(mpg > 20) %>% 
  arrange(mpg)

mtcars %>% 
  select(mpg, cyl) %>% 
  filter(mpg > 20) %>% 
  arrange(desc(mpg)) # ordem decrescente

# rename()
mtcars %>% 
  filter(mpg > 30) %>% 
  select(mpg, hp, wt) %>% 
  rename(MPG = mpg, HP = hp, WT = wt)

# mutate() - Adiciona uma coluna ou mais colunas ao dataframe
mtcars %>%  
  mutate(wt_kg = round(wt / 2.2 * 1000),
         potencia = ifelse(hp > 200, "S", "N")) 

mtcars %>%  
  mutate(wt = round(wt / 2.2 * 1000))

mtcars %>% 
  transmute(wt_kg = round(wt / 2.2 * 1000))

# group_by() / summarise() - realiza alguma redução ao dataframe
mtcars %>% 
  group_by(cyl, am) %>% 
  summarise(mean_mpg = mean(mpg),
            sd_mpg = sd(mpg))

# Contagem de ocorrências (linhas do dataframe) por grupo
mtcars %>% 
  group_by(cyl, am) %>% 
  summarise(n = n())

# Ou
mtcars %>% 
  group_by(cyl) %>% 
  count(am)

# Operações com conjuntos ----
v1 <- 1:10
v2 <- 6:15

# setdiff() - diferença entre os conjuntos
setdiff(v1, v2)
setdiff(v2, v1)

# intersect()
intersect(v1, v2)

# union()
union(v1, v2)

# Funções tidyr -----------------------------------------------------------
# separate() - Separar uma coluna em duas ou mais colunas diferentes
presidential2 <- presidential %>% 
  separate(col = start, into = c("start_year", "start_month", "start_day"), sep = "-")

# unite() - Une colunas por um separador
presidential3 <- presidential2 %>% 
  unite(col = start, start_year, start_month, start_day, sep = "-")
# ou
presidential3 <- presidential2 %>% 
  unite(col = start, starts_with("start"), sep = "-")

# spread() - Separar os valores de uma coluna baseando-se em outra coluna de keys
# Antes, é preciso criar uma coluna de identificação para que cada linha seja única.
iris2 <- iris
iris2$id <- 1:nrow(iris2)
iris_3 <- iris2 %>% 
  spread(Species, Sepal.Length)

iris_4 <- iris2 %>% 
  spread(Species, Sepal.Width)

# gather () - Reune as observações de um grupo de colunas em uma estrutura key - value
iris_4 <- iris %>% 
  gather(key = feature, value = value, Sepal.Length:Petal.Width)


# Exercícios --------------------------------------------------------------

# Exercício 1 
# Usando o dataset 'starwars', calcule:
data("starwars")

# 1. Quantos personagens há por gênero?




# 2. Qual a média da massa dos personagens por gênero?




# Exercicio 2

# Faca uma funcao "OGanalyzer" que receberah como argumento o dataframe cogdata
# a funcao deverah calcular, para cada especie, quantas proteinas ha em cada orthologous_group
# a saida deverah ser um data.frame de tres colunas: 
# (1) o codigo da especie, (2) o orthologous_group (3) o numero de proteinas daquela especie contida no respectivo orthologous_group 
# lembre-se que para cada especie ha diversos orthologous_group, cada um com diversas proteinas
load("COGs.RData")




# Plotando com o ggplot2 --------------------------------------------------

# O gráfico criado pelo ggplot é um objeto.
# O gráfico é criado em camadas.

# A função ggplot() inicializa o gráfico.
# A função aes() especifica as coordenadas (x, y, color, fill, shape, size)
# As funções 'geom_' especificam que tipo de gráfico será criado 

g <- ggplot(mtcars)

# Criando um gráfico de dispersão
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point()

ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
  geom_point()

ggplot(mtcars, aes(x = mpg, y = hp, shape = factor(cyl))) +
  geom_point()

ggplot(mtcars, aes(x = mpg, y = hp, size = wt)) +
  geom_point()

# Linhas 
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_line() + 
  geom_point()

ggplot(DNase, aes(x = conc, y = density, color = factor(Run))) +
  geom_line() + 
  geom_point()

# Barras
mtcars$names <- rownames(mtcars)
ggplot(mtcars, aes(x = names, y = wt)) +
  geom_bar(stat = "identity")

ggplot(mtcars, aes(x = reorder(names, -wt), y = wt)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Boxplot
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + 
  geom_boxplot()

ggplot(airquality, aes(x = factor(Month), y = Solar.R)) +
  geom_boxplot() +
  geom_point()

# Histograma / gráfico de densidade
ggplot(airquality, aes(x = Solar.R, fill = factor(Month))) +
  geom_histogram()

ggplot(airquality, aes(x = Solar.R, fill = factor(Month))) +
  geom_density(alpha = 0.6)

# Facets
ggplot(airquality, aes(x = Solar.R, fill = factor(Month))) +
  geom_histogram(bins = 10) +
  facet_grid(. ~ factor(Month))

# Configurando outros elementos do gráfico
theme_1 <- theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        axis.line = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5))

theme_2 <- theme_light() +
  theme(axis.title = element_text(size = 12, face = "italic"),
        axis.text = element_text(size = 11),
        axis.line = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5))
  
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) + 
  geom_boxplot() + 
  scale_y_continuous(limits = c(5, 35)) +
  labs(title = "MPG x CYL", x = "cyl", y = "mpg") +
  theme_2



















