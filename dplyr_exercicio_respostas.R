# Exercícios --------------------------------------------------------------

# Exercício 1 
# Usando o dataset 'starwars', calcule:
data("starwars")

# 1. Quantos personagens há por gênero?

starwars %>% 
  select(name, gender) %>% 
  mutate(gender = ifelse(is.na(gender), "none", gender)) %>% 
  group_by(gender) %>% 
  summarise(counts = n())

# 2. Qual a média da massa dos personagens por gênero?

starwars %>% 
  select(name, gender, mass) %>% 
  mutate(gender = ifelse(is.na(gender), "none", gender)) %>% 
  group_by(gender) %>% 
  summarise(mean = mean(mass, na.rm = T))


# Exercicio 2

# Faca uma funcao "OGanalyzer" que receberah como argumento o dataframe cogdata
# a funcao deverah calcular, para cada especie, quantas proteinas ha em cada orthologous_group
# a saida deverah ser um data.frame de tres colunas: 
# (1) o codigo da especie, (2) o orthologous_group (3) o numero de proteinas daquela especie contida no respectivo orthologous_group 
# lembre-se que para cada especie ha diversos orthologous_group, cada um com diversas proteinas
load("COGs.RData")

cog.human.data %>% 
  separate(protein_id, into = c("species_id", "protein"), sep = "\\.", extra = "drop") %>% 
  group_by(ssp_id, cog_id) %>% 
  summarise(n_proteins = n())




