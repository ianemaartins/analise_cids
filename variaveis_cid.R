library(ggplot2)
library(scales)

#criar pasta pra salvar os graficos
#dir.create("graficos_comparacao")

# GRAFICO DE BARRAS DE NUMERO DE OBSERVACOES (LINHAS) POR VARIÁVEL

lista_dfs <- list(dados_br_causabas, dados_br_causabaso, dados_br_linhaa, dados_br_linhab, dados_br_linhab, 
                  dados_br_linhac, dados_br_linhad, dados_br_linhaii) # criar uma lista com todos os dataframes das variaveis 
nomes_dfs <- c("dados_br_causabas", "dados_br_causabaso", "dados_br_linhaa", "dados_br_linhab", "dados_br_linhab", 
               "dados_br_linhac", "dados_br_linhad", "dados_br_linhaii") # criar um vetor com o nome dos dataframes

nomes_variaveis <- sub("dados_br_", "", nomes_dfs)#remover o prefixo dos dataframes e salvar apenas os nomes das variaveis

num_obs <- sapply(lista_dfs, nrow)#contar o numero de linhas de cada dataframe

obs_variavel <- data.frame(Nome = nomes_variaveis, Observacoes = num_obs) #criar dataframe com o numero de linhas de cada variavel


obs.por.variavel <- ggplot(obs_variavel, aes(x = Nome, y = Observacoes, fill = Nome)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +  # tirar notação científica
  labs(title = "Número de Observações por Variável",
       x = "Variável",
       y = "Número de Observações")

#salvar grafico
ggsave(filename = "graficos_comparacao/obs_por_variavel.png", 
       plot = obs.por.variavel, 
       width = 7, height = 4, dpi = 150)



# COMPARACAO ENTRE O NUMERO TOTAL DE MORTES POR ANO EM CADA VARIAVEL

df_total_mortes <- data.frame() #dataframe vazio pra armazenar os dados de morte por ano e variavel

# Loop para agrupar as mortes por ANOOBITO em cada data frame
for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  # Agrupar por ANOOBITO e somar o número de mortes
  df_agrupado <- df %>%
    group_by(ANOOBITO) %>%
    summarise(Total_Mortes = n()) %>%
    mutate(Variavel = nome_variavel)
  
  # Combinar com o data frame final
  df_total_mortes <- rbind(df_total_mortes, df_agrupado)
}

# Plotar o gráfico
mortes.por.variavel <- ggplot(df_total_mortes, aes(x = ANOOBITO, y = Total_Mortes, color = Variavel, group = Variavel)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Número Total de Mortes por Ano",
       x = "Ano",
       y = "Número de Mortes") +
  scale_x_continuous(breaks = 2013:2022)  # Definir os anos no eixo x


#salvar grafico
ggsave(filename = "graficos_comparacao/mortes_por_variavel.png", 
       plot = mortes.por.variavel, 
       width = 7, height = 4, dpi = 150)

