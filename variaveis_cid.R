#-------------- CONFIGURACOES INICIAIS ----------------------------
#rodar primeiramente o codigo de filtragem de dados


library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(DT)


#----------------------- PADRONIZACAO -------------------------------------


Sys.setlocale("LC_ALL", "pt_BR.UTF-8") #configurar o R pra aceitar acentuação nas palavras

# Definir um tema global para todos os gráficos GGPLOT
#theme_padronizado <- theme(
 # plot.title = element_text(size = 16),   # Tamanho do título
 # axis.title = element_text(size = 11),   # Tamanho dos títulos dos eixos
 # axis.text = element_text(size = 11),    # Tamanho dos textos dos eixos
  #legend.position = "right",              # Mantém a posição da legenda à direita
  #legend.justification = "top",           # Alinha a legenda ao topo
  #legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0), # Remove margens extras
  #legend.key.height = unit(0, "lines"),   # Define a altura da chave da legenda para 0
  #legend.key.width = unit(0, "lines"),    # Define a largura da chave da legenda para 0
  #legend.title = element_blank(),         # Remove o título da legenda
  #legend.text = element_blank()           # Remove o texto da legenda
#)

# Aplicar o tema a todos os gráficos subsequentes
#theme_set(theme_minimal() + theme_padronizado)



#criar pasta pra salvar os graficos
#dir.create("graficos_comparacao")



#---------------- GRAFICO DE BARRAS DE NUMERO DE OBSERVACOES (LINHAS) POR VARIÁVEL -----------------

lista_dfs <- list(dados_br_causabas, dados_br_causabaso, dados_br_linhaa, dados_br_linhab, dados_br_linhab, 
                  dados_br_linhac, dados_br_linhad, dados_br_linhaii) # criar uma lista com todos os dataframes das variaveis 
nomes_dfs <- c("dados_br_causabas", "dados_br_causabaso", "dados_br_linhaa", "dados_br_linhab", "dados_br_linhab", 
               "dados_br_linhac", "dados_br_linhad", "dados_br_linhaii") # criar um vetor com o nome dos dataframes

nomes_variaveis <- sub("dados_br_", "", nomes_dfs)#remover o prefixo dos dataframes e salvar apenas os nomes das variaveis

num_obs <- sapply(lista_dfs, nrow)#contar o numero de linhas de cada dataframe

obs_variavel <- data.frame(Nome = nomes_variaveis, Observacoes = num_obs) #criar dataframe com o numero de linhas de cada variavel


#plotando e salvando grafico ggplot

obs.por.variavel <- ggplot(obs_variavel, aes(x = Nome, y = Observacoes, fill = Nome, 
                                             text = paste("Variável:", Nome, "<br>",
                                                          "N. de Observações:", Observacoes))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # tirar notação científica
  labs(title = "Observações por Variável CID - BR",
       x = "Variável",
       y = "Número de Observações")+
  theme(legend.position = "none")  # Remove a legenda

ggsave(filename = "graficos_comparacao/obs_por_variavel.png", 
       plot = obs.por.variavel, 
       width = 10, height = 6, dpi = 150)


# -------- COMPARACAO ENTRE O NUMERO TOTAL DE MORTES POR ANO EM CADA VARIAVEL----------------

df_comparacoes <- data.frame() #dataframe vazio pra armazenar os dados de morte por ano e variavel

# Loop para agrupar as mortes por ANOOBITO em cada data frame
for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  # Agrupar por ANOOBITO, somar o número de mortes e tirar media das idades
  df_agrupado <- df %>%
    group_by(ANOOBITO) %>%
    summarise(Total_Mortes = n(),
              Media_Idade = mean(IDADE2, na.rm = TRUE)) %>%
    mutate(Variavel = nome_variavel)
  
  # Combinar com o dataframe final
  df_comparacoes <- rbind(df_comparacoes, df_agrupado)
} 

#rm("df_agrupado")



# Plotar o gráfico
mortes.por.variavel <- ggplot(df_comparacoes, aes(x = ANOOBITO, y = Total_Mortes, color = Variavel, group = Variavel, 
                                                  text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                               "N. de Mortes:", Total_Mortes, "<br>", "Variável CID:", Variavel))) +
  geom_line(size = 1) +
  labs(title = "Mortes por Psicoativos por Ano - BR",
       x = "Ano",
       y = "Número de Mortes") +
  scale_x_continuous(breaks = 2013:2022)  # Definir os anos no eixo x


#salvar grafico
ggsave(filename = "graficos_comparacao/mortes_por_variavel.png", 
       plot = mortes.por.variavel, 
       width = 10, height =12, dpi = 150)


# ----------------- IDADE E FAIXA ETARIA-------------------------------

# Gráfico da idade média das mortes por ano
idade.por.variavel <- ggplot(df_comparacoes, aes(x = ANOOBITO, y = Media_Idade, color = Variavel, group = Variavel,
                                                  text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                               "Media das Idade:", Media_Idade, "<br>", "Variável CID:", Variavel))) +
  
  
  geom_line(size = 1) +
  labs(title = "Idade Média das Mortes por Ano - BR",
       x = "Ano",
       y = "Idade Média") +
  scale_x_continuous(breaks = 2013:2022)+
  scale_y_continuous(breaks = seq(floor(min(df_comparacoes$Media_Idade)), 
                                  ceiling(max(df_comparacoes$Media_Idade)), 
                                  by = 2))

# Salvar gráfico
ggsave(filename = "graficos_comparacao/idade_por_variavel.png", 
       plot = idade.por.variavel, 
       width = 10, height = 6, dpi = 150)



#BOXPLOT IDADES

df_idades <- data.frame() 

for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  df <- df %>% mutate(Variavel = nome_variavel) # Adicionar uma coluna com o nome da variável do cid
  
  df_idades <- rbind(df_idades, df[, c("IDADE2", "Variavel")]) # Selecionar apenas as colunas que eu quero e combinar com o dataframe criado
}

boxplot.idades <- ggplot(df_idades, aes(x = Variavel, y = IDADE2, color = Variavel)) +
  geom_boxplot() +
  labs(title = "Idades por Variável CID - BR",
       x = "Variável",
       y = "Idade")

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/boxplot_idades_por_variavel.png", 
       plot = boxplot.idades, 
       width = 10, height = 6, dpi = 150)


#SERIES POR FAIXA ETÁRIA

df_faixas_etarias <- data.frame()

for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  # Filtrar as idades para manter apenas [30-60) e [60-infinito)
  df <- df %>%
    filter(IDADE2 >= 30) %>%  # Filtrar idades >= 30
    mutate(Faixa_Etaria = case_when(
      IDADE2 >= 30 & IDADE2 < 60 ~ "[30-60)",
      IDADE2 >= 60 ~ "[60-infinito)"
    )) %>%
    group_by(ANOOBITO, Faixa_Etaria) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel)
  
  df_faixas_etarias <- rbind(df_faixas_etarias, df) # Combinar os dados no dataframe
} 

# Criar o gráfico de séries
grafico.series.faixaeta <- ggplot(df_faixas_etarias, aes(x = ANOOBITO, y = Total, color = Variavel, shape = Faixa_Etaria,
                                                         text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                                      "N. de Mortes:", Total, "<br>", 
                                                                      "Variável CID:", Variavel, "<br>", 
                                                                      "Faixa Etária:", Faixa_Etaria))) +
  geom_line(aes(group = interaction(Variavel, Faixa_Etaria)), size = 0.5) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c("[30-60)" = 16, "[60-infinito)" = 15)) +  # Bolinha para [30-60), quadrado para [60-infinito)
  labs(title = "Mortes por Faixa Etária e Variável CID - BR",
       x = "Ano",
       y = "Quantidade de Pessoas",
       shape = "Faixa Etária") 
  scale_x_continuous(breaks = 2013:2022)  # Definir os anos no eixo x

# Exibir o gráfico
print(grafico.series.faixaeta)

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/grafico_series_faixaeta.png", 
       plot = grafico.series.faixaeta, 
       width = 10, height = 12, dpi = 150)




#---------------------- SEXO ---------------

df_genero <- data.frame()

for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  # Agrupar por ANOOBITO, SEXO e calcular o número de mortes
  df_agrupado <- df %>%
    filter(!is.na(SEXO)) %>%
    group_by(ANOOBITO, SEXO) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel)
  
  df_genero <- rbind(df_genero, df_agrupado)
}

# Criar o gráfico
grafico.series.genero <- ggplot(df_genero, aes(x = ANOOBITO, y = Total, color = Variavel, shape = SEXO,
                                               text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                            "N. de Mortes:", Total, "<br>", "Variável CID:", Variavel, "<br>", "Sexo:", SEXO ))) +
  geom_line(aes(group = interaction(Variavel, SEXO)), size = 0.5) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c("Masculino" = 16, "Feminino" = 17)) +  # Bolinha para masculino, triângulo para feminino
  labs(title = "Mortes por Sexo e Variável CID - BR",
       x = "Ano",
       y = "Número de Mortes",
       shape = "Sexo") +
  scale_x_continuous(breaks = 2013:2022)

# Exibir o gráfico
print(grafico.series.genero)

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/grafico_series_genero.png", 
       plot = grafico.series.genero, 
       width = 10, height = 12, dpi = 150)


#----------------------- RACA ---------------------------------

df_raca <- data.frame()

for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  # Filtrar  brancos, rpetos e partos e juntar pretos com pardos
  df <- df %>%
    filter(RACACOR %in% c("Branca", "Preta", "Parda")) %>%
    mutate(Raca_Agrupada = case_when(
      RACACOR %in% c("Preta", "Parda") ~ "Pretos/Pardos",
      RACACOR == "Branca" ~ "Brancos"
    ))
  
  # Agrupar por ANOOBITO, Raca_Agrupada e calcular o número de mortes
  df_agrupado <- df %>%
    group_by(ANOOBITO, Raca_Agrupada) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel)
  
  df_raca <- rbind(df_raca, df_agrupado)
}

# Criar o gráfico de séries
grafico.series.raca <- ggplot(df_raca, aes(x = ANOOBITO, y = Total, color = Variavel, shape = Raca_Agrupada,
                                           text = paste("Ano do Óbito:", ANOOBITO, "<br>",
                                                        "N. de Mortes:", Total, "<br>", "Variável CID:", Variavel, "<br>", "Cor/Raça:", Raca_Agrupada ))) +
  geom_line(aes(group = interaction(Variavel, Raca_Agrupada)), size = 0.5) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c("Brancos" = 16, "Pretos/Pardos" = 17)) +  # Bolinha para Brancos, triângulo para Pretos/Pardos
  labs(title = "Mortes por Raça e Variável CID - BR",
       x = "Ano",
       y = "Número de Mortes",
       color = "Variável CID",
       shape = "Raça") +
  scale_x_continuous(breaks = 2013:2022) +
  guides(
    color = guide_legend(title = "Variável CID", order = 1),  # Legenda para as cores
    shape = guide_legend(title = "Raça", order = 2)  # Legenda para os formatos
  ) 

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/grafico_series_raca.png", 
       plot = grafico.series.raca,
       width = 10, height = 12, dpi = 150)



#------------- TABELA DE DADOS NA ----------------------------

df_na_porcentagem <- data.frame()

# Variáveis sociodemográficas que serão analisadas
var_sociodemo <- c("SEXO", "RACACOR", "ESC", "ESTCIV")

for (i in seq_along(lista_dfs)) {
  df <- lista_dfs[[i]]
  nome_variavel <- sub("dados_br_", "", nomes_dfs[i])
  
  # Loop para calcular a porcentagem de NA para cada variável sociodemográfica
  for (var_soc in var_sociodemo) {
    df_na <- df %>%
      group_by(ANOOBITO) %>%
      summarise(
        Total_Casos = n(),
        Total_NA = sum(is.na(.data[[var_soc]])),
        Porcentagem_NA = (Total_NA / Total_Casos) * 100
      ) %>%
      mutate(Variavel_CID = nome_variavel,
             Var_Sociodemo = var_soc)
    
    # Combinar os resultados no dataframe final
    df_na_porcentagem <- bind_rows(df_na_porcentagem, df_na)
  }
}

# Criar tabela interativa
datatable(
  df_na_porcentagem,
  filter = 'top',  # Adiciona filtros no topo de cada coluna
  options = list(pageLength = 10),  # Mostra 10 linhas por página
  caption = 'Porcentagem de Dados NA por Ano, Variável do CID e Variável Sociodemográfica - BR'
)

# Salvar a tabela como CSV ou outro formato se necessário
write.csv(df_na_porcentagem, "graficos_comparacao/na_porcentagem_por_ano.csv", row.names = FALSE)

