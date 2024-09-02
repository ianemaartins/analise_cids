#-------------- CONFIGURACOES INICIAIS ----------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(DT)

Sys.setlocale("LC_ALL", "pt_BR.UTF-8") #configurar o R pra aceitar acentuação nas palavras

# Definir um tema global para todos os gráficos
theme_padronizado_es <- theme(
  plot.title = element_text(size = 22),   # Tamanho do título
  axis.title = element_text(size = 15), # Tamanho dos títulos dos eixos
  axis.text = element_text(size = 15),    # Tamanho dos textos dos eixos
  legend.title = element_text(size = 18),  # Tamanho dos títulos das legendas
  legend.text = element_text(size = 18)   # Tamanho dos textos das legendas
)

# Aplicar o tema a todos os gráficos subsequentes
theme_set(theme_minimal() + theme_padronizado_es)

#criar pasta pra salvar os graficos
#dir.create("graficos_comparacao")

#---------------- GRAFICO DE BARRAS DE NUMERO DE OBSERVACOES (LINHAS) POR VARIÁVEL -----------------

lista_dfs_es <- list(dados_es_causabas, dados_es_causabaso, dados_es_linhaa, dados_es_linhab, dados_es_linhab, 
                     dados_es_linhac, dados_es_linhad, dados_es_linhaii) # criar uma lista com todos os dataframes das variaveis 
nomes_dfs_es <- c("dados_es_causabas", "dados_es_causabaso", "dados_es_linhaa", "dados_es_linhab", "dados_es_linhab", 
                  "dados_es_linhac", "dados_es_linhad", "dados_es_linhaii") # criar um vetor com o nome dos dataframes

nomes_variaveis_es <- sub("dados_es_", "", nomes_dfs_es) #remover o prefixo dos dataframes e salvar apenas os nomes das variaveis

num_obs_es <- sapply(lista_dfs_es, nrow) #contar o numero de linhas de cada dataframe

obs_variavel_es <- data.frame(Nome = nomes_variaveis_es, Observacoes = num_obs_es) #criar dataframe com o numero de linhas de cada variavel

obs.por.variavel.es <- ggplot(obs_variavel_es, aes(x = Nome, y = Observacoes, fill = Nome)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +  # tirar notação científica
  labs(title = "Observações por Variável CID - ES",
       x = "Variável",
       y = "Número de Observações")

#salvar grafico
ggsave(filename = "graficos_comparacao/obs_por_variavel_es.png", 
       plot = obs.por.variavel.es, 
       width = 10, height = 6, dpi = 150)

# -------- COMPARACAO ENTRE O NUMERO TOTAL DE MORTES POR ANO EM CADA VARIAVEL ----------------

df_comparacoes_es <- data.frame() #dataframe vazio pra armazenar os dados de morte por ano e variavel

# Loop para agrupar as mortes por ANOOBITO em cada data frame
for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Agrupar por ANOOBITO, somar o número de mortes e tirar media das idades
  df_agrupado_es <- df %>%
    group_by(ANOOBITO) %>%
    summarise(Total_Mortes = n(),
              Media_Idade = mean(IDADE2, na.rm = TRUE)) %>%
    mutate(Variavel = nome_variavel_es)
  
  # Combinar com o dataframe final
  df_comparacoes_es <- rbind(df_comparacoes_es, df_agrupado_es)
} 

# Plotar o gráfico
mortes.por.variavel.es <- ggplot(df_comparacoes_es, aes(x = ANOOBITO, y = Total_Mortes, color = Variavel, group = Variavel)) +
  geom_line(size = 1) +
  labs(title = "Mortes por Psicoativos por Ano - ES",
       x = "Ano",
       y = "Número de Mortes") +
  scale_x_continuous(breaks = 2013:2022)  # Definir os anos no eixo x

#salvar grafico
ggsave(filename = "graficos_comparacao/mortes_por_variavel_es.png", 
       plot = mortes.por.variavel.es, 
       width = 10, height = 12, dpi = 150)

# ----------------- IDADE E FAIXA ETARIA -------------------------------

# Gráfico da idade média das mortes por ano
idade.por.variavel.es <- ggplot(df_comparacoes_es, aes(x = ANOOBITO, y = Media_Idade, color = Variavel, group = Variavel)) +
  geom_line(size = 1) +
  labs(title = "Idade Média das Mortes por Ano - ES",
       x = "Ano",
       y = "Idade Média") +
  scale_x_continuous(breaks = 2013:2022) +
  scale_y_continuous(breaks = seq(floor(min(df_comparacoes_es$Media_Idade)), 
                                  ceiling(max(df_comparacoes_es$Media_Idade)), 
                                  by = 2))

# Salvar gráfico
ggsave(filename = "graficos_comparacao/idade_por_variavel_es.png", 
       plot = idade.por.variavel.es, 
       width = 10, height = 6, dpi = 150)

# BOXPLOT IDADES

df_idades_es <- data.frame() 

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  df <- df %>% mutate(Variavel = nome_variavel_es) # Adicionar uma coluna com o nome da variável do cid
  
  df_idades_es <- rbind(df_idades_es, df[, c("IDADE2", "Variavel")]) # Selecionar apenas as colunas que eu quero e combinar com o dataframe criado
}

boxplot_idades_es <- ggplot(df_idades_es, aes(x = Variavel, y = IDADE2, color = Variavel)) +
  geom_boxplot() +
  labs(title = "Idades por Variável CID - ES",
       x = "Variável",
       y = "Idade")

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/boxplot_idades_por_variavel_es.png", 
       plot = boxplot_idades_es, 
       width = 10, height = 6, dpi = 150)

# SERIES POR FAIXA ETÁRIA

df_faixas_etarias_es <- data.frame()

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Filtrar as idades para manter apenas [30-60) e [60-infinito)
  df <- df %>%
    filter(IDADE2 >= 30) %>%  # Filtrar idades >= 30
    mutate(Faixa_Etaria = case_when(
      IDADE2 >= 30 & IDADE2 < 60 ~ "[30-60)",
      IDADE2 >= 60 ~ "[60-infinito)"
    )) %>%
    group_by(ANOOBITO, Faixa_Etaria) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel_es)
  
  df_faixas_etarias_es <- rbind(df_faixas_etarias_es, df) # Combinar os dados no dataframe
} 

# Criar o gráfico de séries
grafico.series.faixaeta.es <- ggplot(df_faixas_etarias_es, aes(x = ANOOBITO, y = Total, color = Variavel, shape = Faixa_Etaria)) +
  geom_line(aes(linetype = Variavel), size = 1) +
  geom_point(size = 3) +
  scale_shape_manual(values = c("[30-60)" = 16, "[60-infinito)" = 15)) +  # Bolinha para [30-60), quadrado para [60-infinito)
  labs(title = "Mortes por Faixa Etária e Variável CID - ES",
       x = "Ano",
       y = "Quantidade de Pessoas",
       shape = "Faixa Etária") +
  scale_x_continuous(breaks = 2013:2022)  # Definir os anos no eixo x

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/grafico_series_faixaeta_es.png", 
       plot = grafico.series.faixaeta.es, 
       width = 10, height = 12, dpi = 150)

#---------------------- SEXO ---------------

df_genero_es <- data.frame()

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Agrupar por ANOOBITO, SEXO e calcular o número de mortes
  df_agrupado_es <- df %>%
    filter(!is.na(SEXO)) %>%
    group_by(ANOOBITO, SEXO) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel_es)
  
  df_genero_es <- rbind(df_genero_es, df_agrupado_es)
}

# Criar o gráfico
grafico.series.genero.es <- ggplot(df_genero_es, aes(x = ANOOBITO, y = Total, color = Variavel, shape = SEXO)) +
  geom_line(aes(linetype = Variavel), size = 1) +
  geom_point(size = 3) +
  scale_shape_manual(values = c("Masculino" = 16, "Feminino" = 17)) +  # Bolinha para masculino, triângulo para feminino
  labs(title = "Mortes por Sexo e Variável CID - ES",
       x = "Ano",
       y = "Número de Mortes",
       shape = "Sexo") +
  scale_x_continuous(breaks = 2013:2022)

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/grafico_series_genero_es.png", 
       plot = grafico.series.genero.es, 
       width = 10, height = 12, dpi = 150)

#----------------------- RACA ---------------------------------

df_raca_es <- data.frame()

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Filtrar brancos, pretos e pardos e juntar pretos com pardos
  df <- df %>%
    filter(RACACOR %in% c("Branca", "Preta", "Parda")) %>%
    mutate(Raca_Agrupada = case_when(
      RACACOR %in% c("Preta", "Parda") ~ "Pretos/Pardos",
      RACACOR == "Branca" ~ "Brancos"
    ))
  
  # Agrupar por ANOOBITO, Raca_Agrupada e calcular o número de mortes
  df_agrupado_es <- df %>%
    group_by(ANOOBITO, Raca_Agrupada) %>%
    summarise(Total = n()) %>%
    mutate(Variavel = nome_variavel_es)
  
  df_raca_es <- rbind(df_raca_es, df_agrupado_es)
}

# Criar o gráfico de séries
grafico.series.raca.es <- ggplot(df_raca_es, aes(x = ANOOBITO, y = Total, color = Variavel, shape = Raca_Agrupada)) +
  geom_line(aes(linetype = Variavel), size = 1) +
  geom_point(size = 3) +
  scale_shape_manual(values = c("Brancos" = 16, "Pretos/Pardos" = 17)) +  # Bolinha para Brancos, triângulo para Pretos/Pardos
  labs(title = "Mortes por Raça e Variável CID - ES",
       x = "Ano",
       y = "Número de Mortes",
       shape = "Raça") +
  scale_x_continuous(breaks = 2013:2022)  

# Salvar o gráfico
ggsave(filename = "graficos_comparacao/grafico_series_raca_es.png", 
       plot = grafico.series.raca.es, 
       width = 10, height = 12, dpi = 150)

#------------- TABELA DE DADOS NA ----------------------------

df_na_porcentagem_es <- data.frame()

# Variáveis sociodemográficas que serão analisadas
var_sociodemo_es <- c("SEXO", "RACACOR", "ESC", "ESTCIV")

for (i in seq_along(lista_dfs_es)) {
  df <- lista_dfs_es[[i]]
  nome_variavel_es <- sub("dados_es_", "", nomes_dfs_es[i])
  
  # Loop para calcular a porcentagem de NA para cada variável sociodemográfica
  for (var_soc in var_sociodemo_es) {
    df_na_es <- df %>%
      group_by(ANOOBITO) %>%
      summarise(
        Total_Casos = n(),
        Total_NA = sum(is.na(.data[[var_soc]])),
        Porcentagem_NA = (Total_NA / Total_Casos) * 100
      ) %>%
      mutate(Variavel_CID = nome_variavel_es,
             Var_Sociodemo = var_soc)
    
    # Combinar os resultados no dataframe final
    df_na_porcentagem_es <- bind_rows(df_na_porcentagem_es, df_na_es)
  }
}

# Criar tabela interativa
datatable(
  df_na_porcentagem_es,
  filter = 'top',  # Adiciona filtros no topo de cada coluna
  options = list(pageLength = 10),  # Mostra 10 linhas por página
  caption = 'Porcentagem de Dados NA por Ano, Variável do CID e Variável Sociodemográfica - ES'
)

# Salvar a tabela como CSV ou outro formato se necessário
write.csv(df_na_porcentagem_es, "graficos_comparacao/na_porcentagem_por_ano_es.csv", row.names = FALSE)
