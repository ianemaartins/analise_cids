# ANALISE BIVARIADA RELACIONANDO AS VARIÁVEIS CID LINHAII E CAUSABAS

#necesário rodar o código "filtragem.R"  e "bivariada_linhaii_causabas.R" antes

#library(plotly)
#library(htmlwidgets)

#------------- QUAIS OS CONTRIBUINTES NAS MORTES QUE TIVERAM OS PSICOATIVOS COMO A CAUSA BÁSICA DE MORTE? --------

# Aplicar a função de categorização feita em bivariada_linhaii_causabas.R" 
dados_br_causabas$categoria_linhaii <- sapply(dados_br_causabas$LINHAII, categorizar_cid)

#Agrupar dados da coluna categoria_linhaii
causabas_linhaii <- dados_br_causabas %>%
  group_by(categoria_linhaii) %>%
  summarize(quantidade = n()) %>%
  arrange(desc(quantidade))

# Reordenar a variável 'categoria_causabas' como um fator ordenado
linhaii_causabas$categoria_causabas <- factor(
  linhaii_causabas$categoria_causabas, 
  levels = linhaii_causabas$categoria_causabas[order(linhaii_causabas$quantidade, decreasing = FALSE)]
)


#plotar grafico plotly com as cids no eixo y e a quantidade do eixo x
gbarra_linhaii_causabas <- plot_ly(
  data = linhaii_causabas,
  x = ~quantidade,
  y = ~categoria_causabas,
  type = 'bar',
  orientation = 'h'  # 'h' para horizontal
)

# Ajustar layout
gbarra_linhaii_causabas <-gbarra_linhaii_causabas %>%
  layout(
    xaxis = list(title = 'Número de Óbitos'),
    yaxis = list(title = 'Causa Básica do Óbito')
  )


# Salvar o gráfico HTML
pasta <- "graficos_comparacao/gbarra_linhaii_causabas.html"
saveWidget(gbarra_linhaii_causabas, pasta)