# ANALISE BIVARIADA RELACIONANDO AS VARIÁVEIS CID LINHAII E CAUSABAS

#necesário rodar o código "filtragem.R" antes

library(plotly)
library(htmlwidgets)


#------------- QUAL A CAUSA BÁSICA DE MORTE MAIS COMUM EM MORTES QUE TIVERAM A CONTRIBUIÇÃO DE PSICOATIVOS? --------

# Função para categorizar os códigos CID, considerando apenas os três primeiros caracteres
categorizar_cid <- function(causa) {
  # Extrair os três primeiros caracteres
  causa_principal <- substr(causa, 1, 3)
  
  if (causa_principal >= "A00" & causa_principal <= "B99") {
    return("Doenças infecciosas e parasitárias (A00-B99)")
  } else if (causa_principal >= "C00" & causa_principal <= "D48") {
    return("Neoplasias (tumores) (C00-D48)")
  } else if (causa_principal >= "D50" & causa_principal <= "D89") {
    return("Doenças do sangue e órgãos hematopoéticos (D50-D89)")
  } else if (causa_principal >= "E00" & causa_principal <= "E90") {
    return("Doenças endócrinas, metabólicas e nutricionais  (E00-E90)")
  } else if (causa_principal >= "F00" & causa_principal <= "F99") {
    return("Transtornos mentais e comportamentais  (F00-F99)")
  } else if (causa_principal >= "G00" & causa_principal <= "G99") {
    return("Doenças do sistema nervoso  (G00-G99)")
  } else if (causa_principal >= "H00" & causa_principal <= "H59") {
    return("Doenças dos olhos  (H00-H59")
  } else if (causa_principal >= "H60" & causa_principal <= "H95") {
    return("Doenças do ouvido e apófise mastoide  (H60-H95")
  } else if (causa_principal >= "I00" & causa_principal <= "I99") {
    return("Doenças do aparelho circulatório  (I00-I99)")
  } else if (causa_principal >= "J00" & causa_principal <= "J99") {
    return("Doenças do aparelho respiratório  (J00-J99)")
  } else if (causa_principal >= "K00" & causa_principal <= "K93") {
    return("Doenças do aparelho digestivo  (K00-K93")
  } else if (causa_principal >= "L00" & causa_principal <= "L99") {
    return("Doenças da pele e tecido subcutâneo  (L00-L99)")
  } else if (causa_principal >= "M00" & causa_principal <= "M99") {
    return("Doenças do sistema osteomuscular (M00-M99)")
  } else if (causa_principal >= "N00" & causa_principal <= "N99") {
    return("Doenças do aparelho geniturinário (N00-N99)")
  } else if (causa_principal >= "O00" & causa_principal <= "O99") {
    return("Gravidez, parto e puerpério (O00-O99)")
  } else if (causa_principal >= "P00" & causa_principal <= "P96") {
    return("Afecções perinatais (P00-P96)")
  } else if (causa_principal >= "Q00" & causa_principal <= "Q99") {
    return("Malformações congênitas (Q00-Q99)")
  } else if (causa_principal >= "R00" & causa_principal <= "R99") {
    return("Sinais e achados anormais (R00-R99)")
  } else if (causa_principal >= "S00" & causa_principal <= "T98") {
    return("Lesões e envenenamentos (S00-T98)")
  } else if (causa_principal >= "V01" & causa_principal <= "Y98") {
    return("Causas externas de morbidade e mortalidade")
  } else if (causa_principal >= "Z00" & causa_principal <= "Z99") {
    return("Fatores que influenciam o estado de saúde (Z00-Z99)")
  } else {
    return("Categoria desconhecida")
  }
}

# Aplicar a função de categorização
dados_br_linhaii$categoria_causabas <- sapply(dados_br_linhaii$CAUSABAS, categorizar_cid)

#Agrupar dados da coluna categoria_causabas
linhaii_causabas <- dados_br_linhaii %>%
  group_by(categoria_causabas) %>%
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


  