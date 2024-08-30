library(readr)


#LER BASES DE DADOS
dados_br <- data.frame()

for (ano in 2018:2021) {
  
  df2 <- readRDS(paste0("bases_de_dados/dados_", ano,".rds"))
  dados_br <- bind_rows(df2,dados_br)
  
}

#ADICIONANDO COLUNAS EXTRAS

#criar coluna do ano em que o obito ocorreu
dados_br$DTOBITO <- ymd(dados_br$DTOBITO)
dados_br$ANOOBITO <- year(dados_br$DTOBITO)

#criar coluna da idade em anos completos
dados_br$DTNASC  <- ymd(dados_br$DTNASC)  
dados_br$DTOBITO  <- ymd(dados_br$DTOBITO)
dados_br$IDADE2  <- floor(interval(start  =  dados_br$DTNASC , 
                                         end = dados_br$DTOBITO) / years(1)) # arrendondado p baixo - anos completos

#FILTRANDO POR ESTADO ES

#acrescentar codigos UF
var_extras_UF <- read_table("var_extras_UF.txt")
var_extras_UF$codigoUF <- as.character(var_extras_UF$codigoUF)

# criando a var codigosUF
dados_br <- dados_br %>% 
  mutate(
    codigoUF = substr(as.character(CODMUNOCOR), 1, 2)
  )

dados_br <- inner_join(dados_br, var_extras_UF, by = "codigoUF")


dados_es <- dados_br %>%
  filter(Sigla == "ES")



#FILTRANDO POR CID

#CAUSABAS_O
#br
dados_br_causabas <- dados_br[grepl("F1", dados_br$CAUSABAS, ignore.case = TRUE),]
#es
dados_es_causabas <- dados_es[grepl("F1", dados_es$CAUSABAS, ignore.case = TRUE),]

#CAUSABAS_O
#br
dados_br_causabaso <- dados_br[grepl("F1", dados_br$CAUSABAS_O, ignore.case = TRUE),]
#es
dados_es_causabaso <- dados_es[grepl("F1", dados_es$CAUSABAS_O, ignore.case = TRUE),]

#ATESTADO (alguns anos não possuem essa variavel)
#br
#dados_br_atestado <- dados_br[grepl("F1", dados_br$ATESTADO, ignore.case = TRUE),]
#es
#dados_es_atestado <- dados_es[grepl("F1", dados_es$ATESTADO, ignore.case = TRUE),]

#CB_PRE - contem apenas dados NA

#LINHAA
#br
dados_br_linhaa <- dados_br[grepl("F1", dados_br$LINHAA, ignore.case = TRUE),]
#es
dados_es_linhaa <- dados_es[grepl("F1", dados_es$LINHAA, ignore.case = TRUE),]

#LINHAB
#br
dados_br_linhab <- dados_br[grepl("F1", dados_br$LINHAB, ignore.case = TRUE),]
#es
dados_es_linhab <- dados_es[grepl("F1", dados_es$LINHAB, ignore.case = TRUE),]

#LINHAC
#br
dados_br_linhac <- dados_br[grepl("F1", dados_br$LINHAC, ignore.case = TRUE),]
#es
dados_es_linhac <- dados_es[grepl("F1", dados_es$LINHAC, ignore.case = TRUE),]

#LINHAD
#br
dados_br_linhad <- dados_br[grepl("F1", dados_br$LINHAD, ignore.case = TRUE),]
#es
dados_es_linhad <- dados_es[grepl("F1", dados_es$LINHAD, ignore.case = TRUE),]

#LINHAII
#br
dados_br_linhaii <- dados_br[grepl("F1", dados_br$LINHAII, ignore.case = TRUE),]
#es
dados_es_linhaii <- dados_es[grepl("F1", dados_es$LINHAII, ignore.case = TRUE),]

