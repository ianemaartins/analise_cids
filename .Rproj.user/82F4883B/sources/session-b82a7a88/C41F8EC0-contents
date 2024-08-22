#VARIAVEL LINHAD

#criar pasta para salvar os graficos dessa variavel
dir.create("variavel_linhad")



#--------- SEXO

#BR

# Agrupar dados por sexo e calcular porcentagem
sexo.br.linhad <- dados_br_linhad %>%
  group_by(ANOOBITO, SEXO) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.sexo.br.linhad <- ggplot(data = sexo.br.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Sexo - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.sexo.br.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_sexo_br_linhad.png", 
       plot = series.sexo.br.linhad, 
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.sexo.br.linhad <- ggplot(sexo.br.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Sexo",
       title = "% de Mortes por Psicoativos no Brasil por Sexo e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.sexo.br.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_sexo_br_linhad.png", 
       plot = proporcao.sexo.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por genero e calcular porcentagem
sexo.es.linhad <- dados_es_linhad %>%
  group_by(ANOOBITO, SEXO) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.sexo.es.linhad <- ggplot(data = sexo.es.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 10) +
  labs(title = "Número de óbitos no ES por Sexo - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.sexo.es.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_sexo_es_linhad.png", 
       plot = series.sexo.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.sexo.es.linhad <- ggplot(sexo.es.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Sexo",
       title = "% de Mortes por Psicoativos no ES por Sexo e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.sexo.es.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_sexo_es_linhad.png", 
       plot = proporcao.sexo.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)





#------------- RAÇA

#BR

# Agrupar dados por raça e calcular porcentagem
raca.br.linhad <- dados_br_linhad %>%
  group_by(ANOOBITO, RACACOR) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.raca.br.linhad <- ggplot(data = raca.br.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Raça/Cor - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.raca.br.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_raca_br_linhad.png", 
       plot = series.raca.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por raça a cada ano
proporcao.raca.br.linhad <- ggplot(raca.br.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça/Cor",
       title = "% de Mortes por Psicoativos no Brasil por Raça/Cor e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.raca.br.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_raca_br_linhad.png", 
       plot = proporcao.raca.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por raça e calcular porcentagem
raca.es.linhad <- dados_es_linhad %>%
  group_by(ANOOBITO, RACACOR) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.raca.es.linhad <- ggplot(data = raca.es.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Raça/Cor - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.raca.es.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_raca_es_linhad.png", 
       plot = series.raca.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por raça a cada ano
proporcao.raca.es.linhad <- ggplot(raca.es.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça/Cor",
       title = "% de Mortes por Psicoativos no ES por Raça/Cor e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 13),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.raca.es.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_raca_es_linhad.png", 
       plot = proporcao.raca.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)





#---------- ESCOLARIDADE

#BR

# Agrupar dados por escolaridade e calcular porcentagem
esc.br.linhad <- dados_br_linhad %>%
  group_by(ANOOBITO, ESC) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.esc.br.linhad <- ggplot(data = esc.br.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Escolaridade - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.esc.br.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_esc_br_linhad.png", 
       plot = series.esc.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por escolaridade a cada ano
proporcao.esc.br.linhad <- ggplot(esc.br.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESC)) +
  geom_bar(stat = "identity") + 
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "% de Mortes por Psicoativos no Brasil por Escolaridade e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.esc.br.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_esc_br_linhad.png", 
       plot = proporcao.esc.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por escolaridade e calcular porcentagem
esc.es.linhad <- dados_es_linhad %>%
  group_by(ANOOBITO, ESC) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.esc.es.linhad <- ggplot(data = esc.es.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Escolaridade - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.esc.es.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_esc_es_linhad.png", 
       plot = series.esc.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)


#plotar grafico de proporção de mortes por escolaridade a cada ano
proporcao.esc.es.linhad <- ggplot(esc.es.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESC)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "% de Mortes por Psicoativos no ES por Escolaridade e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.esc.es.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_esc_es_linhad.png", 
       plot = proporcao.esc.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)





#ESTADO CIVIL

#BR

# Agrupar dados por estado civil e calcular porcentagem
estciv.br.linhad <- dados_br_linhad %>%
  group_by(ANOOBITO, ESTCIV) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.estciv.br.linhad <- ggplot(data = estciv.br.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Estado Civil - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.estciv.br.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_estciv_br_linhad.png", 
       plot = series.estciv.br.linhad, 
       width = 3.5, height = 2.5, dpi = 150)


#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.estciv.br.linhad <- ggplot(estciv.br.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Estado Civil",
       title = "% de Mortes por Psicoativos no Brasil por Estado Civil e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.estciv.br.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_estciv_br_linhad.png", 
       plot = proporcao.estciv.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por estado civil e calcular porcentagem
estciv.es.linhad <- dados_es_linhad %>%
  group_by(ANOOBITO, ESTCIV) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.estciv.es.linhad <- ggplot(data = estciv.es.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Estado Civil - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.estciv.es.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_estciv_es_linhad.png", 
       plot = series.estciv.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por estciv a cada ano
proporcao.estciv.es.linhad <- ggplot(estciv.es.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Estado Civil",
       title = "% de Mortes por Psicoativos no ES por Estado Civil e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.estciv.es.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_estciv_es_linhad.png", 
       plot = proporcao.estciv.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)






#FAIXA ETARIA

#BR

#montar coluna de faixa etaria
dados_br_linhad$FAIXAETA <- cut(dados_br_linhad$IDADE2,
                                  breaks = c(-Inf, 12, 17, 30, 60, Inf),
                                  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
                                  right = TRUE) #inclusivo a direita (x-y]

#agrupar por faixa etaria
faixaeta.br.linhad <- dados_br_linhad %>%
  group_by(ANOOBITO, FAIXAETA) %>%
  summarise(N.OBITOS = n()) %>%
  ungroup() %>%
  group_by(ANOOBITO) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()


# Plotar gráfico de séries
series.faixaeta.br.linhad <- ggplot(data = faixaeta.br.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = FAIXAETA)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Faixa Etária - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.faixaeta.br.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_faixaeta_br_linhad.png", 
       plot = series.faixaeta.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)

#plotar grafico de proporção de mortes por faixa etaria a cada ano
proporcao.faixaeta.br.linhad <- ggplot(faixaeta.br.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Faixa Etária",
       title = "% de Mortes por Psicoativos no Brasil por Faixa Etária e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.faixaeta.br.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_faixaeta_br_linhad.png", 
       plot = proporcao.faixaeta.br.linhad,
       width = 3.5, height = 2.5, dpi = 150)



#ES

#montar coluna de faixa etaria
dados_es_linhad$FAIXAETA <- cut(dados_es_linhad$IDADE2,
                                  breaks = c(-Inf, 12, 17, 30, 60, Inf),
                                  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
                                  right = TRUE) #inclusivo a direita (x-y]

#agrupar por faixa etaria
faixaeta.es.linhad <- dados_es_linhad %>%
  group_by(ANOOBITO, FAIXAETA) %>%
  summarise(N.OBITOS = n()) %>%
  ungroup() %>%
  group_by(ANOOBITO) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()


# Plotar gráfico de séries
series.faixaeta.es.linhad <- ggplot(data = faixaeta.es.linhad, aes(x = ANOOBITO, y = N.OBITOS, color = FAIXAETA)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Faixa Etária - variável LINHAD",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.faixaeta.es.linhad)

#salvar grafico
ggsave(filename = "variavel_linhad/series_faixaeta_es_linhad.png", 
       plot = series.faixaeta.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)

#plotar grafico de proporção de mortes por faixa etaria a cada ano
proporcao.faixaeta.es.linhad <- ggplot(faixaeta.es.linhad, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Faixa Etária",
       title = "% de Mortes por Psicoativos no ES por Faixa Etária e Ano - Variável LINHAD") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.faixaeta.es.linhad)

# Salvar gráfico
ggsave(filename = "variavel_linhad/proporcao_faixaeta_es_linhad.png", 
       plot = proporcao.faixaeta.es.linhad,
       width = 3.5, height = 2.5, dpi = 150)


