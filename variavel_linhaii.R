#VARIAVEL LINHAII

#criar pasta para salvar os graficos dessa variavel
dir.create("variavel_linhaii")



#--------- SEXO

#BR

# Agrupar dados por sexo e calcular porcentagem
sexo.br.linhaii <- dados_br_linhaii %>%
  group_by(ANOOBITO, SEXO) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.sexo.br.linhaii <- ggplot(data = sexo.br.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Sexo - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.sexo.br.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_sexo_br_linhaii.png", 
       plot = series.sexo.br.linhaii, 
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.sexo.br.linhaii <- ggplot(sexo.br.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Sexo",
       title = "% de Mortes por Psicoativos no Brasil por Sexo e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.sexo.br.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_sexo_br_linhaii.png", 
       plot = proporcao.sexo.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por genero e calcular porcentagem
sexo.es.linhaii <- dados_es_linhaii %>%
  group_by(ANOOBITO, SEXO) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.sexo.es.linhaii <- ggplot(data = sexo.es.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 10) +
  labs(title = "Número de óbitos no ES por Sexo - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.sexo.es.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_sexo_es_linhaii.png", 
       plot = series.sexo.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.sexo.es.linhaii <- ggplot(sexo.es.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Sexo",
       title = "% de Mortes por Psicoativos no ES por Sexo e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.sexo.es.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_sexo_es_linhaii.png", 
       plot = proporcao.sexo.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)





#------------- RAÇA

#BR

# Agrupar dados por raça e calcular porcentagem
raca.br.linhaii <- dados_br_linhaii %>%
  group_by(ANOOBITO, RACACOR) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.raca.br.linhaii <- ggplot(data = raca.br.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Raça/Cor - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.raca.br.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_raca_br_linhaii.png", 
       plot = series.raca.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por raça a cada ano
proporcao.raca.br.linhaii <- ggplot(raca.br.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça/Cor",
       title = "% de Mortes por Psicoativos no Brasil por Raça/Cor e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.raca.br.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_raca_br_linhaii.png", 
       plot = proporcao.raca.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por raça e calcular porcentagem
raca.es.linhaii <- dados_es_linhaii %>%
  group_by(ANOOBITO, RACACOR) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.raca.es.linhaii <- ggplot(data = raca.es.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Raça/Cor - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.raca.es.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_raca_es_linhaii.png", 
       plot = series.raca.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por raça a cada ano
proporcao.raca.es.linhaii <- ggplot(raca.es.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça/Cor",
       title = "% de Mortes por Psicoativos no ES por Raça/Cor e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 13),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.raca.es.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_raca_es_linhaii.png", 
       plot = proporcao.raca.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)





#---------- ESCOLARIDADE

#BR

# Agrupar dados por escolaridade e calcular porcentagem
esc.br.linhaii <- dados_br_linhaii %>%
  group_by(ANOOBITO, ESC) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.esc.br.linhaii <- ggplot(data = esc.br.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Escolaridade - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.esc.br.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_esc_br_linhaii.png", 
       plot = series.esc.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por escolaridade a cada ano
proporcao.esc.br.linhaii <- ggplot(esc.br.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESC)) +
  geom_bar(stat = "identity") + 
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "% de Mortes por Psicoativos no Brasil por Escolaridade e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.esc.br.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_esc_br_linhaii.png", 
       plot = proporcao.esc.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por escolaridade e calcular porcentagem
esc.es.linhaii <- dados_es_linhaii %>%
  group_by(ANOOBITO, ESC) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.esc.es.linhaii <- ggplot(data = esc.es.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Escolaridade - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.esc.es.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_esc_es_linhaii.png", 
       plot = series.esc.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)


#plotar grafico de proporção de mortes por escolaridade a cada ano
proporcao.esc.es.linhaii <- ggplot(esc.es.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESC)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "% de Mortes por Psicoativos no ES por Escolaridade e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.esc.es.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_esc_es_linhaii.png", 
       plot = proporcao.esc.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)





#ESTADO CIVIL

#BR

# Agrupar dados por estado civil e calcular porcentagem
estciv.br.linhaii <- dados_br_linhaii %>%
  group_by(ANOOBITO, ESTCIV) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.estciv.br.linhaii <- ggplot(data = estciv.br.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Estado Civil - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.estciv.br.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_estciv_br_linhaii.png", 
       plot = series.estciv.br.linhaii, 
       width = 3.5, height = 2.5, dpi = 150)


#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.estciv.br.linhaii <- ggplot(estciv.br.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Estado Civil",
       title = "% de Mortes por Psicoativos no Brasil por Estado Civil e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.estciv.br.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_estciv_br_linhaii.png", 
       plot = proporcao.estciv.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por estado civil e calcular porcentagem
estciv.es.linhaii <- dados_es_linhaii %>%
  group_by(ANOOBITO, ESTCIV) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.estciv.es.linhaii <- ggplot(data = estciv.es.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Estado Civil - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.estciv.es.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_estciv_es_linhaii.png", 
       plot = series.estciv.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por estciv a cada ano
proporcao.estciv.es.linhaii <- ggplot(estciv.es.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Estado Civil",
       title = "% de Mortes por Psicoativos no ES por Estado Civil e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.estciv.es.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_estciv_es_linhaii.png", 
       plot = proporcao.estciv.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)






#FAIXA ETARIA

#BR

#montar coluna de faixa etaria
dados_br_linhaii$FAIXAETA <- cut(dados_br_linhaii$IDADE2,
                                  breaks = c(-Inf, 12, 17, 30, 60, Inf),
                                  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
                                  right = TRUE) #inclusivo a direita (x-y]

#agrupar por faixa etaria
faixaeta.br.linhaii <- dados_br_linhaii %>%
  group_by(ANOOBITO, FAIXAETA) %>%
  summarise(N.OBITOS = n()) %>%
  ungroup() %>%
  group_by(ANOOBITO) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()


# Plotar gráfico de séries
series.faixaeta.br.linhaii <- ggplot(data = faixaeta.br.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = FAIXAETA)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Faixa Etária - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.faixaeta.br.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_faixaeta_br_linhaii.png", 
       plot = series.faixaeta.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)

#plotar grafico de proporção de mortes por faixa etaria a cada ano
proporcao.faixaeta.br.linhaii <- ggplot(faixaeta.br.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Faixa Etária",
       title = "% de Mortes por Psicoativos no Brasil por Faixa Etária e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.faixaeta.br.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_faixaeta_br_linhaii.png", 
       plot = proporcao.faixaeta.br.linhaii,
       width = 3.5, height = 2.5, dpi = 150)



#ES

#montar coluna de faixa etaria
dados_es_linhaii$FAIXAETA <- cut(dados_es_linhaii$IDADE2,
                                  breaks = c(-Inf, 12, 17, 30, 60, Inf),
                                  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
                                  right = TRUE) #inclusivo a direita (x-y]

#agrupar por faixa etaria
faixaeta.es.linhaii <- dados_es_linhaii %>%
  group_by(ANOOBITO, FAIXAETA) %>%
  summarise(N.OBITOS = n()) %>%
  ungroup() %>%
  group_by(ANOOBITO) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()


# Plotar gráfico de séries
series.faixaeta.es.linhaii <- ggplot(data = faixaeta.es.linhaii, aes(x = ANOOBITO, y = N.OBITOS, color = FAIXAETA)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Faixa Etária - variável LINHAII",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.faixaeta.es.linhaii)

#salvar grafico
ggsave(filename = "variavel_linhaii/series_faixaeta_es_linhaii.png", 
       plot = series.faixaeta.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)

#plotar grafico de proporção de mortes por faixa etaria a cada ano
proporcao.faixaeta.es.linhaii <- ggplot(faixaeta.es.linhaii, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Faixa Etária",
       title = "% de Mortes por Psicoativos no ES por Faixa Etária e Ano - Variável LINHAII") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.faixaeta.es.linhaii)

# Salvar gráfico
ggsave(filename = "variavel_linhaii/proporcao_faixaeta_es_linhaii.png", 
       plot = proporcao.faixaeta.es.linhaii,
       width = 3.5, height = 2.5, dpi = 150)

