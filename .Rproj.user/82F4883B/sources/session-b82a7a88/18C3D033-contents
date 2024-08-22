#VARIAVEL ATESTADO

#criar pasta para salvar os graficos dessa variavel
dir.create("variavel_atestado")



#--------- SEXO

#BR

# Agrupar dados por sexo e calcular porcentagem
sexo.br.atestado <- dados_br_atestado %>%
  group_by(ANOOBITO, SEXO) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.sexo.br.atestado <- ggplot(data = sexo.br.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Sexo - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.sexo.br.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_sexo_br_atestado.png", 
       plot = series.sexo.br.atestado, 
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.sexo.br.atestado <- ggplot(sexo.br.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Sexo",
       title = "% de Mortes por Psicoativos no Brasil por Sexo e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.sexo.br.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_sexo_br_atestado.png", 
       plot = proporcao.sexo.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por genero e calcular porcentagem
sexo.es.atestado <- dados_es_atestado %>%
  group_by(ANOOBITO, SEXO) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.sexo.es.atestado <- ggplot(data = sexo.es.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 10) +
  labs(title = "Número de óbitos no ES por Sexo - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.sexo.es.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_sexo_es_atestado.png", 
       plot = series.sexo.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.sexo.es.atestado <- ggplot(sexo.es.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Sexo",
       title = "% de Mortes por Psicoativos no ES por Sexo e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.sexo.es.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_sexo_es_atestado.png", 
       plot = proporcao.sexo.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)





#------------- RAÇA

#BR

# Agrupar dados por raça e calcular porcentagem
raca.br.atestado <- dados_br_atestado %>%
  group_by(ANOOBITO, RACACOR) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.raca.br.atestado <- ggplot(data = raca.br.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Raça/Cor - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.raca.br.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_raca_br_atestado.png", 
       plot = series.raca.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por raça a cada ano
proporcao.raca.br.atestado <- ggplot(raca.br.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça/Cor",
       title = "% de Mortes por Psicoativos no Brasil por Raça/Cor e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.raca.br.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_raca_br_atestado.png", 
       plot = proporcao.raca.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por raça e calcular porcentagem
raca.es.atestado <- dados_es_atestado %>%
  group_by(ANOOBITO, RACACOR) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.raca.es.atestado <- ggplot(data = raca.es.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Raça/Cor - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.raca.es.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_raca_es_atestado.png", 
       plot = series.raca.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por raça a cada ano
proporcao.raca.es.atestado <- ggplot(raca.es.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça/Cor",
       title = "% de Mortes por Psicoativos no ES por Raça/Cor e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 13),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.raca.es.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_raca_es_atestado.png", 
       plot = proporcao.raca.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)





#---------- ESCOLARIDADE

#BR

# Agrupar dados por escolaridade e calcular porcentagem
esc.br.atestado <- dados_br_atestado %>%
  group_by(ANOOBITO, ESC) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.esc.br.atestado <- ggplot(data = esc.br.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Escolaridade - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.esc.br.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_esc_br_atestado.png", 
       plot = series.esc.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por escolaridade a cada ano
proporcao.esc.br.atestado <- ggplot(esc.br.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESC)) +
  geom_bar(stat = "identity") + 
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "% de Mortes por Psicoativos no Brasil por Escolaridade e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.esc.br.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_esc_br_atestado.png", 
       plot = proporcao.esc.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por escolaridade e calcular porcentagem
esc.es.atestado <- dados_es_atestado %>%
  group_by(ANOOBITO, ESC) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.esc.es.atestado <- ggplot(data = esc.es.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Escolaridade - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.esc.es.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_esc_es_atestado.png", 
       plot = series.esc.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)


#plotar grafico de proporção de mortes por escolaridade a cada ano
proporcao.esc.es.atestado <- ggplot(esc.es.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESC)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Escolaridade",
       title = "% de Mortes por Psicoativos no ES por Escolaridade e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.esc.es.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_esc_es_atestado.png", 
       plot = proporcao.esc.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)





#ESTADO CIVIL

#BR

# Agrupar dados por estado civil e calcular porcentagem
estciv.br.atestado <- dados_br_atestado %>%
  group_by(ANOOBITO, ESTCIV) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.estciv.br.atestado <- ggplot(data = estciv.br.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Estado Civil - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.estciv.br.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_estciv_br_atestado.png", 
       plot = series.estciv.br.atestado, 
       width = 3.5, height = 2.5, dpi = 150)


#plotar grafico de proporção de mortes por sexo a cada ano
proporcao.estciv.br.atestado <- ggplot(estciv.br.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Estado Civil",
       title = "% de Mortes por Psicoativos no Brasil por Estado Civil e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.estciv.br.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_estciv_br_atestado.png", 
       plot = proporcao.estciv.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#ES

# Agrupar dados por estado civil e calcular porcentagem
estciv.es.atestado <- dados_es_atestado %>%
  group_by(ANOOBITO, ESTCIV) %>%
  summarise(N.OBITOS = n()) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS, na.rm = TRUE)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()



# Plotar gráfico de séries
series.estciv.es.atestado <- ggplot(data = estciv.es.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Estado Civil - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.estciv.es.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_estciv_es_atestado.png", 
       plot = series.estciv.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#plotar grafico de proporção de mortes por estciv a cada ano
proporcao.estciv.es.atestado <- ggplot(estciv.es.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Estado Civil",
       title = "% de Mortes por Psicoativos no ES por Estado Civil e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.estciv.es.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_estciv_es_atestado.png", 
       plot = proporcao.estciv.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)






#FAIXA ETARIA

#BR

#montar coluna de faixa etaria
dados_br_atestado$FAIXAETA <- cut(dados_br_atestado$IDADE2,
                       breaks = c(-Inf, 12, 17, 30, 60, Inf),
                       labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
                       right = TRUE) #inclusivo a direita (x-y]

#agrupar por faixa etaria
faixaeta.br.atestado <- dados_br_atestado %>%
  group_by(ANOOBITO, FAIXAETA) %>%
  summarise(N.OBITOS = n()) %>%
  ungroup() %>%
  group_by(ANOOBITO) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()


# Plotar gráfico de séries
series.faixaeta.br.atestado <- ggplot(data = faixaeta.br.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = FAIXAETA)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no Brasil por Faixa Etária - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.faixaeta.br.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_faixaeta_br_atestado.png", 
       plot = series.faixaeta.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)

#plotar grafico de proporção de mortes por faixa etaria a cada ano
proporcao.faixaeta.br.atestado <- ggplot(faixaeta.br.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Faixa Etária",
       title = "% de Mortes por Psicoativos no Brasil por Faixa Etária e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.faixaeta.br.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_faixaeta_br_atestado.png", 
       plot = proporcao.faixaeta.br.atestado,
       width = 3.5, height = 2.5, dpi = 150)



#ES

#montar coluna de faixa etaria
dados_es_atestado$FAIXAETA <- cut(dados_es_atestado$IDADE2,
                                  breaks = c(-Inf, 12, 17, 30, 60, Inf),
                                  labels = c("0-12 anos", "13-17 anos", "18-30 anos", "31-60 anos", "60+ anos"),
                                  right = TRUE) #inclusivo a direita (x-y]

#agrupar por faixa etaria
faixaeta.es.atestado <- dados_es_atestado %>%
  group_by(ANOOBITO, FAIXAETA) %>%
  summarise(N.OBITOS = n()) %>%
  ungroup() %>%
  group_by(ANOOBITO) %>%
  mutate(TOTAL_ANO = sum(N.OBITOS)) %>%
  mutate(PERCENTUAL = (N.OBITOS / TOTAL_ANO) * 100) %>%
  as.data.frame()


# Plotar gráfico de séries
series.faixaeta.es.atestado <- ggplot(data = faixaeta.es.atestado, aes(x = ANOOBITO, y = N.OBITOS, color = FAIXAETA)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15) +
  labs(title = "Número de óbitos no ES por Faixa Etária - variável ATESTADO",
       x = "Anos", y = "Óbitos Totais") +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

# Exibir o gráfico
print(series.faixaeta.es.atestado)

#salvar grafico
ggsave(filename = "variavel_atestado/series_faixaeta_es_atestado.png", 
       plot = series.faixaeta.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)

#plotar grafico de proporção de mortes por faixa etaria a cada ano
proporcao.faixaeta.es.atestado <- ggplot(faixaeta.es.atestado, aes(x = factor(ANOOBITO), y = PERCENTUAL, fill = FAIXAETA)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Faixa Etária",
       title = "% de Mortes por Psicoativos no ES por Faixa Etária e Ano - Variável ATESTADO") +
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(size = 10))

# Exibir o gráfico
print(proporcao.faixaeta.es.atestado)

# Salvar gráfico
ggsave(filename = "variavel_atestado/proporcao_faixaeta_es_atestado.png", 
       plot = proporcao.faixaeta.es.atestado,
       width = 3.5, height = 2.5, dpi = 150)

