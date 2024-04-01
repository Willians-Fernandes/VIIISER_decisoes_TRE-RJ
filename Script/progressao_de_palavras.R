# Carregamento de Pacotes.......................................................

pacman::p_load (
ggbump,
dplyr,
ggplot2,
ggrepel,
ggsci,
tidyr)

# Organização do Dataframe......................................................

palavras_chave = aggregate(Frequência ~ Palavras, data = decisoes_freq_anual, sum)
palavras_chave = palavras_chave[order(-palavras_chave$Frequência),]
palavras_frequentes_20 = head(palavras_chave$Palavras, 20)
grafico = subset(decisoes_freq_anual, Palavras %in% palavras_frequentes_20)
grafico$Classificação = ave(grafico$Frequência, grafico$Ano, FUN = function(x) rank(-x, ties.method = "first"))

# Definição de Cores............................................................

cores = pal_d3("category20")(20)

# Análise de Frequência.........................................................

ggplot(grafico, aes(x = Ano, y = Frequência, group = Palavras, color = Palavras)) +
  geom_bump(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = cores) +
  labs(title = "Progressão de Palavras",
       subtitle = "Análise de frequência de palavras por ano",
       x = "Ano", y = "Frequência",
       caption = "Fonte: Diário de Justiça Eletrônico") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16), 
        legend.text = element_text(size = 14)) +  
  guides(color = guide_legend(title = "Palavra"))
