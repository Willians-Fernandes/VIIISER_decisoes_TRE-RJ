# Carregamento de Pacotes........................................................

pacman::p_load(
tidyverse,
tidytext,
topicmodels,
topicdoc,
stopwords,
stringr,
tm,
ggplot2,
LDAvis,
webshot)

# Limpeza de Dados..............................................................

dje_decisoes$decisões = str_replace_all(dje_decisoes$decisões, "[0-9]", "")
dje_decisoes$decisões = removeWords(dje_decisoes$decisões, stopwords("pt"))
dje_decisoes$decisões = gsub("seçao", "seção", dje_decisoes$decisões)
dje_decisoes$decisões = gsub("eleicao", "eleição", dje_decisoes$decisões)
palavras_remover = c("ª", "º", "1º", "1°", "2º", "2°", "3º", "3°", "5º", "5°", "6º", "6°", "abel", "abril", "acessado", "ad", "advogada", "advogado", "advogados", "agosto", "ainda", "alexandre", "aline", "almeida", "alves", "amanda", "ana", "andrade", "ane", "ano", "antonio", "aparecida", "após", "araujo", "art", "artigo", "assim", "assinado", "augusto", "ayoub", "azevedo", "b", "barbosa", "barros", "batista", "beatriz", "bem", "br", "brasil", "bruno", "c", "campos", "cardoso", "carlos", "carolina", "carvalho", "castro", "chaves", "cinco", "conceição", "conforme", "cruz", "cristina", "costa", "cunha", "dado", "decisão", "desembargador", "desembargadora", "dez", "dezembro", "diário", "digitalmente", "documento", "dois", "domingo", "dr", "dê", "deste", "dje", "é", "edital", "eduardo", "eleitoral", "eletrônico", "endereço", "éo", "faz", "feira", "fevereiro", "fernanda", "fernandes", "ferreira", "fica", "fl", "fls", "fonseca", "freitas", "gaspar", "gonçalves", "gomes", "henrique", "http", "icp", "id", "infra", "i", "ii", "iii", "iv", "janeiro", "jesus", "jose", "josé", "julho", "juliana", "junho", "junior", "jus", "juiz", "juíza", "leonardo", "lima", "lo", "lopes", "lucas", "lucio", "luiz", "m", "machado", "maio", "marcelo", "março", "marques", "maria", "martins", "melo", "mendes", "mil", "monteiro", "moreira", "moraes", "n", "n°", "nº", "nascimento", "nesta", "neste", "nome", "nove", "novembro", "nunes", "número", "oab", "oito", "oliveira", "outubro", "p", "página", "parte", "paula", "paulo", "pedro", "pereira", "presidente", "pode", "podendo", "poderão", "publicá", "qualquer", "quarta", "quatro", "quinta", "r", "rafael", "ramos", "regional",  "reis", "requerente", "res", "ribeiro", "rio", "rj", "roberto", "rocha", "rodrigues", "rodrigo", "rosa", "s", "sábado", "santos", "segunda", "seis", "ser", "sergio", "sete", "setembro", "sexta", "silva", "soares", "sob", "sobre", "sousa", "souza", "terça", "teixeira", "topicos", "thiago", "trata", "tre", "três", "tribunal", "todos", "um", "uso", "vieira", "virem", "www", "ze", "zé", "zona")
dje_decisoes$decisões = str_replace_all(dje_decisoes$decisões, paste0("\\b(", paste(palavras_remover, collapse = "|"), ")\\b"), "")
dje_decisoes$decisões = str_squish(dje_decisoes$decisões)

# Modelagem de Tópicos..........................................................

dtm = dje_decisoes %>%
  unnest_tokens(word, decisões) %>%
  count(dec_id, word) %>%
  cast_dtm(dec_id, word, n)

modelagem_topicos = LDA(dtm, k = 12)

topicos = tidy(modelagem_topicos, matrix = "beta")
gamma = tidy(modelagem_topicos, matrix = "gamma")
save(modelagem_topicos, file = "modelagem_topicos.RData")

# Nomeando Tópicos..............................................................

nomes_topicos = c("Recursos e Embargos","Doações e Sigilo Fiscal","Partidos Políticos",
                  "Prestação de Contas de Campanha","Agravos e Recursos Especiais","Ação Penal",
                  "Propaganda Eleitoral","Registro de Candidatura","Prazos e Multas",
                  "Prestação de Contas Partidárias","Processo Judicial e Resoluções","Pedidos de Liminar")
topicos$topic = factor(topicos$topic, labels = nomes_topicos)
gamma$topic = factor(gamma$topic, labels = nomes_topicos)
doc_topicos = gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

# Combinando Informações........................................................

dje_decisoes = dje_decisoes %>%
  left_join(doc_topicos, by = c("dec_id" = "document"))

# Análise de Frequência por Tópicos.............................................

topicos_por_palavra = topicos %>%
  group_by(topic) %>%
  summarise(total_palavras = sum(beta)) %>%
  arrange(desc(total_palavras))

# Análise de Distribuição de Tópicos nos Documentos.............................

gamma = gamma %>%
  left_join(dje_decisoes %>% select(dec_id, ano), by = c("document" = "dec_id"))

distribuicao_anual = gamma %>%
  group_by(ano, topic) %>%
  summarise(total_gamma = sum(gamma)) %>%
  mutate(proporcao = total_gamma / sum(total_gamma)) %>%
  ungroup()

# Análise de Exclusividade......................................................

palavras_tfidf = topicos %>%
  group_by(topic) %>%
  top_n(1, beta)

exclusividade_palavras = topicos %>%
  semi_join(palavras_tfidf, by = c("term" = "term")) %>%
  group_by(topic, term) %>%
  summarise(importancia = sum(beta)) %>%
  mutate(porcentagem = importancia / sum(importancia))

# Análise de Coerência..........................................................

coerencia = topic_coherence(modelagem_topicos, dtm)
coerencia_dados = as.data.frame(coerencia)
coerencia_dados$Topic = nomes_topicos

# Gráficos......................................................................

cores = ggsci::pal_d3("category10")(10)
cores = c(cores, "#8dd3c7", "#f7ec0f")
topicos_ordenados = sort(unique(topicos$topic))
mapa_cores = setNames(cores, topicos_ordenados)

topicos %>%
  mutate(topic = factor(topic, levels = topicos_por_palavra$topic)) %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = mapa_cores, labels = paste("Tópico", topicos_por_palavra$topic, ": ", round(topicos_por_palavra$total_palavras, 2), " palavras")) +
  labs(x = "Palavras", y = "Porcentagem") +
  theme(strip.text = element_text(size = 14),  
        axis.text.y = element_text(size = 14))

ggplot(distribuicao_anual, aes(x = ano, y = proporcao, fill = topic)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = mapa_cores) + 
  scale_x_continuous(breaks = seq(2011, 2022, by = 1)) +
  labs(x = "Ano", y = "Proporção", fill = "Tópicos") +
  theme(legend.text = element_text(size = 14))

ggplot(exclusividade_palavras, aes(x = reorder(term, porcentagem), y = porcentagem, fill = as.factor(topic))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = mapa_cores) +
  labs(x = "Palavras", y = "Porcentagem", fill = "Tópicos") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 14))

ggplot(coerencia_dados, aes(x = reorder(Topic, coerencia), y = coerencia, fill = Topic)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = mapa_cores) +
  labs(x = "Tópico", y = "Coerência") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 14))