# Carregamento de Pacotes.......................................................

pacman::p_load (
quanteda,
dplyr,
tm,
ggplot2)

# Seleção de Termos.............................................................

termos_ano = decisoes_freq_anual %>%
  filter(Classificação <= 20) %>%
  group_by(Ano) %>%
  summarise(Termos = list(Palavras))

# TF-IDF Total..................................................................

corpus_total = Corpus(VectorSource(dje_decisoes$decisões))
corpus_total = as.character(corpus_total)
dfm_total = dfm(corpus_total)
tfidf_total = dfm_tfidf(dfm_total)
termos = c("contas", "recurso", "relator", "justiça", "tse", "prestação", "voto", "mp", "brasileira", "públicas", "candidato", "institui", "unanimidade", "lei", "autos", "resolução", "processo", "pedido", "cargo", "estrutura")
tfidf_termos_total = tfidf_total[, termos]
importancia_total = data.frame(Palavras = termos, Importância = tfidf_termos_total@x)

saveRDS(importancia_total, file = "C:/Users/willi/Desktop/Banco de Dados/TF_IDF/importancia_total.rds")

# TF-IDF Por Ano................................................................

corpus_2011 = Corpus(VectorSource(dje_2011_decisoes$decisões))
corpus_2011 = as.character(corpus_2011)
dfm_2011 = dfm(corpus_2011)
tfidf_2011 = dfm_tfidf(dfm_2011)
termos_2011 = termos_ano$Termos[[1]]
tfidf_termos_2011 = tfidf_2011[, termos_2011]
importancia_2011 = data.frame(Palavras = termos_2011, Importância = tfidf_termos_2011@x)
importancia_2011$Ano = 2011

corpus_2012 = Corpus(VectorSource(dje_2012_decisoes$decisões))
corpus_2012 = as.character(corpus_2012)
dfm_2012 = dfm(corpus_2012)
tfidf_2012 = dfm_tfidf(dfm_2012)
termos_2012 = termos_ano$Termos[[2]]
tfidf_termos_2012 = tfidf_2012[, termos_2012]
importancia_2012 = data.frame(Palavras = termos_2012, Importância = tfidf_termos_2012@x)
importancia_2012$Ano = 2012

corpus_2013 = Corpus(VectorSource(dje_2013_decisoes$decisões))
corpus_2013 = as.character(corpus_2013)
dfm_2013 = dfm(corpus_2013)
tfidf_2013 = dfm_tfidf(dfm_2013)
termos_2013 = termos_ano$Termos[[3]]
tfidf_termos_2013 = tfidf_2013[, termos_2013]
importancia_2013 = data.frame(Palavras = termos_2013, Importância = tfidf_termos_2013@x)
importancia_2013$Ano = 2013

corpus_2014 = Corpus(VectorSource(dje_2014_decisoes$decisões))
corpus_2014 = as.character(corpus_2014)
dfm_2014 = dfm(corpus_2014)
tfidf_2014 = dfm_tfidf(dfm_2014)
termos_2014 = termos_ano$Termos[[4]]
tfidf_termos_2014 = tfidf_2014[, termos_2014]
importancia_2014 = data.frame(Palavras = termos_2014, Importância = tfidf_termos_2014@x)
importancia_2014$Ano = 2014

corpus_2015 = Corpus(VectorSource(dje_2015_decisoes$decisões))
corpus_2015 = as.character(corpus_2015)
dfm_2015 = dfm(corpus_2015)
tfidf_2015 = dfm_tfidf(dfm_2015)
termos_2015 = termos_ano$Termos[[5]]
tfidf_termos_2015 = tfidf_2015[, termos_2015]
importancia_2015 = data.frame(Palavras = termos_2015, Importância = tfidf_termos_2015@x)
importancia_2015$Ano = 2015

corpus_2016 = Corpus(VectorSource(dje_2016_decisoes$decisões))
corpus_2016 = as.character(corpus_2016)
dfm_2016 = dfm(corpus_2016)
tfidf_2016 = dfm_tfidf(dfm_2016)
termos_2016 = termos_ano$Termos[[6]]
tfidf_termos_2016 = tfidf_2016[, termos_2016]
importancia_2016 = data.frame(Palavras = termos_2016, Importância = tfidf_termos_2016@x)
importancia_2016$Ano = 2016

corpus_2017 = Corpus(VectorSource(dje_2017_decisoes$decisões))
corpus_2017 = as.character(corpus_2017)
dfm_2017 = dfm(corpus_2017)
tfidf_2017 = dfm_tfidf(dfm_2017)
termos_2017 = termos_ano$Termos[[7]]
tfidf_termos_2017 = tfidf_2017[, termos_2017]
importancia_2017 = data.frame(Palavras = termos_2017, Importância = tfidf_termos_2017@x)
importancia_2017$Ano = 2017

corpus_2018 = Corpus(VectorSource(dje_2018_decisoes$decisões))
corpus_2018 = as.character(corpus_2018)
dfm_2018 = dfm(corpus_2018)
tfidf_2018 = dfm_tfidf(dfm_2018)
termos_2018 = termos_ano$Termos[[8]]
tfidf_termos_2018 = tfidf_2018[, termos_2018]
importancia_2018 = data.frame(Palavras = termos_2018, Importância = tfidf_termos_2018@x)
importancia_2018$Ano = 2018

corpus_2019 = Corpus(VectorSource(dje_2019_decisoes$decisões))
corpus_2019 = as.character(corpus_2019)
dfm_2019 = dfm(corpus_2019)
tfidf_2019 = dfm_tfidf(dfm_2019)
termos_2019 = termos_ano$Termos[[9]]
tfidf_termos_2019 = tfidf_2019[, termos_2019]
importancia_2019 = data.frame(Palavras = termos_2019, Importância = tfidf_termos_2019@x)
importancia_2019$Ano = 2019

corpus_2020 = Corpus(VectorSource(dje_2020_decisoes$decisões))
corpus_2020 = as.character(corpus_2020)
dfm_2020 = dfm(corpus_2020)
tfidf_2020 = dfm_tfidf(dfm_2020)
termos_2020 = termos_ano$Termos[[10]]
tfidf_termos_2020 = tfidf_2020[, termos_2020]
importancia_2020 = data.frame(Palavras = termos_2020, Importância = tfidf_termos_2020@x)
importancia_2020$Ano = 2020

corpus_2021 = Corpus(VectorSource(dje_2021_decisoes$decisões))
corpus_2021 = as.character(corpus_2021)
dfm_2021 = dfm(corpus_2021)
tfidf_2021 = dfm_tfidf(dfm_2021)
termos_2021 = termos_ano$Termos[[11]]
tfidf_termos_2021 = tfidf_2021[, termos_2021]
importancia_2021 = data.frame(Palavras = termos_2021, Importância = tfidf_termos_2021@x)
importancia_2021$Ano = 2021

corpus_2022 = Corpus(VectorSource(dje_2022_decisoes$decisões))
corpus_2022 = as.character(corpus_2022)
dfm_2022 = dfm(corpus_2022)
tfidf_2022 = dfm_tfidf(dfm_2022)
termos_2022 = termos_ano$Termos[[12]]
tfidf_termos_2022 = tfidf_2022[, termos_2022]
importancia_2022 = data.frame(Palavras = termos_2022, Importância = tfidf_termos_2022@x)
importancia_2022$Ano = 2022

# Combinação de Dataframes......................................................

importancia_por_ano = bind_rows(importancia_2011, importancia_2012, importancia_2013,
                                importancia_2014, importancia_2015, importancia_2016,
                                importancia_2017, importancia_2018, importancia_2019,
                                importancia_2020, importancia_2021, importancia_2022)

saveRDS(importancia_por_ano, file = "C:/Users/willi/Desktop/Banco de Dados/TF_IDF/importancia_por_ano.rds")

# Preparação de Dados Para Visualização.........................................

importancia_por_ano_geral = importancia_por_ano %>%
  group_by(Palavras) %>%
  summarise(Importância = sum(Importância))
palavras_30 = importancia_por_ano_geral %>%
  arrange(desc(Importância)) %>%
  head(30)
importancia_30_ordenados = importancia_por_ano %>%
  filter(Palavras %in% palavras_30$Palavras)

# Gráfico.......................................................................

ggplot(importancia_30_ordenados, aes(x = Ano, y = Importância, fill = Palavras)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Palavras, ncol = 5) +
  scale_x_continuous(breaks = seq(2011, 2022, by = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  theme(strip.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12))

