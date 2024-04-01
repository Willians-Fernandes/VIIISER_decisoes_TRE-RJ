# Carregamento de Pacotes.......................................................

pacman::p_load (
dplyr,
stringr,
readtext,
tidyr,
tidytext,
tm,
waldo,
stopwords,
quanteda,
quanteda.textstats,
flextable,
magick)

# Criação de Dataframe..........................................................

dje_2011 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2011/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2012 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2012/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2013 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2013/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2014 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2014/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2015 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2015/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2016 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2016/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2017 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2017/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2018 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2018/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2019 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2019/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2020 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2020/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2021 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2021/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))
dje_2022 = readtext("C:/Users/willi/Desktop/Diário de Justiça Eletrônico - TRE-RJ/2022/*",
                    docvarsfrom = "filenames",
                    dvsep = "_",
                    docvarnames = c("ano","edição"))

# Limpeza - Diário Nº 65 de 2017................................................

x = "\\[b.*?\\]|\\[/.*?\\]"
dje_2017$text = gsub(x, "", dje_2017$text)

# Captura de Decisões...........................................................

pro = function(text) {
  text = str_replace_all(text, "[[:punct:]]", " ")
  text = str_to_lower(text)
  return(text)}

analise = '(?i)decis[ãa]o(.*?)(?:(?:\\n(?:despacho|ac[óo]rd[ãa]o)s?)|(?:\\b\\w+\\b\\s*)+,\\s*(?:\\d{2}/\\d{2}/\\d{4}|\\d{1,2}\\s+de\\s+\\w+\\s+de\\s+\\d{4}))'
remover_da_analise = '(?:(?:\\n(?:despacho|ac[óo]rd[ãa]o)s?)|(?:\\b\\w+\\b\\s*)+,\\s*(?:\\d{2}/\\d{2}/\\d{4}|\\d{1,2}\\s+de\\s+\\w+\\s+de\\s+\\d{4}))'

dje_2011_decisoes = dje_2011 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2011_decisoes$text = str_squish(dje_2011_decisoes$text)
dje_2011_decisoes$decisões = str_replace(dje_2011_decisoes$decisões, regex(remover_da_analise), "")

dje_2012_decisoes = dje_2012 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2012_decisoes$text = str_squish(dje_2012_decisoes$text)
dje_2012_decisoes$decisões = str_replace(dje_2012_decisoes$decisões, regex(remover_da_analise), "")

dje_2013_decisoes = dje_2013 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2013_decisoes$text = str_squish(dje_2013_decisoes$text)
dje_2013_decisoes$decisões = str_replace(dje_2013_decisoes$decisões, regex(remover_da_analise), "")

dje_2014_decisoes = dje_2014 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2014_decisoes$text = str_squish(dje_2014_decisoes$text)
dje_2014_decisoes$decisões = str_replace(dje_2014_decisoes$decisões, regex(remover_da_analise), "")

dje_2015_decisoes = dje_2015 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2015_decisoes$text = str_squish(dje_2015_decisoes$text)
dje_2015_decisoes$decisões = str_replace(dje_2015_decisoes$decisões, regex(remover_da_analise), "")

dje_2016_decisoes = dje_2016 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2016_decisoes$text = str_squish(dje_2016_decisoes$text)
dje_2016_decisoes$decisões = str_replace(dje_2016_decisoes$decisões, regex(remover_da_analise), "")

dje_2017_decisoes = dje_2017 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2017_decisoes$text = str_squish(dje_2017_decisoes$text)
dje_2017_decisoes$decisões = str_replace(dje_2017_decisoes$decisões, regex(remover_da_analise), "")

dje_2018_decisoes = dje_2018 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2018_decisoes$text = str_squish(dje_2018_decisoes$text)
dje_2018_decisoes$decisões = str_replace(dje_2018_decisoes$decisões, regex(remover_da_analise), "")

dje_2019_decisoes = dje_2019 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2019_decisoes$text = str_squish(dje_2019_decisoes$text)
dje_2019_decisoes$decisões = str_replace(dje_2019_decisoes$decisões, regex(remover_da_analise), "")

dje_2020_decisoes = dje_2020 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2020_decisoes$text = str_squish(dje_2020_decisoes$text)
dje_2020_decisoes$decisões = str_replace(dje_2020_decisoes$decisões, regex(remover_da_analise), "")

dje_2021_decisoes = dje_2021 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2021_decisoes$text = str_squish(dje_2021_decisoes$text)
dje_2021_decisoes$decisões = str_replace(dje_2021_decisoes$decisões, regex(remover_da_analise), "")

dje_2022_decisoes = dje_2022 %>%
  mutate(decisões = str_extract_all(text, regex(analise, dotall = TRUE))) %>%
  unnest(decisões) %>%
  mutate(decisões = pro(decisões))
dje_2022_decisoes$text = str_squish(dje_2022_decisoes$text)
dje_2022_decisoes$decisões = str_replace(dje_2022_decisoes$decisões, regex(remover_da_analise), "")

# Combinação de Dataframes......................................................

dje_decisoes = bind_rows(dje_2011_decisoes, dje_2012_decisoes, dje_2013_decisoes,
                         dje_2014_decisoes, dje_2015_decisoes, dje_2016_decisoes,
                         dje_2017_decisoes, dje_2018_decisoes, dje_2019_decisoes,
                         dje_2020_decisoes, dje_2021_decisoes, dje_2022_decisoes)

# Limpeza de Dados..............................................................

dje_decisoes$decisões = str_squish(dje_decisoes$decisões)
dje_decisoes$decisões = str_replace(dje_decisoes$decisões, "(?i)rio de janeiro\\s+\\d{2}\\s+\\d{2}\\s+\\d{4}.*$", "")

# Comparação (1)................................................................

dje_decisoes = dje_decisoes %>% filter(doc_id=="2013_004.pdf")
dje_2013 = dje_2013  %>% filter(doc_id=="2013_004.pdf")
dje_2013$text = str_squish(dje_2013$text)

nchar(dje_2013[,'text']) #236789
nchar(dje_decisoes[1,'text']) #236789

# Comparação (2)................................................................

compare(dje_2013$text, head(dje_decisoes$text, 1))

# Análise de Frequências........................................................

dje_decisoes$dec_id = paste0("dec_", seq_len(nrow(dje_decisoes)))

decisoes_corpus = Corpus(VectorSource(dje_decisoes$decisões))
decisoes_texto = as.character(decisoes_corpus)

decisoes = tokens(decisoes_texto,
                  remove_numbers = TRUE,
                  remove_symbols = TRUE,
                  remove_punct = TRUE,
                  remove_separators = TRUE) %>%
  tokens_remove(pattern = c(stopwords(language = "pt"),
                            "ª", "º", "1º", "1°", "2º", "2°", "3º", "3°", "5º", "5°", "6º", "6°", "abel", "abril", "acessado", "ad", "advogada", "advogado", "advogados", "agosto", "ainda", "alexandre", "aline", "almeida", "alves", "amanda", "ana", "andrade", "ane", "ano", "antonio", "aparecida", "após", "araujo", "art", "artigo", "assim", "assinado", "augusto", "ayoub", "azevedo", "b", "barbosa", "barros", "batista", "beatriz", "bem", "br", "brasil", "bruno", "c", "campos", "cardoso", "carlos", "carolina", "carvalho", "castro", "chaves", "cinco", "conceição", "conforme", "cruz", "cristina", "costa", "cunha", "dado", "decisão", "desembargador", "desembargadora", "dez", "dezembro", "diário", "digitalmente", "documento", "dois", "domingo", "dr", "dê", "deste", "dje", "é", "edital", "eduardo", "eleitoral", "eletrônico", "endereço", "éo", "faz", "feira", "fevereiro", "fernanda", "fernandes", "ferreira", "fica", "fl", "fls", "fonseca", "freitas", "gaspar", "gonçalves", "gomes", "henrique", "http", "icp", "id", "infra", "i", "ii", "iii", "iv", "janeiro", "jesus", "jose", "josé", "julho", "juliana", "junho", "junior", "jus", "juiz", "juíza", "leonardo", "lima", "lo", "lopes", "lucas", "lucio", "luiz", "m", "machado", "maio", "marcelo", "março", "marques", "maria", "martins", "melo", "mendes", "mil", "monteiro", "moreira", "moraes", "n", "n°", "nº", "nascimento", "nesta", "neste", "nome", "nove", "novembro", "nunes", "número", "oab", "oito", "oliveira", "outubro", "p", "página", "parte", "paula", "paulo", "pedro", "pereira", "presidente", "pode", "podendo", "poderão", "publicá", "qualquer", "quarta", "quatro", "quinta", "r", "rafael", "ramos", "regional",  "reis", "requerente", "res", "ribeiro", "rio", "rj", "roberto", "rocha", "rodrigues", "rodrigo", "rosa", "s", "sábado", "santos", "segunda", "seis", "ser", "sergio", "sete", "setembro", "sexta", "silva", "soares", "sob", "sobre", "sousa", "souza", "terça", "teixeira", "termos", "thiago", "trata", "tre", "três", "tribunal", "todos", "um", "uso", "vieira", "virem", "www", "ze", "zé", "zona",
                            padding= FALSE))

decisoes_dfm = dfm(decisoes)
decisoes_freq_total_10 = textstat_frequency(decisoes_dfm, n = 10)
decisoes_freq_total_100 = textstat_frequency(decisoes_dfm, n = 100)


# Tabela de Frequência Total....................................................

decisoes_tabela_total_10 = flextable(decisoes_freq_total_10)
decisoes_tabela_total_10 = theme_box(decisoes_tabela_total_10)
imagem = image_graph(width = 800, height = 600, res = 100)
print(decisoes_tabela_total_10)

# Tabela de Frequência por Ano..................................................

decisoes_anual = dje_decisoes %>%
  group_by(ano) %>%
  summarize(decisoes = paste(decisões, collapse = " ")) %>%
  ungroup()
decisoes_corpus_anual = corpus(decisoes_anual$decisoes)
decisoes_tokens_anual = tokens(decisoes_corpus_anual,
                               remove_numbers = TRUE,
                               remove_symbols = TRUE,
                               remove_punct = TRUE,
                               remove_separators = TRUE) %>%
  tokens_remove(pattern = c(stopwords(language = "pt"),
                            "ª", "º", "1º", "1°", "2º", "2°", "3º", "3°", "5º", "5°", "6º", "6°", "abel", "abril", "acessado", "ad", "advogada", "advogado", "advogados", "agosto", "ainda", "alexandre", "aline", "almeida", "alves", "amanda", "ana", "andrade", "ane", "ano", "antonio", "aparecida", "após", "araujo", "art", "artigo", "assim", "assinado", "augusto", "ayoub", "azevedo", "b", "barbosa", "barros", "batista", "beatriz", "bem", "br", "brasil", "bruno", "c", "campos", "cardoso", "carlos", "carolina", "carvalho", "castro", "chaves", "cinco", "conceição", "conforme", "cruz", "cristina", "costa", "cunha", "dado", "decisão", "desembargador", "desembargadora", "dez", "dezembro", "diário", "digitalmente", "documento", "dois", "domingo", "dr", "dê", "deste", "dje", "é", "edital", "eduardo", "eleitoral", "eletrônico", "endereço", "éo", "faz", "feira", "fevereiro", "fernanda", "fernandes", "ferreira", "fica", "fl", "fls", "fonseca", "freitas", "gaspar", "gonçalves", "gomes", "henrique", "http", "icp", "id", "infra", "i", "ii", "iii", "iv", "janeiro", "jesus", "jose", "josé", "julho", "juliana", "junho", "junior", "jus", "juiz", "juíza", "leonardo", "lima", "lo", "lopes", "lucas", "lucio", "luiz", "m", "machado", "maio", "marcelo", "março", "marques", "maria", "martins", "melo", "mendes", "mil", "monteiro", "moreira", "moraes", "n", "n°", "nº", "nascimento", "nesta", "neste", "nome", "nove", "novembro", "nunes", "número", "oab", "oito", "oliveira", "outubro", "p", "página", "parte", "paula", "paulo", "pedro", "pereira", "presidente", "pode", "podendo", "poderão", "publicá", "qualquer", "quarta", "quatro", "quinta", "r", "rafael", "ramos", "regional",  "reis", "requerente", "res", "ribeiro", "rio", "rj", "roberto", "rocha", "rodrigues", "rodrigo", "rosa", "s", "sábado", "santos", "segunda", "seis", "ser", "sergio", "sete", "setembro", "sexta", "silva", "soares", "sob", "sobre", "sousa", "souza", "terça", "teixeira", "termos", "thiago", "trata", "tre", "três", "tribunal", "todos", "um", "uso", "vieira", "virem", "www", "ze", "zé", "zona",
                            padding= FALSE))

decisoes_dfm_anual = dfm(decisoes_tokens_anual)
groups_anual = decisoes_anual$ano
decisoes_freq_anual = textstat_frequency(decisoes_dfm_anual, groups = groups_anual, n = 100)
decisoes_freq_anual$docfreq = NULL
names(decisoes_freq_anual)[1] = "Palavras"
names(decisoes_freq_anual)[2] = "Frequência"
names(decisoes_freq_anual)[3] = "Classificação"
names(decisoes_freq_anual)[4] = "Ano"