agressoes <- read.csv2("data/agressoes.csv", fileEncoding = "UTF-8")
ideb <- read.csv2("data/ideb.csv", fileEncoding = "UTF-8")
mortalidade <- read.csv2("data/mortalidade.csv", fileEncoding = "UTF-8")
nascimentos <- read.csv2("data/nascimentos.csv", fileEncoding = "UTF-8")
pib <- read.csv2("data/pib.csv", fileEncoding = "UTF-8")
populacao <- read.csv2("data/populacao.csv", fileEncoding = "UTF-8")

#Arrumando as bases
agressoes$cod_mun <- sub(" .*", "", agressoes$Município)
agressoes$nome_mun <- sub("^[^ ]+ ", "", agressoes$Município)
names(agressoes)[names(agressoes) == "Óbitos_p.Ocorrênc"] <- "obitos_agressao"
agressoes$Município <- NULL

mortalidade$cod_mun <- sub(" .*", "", mortalidade$Município)
mortalidade$Município <- NULL

nascimentos$cod_mun <- sub(" .*", "", nascimentos$Município)
nascimentos$Município <- NULL

populacao$cod_mun <- sub(" .*", "", populacao$Município)
populacao$Município <- NULL

pib$cod_mun <- substr(pib$Código.do.Município, 1, nchar(pib$Código.do.Município) - 1)
pib$Nome.do.Município <- NULL
pib$Código.do.Município <- NULL

ideb$cod_mun <- substr(ideb$codigo, 1, nchar(ideb$codigo) - 1)
ideb$municipio <- NULL
ideb$codigo <- NULL

raw_data <- merge(agressoes, mortalidade, by = "cod_mun", all.x = TRUE)
raw_data <- raw_data[, c(1, 3, 2, 4)]
raw_data <- merge(raw_data, nascimentos, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, populacao, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, pib, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, ideb, by = "cod_mun", all.x = TRUE)

raw_data[] <- lapply(raw_data, function(x) replace(x, x == "-", 0))
raw_data <- data.frame(lapply(raw_data, type.convert, as.is = TRUE))

raw_data$porc_agropecuaria <- round(raw_data$Valor.agropecuária / raw_data$Total, 2)
raw_data$porc_industria <- round(raw_data$Valor.Indústria / raw_data$Total, 2)

#IDEB para númerico
raw_data$ideb <- as.numeric(gsub(",", ".", raw_data$ideb))

#Transformando as taxas
raw_data$taxa_mortalidade <- round((raw_data$Óbitos_p.Ocorrênc / raw_data$População_estimada) * 100000, 2)
raw_data$taxa_mort_agressao <- round((raw_data$obitos_agressao / raw_data$População_estimada) * 100000, 2)
raw_data$taxa_nascimentos <- round((raw_data$Nascim_p.ocorrênc / raw_data$População_estimada) * 100000, 2)

cols_remover <- c("Valor.agropecuária", "Valor.Indústria", "Valor.Serviços", 'Valor.Administrativo', 'Total', 'Impostos',
                  'PIB', 'obitos_agressao', 'Óbitos_p.Ocorrênc', 'Nascim_p.ocorrênc')

raw_data <- raw_data[, !(names(raw_data) %in% cols_remover)]

colnames(raw_data)
names(raw_data)[names(raw_data) == "População_estimada"] <- "populacao"
names(raw_data)[names(raw_data) == "PIB.per.capita"] <- "pib_percap"

write.csv2(raw_data, 'data/dados.csv', row.names = FALSE)
