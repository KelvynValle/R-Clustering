install.packages("WDI")
library(WDI) # baixar os dados do World Bank
library(magrittr)
install.packages("rmarkdown", repos = "https://cran.revolutionanalytics.com")

install.packages("formattable")
library(formattable)

WDIsearch("Inflation")

# lista de indicadores econômicos dos países:
lista_indicadores <- c("FP.CPI.TOTL.ZG", # inflação (%)
                       "NY.GDP.PCAP.CD", # Pib per capita (USD)
                       "NY.GDP.MKTP.KD.ZG", # crescimento do PIB anual (%),
                       "SL.UEM.TOTL.ZS" # Desemprego (%)
)


df2014 <- WDI(indicator = lista_indicadores, country = "all", start = 2014, end = 2014,
              extra = TRUE)
str(df2014)

# Ou você pode consultar os dados carregados com o comando abaixo
View(df2014)


df2014$region %<>% as.character
# Remover agregados
df2014 <- subset(df2014, region != "Aggregates")


dfi2014 <- df2014[, lista_indicadores]
row.names(dfi2014) <- df2014$country
colnames(dfi2014) <- c("Inflacao", "PIB_per_Capita", "Crescimento_PIB", "Desemprego")
summary(dfi2014)


# Podemos visualizar também estes dados utilizando o comando
View(dfi2014)


dfi2014 <- na.omit(dfi2014)
dfi2014$Desemprego <- 100 - dfi2014$Desemprego
names(dfi2014)[4] <- "Emprego"
View(dfi2014)



