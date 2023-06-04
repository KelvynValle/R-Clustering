install.packages("WDI")
library(WDI) # baixar os dados do World Bank
library(magrittr)
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
dfi2014_escala <- scale(dfi2014)
# Conferindo o resultado para o Brasil
dfi2014_escala["Brazil", ]
# referencia: http://www.statmethods.net/advstats/cluster.html
wss <- (nrow(dfi2014_escala)-1)*sum(apply(dfi2014_escala,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dfi2014_escala,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Número de Grupos",
     ylab="Soma dos quadrados dentro dos grupos") 
dendo <- dfi2014_escala %>% dist %>% hclust
plot(dendo)
rect.hclust(dendo, k = 4, border = "blue")
rect.hclust(dendo, k = 5, border = "red")
rect.hclust(dendo, k = 8, border = "green")

##################
library(cluster)
library(fpc)
grupos <- kmeans(dfi2014_escala, centers=5)
clusplot(dfi2014_escala, grupos$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
grupos <- kmeans(dfi2014_escala, centers=4)
clusplot(dfi2014_escala, grupos$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
dfi2014_escala[c("Brazil", "Chile", "Colombia", "Norway", "United States"),] %>% dist
mat_brasil <- dfi2014_escala %>% dist(diag = TRUE, upper = TRUE) %>% as.matrix
mat_brasil[, "Brazil"] %>% sort() %>% head(6)
# fixar uma seed (semente) para garantir a reprodutibilidade da análise:
set.seed(123)
# criar os clusters ou grupos
lista_clusteres <- kmeans(dfi2014_escala, centers = 5)$cluster
# função customizada para calcular a média dos indicadores para cada cluster
cluster.summary <- function(data, groups) {
  x <- round(aggregate(data, list(groups), mean), 2)
  x$qtd <- as.numeric(table(groups))
  # colocar coluna de quantidade na segunda posição
  x <- x[, c(1, 6, 2, 3, 4, 5)]
  return(x)
}
(tabela <- cluster.summary(dfi2014, lista_clusteres))

colorir.valor <- function(x) ifelse(x >= mean(x), style(color = "green"), style(color =
                                                                                  "red"))
nome_colunas <- c("Cluster", "Quantidade de países do Grupo", "Taxa de Inflação (%)",
                  "PIB Per Capita (US$)","Crescimento anual do PIB (%)", "Taxa de Emprego
(%)")
formattable(
  tabela,
  list(
    pib_per_capita = formatter("span", style = x ~ colorir.valor(x)),
    crescimento_pib = formatter("span", style = x ~ colorir.valor(x)),
    emprego = formatter("span", style = x ~ colorir.valor(x))
  ), col.names = nome_colunas, format = "markdown", pad = 0
)

dfi2014$cluster <- lista_clusteres
dfi2014["Brazil",]

cl_brasil <- dfi2014["Brazil", ]$cluster
x <- dfi2014[dfi2014$cluster == cl_brasil, ]
x[order(-x$PIB_per_Capita),] %>% knitr::kable()

###
###

set.seed(786)
file_loc <- 'C:/CODE/R-Clustering/seeds.txt'
seeds_df <- read.csv(file_loc,sep = '\t',header = FALSE)

feature_name <-
  c('area','perimeter','compactness','length.of.kernel','width.of.kernal
','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_df) <- feature_name

str(seeds_df)
summary(seeds_df)
any(is.na(seeds_df))
seeds_df <- na.omit(seeds_df)
seeds_label <- seeds_df$type.of.seed
seeds_df$type.of.seed <- NULL
str(seeds_df)

seeds_df_sc <- as.data.frame(scale(seeds_df))
summary(seeds_df_sc)

dist_mat <- dist(seeds_df_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 3)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')

install.packages("dendextend")
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)

install.packages("dplyr")
suppressPackageStartupMessages(library(dplyr))
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
count(seeds_df_cl,cluster)

install.packages("ggplot2")
suppressPackageStartupMessages(library(ggplot2))
ggplot(seeds_df_cl, aes(x=area, y = perimeter, color =
                          factor(cluster))) + geom_point()


table (seeds_df_cl $cluster, seeds_label)


