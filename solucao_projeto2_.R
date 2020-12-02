#SOLUÇÃO DO PROJETO COM FEEDBACK:
  #"Prevendo Demanda de Estoque com Base
    #em Vendas"
          #FCD/ReAZURE

#1.CONFIGURAÇÕES PRELIMINARES
#configurando o diretório de trabalho
setwd("C:/FCD/BigDataRAzure/Pojeto2")
getwd()

#2.ACESSANDO OS DADOS
    #usei os dados de treino oferecidos no Kaggle e utilizei o
    #Azure para obter uma amostra destes dados

#lendo o arquivo ".csv" e gravando-o em um objeto
amostra <- read.csv("train/sampleTrain2.csv", sep = ",",
                        stringsAsFactors = FALSE, encoding = "UTF-8")

#visualizando as primeiras linhas do dataset
head(amostra)

#Visualizando o dataset completo
View(amostra)

#nomeando as colunas
colnames(amostra) <- c("Semana", "Agencia_ID", "Canal_ID", "Ruta_SAK",
                           "Cliente_ID", "Producto_ID", "Venta_uni_hoy",
                           "Venta_hoy", "Dev_uni_proxima", "Dev_proxima",
                           "Demanda_uni_equil")

#obtendo características das variáveis
str(amostra)

#obtendo um resumo estatístico das variáveis
summary(amostra)

#lendo os demais arquivos
NomeCliente <- read.csv("cliente_tabla.csv", sep = ",",
                    stringsAsFactors = FALSE, encoding = "UTF-8")
NomeProduto <- read.csv("producto_tabla.csv", sep = ",",
                    stringsAsFactors = FALSE, encoding = "UTF-8")
NomeCidade <- read.csv("town_state.csv", sep = ",",
                    stringsAsFactors = FALSE, encoding = "UTF-8")

View(NomeCliente)
View(NomeProduto)
View(NomeCidade)

#3.Fazendo alterações para averiguar a pertinência de um dadtaset
#organizado de forma distinta

#juntando os data frames
amostra2 <- merge(amostra, NomeCidade, by = "Agencia_ID")
View(amostra2)

amostra3 <- merge(amostra2, NomeProduto, by = "Producto_ID")
View(amostra3)

amostra4 <- merge(amostra3, NomeCliente, by = "Cliente_ID")
View(amostra4)


#Alterando o formato dos dados "Town"
library(tidyr)
amostra5 <- amostra4 %>%
  separate(Town, into = c("code", "Town", "neghborhood"), sep = "\\ ")
View(amostra5)
  #Comentário: perdi a parte final da coluna. Não consegui resolver isto.

library(dplyr)
amostra5 <- arrange(amostra5, Semana)
amostra5 <- arrange(amostra5, desc(Dev_uni_proxima), Producto_ID, 
                   Cliente_ID)


library(data.table)
amostra5 <- as.data.table(amostra5)
class(amostra5)
str(amostra5)

#Variável "NombreProducto" certamente contém mais informações do que
#somente o nome do produto. Mas, para realizar a divisão seria necessário
#conhecer mais os dados. Além disso, eu também não conseguiria realizar
#uma separação como fiz para a coluna "Town".

#4. Primeira conclusões
#4.a
#somente olhando para as descrições das variáveis, podemos
#esperar que as variáveis ""enta_uni_hoy" e "Dev_uni_proxima"
#apresentam colinearidade com as variáveis Venta_hoy" e
#"Venta_proxima" respectivamente. Portanto, devem apresentar
#correlações elevadas

#4.b. A despeito de o dataset amostra5 conter estar mais completo,
#não consegui organizá-lo de modo a aproveitar seu conteúdo mais amplo.
#Por isto, decidi trabalhar com o dataset amostra (uma amostra do
#dataset de traino oferecido no Kaggle)
# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")

#5.Explorando os dados

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(amostra[,], method = method)))

head(cors)

library(lattice)

#Preprando o plot
require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correla??o usando M?todo", labs),
                  scales = list(x = list(rot = 80), cex = 1.0)) )
}

#Mapas de corelações
Map(plot.cors, cors, metodos)
  #Pelos mapas, podemos verificar que as a variável target apresenta correlação
#com as variáveis "Venta_uni_hoy", "Venta_hoy" e, em menor n?vel, "Dev_uni_proxima".
#Contudo, como havíamos observado,estas duas variáveis também apresentam
#correlação entre si. Portanto, em nosso modelo utilizaremos apenas a
#variável "Venta_uni_hoy" e "Dev_uni_proxima".


library(ggplot2)

amostra %>%
  group_by(Cliente_ID) %>%
  summarise(avg = mean(Venta_uni_hoy), 
            minimo = mean(Venta_uni_hoy),
            maximo = max(Venta_uni_hoy),
            total = n())
#Os dados nos mostram que as vendas provavelmente estão
#distribuídas entre muitos clientes. Portanto, dificilmente
#um único cliente apresentaria um peso relevante para as
#perdas. (Observação: tentei ordenar pela "mean(Venta_uni_hoy)",
#mas não consegui)

amostra %>%
  group_by(Producto_ID) %>%
  summarise(avg = mean(Venta_uni_hoy), 
            minimo = mean(Venta_uni_hoy),
            maximo = max(Venta_uni_hoy),
            total = n())
#A despeito de verificarmos variáveis mais acentuadas
#em alguns produtos, e que alguns produtos são vendidos
#em quantidade maior, uma melhor exploração destes dados
#parece difícil devido ao grande número de "Producto_ID".
#(Obsrvação: também tentei ordenar, mas não consegui)

amostra %>%
  group_by(Semana) %>%
  summarise(avg = mean(Demanda_uni_equil), 
            minimo = mean(Demanda_uni_equil),
            maximo = max(Demanda_uni_equil),
            total = n())
#Os dados se apresentam razoavelmente bem distribuídos entre as semanas,
#mas com dados máximos bem acima da média.

amostra %>%
  group_by(Cliente_ID) %>%
  summarise(avg = mean(Demanda_uni_equil), 
            minimo = mean(Demanda_uni_equil),
            maximo = max(Demanda_uni_equil),
            total = n())
#os dados se apresentam razoavlemete bem distribuídos

amostra %>%
  group_by(Producto_ID) %>%
  summarise(avg = mean(Demanda_uni_equil), 
            minimo = mean(Demanda_uni_equil),
            maximo = max(Demanda_uni_equil),
            total = n())
#os dados se apresentam razoavelmente bem distribuídos

amostra %>%
  group_by(Semana) %>%
  summarise(avg = mean(Venta_uni_hoy), 
            minimo = mean(Venta_uni_hoy),
            maximo = max(Venta_uni_hoy),
            total = n())
#Isto nos mostra que os dados estão semanalmente bem
#ditribuídos. As médias são próximas aos mínimos, mas
#os máximos estão bem distantes, indicando poss?veis outliers.

#Verificando outliers
  #agrupados por semana
ggplot(amostra, aes_string(x = amostra$Semana, 
                               y = amostra$Dev_uni_proxima, 
                               group = amostra$Semana)) +
  geom_boxplot()

ggplot(amostra, aes_string(x = amostra$Semana, 
                               y = amostra$Venta_uni_hoy, 
                               group = amostra$Semana)) +
  geom_boxplot()


#Visualizando os dados através de um gráfico de deensidade

#nomeando os labels
labels <- c("Demanda x Unidades vendidas",
            "Demanda x Valor das vendas",
            "Demanda x Unidades devolvidas",
            "Demanda x Valor das unidades devolvidas")

#definindo as variáveis a serem relacionadas com a Demanda
xAxis <- c("Venta_uni_hoy", "Venta_hoy",
           "Dev_uni_proxima", "Devi_proxima")

# Função para os Density Plots
plot.scatter <- function(X, label){ 
  ggplot(amostra, aes_string(x = X, y = "Demanda_uni_equil")) + 
    geom_point(aes_string(colour = "Demanda_uni_equil"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20))
}

#criando os plots usando a função "plo.sctter" dentro da função "Map"
Map(plot.scatter, xAxis, labels)
#Comentário: tentei fazer estes plots, mas as imagens não foram carregadas.


#6.Os modelos

#Criando o modelo de regressão linear usando as correlações mais elevadas
modelo <- lm(Demanda_uni_equil ~ Venta_uni_hoy + Dev_uni_proxima, data = amostra)
modelo
summary(modelo)

#A despeito dos bons resultados obtidos com o modelo linear, acreditamos
#haver alguns problemas. O principal dele é que a "Dev_uni_proxima" é um
#dado que se deseja ser igual a zero. Então, não faz sentido inserir no
#modelo. Além disso, tivemos dificuldades de organizar os dados de modo
#a avaliar a influência dos clientes e produtos para definir a demanda.


#Por isto, decidimos fazer também um modelo de randomforest
library(randomForest)
library(dplyr)
require(randomForest)

#verificando a importância de cada variável preditora para a variável target
modelo2 <- randomForest(Demanda_uni_equil ~ .,
                       data = amostra, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)
varImpPlot(modelo2)

#dividindo os dados em dados de treino e dados de teste
  #Eu sei que os dados são entregues já divididos, mas estou usando este
  #recurso para evitar lentidão no processo, dadas as limitações da minha
  #m?quina.
indexes <- sample(1:nrow(amostra), size = 0.7 * nrow(amostra))
train.data <- amostra[indexes,]
test.data <- amostra[-indexes,]

#construindo o modelo
modeloDef <- randomForest(Demanda_uni_equil ~ Venta_uni_hoy
                          + Producto_ID
                          + Canal_ID
                          +Ruta_SAK,
                          data = train.data,
                          ntree = 50, 
                          nodesize = 5)
print(modeloDef)

#testando o modelo
scores <- data.frame(actual = train.data$Demanda_uni_equil,
                     prediction = predict(modeloDef, newdata = train.data))
View(scores)

residuos <- data.frame(actual = test.data$Demanda_uni_equil,
                      prediction = predict(modeloDef, newdata = test.data))
diferenca = residuos$actual - residuos$prediction

#analisando os resíduos
ggplot(residuos, aes(x = diferenca)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black")
