library(ggplot2)
library(readxl)
library(dplyr)
library(tidyverse)
HiperUFSC_Dataset <- read_excel("C:/Users/07708966930/Downloads/HiperUFSC Dataset.xlsx")
View(HiperUFSC_Dataset)
dados<-HiperUFSC_Dataset
str(dados)
#Transformando Charac em Fator (Variáveis Categoricas)
dados$Item_Fat_Content <- factor(dados$Item_Fat_Content)
dados$Item_Type <- factor(dados$Item_Type)
dados$Outlet_Size <- factor(dados$Outlet_Size)
dados$Outlet_Location_Type <- factor(dados$Outlet_Location_Type)
dados$Outlet_Type <- factor(dados$Outlet_Type)
dados$Outlet_Establishment_Year<- factor(dados$Outlet_Establishment_Year)
## Primeiro Trabalho: Estatistica Descritiva 
##Item identifier = nada a se fazer é apenas identificação


##variáveis qualitativas
summary.factor(dados$Item_Fat_Content)
summary.factor(dados$Item_Type)
summary.factor(dados$Outlet_Size)
summary.factor(dados$Outlet_Location_Type)
summary.factor(dados$Outlet_Type)

str(dados)
##Variaveis Quantitativas
##Item Weight, Item Visibility, Item MRP, Estabilishiment Year, Outlet Sales
##Obter medidas de dispersão e medias summary
summary(dados$Item_Weight)
sd(dados$Item_Weight, na.rm = TRUE)
var(dados$Item_Weight, na.rm = TRUE)

summary(dados$Item_Visibility)
sd(dados$Item_Visibility, na.rm = TRUE)
var(dados$Item_Visibility, na.rm = TRUE)

summary(dados$Item_MRP)
sd(dados$Item_MRP, na.rm = TRUE)
var(dados$Item_MRP, na.rm = TRUE)

summary(dados$Item_Outlet_Sales)
sd(dados$Item_Outlet_Sales, na.rm = TRUE)
var(dados$Item_Outlet_Sales, na.rm = TRUE)


##Sumarrizando Médias
desviopadrao <- summarize(dados, 
                    sdweight = sd(dados$Item_Weight, na.rm=TRUE), 
                    sdvisibility = sd(dados$Item_Visibility, na.rm=TRUE),
                    sdmrp = sd(dados$Item_MRP, na.rm=TRUE),
                    sdoutletsales = sd(dados$Item_Outlet_Sales, na.rm=TRUE))

View(desviopadrao)


##Exploração dos Dados por meio dos Gráficos

##Base
 
##Usado em variaveis Numericas
##Grafico de uma variável
##histograma
hist(dados$Item_Visibility)
hist(dados$Item_Weight)
hist(dados$Item_MRP)
hist(dados$Outlet_Establishment_Year)
hist(dados$Item_Outlet_Sales)
##grafico de densidade
plot(density(dados$Item_Visibility))
plot(density(dados$Item_Weight))
plot(density(dados$Item_MRP))
plot(density(dados$Outlet_Establishment_Year))
plot(density(dados$Item_Outlet_Sales))
##grafico Boxplot Analisar MRP Weight Outlet Sales EstabilishimentYear
boxplot(dados$Item_MRP)
boxplot(dados$Item_Weight)
boxplot(dados$Item_Outlet_Sales) ##demonstrou mais outliers (interessante)
boxplot(dados$Outlet_Establishment_Year)
##Grafico de Barras para variaveis categoricas fator
plot(dados$Item_Fat_Content)
plot(dados$Item_Type)
plot(dados$Item_Fat_Content)

## duas variaveis scatter plot num x num
plot(dados$Item_Outlet_Sales, dados$Item_Visibility)

##categorica x categorica 
##TIPO MOSAICO
# Relacionar Tipo de Cidade (Outlet Location Type) x Tamanho da Loja (Outlet Size) são duas variaveis categoricas. Usar Grafico Mosaico.
plot(table(dados$Outlet_Location_Type, dados$Outlet_Size))

#fator x qualitativa = Outlet Size  x estabilishiment Year 

ggplot(dados, 
       aes(x = dados$Outlet_Establishment_Year, 
           fill = dados$Outlet_Size)) + 
  geom_bar()
##Conclussão: Empresa média é a tendencia desde 1980 

##NUMX CATEGORICA
#Relacionar Item Weight com Item Fat Content = (PesoxGordura) NumxFator

plot(dados$Item_Fat_Content, dados$Item_Weight)

##grafico multivariaveis usando ggplot
##Dados Preço x Venda de Produtos com Cor determinado o tipo de produto
ggplot(dados, aes( x=dados$Item_MRP, y=dados$Item_Outlet_Sales))+
  geom_point(aes(color=dados$Item_Type))+
  geom_smooth()

# Relacionar Tipo de Cidade (Outlet Location Type) x Tamanho da Loja (Outlet Size) são duas variaveis categoricas. Usar Grafico Mosaico.
plot(table(dados$Outlet_Location_Type, dados$Outlet_Size))
##Grafico Multivariaveis
##ideia outlet type x item visibility
ggplot(dados, 
       aes(x = dados$Item_MRP  , 
           y = dados$Item_Visibility, 
          color = Item_Type)) +
  geom_point() +
  labs(title = "Relação Visibilidade do Item com Preço (MRP)")
##Resultado: Pouco Conclusivo.

##Relacionar Venda de Produtos por Tipo de Loja (OutletSales x Outlet Type)
##NumericaxFactor
ggplot(dados, 
       aes(x = dados$Outlet_Type, 
           y = dados$Item_Outlet_Sales)) +
  geom_bar(stat = "identity")

ggplot(dados, 
       aes(x = dados$Item_Outlet_Sales, 
           fill = dados$Outlet_Type )) +
  geom_density(alpha = 0.4) +
  labs(title = "Vendas Distribuidas por Tipo de 'Loja'")


##Selecionar Observações
mercearia1 <- filter(dados, dados$Outlet_Type== "Grocery Store")
mercearia1

##Usar mercearia1 = Grocery Store x Item Fat Content
##Grafico tipo hist
##mutate criar nova tabela a partir das existentes

##mutate OutletSales x Area da Loja = Venda por Area de loja.

