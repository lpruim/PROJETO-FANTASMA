#Análise 1

bancod<-read.csv("banco/vendas.csv")

install.packages("dplyr")
library(dplyr)


banco1 <- bancod[!is.na(bancod$Price), ]



faturamento_por_categoria1 <- banco1%>%
  group_by(Category) %>%     
  summarize(Faturamento_ = sum(Price))  
print(faturamento_por_categoria1)
write.csv(faturamento_por_categoria1, file = "faturamento.csv", row.names = FALSE)


install.packages("ggplot2")
library(ggplot2)
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}



library(ggplot2)

meu_dataframe <- data.frame(
  Coluna1 = c("Kids' Fashion", "Men's Fashion", "Women's Fashion"),  
  Coluna2 = c(19333, 17261, 18219) 
)

print(meu_dataframe)

ggplot(meu_dataframe) +
  aes(x = Coluna1, y = Coluna2) +
  geom_bar(stat = "identity", 
           fill = "#A11D21",
           width = 0.5) +
  labs(x = "Categorias", y = "Faturamento em R$") +
  ggtitle("Faturamento por categoria") +
  theme_estat() +  
  coord_cartesian(ylim = c(0, 20000))  

ggsave("hist_uni_porc.pdf", width = 158, height = 93, units = "mm")


#Análise 3

bancod<-read.csv("banco/vendas.csv")

install.packages("dplyr")
library(dplyr)




dados_ana3 <- subset(vendas, Category =="Men's Fashion")

dados_ana3 <- dados_ana3[complete.cases(dados_ana3$Color), ]




dados_ana2 <- subset(vendas, Category =="Women's Fashion")


dados_ana2 <- dados_ana2[complete.cases(dados_ana2$Color), ]



install.packages("ggplot2")
library(ggplot2)
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}


contagem1 <- dados_ana3 %>% 
  group_by(Color) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(Color)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem1) +
  aes(x = factor(""), y = Prop , fill = factor(Color)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Cores') +
  labs(title = "Roupas masculinas")

ggsave("setor.pdf", width = 158, height = 93, units = "mm")









contagem <- dados_ana2 %>% 
  group_by(Color) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(Color)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem) +
  aes(x = factor(""), y = Prop , fill = factor(Color)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Cores') +
  labs(title = "Roupas femininas")

ggsave("setor1.pdf", width = 158, height = 93, units = "mm")


