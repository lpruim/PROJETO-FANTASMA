#Análise 1

bancod<-read.csv("banco/vendas.csv")
banco1 <- bancod[!is.na(bancod$Price), ] 


library(dplyr)
library(lubridate)
library(ggplot2)
banco1 <- banco[!is.na(banco$Price), ]
banco1 <- banco[!is.na(banco$Data.Venda), ]
banco1 <- banco[!is.na(banco$Category), ]

banco1$Data.Venda <-  mdy(banco1$Data.Venda)

banco1$mes <- month(banco1$Data.Venda)

banco_mes<- banco1 %>% 
  filter(!is.na(Price)) %>% 
  filter(!is.na(mes)) %>% 
  group_by(mes, Category) %>% 
  summarise(Faturamento= sum(Price)) 

colnames(banco_mes)[colnames(banco_mes) == "Category"] <- "Categorias"


banco_mes$Categorias <- gsub("Kids' Fashion", "Infantil", banco_mes$Categorias, ignore.case = TRUE)
banco_mes$Categorias <- gsub("Women's Fashion", "Feminino", banco_mes$Categorias, ignore.case = TRUE)
banco_mes$Categorias <- gsub("Men's Fashion", "Masculino", banco_mes$Categorias, ignore.case = TRUE)



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


ggplot(banco_mes) +
  aes(x = mes, y = Faturamento, group = Categorias, colour = Categorias) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Produto", labels = c("Homens", "Mulheres","Crianças")) +
  labs(x = "Meses", y = "Faturamento em R$") +
  scale_x_continuous(breaks = seq(1, 12, by = 1), limits = c(1, 12)) +
  scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, by = 500)) + 
  theme_estat()
ggsave("series_grupo1.pdf", width = 158, height = 93, units = "mm")

#Análise 3






dados_ana3 <- subset(vendas, Category =="Men's Fashion")

dados_ana3 <- dados_ana3[complete.cases(dados_ana3$Color), ]




dados_ana2 <- subset(vendas, Category =="Women's Fashion")

dados_ana2 <- dados_ana2[complete.cases(dados_ana2$Color), ]

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


