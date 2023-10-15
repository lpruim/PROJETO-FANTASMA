bancod<-read.csv("banco/vendas.csv")

install.packages("dplyr")
library(dplyr)


banco1 <- banco[!is.na(banco$Price), ]



faturamento_por_categoria1 <- banco1%>%
  group_by(Category) %>%     
  summarize(Faturamento = sum(Price))  
print(faturamento_por_categoria1)
