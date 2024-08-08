#install.packages("basedosdados")
library("basedosdados")
library(tidyverse)
pacman::p_load('scales','tidyverse','survival','survminer','ggsurvfit','KMsurv', 'ggplot2')
library(ggsurvfit)

# Defina o seu projeto no Google Cloud
set_billing_id("dadosobras")

# Para carregar o dado direto no R
query <- bdplyr("br_me_cno.microdados")
df <- bd_collect(query)

write.csv(df,file = "C:/Users/Amauri/Desktop/estatistica pastas/trabalhos_estatistica/dadosbrutos.csv")



df2 = df %>% filter(sigla_uf=="PB")



df2$diferenca_dias <- as.numeric(difftime( df2$data_situacao,df2$data_inicio, units = "days"))
df2$status <- ifelse(df2$situacao == 15, 1, 0)
df3 <- subset(df2, diferenca_dias != 0)





write.csv(df3,file = "C:/Users/Amauri/Desktop/estatistica pastas/trabalhos_estatistica/dadosobras.csv")



dados_surv <- Surv(df3$diferenca_dias, df3$status)
kaplan_meier <- survfit(dados_surv ~ 1)

ggsurvfit(kaplan_meier, color= 'tomato')+
  add_confidence_interval(fill='tomato')+
  add_censor_mark(color= 'tomato')+
  add_quantile(y_value = 0.5, linetype= 'dashed',color = 'black',size= 0.6)+
  #add_risktable(stats_label = c('em risco','eventos'))+
  scale_ggsurvfit(x_scales = list(breaks= seq(0,20000,by = 5000)))+
  labs(x= 'Tempo(dias)', y='Probabilidade de sobrevivência ', title = 'Curva de sobrevivência ( Kaplan-Meier)')+
  theme(axis.title = element_text(size=11),axis.title.x = element_text(margin = margin(5,5,15,5)))


summary(kaplan_meier,times = c(1,10,50,100,500,1000,1500,2000,2500,3000,3327))


ggplot(data = df3,aes(y=diferenca_dias))+
  geom_boxplot(color= 'red')+
  scale_y_continuous(n.breaks = 20)+
  theme_bw()+
  labs(title = 'Boxplot dos tempos das obras', y= 'tempo (dias)')



# Calcular as proporções
proporcoes <- table(df3$status) / length(df3$status)


dados_grafico <- data.frame(valores = names(proporcoes),proporcao = as.numeric(proporcoes))
# Criar o gráfico de barras

ggplot(data = dados_grafico, aes(x = valores, y = proporcao, fill = valores)) +
  geom_bar(stat = "identity") +
  xlab("Valores") +
  ylab("Proporção") +
  ggtitle("Gráfico de Proporções de Valores 0 e 1", )+
  scale_fill_manual(values = c('0' = 'red', '1' = 'green'),labels=c('0' = 'censurado', '1' = 'não censurado'))+
  geom_text(aes(label= scales::percent(proporcao)),vjust= 2)+
  theme_bw()

print(kaplan_meier, print.rmean = TRUE)

summary(kaplan_meier, times = 365)













df3$tipodaobra <- ifelse(df3$qualificacao_responsavel == 70 |df3$qualificacao_responsavel == 57,1,2)

kaplan_meier2 <- survfit(dados_surv ~ tipodaobra)



ggsurvfit(kaplan_meier2)+
  add_confidence_interval()+
  add_censor_mark()+
  #add_quantile(y_value = 0.5, linetype= 'dashed',color = 'black',size= 0.6)+
  add_risktable(stats_label = c('em risco','eventos'))+
  scale_ggsurvfit(x_scales = list(breaks= seq(0,20000,by = 5000)))+
  labs(x= 'Tempo(dias)', y='Probabilidade de sobrevivência ', title = 'Curva de sobrevivência ( Kaplan-Meier)')+
  theme(axis.title = element_text(size=11),axis.title.x = element_text(margin = margin(5,5,15,5)))

testeLogRank <- survdiff(dados_surv ~ tipodaobra, data = df3, rho = 0)
testeLogRank$pvalue







incon = df3 %>% filter(area == 0.01)



incon2 = incon[,22:27]
