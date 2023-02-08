library("readxl")
library(dplyr)
library(ggplot2)

# Tratamento dos dados
# Inicio

meus_dados <- readxl::read_excel("C:/Users/victo/OneDrive/Área de Trabalho/MAIN/projeto_est_exp/sp_beaches.xlsx", sheet=1) %>% 
dplyr::filter(City == "SÃO VICENTE") %>% mutate(Enterococcus = as.numeric(Enterococcus)) %>% group_by(Beach) %>% 
  add_count(across(Beach))

# Fim
# -
# Média de cada praia
# Inicio
praias <- unique(meus_dados$Beach)

#medias
# Fim
# -
# Desvio padrão para cada praia
# Inicio

retorna_parametros <- function(i = 1){
  if(i <= length(praias)){
    praia <- meus_dados %>% filter(Beach == praias[i])
    enterococus <- (praia$Enterococcus)
    print("===============================================================================")
    cat("*** PRAIA:", praias[i],"***", "\n") %>% 
      cat("# Média:... ", mean(enterococus), "\n" ) %>% 
      cat("- Desvio padrão:...", sd(enterococus), "\n") %>% 
      cat("- Mediana:...", median(enterococus), "\n") %>%
      cat("- Primeiro Quartil:...", quantile(enterococus, probs = 0.25), "\n") %>%
      cat("- Terceiro Quartil:...", quantile(enterococus, probs = 0.75), "\n") %>%
      cat("- Valor máximo:...", max(enterococus), "\n") %>%
      cat("- Valor minimo:...", min(enterococus), "\n")
  }
  

}
retorna_parametros()

bar_plot <- ggplot(meus_dados,aes(y=Beach, label = scales::percent(prop.table(stat(count))))) + 
  geom_text(stat = 'count', vjust=-3, color="black",
            position = position_dodge(0.9), size=3.5)+
  geom_bar(aes( fill= Beach), width=0.5)+
  theme_minimal()
bar_plot

pizza_plot <- ggplot(meus_dados,aes(x="", y=n,fill=Beach)) + 
  geom_bar( width=1, stat="identity")+
  coord_polar("y", start=0)
pizza_plot

bar_plot <- ggplot(meus_dados,aes(y=Beach, label = scales::percent(prop.table(stat(count))))) + 
  geom_text(stat = 'count', vjust=-3, color="black",
            position = position_dodge(0.9), size=3.5)+
  geom_bar(aes( fill= Beach), width=0.5)+
  theme_minimal()

histogram <- ggplot(meus_dados, aes(x=Enterococcus)) +
  geom_histogram(aes(fill = Beach))
histogram

box_plot <- ggplot(meus_dados, aes(x=Enterococcus, y=Beach)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
box_plot
#media <- mean(meus_dados$Enterococcus)
#View(media)
#colnames(meus_dados)


