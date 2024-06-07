library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggcorrplot) 
library(gghighlight)
library(ggpubr)
library(gridExtra)
library(viridis)
library(forcats)
library(broom)
library(MVN)
library(agricolae)
library(modelr)   
library(broom)
library(hrbrthemes)
library(car)
library(cowplot)


df_general_num <- read.csv(file.path(getwd(), "data", "survey_num_27_10.csv"),header = TRUE,sep = ",")
df_general_text <- read.csv(file.path(getwd(), "data", "survey_text_27_10.csv"),header = TRUE,sep = ",")

df_general_text <- df_general_text %>%
  slice(-1:-2) %>%
  select(12:36) %>%
  filter(demo_domain == "Facility",
         demo_age != "") %>%
  na.omit() 

x_order <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree","Somewhat disagree","Strongly disagree")  
x_order1 <- c("Extremely good", "Somewhat good","Neither good nor bad","Somewhat bad","Extremely bad")  

groupby_n <- function(df, colx, coly) {
  colx <- enquo(colx)
  coly <- enquo(coly)
  
  agg <- df %>% 
    group_by(!!colx, !!coly) %>%
    summarise(n = length(!!coly))
  
  return(agg)

}

bar <- function(df, colx, coly, title) {
  colx <- enquo(colx)
  coly <- enquo(coly)
  agg <- groupby_n(df, !!colx, !!coly)
  
  gg <- ggplot(agg, aes(x = !!colx, y = n, fill = !!coly)) + 
    geom_bar(stat = "identity", position=position_stack()) +
    scale_x_discrete(limits = x_order) + 
    labs(title = title, 
         x = '',
         y = "Count") +
    guides(fill = guide_legend(title = "Groups"),
           alpha = guide_legend(title = "Groups")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 7),
      axis.text.x = element_text(size = 7,angle = 15, hjust = 1),
      axis.text.y = element_text(size = 6),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7)
    ) +
      scale_fill_brewer(palette = "BuPu")
    
  return(gg)
  
  }


role <- bar(df_general_text,acc_3,demo_role, "Role")
gender <- bar(df_general_text,acc_3,demo_gender, "Gender")
age <- bar(df_general_text,acc_3,demo_age,"Age")
year_study <- bar(df_general_text,acc_3 , demo_year_study, "Year of study")
expr <- bar(df_general_text,acc_3,demo_experience, "Experience in the domain")

demo_bcharts <- grid.arrange(role,gender,
                             age,expr,                              
                             ncol = 2, nrow = 2)

