library(tidyverse)
library(openxlsx)
library(readxl)
library(lubridate)

df <- read_xls("处方数.xls")


df_stat <- df%>%
  mutate(时间=floor_date(日期,"month"))%>%
  group_by(时间)%>%
  summarise(每月处方数=sum(处方总数),
            月销售金额=sum(金额),
            抗菌药物使用率=sum(抗生素处方)/sum(处方总数))%>%
  mutate(年=year(时间),月=month(时间),.before = 2)

bg <- subset(df_stat,month(时间)==1,select = 时间,drop=TRUE)



ggplot(df_stat,aes(时间,每月处方数))+
  geom_vline(xintercept = bg,
             linetype=2)+
  geom_line()+
  geom_point(color="red")+
  scale_x_datetime(name="年",
                   date_breaks = "1 year",
                   date_labels = "%Y",
                   date_minor_breaks = "1 month"
                   )+
  
  theme_classic()+
  labs(title = "2018-2025年每月门诊处方数")+
  theme(plot.title = element_text(hjust = 0.5,size=18))

ggplot(df_stat,aes(时间,月销售金额))+
  geom_vline(xintercept = bg,
             linetype=2)+
  geom_line()+
  geom_point(color="blue")+
  scale_x_datetime(name=NULL,
                   date_breaks = "1 year",
                   date_labels = "%Y",
                   date_minor_breaks = "1 month"
  )+
  theme_classic()+
  labs(title = "2018-2025年每月门诊药品销售金额")+
  theme(plot.title = element_text(hjust = 0.5,size=18))

  

ggplot(df_stat,aes(月,每月处方数))+
  geom_col(aes(fill=年))+
  geom_text(aes(label=每月处方数),
            vjust = 0,nudge_y = 200,size=3)+
  scale_x_continuous(name="月",
                     breaks = c(1:12))+
  facet_wrap(vars(年),ncol = 3)+
  theme(legend.position = "none")

df_stat|>
  filter(年==2024)|>
  ggplot(aes(月,每月处方数,fill=年))+
  geom_col()+
  geom_text(aes(label=每月处方数),
            vjust = 0,nudge_y = 200,size=3)+
  scale_x_continuous(name="月",
                     breaks = c(1:12))+
  labs(title = "2024年每月门诊药品处方数")+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5,size=18))


df_stat|>
  filter(月==2)|>
  ggplot(aes(年,每月处方数,fill=每月处方数))+
  geom_col()+
  geom_text(aes(label=每月处方数),
            vjust = 0,nudge_y = 200)+
  scale_x_continuous(name="年",
                     breaks = c(2018:2025))+
  theme_classic()+
  labs(title = "2018-2025年2月份门诊处方数对比",
       y = "处方数")+
  theme(plot.title = element_text(hjust = 0.5,size=18))+
  theme(legend.position = "none")+
  scale_fill_distiller(direction = 1,palette = "Reds")
