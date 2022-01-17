# # LIST OF REQUIRED PACKAGES -----------------------------------------------
# 
# required_packages <- c(
#   "checkpoint"
# )
# 
# # install missing packages
# 
# new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# 
# if (length(new.packages)) {
#   install.packages(new.packages)
# }
# 
# rm(new.packages)
# 
# library(checkpoint)
# checkpoint(snapshot_date ='2022-01-11')
library(tidyverse)
library(readxl)
library(zoo)
library(ggplot2)
library(directlabels)
library(ggrepel)
library(RColorBrewer)
library(chron)
library(openxlsx)
library(purrr)
library(stringr)
library(afex)
library(ggthemes)
library(ggstatsplot)
library(ggpubr)
library(openxlsx)
library(survminer)
library(chron)
library(survival)

library(rintrojs)
library(plotly)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}


date_day <- as_tibble(read_excel('./data/11172020 Shamima_ISO_IO_Moc1_PD1_ISO_4-1BB_12.29.2021.xlsx', sheet = 'Data Input', n_max = 2, col_names = FALSE))


di <- read_excel('./data/11172020 Shamima_ISO_IO_Moc1_PD1_ISO_4-1BB_12.29.2021.xlsx', sheet="Data Input", skip=2)[,-1]



dead<-di %>% filter(is.na(`Date of death`))%>% filter(!is.na(`Tag #`)) %>%select(c(1,2))
# 
# 
colnames(dead)<-c('mouse_n','tag_n')
# 

di2<-di[rowSums(!is.na(di)) > 2,]


di_full<-bind_cols(as_tibble(apply( di2[c(1:4,6,9,10)] , 2 , function(x) na.locf(x) )),di2[c(-1:-4,-6,-9,-10)] )

di_full<-di_full[colSums(!is.na(di_full)) > 0]




options(chron.year.expand = 
          function (y, cut.off = 12, century = c(2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)


date_day2<-date_day[colSums(!is.na(date_day)) > 0]%>%select(-1:-2)

date_day3<-c(as.matrix(date_day2[-1,]))

date_day3<-date_day3[date_day3 >= 0]


colnames(di_full)<-c('mouse_n','tag_n','sort_n','group','80PBW','Date_of_death','cage_n','measure',paste0('day',date_day3)[1:(ncol(di_full)-8)])


di_full2<-bind_cols(di_full%>%select(c(1:8)),di_full%>%select(c(-1:-8))%>%
                      mutate_all(function(x) as.numeric(gsub("[^0-9.-]", "", x)))) 


di_full3<-rows_update(di_full2,di_full2%>%filter(is.na(day0)) %>% mutate( day0=0)  
                      , by=c('mouse_n','tag_n','measure','sort_n','group'))




di_temp<-di_full3 %>% filter(!tag_n %in% dead$tag_n)%>%select(c(-5:-7))

di_list<-list()

for(i in 1:nrow(di_temp)) {
  
  di_temp2<-di_temp%>% mutate(across(everything(), as.character))
  di_temp_row<-as_tibble_row(na.locf(c(as.matrix(di_temp[i,]))),.name_repair='minimal')
  colnames(di_temp_row)<-colnames(di_temp)
  di_list[[i]]<-di_temp_row
  
}

di_temp3<-bind_rows(di_list)


di_full4<-rows_update(di_full3%>% mutate(across(everything(), as.character)),di_temp3
                      , by=c('mouse_n','tag_n','measure','sort_n','group'))

di_full5<-di_full4%>%type_convert()%>%mutate_at(c('mouse_n','tag_n','measure','sort_n','group'), as.character)%>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )


di_w<- di_full5%>%filter(measure=="W")

di_a<- di_full5%>%filter(measure=="A")

di_b<- di_full5%>%filter(measure=="B")



di_v_list<-list()

for(i in 9:ncol(di_b)) {
  
  di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2, column_name = colnames(di_b[,i]))
  # colnames(di_v_temp_row)<-colnames(di_b[,i])
  di_v_list[[i]]<-di_v_temp_row
  
}

di_v_temp<-bind_cols(di_v_list)


di_v<- (bind_cols(di_w[,1:7],di_b[,8],di_v_temp ))


di_v$measure<-rep("V",nrow(di_v))





condition_options <- c(paste0('day',date_day3)[1:(ncol(di_full)-8)])

conditions <- purrr::map(condition_options, 
                         ~quo(str_detect(day, fixed(!!.x, ignore_case = T))~as.numeric(gsub("[^0-9.-]", "", !!.x))))


di_v_tran<-di_v%>%pivot_longer(c(paste0('day',date_day3)[1:(ncol(di_full)-8)] ), names_to="day", values_to='tumor_volume' )%>% 
  separate(day, c('a', 'day_n'),sep ='y',remove=F) %>%type_convert()%>% dplyr::select(c(-5,-6,-7,-10))%>%
  mutate_at(c('mouse_n','tag_n','measure','sort_n','group'), as.character)



di_w_tran<-di_w%>%pivot_longer(c(paste0('day',date_day3)[1:(ncol(di_full)-8)]), names_to="day", values_to='body_weight' )%>%
  separate(day, c('a', 'day_n'),sep ='y',remove=F) %>%type_convert() %>% dplyr::select(c(-5,-6,-7,-10))%>%
  mutate_at(c('mouse_n','tag_n','measure','sort_n','group'), as.character)


di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))

n <- nrow(di_v)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

col_vector<-darken(col_vector)

di_plot_mean<-di_plot%>%group_by(sort_n,day_n)%>%summarise(tumor_volume_mean=mean(tumor_volume), sd=sd(tumor_volume),.groups='keep')


di_plot2<-  rows_update(di_plot,di_plot%>%filter(body_weight>2000)%>%mutate(body_weight=body_weight/100)
                        , by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))



di_plot2_mean<-di_plot2%>%group_by(sort_n,day_n)%>%summarise(body_weight_mean=mean(body_weight), sd=sd(body_weight),.groups='keep')

di_plot3_mean<-di_plot2%>%group_by(sort_n,day_n)%>%summarise(body_weight_mean=mean(body_weight), sd_bd=sd(body_weight),
                                                             tumor_volume_mean=mean(tumor_volume), sd_tv=sd(tumor_volume),.groups='keep')





day_names <- c(paste0('day',date_day3)[1:(ncol(di_full)-8)])

names(day_names) <- date_day3[1:(ncol(di_full)-8)]





my_comparisons2<-list(c("4-1BB", "Control"), c("4-1BB", '4-1BB+ISO'),
                      c("PD1",'PD1+ISO'),c("PD1",'Control'))



di3<-di2%>%filter(M=="W") 


date_day4<-c(as.matrix(date_day2[-1,]))




di_surv_ful<-(dplyr::bind_cols(di3[,1], zoo::na.locf(di3[,2]),zoo::na.locf(di3[,3]),
                               zoo::na.locf(di3[,4]), di3[,9], zoo::na.locf(di3[,10])))

colnames(di_surv_ful)<-c('mouse_n','tag_n','sort_n','group','date_of_death','status')




di_surv_ful1<-di_surv_ful%>%filter(str_detect(date_of_death, 'exclude,*')) %>% 
  tidyr::separate('date_of_death',sep=",", c("a", "date_of_death")) %>%
  mutate(date_of_death=as.Date(chron(format(as.Date(date_of_death, "%m/%d/%y"), "%m/%d/%y")))) %>%
  mutate(status=0)%>%
  select(!'a') 








di_surv_ful2<-di_surv_ful%>%filter(!str_detect(date_of_death, 'exclude,*')) %>%
  mutate(date_of_death=convertToDate(date_of_death))%>%
  mutate(status=1)


di_surv_ful3<-bind_rows(di_surv_ful1,di_surv_ful2)%>%mutate(days=as.numeric(
  date_of_death-convertToDate(date_day2[1,date_day4==0])))

di_surv_ful4<-di_surv_ful%>%filter(is.na(date_of_death)) %>%
  mutate(date_of_death=as.Date(Sys.time()))%>%
  mutate(status=0)%>%
  mutate(days=as.numeric(
    date_of_death-convertToDate(date_day2[1,date_day4==0])))

di_surv_ful5<-bind_rows(di_surv_ful3,di_surv_ful4)



mx<-max(di_surv_ful5$days)


fit <- survfit(Surv(days, status) ~ sort_n, data = di_surv_ful5)



fit2 <- survfit(Surv(days, status)~1, data = di_surv_ful5)

