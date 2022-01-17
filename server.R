


server <- function(input, output, session) {
  
  
  # INSTALL DEPENDENCIES ----------------------------------------------------
  
  source('dependencies.R')
  # load all packages
  #lapply(required_packages, require, character.only = TRUE)
  
  
  

  # DEFINE SETS -------------------------------------------------
  
  
  di_plot <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
 
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1]
        
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
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
        
        
        
        
      }
    })


    
    di_plot%>%filter(tag_n %in% c(input$tagInput))
  })
  
  
 
  
  
  col_vector <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
          
          di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                       column_name = colnames(di_b[,i]))

          
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
        
        
        di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))%>%filter(tag_n %in% c(input$tagInput))
        
        
        
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
        
        
      }
    })
    
    
    
    col_vector
  })
  

  
  di_plot_mean <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
          
          di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                       column_name = colnames(di_b[,i]))
          
          
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
        
        
        di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))%>%filter(tag_n %in% c(input$tagInput))
        
        
        
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
        
        
      }
    })
     
    
 
    di_plot_mean
  })
  
  


  
  di_plot2 <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
          
          di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                       column_name = colnames(di_b[,i]))
          
          
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
        
        
        di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))%>%filter(tag_n %in% c(input$tagInput))
        
        
        
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
        
        di_plot2<- rows_update(di_plot,di_plot%>%filter(body_weight>2000)%>%mutate(body_weight=body_weight/100)
                                , by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))
        
        
        
      }
    })
    
    
    
    di_plot2
  })
  
  

  
  
  di_plot2_mean <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
          
          di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                       column_name = colnames(di_b[,i]))
          
          
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
        
        
        di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))%>%filter(tag_n %in% c(input$tagInput))
        
        
        
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
        
        
        di_plot2<- rows_update(di_plot,di_plot%>%filter(body_weight>2000)%>%mutate(body_weight=body_weight/100)
                               , by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))
        
        
        
        di_plot2_mean<-di_plot2%>%group_by(sort_n,day_n)%>%summarise(body_weight_mean=mean(body_weight), sd=sd(body_weight),.groups='keep')
         
        
      }
    })
    
    
    
    di_plot2_mean
  })
  
  
  
  
 
  
  
  
  
  di_plot3_mean <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
          
          di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                       column_name = colnames(di_b[,i]))
          
          
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
        
        
        di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))%>%filter(tag_n %in% c(input$tagInput))
        
        
        
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
        
        
        di_plot2<- rows_update(di_plot,di_plot%>%filter(body_weight>2000)%>%mutate(body_weight=body_weight/100)
                               , by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))
        
        
        di_plot3_mean<- di_plot2%>%group_by(sort_n,day_n)%>%summarise(body_weight_mean=mean(body_weight), sd_bd=sd(body_weight),
                                                                              tumor_volume_mean=mean(tumor_volume), sd_tv=sd(tumor_volume),.groups='keep')
        
        
        
        
        
      }
    })
    
    
    
    di_plot3_mean
  })
  
  
  
  
  day_names <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        date_day2<-date_day[colSums(!is.na(date_day)) > 0]%>%select(-1:-2)
        
        date_day3<-c(as.matrix(date_day2[-1,]))
        
        date_day3<-date_day3[date_day3 >= 0]
        
        
        
        day_names <- c(paste0('day',date_day3)[1:(ncol(di_full)-8)])
        
        names(day_names) <- date_day3[1:(ncol(di_full)-8)]
        
        
        
      }
    })
    
    
    
    day_names
  })

  
  
  my_comparisons2 <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        
        
        my_comparisons2<-list(c(input$gcInput) )
        
        
      }
    })
    
    
    
    my_comparisons2
  })
  
 
  
  
  
  
  
  fit <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
        
        
        di_plot<-full_join(di_w_tran, di_v_tran[c(-6,-5)], by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))%>%filter(tag_n %in% c(input$tagInput))
        
        
        
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
        
        di_surv_ful5<-bind_rows(di_surv_ful3,di_surv_ful4)%>%filter(tag_n %in% c(input$tagInput))
        
        
        
        mx<-max(di_surv_ful5$days)
        
        
        fit <- survfit(Surv(days, status) ~ sort_n, data = di_surv_ful5)
        
        
        
      }
    })
    
    
    
    fit
  })
  
  
  
  
  
  
  
  
  fit2 <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
        
        di_surv_ful5<-bind_rows(di_surv_ful3,di_surv_ful4)%>%filter(tag_n %in% c(input$tagInput))
        
        
        
        mx<-max(di_surv_ful5$days)
        
        
        fit2 <- survfit(Surv(days, status)~1, data = di_surv_ful5)
        
        
        
      }
    })
    
    
    
    fit2
  })
  
  
  
 
  
  di_surv_ful5 <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    input$confirm # confirm buttons needs to be pressed to initiate this code
    
    isolate({
      if(is.null(input$file1))
      {
        
      }
      else
      {
        inFile <- input$file1 
        
        
        di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
        date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
        
        
        
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
        
        
        
        
        
      }
    })
    
    
    
    di_surv_ful5%>%filter(tag_n %in% c(input$tagInput))
  })
  
  
  
  
  
 
    
  # UI - GENERAL --------------------------------------------------------------
  
  
  # use action buttons as tab selectors
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "v", "w", "gc", "s"),
                      label = "",
                      selected = x
    )
  }
  
  observeEvent(input$v, {
    update_all("v")
  })
  observeEvent(input$w, {
    update_all("w")
  })
  observeEvent(input$gc, {
    update_all("gc")
  })
  observeEvent(input$s, {
    update_all("s")
  })
  
  # update confirm button
  
  observeEvent(input$confirm, {
    updateButton(
      session, 
      inputId = "confirm", 
      label = "CONFIRM SELECTION", 
      icon = icon("play-circle"), 
      style = "primary")
  })
  
  # hide the underlying selectInput in sidebar for better design
  observeEvent("", {
    hide("tab")
  })
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    show("v_panel")
    hide("w_panel")
    hide("gc_panel")
    hide("s_panel")
  }, once = TRUE)
  
  observeEvent(input$v, {
    show("v_panel")
    hide("w_panel")
    hide("gc_panel")
    hide("s_panel")
  })
  observeEvent(input$w, {
    show("w_panel")
    hide("gc_panel")
    hide("s_panel")
    hide("v_panel")
  })
  observeEvent(input$gc, {
    show("gc_panel")
    hide("w_panel")
    hide("s_panel")
    hide("v_panel")
  })
  observeEvent(input$s, {
    show("s_panel")
    hide("gc_panel")
    hide("w_panel")
    hide("v_panel")
  })
  
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "v", style = {
      if (x == "v") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "w", style = {
      if (x == "w") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "gc", style = {
      if (x == "gc") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "s", style = {
      if (x == "s") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  
  observeEvent( input$file1,  {
    
    inFile <- input$file1 
    
    
    
    di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1] 
    date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
    
    
    
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
      
      di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                   column_name = colnames(di_b[,i]))
      
      
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
    
    di_plot2<- rows_update(di_plot,di_plot%>%filter(body_weight>2000)%>%mutate(body_weight=body_weight/100)
                           , by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))

          updateCheckboxGroupInput (session, "tagInput",

                      choices = unique(di_plot2$tag_n),
                      selected = unique(di_plot2$tag_n)
    )})

  observeEvent( input$file1,   {

    
    inFile <- input$file1 
    
    
    
    di <- read_excel(inFile$datapath, sheet="Data Input", skip=2)[,-1]
    
    date_day <- as_tibble(read_excel(inFile$datapath, sheet = 'Data Input', n_max = 2, col_names = FALSE))
    
    
    
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
      
      di_v_temp_row<-as_tibble_col((((di_b[,i]%>%as.matrix()%>%as.numeric())^2)*(di_a[,i]%>%as.matrix()%>%as.numeric()))/2,
                                   column_name = colnames(di_b[,i]))
      
      
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
    
    di_plot2<- rows_update(di_plot,di_plot%>%filter(body_weight>2000)%>%mutate(body_weight=body_weight/100)
                           , by=c('mouse_n','tag_n', "sort_n", "group",'day_n'))
    
    
          updateCheckboxGroupInput (session,  inputId = "gcInput",
                      choices = unique(di_plot2$sort_n),
                      selected = unique(di_plot2$sort_n)[1:2]
                      
                      
                      
                      )})
  # UI - Volume - 1 ----------------------------------------------------------
  
  output$box_pat <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 520,
        tabPanel(
          title = "volume vs day indv",
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "down_vdi_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_vdi_select", height = 450),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  
  # UI - Volume - 2 -------------------------------------------------------
  
  output$box_pat2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "volume vs day mean facet",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_vdmf_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_vdmf_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "volume vs day mean single",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_vdms_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_vdms_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })

  
  
  
  # UI - Volume - 3 -------------------------------------------------------
  
  output$box_year <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_year",
        width = NULL,
        height = 400,
        tabPanel(
          title = "volume vs weight mean facet",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_vwmf_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_vwmf_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "volume vs weight mean single",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_vwms_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_vwms_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "volume vs weight indv",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_vwi_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_vwi_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
  
  
  # UI - Weight - 1 ------------------------------------------------------------------
  output$box1 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "weight vs day indv",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_wdi_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_wdi_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "weight vs day mean facet",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_wdmf_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_wdmf_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "weight vs day mean single",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_wdms_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_wdms_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  

  
  # UI - Weight - 2 -------------------------------------------------------------
  
  output$box2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "weight vs volume indv",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_wvi_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_wvi_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "weight vs volume mean facet",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_wvmf_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_wvmf_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "weight vs volume mean single",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_wvms_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_wvms_select", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ) 
        
        )
      )
    
  })
 
  
  # UI - gc - 1 ------------------------------------------------------------------
  
  output$box5 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box5",
        width = NULL,
        height = 1000,
        tabPanel(
          title = "group compare",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_gc_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_gc_select", height = 800),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
 
      )
    )
  }) 
  # UI - s - 1 -----------------------------------------------------
  
  output$box_los1 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_los1",
        width = NULL,
        height = 800,
        
        
        tabPanel(
          title = "KM with ALL Group Seperated",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_kags_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_kags_select", height = 600),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  
 
  
  # UI - s - 2 ---------------------------------------------------
  
  output$box_los2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_los2",
        width = NULL,
        height = 800,
        tabPanel(
          title = "KM with ALL Group",
          
          div(
            style = "position: absolute; left: 4em; bottom: 0.5em;",
            dropdown( 
              downloadButton(outputId = "down_kag_select", label = "Download plot"),
              size = "xs",
              icon = icon("download", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotOutput("plot_kag_select", height = 600),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
    )
  })
  

 
  
  # BOX v - 1 ------------------------------------------------------------
  
  plot_vdi_select <- reactive({
   
    
    p<-ggplot(data=di_plot(), aes(x=day_n, y=tumor_volume, color=mouse_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line() + geom_point() +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    
    direct.label(p,"last.polygons")  
    
  
 
  })
  
  output$plot_vdi_select <- renderPlot({
    plot_vdi_select()
  })
  
  
  # BOX v - 2 ----------------------------------------------------------
  
  plot_vdms_select <- reactive({
    
    p<-ggplot(data=di_plot_mean(), aes(x=day_n, y=tumor_volume_mean, color=sort_n)) +
      #facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) +
      geom_errorbar(aes(ymin=tumor_volume_mean-sd, ymax=tumor_volume_mean+sd), width=.2,
                    position=position_dodge(0.05)) 

    
    
    direct.label(p,"last.polygons")
    
  })
  
  output$plot_vdms_select <- renderPlot({
    plot_vdms_select()
  })
  
  
  
  
  plot_vdmf_select <- reactive({
    
    
    p<-ggplot(data=di_plot_mean(), aes(x=day_n, y=tumor_volume_mean, color=sort_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) +
      geom_errorbar(aes(ymin=tumor_volume_mean-sd, ymax=tumor_volume_mean+sd), width=.2,
                    position=position_dodge(0.05)) 
    

    
    
    direct.label(p,"last.polygons")
    
    
  })
  
  output$plot_vdmf_select <- renderPlot({
    plot_vdmf_select()
  })
  
  # BOX v - 3 ----------------------------------------------------------
  
  plot_vwms_select <- reactive({
    
    
    p<-ggplot(data=di_plot3_mean(), aes(x=tumor_volume_mean, y=body_weight_mean, color=sort_n)) +
      #facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    
    
    
    

    
    
    direct.label(p,"last.polygons")
    
    
  })
  
  output$plot_vwms_select <- renderPlot({
    plot_vwms_select()
  })
  
  

  
  plot_vwmf_select <- reactive({
    
    p<-ggplot(data=di_plot3_mean(), aes(x=tumor_volume_mean, y=body_weight_mean, color=sort_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) +
      geom_errorbar(aes(ymin=body_weight_mean-sd_bd, ymax=body_weight_mean+sd_bd), width=.2,
                    position=position_dodge(0.05)) 
    
    
    direct.label(p,"last.polygons")
    
    
    
  })
  
  output$plot_vwmf_select <- renderPlot({
    plot_vwmf_select()
  })
  
  
  plot_vwi_select <- reactive({
    
    p<-ggplot(data=di_plot2(), aes(x=tumor_volume, y= body_weight, color=mouse_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_y", strip.position = "bottom")+
      geom_line() + geom_point() +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    


    
    
    direct.label(p,"last.bumpup")
    
    
    
  })
  
  output$plot_vwi_select <- renderPlot({
    plot_vwi_select()
  })
  
  # BOX w - 1 -------------------------------------------------------------------
  
  
  plot_wdi_select <- reactive({
    
    p<-ggplot(data=di_plot2(), aes(x=day_n, y=body_weight, color=mouse_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line() + geom_point() +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    
    
    direct.label(p,"last.polygons")
    
    
  })
  
  output$plot_wdi_select <- renderPlot({
    plot_wdi_select()
  })
  
  

  
  plot_wdmf_select <- reactive({
    
    p<-ggplot(data=di_plot2_mean(), aes(x=day_n, y=body_weight_mean, color=sort_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) +
      geom_errorbar(aes(ymin=body_weight_mean-sd, ymax=body_weight_mean+sd), width=.2,
                    position=position_dodge(0.05)) 
    

    
    
    
    direct.label(p,"last.polygons")
    
    
  })
  
  output$plot_wdmf_select <- renderPlot({
    plot_wdmf_select()
  }) 
  
  
  plot_wdms_select <- reactive({
    
    p<-ggplot(data=di_plot2_mean(), aes(x=day_n, y=body_weight_mean, color=sort_n)) +
      #facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    
    
    

    
    
    direct.label(p,"last.polygons")    
    
  })
  
  output$plot_wdms_select <- renderPlot({
    plot_wdms_select()
  })
  # BOX w - 2 --------------------------------------------------------------
  
  
  plot_wvi_select <- reactive({
    
    p<-ggplot(data=di_plot2(), aes(x=body_weight, y=tumor_volume, color=mouse_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line() + geom_point() +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    
    

 
    
    direct.label(p,"last.bumpup")
    
    
  })
  
  output$plot_wvi_select <- renderPlot({
    plot_wvi_select()
  })
  
  
  
  
  plot_wvmf_select <- reactive({
    
    p<-ggplot(data=di_plot3_mean(), aes(y=tumor_volume_mean, x=body_weight_mean, color=sort_n)) +
      facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) +
      geom_errorbar(aes(ymin=tumor_volume_mean-sd_tv, ymax=tumor_volume_mean+sd_tv), width=.2,
                    position=position_dodge(0.05)) 
    
    
    
    direct.label(p,"last.polygons")
    
    
  })
  
  output$plot_wvmf_select <- renderPlot({
    plot_wvmf_select()
  }) 
  
  
  plot_wvms_select <- reactive({
    
    
    
    
    p<-ggplot(data=di_plot3_mean(), aes(y=tumor_volume_mean, x=body_weight_mean, color=sort_n)) +
      #facet_wrap(~sort_n,nrow=4, scales = "free_x", strip.position = "bottom")+
      geom_line( ) + geom_point( ) +
      scale_color_manual("Mouse #",
                         values=col_vector()[1:n]) 
    
    
    

    
    direct.label(p,"last.polygons")    
  })
  
  output$plot_wvms_select <- renderPlot({
    plot_wvms_select()
  })
  
  
  
  # BOX gc - 1  ------------------------------------------------------------------
  
  
  plot_gc_select <- reactive({
    
    
    
    
    p <- ggviolin(data=di_plot2(),
                  x = 'sort_n', y = 'tumor_volume',
                  fill = 'sort_n',
                  facet.by='day_n',
                  palette = "npg",
                  legend = "none",
                  add = c("boxplot","jitter"), 
                  add.params = list(fill = "white"),
                  #remove = c(!is.na(dat[j])),
                  xlab = "Treatment Group",
                  ylab = "Tumor Volume"
                  
    ) 
    
    
    
    
    print(
      p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                             method = "anova", na.rm = TRUE, label.x = 2
      )
      + stat_compare_means(
        comparisons = my_comparisons2(), method = "t.test", label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
      
      + facet_wrap(day_n ~ ., labeller = as_labeller(day_names()), scale="free_y")) 
    
    
    
    
    
  })
  
  output$plot_gc_select <- renderPlot({
    plot_gc_select()
  })
  
  
  
  # BOX s - 1 -------------------------------------------------------------
  
  
  
  plot_kags_select <- reactive({
    
    ggsurv <- ggsurvplot(
      fit(),                     # survfit object with calculated statistics.
      data = di_surv_ful5(),             # data used to fit survival curves.
      risk.table = TRUE,       # show risk table.
      pval = TRUE,             # show p-value of log-rank test.
      conf.int = TRUE,         # show confidence intervals for 
      # point estimates of survival curves.
      #palette = c("#E7B800", "#2E9FDF"),
      xlim = c(0,(mx+5)),         # present narrower X axis, but not affect
      # survival estimates.
      xlab = "Time in days",   # customize X axis label.
      break.time.by = 10,     # break X axis in time intervals by 500.
      ggtheme = theme_light(), # customize plot and risk table with a theme.
      risk.table.y.text.col = T,# colour risk table text annotations.
      risk.table.height = 0.25, # the height of the risk table
      risk.table.y.text = FALSE,# show bars instead of names in text annotations
      # in legend of risk table.
      ncensor.plot = TRUE,      # plot the number of censored subjects at time t
      ncensor.plot.height = 0.25,
      conf.int.style = "step",  # customize style of confidence intervals
      surv.median.line = "hv",  # add the median survival pointer.
      #legend.labs =
      # c("Male", "Female")    # change legend labels.
    )
    
    
    customize_labels <- function (p, font.title = NULL,
                                  font.subtitle = NULL, font.caption = NULL,
                                  font.x = NULL, font.y = NULL, font.xtickslab = NULL, font.ytickslab = NULL)
    {
      original.p <- p
      if(is.ggplot(original.p)) list.plots <- list(original.p)
      else if(is.list(original.p)) list.plots <- original.p
      else stop("Can't handle an object of class ", class (original.p))
      .set_font <- function(font){
        font <- ggpubr:::.parse_font(font)
        ggtext::element_markdown (size = font$size, face = font$face, colour = font$color)
      }
      for(i in 1:length(list.plots)){
        p <- list.plots[[i]]
        if(is.ggplot(p)){
          if (!is.null(font.title)) p <- p + theme(plot.title = .set_font(font.title))
          if (!is.null(font.subtitle)) p <- p + theme(plot.subtitle = .set_font(font.subtitle))
          if (!is.null(font.caption)) p <- p + theme(plot.caption = .set_font(font.caption))
          if (!is.null(font.x)) p <- p + theme(axis.title.x = .set_font(font.x))
          if (!is.null(font.y)) p <- p + theme(axis.title.y = .set_font(font.y))
          if (!is.null(font.xtickslab)) p <- p + theme(axis.text.x = .set_font(font.xtickslab))
          if (!is.null(font.ytickslab)) p <- p + theme(axis.text.y = .set_font(font.ytickslab))
          list.plots[[i]] <- p
        }
      }
      if(is.ggplot(original.p)) list.plots[[1]]
      else list.plots
    }
    
    
    
    # Changing Labels
    # %%%%%%%%%%%%%%%%%%%%%%%%%%
    # Labels for Survival Curves (plot)
    ggsurv$plot <- ggsurv$plot + labs(
      title    = "Survival curves",
      subtitle = "Based on Kaplan-Meier estimates",
      caption  = "created with survminer"
    )
    
    # Labels for Risk Table 
    ggsurv$table <- ggsurv$table + labs(
      title    = "Note the risk set sizes",
      subtitle = "and remember about censoring.",
      #caption  = "source code: website.com"
    )
    
    # Labels for ncensor plot 
    ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(
      title    = "Number of censorings",
      subtitle = "over the time.",
      #caption  = "source code: website.com"
    )
    
    # Changing the font size, style and color
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Applying the same font style to all the components of ggsurv:
    # survival curves, risk table and censor part
    
    ggsurv <- customize_labels(
      ggsurv,
      font.title    = c(16, "bold", "darkblue"),
      font.subtitle = c(15, "bold.italic", "purple"),
      font.caption  = c(14, "plain", "orange"),
      font.x        = c(14, "bold.italic", "red"),
      font.y        = c(14, "bold.italic", "darkred"),
      font.xtickslab = c(12, "plain", "darkgreen")
    )
    
    
    
    
    # Using specific fonts for risk table and ncensor plots
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Font for Risk Table
    ggsurv$table <- customize_labels(
      ggsurv$table,
      font.title    = c(13, "bold.italic", "green"),
      font.subtitle = c(15, "bold", "pink"),
      font.caption  = c(11, "plain", "darkgreen"),
      font.x        = c(8, "bold.italic", "orange"),
      font.y        = c(11, "bold.italic", "darkgreen"),
      font.xtickslab = c(9, "bold", "red")
    )
    
    
    # Font for ncensor plot
    ggsurv$ncensor.plot <- customize_labels(
      ggsurv$ncensor.plot,
      font.title    = c(13, "bold.italic", "green"),
      font.subtitle = c(15, "bold", "pink"),
      font.caption  = c(11, "plain", "darkgreen"),
      font.x        = c(8, "bold.italic", "orange"),
      font.y        = c(11, "bold.italic", "darkgreen"),
      font.xtickslab = c(9, "bold", "red")
    )
    
    print(ggsurv$plot)
    
    
    
    
    
  })
  
  output$plot_kags_select <- renderPlot({
    plot_kags_select()
  })
  
  
  
  # BOX s - 2 -------------------------------------------------------------
 
  
  
  
  plot_kag_select <- reactive({
    
    
    ggsurv <- ggsurvplot(
      fit2(),                     # survfit object with calculated statistics.
      data = di_surv_ful5(),             # data used to fit survival curves.
      conf.int = TRUE,         # show confidence intervals for 
      # point estimates of survival curves.
      xlim = c(0,(mx+5)),         # present narrower X axis, but not affect
      # survival estimates.
      xlab = "Time in days",   # customize X axis label.
      break.time.by = 10,     # break X axis in time intervals by 500.
      ggtheme = theme_light(), # customize plot and risk table with a theme.
      conf.int.style = "step",  # customize style of confidence intervals
      surv.median.line = "hv",  # add the median survival pointer.
    )
    
    
    
    
    print(ggsurv$plot)
    
    
    
    
    
    
    
  })
  
  output$plot_kag_select <- renderPlot({
    plot_kag_select()
  })
  
  
  
  # DOWNLOAD ----------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, "_volume_weight_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(di_plot2(), file)
    }
  )
  
  output$downloadMicroData <- downloadHandler(
    filename = function() {
      paste(input$filename, "_survival_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(di_surv_ful5(), file)
    }
  )
  
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot, device = "png", width = 20, height=25)
      }
    )
  }
  
  output$down_vdi_select <- download_box("volumevsday_indv", plot_vdi_select())
  output$down_vdmf_select <- download_box("volumevsday_mean_facet", plot_vdmf_select())
  output$down_vdms_select <- download_box("volumevsday_mean_single", plot_vdms_select())
  output$down_vwmf_select <- download_box("volumevsweight_mean_facet", plot_vwmf_select())
  output$down_vwms_select <- download_box("volumevsweight_mean_single", plot_vwms_select())
  output$down_vwi_select <- download_box("volumevsweight_indv", plot_vwi_select())
  output$down_wdi_select <- download_box("weightvsday_indv", plot_wdi_select())
  output$down_wdmf_select <- download_box("weightvsday_mean_facet", plot_wdmf_select())
  output$down_wdms_select <- download_box("weightvsday_mean_single", plot_wdms_select())
  output$down_wvi_select <- download_box("weightvsvolume_indv", plot_wvi_select())
  output$down_wvmf_select <- download_box("weightvsvolume_mean_facet", plot_wvmf_select())
  output$down_wvms_select <- download_box("weightvsvolume_mean_single", plot_wvms_select())

  
  output$down_gc_select <- download_box("group compare", plot_gc_select())
  output$down_kags_select <- download_box("KM with ALL Group Seperated", plot_kags_select()) 
  output$down_kag_select <- download_box("KM with ALL Group", plot_kag_select())
  
  
}