
#' @title 使用joinpoint command line进行ASR的APC与AAPC的计算
#'
#' @param data GBD数据
#' @param model 模型选择: 'linear' or 'ln' (default)
#' @param rei_included 是否包含rei数据
#' @param CI 是否计算CI
#' @param digits 保留小数点位数
#' @param sep 95%CI的连接符号
#' @param constant_variance 是否认为方差恒定，如果你只选择val数据进行建模，则选择恒定方差，T；如果你想充分利用模型的区间信息，这选择非恒定方差，F；
#' @param AAPCrange  计算各个区段的AAPC值，若不需要，可以填写NULL；若需要计算，需要指定6个数值，分别对应各个区段起始段和结束段，比如AAPCrange=c(1990,1999,2000,2009,2010,2019)代表1990-1999，2000-2009，2010-2019的AAPC值
#' @param startyear 计算您需要的AAPC的起始年份
#' @param endyear 计算您需要的AAPC的结束年份
#' @param joinpoints joinpoint最大节点数
#' @param metric_name 可以从c('Rate','Number','Percent')选择
#'
#' @import stringr
#' @import dplyr
#' @import do
#' @import configr
#' @importFrom parallel detectCores
#' @importFrom fs file_delete
#' @references https://surveillance.cancer.gov/help/joinpoint
#' @return database of AAPC results
#' @export
#' @examples
GBDASR_aapc <- function(data,startyear=1990,endyear=2023, model = 'ln', joinpoints = 3, rei_included = F,
                        CI = TRUE, digits=2, sep=' to ',constant_variance=F, metric_name = 'Rate',
                        AAPCrange=NULL) {
  
  
  message(paste0(tmcn::toUTF8("\n-- 根据您设置的startyear与endyear，GBDASR_aapc运行结果中full range代表的是计算您提供数据中"),startyear,tmcn::toUTF8("至"),endyear,
                 tmcn::toUTF8("期间ASR的AAPC值 --\n")))
  
  data <- data %>% dplyr::filter(year %in% startyear:endyear)
  
  if ('location_name' %in% names(data) | 'location_id' %in% names(data)){
    stop(tmcn::toUTF8("\n-- 请重新准备或者下载数据，变量名不能为'变量_id'或者'变量_name' --\n"))
  }
  data <- data %>% as.data.frame()
  
  year_range <- unique(as.numeric(data$year))
  max_year <- max(as.numeric(data$year))
  min_year <- min(as.numeric(data$year))
  
  if (max_year-min_year + 1  != length(year_range)){
    stop(tmcn::toUTF8("\n-- 您提供的年份数据不是连续的，请重新提供 --\n"))
  }
  
  if (!is.null(AAPCrange)){
    if (sum(AAPCrange %in% year_range) != 6){
      stop(tmcn::toUTF8("\n-- 您设置的AAPCrange的年份未包含在您提供的数据里，请重新设置AAPCrange或重新提供数据,且AAPCrange必须是6个年份 --\n"))
    }
  }
  
  # if (.check_GBD2021()) {
  #  if (length(year_range) != 42){
  #    stop(tmcn::toUTF8("\n-- 您提供的data数据必须包含1980-2021的数据，请重新提供 --\n"))
  #  }
  #} else {
  # if (length(year_range) != 30){
  #    stop(tmcn::toUTF8("\n-- 您提供的data数据必须包含1990-2019的数据，请重新提供 --\n"))
  #   }
  # }
  
  
  
  if(length(strsplit(getwd(), " ")[[1]]) > 1) {
    stop(paste('\n-- 工作路径里',getwd(),tmcn::toUTF8('有空格键，请修改成不含空格的路径 --\n'),sep=" "))
  }
  
  # if(length(strsplit(getwd(), "-")[[1]]) > 1) {
  #   stop(paste('\n-- 工作路径里',getwd(),tmcn::toUTF8("有'-'符号，请修改成不含'-'的路径 --\n"),sep=" "))
  # }
  
  if (stringr::str_detect(getwd(),"[\\p{Han}]")){
    stop(paste('\n-- 工作路径里',getwd(),tmcn::toUTF8('有中文名，请修改成英文路径 --\n'),sep=" "))
  }
  
  
  # if (do::is.mac()){
  #   stop(tmcn::toUTF8('\n-- MAC系统无法使用该功能 --\n'))
  # }
  if (nrow(data)==0){
    stop(tmcn::toUTF8('\n-- 数据只有0行，请核对数据 --\n'))
  }
  data <- subset(data,age =='Age-standardized' & metric == metric_name)
  if (nrow(data)==0){
    stop(tmcn::toUTF8('\n-- 数据没有age-standardized rate数据，请核对数据 --\n'))
  }
  
  
  
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  data$location <- sub(",", replacement = "", data$location)
  
  
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  data$cause <- sub(",", replacement = "", data$cause)
  
  if ('rei' %in% names(data)){
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
    data$rei <- sub(",", replacement = "", data$rei)
  }
  
  data <- subset(data, age == 'Age-standardized' &
                   metric == metric_name &
                   val > 0)
  
  if (TRUE){
    result <- .GBDASR_aapc(data, model, joinpoints, rei_included,
                           CI, digits, sep,constant_variance,AAPCrange,startyear,endyear,metric_name)
  }
  
  return(result)
}



#' @import stringr
#' @import dplyr
#' @import do
#' @import configr
#' @importFrom parallel detectCores
#' @importFrom fs file_delete
.GBDASR_aapc <- function(data, model, joinpoints,rei_included,
                         CI, digits, sep,constant_variance,AAPCrange,startyear,endyear,metric_name) {
  
  
  if (rei_included == F) {
    
    if (constant_variance==T) {
      
      data <- data %>% dplyr::select(measure,location,sex,cause,year,val) %>%
        dplyr::arrange(measure,location,sex,cause,year)
      
      write.table(data,'joinpoint.csv',row.names = F,col.names = F,sep = ',')
      
      AAPC_Created_Session <- configr::read.config(system.file('data','AAPC_constant.Created.Session.ini',package = 'easyGBDR'))
      AAPC_JPOptions <- configr::read.config(system.file('data','AAPC_constant.JPOptions.ini',package = 'easyGBDR'))
    } else {
      if ((!"rei" %in% names(data)) & ncol(data)<10){
        stop(tmcn::toUTF8('\n-- constant_variance==F情况下请使用原始数据，不要对变量进行删减 --\n'))
      }
      
      if ("rei" %in% names(data) & ncol(data)<11){
        stop(tmcn::toUTF8('\n-- constant_variance==F情况下请使用原始数据，不要对变量进行删减 --\n'))
      }
      data$SE <- (data$upper-data$lower)/(1.96*2)
      
      data <- data %>% dplyr::select(measure,location,sex,cause,year,val,SE) %>%
        dplyr::arrange(measure,location,sex,cause,year)
      
      write.table(data,'joinpoint.csv',row.names = F,col.names = F,sep = ',')
      
      AAPC_Created_Session <- configr::read.config(system.file('data','AAPC.Created.Session.ini',package = 'easyGBDR'))
      AAPC_JPOptions <- configr::read.config(system.file('data','AAPC.JPOptions.ini',package = 'easyGBDR'))
    }
    AAPC_JPOptions[["Session Options"]][["Maximum joinpoints"]] <- joinpoints
    AAPC_JPOptions[["Session Options"]][["Num Cores"]] <- ifelse((parallel::detectCores()-2)<2,2,(parallel::detectCores()-2))
    AAPC_JPOptions[["Session Options"]][["Model"]] <- model
    #   if (!is.null(pairwise)){
    #    AAPC_JPOptions[["Session Options"]][["Pairwise"]] <- pairwise
    #  }
    
    AAPC_Created_Session[["Datafile options"]][["Datafile name"]] <- paste(getwd(),'joinpoint.csv',sep='/')
    
    if (!is.null(AAPCrange)){
      AAPC_JPOptions[["Export Options"]][["AAPC Start Range1"]] <- AAPCrange[1]
      AAPC_JPOptions[["Export Options"]][["AAPC End Range1"]] <- AAPCrange[2]
      AAPC_JPOptions[["Export Options"]][["AAPC Start Range2"]] <- AAPCrange[3]
      AAPC_JPOptions[["Export Options"]][["AAPC End Range2"]] <- AAPCrange[4]
      AAPC_JPOptions[["Export Options"]][["AAPC Start Range3"]] <- AAPCrange[5]
      AAPC_JPOptions[["Export Options"]][["AAPC End Range3"]] <- AAPCrange[6]
    }
    
    
    
    if (metric_name == 'Number'){
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["count location"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]]
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["count"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]]
      
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]] <- NULL
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]] <- NULL
    } else if (metric_name == 'Percent'){
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["percent location"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]]
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["percent"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]]
      
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]] <- NULL
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]] <- NULL
    }
    
    
    configr::write.config(config.dat = AAPC_Created_Session, file.path = sprintf("%s/AAPC.Created.Session.ini", getwd()), write.type = "ini")
    configr::write.config(config.dat = AAPC_JPOptions, file.path = sprintf("%s/AAPC.JPOptions.ini", getwd()), write.type = "ini")
    
    
    # Create an input file for the Joinpoint program(?????????????ļ?)
    filedir <- paste0(getwd(),'/')
    jprun.ini.file =  "AAPC.JPRun.ini"
    cat(file = jprun.ini.file, "[Joinpoint Input Files]\n", append = FALSE)
    cat(file = jprun.ini.file, paste0("Session File=", filedir, "AAPC.Created.Session.ini\n"), append = TRUE)
    cat(file = jprun.ini.file, paste0("Output File=", filedir, "AAPC", ".jpo\n"), append = TRUE)
    cat(file = jprun.ini.file, paste0("Export Options File=", filedir, "AAPC.JPOptions.ini\n"), append = TRUE)
    cat(file = jprun.ini.file, paste0("Run Options File=", filedir, "AAPC.JPOptions.ini"), append = TRUE)
    
    
    file.copy(system.file('data','jpCommand.exe',package = 'easyGBDR'),getwd())
    file_name <- paste(getwd(),'jpCommand.exe',sep='/')
    system(paste(file_name, jprun.ini.file))
    
    # system(paste(system.file('data','jpCommand.exe',package = 'easyGBDR'), jprun.ini.file))
    
    AAPC <- read.table('AAPC.aapcexport.txt', row.names = NULL,header= TRUE)
    APC <- read.table('AAPC.apcexport.txt', row.names = NULL,header= TRUE)
    data_model <- read.table('AAPC.dataexport.txt', row.names = NULL,header= TRUE)
    
    names(AAPC)<- c('measure','location','sex','cause','joinpoint',"AAPC.Index","Start.Obs","End.Obs" ,'AAPC','AAPC_LCI','AAPC_UCI',
                    'Significant_indicator',"Test.Statistic", "P.Value")
    
    names(APC) <- c('measure','location','sex','cause','joinpoint',"Segment","Segment.Start","Segment.End",'APC','APC_LCI','APC_UCI',
                    'Significant_indicator',"Test.Statistic", "P.Value")
    
    if (constant_variance){
      names(data_model) <-c('measure','location','sex','cause','year','val','model_val',
                            'APC_label','joinpoint')
    } else {
      names(data_model) <-c('measure','location','sex','cause','year','val','model_val','Standard.Error',
                            'APC_label','joinpoint')
    }
    
    AAPC1 <- AAPC %>% dplyr::filter(AAPC.Index == 'Full Range')
    
    data_model <- dplyr::left_join(data_model,AAPC1 %>% dplyr::select(measure,location,sex,cause,AAPC))
    
    
    invisible()
  }
  
  if (rei_included == T) {
    
    
    if (constant_variance==T) {
      data <- data %>% dplyr::select(measure,location,sex,cause,rei,year,val) %>%
        dplyr::arrange(measure,location,sex,cause,rei,year)
      
      write.table(data,'joinpoint.csv',row.names = F,col.names = F,sep = ',')
      AAPC_Created_Session <- configr::read.config(system.file('data','AAPC_constant.Created.Session.ini',package = 'easyGBDR'))
      AAPC_JPOptions <- configr::read.config(system.file('data','AAPC_constant.JPOptions.ini',package = 'easyGBDR'))
    } else {
      if ((!"rei" %in% names(data)) & ncol(data)<10){
        stop(tmcn::toUTF8('\n-- constant_variance==F情况下请使用原始数据，不要对变量进行删减 --\n'))
      }
      
      if ("rei" %in% names(data) & ncol(data)<11){
        stop(tmcn::toUTF8('\n-- constant_variance==F情况下请使用原始数据，不要对变量进行删减 --\n'))
      }
      data$SE <- (data$upper-data$lower)/(1.96*2)
      data <- data %>% dplyr::select(measure,location,sex,cause,rei,year,val,SE) %>%
        dplyr::arrange(measure,location,sex,cause,rei,year)
      
      write.table(data,'joinpoint.csv',row.names = F,col.names = F,sep = ',')
      AAPC_Created_Session <- configr::read.config(system.file('data','AAPC.Created.Session.ini',package = 'easyGBDR'))
      AAPC_JPOptions <- configr::read.config(system.file('data','AAPC.JPOptions.ini',package = 'easyGBDR'))
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["standard error location"]] <- '8'
    }
    AAPC_JPOptions[["Session Options"]][["Maximum joinpoints"]] <- joinpoints
    AAPC_JPOptions[["Session Options"]][["Num Cores"]] <- ifelse((parallel::detectCores()-2)<2,2,(parallel::detectCores()-2))
    AAPC_JPOptions[["Session Options"]][["Model"]] <- model
    # if (!is.null(pairwise)){
    #   AAPC_JPOptions[["Session Options"]][["Pairwise"]] <- pairwise
    # }
    
    AAPC_Created_Session[["Datafile options"]][["Datafile name"]] <- paste(getwd(),'joinpoint.csv',sep='/')
    AAPC_Created_Session[["Joinpoint Session Parameters"]][["by-var5"]] <- "rei"
    AAPC_Created_Session[["Joinpoint Session Parameters"]][["by-var5 location"]] <- "5"
    AAPC_Created_Session[["Joinpoint Session Parameters"]][["independent variable location"]] <- '6'
    AAPC_Created_Session[["Joinpoint Session Parameters"]][["age-adjusted rate location"]] <- '7'
    
    if (!is.null(AAPCrange)){
      AAPC_JPOptions[["Export Options"]][["AAPC Start Range1"]] <- AAPCrange[1]
      AAPC_JPOptions[["Export Options"]][["AAPC End Range1"]] <- AAPCrange[2]
      AAPC_JPOptions[["Export Options"]][["AAPC Start Range2"]] <- AAPCrange[3]
      AAPC_JPOptions[["Export Options"]][["AAPC End Range2"]] <- AAPCrange[4]
      AAPC_JPOptions[["Export Options"]][["AAPC Start Range3"]] <- AAPCrange[5]
      AAPC_JPOptions[["Export Options"]][["AAPC End Range3"]] <- AAPCrange[6]
    }
    
    
    if (metric_name == 'Number'){
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["count location"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]]
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["count"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]]
      
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]] <- NULL
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]] <- NULL
    } else if (metric_name == 'Percent'){
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["percent location"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]]
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["percent"]] <- AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]]
      
      
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate location"]] <- NULL
      AAPC_Created_Session[["Joinpoint Session Parameters"]][["crude rate"]] <- NULL
    }
    
    configr::write.config(config.dat = AAPC_Created_Session, file.path = sprintf("%s/AAPC.Created.Session.ini", getwd()), write.type = "ini")
    configr::write.config(config.dat = AAPC_JPOptions, file.path = sprintf("%s/AAPC.JPOptions.ini", getwd()), write.type = "ini")
    
    filedir <- paste0(getwd(),'/')
    jprun.ini.file =  "AAPC.JPRun.ini"
    cat(file = jprun.ini.file, "[Joinpoint Input Files]\n", append = FALSE)
    cat(file = jprun.ini.file, paste0("Session File=", filedir, "AAPC.Created.Session.ini\n"), append = TRUE)
    cat(file = jprun.ini.file, paste0("Output File=", filedir, "AAPC", ".jpo\n"), append = TRUE)
    cat(file = jprun.ini.file, paste0("Export Options File=", filedir, "AAPC.JPOptions.ini\n"), append = TRUE)
    cat(file = jprun.ini.file, paste0("Run Options File=", filedir, "AAPC.JPOptions.ini"), append = TRUE)
    
    
    file.copy(system.file('data','jpCommand.exe',package = 'easyGBDR'),getwd())
    file_name <- paste(getwd(),'jpCommand.exe',sep='/')
    system(paste(file_name, jprun.ini.file))
    
    system(paste(system.file('data','jpCommand.exe',package = 'easyGBDR'), jprun.ini.file))
    
    
    AAPC <- read.table('AAPC.aapcexport.txt',row.names = NULL, header= TRUE)
    
    APC <- read.table('AAPC.apcexport.txt',row.names = NULL, header= TRUE)
    data_model <- read.table('AAPC.dataexport.txt', row.names = NULL,header= TRUE)
    
    names(AAPC)<- c('measure','location','sex','cause','rei','joinpoint',"AAPC.Index","Start.Obs","End.Obs" ,'AAPC','AAPC_LCI','AAPC_UCI',
                    'Significant_indicator',"Test.Statistic", "P.Value")
    
    names(APC) <- c('measure','location','sex','cause','rei','joinpoint',"Segment","Segment.Start","Segment.End",'APC','APC_LCI','APC_UCI',
                    'Significant_indicator',"Test.Statistic", "P.Value")
    
    
    
    
    if (constant_variance){
      names(data_model) <-c('measure','location','sex','cause','rei','year','val','model_val',
                            'APC_label','joinpoint')
    } else {
      names(data_model) <-c('measure','location','sex','cause','rei','year','val','model_val','Standard.Error',
                            'APC_label','joinpoint')
    }
    AAPC1 <- AAPC %>% dplyr::filter(AAPC.Index == 'Full Range')
    data_model <- dplyr::left_join(data_model,AAPC1 %>% dplyr::select(measure,location,sex,cause,rei,AAPC))
    
    invisible()
  }
  
  AAPC$Significant_indicator[AAPC$Significant_indicator==0] <- 'No'
  AAPC$Significant_indicator[AAPC$Significant_indicator==1] <- 'Yes'
  
  APC$Significant_indicator[APC$Significant_indicator==0] <- 'No'
  APC$Significant_indicator[APC$Significant_indicator==1] <- 'Yes'
  
  
  AAPC$AAPC <- as.numeric(AAPC$AAPC)
  AAPC$AAPC_LCI <- as.numeric(AAPC$AAPC_LCI)
  AAPC$AAPC_UCI <- as.numeric(AAPC$AAPC_UCI)
  # AAPC$Test.Statistic <- as.numeric(AAPC$Test.Statistic)
  AAPC$P.Value <- as.numeric(AAPC$P.Value)
  
  
  
  APC$APC <- as.numeric(APC$APC)
  APC$APC_LCI <- as.numeric(APC$APC_LCI)
  APC$APC_UCI <- as.numeric(APC$APC_UCI)
  # APC$Test.Statistic <- as.numeric(APC$Test.Statistic)
  APC$P.Value <- as.numeric(APC$P.Value)
  
  data_model$AAPC <- as.numeric(data_model$AAPC)
  
  APC$metric <- metric_name
  AAPC$metric <- metric_name
  data_model$metric <- metric_name
  AAPC$age <- 'Age-standardized'
  APC$age <- 'Age-standardized'
  data_model$age <- 'Age-standardized'
  
  if (CI) {
    
    AAPC$AAPC_95CI <- paste(round(as.numeric(AAPC$AAPC),digits = digits),
                            round(as.numeric(AAPC$AAPC_LCI),digits = digits),sep='\n(') %>%
      paste(round(as.numeric(AAPC$AAPC_UCI),digits = digits),sep = sep) %>%
      paste0(')')
    
    APC$APC_95CI <- paste(round(as.numeric(APC$APC),digits = digits),
                          round(as.numeric(APC$APC_LCI),digits = digits),sep='\n(') %>%
      paste(round(as.numeric(APC$APC_UCI),digits = digits),sep = sep) %>%
      paste0(')')
  }
  
  
  #if (rei_included==T) {
  #   APC <- APC %>% select(1:3,17,4:16)
  #   AAPC <- AAPC %>% select(1:3,17,4:16)
  #} else {
  #   APC <- APC %>% select(1:3,16,4:15)
  #   AAPC <- AAPC %>% select(1:3,16,4:15)
  # }
  
  fs::file_delete('AAPC.aapcexport.txt')
  fs::file_delete('AAPC.apcexport.txt')
  fs::file_delete('AAPC.dataexport.txt')
  fs::file_delete('AAPC.Created.Session.ini')
  fs::file_delete('AAPC.finalselectedmodelexport.txt')
  #fs::file_delete('AAPC.jpo')
  fs::file_delete('AAPC.JPRun.ini')
  fs::file_delete('AAPC.JPRun.jps')
  fs::file_delete('AAPC.RunSummary.txt')
  fs::file_delete('joinpoint.csv')
  fs::file_delete('AAPC.JPOptions.ini')
  fs::file_delete('jpCommand.exe')
  
  
  AAPC_Range <- AAPC %>% filter(AAPC.Index != 'Full Range')
  AAPC <- subset(AAPC,AAPC.Index == 'Full Range')
  data_model <- unique(data_model)
  result <- list(AAPC=AAPC,
                 AAPC_Range = AAPC_Range,
                 APC=APC,
                 data=data_model,
                 label="ASR")
  return(result)
}
