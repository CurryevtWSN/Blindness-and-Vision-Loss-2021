
#' @title Prediction of rate for GBD data using BAPC package
#'
#' @param data GBD数据
#' @param full_age_adjusted full_age_adjusted = T是预测0-95岁年龄段的数据，如果部分年龄段数据有缺失，会自动用0对该部分年龄段数据进行填充；full_age_adjusted = F 是预测你提供数据里的年龄段数据段，比如你的数据里只有40-59的年龄段的，那么这种情况下只会预测40-59的年龄段的结果
#' @param measure_name 筛选measure名称
#' @param cause_name 筛选cause名称
#' @param location_name 筛选location名称
#' @param rei_name 筛选rei名称，若无填写NULL
#' @param By_sex 是否根据male和female的预测结果去计算both的预测结果
#' @param predyear 预测的最大年限
#' @param pop_predict 预测人口数据，可以选择GBD2017的预测人口学数据或者WHO的预测人口学数据，只能从c('GBD','WHO')中选择一个
#' @param rate_lessen
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom reshape2 dcast
#' @import epitools
#' @return list
#' @export
#'
#' @examples
GBDbapc_prediction <- function(data,measure_name,cause_name,
                               location_name,rei_name=NULL,By_sex=T,predyear = 2030,
                               full_age_adjusted=F,rate_lessen=NULL,pop_predict='GBD') {
  
  
  
  
  
  if (is.logical(rate_lessen)){
    stop(tmcn::toUTF8("\n-- rate_lessen必须为具体的缩放倍数 --\n"))
  }
  
  if ('location_name' %in% names(data) | 'location_id' %in% names(data)){
    stop(tmcn::toUTF8("\n-- 请重新准备或者下载数据，变量名不能为'变量_id'或者'变量_name' --\n"))
  }
  
  if (pop_predict=='GBD'){
    cat(sprintf("\n-- 温馨提示 --\n"))
    GBDpredict_loct <- unique(easyGBDR::GBDprediction2$location)
    cat("\n-- pop_predict='GBD'，代表您使用的预测人口为GBD2017的数据 --\n")
  } else if (pop_predict=='WHO'){
    cat(sprintf("\n-- 温馨提示 --\n"))
    GBDpredict_loct <- unique(easyGBDR::WHOpop_predict$location)
    cat("\n-- pop_predict='WHO'，代表您使用的预测人口为联合国的预测人口数据 --\n")
  }
  
  data <- data %>% dplyr::filter(location %in% GBDpredict_loct)
  
  location_name <- location_name[location_name %in% unique(data$location)]
  
  
  if (!is.null(rei_name)){
    if (is.logical(rei_name)) {
      stop(tmcn::toUTF8("\n-- 请重新设置你您的rei_name，您的rei_name为具体的label，比如All risk factors,而非逻辑判断值 --\n"))
    }
  }
  
  if (!is.null(rei_name)){
    if (length(rei_name %in% unique(data$rei)) != sum(rei_name %in% unique(data$rei))){
      stop(tmcn::toUTF8("\n-- 请重新设置你您的rei_name，您的rei_name参数里的label无法和您data里rei变量的label一一对应 --\n"))
    }
  }
  
  if (length(cause_name %in% unique(data$cause)) != sum(cause_name %in% unique(data$cause))){
    stop(tmcn::toUTF8("\n-- 请重新设置你您的cause_name，您的cause_name参数里的label无法和您data里cause变量的label一一对应 --\n"))
  }
  
  if (length(measure_name %in% unique(data$measure)) != sum(measure_name %in% unique(data$measure))){
    stop(tmcn::toUTF8("\n-- 请重新设置你您的measure_name，您的measure_name参数里的label无法和您data里measure变量的label一一对应 --\n"))
  }
  if (length(location_name %in% unique(data$location)) != sum(location_name %in% unique(data$location))){
    stop(tmcn::toUTF8("\n-- 请重新设置你您的location_name，您的location_name参数里的label无法和您data里location变量的label一一对应 --\n"))
  }
  
  
  ages_2 <- c("0 to 4","5 to 9","10 to 14", "15 to 19","20 to 24", "25 to 29",
              "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
              "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",  "85 to 89",
              "90 to 94", "95 plus")
  
  
  if (("rei" %in% names(data))==TRUE & length(rei_name)==0) {
    stop('Please define your rei in your analysis')
  }
  
  
  if (!'Number' %in% unique(data$metric)) {
    stop(tmcn::toUTF8('\n-- 您提供的各个年龄段的数据metric没有Number数据，请重新提供GBD数据'))
  }
  if (min(data$val)<0) {
    stop(tmcn::toUTF8('\n-- 数据集中有数据val值<0，请用filter或subset过滤后再使用该函数'))
  }
  
  if (nrow(data)==0){
    stop(tmcn::toUTF8('\n-- 您提供的数据metric没有Number数据，请重新提供GBD数据'))
  }
  if (length(unique(data$sex)) !=3){
    stop(tmcn::toUTF8('\n-- 您提供的数据必须包含Male,Female以及Both数据，请重新提供GBD数据'))
  }
  
  
  
  data$location <- sub("Côte d'Ivoire", replacement = "Coted'Ivoire", data$location)
  data$location <- sub("C么te d'Ivoire", replacement = "Coted'Ivoire", data$location)
  
  location_name <- sub("Côte d'Ivoire", replacement = "Coted'Ivoire", location_name)
  location_name <- sub("C么te d'Ivoire", replacement = "Coted'Ivoire", location_name)
  
  
  data <- trans_age(data)
  
  
  
  dat <- data
  data <- subset(data, metric=='Number')
  dat2 <- data
  data_NA <- subset(data,val == 0)
  #data <- subset(data,val != 0)
  
  
  if (full_age_adjusted==T){
    
    cat(sprintf("\n-- 您设置的参数full_age_adjusted=T，代表您预测的是 0 to 4 到 95 plus 的数据 --\n"))
    cat(sprintf("\n-- 请确保数据中已含有 0 to 4 到 95 plus 的所有数据，若数据有缺失，我们将对缺失的年龄段数据用0进行填充 --\n"))
  } else {
    data_indicate <- dat2 %>% dplyr::filter(!age %in% c('All ages','Age-standardized'))
    unique_age <- unique(data_indicate$age)
    unique_age <- sub('95 plus',replacement = '95 to 99', unique_age)
    unique_age <- matrix(as.numeric(unlist(strsplit(unique(unique_age),split="to"))),
                         ncol=2,byrow=T)[,1]
    startage <- min(unique_age)
    endage <- max(unique_age)
    
    if (endage==95) {
      endage = '95 plus'
    } else {
      endage <-  paste(endage,endage+4,sep = ' to ')
    }
    startage <-  paste(startage,startage+4,sep = ' to ')
    
    
    cat(sprintf("\n-- 您设置的参数full_age_adjusted=F，代表您预测的是部分年龄段的数据 --\n"))
    cat(sprintf(paste("\n-- 根据您提供的数据，您预测的年龄段为",startage,'到',endage,'的数据 --\n')))
    
    age_zero <- unique(data_NA$age)
    if (length(age_zero)>0){
      cat(sprintf(paste("\n-- 需要注意的是，您提供的",paste(age_zero,collapse =','),'的数据均为0 --\n')))
    }
    
    
  }
  
  
  if (.check_GBDedition()==2023){
    basicyear = min(data$year)
    endyear =2023
    
  } else if (.check_GBDedition()==2021) {
    basicyear = min(data$year)
    endyear =2021
    
  } else if (.check_GBDedition()==2019) {
    basicyear = 1990
    endyear =2019
    
  }
  
  
  
  if (pop_predict=='GBD'){
    GBDpredict <- easyGBDR::GBDprediction2
  } else if (pop_predict=='WHO'){
    GBDpredict <- easyGBDR::WHOpop_predict
  }
  
  
  location_name2 <- unique(GBDpredict$location)
  location_name <- location_name[which(location_name %in% location_name2)]
  
  
  
  if (.check_GBDedition()==2023){
    population <- easyGBDR::GBDpopulation_2023 %>% tidytable::filter(location %in% location_name,year %in% basicyear:endyear) %>%
      tidytable::select(location,sex,year,age,val) %>% as.data.frame()
    
  } else if (.check_GBDedition()==2021) {
    population <- easyGBDR::GBDpopulation_2021 %>% tidytable::filter(location %in% location_name,year %in% basicyear:endyear) %>%
      tidytable::select(location,sex,year,age,val) %>% as.data.frame()
    
  } else if (.check_GBDedition()==2019) {
    population <- easyGBDR::GBDpopulation_2019 %>% dplyr::filter(location %in% location_name,year %in% basicyear:endyear)
    
  }
  
  GBDpredict <- GBDpredict %>% dplyr::filter(year %in% ((max(population$year)+1):predyear),location %in% location_name)
  
  population <- rbind(population,GBDpredict)
  
  names(population) <- c('location','sex','year','age','val')
  
  
  
  if (.check_GBDedition()==2023){
    std <- easyGBDR::std_GBD2021
    std$std_population[6] <- sum(std$std_population[1:6])
    std[6,1] <- '0 to 4'
    std <- std[-c(1:5),]
    std$percent <-  std$std_population/sum(std$std_population)
    
  } else if (.check_GBDedition()==2021) {
    std <- easyGBDR::std_GBD2021
    std$std_population[6] <- sum(std$std_population[1:6])
    std[6,1] <- '0 to 4'
    std <- std[-c(1:5),]
    std$percent <-  std$std_population/sum(std$std_population)
    
  } else if (.check_GBDedition()==2019) {
    std <- easyGBDR::std_GBD2019[-c(1:3),]
    std$std_population[2] <- std$std_population[1] + std$std_population[2]
    std <- std[-1,]
    std[1,1] <- '0 to 4'
    std$percent <-  std$std_population/sum(std$std_population)
    
  }
  
  # std <- force(std_GBD2019)[-c(1:3),]
  # std$std_population[2] <- std$std_population[1] + std$std_population[2]
  # std <- std[-1,]
  # std[1,1] <- '0 to 4'
  
  
  
  
  a = measure_name[1]
  b = cause_name[1]
  e = rei_name[1]
  d = location_name[1]
  if (is.null(rei_name)==T){
    i=0
    for (a in measure_name){
      for (b in cause_name){
        for (d in location_name){
          if (i==0) {
            if (.checkAccess()){
              result <- .GBDbapc_prediction(data,data_NA,ages_2,dat,population,std,
                                            measure_name=a,cause_name=b,
                                            location_name=d,rei_name,
                                            By_sex,predyear,full_age_adjusted,rate_lessen,basicyear,endyear)
            }
            
            
            
            i = i + 1
          } else {
            if (.checkAccess()){
              temp <- .GBDbapc_prediction(data,data_NA,ages_2,dat,population,std,
                                          measure_name=a,cause_name=b,
                                          location_name=d,rei_name,
                                          By_sex,predyear,full_age_adjusted,rate_lessen,basicyear,endyear)
            }
            
            
            result[["all_age_projection"]] <- rbind(result[["all_age_projection"]],temp[["all_age_projection"]])
            result[["crude_rate"]] <- rbind(result[["crude_rate"]],temp[["crude_rate"]])
            result[["ASR"]] <- rbind(result[["ASR"]],temp[["ASR"]])
            result[["Age_standardized_projection"]] <- rbind(result[["Age_standardized_projection"]],temp[["Age_standardized_projection"]])
            result[["age_specific_rate"]] <- rbind(result[["age_specific_rate"]],temp[["age_specific_rate"]])
            result[["age_specific_projection"]] <- rbind(result[["age_specific_projection"]],temp[["age_specific_projection"]])
            i = i + 1
          }
        }
        
      }
    }
  } else {
    i=0
    for (a in measure_name){
      for (b in cause_name){
        for (d in location_name){
          for (e in rei_name){
            if (i==0) {
              if (.checkAccess()){
                result <- .GBDbapc_prediction(data,data_NA,ages_2,dat,population,std,
                                              measure_name=a,cause_name=b,
                                              location_name=d,rei_name=e,
                                              By_sex,predyear,full_age_adjusted,rate_lessen,basicyear,endyear)
              }
              
              
              
              i = i + 1
            } else {
              if (.checkAccess()){
                temp <- .GBDbapc_prediction(data,data_NA,ages_2,dat,population,std,
                                            measure_name=a,cause_name=b,
                                            location_name=d,rei_name=e,
                                            By_sex,predyear,full_age_adjusted,rate_lessen,basicyear,endyear)
              }
              
              
              result[["all_age_projection"]] <- rbind(result[["all_age_projection"]],temp[["all_age_projection"]])
              result[["crude_rate"]] <- rbind(result[["crude_rate"]],temp[["crude_rate"]])
              result[["ASR"]] <- rbind(result[["ASR"]],temp[["ASR"]])
              result[["Age_standardized_projection"]] <- rbind(result[["Age_standardized_projection"]],temp[["Age_standardized_projection"]])
              result[["age_specific_rate"]] <- rbind(result[["age_specific_rate"]],temp[["age_specific_rate"]])
              result[["age_specific_projection"]] <- rbind(result[["age_specific_projection"]],temp[["age_specific_projection"]])
              
              i = i + 1
            }
          }
        }
      }
    }
  }
  
  return(result)
}

.GBDbapc_prediction <- function(data,data_NA,ages_2,dat,population,std,measure_name,cause_name,
                                location_name,rei_name,By_sex,predyear,
                                full_age_adjusted,rate_lessen,basicyear,endyear) {
  
  
  
  if (sum(location_name %in% unique(data$location)) !=length(location_name)){
    stop(tmcn::toUTF8('结果出错，请核对您的location_name的label是否和和原数据中的location的label一致'))
  }
  
  if (sum(measure_name %in% unique(data$measure)) !=length(measure_name)){
    stop(tmcn::toUTF8('结果出错，请核对您的measure_name的label是否和和原数据中的measure的label一致'))
  }
  
  if (sum(cause_name %in% unique(data$cause)) !=length(cause_name)){
    stop(tmcn::toUTF8('结果出错，请核对您的cause_name的label是否和和原数据中的cause的label一致'))
  }
  
  if (is.null(rei_name) & 'rei' %in% names(data)){
    stop(tmcn::toUTF8('结果出错，您的原始数据有rei变量，请制定您的分析中的rei_name，且不不能为NULL'))
  }
  
  if (!is.null(rei_name)){
    if (sum(rei_name %in% unique(data$rei)) !=length(rei_name)){
      stop(tmcn::toUTF8('结果出错，请核对您的rei_name的label是否和和原数据中的rei的label一致'))    }
  }
  
  
  
  
  
  
  
  if (is.null(rei_name) == T) {
    Male_data <- subset(data,age != 'Age-standardized' &
                          age != 'All ages' &
                          sex == 'Male' &
                          metric == 'Number' &
                          measure == measure_name &
                          location  == location_name &
                          cause  == cause_name)
    
    Female_data <- subset(data,age != 'Age-standardized' &
                            age != 'All ages' &
                            sex == 'Female' &
                            metric == 'Number' &
                            measure == measure_name &
                            location  == location_name &
                            cause  == cause_name)
    
    Both_data <- subset(data,age != 'Age-standardized' &
                          age != 'All ages' &
                          sex == 'Both' &
                          metric == 'Number' &
                          measure == measure_name &
                          location  == location_name &
                          cause  == cause_name)
    
    data_NA <- subset(data_NA,age != 'Age-standardized' &
                        age != 'All ages' &
                        metric == 'Number' &
                        measure == measure_name &
                        location  == location_name &
                        cause  == cause_name)
  } else {
    Male_data <- subset(data,age != 'Age-standardized' &
                          age != 'All ages' &
                          sex == 'Male' &
                          metric == 'Number' &
                          measure == measure_name &
                          location  == location_name &
                          cause  == cause_name &
                          rei == rei_name)
    
    Female_data <- subset(data,age != 'Age-standardized' &
                            age != 'All ages' &
                            sex == 'Female' &
                            metric == 'Number' &
                            measure == measure_name &
                            location  == location_name &
                            cause  == cause_name &
                            rei == rei_name)
    
    Both_data <- subset(data,age != 'Age-standardized' &
                          age != 'All ages' &
                          sex == 'Both' &
                          metric == 'Number' &
                          measure == measure_name &
                          location  == location_name &
                          cause  == cause_name &
                          rei == rei_name)
    
    data_NA <- subset(data_NA,age != 'Age-standardized' &
                        age != 'All ages' &
                        metric == 'Number' &
                        measure == measure_name &
                        location  == location_name &
                        cause  == cause_name &
                        rei == rei_name)
    
  }
  data_all <- rbind(Male_data, Female_data, Both_data)
  
  ## 用于填塞0用
  age_0 <- unique(data_NA$age)
  
  
  if (.check_GBDedition()==2023){
    basicyear = min(data_all$year)
    endyear =2023
    
  } else if (.check_GBDedition()==2021) {
    basicyear = min(data_all$year)
    endyear =2021
    
  } else if (.check_GBDedition()==2019) {
    basicyear = 1990
    endyear =2019
    
  }
  
  
  unique_age <- unique(Both_data$age)
  
  
  
  unique_age <- sub('95 plus',replacement = '95 to 99', unique_age)
  unique_age2 <- matrix(as.numeric(unlist(strsplit(unique(unique_age),split="to"))),
                        ncol=2,byrow=T)[,2]
  unique_age <- matrix(as.numeric(unlist(strsplit(unique(unique_age),split="to"))),
                       ncol=2,byrow=T)[,1]
  
  diff_age <- unique_age2 - unique_age
  
  if (length(unique(diff_age)) !=1 | max(diff_age) !=4 |min(diff_age) !=4){
    stop(tmcn::toUTF8('您提供的年龄段只能含有5岁为一个年龄段的数据（可以包含ASR与all ages数据），但您的数据里有间隔不是5岁的年龄段数据，请重新提供数据'))
  }
  
  startage <- min(unique_age)
  endage <- max(unique_age)
  ages <- c()
  for (j in seq(from=startage,to=endage,by=5)) {
    ages <- c(ages,paste(j,j+4,sep=' to '))
  }
  
  if (endage == 95) {
    ages[length(ages)] <- '95 plus' }
  
  # for (a in basicyear:endyear){
  #   check_data <- subset(data_NA,year==a)
  #   if (length(unique(check_data$age) %in% ages)==length(ages)){
  #     Male_data <- rbind(Male_data,subset(check_data,sex=='Male'))
  #     Female_data <- rbind(Female_data,subset(check_data,sex=='Female'))
  #     Both_data <- rbind(Both_data,subset(check_data,sex=='Both'))
  #   }
  # }
  
  
  
  if (full_age_adjusted==F) {
    population_Male <- subset(population,location == location_name & sex == 'Male' & age %in% ages & year %in% basicyear:predyear)
    population_Female <- subset(population,location == location_name & sex == 'Female' & age %in% ages & year %in% basicyear:predyear)
  } else {
    population_Male <- subset(population,location == location_name & sex == 'Male' & age %in% ages_2 & year %in% basicyear:predyear)
    population_Female <- subset(population,location == location_name & sex == 'Female' & age %in% ages_2 & year %in% basicyear:predyear)
  }
  
  
  
  if (full_age_adjusted==F) {
    std2 <- subset(std,age %in% ages)
  } else {
    std2 <- subset(std,age %in% ages_2)
  }
  
  std2$percent <-  std2$std_population/sum(std2$std_population)
  wstand <- std2$percent
  
  population_Male_n <- reshape2::dcast(data = population_Male, year ~ age,value.var = c("val"))
  population_Female_n <- reshape2::dcast(data = population_Female, year ~ age,value.var = c("val"))
  
  rownames(population_Male_n) <- population_Male_n$year
  rownames(population_Female_n) <- population_Female_n$year
  
  if (nrow(population_Male_n)<=30){
    stop(paste0(tmcn::toUTF8('预测人口学数据中没有 '),location_name,tmcn::toUTF8('地区的数据，无法进行预测分析，请在使用函数前先过滤此地区')))
  }
  
  if (full_age_adjusted==F) {
    population_Male_n <- population_Male_n %>%
      dplyr::arrange(year) %>% dplyr::select(-1) %>%
      dplyr::select(ages)
    population_Female_n <- population_Female_n %>%
      dplyr::arrange(year) %>% dplyr::select(-1) %>%
      dplyr::select(ages)
  } else {
    population_Male_n <- population_Male_n %>%
      dplyr::arrange(year) %>% dplyr::select(-1) %>%
      dplyr::select(ages_2)
    population_Female_n <- population_Female_n %>%
      dplyr::arrange(year) %>% dplyr::select(-1) %>%
      dplyr::select(ages_2)
  }
  
  
  population_Male_n <- apply(population_Male_n, c(1,2), as.numeric) %>% as.data.frame()
  population_Female_n <- apply(population_Female_n, c(1,2), as.numeric) %>% as.data.frame()
  population_Male_n <- apply(population_Male_n, c(1,2), round) %>% as.data.frame()
  population_Female_n <- apply(population_Female_n, c(1,2), round) %>% as.data.frame()
  
  population_Both_n <- population_Female_n + population_Male_n
  
  rownames(population_Female_n) <- basicyear:predyear
  rownames(population_Male_n) <- basicyear:predyear
  rownames(population_Both_n) <- basicyear:predyear
  
  
  
  Male_data_n <- reshape2::dcast(data = Male_data, year~age, value.var = "val")
  Male_data_n <- Male_data_n %>%
    dplyr::arrange(year) ### 将数据按照year大小进行排列
  rownames(Male_data_n) <- Male_data_n$year ## 对行名重新命名
  Male_data_n <- Male_data_n[,-1]  #
  Male_data_n <- Male_data_n %>%
    apply(c(1,2), as.numeric) %>%
    apply(c(1,2), round) %>%
    as.data.frame()  %>%
    dplyr::select(ages)
  
  
  Female_data_n <- reshape2::dcast(data = Female_data, year~age, value.var = "val")
  Female_data_n <- Female_data_n %>%
    dplyr::arrange(year) ### 将数据按照year大小进行排列
  rownames(Female_data_n) <- Female_data_n$year ## 对行名重新命名
  Female_data_n <- Female_data_n[,-1]  #
  Female_data_n <- Female_data_n %>%
    apply(c(1,2), as.numeric) %>%
    apply(c(1,2), round) %>%
    as.data.frame()  %>%
    dplyr::select(ages)
  
  Both_data_n <- reshape2::dcast(data = Both_data, year~age, value.var = "val")
  Both_data_n <- Both_data_n %>%
    dplyr::arrange(year) ### 将数据按照year大小进行排列
  rownames(Both_data_n) <- Both_data_n$year ## 对行名重新命名
  Both_data_n <- Both_data_n[,-1]  #
  Both_data_n <- Both_data_n %>%
    apply(c(1,2), as.numeric) %>%
    apply(c(1,2), round) %>%
    as.data.frame()  %>%
    dplyr::select(ages)
  
  
  
  
  data_pro <- matrix(data = NA, nrow = predyear-max(data$year), ncol = ncol(population_Male_n)) %>% as.data.frame()
  rownames(data_pro) <- seq(max(data$year)+1,predyear,1)
  colnames(data_pro) <-  names(population_Male_n)
  
  
  if (full_age_adjusted==T){
    age_pro <- matrix(0,ncol = sum((!(ages_2 %in% ages))),nrow = nrow(Male_data_n)) %>% as.data.frame()
    names(age_pro) <-  ages_2[which(!(ages_2 %in% ages))]
    rownames(age_pro) <- basicyear:(basicyear-1+nrow(Male_data_n))
    Female_data_n <- cbind(age_pro,Female_data_n)
    Male_data_n <- cbind(age_pro,Male_data_n)
    Both_data_n <- cbind(age_pro,Both_data_n)
  }
  
  Male_data_n  <- rbind(Male_data_n, data_pro)
  Female_data_n  <- rbind(Female_data_n, data_pro)
  Both_data_n  <- rbind(Both_data_n, data_pro)
  
  if (length(rate_lessen)>0) {
    Male_data_n  <- Male_data_n*rate_lessen
    Female_data_n  <- Female_data_n*rate_lessen
    Both_data_n  <- Both_data_n*rate_lessen
  }
  
  Male_esoph <- BAPC::APCList(Male_data_n, population_Male_n, gf = 5)
  Male_bapc_result <- BAPC::BAPC(Male_esoph, predict = list(npredict = predyear - max(population$year), retro = F),
                                 secondDiff = FALSE, stdweight = wstand, verbose = F)
  
  Female_esoph <- BAPC::APCList(Female_data_n, population_Female_n, gf = 5)
  Female_bapc_result <- BAPC::BAPC(Female_esoph, predict = list(npredict = predyear - max(population$year), retro = F),
                                   secondDiff = FALSE, stdweight = wstand, verbose = F)
  
  Male_bapc_result <- BAPC::qapc(Male_bapc_result,percentiles=c(0.025,0.975))
  Female_bapc_result <- BAPC::qapc(Female_bapc_result,percentiles=c(0.025,0.975))
  
  if (full_age_adjusted==T) {
    ages_3 <- ages_2
  } else {
    ages_3 <- ages
  }
  
  if (full_age_adjusted==T) {
    if (length(setdiff(ages_3,ages)) !=0){
      for (i in 1:length(setdiff(ages_3,ages))) {
        Male_bapc_result@agespec.rate[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
        names(Male_bapc_result@agespec.rate[[i]]) <- c('mean','sd','0.025Q','0.975Q')
        Male_bapc_result@agespec.proj[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
        names(Male_bapc_result@agespec.proj[[i]]) <- c('mean','sd','0.025Q','0.975Q')
        
        Female_bapc_result@agespec.rate[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
        names(Female_bapc_result@agespec.rate[[i]]) <- c('mean','sd','0.025Q','0.975Q')
        Female_bapc_result@agespec.proj[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
        names(Female_bapc_result@agespec.proj[[i]]) <- c('mean','sd','0.025Q','0.975Q')
      }
    }
    
  }
  
  
  if (length(age_0) !=0){
    for (i in which(ages %in% age_0)) {
      Male_bapc_result@agespec.rate[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
      names(Male_bapc_result@agespec.rate[[i]]) <- c('mean','sd','0.025Q','0.975Q')
      Male_bapc_result@agespec.proj[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
      names(Male_bapc_result@agespec.proj[[i]]) <- c('mean','sd','0.025Q','0.975Q')
      
      Female_bapc_result@agespec.rate[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
      names(Female_bapc_result@agespec.rate[[i]]) <- c('mean','sd','0.025Q','0.975Q')
      Female_bapc_result@agespec.proj[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
      names(Female_bapc_result@agespec.proj[[i]]) <- c('mean','sd','0.025Q','0.975Q')
    }
  }
  
  
  
  ##### age-specific number
  ## Male
  Male_proj_mean <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Male_proj_low <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Male_proj_up <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  ### 建立循环语句，循环length(ages)次，依次将每个年龄组的数据的mean,95%CrI的上下限
  for (i in 1:length(ages_3)) {
    Male_proj_mean[,i] <- BAPC::agespec.proj(Male_bapc_result)[[i]][,1]
    Male_proj_low[,i] <- BAPC::agespec.proj(Male_bapc_result)[[i]][,3]
    Male_proj_up[,i] <- BAPC::agespec.proj(Male_bapc_result)[[i]][,4]
  }
  
  names(Male_proj_mean) <- ages_3
  rownames(Male_proj_mean) <- basicyear:predyear
  names(Male_proj_up) <- ages_3
  rownames(Male_proj_up) <- basicyear:predyear
  names(Male_proj_low) <- ages_3
  rownames(Male_proj_low) <- basicyear:predyear
  
  ## Female
  Female_proj_mean <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Female_proj_low <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Female_proj_up <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  ### 建立循环语句，循环length(ages_3)次，依次将每个年龄组的数据的mean,95%CrI的上下限
  for (i in 1:length(ages_3)) {
    Female_proj_mean[,i] <- BAPC::agespec.proj(Female_bapc_result)[[i]][,1]
    Female_proj_low[,i] <- BAPC::agespec.proj(Female_bapc_result)[[i]][,3]
    Female_proj_up[,i] <- BAPC::agespec.proj(Female_bapc_result)[[i]][,4]
  }
  
  names(Female_proj_mean) <- ages_3
  rownames(Female_proj_mean) <- basicyear:predyear
  names(Female_proj_up) <- ages_3
  rownames(Female_proj_up) <- basicyear:predyear
  names(Female_proj_low) <- ages_3
  rownames(Female_proj_low) <- basicyear:predyear
  
  ##### age-specific rate
  ## Male
  Male_rate_mean <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Male_rate_low <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Male_rate_up <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  ### 建立循环语句，循环length(ages_3)次，依次将每个年龄组的数据的mean,95%CrI的上下限
  for (i in 1:length(ages_3)) {
    Male_rate_mean[,i] <- BAPC::agespec.rate(Male_bapc_result)[[i]][,1]*10^5
    Male_rate_low[,i] <- BAPC::agespec.rate(Male_bapc_result)[[i]][,3]*10^5
    Male_rate_up[,i] <- BAPC::agespec.rate(Male_bapc_result)[[i]][,4]*10^5
  }
  
  names(Male_rate_mean) <- ages_3
  rownames(Male_rate_mean) <- basicyear:predyear
  names(Male_rate_up) <- ages_3
  rownames(Male_rate_up) <- basicyear:predyear
  names(Male_rate_low) <- ages_3
  rownames(Male_rate_low) <- basicyear:predyear
  
  ## Female
  Female_rate_mean <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Female_rate_low <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  Female_rate_up <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
  ### 建立循环语句，循环length(ages_3)次，依次将每个年龄组的数据的mean,95%CrI的上下限
  for (i in 1:length(ages_3)) {
    Female_rate_mean[,i] <- BAPC::agespec.rate(Female_bapc_result)[[i]][,1]*10^5
    Female_rate_low[,i] <- BAPC::agespec.rate(Female_bapc_result)[[i]][,3]*10^5
    Female_rate_up[,i] <- BAPC::agespec.rate(Female_bapc_result)[[i]][,4]*10^5
  }
  
  names(Female_rate_mean) <- ages_3
  rownames(Female_rate_mean) <- basicyear:predyear
  names(Female_rate_up) <- ages_3
  rownames(Female_rate_up) <- basicyear:predyear
  names(Female_rate_low) <- ages_3
  rownames(Female_rate_low) <- basicyear:predyear
  
  if (By_sex==F) {
    Both_esoph <- BAPC::APCList(Both_data_n, population_Both_n, gf = 5)
    Both_bapc_result <- BAPC::BAPC(Both_esoph, predict = list(npredict = predyear - max(population$year), retro = F),
                                   secondDiff = FALSE, stdweight = wstand, verbose = F)
    Both_bapc_result <- BAPC::qapc(Both_bapc_result,percentiles=c(0.025,0.975))
    
    if (full_age_adjusted==T) {
      if(length(setdiff(ages_3,ages)) != 0){
        for (i in 1:length(setdiff(ages_3,ages))) {
          Both_bapc_result@agespec.rate[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
          names(Both_bapc_result@agespec.rate[[i]]) <- c('mean','sd','0.025Q','0.975Q')
          Both_bapc_result@agespec.proj[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
          names(Both_bapc_result@agespec.proj[[i]]) <- c('mean','sd','0.025Q','0.975Q')
        }
      }
    }
    
    
    if(length(age_0) != 0){
      for (i in which(ages %in% age_0)) {
        Both_bapc_result@agespec.rate[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
        names(Both_bapc_result@agespec.rate[[i]]) <- c('mean','sd','0.025Q','0.975Q')
        Both_bapc_result@agespec.proj[[i]] <- matrix(0,nrow=predyear-(basicyear-1),ncol=4) %>% as.data.frame()
        names(Both_bapc_result@agespec.proj[[i]]) <- c('mean','sd','0.025Q','0.975Q')
      }
    }
    
    
    ##### age-specific number
    ## Both
    Both_proj_mean <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
    Both_proj_low <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
    Both_proj_up <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
    ### 建立循环语句，循环length(ages_3)次，依次将每个年龄组的数据的mean,95%CrI的上下限
    for (i in 1:length(ages_3)) {
      Both_proj_mean[,i] <- BAPC::agespec.proj(Both_bapc_result)[[i]][,1]
      Both_proj_low[,i] <- BAPC::agespec.proj(Both_bapc_result)[[i]][,3]
      Both_proj_up[,i] <- BAPC::agespec.proj(Both_bapc_result)[[i]][,4]
    }
    
    names(Both_proj_mean) <- ages_3
    rownames(Both_proj_mean) <- basicyear:predyear
    names(Both_proj_up) <- ages_3
    rownames(Both_proj_up) <- basicyear:predyear
    names(Both_proj_low) <- ages_3
    rownames(Both_proj_low) <- basicyear:predyear
    
    ##### age-specific rate
    ## Both
    Both_rate_mean <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
    Both_rate_low <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
    Both_rate_up <- matrix(NA,nrow= predyear-(basicyear-1), ncol=length(ages_3)) %>% as.data.frame()
    ### 建立循环语句，循环length(ages_3)次，依次将每个年龄组的数据的mean,95%CrI的上下限
    for (i in 1:length(ages_3)) {
      Both_rate_mean[,i] <- BAPC::agespec.rate(Both_bapc_result)[[i]][,1]*10^5
      Both_rate_up[,i] <- BAPC::agespec.rate(Both_bapc_result)[[i]][,3]*10^5
      Both_rate_low[,i] <- BAPC::agespec.rate(Both_bapc_result)[[i]][,4]*10^5
    }
  } else {
    Both_proj_mean <- Male_proj_mean + Female_proj_mean
    Both_proj_up <- Male_proj_up + Female_proj_up
    Both_proj_low <- Male_proj_low + Female_proj_low
    
    Both_rate_mean <- Both_proj_mean/population_Both_n*10^5
    Both_rate_up <- Both_proj_up/population_Both_n*10^5
    Both_rate_low <- Both_proj_low/population_Both_n*10^5
  }
  
  
  names(Both_proj_mean) <- ages_3
  rownames(Both_proj_mean) <- basicyear:predyear
  names(Both_proj_up) <- ages_3
  rownames(Both_proj_up) <- basicyear:predyear
  names(Both_proj_low) <- ages_3
  rownames(Both_proj_low) <- basicyear:predyear
  names(Both_rate_mean) <- ages_3
  rownames(Both_rate_mean) <- basicyear:predyear
  names(Both_rate_up) <- ages_3
  rownames(Both_rate_up) <- basicyear:predyear
  names(Both_rate_low) <- ages_3
  rownames(Both_rate_low) <- basicyear:predyear
  
  ######  ASR
  # if (length(rei_name) == 0) {
  #   Male_ture_data <- subset(dat,age == 'Age-standardized' &
  #                              sex == 'Male' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name)[,7:10]
  #
  #   Female_ture_data <- subset(dat,age == 'Age-standardized' &
  #                                sex == 'Female' &
  #                                metric == 'Rate' &
  #                                measure == measure_name &
  #                                location  == location_name &
  #                                cause  == cause_name)[,7:10]
  #
  #
  #   Both_ture_data <- subset(dat,age == 'Age-standardized' &
  #                              sex == 'Both' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name)[,7:10]
  # } else {
  #   Male_ture_data <- subset(dat,age == 'Age-standardized' &
  #                              sex == 'Male' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name &
  #                              rei == rei_name)[,8:11]
  #
  #   Female_ture_data <- subset(dat,age == 'Age-standardized' &
  #                                sex == 'Female' &
  #                                metric == 'Rate' &
  #                                measure == measure_name &
  #                                location  == location_name &
  #                                cause  == cause_name &
  #                                rei == rei_name)[,8:11]
  #
  #   Both_ture_data <- subset(dat,age == 'Age-standardized' &
  #                              sex == 'Both' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name &
  #                              rei == rei_name)[,8:11]
  # }
  
  
  
  Male_ASR <- BAPC::agestd.rate(x = Male_bapc_result) %>% as.data.frame()*10^5
  Male_ASR <- Male_ASR[,-2]
  Female_ASR <- BAPC::agestd.rate(x = Female_bapc_result) %>% as.data.frame()*10^5
  Female_ASR <- Female_ASR[,-2]
  
  Male_ASP <- BAPC::agestd.proj(x = Male_bapc_result) %>% as.data.frame()
  Male_ASP <- Male_ASP[,-2]
  Female_ASP <- BAPC::agestd.proj(x = Female_bapc_result) %>% as.data.frame()
  Female_ASP <- Female_ASP[,-2]
  
  if (length(rate_lessen)>0) {
    Male_ASR <- Male_ASR/rate_lessen
    Female_ASR <- Female_ASR/rate_lessen
    
    Male_ASP <- Male_ASP/rate_lessen
    Female_ASP <- Female_ASP/rate_lessen
  }
  
  Male_ASR$year <- basicyear:predyear
  Female_ASR$year <- basicyear:predyear
  # Male_ture_data$year <- as.numeric(Male_ture_data$year)
  # Female_ture_data$year <- as.numeric(Female_ture_data$year)
  
  names(Male_ASR)[1:3] <- c('pred_val','pred_low','pred_up')
  names(Female_ASR)[1:3] <- c('pred_val','pred_low','pred_up')
  
  
  Male_ASP$year <- basicyear:predyear
  Female_ASP$year <- basicyear:predyear
  
  names(Male_ASP)[1:3] <- c('pred_val','pred_low','pred_up')
  names(Female_ASP)[1:3] <- c('pred_val','pred_low','pred_up')
  
  if (By_sex==F) {
    Both_ASR <- BAPC::agestd.rate(x = Both_bapc_result) %>% as.data.frame()*10^5
    Both_ASR <- Both_ASR[,-2]
    Both_ASP <- BAPC::agestd.proj(x = Both_bapc_result) %>% as.data.frame()
    Both_ASP <- Both_ASP[,-2]
    
    if (length(rate_lessen)>0) {
      Both_ASR <- Both_ASR/rate_lessen
      Both_ASP <- Both_ASP/rate_lessen
    }
    Both_ASR$year <-  basicyear:predyear
    names(Both_ASR)[1:3] <- c("pred_val","pred_low","pred_up")
    
    Both_ASP$year <-  basicyear:predyear
    names(Both_ASP)[1:3] <- c("pred_val","pred_low","pred_up")
  } else {
    Both_ASR <- matrix(nrow = 0, ncol = 3) %>% as.data.frame()
    names(Both_ASR) <- c("pred_val","pred_low","pred_up")
    
    
    for (i in 1:(predyear-(basicyear-1))) {
      Both_ASR[i,1] <-  epitools::ageadjust.direct(count = Both_proj_mean[i,], pop = population_Both_n[i,],
                                                   stdpop = wstand)[2]*10^5
      Both_ASR[i,3] <-  epitools::ageadjust.direct(count = Both_proj_up[i,], pop = population_Both_n[i,],
                                                   stdpop = wstand)[2]*10^5
      Both_ASR[i,2] <-  epitools::ageadjust.direct(count = Both_proj_low[i,], pop = population_Both_n[i,],
                                                   stdpop = wstand)[2]*10^5
      
    }
    
    if (length(rate_lessen)>0) {
      Both_ASR <- Both_ASR/rate_lessen
    }
    Both_ASR$year <- basicyear:predyear
    # Both_ture_data$year <- as.numeric(Both_ture_data$year)
  }
  
  Both_ASR$sex <- 'Both'
  Female_ASR$sex <- 'Female'
  Male_ASR$sex <- 'Male'
  
  Female_ASP$sex <- 'Female'
  Male_ASP$sex <- 'Male'
  
  ASR <- rbind(Male_ASR,Female_ASR,Both_ASR)
  
  if (By_sex==F) {
    Both_ASP$sex <- 'Both'
    ASP <- rbind(Male_ASP,Female_ASP,Both_ASP)
  } else {
    ASP <- rbind(Male_ASP,Female_ASP)
  }
  
  # ######### all age rate and projection
  # if (length(rei_name) == 0) {
  #   Male_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Male' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name)[,7:10]
  #
  #   Female_ture_data <- subset(dat,age == 'All ages' &
  #                                sex == 'Female' &
  #                                metric == 'Rate' &
  #                                measure == measure_name &
  #                                location  == location_name &
  #                                cause  == cause_name)[,7:10]
  #
  #
  #   Both_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Both' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name)[,7:10]
  # } else {
  #   Male_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Male' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name &
  #                              rei == rei_name)[,8:11]
  #
  #   Female_ture_data <- subset(dat,age == 'All ages' &
  #                                sex == 'Female' &
  #                                metric == 'Rate' &
  #                                measure == measure_name &
  #                                location  == location_name &
  #                                cause  == cause_name &
  #                                rei == rei_name)[,8:11]
  #
  #   Both_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Both' &
  #                              metric == 'Rate' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name &
  #                              rei == rei_name)[,8:11]
  # }
  
  
  ## Male
  Male_allprojection <- data.frame(pred_val=rowSums(Male_proj_mean),
                                   pred_low=rowSums(Male_proj_low),
                                   pred_up=rowSums(Male_proj_up),
                                   year = basicyear: predyear)
  
  Male_rate <- matrix(nrow = 0, ncol = 3) %>% as.data.frame()
  names(Male_rate) <- c("pred_val","pred_low","pred_up")
  
  for (i in 1:(predyear-(basicyear-1))) {
    Male_rate[i,1] <-  epitools::ageadjust.direct(count = Male_proj_mean[i,], pop = population_Male_n[i,],
                                                  stdpop = wstand)[1]*10^5
    Male_rate[i,3] <-  epitools::ageadjust.direct(count = Male_proj_up[i,], pop = population_Male_n[i,],
                                                  stdpop = wstand)[1]*10^5
    Male_rate[i,2] <-  epitools::ageadjust.direct(count = Male_proj_low[i,], pop = population_Male_n[i,],
                                                  stdpop = wstand)[1]*10^5
    
  }
  
  
  ## Female
  Female_allprojection <- data.frame(pred_val=rowSums(Female_proj_mean),
                                     pred_low=rowSums(Female_proj_low),
                                     pred_up=rowSums(Female_proj_up),
                                     year = basicyear: predyear)
  
  Female_rate <- matrix(nrow = 0, ncol = 3) %>% as.data.frame()
  names(Female_rate) <- c("pred_val","pred_low","pred_up")
  
  for (i in 1:(predyear-(basicyear-1))) {
    Female_rate[i,1] <-  epitools::ageadjust.direct(count = Female_proj_mean[i,], pop = population_Female_n[i,],
                                                    stdpop = wstand)[1]*10^5
    Female_rate[i,3] <-  epitools::ageadjust.direct(count = Female_proj_up[i,], pop = population_Female_n[i,],
                                                    stdpop = wstand)[1]*10^5
    Female_rate[i,2] <-  epitools::ageadjust.direct(count = Female_proj_low[i,], pop = population_Female_n[i,],
                                                    stdpop = wstand)[1]*10^5
    
  }
  
  if (length(rate_lessen)>0) {
    Male_rate <- Male_rate/rate_lessen
    Female_rate <- Female_rate/rate_lessen
    
    Male_allprojection <- Male_allprojection/rate_lessen
    Female_allprojection <- Female_allprojection/rate_lessen
  }
  
  Male_rate$year <-  basicyear: predyear
  Male_allprojection$sex <- 'Male'
  Male_rate$sex <- 'Male'
  
  Female_rate$year <-  basicyear: predyear
  Female_allprojection$sex <- 'Female'
  Female_rate$sex <- 'Female'
  
  # Male_crude <- BAPC_crude(Male_bapc_result)
  # Male_rate <- Male_crude[["rate"]]*10^5
  # Male_rate$pred_low <- Male_rate$mean - 1.96*Male_rate$sd
  # Male_rate$pred_up <- Male_rate$mean + 1.96*Male_rate$sd
  # Male_rate <- Male_rate[,-2]
  #
  # Male_allprojection <- Male_crude[["proj"]]
  # Male_allprojection$pred_low <- Male_allprojection$mean - 1.96*Male_allprojection$sd
  # Male_allprojection$pred_up <- Male_allprojection$mean + 1.96*Male_allprojection$sd
  # Male_allprojection <- Male_allprojection[,-2]
  #
  # ## Female
  # Female_crude <- BAPC_crude(Female_bapc_result)
  # Female_rate <- Female_crude[["rate"]]*10^5
  # Female_rate$pred_low <- Female_rate$mean - 1.96*Female_rate$sd
  # Female_rate$pred_up <- Female_rate$mean + 1.96*Female_rate$sd
  # Female_rate <- Female_rate[,-2]
  #
  # Female_allprojection <- Female_crude[["proj"]]
  # Female_allprojection$pred_low <- Female_allprojection$mean - 1.96*Female_allprojection$sd
  # Female_allprojection$pred_up <- Female_allprojection$mean + 1.96*Female_allprojection$sd
  # Female_allprojection <- Female_allprojection[,-2]
  
  
  
  
  
  
  # Male_rate <- dplyr::left_join(Male_rate,Male_ture_data,by="year")
  # Female_rate <- dplyr::left_join(Female_rate,Female_ture_data,by="year")
  
  # names(Male_rate)[1:3] <- c('pred_val','pred_low','pred_up')
  # names(Female_rate)[1:3] <- c('pred_val','pred_low','pred_up')
  
  
  
  
  # if (length(rei_name) == 0) {
  #   Male_num_data <- subset(dat,age == 'All ages' &
  #                             sex == 'Male' &
  #                             metric == 'Number' &
  #                             measure == measure_name &
  #                             location  == location_name &
  #                             cause  == cause_name)[,7:10]
  #
  #   Female_num_data <- subset(dat,age == 'All ages' &
  #                               sex == 'Female' &
  #                               metric == 'Number' &
  #                               measure == measure_name &
  #                               location  == location_name &
  #                               cause  == cause_name)[,7:10]
  #
  #
  #   Both_num_data <- subset(dat,age == 'All ages' &
  #                             sex == 'Both' &
  #                             metric == 'Number' &
  #                             measure == measure_name &
  #                             location  == location_name &
  #                             cause  == cause_name)[,7:10]
  # } else {
  #   Male_num_data <- subset(dat,age == 'All ages' &
  #                             sex == 'Male' &
  #                             metric == 'Number' &
  #                             measure == measure_name &
  #                             location  == location_name &
  #                             cause  == cause_name &
  #                             rei == rei_name)[,8:11]
  #
  #   Female_num_data <- subset(dat,age == 'All ages' &
  #                               sex == 'Female' &
  #                               metric == 'Number' &
  #                               measure == measure_name &
  #                               location  == location_name &
  #                               cause  == cause_name &
  #                               rei == rei_name)[,8:11]
  #
  #   Both_num_data <- subset(dat,age == 'All ages' &
  #                             sex == 'Both' &
  #                             metric == 'Number' &
  #                             measure == measure_name &
  #                             location  == location_name &
  #                             cause  == cause_name &
  #                             rei == rei_name)[,8:11]
  # }
  
  
  
  # Male_allprojection <- dplyr::left_join(Male_allprojection,Male_num_data,by="year")
  # Female_allprojection <- dplyr::left_join(Female_allprojection,Female_num_data,by="year")
  
  # names(Male_allprojection)[1:3] <- c('pred_val','pred_low','pred_up')
  # names(Female_allprojection)[1:3] <- c('pred_val','pred_low','pred_up')
  
  if (By_sex==F) {
    ## Both
    Both_allprojection <- data.frame(pred_val=rowSums(Both_proj_mean),
                                     pred_low=rowSums(Both_proj_low),
                                     pred_up=rowSums(Both_proj_up),
                                     year = basicyear: predyear)
    
    Both_rate <- matrix(nrow = 0, ncol = 3) %>% as.data.frame()
    names(Both_rate) <- c("pred_val","pred_low","pred_up")
    
    for (i in 1:(predyear-(basicyear-1))) {
      Both_rate[i,1] <-  epitools::ageadjust.direct(count = Both_proj_mean[i,], pop = population_Both_n[i,],
                                                    stdpop = wstand)[1]*10^5
      Both_rate[i,3] <-  epitools::ageadjust.direct(count = Both_proj_up[i,], pop = population_Both_n[i,],
                                                    stdpop = wstand)[1]*10^5
      Both_rate[i,2] <-  epitools::ageadjust.direct(count = Both_proj_low[i,], pop = population_Both_n[i,],
                                                    stdpop = wstand)[1]*10^5
      
    }
    
    
    if (length(rate_lessen)>0) {
      Both_rate <- Both_rate/rate_lessen
      Both_allprojection <- Both_allprojection/rate_lessen
    }
    
    
    Both_rate$year <-  basicyear: predyear
    Both_allprojection$sex <- 'Both'
    Both_rate$sex <- 'Both'
    
    # Both_crude <- BAPC_crude(Both_bapc_result)
    # Both_rate <- Both_crude[["rate"]]*10^5
    # Both_rate$pred_low <- Both_rate$mean - 1.96*Both_rate$sd
    # Both_rate$pred_up <- Both_rate$mean + 1.96*Both_rate$sd
    # Both_rate <- Both_rate[,-2]
    #
    # Both_allprojection <- Both_crude[["proj"]]
    # Both_allprojection$pred_low <- Both_allprojection$mean - 1.96*Both_allprojection$sd
    # Both_allprojection$pred_up <- Both_allprojection$mean + 1.96*Both_allprojection$sd
    # Both_allprojection <- Both_allprojection[,-2]
    
    
    
    
    
    #Both_rate <- dplyr::left_join(Both_rate,Both_ture_data,by="year")
    names(Both_rate)[1:3] <- c("pred_val","pred_low","pred_up")
    
    #Both_allprojection <- dplyr::left_join(Both_allprojection,Both_num_data,by="year")
    names(Both_allprojection)[1:3] <- c("pred_val","pred_low","pred_up")
  } else {
    Both_rate <- matrix(nrow = 0, ncol = 3) %>% as.data.frame()
    names(Both_rate) <- c("pred_val","pred_low","pred_up")
    
    for (i in 1:(predyear-(basicyear-1))) {
      Both_rate[i,1] <-  epitools::ageadjust.direct(count = Both_proj_mean[i,], pop = population_Both_n[i,],
                                                    stdpop = wstand)[1]*10^5
      Both_rate[i,3] <-  epitools::ageadjust.direct(count = Both_proj_up[i,], pop = population_Both_n[i,],
                                                    stdpop = wstand)[1]*10^5
      Both_rate[i,2] <-  epitools::ageadjust.direct(count = Both_proj_low[i,], pop = population_Both_n[i,],
                                                    stdpop = wstand)[1]*10^5
      
    }
    Both_allprojection <- Male_allprojection[,1:3] + Female_allprojection[,1:3]
    
    if (length(rate_lessen)>0) {
      Both_rate <- Both_rate/rate_lessen
    }
    Both_allprojection$year <- basicyear:predyear
    Both_rate$year <- basicyear:predyear
    
    
    # Both_rate <- dplyr::left_join(Both_rate,Both_ture_data,by="year")
    # Both_allprojection <- dplyr::left_join(Both_allprojection,Both_num_data,by="year")
  }
  
  Both_rate$sex <- 'Both'
  Female_rate$sex <- 'Female'
  Male_rate$sex <- 'Male'
  
  Both_allprojection$sex <- 'Both'
  Female_allprojection$sex <- 'Female'
  Male_allprojection$sex <- 'Male'
  
  crude_rate <- rbind(Male_rate,Female_rate,Both_rate)
  
  allprojection <- rbind(Male_allprojection,Female_allprojection,Both_allprojection)
  
  
  ### 整理不同年龄段的rate 和 number数据
  ## age-specific rate
  ## mean
  
  if (length(rate_lessen)>0) {
    Male_rate_up <- Male_rate_up/rate_lessen
    Male_rate_low <- Male_rate_low/rate_lessen
    Male_rate_mean <- Male_rate_mean/rate_lessen
    
    Female_rate_up <- Female_rate_up/rate_lessen
    Female_rate_low <- Female_rate_low/rate_lessen
    Female_rate_mean <- Female_rate_mean/rate_lessen
    
    Both_rate_up <- Both_rate_up/rate_lessen
    Both_rate_low <- Both_rate_low/rate_lessen
    Both_rate_mean <- Both_rate_mean/rate_lessen
    
    Male_proj_up <- Male_proj_up/rate_lessen
    Male_proj_low <- Male_proj_low/rate_lessen
    Male_proj_mean <- Male_proj_mean/rate_lessen
    
    Female_proj_up <- Female_proj_up/rate_lessen
    Female_proj_low <- Female_proj_low/rate_lessen
    Female_proj_mean <- Female_proj_mean/rate_lessen
    
    Both_proj_up <- Both_proj_up/rate_lessen
    Both_proj_low <- Both_proj_low/rate_lessen
    Both_proj_mean <- Both_proj_mean/rate_lessen
  }
  
  if (full_age_adjusted==T) {
    Male_rate_up <- Male_rate_up %>% dplyr::select(ages)
    Male_rate_low <- Male_rate_low %>% dplyr::select(ages)
    Male_rate_mean <- Male_rate_mean %>% dplyr::select(ages)
    
    Female_rate_up <- Female_rate_up %>% dplyr::select(ages)
    Female_rate_low <- Female_rate_low %>% dplyr::select(ages)
    Female_rate_mean <- Female_rate_mean %>% dplyr::select(ages)
    
    Both_rate_up <- Both_rate_up %>% dplyr::select(ages)
    Both_rate_low <- Both_rate_low %>% dplyr::select(ages)
    Both_rate_mean <- Both_rate_mean %>% dplyr::select(ages)
    
    Male_proj_up <- Male_proj_up %>% dplyr::select(ages)
    Male_proj_low <- Male_proj_low %>% dplyr::select(ages)
    Male_proj_mean <- Male_proj_mean %>% dplyr::select(ages)
    
    Female_proj_up <- Female_proj_up %>% dplyr::select(ages)
    Female_proj_low <- Female_proj_low %>% dplyr::select(ages)
    Female_proj_mean <- Female_proj_mean %>% dplyr::select(ages)
    
    Both_proj_up <- Both_proj_up %>% dplyr::select(ages)
    Both_proj_low <- Both_proj_low %>% dplyr::select(ages)
    Both_proj_mean <- Both_proj_mean %>% dplyr::select(ages)
  }
  
  names(Male_rate_up) <- ages
  Male_rate_up$year <- basicyear:predyear
  names(Female_rate_up) <- ages
  Female_rate_up$year <- basicyear:predyear
  names(Both_rate_up) <- ages
  Both_rate_up$year <- basicyear:predyear
  
  names(Male_rate_mean) <- ages
  Male_rate_mean$year <- basicyear:predyear
  names(Female_rate_mean) <- ages
  Female_rate_mean$year <- basicyear:predyear
  names(Both_rate_mean) <- ages
  Both_rate_mean$year <- basicyear:predyear
  
  names(Male_rate_low) <- ages
  Male_rate_low$year <- basicyear:predyear
  names(Female_rate_low) <- ages
  Female_rate_low$year <- basicyear:predyear
  names(Both_rate_low) <- ages
  Both_rate_low$year <- basicyear:predyear
  
  names(Male_proj_up) <- ages
  Male_proj_up$year <- basicyear:predyear
  names(Female_proj_up) <- ages
  Female_proj_up$year <- basicyear:predyear
  names(Both_proj_up) <- ages
  Both_proj_up$year <- basicyear:predyear
  
  names(Male_proj_mean) <- ages
  Male_proj_mean$year <- basicyear:predyear
  names(Female_proj_mean) <- ages
  Female_proj_mean$year <- basicyear:predyear
  names(Both_proj_mean) <- ages
  Both_proj_mean$year <- basicyear:predyear
  
  
  names(Male_proj_low) <- ages
  Male_proj_low$year <- basicyear:predyear
  names(Female_proj_low) <- ages
  Female_proj_low$year <- basicyear:predyear
  names(Both_proj_low) <- ages
  Both_proj_low$year <- basicyear:predyear
  
  ## mean
  Male_rate_mean <- Male_rate_mean %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_val')
  
  Female_rate_mean <- Female_rate_mean %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_val')
  
  Both_rate_mean <- Both_rate_mean %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_val')
  
  ## up
  Male_rate_up <- Male_rate_up %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_up')
  
  Female_rate_up <- Female_rate_up %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_up')
  
  Both_rate_up <- Both_rate_up %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_up')
  ## low
  Male_rate_low <- Male_rate_low %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_low')
  
  Female_rate_low <- Female_rate_low %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_low')
  
  Both_rate_low <- Both_rate_low %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_low')
  
  ## age-specific proj
  ## mean
  Male_proj_mean <- Male_proj_mean %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_val')
  
  Female_proj_mean <- Female_proj_mean %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_val')
  
  Both_proj_mean <- Both_proj_mean %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_val')
  
  ## up
  Male_proj_up <- Male_proj_up %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_up')
  
  Female_proj_up <- Female_proj_up %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_up')
  
  Both_proj_up <- Both_proj_up %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_up')
  ## low
  Male_proj_low <- Male_proj_low %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_low')
  
  Female_proj_low <- Female_proj_low %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_low')
  
  Both_proj_low <- Both_proj_low %>%
    tidyr::pivot_longer(ages,
                        names_to = "age",
                        values_to = 'pred_low')
  
  
  ## add sex variable
  Male_rate_mean$sex <- 'Male'
  Male_proj_mean$sex <- 'Male'
  Female_rate_mean$sex <- 'Female'
  Female_proj_mean$sex <- 'Female'
  Both_rate_mean$sex <- 'Both'
  Both_proj_mean$sex <- 'Both'
  
  Male_rate_up$sex <- 'Male'
  Male_proj_up$sex <- 'Male'
  Female_rate_up$sex <- 'Female'
  Female_proj_up$sex <- 'Female'
  Both_rate_up$sex <- 'Both'
  Both_proj_up$sex <- 'Both'
  
  Male_rate_low$sex <- 'Male'
  Male_proj_low$sex <- 'Male'
  Female_rate_low$sex <- 'Female'
  Female_proj_low$sex <- 'Female'
  Both_rate_low$sex <- 'Both'
  Both_proj_low$sex <- 'Both'
  
  proj_mean <- rbind(Male_proj_mean,Female_proj_mean,Both_proj_mean)
  proj_up <- rbind(Male_proj_up,Female_proj_up,Both_proj_up)
  proj_low <- rbind(Male_proj_low,Female_proj_low,Both_proj_low)
  
  rate_mean <- rbind(Male_rate_mean,Female_rate_mean,Both_rate_mean)
  rate_up <- rbind(Male_rate_up,Female_rate_up,Both_rate_up)
  rate_low <- rbind(Male_rate_low,Female_rate_low,Both_rate_low)
  
  
  # combination of age-specific rate/proj for sex
  age_specific_rate <- merge(rate_mean,rate_low,by=c("age","sex","year")) %>%
    merge(rate_up,by=c("age","sex","year"))
  age_specific_proj <- merge(proj_mean,proj_low,by=c("age","sex","year")) %>%
    merge(proj_up,by=c("age","sex","year"))
  #
  # if (length(rei_name) == 0) {
  #   ture_data <- subset(dat,age %in% ages &
  #                         metric == 'Rate' &
  #                         measure == measure_name &
  #                         location  == location_name &
  #                         cause  == cause_name) %>% dplyr::select(age,sex,year,val,upper,lower)
  #
  #
  # } else {
  #   ture_data <- subset(dat,age %in% ages &
  #                         metric == 'Rate' &
  #                         measure == measure_name &
  #                         location  == location_name &
  #                         cause  == cause_name &
  #                         rei == rei_name) %>% dplyr::select(age,sex,year,val,upper,lower)
  #
  # }
  # ture_data$year <- as.numeric(ture_data$year)
  # age_specific_rate <- dplyr::left_join(age_specific_rate,ture_data,by=c('age','sex','year'))
  #
  #
  # if (length(rei_name) == 0) {
  #   ture_data <- subset(dat,age %in% ages &
  #                         metric == 'Number' &
  #                         measure == measure_name &
  #                         location  == location_name &
  #                         cause  == cause_name) %>% dplyr::select(age,sex,year,val,upper,lower)
  #
  #
  # } else {
  #   ture_data <- subset(dat,age %in% ages &
  #                         metric == 'Number' &
  #                         measure == measure_name &
  #                         location  == location_name &
  #                         cause  == cause_name &
  #                         rei == rei_name) %>% dplyr::select(age,sex,year,val,upper,lower)
  #
  # }
  # ture_data$year <- as.numeric(ture_data$year)
  # age_specific_proj <- dplyr::left_join(age_specific_proj,ture_data,by=c('age','sex','year'))
  
  #####总发病人数
  # if (length(rei_name) == 0) {
  # Male_ture_data <- subset(dat,age == 'All ages' &
  #                            sex == 'Male' &
  #                            metric == 'Number' &
  #                            measure == measure_name &
  #                            location  == location_name &
  #                            cause  == cause_name)[,7:10] %>%
  #                            mutate(sex='Male')
  #
  #
  # Female_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Female' &
  #                              metric == 'Number' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name)[,7:10] %>%
  #                             mutate(sex='Female')
  #
  # Both_ture_data <- subset(dat,age == 'All ages' &
  #                            sex == 'Both' &
  #                            metric == 'Number' &
  #                            measure == measure_name &
  #                            location  == location_name &
  #                            cause  == cause_name)[,7:10] %>%
  #                            mutate(sex='Both')
  # } else {
  #   Male_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Male' &
  #                              metric == 'Number' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name &
  #                              rei == rei_name)[,8:11] %>%
  #                              mutate(sex='Male')
  #
  #
  #   Female_ture_data <- subset(dat,age == 'All ages' &
  #                                sex == 'Female' &
  #                                metric == 'Number' &
  #                                measure == measure_name &
  #                                location  == location_name &
  #                                cause  == cause_name &
  #                                rei == rei_name)[,8:11] %>%
  #                                mutate(sex='Female')
  #
  #   Both_ture_data <- subset(dat,age == 'All ages' &
  #                              sex == 'Both' &
  #                              metric == 'Number' &
  #                              measure == measure_name &
  #                              location  == location_name &
  #                              cause  == cause_name &
  #                              rei == rei_name)[,8:11] %>%
  #                              mutate(sex='Both')
  # }
  #
  # ture_data <- rbind(Male_ture_data,Female_ture_data,Both_ture_data)
  # sum_year <- age_specific_proj %>%
  #               mutate(pred_val=as.numeric(pred_val),
  #                      pred_up=as.numeric(pred_up),
  #                      pred_low=as.numeric(pred_low)) %>%
  #               group_by(sex,year) %>%
  #               summarise(pred_val = sum(pred_val),
  #               pred_up = sum(pred_up),
  #               pred_low = sum(pred_low))
  # sum_year <- dplyr::left_join(sum_year,ture_data,by=c('sex','year'))
  
  
  ## all-age rate
  # Male_bapc_result <- BAPC::BAPC(Male_esoph, predict = list(npredict = predyear - max(population$year), retro = F),
  #                                secondDiff = FALSE, stdweight = NULL, verbose = F)
  #
  # Female_bapc_result <- BAPC::BAPC(Female_esoph, predict = list(npredict = predyear - max(population$year), retro = F),
  #                                  secondDiff = FALSE, stdweight = NULL, verbose = F)
  #
  #
  # Male_bapc_result <- BAPC::qapc(Male_bapc_result,percentiles=c(0.025,0.975))
  # Female_bapc_result <- BAPC::qapc(Female_bapc_result,percentiles=c(0.025,0.975))
  
  
  #Male_all_rate <- BAPC::agestd.rate(x = Male_bapc_result) %>% as.data.frame()*10^5
  #Male_all_rate <- Male_all_rate[,-2]
  #Female_all_rate <- BAPC::agestd.rate(x = Female_bapc_result) %>% as.data.frame()*10^5
  # Female_all_rate <- Female_all_rate[,-2]
  #Male_all_rate$year <- rownames(Male_all_rate) %>% as.numeric()
  #Female_all_rate$year <- rownames(Female_all_rate) %>% as.numeric()
  
  #Male_all_rate <- dplyr::left_join(Male_all_rate,Male_ture_data,by="year")
  #Female_all_rate <- dplyr::left_join(Female_all_rate,Female_ture_data,by="year")
  
  #names(Male_all_rate) <- c('pred_val','pred_up','pred_low','year')
  #names(Female_all_rate) <- c('pred_val','pred_up','pred_low','year')
  
  # year_index <- basicyear:predyear
  #
  #
  # if (By_sex==F) {
  #   Both_all_rate <- BAPC::agestd.rate(x = Male_bapc_result) %>% as.data.frame()*10^5 %>% select(-2)
  #   Both_all_rate$year <- rownames(Both_all_rate) %>% as.numeric()
  #   Both_all_rate <- dplyr::left_join(Both_all_rate,Male_ture_data,by="year")
  # } else {
  #   Both_all_rate <- matrix(nrow = 0, ncol = 4) %>% as.data.frame()
  #   names(Both_all_rate) <- c("pred_val","pred_up","pred_low")
  #   #i=1
  #   for (i in 1:(predyear-(basicyear-1))) {
  #     Both_all_rate[i,1] <-  epitools::ageadjust.direct(count = Both_proj_mean[i,], pop = population_Both_n[i,],
  #                                                  stdpop = wstand)[1]
  #     Both_all_rate[i,2] <-  epitools::ageadjust.direct(count = Both_proj_up[i,], pop = population_Both_n[i,],
  #                                                  stdpop = wstand)[1]
  #     Both_all_rate[i,3] <-  epitools::ageadjust.direct(count = Both_proj_low[i,], pop = population_Both_n[i,],
  #                                                  stdpop = wstand)[1]
  #
  #   }
  #   Both_all_rate$year <- basicyear:predyear
  #   Both_all_rate <- dplyr::left_join(Both_all_rate,Both_ture_data,by="year")
  # }
  #
  # Both_all_rate$sex <- 'Both'
  # Female_all_rate$sex <- 'Female'
  # Male_all_rate$sex <- 'Male'
  # all_rate <- rbind(Male_all_rate,Female_all_rate,Both_all_rate)
  ## summary
  
  crude_rate <- rbind(Male_rate,Female_rate,Both_rate)
  allprojection <- rbind(Male_allprojection,Female_allprojection,Both_allprojection)
  
  crude_rate$measure <- measure_name
  crude_rate$location <- location_name
  crude_rate$cause <- cause_name
  
  allprojection$measure <- measure_name
  allprojection$location <- location_name
  allprojection$cause <- cause_name
  
  ASR$measure <- measure_name
  ASR$location <- location_name
  ASR$cause <- cause_name
  
  
  ASP$measure <- measure_name
  ASP$location <- location_name
  ASP$cause <- cause_name
  
  # all_rate$measure <- measure_name
  # all_rate$location <- loct_name
  # all_rate$cause <- cause_name
  
  
  age_specific_rate$measure <- measure_name
  age_specific_rate$location <- location_name
  age_specific_rate$cause <- cause_name
  
  age_specific_proj$measure <- measure_name
  age_specific_proj$location <- location_name
  age_specific_proj$cause <- cause_name
  
  if (length(rei_name) != 0) {
    age_specific_rate$rei <- rei_name
    age_specific_proj$rei <- rei_name
    ASP$rei <- rei_name
    ASR$rei <- rei_name
    crude_rate$rei <- rei_name
    allprojection$rei <- rei_name
  }
  
  ## output
  row.names(allprojection) <- 1:nrow(allprojection)
  row.names(crude_rate) <- 1:nrow(crude_rate)
  row.names(ASR) <- 1:nrow(ASR)
  row.names(ASP) <- 1:nrow(ASP)
  row.names(age_specific_rate) <- 1:nrow(age_specific_rate)
  row.names(age_specific_proj) <- 1:nrow(age_specific_proj)
  
  result <- list(all_age_projection = allprojection,
                 crude_rate = crude_rate,
                 ASR = ASR,
                 Age_standardized_projection = ASP,
                 #all_rate_rate = rate_rate,
                 age_specific_rate = age_specific_rate,
                 age_specific_projection = age_specific_proj,
                 label='bapc')
  invisible()
  return(result)
}
