#'@title Visualizing academic year proprtions
#'@description This function plots a piechart that is consisted of sectors,
#' each sector represents the ratio of submissions of each academic year to the whole.
#'@param path The spreadsheet file path.
#'@return Returns a ggplot2 plot
#'@importFrom ggplot2 ggplot aes labs coord_polar ggtitle geom_bar
#'@importFrom ggthemes theme_excel
#'@export

year_ratio <- function(path){
  data <- read_data(path)
plot <-  ggplot2::ggplot(data,ggplot2::aes(x=nrow(data),fill=Year))+
                    ggplot2::geom_bar(stat='count')+
    ggthemes::theme_excel()+
    ggplot2::coord_polar("y")+
    ggplot2::labs(x='',y='')+
    ggplot2::ggtitle("Academic year submission protortion",
                     subtitle = "Number of submissions per academic year")

    return(plot)
}
#'@title Visualizing department proprtions
#'@description This function plots a stacked bar chart that is consisted of stacked bars,
#'each bar represents the number of submissions of each department to the whole each stack in a bar
#'represents the propotion of a year in a department
#'@param path The spreadsheet file path.
#'@return Returns a ggplot2 plot
#'@importFrom ggplot2 ggplot aes coord_flip ggtitle geom_bar
#'@importFrom ggthemes theme_fivethirtyeight
#'@export


department_ratio <- function(path){
  data <- read_data(path)
  data <- data[!grepl(',',data$Department),]
  data <- na.omit(data)
  data <- within(data,
                     Department <- factor(Department,
                                        levels=names(sort(table(Department),
                                                          decreasing=F))))
plot <-  ggplot2::ggplot(data,ggplot2::aes(x=Department,fill=Year))+
    ggplot2::geom_bar()+
    ggthemes::theme_fivethirtyeight()+
    ggplot2::coord_flip()+
    ggplot2::ggtitle("Department submission protortion",
                     subtitle = "Number of submissions per department")

  return(plot)
}


#'@title Visualizing malicious/non-malicious submissions ratio
#'@description This function plots a piechart that plots the stated propotion
#'@param path The spreadsheet file path.
#'@return Returns a ggplot2 plot
#'@importFrom ggplot2 ggplot aes labs coord_polar ggtitle scale_fill_manual theme geom_text geom_col
#'@importFrom ggthemes theme_excel
#'@importFrom dplyr mutate
#'@export


malicious_ratio <- function(path){
  data <- read_data(path)
  data <- dplyr::mutate(data, malicious_col ='Not' )

  for(i in 1:nrow(data)){

    if(!grepl('cis',data[['Academic_Email']][i])|grepl('dummy',data[['Academic_Email']][i]))
      data[['malicious_col']][i] <- 'Academic_Email'
    if(stringr::str_length(data[['Name']][i]) < 3 )
      data[['malicious_col']][i] <- 'Name'
    if(grepl('e\\.g*',data[['Name']])[i])
      data[['malicious_col']][i] <- 'Name'
    if(is.null(data[['Department']][i])|grepl(',',data[['Department']][i]))
      data[['malicious_col']][i] <- 'Department'

  }
  malicious <- nrow(data[data$malicious_col != 'Not',])
  non_malicious <- nrow(data)-malicious
  df <- data.frame(Key = c("Malicious","Non-Malicious"),val = c(malicious,non_malicious))

plot <-  ggplot2::ggplot(df,ggplot2::aes(x=1,y=val,fill=Key))+
    ggplot2::geom_col()+
    ggthemes::theme_excel()+
    ggplot2::coord_polar("y")+
    ggplot2::labs(x='',y='')+
  ggplot2::geom_text(ggplot2::aes(y = val/2 + c(0, cumsum(val)[-length(val)]),
                label = round(val/sum(val) *100,3)), size=4,col='black',angle=45)+
    ggplot2::ggtitle('Malicious to Non-Malicious submission ratio')+
    ggplot2::theme(legend.title = ggplot2::element_blank())+
    ggplot2:: scale_fill_manual(values = c("red", "steelblue"))

    return(plot)
}


#'@title Time Series
#'@description Plots a time series ( day ) of the number of submissions
#'@param path The spreadsheet file path.
#'@return Returns a ggplot2 plot
#'@importFrom tidyr separate
#'@importFrom lubridate ymd
#'@importfrom dplyr n group_by summarize
#'@importFrom ggplot2 ylab ggplot aes ggtitle theme geom_line
#'@importFrom ggthemes theme_fivethirtyeight
#'@export
day_series <- function(path){
  data <- read_data(path)
  data <- tidyr::separate(data,col='Timestamp',into=c('Day','Duration'),sep=" ")
  data$Day <- lubridate::ymd(data$Day)
  df <- dplyr::summarize(dplyr::group_by(data,Day),n_subs = dplyr::n())
  plot <- ggplot2::ggplot(df,ggplot2::aes(x=Day,y=n_subs,col='red'))+ggplot2::geom_line()+
    ggplot2::ylab('Number of submissions')+
    ggplot2::ggtitle('Daily Submissions',subtitle = 'Number of submissions per day.')+
    ggthemes::theme_fivethirtyeight()+
    ggplot2::theme(legend.position = 'none')
  return(plot)
}

