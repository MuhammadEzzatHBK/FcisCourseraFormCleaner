#'@title Splitting Malicious Submissions
#'@description A helper function that detects common malicious submission casses & marks them as so.
#'@param data The RAW data you want to split
#'@return Returns a list of data frames where each type of malice is included in a data frame of its own. While non
#'malicious data is included in the Malicious_Not data frame.
#'@importFrom dplyr mutate
#'@example
#'\dontrun{
#' data_list <- split_malicious(raw_data)
#' }
split_malicious <- function(data){

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

  data_vector <- split(data,data[['malicious_col']])
  names(data_vector) <- paste('Malicious_',names(data_vector),sep='')
    return(data_vector)
      }










#'@title Splitting Mainstream group from first year to third year.
#'@description A helper function that splits the mainstream students from the rest.
#'@param data_vector A list of data frames, must be the output of the split_malicious() function.
#'@return A list of data frames.
#'@importFrom dplyr filter
#'@importFrom rlist list.remove
#'@examples \dontrun{
#' library(dplyr)
#' data_list <- split_malicious(raw_data) %>% general_department_split()
#'}
general_department_split <- function(data_Vector){
  Departments <- dplyr::filter(data_Vector$Malicious_Not,data_Vector$Malicious_Not$Department != 'General_1st_to_3rd_Year'	)
  Mainstream <- dplyr::filter(data_Vector$Malicious_Not,data_Vector$Malicious_Not$Department == 'General_1st_to_3rd_Year')
  data_Vector <- rlist::list.remove(data_Vector,4)
  data_Vector <- append(list(Departments,Mainstream),data_Vector)
  names(data_Vector)[1:2] <- c('Departments','Mainstream')
  return(data_Vector)
}















#'@title Distributing the Mainstream
#'@description A helper function that splits the mainstream frame into many 100-row frame for each academic year.
#'@param data_vector The list produced by general_department_split()
#'@return A List of data frames
#'@importFrom rlist list.remove
#'@examples \dontrun{
#'library(dplyr)
#'data_list <- raw_data %>%
#'                  split_malicious() %>%
#'                  general_department_split() %>%
#'                  general_distribute()
#'}
#'
general_distribute <- function(data_vector){
  sublist <- split(data_vector$Mainstream,data_vector$Mainstream$Year)
  names(sublist) <- paste("Mainstream",names(sublist),sep="_")
  data_vector <- append(data_vector,sublist)
  data_vector <- rlist::list.remove(data_vector,2)
  indices <- grep("Mainstream_",names(data_vector))
  x <- list()
  toremove <- vector()
  for(i in indices){
    if(nrow(data_vector[[i]]) >100){
      toremove <- append(toremove,i)
      n <- 100
      nr <- nrow(data_vector[[i]])
      name <- names(data_vector)[i]
      sublist <-split(data_vector[[i]], rep(1:ceiling(nr/n), each=n, length.out=nr))
      names(sublist) <- paste(name,names(sublist),sep='_')
      x <- append(x,sublist)
    }
  }
  data_vector <- rlist::list.remove(data_vector,toremove)
  data_vector <- append(data_vector,x)
  return(data_vector)
}










#'@title Distributing Departments
#'@description A helper function that splits Departments frame into many 100-row frame for each academic year for each department
#'@param data_vector The list produced by general_distribute()
#'@return A List of data frames
#'@importFrom rlist list.remove
#'@importFrom dplyr mutate
#'@examples \dontrun{
#'library(dplyr)
#'data_list <- raw_data %>%
#'                  split_malicious() %>%
#'                  general_department_split() %>%
#'                  general_distribute() %>%
#'                  depatment_distribute()
#'}
#'
department_distribute <- function(data_vector){
   data_vector$Departments <- dplyr::mutate(data_vector$Departments,
                                             occupation = paste(Department,Year,sep = "_"))
  sublist <- split(data_vector$Departments,data_vector$Departments$occupation)
  len <- length(sublist)
  toremove <- vector()
  i<-1
  while(i <= 29){
    if( nrow(sublist[[i]]) > 25 ){
      toremove <- append(toremove,i)
      n <- 25
      nr <- nrow(sublist[[i]])
      name <- sublist[[i]]$occupation[1]
      subsublist <-split(sublist[[i]], rep(1:ceiling(nr/n), each=n, length.out=nr))
      names(subsublist) <- paste(name,names(subsublist),sep="_")
      sublist <- append(sublist,subsublist)
    }
    i <- i+1

  }
  sublist <- rlist::list.remove(sublist,toremove)
  data_vector <- rlist::list.remove(data_vector,1)
  data_vector <- append(data_vector,sublist)
  return(data_vector)
}















#'@title Saving Data
#'@description Saving data into spreadsheets.
#'@param data_vector The list resulted from department_distribute().
#'@importFrom xlsx write.xlsx
write_data <- function(data_vector){
  for(i in 1:length(data_vector)){
    xlsx::write.xlsx(data_vector[[i]],paste(names(data_vector)[i],'.xlsx',sep=''))
  }
}

#'@title Data Cleaning Pipeline
#'@description This is the function you are going to use.
#'@param path the Excel file path
#'@example \dontrun{
#'pipeline('sheet.xlsx')
#'}
#'@return The list of all data frames.
#'@importFrom dplyr %>%
#'@export
pipeline <- function(path){
  raw_data <- read_data(path)
     data_vector <-   raw_data %>%
                      split_malicious() %>%
                      general_department_split() %>%
                      general_distribute() %>%
                      department_distribute()
    write_data(data_vector)
    return(data_vector)
}

#'@title Reading data
#'@description A simple helper function that reads excel files.
#'@param path The spreadsheet file path
#'@return A data frame
#'@importFrom readxl read_xlsx
read_data <- function(path){
  raw_data <- readxl::read_xlsx(path)
  names(raw_data) <- c('Timestamp','Email','Academic_Email','Name','Gender','Department','Year')
  return(raw_data)
}
