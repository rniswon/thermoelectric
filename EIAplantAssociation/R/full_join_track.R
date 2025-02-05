#'@title full_join_track
#'@description merge two data frames while keeping track of mismatches using custom 
#'application of`dplyr::full_join` \cr \cr
#'@param x left data.frame object, must contain all columns found in `by`
#'@param y right data.frame object, must contain all columns found in `by`
#'@param by A character vector of variables to join by. If NULL, the default, `*_join()` will
#'perform a natural join, using all variables in common across x and y. A message lists 
#'the variables so that you can check they're correct; suppress the message by supplying by 
#'explicitly.
#'@param suffix If there are non-joined duplicate variables in x and y, these suffixes 
#'will be added to the output to disambiguate them. Should be a character vector of length 2.
#'@param .merge TRUE/FALSE indicating whether an additional variable ".merge" should be added
#'to the output data.frame indicating whether the data is from "left_only", "right_only" or 
#'"matched"
#'@param printMessages TRUE/FALSE indicating whether number or rows from left data.frame,
#'number of rows from right data.frame, and number of rows matched should be printed to the 
#'console
#'@param ... Other parameters passed onto methods.
#'@return An object of the same type as `x`. The order of the rows and columns of x is preserved 
#'as much as possible. All `x` rows, followed by unmatched `y` rows. 
#'Rows will be duplicated if one or more rows in `x` matches multiple rows in `y`. ".merge" column
#'included if `.merge`=TRUE. For more details on join structure see `dplyr::full_join`
#'@examples
#'x<-data.frame(Plant.Code=c(3,3,3,3),ID=c("01","1","02","CT02"))
#'y<-data.frame(Plant.Code=c(3,3,3,56),ID=c("01","1","2","CT02"))
#'full_join_track(x,y,by=c("Plant.Code","ID"),.merge=TRUE,printMessages=TRUE)
#'@export

full_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                            .merge = FALSE, printMessages=FALSE, ...){
  
  # Checking to make sure used variable names are not already in use
  if(".x_tracker" %in% names(x)){
    message("Warning: variable .x_tracker in left data was dropped")
  }
  if(".y_tracker" %in% names(y)){
    message("Warning: variable .y_tracker in right data was dropped")
  }
  if(.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
    stop("Variable .merge already exists; change name before proceeding")
  }
  
  # Adding simple merge tracker variables to data frames
  if(nrow(x)!=0){
  x[, ".x_tracker"]  <-  1
  }else{
    x$.x_tracker<-numeric(0)
  }
  if(nrow(y)!=0){
  y[, ".y_tracker"]  <-  1
  }else{
    y$.y_tracker<-numeric(0)
  }
  
  # Doing full join
  joined  <-  dplyr::full_join(x, y, by = by, suffix = suffix,  ...)
  
  # Calculating merge diagnoses 
  matched  <-  joined %>%
    dplyr::filter(!is.na(.x_tracker) & !is.na(.y_tracker)) %>%
    NROW()
  unmatched_x  <-  joined %>%
    dplyr::filter(!is.na(.x_tracker) & is.na(.y_tracker)) %>%
    NROW()
  unmatched_y  <-  joined %>%
    dplyr::filter(is.na(.x_tracker) & !is.na(.y_tracker)) %>%
    NROW()
  
  # Print merge diagnoses
  if(printMessages){
  message(
    unmatched_x, " Rows ONLY from left data frame", "\n",
    unmatched_y, " Rows ONLY from right data frame", "\n",
    matched, " Rows matched"
  )
  }
  # Create .merge variable if specified
  if(.merge){
    joined  <-  joined %>%
      dplyr::mutate(.merge = 
               dplyr::case_when(
                 !is.na(.$.x_tracker) & is.na(.$.y_tracker) ~ "left_only",
                 is.na(.$.x_tracker) & !is.na(.$.y_tracker) ~ "right_only",
                 TRUE ~ "matched"
               )
      )
  }
  
  # Dropping tracker variables and returning data frame
  joined  <-  joined %>%
    dplyr::select(-.x_tracker, -.y_tracker)
  return(joined)
}
