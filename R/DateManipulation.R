AddDate=function(date = Sys.Date(),addDays=0,addMonths=0,addYears=0)  {
  #' AddDate
  #' @author Julian Chitiva and Diego Jara
  #' @description
  #' Function to add a number of days, months and years to a specific date.
  #' The length of addDays, addMonths and addYears must be the same.
  #'
  #' @param date  Initial date.
  #' @param addDays  If specified, vector number of days to add to the initial date.
  #' @param addMonths  If specified, vector number of months to add to the initial date.
  #' @param addYears  If specified, vector number of years to add to the initial date.
  #' @return The output is the final date after adding the number of days, months and years to the initial date.
  #'
  #' @examples
  #' # Date input as Date object
  #' AddDate(date = Sys.Date(),addDays=14,addMonths=2,addYears=3)
  #'
  #' # Date input as character object
  #' AddDate(date = '2019-10-04',addDays=14,addMonths=2,addYears=3)
  #'
  #' @export

  ## Param validation
  if(!lubridate::is.Date(date)){
    try(date <- as.Date(date),
        stop(paste0(deparse(sys.call()),':',date,' is not valid as Date.'),call. = FALSE))
  }
  if(suppressWarnings(is.na(as.numeric(addDays)))){
    stop(paste0(deparse(sys.call()),':',addDays,' is not valid amount to add.'),call. = FALSE)
  }
  if(suppressWarnings(is.na(as.numeric(addMonths)))){
    stop(paste0(deparse(sys.call()),':',addMonths,' is not valid amount to add.'),call. = FALSE)
  }
  if(suppressWarnings(is.na(as.numeric(addYears)))){
    stop(paste0(deparse(sys.call()),':',addMonths,' is not valid amount to add.'),call. = FALSE)
  }

  ## Function
  date=lubridate::add_with_rollback(date, lubridate::days(addDays), roll_to_first = FALSE, preserve_hms = TRUE)
  date=lubridate::add_with_rollback(date, months(addMonths), roll_to_first = FALSE, preserve_hms = TRUE)
  date=lubridate::add_with_rollback(date, lubridate::years(addYears), roll_to_first = FALSE, preserve_hms = TRUE)
  return(date)
}

AddBusinessDays=function(date = Sys.Date(), numDate,
                         loc="BOG")  {
  #' AddBusinessDays
  #' @author Diego Jara
  #' @description
  #' Function to add a number of business days to a specific date. Currently the function work for
  #' returning values between 2000 and 2030.
  #'
  #' @param date  Initial date, the default is set to the date returned by Sys.Date().
  #' @param numDate  Number of dates to be add (positive or negative).
  #' @param loc  String that determines the location for business days. See details.
  #'
  #' @details
  #'  loc refers to the location for business days:
  #'  \itemize{
  #'      \item NY for New York.
  #'      \item LDN for London.
  #'      \item NYLDN for the intersection of business days in New York and London.
  #'      \item BOG for Bogota.
  #'      \item BOGNY for the intersection of business days in Bogota and New York.
  #' }
  #'
  #' @return The output is the final date after adding the number of business dates to the initial date.
  #' If the initial date is a non-working date, the result of the function for numDate equal to 0 or 1
  #' is the same.
  #'
  #' @examples
  #' # Date input as Date object
  #' AddBusinessDays(date = Sys.Date(),numDate = 15,loc = 'BOG')
  #'
  #' # Date input as character object
  #' AddBusinessDays(date = as.character(Sys.Date()),numDate = 15,loc = 'BOG')
  #'
  #' @export

  ## Param validation
  if(!lubridate::is.Date(date)){
    try(date <- as.Date(date),
        stop(paste0(deparse(sys.call()),':',date,' is not valid as Date.'),call. = FALSE))
  }

  if(!loc %in% c('NY','BOG','LDN','NYLDN','BOGNY')) stop('Invalid loc parameter.')

  ## Function

  if(loc=="NY") {workDays=wdNY; NumLab=julian(workDays)}
  else if(loc=="LDN") {workDays=wdLDN; NumLab=julian(workDays)}
  else if(loc=="NYLDN") {workDays=wdLDN;temp1=julian(wdNY);temp2=julian(workDays);NumLab=intersect(temp1,temp2)}
  else if(loc=="BOG") {workDays=wdBOG; NumLab=julian(workDays)}
  else if(loc=="BOGNY") {workDays=wdBOG;temp1=julian(wdNY);temp2=julian(workDays);NumLab=intersect(temp1,temp2)}

  Periodo<-seq(from = date, to = date+numDate+10, "day")
  temp1=julian(Periodo)
  pos=min(which(NumLab>=(temp1[1])))
  if(numDate>0 && NumLab[pos]>(temp1[1])) {numDate=numDate-1}
  initialDate <- NumLab[pos+numDate]
  return(as.Date(initialDate:initialDate,origin="1970-01-01"))
}

LastDayOfMonth=function (year, month, date=NULL) {
  #' LastDayOfMonth
  #'
  #' @author Diego Jara
  #'
  #' @description
  #' Returns the last day of a month.
  #'
  #' @param year Year as a number.
  #' @param month Month as a number.
  #' @param date If provided, uses year and month from this date.
  #' It could be date or a string format date YYYY-MM-DD.
  #'
  #' @return Last day of the month in the current year.
  #'
  #' @examples
  #' # Return last day of the month in year
  #' LastDayOfMonth(year = 2020, month = 2)
  #'
  #' # Return last day of the month for the date
  #' LastDayOfMonth(date = '2020-02-03')
  #'
  #' @export

  if(!is.null(date)){
    if(!lubridate::is.Date(date)) try(date <- as.Date(date),
                                        stop(paste0(deparse(sys.call()),':',date,' is not valid as Date.'),call. = FALSE))
    year <- lubridate::year(date)
    month <- lubridate::month(date)
  }else{
    if(suppressWarnings(is.na(as.numeric(year))))stop(paste0(deparse(sys.call()),':',year,' is not valid as year.'),call. = FALSE)
    if(suppressWarnings(is.na(as.numeric(month))))stop(paste0(deparse(sys.call()),':',month,' is not valid as month.'),call. = FALSE)
  }
  x <- as.POSIXct(ISOdatetime(year,month,1,0,0,0), format="%Y-%m-%d")
  next.mon <- seq(x, length=2, by='1 month')[2]
  last.day <- seq(next.mon, length=2, by='-1 day')[2]
  return(as.Date(last.day))
}

BusinessDays = function(loc='BOG', from=NULL, to=NULL){
  #' BusinessDays
  #'
  #' @author Diego Jara and Julian Chitiva
  #'
  #' @description
  #' Calculate business days for a given location. Data availability
  #' depends on the location.
  #'
  #' @param loc  String that determines the location for business days. See details.
  #' @param from  If provided returns available business dates after this date (inclusive).
  #' @param to  If provided returns available business dates until this date (inclusive).
  #'
  #' @return Vector of business days. Data availability depends on the location.
  #'
  #' @details
  #'  loc refers to the location for business days:
  #'  \itemize{
  #'      \item NY for New York.
  #'      \item LDN for London.
  #'      \item NYLDN for the intersection of business days in New York and London.
  #'      \item BOG for Bogota.
  #'      \item BOGNY for the intersection of business days in Bogota and New York.
  #' }
  #'
  #'
  #' @examples
  #' # Returns all business days available for the location
  #' BusinessDays(loc='BOG')
  #'
  #' # Returns business days within given range for the location and Dates as
  #' # character
  #' BusinessDays(loc='BOG', from='2020-10-10', to='2020-11-10')
  #'
  #' # Returns business days within given range for the location and Dates as
  #' # Dates
  #' BusinessDays(loc='BOG', from=as.Date('2020-10-10'), to='2020-11-10')
  #'
  #' # Returns all available business days for the locatio after given
  #' # 'from' date as character
  #' BusinessDays(loc='BOG', from='2020-10-10')
  #'
  #' @export

  ## Param validation
  if(!is.null(from)){
    if(!lubridate::is.Date(from)){
      try(date <- as.Date(from),
          stop(paste0(deparse(sys.call()),':',from,' is not valid as Date.'),call. = FALSE))
    }
  }
  if(!is.null(to)){
    if(!lubridate::is.Date(to)){
      try(date <- as.Date(to),
          stop(paste0(deparse(sys.call()),':',to,' is not valid as Date.'),call. = FALSE))
    }
  }


  if(!loc %in% c('NY','BOG','LDN','NYLDN','BOGNY')) stop('Invalid loc parameter.')


  ## Function
  if(loc=='BOG'){holiDays=holiDaysBOG}
  else if(loc=='NY'){holiDays=holiDaysNY}
  else if(loc=='LDN'){holiDays=holiDaysLDN}
  else if(loc=='NYLDN'){holiDays=c(holiDaysLDN,holiDaysNY);holiDays=holiDays[order(holiDays)]}
  else if(loc=='BOGNY'){holiDays=c(holiDaysBOG,holiDaysNY);holiDays=holiDays[order(holiDays)]}

  dates = seq(holiDays[1],max(holiDays),by = "day")
  dates = setdiff(dates,holiDays)
  dates  = as.Date(dates,origin="1970-01-01")
  dates = dates[!weekdays(dates) %in% c('Saturday','Sunday')]
  if(!is.null(from)) dates <- dates[dates>=from]
  if(!is.null(to)) dates <- dates[dates<=to]

  return(dates)
}

NumExcel2DateR=function(date){
  #' NumExcel2DateR
  #'
  #' @author Diego Jara
  #'
  #' @description
  #' Takes a date represented by a number in Excel format (origin="1899-12-30") and returns a date in
  #' R format.
  #'
  #' @param date  numeric vector.
  #' @return date in R.
  #'
  #' @examples
  #' NumExcel2DateR(as.numeric(Sys.Date()))
  #'
  #' @family Number to Date
  #' @seealso For dates with R origin.
  #'
  #' @export

  ## Param validation
  if(!is.numeric(date) & is.na(as.numeric(date))) stop('Invalid format for param date. It must be numeric.')

  ## Function
  as.Date(as.numeric(date),origin="1899-12-30")
}

NumR2DateR=function(date){
  #' NumR2DateR
  #'
  #' @author Diego Jara
  #'
  #' @description
  #' Takes a date represented by a number in R format (origin="1970-01-01") and returns a date.
  #'
  #' @param date numeric vector.
  #' @return date in R.
  #'
  #' @examples
  #' NumR2DateR(as.numeric(Sys.Date()))
  #'
  #' @family Number to Date
  #' @seealso For dates with Excel origin.
  #'
  #' @export

  ## Param validation
  if(!is.numeric(date) & is.na(as.numeric(date))) stop('Invalid format for param date. It must be numeric.')

  ## Function
  as.Date(date,origin="1970-01-01")
}

