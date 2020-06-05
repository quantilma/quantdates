
difftime_leap_year=function(tfinal,tinitial,leapDatesIn=TRUE)  {
  #' difftime_leap_year
  #'
  #' @author Julian Chitiva and Diego Jara
  #' @description
  #' Function to count the number of days between two dates.
  #' Optional parameters to count without the leap-days.
  #'
  #' @param tinitial  Initial date.
  #' @param tfinal  Final date.
  #' @param leapDatesIn  If TRUE count leap Dates, else exclude from counting.
  #' @return Number of days between the specified dates.
  #'
  #' @examples
  #' #Function accepts Dates as Dates or as characters.
  #' difftime_leap_year(tfinal='2023-03-05',tinitial='2019-02-28',leapDatesIn=TRUE)
  #' difftime_leap_year(tfinal=as.Date('2023-03-05'),tinitial=as.Date('2019-02-28'),leapDatesIn=TRUE)
  #' difftime_leap_year(tfinal='2023-03-05',tinitial='2019-02-28',leapDatesIn=FALSE)
  #' difftime_leap_year(tfinal='2023-03-05',tinitial=as.Date('2019-02-28'),leapDatesIn=FALSE)
  #'
  #' @export

  if(!lubridate::is.Date(tfinal)) try(tfinal <- as.Date(tfinal),
                           stop(paste0(deparse(sys.call()),':',tfinal,' is not valid as Date.'),call. = FALSE))
  if(!lubridate::is.Date(tinitial)) try(tinitial <- as.Date(tinitial),
                             stop(paste0(deparse(sys.call()),':',tinitial,' is not valid as Date.'),call. = FALSE))


  leap_dates <- 0
  if(!leapDatesIn){
    leap_dates <- seq(tinitial, tfinal, by='day')
    leap_dates <- leap_dates[which(format(leap_dates,'%m-%d')=='02-29')]
    leap_dates <- length(leap_dates)
  }

  return(as.numeric(difftime(tfinal,tinitial))-leap_dates)
}

difftime_business = function(tfinal,tinitial,wd=wdBOG){
  #' difftime_business
  #'
  #' @author Diego Jara
  #'
  #' Function to count the number of business days between two dates.
  #'
  #' @param tinitial  Initial date, it must be a business day.
  #' @param tfinal  Final date, it must be a business day.
  #' @param wd  Vector of dates with business days. The default are the business
  #' days of Bogota.
  #' @return Number of days between the specified dates.
  #'
  #' @examples
  #' #Function accepts Dates as Dates or as characters.
  #' difftime_business(tfinal='2023-03-08',tinitial='2019-02-28',wd=wdBOG)
  #' difftime_business(tfinal=as.Date('2023-03-08'),tinitial=as.Date('2019-02-28'),wd=wdBOG)
  #' difftime_business(tfinal='2023-03-08',tinitial=as.Date('2019-02-28'),wd=wdLDN)
  #' difftime_business(tfinal='2023-03-08',tinitial='2019-02-28',wd=wdNY)
  #'
  #' @export

  if(!lubridate::is.Date(tfinal)) try(tfinal <- as.Date(tfinal),
                           stop(paste0(deparse(sys.call()),': ',tfinal,' is not valid as Date.'),call. = FALSE))
  if(!lubridate::is.Date(tinitial)) try(tinitial <- as.Date(tinitial),
                             stop(paste0(deparse(sys.call()),': ',tinitial,' is not valid as Date.'),call. = FALSE))
  if(is.null(wd)) stop(paste0(deparse(sys.call()),':',' wd is not provided'),call. = FALSE)


  return( which(wd==tfinal)-which(wd==tinitial) )
}

day_count = function(tfinal, tinitial, convention='ACT/365'){
  #' day_count
  #'
  #' @author Julian Chitiva
  #'
  #' @description
  #' Function to count the number of years between two dates according to
  #' the given convention.
  #'
  #' @param tinitial  Initial date.
  #' @param tfinal  Final date.
  #' @param convention  Character that specifies the convention. See details.
  #' @details
  #' The convention accepts the following values:
  #' \itemize{
  #'     \item 30/360.
  #'     \deqn{DayCount = \frac{360\times(Y_2-Y_1)+30\times (M_2-M_1) + (D_2-D_1)}{360}}{DayCount = (360*(Y_2-Y_1)+30*(M_2-M_1) + (D_2-D_1))/360}
  #'     Here the dates are in the following format
  #'     \itemize{
  #'     \item tfinal = \eqn{Y_2}-\eqn{M_2}-\eqn{D_2} (YYYY-MM-DD).
  #'     \item tinitial = \eqn{Y_1}-\eqn{M_1}-\eqn{D_1} (YYYY-MM-DD).
  #'     }
  #'     It is important to note that
  #'     \itemize{
  #'     \item \eqn{D_1=\min(D_1,30)}
  #'     \item If \eqn{D_1=30} then \eqn{D_2=\min(D_2,30)}
  #'     }
  #'
  #'     \item ACT/365 (Default).
  #'     \deqn{DayCount = \frac{Days(tintial, tfinal)}{365}}
  #'
  #'     Also known as ACT/365 Fixed.
  #'
  #'     \item ACT/360.
  #'     \deqn{DayCount = \frac{Days(tintial, tfinal)}{365}}
  #'
  #'     \item ACT/365L.
  #'     \deqn{DayCount = \frac{Days(tintial, tfinal)}{DiY}}
  #'
  #'     If February 29 is in the range from Date1 (exclusive) to Date2 (inclusive),
  #'    then DiY = 366, else DiY = 365.
  #'
  #'     \item NL/365.
  #'
  #'     If February 29 is not in the period then actual number of days between
  #'     dates is used. Else actual number of days minus 1 is used. Day count basis = 365.
  #'
  #'     \item ACT/ACT-ISDA.
  #'     \deqn{DayCount = \frac{Days\; not\; in\; leap\; year}{365} + \frac{Days\; in\; leap\; year}{366}}
  #'
  #'     \item ACT/ACT-AFB.
  #'     \deqn{DayCount = \frac{Days(tintial, tfinal)}{DiY}}
  #'
  #'     The basic rule is that if February 29 is in the range from Date1 (inclusive)
  #'      to Date2 (exclusive), then DiY = 366, else DiY = 365.
  #'
  #'      If the period from Date1 to Date2 is more than one year, the calculation
  #'      is split into two parts:
  #'      \itemize{
  #'      \item The number of complete years, counted back from the last day of the period.
  #'      \item The remaining initial stub, calculated using the basic rule.
  #'      }
  #'
  #'
  #' }
  #'
  #' @return Number of years between the specified dates according to the convention.
  #'
  #' @examples
  #' #Function accepts Dates as Dates or as characters.
  #' day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/365')
  #' day_count(tfinal=as.Date('2023-03-08'),tinitial=as.Date('2019-02-28'),convention='ACT/360')
  #' day_count(tfinal='2023-03-08',tinitial=as.Date('2019-02-28'),convention='30/360')
  #' day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='NL/365')
  #' day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/ACT-ISDA')
  #' day_count(tfinal='2023-03-08',tinitial='2019-02-28',convention='ACT/ACT-AFB')
  #'
  #' @source
  #' International Swaps and Derivatives Association - ISDA.
  #' @references
  #' International Swaps and Derivatives Association. (2006). 2006 ISDA definitions. New York, N.Y: International Swaps and Derivatives Association.
  #'
  #' @export


  if(!convention %in% c('ACT/365','ACT/360','ACT/365L','NL/365'
                       ,'ACT/ACT-ISDA','ACT/ACT-AFB',
                       '30/360')) stop('Invalid day count convention.')

  if(!lubridate::is.Date(tfinal)) try(tfinal <- as.Date(tfinal),
                           stop(paste0(deparse(sys.call()),':',tfinal,' is not valid as Date.'),call. = FALSE))
  if(!lubridate::is.Date(tinitial)) try(tinitial <- as.Date(tinitial),
                             stop(paste0(deparse(sys.call()),':',tinitial,' is not valid as Date.'),call. = FALSE))



  if(convention %in% c('ACT/365','ACT/360','NL/365')){
    leapDatesIn <- convention!='NL/365'
    return(.day_count_ACT_FIX(tfinal, tinitial, convention, leapDatesIn))
  }else if(convention %in% c('ACT/365L')){
    return(.day_count_ACT_L(tfinal, tinitial))
  }else if(convention %in% c('30/360')){
    return(.day_count_30360(tfinal, tinitial))
  }else if(convention %in% c('ACT/ACT-ISDA')){
    return(.day_count_ACT_ISDA(tfinal, tinitial))
  }else if(convention %in% c('ACT/ACT-AFB')){
    return(.day_count_ACT_AFB(tfinal, tinitial))
  }else{
    # Never
  }


}

.day_count_ACT_FIX <- function(tfinal, tinitial, convention,...){
  factor <- as.numeric(gsub('[A-Z]{1,}[/]','',convention))
  return(difftime_leap_year(tfinal,tinitial,...)/factor)
}
.day_count_ACT_L <- function(tfinal, tinitial){
  tinitial <- lubridate::add_with_rollback(tinitial, lubridate::days(1),
                                           roll_to_first = FALSE, preserve_hms = TRUE)
  factor <- ifelse(test = '02-29' %in% format(seq(tinitial,
                                                  tfinal,
                                                  by='days'),'%m-%d'),
                   yes=366,
                   no=365)
  return(difftime_leap_year(tfinal,tinitial,leapDatesIn = F)/factor)

}
.day_count_30360 <- function(tfinal, tinitial){
  num.years <- lubridate::year(tfinal)-lubridate::year(tinitial)
  num.months <- lubridate::month(tfinal)-lubridate::month(tinitial)
  d1 <- min(lubridate::day(tinitial), 30)
  d2 <- ifelse(d1==30, min(lubridate::day(tfinal), 30), lubridate::day(tfinal))
  num.days <- d2-d1

  return((360*num.years+30*num.months+num.days)/360)
}
.day_count_ACT_ISDA <- function(tfinal, tinitial){
  if(any(sapply(lubridate::year(tfinal):lubridate::year(tinitial),lubridate::leap_year))){
    dates <- seq(tinitial, tfinal, by='day')
    count <- sapply(lubridate::year(tfinal):lubridate::year(tinitial),function(x, dates){
      factor <- ifelse(lubridate::leap_year(x), 366,365)
      return(sum(lubridate::year(dates)==x)/factor)
    }, dates)
    count <- sum(count)
    return(count)
  }else{
   return(difftime_leap_year(tfinal,tinitial,leapDatesIn = T)/365)
  }
}
.day_count_ACT_AFB <- function(tfinal, tinitial){
  result <- 0
  if(lubridate::time_length(lubridate::interval(tinitial, tfinal),unit ='years')>=1){
    result <- floor(lubridate::time_length(lubridate::interval(tinitial, tfinal),unit ='years'))
    lubridate::year(tfinal) <- lubridate::year(tinitial)
  }
  tfinal <- lubridate::add_with_rollback(tfinal, lubridate::days(-1),
                                         roll_to_first = FALSE, preserve_hms = TRUE)
  factor <- ifelse(test = '02-29' %in% format(seq(tinitial,
                                                  (tfinal),
                                                  by='days'),'%m-%d'),
                   yes=366,
                   no=365)
  result <- result + (difftime_leap_year(tfinal,tinitial,leapDatesIn = F)/factor)
  return(result)

}
