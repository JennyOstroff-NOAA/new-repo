#Finding Duplicate Records

#This code provides a list of duplicated or overlapping records by selecting for a vessel and date/time range


#### Set up ####

#Load all necessary packages
library(tidyverse) #contains dplyr, lubridate, and readr

#set your path
jennys_path <- r"(C:\Users\jenny.ostroff\Desktop\Rcode\QAQC\DuplicateRecordsReport)"
# !! Change to your path !!
Path <- jennys_path
# create these folders in your directory first
Inputs <- "Inputs"
Outputs <- "Outputs"

#set year, for file naming later
my_year <- "2024"


#### import the data ####

#So that we don’t have to manually rename the logbook source file, we can use this chunk of code
# Explanations:
# - This function, `get_the_dates`, generates a list of date strings and date objects based on a specified year and a start day for weeks.
# - It takes two parameters: `my_year`, which is defined above, and `week_start_day`, which defaults to "Monday".
# - The function returns a list (`lst`) of the start and end dates for the calendar year, as well as the start and end dates for compliance weeks to be included, based on week boundaries. lst is used to keep entries names.
# The beg and end dates include the "fringe" weeks if needed.
#
# 1. **Generating Calendar Dates**:
#     - The function first creates two strings representing the start and end dates of the calendar year based on the provided `my_year` parameter.
#     - `my_calendar_date_beg` is set to "01-JAN-{my_year}" and `my_calendar_date_end` is set to "31-DEC-{my_year}" using string interpolation (`str_glue`).
#
# 2. **Calculating Beg and End Date Boundaries**:
#     - The function calculates the compliance week start and end dates based on the provided `week_start_day` option.
#     - It uses `lubridate` functions to convert the calendar date strings to date objects (`dmy`) and then adjust them to the nearest week boundaries.
#     - `my_date_beg` is calculated as the beginning of the week containing the calendar start date.
#     - `my_date_end` is calculated as the end of the week containing the calendar end date, minus one day.
#     - The `getOption` function is used to ensure the start of the week is set according to `week_start_day`.
#
# 3. **Creating the List of Dates**:
#     - The function combines the four calculated dates (`my_calendar_date_beg`, `my_calendar_date_end`, `my_date_beg`, and `my_date_end`) into a list using the `lst` function.
#
week_start_day = "Monday"


get_the_dates <-
  function(my_year,
           week_start_day = "Monday") {
    my_calendar_date_beg <- str_glue("01-JAN-{my_year}")
    my_calendar_date_end <- str_glue("31-DEC-{my_year}")
    my_date_beg <-
      dmy(my_calendar_date_beg) |>
      floor_date('weeks', week_start = getOption("lubridate.week.start", week_start_day))
    my_date_end <-
      dmy(my_calendar_date_end) |>
      ceiling_date('weeks',
                   week_start = getOption("lubridate.week.start", week_start_day)) - 1
    
    
    my_dates <- lst(
      my_calendar_date_beg,
      my_calendar_date_end,
      my_date_beg,
      my_date_end
    )
    
    
    return(my_dates)
  }


curr_dates <- get_the_dates(my_year)
my_date_beg <- curr_dates$my_date_beg
my_date_end <- curr_dates$my_date_end

#import data - from what source? Raw logbooks or processed logbooks
#load logbook data using the my_date_beg and my_date_end calculated above
myLogbookData <- str_glue("Raw_Oracle_Downloaded_logbook_{my_date_beg}__{my_date_end}.rds")
#read in all logbook data 
RawLogbooks <- readRDS(file.path(Path,Inputs,myLogbookData)) 


#### analysis #### 

#reduce logbook data frame down to one row per unique TRIP_ID
#it it's current form, each time there is a new catch sequence for a TRIP_ID, there is a new row
#this could create false duplicates, so we want to filter the data frame to just one row for each TRIP_ID
RawLogbooksTripID <- unique(RawLogbooks[,c(1,3,4,6,7,10:13,26,27)])

#merge date and time so we can work with these variables


#find overlapping trip dates/times by vessel 



