library(readr)

birthplace_rawdata<-read_csv("../../../data/Birthplace/TABLECODE8281_Data_7f89cac6-6a71-4fea-a52a-5a60be94c4e6.csv")



# key_interest_countries <- c("US","China (mainland)",,"United Kingdom",
#                             "Japan","India",,
#                             ,"Canada","Korea, South",
#                             "Germany","Indonesia",,
#                             "Singapore","Hong Kong, China",
#                             ,"Philippines",
#                             "Malaysia","France",
#                             #australian states, listed separately.
#                             "New South Wales, Australia",
#                             "Victoria, Australia","Queensland, Australia",
#                             "South Australia, Australia","Western Australia, Australia",
#                             "Tasmania, Australia","Northern Territory, Australia",
#                             "Australian Capital Territory, Australia"
#                             
# )

#how many people 
covid_free<-c("Cook Islands","Samoa","Fiji","Taiwan","Thailand")

v_low_prev <-c("China, People's Republic of","Malaysia")
birthplace_rawdata %>% filter(Birthplace %in% covid_free) %>% summarise(TotalBornInLocation=sum(Value))

birthplace_rawdata %>% filter(Birthplace %in% v_low_prev) %>% summarise(TotalBornInLocation=sum(Value))
low_prev <- c("Korea, Republic of")
birthplace_rawdata %>% filter(Birthplace %in% low_prev) %>% summarise(TotalBornInLocation=sum(Value))

mod_prev <- c("Canada", "Germany", "Hong Kong", "Japan", "Singapore")
birthplace_rawdata %>% filter(Birthplace %in% mod_prev) %>% summarise(TotalBornInLocation=sum(Value))


#holiday arrivals

holiday_arrivals_raw<-read_csv("../../../symposium/holiday_arrivals_2019.csv")

holiday_arrivals <- 
  holiday_arrivals_raw %>% tidyr::gather("Month","ArrivalCount",2:ncol(holiday_arrivals_raw)) %>%
  mutate(ArrivalCount=as.integer(ArrivalCount)) %>%
  group_by(`Visitor arrivals by EVERY country of residence and purpose (Monthly)`) %>% 
  summarise("ArrivalCountYearly"=sum(ArrivalCount,na.rm = TRUE))
holiday_arrivals %>% filter(`Visitor arrivals by EVERY country of residence and purpose (Monthly)` %in% covid_free) %>% 
  .$ArrivalCountYearly %>%  sum

holiday_arrivals %>% filter(`Visitor arrivals by EVERY country of residence and purpose (Monthly)` %in% v_low_prev) %>% 
  .$ArrivalCountYearly %>%  sum

holiday_arrivals %>% filter(`Visitor arrivals by EVERY country of residence and purpose (Monthly)` %in% v_low_prev)
  
