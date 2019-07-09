library(dplyr)

#AllHotels <- read.csv("data/Hotel_Reviews.csv")

# HotelsPart1 <- read.csv("data/Hotel_Reviews1.csv")
# HotelsPart2 <- read.csv("data/Hotel_Reviews2.csv")
# HotelsPart3 <- read.csv("data/Hotel_Reviews3.csv")
# HotelsPart4 <- read.csv("data/Hotel_Reviews4.csv")
# HotelsPart5 <- read.csv("data/Hotel_Reviews5.csv")
# HotelsPart6 <- read.csv("data/Hotel_Reviews6.csv")
# AllHotels <- do.call("rbind", list(HotelsPart1, HotelsPart2, HotelsPart3, HotelsPart4, HotelsPart5, HotelsPart6))

AllHotels <- readRDS("data/Hotel_Reviews.rds")

AllHotelsGrouped <- AllHotels %>%
  select(
    Hotel = Hotel_Name,
    Adres = Hotel_Address,
    Lat = lat,
    Long = lng,
    Score = Average_Score,
    Reviews = Total_Number_of_Reviews
  ) %>%
  group_by(Adres) %>%
  summarise(Hotel = first(Hotel), Score = mean(Score, na.rm = TRUE), Lat = mean(Lat, na.rm = TRUE), Long = mean(Long, na.rm = TRUE), Reviews = mean(Reviews)) %>%
  na.omit()


AllHotels$Positive <- ifelse(AllHotels$Reviewer_Score > 6, 1, 0)

PositiveHotels <- subset(AllHotels, Positive == 1,
                         select=c(Hotel_Address,Hotel_Name,lat,lng, Average_Score,Total_Number_of_Reviews,Additional_Number_of_Scoring,
                                  Reviewer_Nationality,Review_Date,Positive_Review,Review_Total_Positive_Word_Counts, 
                                  Total_Number_of_Reviews_Reviewer_Has_Given, Reviewer_Score, Tags, Positive))

NegativeHotels <- subset(AllHotels, Positive == 0,
                         select=c(Hotel_Address,Hotel_Name,lat,lng, Average_Score,Total_Number_of_Reviews,Additional_Number_of_Scoring 
                           ,Reviewer_Nationality,Review_Date,Negative_Review,Review_Total_Negative_Word_Counts
                           , Total_Number_of_Reviews_Reviewer_Has_Given, Reviewer_Score,Tags, Positive))

colnames(PositiveHotels)[colnames(PositiveHotels)=="Positive_Review"] <- "Review"
colnames(PositiveHotels)[colnames(PositiveHotels)=="Review_Total_Positive_Word_Counts"] <- "Review_Word_Counts"
colnames(NegativeHotels)[colnames(NegativeHotels)=="Negative_Review"] <- "Review"
colnames(NegativeHotels)[colnames(NegativeHotels)=="Review_Total_Negative_Word_Counts"] <- "Review_Word_Counts"

AllHotels <- rbind(PositiveHotels, NegativeHotels)
AllHotels <- AllHotels[order(AllHotels$Hotel_Name),]

cleanTable <- AllHotels %>%
  select(
    Hotel = Hotel_Name,
    Adres = Hotel_Address,
    Score = Average_Score,
    Reviews = Total_Number_of_Reviews,
    Review_Date,
    Reviewer_Score,
    Reviewer_Nationality,
    Review,
    Review_Word_Counts,
    Positive,
    Lat = lat,
    Long = lng
  )

