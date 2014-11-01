AddZeroes <- function(x){
  Ind <- as.integer(x) < 10
  x[Ind] <- paste("0", as.integer(x[Ind]), sep = "")
  return(x)
}

InterEphem <- function(t){
  Ind <- which.max(Ephemeris$DateTime[Ephemeris$DateTime < t])
  InterFunRa <- approxfun(Ephemeris$DateTime[Ind:(Ind + 1)], y = Ephemeris$RightAscension[Ind:(Ind + 1)])
  InterFunDec <- approxfun(Ephemeris$DateTime[Ind:(Ind + 1)], y = Ephemeris$Declination[Ind:(Ind + 1)])
  InterFunDist <- approxfun(Ephemeris$DateTime[Ind:(Ind + 1)], y = Ephemeris$Distance[Ind:(Ind + 1)])
  ra <- InterFunRa(t)
  Dec <- InterFunDec(t)
  Dist <- InterFunDist(t)
  return(c(ra, Dec, Dist))
}

require(stringr)
require(lubridate)

FileConnection <- file('phencE.2014.txt')
FileContent <- readLines(con = FileConnection)
close(FileConnection)

FileContentWithoutHeader = FileContent[11:length(FileContent)]

Years <- substring(FileContentWithoutHeader, 21, 22)
Months <- substring(FileContentWithoutHeader, 23, 24)
Days <- substring(FileContentWithoutHeader, 26, 27)
Hours <- substring(FileContentWithoutHeader, 29, 30)
FractionalMinutes <- as.numeric(substring(FileContentWithoutHeader, 31,34))
MoonNumber <- str_trim(substring(FileContentWithoutHeader, 36, 38))
EventId <- substring(FileContentWithoutHeader, 40, 41)
EventLimitId <- substring(FileContentWithoutHeader, 43, 43)

Minutes <- as.character(floor(FractionalMinutes))
Seconds <- as.character(round((FractionalMinutes %% 1) * 60)) 

Years <- AddZeroes(Years)
Months <- AddZeroes(Months)
Days <- AddZeroes(Days)
Hours <- AddZeroes(Hours)
Minutes <- AddZeroes(Minutes)
Seconds <- AddZeroes(Seconds)

EventDateTime <- ymd_hms(paste(Years, Months, Days, Hours, Minutes, Seconds, sep = ""), tz = "GMT")
Ind <- !is.na(EventDateTime)
EventDateTime <- EventDateTime[Ind]
EventDateTime <- EventDateTime - 67
MoonNumber <- MoonNumber[Ind]
EventId <- EventId[Ind]
EventLimitId <- EventLimitId[Ind]

Phenomena <- data.frame(EventDateTime, MoonNumber, EventId, EventLimitId)
Ind <- Phenomena$EventDateTime > ymd("140101", tz = "GMT")
Phenomena <- Phenomena[Ind,]

FileContent <- read.table("tmp21723.Jupiter", skip = 17, header = FALSE)

Months <- AddZeroes(FileContent$V1)
Days <- AddZeroes(FileContent$V2)
Years <- rep("14", times = length(Days))
Years[length(Years)] <- as.character(as.integer(Years[1]) + 1)
Zeroes <- rep("00", times = length(Days))
DateTime <- ymd(paste(Years, Months, Days, sep = ""), tz = "GMT")

RightAscension <- FileContent$V7 + FileContent$V8 / 60 + FileContent$V9 / 3600
Declination <- as.numeric(FileContent$V10) + FileContent$V11 / 60 + FileContent$V12 / 3600
Distance <- FileContent$V13

Ephemeris <- data.frame(DateTime, RightAscension, Declination, Distance)

Coordinates <- sapply(Phenomena$EventDateTime, InterEphem)
Phenomena$RightAscension <- Coordinates[1,]
Phenomena$Declination <- Coordinates[2,]
Phenomena$Distance <- Coordinates[3,]

save(Phenomena, file = "phenomena2014.dat")
remove(list = objects())