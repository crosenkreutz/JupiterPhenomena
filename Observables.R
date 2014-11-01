require(astroFns)

load(file = "phenomena2014.dat")

LongitudeHolzkirchen = "E 11d 42.0m"
LatitudeHolzkirchen = "47d 53m 21.31s"

RAHours <- floor(Phenomena$RightAscension)
Dummy <- Phenomena$RightAscension %% 1 * 60.
RAMinutes <- floor(Dummy)
RASeconds <- round(Dummy %% 1 * 60., digits = 2)
RAString <- paste(RAHours, "h ", RAMinutes, "m ", RASeconds,"s", sep = "")

DecDegrees <- floor(Phenomena$Declination)
Dummy <- Phenomena$Declination %% 1 * 60.
DecMinutes <- floor(Dummy)
DecSeconds <- round(Dummy %% 1 * 60., digits = 2)
DecString <- paste(DecDegrees, "d ", DecMinutes, "m ", DecSeconds, "s", sep = "")

HourAngles <- ut2ha(yr = year(Phenomena$EventDateTime), mo = month(Phenomena$EventDateTime), dy = day(Phenomena$EventDateTime), hr = hour(Phenomena$EventDateTime), mi = minute(Phenomena$EventDateTime), se = second(Phenomena$EventDateTime), ra.sou = RAString, lon.obs = LongitudeHolzkirchen)
Elevations <- elev(dec.sou = DecString, ha = HourAngles, lat.obs = LatitudeHolzkirchen)
Azimuths <- azimuth(dec.sou = DecString, ha = HourAngles, lat.obs = LatitudeHolzkirchen)