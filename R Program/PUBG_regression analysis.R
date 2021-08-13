data = read.csv('C:/Users/GaeJu/Desktop/21년/1학기/회귀분석/기말 과제/train.csv')
head(data)

data$matchType <- gsub("solo-fpp", 'solo', data$matchType)
data$matchType <- gsub("duo-fpp", 'duo', data$matchType)
data$matchType <- gsub("squad-fpp", 'squad', data$matchType)

# solo mode
solo <- subset(data, matchType == 'solo')
# head(solo)
solo.lm <- lm(winPlacePerc ~ boosts + damageDealt + headshotKills + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , solo)
library(regbook)
summary(vif(solo.lm))

summary(solo.lm)

solo10p <- subset(solo, winPlacePerc >= 0.9)
solo10p.lm <- lm(winPlacePerc ~ boosts + damageDealt + headshotKills + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , solo10p)
summary(vif(solo10p.lm))
summary(solo10p.lm)

solo10p1.lm <- lm(winPlacePerc ~ boosts + damageDealt + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , solo10p)
# summary(vif(solo10p1.lm))
summary(solo10p1.lm)



# duo mode
duo <- subset(data, matchType == 'duo')
# head(duo)
duo.lm <- lm(winPlacePerc ~ assists + boosts + damageDealt + DBNOs + headshotKills + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , duo)
summary(vif(duo.lm))
summary(duo.lm)
duo1.lm <- lm(winPlacePerc ~ assists + boosts + damageDealt + DBNOs + headshotKills + heals + kills + killStreaks + rideDistance + walkDistance , duo)
summary(duo1.lm)

duo10p <- subset(duo, winPlacePerc >= 0.9)
duo10p.lm <- lm(winPlacePerc ~ assists + boosts + damageDealt + DBNOs + headshotKills + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , duo10p)
summary(vif(duo10p.lm))
summary(duo10p.lm)



# squad mode
squad <- subset(data, matchType == 'squad')
# head(squad)
squad.lm <- lm(winPlacePerc ~ assists + boosts + damageDealt + DBNOs + headshotKills + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , squad)
summary(vif(squad.lm))
summary(squad.lm)

squad10p <- subset(squad, winPlacePerc >= 0.9)
squad10p.lm <- lm(winPlacePerc ~ assists + boosts + damageDealt + DBNOs + headshotKills + heals + kills + killStreaks + longestKill + rideDistance + walkDistance , squad10p)
summary(vif(squad10p.lm))
summary(squad10p.lm)

