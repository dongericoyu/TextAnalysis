setwd("C:/Users/Dong/Desktop/Ongoing Project-R/TwitterSentiment_2018")

library(rtweet)
library(tm)

## install httpuv if not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

## installsyuzhet if not already
if (!requireNamespace("syuzhet", quietly = TRUE)) {
  install.packages("syuzhet")
}

## autheticate via web browser
token <- create_token(
  app = "Senate2018Erico",
  consumer_key = "hjMbAiy0OKEqbXqAPyH8kpi5e",
  consumer_secret = "C6d6fVZAmLJr0NIUh95BZiBO18yh6PPIYMQH8wPS8g1kIKwbJs")

## check to see if the token is loaded
identical(token, get_token())

## get user IDs of accounts followed by Senators
JeffFlake <- get_timelines(c("JeffFlake"), n = 3200)

save_as_csv(JeffFlake, "JeffFlake", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")



BernieSanders <- get_timelines(c("SenSanders"), n = 3200)



save_as_csv(BernieSanders, "BernieSanders", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")




#All GOP Senators
DEM <- get_timelines(c("SenFeinstein",
                       "ChrisMurphyCT",
                       "SenatorCarper",
                       "senbillnelson",
                       "maziehirono",
                       "SenDonnelly",
                       "SenatorCardin",
                       "SenWarren",
                       "SenStabenow",
                       "amyklobuchar",
                       "SenTinaSmith",
                       "clairecmc",
                       "SenatorTester",
                       "SenatorMenendez",
                       "MartinHeinrich",
                       "SenGillibrand",
                       "SenSherrodBrown",
                       "SenBobCasey",
                       "SenWhitehouse",
                       "SenatorSanders",
                       "timkaine",
                       "SenatorCantwell",
                       "Sen_JoeManchin",
                       "SenDougJones",
                       "ChrisCoons",
                       "SenatorDurbin",
                       "SenatorCollins",
                       "senmarkey",
                       "SenGaryPeters",
                       "SenatorShaheen",
                       "CoryBooker",
                       "SenatorTomUdall",
                       "SenJeffMerkley",
                       "SenJackReed",
                       "MarkWarner",
                       "KamalaHarris",
                       "SenBennetCO",
                       "SenBlumenthal",
                       "brianschatz",
                       "SenDuckworth",
                       "ChrisVanHollen",
                       "SenCortezMasto",
                       "SenatorHassan",
                       "SenSchumer",
                       "SenJohnHoeven",
                       "RonWyden",
                       "SenatorLeahy",
                       "PattyMurray",
                       "SenRonJohnson"
), n = 3200)

save_as_csv(DEM, "DemTwitters", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")

save_as_csv(DEM, "DemTwitters_new", prepend_ids = TRUE, na = "",
            fileEncoding = "ANSI")




#All GOP Senators
GOP <- get_timelines(c("JeffFlake",
                       "SenAngusKing",
                       "SenatorWicker",
                       "cindyhydesmith",
                       "SenatorFischer",
                       "SenDeanHeller",
                       "SenatorHeitkamp",
                       "SenAlexander",
                       "SenTedCruz",
                       "SenOrrinHatch",
                       "SenatorBaldwin",
                       "SenJohnBarrasso",
                       "SenDanSullivan",
                       "SenTomCotton",
                       "SenCoryGardner",
                       "sendavidperdue",
                       "SenatorRisch",
                       "joniernst",
                       "SenPatRoberts",
                       "SenateMajLdr",
                       "BillCassidy",
                       "SteveDaines",
                       "BenSasse",
                       "SenThomTillis",
                       "jiminhofe",
                       "GrahamBlog",
                       "SenatorRounds",
                       "BobCorker",
                       "JohnCornyn",
                       "SenCapito",
                       "SenatorEnzi",
                       "SenShelby",
                       "lisamurkowski",
                       "SenJohnMcCain",
                       "JohnBoozman",
                       "marcorubio",
                       "SenatorIsakson",
                       "MikeCrapo",
                       "SenToddYoung",
                       "ChuckGrassley",
                       "JerryMoran",
                       "RandPaul",
                       "SenJohnKennedy",
                       "RoyBlunt",
                       "SenatorBurr",
                       "SenRobPortman",
                       "SenatorLankford",
                       "SenToomey",
                       "SenatorTimScott",
                       "SenJohnThune",
                       "SenMikeLee"
), n = 3200)

save_as_csv(GOP, "GopTwitters", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")

## plot the frequency of tweets for each user over time
JeffFlake %>%
  dplyr::filter(created_at > "2014-01-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("month", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


save_as_csv(JeffFlake, "Testing", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")
