install.packages("base64enc")
install.packages(c("RCurl","twitteR","ROAuth"))
library(base64enc)
library(RCurl)
library(twitteR)
library(ROAuth)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"


consumerKey <- "5n7BkdJRlgoSBprjueZaUDSke"
consumerSecret <- "eNfUxMKEzI1OS3iJ0YusJyHA2z006q6015egC1jnYUFeqoMbLq"
accesstoken <- "831483676009467906-p6cViBK2NoYCByBe8grLbALgDDJn4Mr"
accesstokensecret <- "4Op2HAQVTLpZ7kZnhkFitPJ6zR0ncYpfLB0tcU4qSYzzq"


options(RCurlOptions = list(cainfo = system.file("CurlSSL","cacert.pem",package = "RCurl")))
download.file(url = "https://curl.haxx.se/ca/cacert.pem",destfile = "cacert.pem")


setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesstokensecret)


keyword <- enc2utf8("황사")

data1<- searchTwitter(keyword,since='2015-01-01')
data1

