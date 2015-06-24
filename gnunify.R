
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


load("twitter_authentication.Rdata")
registerTwitterOAuth(cred)


################### PACKAGE DEMO ####################################

me <- getUser("DeepSinghB", cainfo="cacert.pem")
me
me$getFollowers(cainfo="cacert.pem")
friendships("dishantkapadia")

showStatus("77680692") # this of dishant guy
fav = favorites("barackobama", n=10)
fav

################### TWITTER APPLICATION ###############################

t=searchTwitter("#namo", n=100)

userInfo<-function(USER)
{
  temp<-getUser(USER, cainfo="cacert.pem")
  USERdata<-c(temp$location)
  return(USERdata)
}


###############################THIS RETURNS A LIST OF USER NAMES#################################
getUsers<-function(t)
{
  user_name<-c()
  user_location<-c()
  for (i in 1:length(t))
  {
    user_name[i]<-t[[i]]$getScreenName()
    temp_name<-user_name[i]
    user_location<-c(userInfo(temp_name),user_location)
    
  }
  ul=user_location
  return (ul)
}
data<-getUsers(t)
data
# GET A LIST OF LATTITUDE AND LONGITUDE 
data2<-ggmap::geocode(data) 
data2
#------- WAIT --- 
map = ggmap(
  get_map(location = "India",
          zoom=5,
          maptype='terrain',
          scale = 2))

#------ WAIT -----#
map
map+ geom_density2d(data = data2,aes(x = lon, y = lat), size = 0.2)+
  stat_density2d(data = data2,
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 size = 0.01,bins = 12, geom = "polygon") +
  scale_fill_gradient(low = "yellow", high = "red",guide = "colourbar")+
  scale_alpha(range = c(0, 0.3),guide=FALSE)


#------ WAIT -----#
map +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = data2,
    geom = "polygon"
  )+
  scale_alpha(guide=FALSE)+
  scale_fill_gradient(guide=FALSE)
