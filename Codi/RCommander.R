data<-read.csv("C:/Users/Fenix/Dropbox/uoc/Master Data Science/Tipologia i cicle de vida de les dades/PRACTICA2/calendar.csv")
sorted_data<-data[order(data$listing_id, data$date),]
Unique_IDs<-with(sorted_data,unique(listing_id))
head(Unique_IDs)
Occupied<-with(sorted_data, tapply(available, listing_id, function(x) sum((x=='t'), na.rm=TRUE)))
head(Occupied)

listings<-read.csv("C:/Users/Fenix/Dropbox/uoc/Master Data Science/Tipologia i cicle de vida de les dades/PRACTICA2/listings.csv")
head(listings)
listings<-listings[order(listings$id),]
listings$occupied<-Occupied

listings<-read.csv("C:/Users/Fenix/Dropbox/uoc/Master Data Science/Tipologia i cicle de vida de les dades/PRACTICA2/listings.csv")
listings<-listings[,c('id','host_id','host_name','host_since','host_response_time','host_response_rate','host_acceptance_rate','host_listings_count','host_identity_verified','host_has_profile_pic','neighbourhood','neighbourhood_cleansed','neighbourhood_group_cleansed','city','zipcode','property_type','room_type','accommodates','bathrooms','bedrooms','beds','bed_type','square_feet','price','cleaning_fee','guests_included','availability_365','number_of_reviews','review_scores_rating','cancellation_policy','instant_bookable','reviews_per_month')]
head(listings)
summary(listings)

listings$price<-as.numeric(listings$price)
listings$cleaning_fee<-as.numeric(listings$cleaning_fee)
Tam<-cut(listings$bedrooms,breaks=c(0,1,3,10), labels=c('small','medium','big'))
table(Tam)
listings$size<-Tam
sum(is.na(listings$size))
listings$size[is.na(listings$size)]<-'small'
with(listings, discretePlot(bedrooms, scale="frequency"))
normalityTest(~accommodates, test="ad.test", data=listings)
normalityTest(~bedrooms, test="ad.test", data=listings)
levels(listings$city)

library(stringr)
listings$city<-str_to_title(listings$city)
listings$city<-as.factor(listings$city)

listings$city<-trimws(listings$city)
listings$city<-str_replace(listings$city,"Barcelone","Barcelona")
listings$city<-str_replace(listings$city,"Barcelonaneta","Barcelona")
listings$city<-str_replace(listings$city,"08014 Barcelona","Barcelona")
listings$city<-str_replace(listings$city,"Barcellona","Barcelona")
listings$city<-str_replace(listings$city,"Barcelona, Catalunya, Es","Barcelona")
listings$city<-str_replace(listings$city,"Barcelona, Catalunya,","Barcelona")
listings$city<-str_replace(listings$city,"Corcega","Barcelona")
listings$city<-str_replace(listings$city,"L'hospitalet De Llobregat","Hospitalet De Llobregat")

levels(listings$neighbourhood_group_cleansed)

listings$neighbourhood_group_cleansed<-str_replace(listings$neighbourhood_group_cleansed,"GrÃ cia","Gràcia")
listings$neighbourhood_group_cleansed<-str_replace(listings$neighbourhood_group_cleansed,"Horta-GuinardÃ³","Horta-Guinardó")
listings$neighbourhood_group_cleansed<-str_replace(listings$neighbourhood_group_cleansed,"Sant MartÃ­","Sant Martí")
listings$neighbourhood_group_cleansed<-str_replace(listings$neighbourhood_group_cleansed,"Sants-MontjuÃ¯c","Sants-Montjuíc")
listings$neighbourhood_group_cleansed<-str_replace(listings$neighbourhood_group_cleansed,"SarriÃ -Sant Gervasi","Sarrià-Sant Gervasi")
listings$neighbourhood_group_cleansed<-as.factor(listings$neighbourhood_group_cleansed)


levels(listings$property_type)


library(colorspace, pos=17)
with(listings, pie(table(property_type), labels=levels(property_type), xlab="", ylab="", 
  main="property_type", col=rainbow_hcl(25)))
with(listings, pie(table(room_type), labels=levels(room_type), xlab="", ylab="", main="room_type", 
  col=rainbow_hcl(3)))
stripchart(availability_365 ~ room_type, vertical=TRUE, method="stack", ylab="availability_365", 
  data=listings)
local({
  .Table <- with(listings, table(neighbourhood_group_cleansed))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
with(listings, qqPlot(accommodates, dist="norm", id.method="y", id.n=2, labels=rownames(listings)))
indexplot(listings[,'accommodates', drop=FALSE], type='h', id.method='y', id.n=2)
with(listings, Dotplot(accommodates, bin=FALSE))
stripchart(accommodates ~ room_type, vertical=TRUE, method="stack", ylab="accommodates", 
  main="Distribución por capacidad", data=listings)
with(listings, Dotplot(accommodates, bin=FALSE))

pie(table(listings$size), labels=levels(listings$size), xlab="", ylab="", 
  main="Tamany", col=rainbow_hcl(4))

head(listings)
sapply(listings, function(x) sum(is.na(x)))

library(VIM)

newlistings=kNN(listings, k=6)

nrow(listings)

for (x in 1:17653) {if (is.na(listings$beds[x])) {listings$beds[x]<-newlistings[,21][x]}}
for (x in 1:17653) {if (is.na(listings$bedrooms[x])) {listings$bedrooms[x]<-newlistings[,20][x]}}
for (x in 1:17653) {if (is.na(listings$bathrooms[x])) {listings$bathrooms[x]<-newlistings[,19][x]}}

sapply(listings, function(x) sum(is.na(x)))

fivenum(listings$price)
summary(listings$price)

boxplot(listings$price)
points(mean(listings$price), pch=4, col="red")

boxplot(listings$accomodates)
Boxplot(~ accommodates, data=listings, id.method="y")
points(mean(listings$accommodates), pch=4, col="red")


hist(listings$price, freq=FALSE, col="lightcyan", main="Histograma del preu dels lloguers", xlab="Preu", ylab="Densitat")

curve(dnorm(x, mean=mean(listings$price), sd=sd(listings$price)), from=0, to=380, add=TRUE, col="red", lwd=2)

legend("topleft", col="red", legend="Densitat normal estimada", lwd=2, bty="n")

hist(listings$cleaning_fee, freq=FALSE, col="lightcyan", main="Histograma Taxa de Neteja", xlab="Preu", ylab="Densitat")
curve(dnorm(x, mean=mean(listings$cleaning_fee), sd=sd(listings$cleaning_fee)), from=0, to=100, add=TRUE, col="red", lwd=2)
legend("topright", col="red", legend="Densitat normal estimada", lwd=2, bty="n")

summary(listings)

sol.test=t.test(listings$price, mu=99.5, alternative="two.sided", conf.levels=0.95)
sol.test

mean(listings$price)
sd(listings$price)

z=(mean(listings$price)-99.5)/(sd(listings$price)/sqrt(length(listings$price)))
z

preus<-listings[,c(13,17,18,20,24,29,33)]
head(preus)

pairs(preus)

levels(listings$neighbourhood_group_cleansed)

resultat<-lm(price ~ neighbourhood_group_cleansed + room_type + accommodates + bedrooms + review_scores_rating + size, data=preus)
summary(resultat)


