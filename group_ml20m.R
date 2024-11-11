#################################################################
#################################################################
### IGNITION PROGRAM APPLICATION - MAIN SCRIPT ##################
### MOVIE RECOMMENDATION ########################################
#################################################################
#################################################################
##### DATA READING ##############################################
#################################################################
### Data set on ~/ml-20m
dir("./ml-20m")
### List of movies
movies = read.csv("ml-20m/movies.csv")
movies$title = as.character(movies$title)
### List of tags
tags = read.csv("ml-20m/tags.csv")
### List of ratings
ratings = read.csv("ml-20m/ratings.csv")
### List of ratings
gentags = read.csv("ml-20m/genome-tags.csv")
### List of ratings
genscores = read.csv("ml-20m/genome-scores.csv")
### Data saving
save.image(file="movielens_init.RData")

#################################################################
##### MOVIE DATA CLEANING #######################################
#################################################################
### Extract title and year from the original data set ###########
# Modify 'movies' table
movies_saved = movies
### Function extractyear(dataframe)
# Modify 'movies' table in order to extract year from 'title' variable
# Standardized title format: '<title> (<year>)'
extractyear = function(movies){
  # Remove useless spaces at the end of each title
  movies$title = gsub(" *$","",movies$title)
  # Prepare structures that will be returned
  title = rep(NA,nrow(movies))
  year = rep(NA,nrow(movies))
  # Compute values for these structures
  title = apply( movies , 1 , function(x){substr(x[["title"]],start = 0,stop=nchar(x[["title"]])-7)}  )
  year = apply( movies , 1 , function(x){as.numeric(substr(x[["title"]],start = nchar(x[["title"]])-4,stop=nchar(x[["title"]])-1))}  )
  # Manage bad format entries (good format: '<title> (<year>)')
  lastcparid=sapply(gregexpr(")", movies$title), tail, 1)
  lastoparid=sapply(gregexpr("(", movies$title,fixed=T), tail, 1)
  lastid=unname(sapply(movies$title,nchar))
  failindex = which(lastid-lastcparid!=0 | lastcparid-lastoparid!=5)
  title[failindex] = movies$title[failindex]
  # Return new version of 'movies' data frame
  return (data.frame(movieId=movies$movieId,title=title,year=year,genres=movies$genres) )
}
movies = extractyear(movies_saved)
summary(movies)
remove(movies_saved)
# Movies still with bad format : 19860, 22369, 22670
movies[19860,"title"] = "Mona and the Time of Burning Love (Mona ja palavan rakkauden aika)"
movies[19860,"year"] = 1983
movies[22369,"title"] = "Diplomatic Immunity"
movies[22369,"year"] = 2009
movies[22670,"title"] = "Big Bang Theory, The"
movies[22670,"year"] = 2007

### Distinguish movie genres ####################################
tabofgenres = as.character(movies$genres)
listofgenres = mapply(tabofgenres,FUN=function(x){strsplit(x,"[|]")})
nbgenres = unname(sapply(listofgenres,length))
barplot(prop.table(table(nbgenres)),yaxp=c(0,1,10),ylim=c(0,1), main="Nb genre per movie")
genres = factor(unlist(unname(mapply(tabofgenres,FUN=function(x){strsplit(x,"[|]")}))))
remove(listofgenres,nbgenres)

#################################################################
##### RATING DATA EXPLORATION ###################################
#################################################################
### Objective: Inferno, Ron Howard (2016)
### Main actor: Tom Hanks, Genres: Action, Adventure, Crime
director = "Ron Howard"
actor = "Tom Hanks"
targettedgenresML = c("Action","Crime","Mystery","Sci.Fi","Thriller")

### Content-based recommendation ################################
# Ron Howard
RHmovieId = tags[tags$tag==director,"movieId"]
RHmovies = movies[movies$movieId %in% RHmovieId,]
# Tom Hanks
THmovieId = tags[tags$tag==actor,"movieId"]
THmovies = movies[movies$movieId %in% THmovieId,]
# Mix Ron Howard / Tom Hanks
recommendation_THRH = movies[movies$movieId %in% THmovieId & movies$movieId %in% RHmovieId,] ; recommendation_THRH
# Ron Howard scoring (0.5 point if Ron Howard is in the movie tag -> hypothesis: it means that he directed the movie)
RHscores = as.numeric(movies$movieId%in%tags[tags$tag==director,"movieId"])/2
# Tom Hanks scoring (0.5 point if Tom Hanks is in the movie tag)
THscores = as.numeric(movies$movieId%in%tags[tags$tag==actor,"movieId"])/2
# Movielens genre scoring : ( accuracy_target / nbgenre_target ) * ( accuracy_target / nbgenre_movie )
accuracy_target = apply(sapply(targettedgenresML,FUN=function(t){regexpr(t,movies$genres)>0}),1,sum)
nbgenre_target = length(targettedgenresML)
nbgenre_movie = sapply(movies$genres,FUN=function(m){1+ifelse(gregexpr("[|]",m)[[1]][1]>0,length(gregexpr("[|]",m)[[1]]),0)})
MLgenrescores = round((accuracy_target/nbgenre_target)*(accuracy_target/nbgenre_movie),2)
# Global scoring (method 1)
rcmd1_movies = cbind(movies[,1:3],RHTHscores=RHscores+THscores,MLgenrescores=MLgenrescores,globalscore1=round(RHscores + THscores + MLgenrescores ,2))
recommendation_globscore1 = rcmd1_movies[order(rcmd1_movies$globalscore1,decreasing=T),]
remove(RHmovieId,THmovieId,RHscores,THscores,predscores,MLgenrescores)
# User ratings
meanratings = aggregate(ratings[,"rating"], list(movieId=ratings$movieId), mean)
nbratings = aggregate(ratings[,"rating"], list(movieId=ratings$movieId), length)
ratingbymovie = merge(meanratings,nbratings,by="movieId")
names(ratingbymovie) = c("movieId","meanratings","nbratings")
# Global scoring (method 2)
rcmd2_movies = merge(rcmd1_movies,ratingbymovie,by="movieId",all.x=T)
rcmd2_movies$meanratings = round(rcmd2_movies$meanratings/5,2) # Rating normalization (between 0 and 1)
rcmd2_movies = cbind(rcmd2_movies,globalscore2=rcmd2_movies$globalscore1+rcmd2_movies$meanratings)
rcmd2_movies = rcmd2_movies[rcmd2_movies$nbratings>10,]
recommendation_globscore2 = rcmd2_movies[order(rcmd2_movies$globalscore2,decreasing=T),]
remove(meanratings,nbratings,ratingbymovie)
### Data saving
save.image(file="movielens_dataexplor.RData")

#################################################################
### ML-BASED RECOMMENDATION #####################################
#################################################################

#################################################################
#### cluster user by notes in each genre ########################
# Build a SQL-oriented version of 'genrematrix'
genrematrix = t(sapply(movies$genres,function(x){return( levels(genres) %in% strsplit(as.character(x), "[|]")[[1]] ) })) ; head(genrematrix)
colnames(genrematrix) = levels(genres)
rownames(genrematrix) = movies$movieId
genretab = data.frame(movieId = rep(as.numeric(rownames(genrematrix)), times = ncol(genrematrix)),
                      genres = rep(colnames(genrematrix), each = nrow(genrematrix)),
                      values = as.vector(genrematrix))
genretab = genretab[genretab$values,c("movieId","genres")]
genretab$movieId = as.numeric(genretab$movieId)
genretab = genretab[order(genretab$movieId),]
# Decompose 'ratings' to avoid out-of-memory issues
fullratingtab = data.frame(userId = rep(unique(ratings$userId),times=length(levels(genres))) , genres = rep(levels(genres),each=length(unique(ratings$userId))) )
ratings1 = ratings[which(ratings$userId<138493*0.25),]
ratings2 = ratings[which(ratings$userId>=138493*0.25&ratings$userId<138493*0.5),]
ratings3 = ratings[which(ratings$userId>=138493*0.5&ratings$userId<138493*0.75),]
ratings4 = ratings[which(ratings$userId>=138493*0.75),]
# Build a SQL-oriented version of 'ratings' that links ratings, users and movie genres
ratingbygenres1 = merge(ratings1,genretab,by="movieId") ; remove(ratings1)
ratingbygenres2 = merge(ratings2,genretab,by="movieId") ; remove(ratings2)
ratingbygenres3 = merge(ratings3,genretab,by="movieId") ; remove(ratings3)
ratingbygenres4 = merge(ratings4,genretab,by="movieId") ; remove(ratings4)
ratingtab1 = aggregate( rating~userId+genres, data=ratingbygenres1, FUN=mean, na.action=na.pass) ; str(ratingtab1) ; remove(ratingbygenres1)
ratingtab2 = aggregate( rating~userId+genres, data=ratingbygenres2, FUN=mean, na.action=na.pass) ; str(ratingtab2) ; remove(ratingbygenres2)
ratingtab3 = aggregate( rating~userId+genres, data=ratingbygenres3, FUN=mean, na.action=na.pass) ; str(ratingtab3) ; remove(ratingbygenres3)
ratingtab4 = aggregate( rating~userId+genres, data=ratingbygenres4, FUN=mean, na.action=na.pass) ; str(ratingtab4) ; remove(ratingbygenres4)
ratingtab = rbind(ratingtab1,ratingtab2,ratingtab3,ratingtab4) ; remove(ratingtab1,ratingtab2,ratingtab3,ratingtab4)
# Finalize table
ratingtabbygenre = merge(ratingtab,fullratingtab,all=T) # Correction on ratingtab, as some users have not watched movies in each genre
ratingtabbygenre = unstack(ratingtabbygenre,rating~genres)
ratingtabbygenre[is.na(ratingtabbygenre)]=0
ratingtabbygenre = ratingtabbygenre[,-1]
# Cluster users
wss = (nrow(ratingtabbygenre)-1)*sum(apply(ratingtabbygenre,2,var))
bss = 0
for (i in 2:15) {
  rat.kmeans = kmeans(ratingtabbygenre,centers=i)
  wss[i] = rat.kmeans$tot.withinss
  bss[i] = rat.kmeans$betweenss
}
# Sum of squares plotting
png(file="figs/Nbcluster_selection.png")
plot(wss, col="red", type="b", pch=3, xlab="Number of Clusters",ylim=c(0,4000000),xaxp=c(0,15,5),ylab="Sum of squares")
lines(bss,col="blue",type="b",pch=15)
legend("topright",c("Total within-cluster sum of squares","Total between sum of squares"),col=c("red","blue"),pch=c(3,15),cex=0.75)
abline(v=0,h=0)
abline(v=seq(3,15,3),lty="dotted")
dev.off()
# 6 seems to be the best number of cluster (take into account the second elbow)
rat.kmeans = kmeans(ratingtabbygenre,centers=6)
rat.kmeans$centers
png(file="figs/barplot_genre_ratings_bycluster.png")
barplot(rat.kmeans$centers[,which(colnames(rat.kmeans$center)%in%targettedgenresML)],beside=T,ylim=c(0,5),las=2,main="Mean ratings in each cluster")
legend("topright",paste("Cluster",1:6),pch=15,col=grey.colors(6),horiz=T,cex=0.75)
dev.off()
# install.library("plotrix") # Needed for 'radial.plot' function call
library("plotrix")
png(file="figs/radialplot_targettedgenres.png")
radial.plot(rat.kmeans$centers[,which(colnames(rat.kmeans$centers)%in%targettedgenresML)], labels=colnames(rat.kmeans$centers)[which(colnames(rat.kmeans$centers)%in%targettedgenresML)]
            , point.symbol=c(15,16,3,17,18,1), line.col=1:6, point.col=1:6, rp.type="ps"
            , start = pi/2, clockwise = TRUE, boxed.radial=F, radial.lim = c(2,4), lwd=3,cex=2,cex.lab=2)
legend("bottom",paste("Cluster",1:6),col=1:6,pch=151,horiz=T,cex=0.7,inset=-0.1)
dev.off()
png(file="figs/radialplot_othergenres.png")
radial.plot(rat.kmeans$centers[,which(!colnames(rat.kmeans$centers)%in%targettedgenresML)], labels=colnames(rat.kmeans$centers)[which(!colnames(rat.kmeans$centers)%in%targettedgenresML)]
            , point.symbol=c(15,16,3,17,18,1), line.col=1:6, point.col=1:6, rp.type="ps"
            , start = pi/2, clockwise = TRUE, boxed.radial=F, radial.lim = c(0,4.2), lwd=3,cex=2,cex.lab=2)
legend("bottom",paste("Cluster",1:6),col=1:6,pch=151,horiz=T,cex=0.7,inset=-0.15)
dev.off()
# Cluster 4 (this number may change if code is run again...!) is interesting to recommend a movie to somebody who've just watched 'Inferno' (14132 users over 138493)
# Look at the best ratings given by those users, on Action/Crime/Mystery/Sci-Fi/Thriller movies
ratings_k4 = ratings[ratings$userId %in% as.numeric(rownames(ratingtabbygenre))[rat.kmeans$cluster==4] , ]
movies_g = unique(genretab[genretab$genres%in%targettedgenresML,"movieId"])
ratings_k4_g = ratings_k4[ratings_k4$movieId %in% movies_g,]
meanratings_k4_g = as.data.frame(as.list(aggregate(rating~movieId, data=ratings_k4_g, FUN=function(x) c(mean = mean(x), n = length(x) ))))
meanratings_k4_g = meanratings_k4_g[meanratings_k4_g$rating.n>=100,]
meanratings_k4_g = merge(meanratings_k4_g,movies,by="movieId")
meanratings_k4_g = meanratings_k4_g[order(meanratings_k4_g$rating.mean,decreasing=T),]
head(meanratings_k4_g)
### Data saving #################################################
save.image(file="movielens_userclust.RData")

#################################################################
#### Frequent item mining #######################################
userrates = merge(ratings,movies,by="movieId")
userrates = userrates[order(userrates$userId),c("userId","movieId","title","rating")] ; str(userrates)
# install.packages("arules") # Needed for apriori algorithm use
library(arules)
# Configure the 'userrates' table so as to use it in apriori algorithm
userrates2 = as( split(as.vector(userrates$title), as.vector(userrates$userId)) , "transactions" )
# Compute the frequent movie associations such that prob(xANDy)>=0.005, prob(y|x)>=0.5 and look for highest prob(y|x)/prob(y) value
rules = apriori( userrates2 , parameter=list(supp=0.005, conf=0.5, target="rules", minlen=2, maxlen=2,maxtime=20) )
rules = sort(rules, by ="lift")
movieassociations = as(rules, "data.frame")
associations_ad = movieassociations[regexpr("Angels & Demons",movieassociations$rules)>0,] ; head(associations_ad,6)
associations_dvc = movieassociations[regexpr("Da Vinci Code",movieassociations$rules)>0,] ; head(associations_dvc,6)
### Data saving #################################################
save.image(file="movielens_itemmining.RData")


#################################################################
#### hierarchical clustering on movies by tag accuracy ##########
# Rebuild the tag genome (movieId wrt tags)
taggenome = unstack(genscores,relevance~tagId)
rownames(taggenome) = unique(genscores$movieId)
colnames(taggenome) = unique(gentags$tag)
# Compute the most accurate tags for "Angels & Demons
head(t(sort(taggenome[which(rownames(taggenome)==movies[which(movies$title=="Angels & Demons"),"movieId"]),],decreasing=T)),10)
# Hierarchical clustering
moviedist = dist(taggenome, method = "euclidean") # distance matrix (long to compute!)
moviefit = hclust(moviedist, method="ward")
# moviegroups = cutree(moviefit, k=5)
moviecophdist = cophenetic(moviefit) # Cophenetic distance (from an individual to the other)
moviecophmat = as.matrix(moviecophdist)
colnames(moviecophmat) = rownames(moviecophmat) = rownames(taggenome)
# function findsimilarmovie(title) : return the 'k' movies that are the most similar to 'title'
findsimilarmovie = function(title,k=5){
  if(! title %in% movies$title ){message("Movie not available");return(NA)}
  movieid = movies[which(movies$title==title),"movieId"]
  if(length(movieid)>1){movieid=movieid[1]}
  movieselection = movies[movies$movieId %in% rownames(taggenome) , ]
  if(! movieid %in% movieselection$movieId ){print("Movie not available");return(NA)}
  if( k >= nrow(movieselection) ){print("Please select a smaller k: k is larger than the movie selection size");return(NA)}
  simvect = moviecophmat[ which(rownames(taggenome)==movieid) , ]
  recommendedmovies = cbind(movieselection,simindex=unname(simvect))
  recommendedmovies = recommendedmovies[order(recommendedmovies$simindex),]
  # movies[ movies$movieId%in%names(head(sort(simvect),k)) , ]
  return( recommendedmovies[ 2:(k+1) , ] )
}
findsimilarmovie("Angels & Demons",10) # Find the 10 closest movies to 'Angels & Demons'
# Hierarcical clustering plotting
moviedendr = as.dendrogram(moviefit)
moviefitleaf = cut(as.dendrogram(moviefit),h=9)$lower # Cut at h=9 to keep the
listofleaf = lapply(moviefitleaf,order.dendrogram)
goodleafindex = which(unlist(lapply(listofleaf,function(x){which(rownames(taggenome)==movies[movies$title=="Angels & Demons","movieId"])%in%x})))
str(moviefitleaf[[goodleafindex]])
goodmoviedendrogram = moviefitleaf[[goodleafindex]]
# install.packages("dendextend") # Packages needed for clever dendrogram plotting
library("dendextend")
labels(goodmoviedendrogram) = gsub(":",":\n",movies[match(rownames(taggenome[order.dendrogram(moviefitleaf[[goodleafindex]]),]),movies$movieId),"title"])
postscript("figs/Dendrogramzoom_angels_demons.eps",horizontal=F,width=4,height=4)
par(mar=c(12,3,0,0))
goodmoviedendrogram %>% set("labels_col", c("black","black","black","red","black","black","black")) %>%
  set("labels" , gsub(":",":\n",movies[match(rownames(taggenome[order.dendrogram(moviefitleaf[[goodleafindex]]),]),movies$movieId),"title"])) %>%
  set("labels_cex", 1.5) %>% plot()
dev.off()
### Data saving #################################################
save.image(file="movielens_hclust.RData")
