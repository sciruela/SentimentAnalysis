library(twitteR)
library(stringr)
library(plyr)
library(ggplot2)
library(doBy)

    scores.sentiment <-function(sentence, pos.words, neg.words) {
      
        # and convert to lower case:
        sentence = tolower(sentence)
 
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
 
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
 
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
 
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
 
        return(score)
    }

 score.sentiment<-function(sentences,scores,allcities)
{
 
    scores.df <-data.frame(score=scores, text=sentences, onecity=allcities)
    return(scores.df)
}


pos.words=scan('/Users/sciruela/Documents/SentimentAnalysis/positive-words.txt',what='character',comment.char=';')
neg.words=scan('/Users/sciruela/Documents/SentimentAnalysis/negative-words.txt',what='character',comment.char=';')
pos.words=append(pos.words,':)')
neg.words=append(neg.words,':(')

cities<-c('#vancouver','#melbourne','#vienna','#toronto','#calgary','#helsinki','#sydney','#auckland','#perth','#adelaide','#colombo','#dakar','#tehran','#douala','#karachi','#algiers','#lagos','#portmoresby','#dhaka','#harare')

all.score<-c()

for (city in cities){
	city.tweets=searchTwitter(city,n=1500)
	tweet<-c()
	scores<-c()
	allcities<-c()
	for(i in 1:length(city.tweets)){
		scores<-append(scores,scores.sentiment(city.tweets[[i]]$getText
											   (),pos.words,neg.words))
		tweet<-append(tweet,city.tweets[[i]]$getText())
		allcities<-append(allcities,city)
	
}	

city.score<-score.sentiment(tweet,scores,allcities)
all.score=rbind(all.score,city.score)	
}



pdf("/Users/sciruela/Documents/SentimentAnalysis/all-cities.pdf")
ggplot(data=all.score)+geom_bar(aes(x=score,fill=onecity),binwidth=1)+facet_grid(onecity ~ .)+theme_bw()
dev.off()


all.score$very.pos=as.numeric(all.score$score>=1)
all.score$very.neg=as.numeric(all.score$score<=-1)

twitter.df=ddply(all.score,c('onecity'),summarise,pos.count=sum(very.pos),neg.count=sum(very.neg))

twitter.df$all.count=twitter.df$pos.count+twitter.df$neg.count
twitter.df$score=round(100*twitter.df$pos.count/twitter.df$all.count)
orderBy(~-score,twitter.df)


theeconomist.df<-c()
theeconomist.df$onecity<-c('#vancouver','#melbourne','#vienna','#toronto','#calgary','#helsinki','#sydney','#auckland','#perth','#adelaide','#colombo','#dakar','#tehran','#douala','#karachi','#algiers','#lagos','#portmoresby','#dhaka','#harare')
theeconomist.df$score<-c(98.0,97.5,97.4,97.2,96.6,96.2,96.1,95.7,95.1,95.0,48.5,48.3,45.8,44.0,40.9,39.4,39.0,38.9,38.7,37.5)


compare.df=merge(twitter.df,theeconomist.df,by='onecity',suffixes=c('.twitter','.theeconomist'))

compare.df=subset(compare.df,all.count>100)




pdf("/Users/sciruela/Documents/SentimentAnalysis/compare-cities.pdf")
ggplot(compare.df)+geom_point(aes(x=score.twitter,y=score.theeconomist,color=onecity),size=5)+geom_smooth(aes(x=score.twitter,y=score.theeconomist,group=1),se=F,method="lm")+theme_bw()+stat_smooth(aes(x=score.twitter,y=score.theeconomist))
dev.off()




pdf("/Users/sciruela/Documents/SentimentAnalysis/compare-point-cities.pdf")
ggplot(compare.df, aes(score.twitter, score.theeconomist, colour = onecity)) + geom_point()
dev.off()

ej<-aov(compare.df$score.twitter ~ compare.df$score.theeconomist)

summary(ej)

compare.res = compare.df
compare.res$M1.Fit = fitted(ej)
compare.res$M1.Resid = resid(ej)



pdf("/Users/sciruela/Documents/SentimentAnalysis/compare-residuals-fitted-cities.pdf")
ggplot(compare.res, aes(M1.Fit, M1.Resid, colour=onecity))+geom_point()+xlab('Fitted Values')+ylab('Residuals')
dev.off()



pdf("/Users/sciruela/Documents/SentimentAnalysis/compare-residuals-fitted-cities2.pdf")
ggplot(compare.res, aes(M1.Fit, M1.Resid, colour=onecity))+geom_point()+xlab('Fitted Values')+ylab('Residuals')+facet_wrap(~ onecity)
dev.off()



pdf("/Users/sciruela/Documents/SentimentAnalysis/compare-residuals-fitted-cities3.pdf")
ggplot(compare.res,aes(sample=M1.Resid),colour=onecity)+stat_qq()
dev.off()
