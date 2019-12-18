library(data.table)
library(ChannelAttribution)

#Simulate Weborama logs

#Data size
N<-1000000

Base<-data.table(cookie=sample(1:(N/10),N,replace = T),
                 
                 date=seq(from=as.POSIXct("2017-01-01"), to=as.POSIXct("2017-01-31"),length.out = N),
                 
                 action=sample(c(rep("conversion",N*0.01),
                                 rep("impression",N*0.85),
                                 rep("click",N*0.14)),N,replace = F),
                 
                 channel=sample(c(rep("display_retargeting",N*0.2),
                                  rep("display_branding",N*0.1),
                                  rep("display_prospecting",N*0.2),
                                  rep("sea",N*0.1),
                                  rep("seo",N*.3),
                                  rep("emailing",N*0.1)),N,replace = F))

Base<-Base[order(cookie,date),]

#create/Add row ID
Base$Row_ID<-1:N

rm(N)

#Remove touchpoints after first conversion
First_Conv<-Base[action=="conversion",]
First_Conv<-First_Conv[!duplicated(cookie),c("cookie","Row_ID"),with=F]
Base<-merge(Base,First_Conv,by="cookie",all.x = T)
Base[is.na(Row_ID.y),Row_ID.y:=Row_ID.x]
Base<-Base[!Row_ID.x>Row_ID.y,]

rm(First_Conv)

#Remove conversions without touchpoints
Touchless_conversions<-Base[!duplicated(cookie),][action =="conversion",cookie]
Base<-Base[!cookie %in% Touchless_conversions,]

rm(Touchless_conversions)

#Get channel Presence
channel_Presence<-Base[cookie %in% Base[action == "conversion",cookie],][!action=="conversion",c("cookie","channel"),with=FALSE]
channel_Presence<-as.data.frame(table(channel_Presence[!duplicated(channel_Presence),channel]))
colnames(channel_Presence)<-c("channel_name","Presence")

#Transform conversion data into path sequence
conv<-Base[ cookie%in% Base[action=="conversion",cookie],c("cookie","channel"),with=FALSE][!channel=="conversion",] 
Ind <- cumsum(rle(as.character(paste(conv$cookie,conv$channel)))$length)
conv<-conv[Ind, ]
conv<-as.data.frame(tapply(conv$channel,conv$cookie,function(x) paste(x,collapse = " ==> "),simplify = T))
conv<-as.data.frame(table(conv))

conv<-conv[order(conv$Freq,decreasing = T),]
conv$Share<-round(100*conv$Freq/sum(conv$Freq),2)

#Transform Non-Conversion data into path sequence
non_conv<-Base[ !cookie%in% Base[action=="conversion",cookie],c("cookie","channel"),with=FALSE] 
Ind <- cumsum(rle(as.character(paste(non_conv$cookie,non_conv$channel)))$length)
non_conv<-non_conv[Ind, ]
non_conv<-as.data.frame(tapply(non_conv$channel,non_conv$cookie,function(x) paste(x,collapse = " ==> "),simplify = T))
non_conv<-as.data.frame(table(non_conv))

non_conv<-non_conv[order(non_conv$Freq,decreasing = T),]
non_conv$Share<-round(100*non_conv$Freq/sum(non_conv$Freq),2)

#Combine all paths for Markov modelling
Path<-merge(conv[,!(names(conv)=="Share")],non_conv[,!(names(non_conv)=="Share")],by.x = "conv",by.y = "non_conv",all = T)
colnames(Path)<-c("path","total_conversions","total_null")
Path$total_conversions[is.na(Path$total_conversions)]<-0
Path$total_null[is.na(Path$total_null)]<-0

rm(Ind,conv,non_conv)

#Get contribution with Markov Chain
set.seed(1234)

Train<-Path
Train$path<-gsub("==>",">",Train$path)

Results<-Reduce(function(...) merge(...,by="channel_name",suffixes = c(".1",".2"), all=TRUE), 
       list(channel_Presence,
            as.data.frame(heuristic_models(Train,"path","total_conversions")),
            as.data.frame(markov_model(Train, "path", "total_conversions",var_null="total_null",out_more=T,order=1)$result),
            as.data.frame(markov_model(Train, "path", "total_conversions",var_null="total_null",out_more=T,order=2)$result),
            as.data.frame(markov_model(Train, "path", "total_conversions",var_null="total_null",out_more=T,order=3)$result)))
names(Results)[6:8]<-c("Marcov_1st_Order","Marcov_2nd_Order","Marcov_3rd_Order")

Results

rm(channel_Presence,Train,Base)


#Plot results
plot_data <- melt(Results[,c(1,3:6)], id='channel_name')

ggplot(plot_data, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL VALUE') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")
