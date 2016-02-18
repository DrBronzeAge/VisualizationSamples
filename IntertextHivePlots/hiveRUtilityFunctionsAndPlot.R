#Housekeeping

library(HiveR)
library(grid)
library(RColorBrewer)
setwd("~/Python Scripts")

#some convenience functions, since HiveR's hive data structure is neither intuitive nor convenient
#to use and modify

resetRads<-function(hiveObject){
	#The hive data structure wants nodes to have a value for radius
	#this sets the radius of a node to be equal to it's position in the text.
	#i.e. the first sentence is at one end of the axis, and the last one is at the other.
  labs<-hiveObject$nodes$lab
  labs<-gsub('  ',' ',labs) #not sure where these extra spaces are coming from, but they're a pain
  rads<-c()
  for (i in 1:length(labs)){
    rads[i]<-as.numeric(unlist(strsplit(labs[i],' '))[2])
  }
  hiveObject$nodes$radius<-rads
  return(hiveObject)
}


MakeEdgeWeights<-function(wordsvector,threshold=3){
	#weight edges by how similar the phrases are (how many words in common)
  end<-length(wordsvector)
  ews<-c()
  for (i in 1:end){
    wds<-as.character(wordsvector[i])
    ews[i]<-length(unlist(strsplit(wds,'  ')))-(threshold-1)
  }
  return(ews)
}

colourEdgeByWord<-function(colour,word,wordvector,edgevector){
	#Function to change the colour of some edges, let's you call out
	#all intertexts that contain a specific term
  key<-grepl(word,wordvector)
  edgevector[key]<-colour
  return(edgevector)
}

resetAxes<-function(hiveObject){
	#Function to ensure that each speech/document is assigned
	#to a different axis
  labs<-hiveObject$nodes$lab
  speeches<-c()
  for (i in 1:length(labs)){
    speeches[i]<-unlist(strsplit(labs[i],' '))[1]
  }
  uniSpeech<-unique(speeches)
  for (i in 1:length(uniSpeech)){
    key<-grepl(uniSpeech[i],labs)
    hiveObject$nodes$axis[key]<-i
  }
  return(hiveObject)
}

WeightNodesByEdges<-function(hiveObject){
	#Lets you assign node weights based on the number of edges connected to it
  for (i in 1: length(hiveObject$nodes$id)){
    hiveObject$nodes$size[i]<-length(hiveObject$edges$color[hiveObject$edges$id1==i|hiveObject$edges$id2==i])
    
  }
  return(hiveObject)
}

#Below here is the code to actually make plots


#these are the edge lists produced by comparing several speeches
#of Cicero against the 4 Catilinarian Orations.
#We're mostly interested in the De Domo Sua (dd), the others
#are a control group
prs<-read.csv('prs_edgematrix4.csv')
prq<-read.csv('prq_edgematrix4.csv')
har<-read.csv('har_edgematrix4.csv')
dd<-read.csv('dd_edgematrix4.csv')

#collect them into a single table
em<-rbind(prs,prq)
em<-rbind(em,har)
em<-rbind(em,dd)

#clear out a column we don't need
em<-em[,2:6]

#a smaller one for the plot we eventually publish
em2d<-rbind(dd,har)


#Use this to make 3d hive plots.  Kind of cool, but not
#practical for powerpoint presentations.

fooem<-edge2HPD(edge_df = em,type="3D")


#now clean it up a bit...

#MakeEdgeWeights(em$WordsInCommon,3)
fooem<-resetAxes(fooem)

fooem<-resetRads(fooem)
#colourEdgeByWord('Navy','public',em$WordsInCommon,fooem$edges$color)
fooem$edges$color<-colourEdgeByWord('Navy','sceler',em$WordsInCommon,fooem$edges$color)
fooem$edges$color<-colourEdgeByWord('Orange','consu',em$WordsInCommon,fooem$edges$color)
#fooem$edges$color<-colourEdgeByWord('Cyan','conloc',em$WordsInCommon,fooem$edges$color)
fooem$edges$color<-colourEdgeByWord('Cyan','co[nl]loc',em$WordsInCommon,fooem$edges$color)


fooem$edges$weight<-MakeEdgeWeights(em$WordsInCommon,3)

# An interactive 3d plot with several additional 'control' speeches.
#Looked neat, but wasn't practical for use in powerpoint presentation.

plot3dHive(fooem, ch=1,axLabs = c('Cat','Post Red. S','Post Red. Q.','Haruspicum','De Domo'))


##########################################################################################333333
#The 2d hiveplot that was actually used
#############################################################################################

#went through several colour palettes with the client...
#themcols<-brewer.pal(8,'Accent')
themcols<-brewer.pal(8,'Dark2')
#themcols<-brewer.pal(8,'Spectral')

#build the data structure HiveR wants
dh2<-edge2HPD(edge_df = em2d[,2:6], type="2D")


#now clean it up to look nice
dh2<-resetAxes(dh2)
dh2<-resetRads(dh2)
dh2$nodes$radius[dh2$nodes$radius<=0]<-1
dh2$edges$color<-themcol[8]
#dh2$edges$color<-colourEdgeByWord(themcols[3],'caed',em2d$WordsInCommon,dh2$edges$color)
dh2$edges$color<-colourEdgeByWord(colours()[645],'meus',em2d$WordsInCommon,dh2$edges$color)
dh2$edges$color<-colourEdgeByWord(colours()[73],'consilium',em2d$WordsInCommon,dh2$edges$color)

#change the colour of the nodes 
nodes<-dh2$nodes
edges<-dh2$edges
lv<-grepl('Cat',dh2$nodes$lab)
lv2<-grep('Domo',dh2$nodes$lab)
lv3<-grep('Harus',dh2$nodes$lab)
dh2$nodes$color[lv]<-themcols[3]

#call out one section of the Catilinarians
dh2$nodes$color[which(dh2$nodes$radius<428 & lv)]<-themcols[4]
dh2$nodes$color[which(dh2$nodes$radius<310 & lv)]<-themcols[3]
dh2$nodes$color[lv2]<-themcols[5]
dh2$nodes$color[lv3]<-themcols[1]

#a couple more modifications...
dh2<-WeightNodesByEdges(dh2)
dh2$nodes$size<-sqrt(dh2$nodes$size)
dh2$edges$weight<-MakeEdgeWeights(em2d$WordsInCommon)
dh2$edges$weight<-dh2$edges$weight*1.5

#and finally make the plot

plotHive(dh2,ch=0.08, method='norm', bkgnd='white',axLabs=c('In Catilinam 1-4','De Domo','Haruspicum \nResponsis'),
         axLab.gpar=gpar(col='Black',cex=1.1,fontface='italic',fontfamily='A'),axLab.pos = c(.2,.2,.3))
