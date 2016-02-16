library(ggplot2)
library(tidyr)
#setting up some initial values
currentProb = 60
obamaNomAvg=(currentProb/100)*90-5
likelyMin=(currentProb/100)*90-10
likelyMax=(currentProb/100)*90
#setting up our payoff matrix for the Republicans
reps=matrix(0,101,101)
for(x in 1:101)
  for(y in 1:101)
    ##this is an algebraic reduction of the Republicans' relative payoff structure, where their payoff bonus is equal to the payoff for approving an Obama nominee (where their payoff is 100-Obama Nominee's Liberal Score, 100-y) minus the expected value of the election.
    ##100-y-((10*prob/100)+(1-prob/100)*100)=100-y-0.1prob-100+prob=0.9prob-y
    reps[x,y]= (0.9*(x-1)-(y-1))
repframe=data.frame(reps)
repframe$probability=c(0:100)
repframe=gather(repframe, key=probability,value=payoffs, 1:101)
colnames(repframe)[2]='obama'
n=nrow(repframe)
#When we converted from matrix to dataframe, the matrix columns (probability scores) were converted to weird factor strings with an X in front (e.g. X3) because data.frames doesn't like integer column titles. We now need to fix this
repframe$obama=as.character(repframe$obama)
for(i in 1:n)
  repframe$obama[i]=substr(repframe$obama[i],2,4)
repframe$obama=as.numeric(repframe$obama)-1
summary(repframe) ##check- we should have obama and probability evenly distributed
##now we make our graph
##probabilitydata
probdata=ggplot(repframe, aes(probability, obama)) +
  geom_raster(data=repframe, aes(fill = payoffs), interpolate = F)+
#   scale_fill_gradientn(colours=c("#452ADF","#FFFFFF","#CB152D"), +
  scale_fill_gradient2(low = "#452ADF", mid = "#FFFFFF", high = ("#CB152D"), midpoint = 0, name="Relative Payoff\nfor Republicans", guide = "colourbar",limits=c(-100,100))+
  theme(legend.text = element_text(size = 16),plot.title=element_text(size = 22),axis.title = element_text(size = 20))+
  xlab("Probability of a Democratic Victory in the Presidential Election") + ylab("Liberality of Obama's SCOTUS Nominee") + # Set axis labels
  ggtitle("Payoffs for Republicans to Approve Obama Nominee\nRather than Wait for the Election") + #set Obama's choice parameters
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=likelyMin, ymax=likelyMax),colour="#D1D1D1",fill="#FFFFFF", alpha=1/50, inherit.aes = FALSE)+
  geom_point(aes(x=currentProb,y=obamaNomAvg),fill="#452ADF", shape=1, size=3)
##see what we're working with
probdata
