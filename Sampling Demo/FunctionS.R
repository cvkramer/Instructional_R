library(plyr)
library(gridExtra)

#Takes a 1D vector of data and a sample size
systematic_sample <- function(data,smp.sz,sys.chg=3){             #Takes a 1D vector of data and a sample size                                                           
  out <- rep(NA,smp.sz)                                           #initilize output vector
  start <- sample(length(data),1)                                 #find 1st obs  
  list <- seq(from=start, to=sys.chg*(smp.sz-1)+start,by=sys.chg)   #initilize output vector
  
  for(i in 1:smp.sz){                                   #initilize output vector
    while(list[i]>length(data)){
      list[i] <- list[i]-length(data)
    }
  } 
  for(i in 1:smp.sz){                                   #initilize output vector
    out[i] <- data[list[i]]
  }
  return(out)
}

mround<-function(x,base,floor=0){
  if(floor==0){return(base*floor(min(x)/base))}
  else{ base*ceiling(max(x)/base)  }
}

gen_plot <- function(data=dat[,3], name="Population", iylim=c(0,.10)){
  h<-hist(data, freq=fflag, ylim=iylim, breaks=seq(0,xmax,1), main=name, xlab="Age in Years");
  text(8,.1/10*9,paste("mean = ",mean(data)))
  text(8,.1/10*8,paste(" std = ",round(sd(data),2)))
}

gen_meanplot <- function(data,name,iylim=c(0,.25)){
  h<-hist(data, freq=fflag, ylim=iylim, xlim=c(30,50), breaks=seq(0,xmax,brk), main=name, xlab="Mean Age of Sample Groups");
     #xlim=c(mean(data)-4*sd(data),mean(data)+4*sd(data))
  if(prob.den==TRUE){lines(d.x,col="red")};
  text(30,iylim[2]*9/10,adj=c(0,NA), paste("mean = ",mean(data)))
  text(30,iylim[2]*8/10,adj=c(0,NA), paste("std     = ",round(sd(data),4)))
}

gen_gg <- function(data,name,pop=data){
  pop <- data.frame(age=pop)
  smp <- data.frame(age=data)
  
  count(data)
  pop$src <- 'pop'
  smp$src <- 'smp'
  totl <- rbind(pop,smp)
  gg<-ggplot(totl, aes(age, fill = src)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',binwidth=1) + labs(title=name)
  return(gg)
}

graphs <- function(in_name="temp",tofile=FALSE){
##########################      ||  MEAN          ##############################################
tquit <- 'a'
if(tofile){pdf(paste(file="./Sampling Demo/Plots/",in_name,".pdf",sep=""), paper="letter")}
par(mfrow=c(3,1))

#Samples without Replacement
gen_meanplot(mage_1,paste(reps[1],"Samples Without Replacement"))
gen_meanplot(mage_2,paste(reps[2],"Samples Without Replacement"))
gen_meanplot(mage_3,paste(reps[3],"Samples Without Replacement"))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}
if(tquit=='q'){stopifnot(tquit!='q')}
stopifnot(tquit!='q')
#Samples with Replacement
gen_meanplot(mage_1r,paste(reps[1],"Samples with Replacement"))
gen_meanplot(mage_2r,paste(reps[2],"Samples with Replacement"))
gen_meanplot(mage_3r,paste(reps[3],"Samples with Replacement"))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}

#Samples with Systematic
gen_meanplot(mage_1s,paste(reps[1]," Systematic Sample : (", sys.chg, ")",sep=""),c(0,.5))
gen_meanplot(mage_2s,paste(reps[2]," Systematic Sample : (", sys.chg, ")",sep=""),c(0,.5))
gen_meanplot(mage_3s,paste(reps[3]," Systematic Sample : (", sys.chg, ")",sep=""),c(0,.5))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}

#Statistics
grid.newpage()
grid.table(means)
grid.text("Means", x=.5, y=.65, rot=0,gp=gpar(fontsize=20, col="Black"))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
grid.newpage()
grid.table(sds)
grid.text("Standard Deviations", x=.5, y=.65, rot=0,gp=gpar(fontsize=20, col="Black"))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}

#population vs sample plots
par(mfrow=c(2,1))
gen_plot()
gen_plot(vals_1,  "100 Samplings Without Replacement")
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_1s, "100 Samplings With Replacement")
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_1r, paste(reps[1]," Systematic Sample : (", sys.chg, ")",sep=""))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_2,  "1000 Samplings Without Replacement")
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_2s, "1000 Samplings With Replacement")
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_2r, paste(reps[2]," Systematic Sample : (", sys.chg, ")",sep=""))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_3,  "10000 Samplings Without Replacement")
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_3s, "10000 Samplings With Replacement")
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
gen_plot()
gen_plot(vals_3r, paste(reps[3]," Systematic Sample : (", sys.chg, ")",sep=""))
if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}

#par(mfrow=c(1,1))
#print(gen_gg(vals_1,  paste(reps[1],"Samplings Without Replacement")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_1s, paste(reps[1],"Samplings With Replacement")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_1r, paste(reps[1]," Systematic Sample : (", sys.chg, ")",sep="")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_2,  paste(reps[2],"Samplings Without Replacement")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_2s, paste(reps[2],"Samplings With Replacement")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_2r, paste(reps[2]," Systematic Sample : (", sys.chg, ")",sep="")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_3,  paste(reps[3],"Samplings Without Replacement")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_3s, paste(reps[3],"Samplings With Replacement")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}
#print(gen_gg(vals_3r, paste(reps[3]," Systematic Sample : (", sys.chg, ")",sep="")));if(tofile==FALSE){tquit<-readline("[Enter] to continue: ")}; if(tquit=='q'){stopifnot(tquit!='q')}

if(tofile){dev.off()}
if(tofile==FALSE){par(mfrow=c(1,1))}
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
