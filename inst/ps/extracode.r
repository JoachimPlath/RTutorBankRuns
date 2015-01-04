#++++++ Aditional Code +++++++


# Exercise1:
# Overwrite
describe <- prettyR::describe
select <- dplyr::select

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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

# Function for summarizing mean,sd and length
# Makes a summary looking nicer
showStats=function(x) {
  c(mean=round(mean(x),3),sd=round(sd(x),3),observations=as.integer(length(na.omit(x))))
}


# Function for performing a t-test on a grouped sample.
# The grouping-variable should be binary
# Use this function only for the given purpose.
TTest=function(x,y) unlist(t.test(x~y)[c("estimate","p.value","statistic")])

prepare=function(data=data,group=group) {
  
  dd=ddply(data,group,colwise(mean)) #hat aufsteigend sortiert nach group!!
  dd=t(dd[-1])
  #m1 
  out1=data.frame(x=dd[,1]) # runner
  out2=data.frame(x=dd[,2]) # stayer
  out=rbind(out1,out2) # zuerst runner, dann stayer
  out=data.frame(avg=out,name=rep(rownames(out1),2),runner=c(rep(0,length(data)-1),rep(1,length(data)-1)))
  colnames(out)=c("mean","variable","runner")
  out[,3]=factor(out[,3])
  out=na.omit(out)
  out=data.frame(out)
  
}


drop.predictors=function(binReg,clustervar1=NULL,show.drops=TRUE) {
  # X is an Object of glm
  if(family(binReg)[1]!="binomial") {
    stop("function needs glm object of the binomnial family")
  }
  X=model.matrix(binReg)

  # ?nderung: 24.11 clustervariables dazu
  if(is.null(clustervar1) == FALSE) {
    # evtl. eigene Fehlermeldung, falls clustervar gar nicht teil des model.frame ist
    # https://stat.ethz.ch/pipermail/r-help/2006-September/113533.html
    X=cbind(X,clustervar1=eval(binReg$call$data)[,clustervar1]) # get orginal dataset
  }
  
  if("(Intercept)" %in% colnames(X)) {
    X=X[,which(colnames(X) != "(Intercept)")] #entferne Intercept
  }
  X=cbind(model.frame(binReg)[,1],X) #Achtung: der Regressor muss dazu!!!!
  colnames(X)[1]=names(model.frame(binReg)[1])
  
  
  
  X=X[order(X[,1],decreasing=T),] # vorraussetzung: Regressor steht in 1.Spalte
  # ?nderung:06.09.2014
  temp1= apply(X,2,function(x)length(table(x))==2||length(table(x))==1) # Vektor mit Name und boolschem Ausdruck,ob Dummy oder nicht bzw. Konstanten
  tempX=names(temp1[temp1==FALSE])# merke nicht-Dummys
  temp1=names(temp1[temp1==TRUE])
  X_dummy=X[,temp1] # nehme nur die Spalten, deren ?berschriften den Dummy's entsprechen;Regrossor dabei!
  
  # 1.Schritt: teile Dummy's auf
  if(!is.null(dim(X_dummy))) {
    X_separated1=X_dummy[X_dummy[,1]==1,] # ich will die Matrix ja horizontal durchschneiden
    X_separated0=X_dummy[X_dummy[,1]==0,]
  }else{ # es muss mindestens eine Weitere bin?r-Variable au?er dem Regressor geben,sonst keine pP m?glich
    stop("there must be at least 1 binary depentant variable")
  }
  # 2.Schritt: pr?fe auf perfect prediction
  sum1=colSums(X_separated1) # hole Spaltensumme
  sum0=colSums(X_separated0)
  # f?r die Elemente muss ?berpr?ft werden:((sum0==0 && sum1>0) || (sum1==0 && sum0>1))
  proof1=cbind(sum1,sum0) # P?fmatrix, ob prerfekte Vorhersage gegeben
  sumX=sum0*sum1 # Trick: wenn ich einmal 0 hab in sum1 oder sum0, ist pP gefunden
  temp1=(sumX==0)# perfect predictor
  tempX=c(all.vars(formula(binReg))[1],tempX,names(temp1[temp1==FALSE])) # Achtung:Regressor in beiden Mengen
  temp1=names(temp1[temp1==TRUE]) #name der pp's
  
  if(all.vars(formula(binReg))[1] %in% temp1) {
    temp1=temp1[which(temp1!=all.vars(formula(binReg))[1])] # der Regressor muss weg
  }
  # ?nderung 16.10.14 : Versch?nerte Ausgabe
  #cat("omitted factors:","\n",temp1)
  
  # ?nderung 28.10.2014: Fallunterscheidung: wenn nur 1 Element in temp1
  if(length(temp1)>1){
    X[,colnames(X)%in%temp1]=ifelse(X[,colnames(X)%in%temp1]==1,NA,X[,colnames(X)%in%temp1])
  }else{
    X[,temp1]=ifelse(X[,temp1]==1,NA,X[,temp1])
  }
  
  X=na.omit(X) # l?sche Zeilen die NA's enthalten
  
  if(show.drops) {
  # ?nderung 16.10.14: versch?nerte Ausgabe
  cat("dropped variables:","\n")
  # ?nderung 22.10.14: Fallunterscheidung; wenn nur 1 Element, wird beschriftung vergessen!
  if(length(temp1)>1) {
    print(proof1[(rownames(proof1) %in% temp1),])
  }else {
    cat(temp1,proof1[temp1,],"\n")
  } 
  cat("total sum : ",length(model.matrix(binReg)[,1])-length(X[,1]))
  }
  
  
  X=X[,tempX] # l?sche Spalten der perfect predictors
  x=data.frame(X)
}


binary.glm=function(formula,link,data,clustervar1=NULL,show.drops=FALSE) {
  
  
  
  if(class(formula) != "formula") {
    stop("formula must be of type formula")
  }
  # Zuschneidung der ben?tigten Spalten
  dataX=data[,c(all.vars(formula),clustervar1)]
  dataX=na.omit(dataX)
  # es werden alles spalten ausgew?hlt, Spaltenname passt auch
  
  # 1. Schritt: glm wird ausgefuehrt um faktoren zu erhalten
  if(link=="probit") {
    binReg=glm(formula,family=binomial(link="probit"),data=dataX,na.action=na.omit)
  }else if(link=="logit") {
    binReg=glm(formula,family=binomial(link="logit"),data=dataX,na.action=na.omit)
  } else {
    stop("no valid link-function given")
  }
  
  X=model.matrix(binReg)
  # ?nderung: 24.11 clustervariables dazu
  if(is.null(clustervar1) == FALSE) {
    # evtl. eigene Fehlermeldung, falls clustervar gar nicht teil des model.frame ist
    # https://stat.ethz.ch/pipermail/r-help/2006-September/113533.html
    X=cbind(X,clustervar1=eval(binReg$call$data)[,clustervar1]) # get orginal dataset
  }
  
  if("(Intercept)" %in% colnames(X)) {
    X=X[,which(colnames(X) != "(Intercept)")] #entferne Intercept
  }
  X=cbind(model.frame(binReg)[,1],X) #Achtung: der Regressor muss dazu!!!!
  colnames(X)[1]=names(model.frame(binReg)[1])
  
  
  
  X=X[order(X[,1],decreasing=T),] # vorraussetzung: Regressor steht in 1.Spalte
  # ?nderung:06.09.2014
  temp1= apply(X,2,function(x)length(unique(x))==2||length(unique(x))==1) # Vektor mit Name und boolschem Ausdruck,ob Dummy oder nicht bzw. Konstanten
  tempX=names(temp1[temp1==FALSE])# merke nicht-Dummys
  temp1=names(temp1[temp1==TRUE])
  X_dummy=X[,temp1] # nehme nur die Spalten, deren ?berschriften den Dummy's entsprechen;Regrossor dabei!
  
  # 1.Schritt: teile Dummy's auf
  if(!is.null(dim(X_dummy))) {
    X_separated1=X_dummy[X_dummy[,1]==1,] # ich will die Matrix ja horizontal durchschneiden
    X_separated0=X_dummy[X_dummy[,1]==0,]
  }else{ # es muss mindestens eine Weitere bin?r-Variable au?er dem Regressor geben,sonst keine pP m?glich
    stop("there must be at least 1 binary depentant variable")
  }
  # 2.Schritt: pr?fe auf perfect prediction
  sum1=colSums(X_separated1) # hole Spaltensumme
  sum0=colSums(X_separated0)
  # f?r die Elemente muss ?berpr?ft werden:((sum0==0 && sum1>0) || (sum1==0 && sum0>1))
  proof1=cbind(sum1,sum0) # P?fmatrix, ob prerfekte Vorhersage gegeben
  sumX=sum0*sum1 # Trick: wenn ich einmal 0 hab in sum1 oder sum0, ist pP gefunden
  temp1=(sumX==0)# perfect predictor
  tempX=c(all.vars(formula(binReg))[1],tempX,names(temp1[temp1==FALSE])) # Achtung:Regressor in beiden Mengen
  temp1=names(temp1[temp1==TRUE]) #name der pp's
  
  if(all.vars(formula(binReg))[1] %in% temp1) {
    temp1=temp1[which(temp1!=all.vars(formula(binReg))[1])] # der Regressor muss weg
  }
  # ?nderung 16.10.14 : Versch?nerte Ausgabe
  #cat("omitted factors:","\n",temp1)
  
  # ?nderung 28.10.2014: Fallunterscheidung: wenn nur 1 Element in temp1
  if(length(temp1)>1){
    X[,colnames(X)%in%temp1]=ifelse(X[,colnames(X)%in%temp1]==1,NA,X[,colnames(X)%in%temp1])
  }else{
    X[,temp1]=ifelse(X[,temp1]==1,NA,X[,temp1])
  }
  
  X=na.omit(X) # l?sche Zeilen die NA's enthalten
  
  if(show.drops) {
    # ?nderung 16.10.14: versch?nerte Ausgabe
    cat("dropped variables:","\n")
    # ?nderung 22.10.14: Fallunterscheidung; wenn nur 1 Element, wird beschriftung vergessen!
    if(length(temp1)>1) {
      print(proof1[(rownames(proof1) %in% temp1),])
    }else {
      cat(temp1,proof1[temp1,],"\n")
    } 
    cat("total sum : ",length(model.matrix(binReg)[,1])-length(X[,1]))
  }
  
  
  X=X[,tempX] # l?sche Spalten der perfect predictors
  X=data.frame(X)
  if(is.null(clustervar1)==FALSE) {
  colnames(X)[colnames(X) == "clustervar1"] =clustervar1 # to get the real name not clustervar1
  # regress all but not the clustervariables!!!!
  f=paste(all.vars(formula(binReg))[1],"~.","-",clustervar1)
  } else {
    f=paste(all.vars(formula(binReg))[1],"~.")
  }
  f=as.formula(f)
  regX=glm(f,data=X,family=binomial(link="probit"),na.action=na.omit)
  
}


# 
# binary.glm.new=function(formula,link,data,clustervar1=NULL,show.drops=FALSE) {
#   
#   if(class(formula) != "formula") {
#     stop("formula must be of type formula")
#   }
#     
#   if(!link %in% c("probit","logit")) {
#     stop("Link must be 'logit' or 'probit'")
#   }
# 
#   # Zuschneidung der ben?tigten Spalten
#   has.na = is.na(data[,c(all.vars(formula)))
#   
#   subset = which(!has.na)
#   # 1. Schritt: glm wird ausgefuehrt um faktoren zu erhalten
#   X = model.matrix(formula, data=data[subset,])
#   
#   if("(Intercept)" %in% colnames(X)) {
#     X=X[,which(colnames(X) != "(Intercept)")] #entferne Intercept
#   }
#   X=cbind(model.frame(binReg)[,1],X) #Achtung: der Regressor muss dazu!!!!
#   colnames(X)[1]=names(model.frame(binReg)[1])
#   
#   
#   
#   X=X[order(X[,1],decreasing=T),] # vorraussetzung: Regressor steht in 1.Spalte
#   # ?nderung:06.09.2014
#   temp1= apply(X,2,function(x)length(unique(x))==2||length(unique(x))==1) # Vektor mit Name und boolschem Ausdruck,ob Dummy oder nicht bzw. Konstanten
#   tempX=names(temp1[temp1==FALSE])# merke nicht-Dummys
#   temp1=names(temp1[temp1==TRUE])
#   X_dummy=X[,temp1] # nehme nur die Spalten, deren ?berschriften den Dummy's entsprechen;Regrossor dabei!
#   
#   # 1.Schritt: teile Dummy's auf
#   if(!is.null(dim(X_dummy))) {
#     X_separated1=X_dummy[X_dummy[,1]==1,] # ich will die Matrix ja horizontal durchschneiden
#     X_separated0=X_dummy[X_dummy[,1]==0,]
#   }else{ # es muss mindestens eine Weitere bin?r-Variable au?er dem Regressor geben,sonst keine pP m?glich
#     stop("there must be at least 1 binary depentant variable")
#   }
#   # 2.Schritt: pr?fe auf perfect prediction
#   sum1=colSums(X_separated1) # hole Spaltensumme
#   sum0=colSums(X_separated0)
#   # f?r die Elemente muss ?berpr?ft werden:((sum0==0 && sum1>0) || (sum1==0 && sum0>1))
#   proof1=cbind(sum1,sum0) # P?fmatrix, ob prerfekte Vorhersage gegeben
#   sumX=sum0*sum1 # Trick: wenn ich einmal 0 hab in sum1 oder sum0, ist pP gefunden
#   temp1=(sumX==0)# perfect predictor
#   tempX=c(all.vars(formula(binReg))[1],tempX,names(temp1[temp1==FALSE])) # Achtung:Regressor in beiden Mengen
#   temp1=names(temp1[temp1==TRUE]) #name der pp's
#   
#   if(all.vars(formula(binReg))[1] %in% temp1) {
#     temp1=temp1[which(temp1!=all.vars(formula(binReg))[1])] # der Regressor muss weg
#   }
#   # ?nderung 16.10.14 : Versch?nerte Ausgabe
#   #cat("omitted factors:","\n",temp1)
#   
#   # ?nderung 28.10.2014: Fallunterscheidung: wenn nur 1 Element in temp1
#   if(length(temp1)>1){
#     X[,colnames(X)%in%temp1]=ifelse(X[,colnames(X)%in%temp1]==1,NA,X[,colnames(X)%in%temp1])
#   }else{
#     X[,temp1]=ifelse(X[,temp1]==1,NA,X[,temp1])
#   }
#   
#   X=na.omit(X) # l?sche Zeilen die NA's enthalten
#   
#   if(show.drops) {
#     # ?nderung 16.10.14: versch?nerte Ausgabe
#     cat("dropped variables:","\n")
#     # ?nderung 22.10.14: Fallunterscheidung; wenn nur 1 Element, wird beschriftung vergessen!
#     if(length(temp1)>1) {
#       print(proof1[(rownames(proof1) %in% temp1),])
#     }else {
#       cat(temp1,proof1[temp1,],"\n")
#     } 
#     cat("total sum : ",length(model.matrix(binReg)[,1])-length(X[,1]))
#   }
#   
#   
#   X=X[,tempX] # l?sche Spalten der perfect predictors
#   X=data.frame(X)
#   if(is.null(clustervar1)==FALSE) {
#   colnames(X)[colnames(X) == "clustervar1"] =clustervar1 # to get the real name not clustervar1
#   # regress all but not the clustervariables!!!!
#   f=paste(all.vars(formula(binReg))[1],"~.","-",clustervar1)
#   } else {
#     f=paste(all.vars(formula(binReg))[1],"~.")
#   }
#   f=as.formula(f)
#   regX=glm(f,data=X,family=binomial(link="probit"),na.action=na.omit)
#   
# }