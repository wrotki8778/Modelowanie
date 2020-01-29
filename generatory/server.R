
library(shiny)
library(plot3D)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$dystr<-renderPlot({
  par(mar=c(5,5,1,1))
  xx=seq(0,1,by=0.01)
  plot(xx,0.5*(1+sin(pi*(xx-0.5))),type='l',lwd=2,asp=1,xlab='x',ylab=expression(paste(F(x),', ',F^{-1},(x))))
  lines(xx,1/pi*asin(2*xx-1)+0.5,type='l',col=2,lwd=2)
  })
  
  
  z=7
   x10000=1/pi*asin(runif(10000,-1,1))+0.5
   x10=x10000[1:10]
   x100=x10000[1:100]
   x1000=x10000[1:1000]
   teoret=c(NA,0,1,0.5,sqrt((pi^2-8)/(4*pi^2)),(pi^2-8)/(4*pi^2),1/3,0.5,2/3)
   opis<-function(x){ c(length(x),min(x),max(x),mean(x),sd(x),var(x),quantile(x,0.25),median(x),quantile(x,0.75))}
   n10=opis(x10)
   n100=opis(x100)
   n1000=opis(x1000)
   n10000=opis(x10000)
   #namesa=c('n','min','max','srednia','odchyl','war','1.kw','mediana','3.kw')
   #namesb=c('','','','','','(\u03c0\u00B2-8)/(4\u03c0\u00B2)','','','')
   #paste(namesa,namesb,sep=' ')
   desct=data.frame('teoretyczne'=teoret,
                    'N=10'=n10,
                    'N=100'=n100,
                    'N=1000'=n1000,
                    'N=10000'=n10000,
                      row.names=c('n','min','max','srednia','odchyl (a.)','war (a.)','1.kw','mediana','3.kw')
                      )
   desctr<-reactive({
     desct[,c(1,as.numeric(input$NN)),drop=FALSE]
     })
   
  output$descTab <-renderTable(
    desctr(), digits = 3,
    rownames = TRUE
    ) 
  
  histfunction<-function(dane){
    bins <- seq(0, 1, length.out = input$bins + 1)
    prob=(input$probb=='dens')
    ylabel=ifelse((input$probb=='dens'),'gestosc','licznosc')
    xx=seq(0,1,by=0.01)
    typ=ifelse((input$probb=='dens'),'l','n')
    # draw the histogram with the specified number of bins
    if(input$ymax<=0){
      hist(dane, breaks = bins, col = 'darkgray', border = 'white',probability = prob,xlab='',ylab=ylabel,main=paste('N=',length(dane)))
    }else{
    hist(dane, breaks = bins, col = 'darkgray', border = 'white',probability = prob,xlab='',ylab=ylabel,main=paste('N=',length(dane)),ylim=c(0,input$ymax))
    }
    lines(xx,pi/2*cos(pi*(xx-.5)),col=2,type=typ)
  }
  
  output$ui_hist <-renderUI({
    out<-list()
    if(length(input$NNp)==0){return(NULL)}
    for (i in 1:length(input$NNp)){
     out[[i]]<- column(3,  plotOutput(outputId=paste0('hist',i)) )
    }
    do.call(tagList,out)
    
    return(out)
  })
  
   observe({
     for(i in 1:4){
      local({
        ii<-i
        output[[paste0('hist',ii)]]<-renderPlot({
          if (length(input$NNp)> ii-1){
            return(
              histfunction(dane=eval(parse(text=input$NNp[[ii]])))
                   )
          }
          NULL
        })
      })
     }
   })
  
  #output$distPlot <- renderPlot({
  #  
  #  bins <- seq(0, 1, length.out = input$bins + 1)
  #  prob=(input$probb=='dens')
  #  ylabel=ifelse((input$probb=='dens'),'gestosc','licznosc')
  #  xx=seq(0,1,by=0.01)
  #  typ=ifelse((input$probb=='dens'),'l','n')
  #  # draw the histogram with the specified number of bins
  #  hist(x10, breaks = bins, col = 'darkgray', border = 'white',probability = prob,xlab='',ylab=ylabel)
  #  lines(xx,pi/2*cos(pi*(xx-.5)),col=2,type=typ)
  #  hist(x1000, breaks = bins, col = 'darkgray', border = 'white',probability = prob,xlab='',ylab=ylabel)
  #  lines(xx,pi/2*cos(pi*(xx-.5)),col=2,type=typ)
  #})
   
   ecdffunction<-function(dane){
     xx=seq(0,1,by=0.01)
     plot(ecdf(dane),xlab='x',ylab=expression(paste(F[n](x),', ',F(x))),main=paste('N=',length(dane)),xlim=c(-0.1,1.1),ylim=c(0,1))
     lines(xx,1/2*(1+sin(pi*(xx-.5))),col=2,lty=2)
   }
   
   output$ui_ecdf <-renderUI({
     out<-list()
     if(length(input$NNd)==0){return(NULL)}
     for (i in 1:length(input$NNd)){
       out[[i]]<- column(3,  plotOutput(outputId=paste0('ecdf',i)) )
     }
     do.call(tagList,out)
     
     return(out)
   })
   
   observe({
     for(i in 1:4){
       local({
         ii<-i
         output[[paste0('ecdf',ii)]]<-renderPlot({
           if (length(input$NNd)> ii-1){
             return(
               ecdffunction(dane=eval(parse(text=input$NNd[[ii]])))
             )
           }
           NULL
         })
       })
     }
   })
   
   geomx<-reactive({
    return(rgeom(input$geomn,input$geomp)+1)
   })
   
   
   
   geomt<-reactive({
      if(input$geomb=='counts'){
      geomt<-data.frame(table(geomx()))
      colnames(geomt)<-c('wartosc','licznosci')}else{
        geomt<-data.frame(table(geomx())/input$geomn)  
      colnames(geomt)<-c('wartosc','czestosci')
      }
    return(geomt)
   })
   
   output$geompr1 <- renderText({
     paste('Probka ', input$geomn, ' elementowa.')
   })
   
   output$geompr2 <- renderText({
     geomx()
   })
   
   output$geomt <-renderTable(
     geomt(),digits=4
   )
   
   geomplot<-reactive({
     if(input$geomb=='counts'){
       plot(table(geomx()),ylab='licznosci')}else{
           plot(table(geomx())/input$geomn,ylab='czestosci')
           xx=seq(1:max(geomx()))
           points(xx,dgeom(xx-1,p=input$geomp),col=2,pch=16)
           }
   })
   
   output$geompl <-renderPlot(
   geomplot()
   )
   
   blad<-reactive({
     if(input$mp1+input$mp2+input$mp3 ==1){return()}else{return('blad prawdopodobienstw')}
   })
   
   output$blad<-renderText(blad())
   
   plotmulti<-reactive({
     if(input$mp1+input$mp2+input$mp3 ==1){
       X <- t(as.matrix(expand.grid(0:input$mN, 0:input$mN)));# X <- X[, colSums(X) <= 10]
       X <- rbind(X, input$mN:input$mN - colSums(X))
       pp=apply(X,2,function(x) ifelse(x[3]>=0,dmultinom(x, input$mN,c(input$mp1,input$mp2,input$mp3)),0))
       x <- seq(0,input$mN,by=1)
       y <- seq(0,input$mN,by=1)
       par (mar = par("mar") + c(0, 0, 0, 2))
       hist3D(x, y, z = matrix(pp,input$mN+1,input$mN+1),
              zlim = c(0, max(pp)),
              ylab = expression(x[2]), xlab = expression(x[1]),main='Rozklad teoretyczny (x[1],x[2],x[3])',
              zlab = '', bty= "b", phi = 45, theta = 65,
              shade = 0.2, col = "white", border = "black",
              d = 6, ticktype = "detailed",alpha=0.5)}
   })
   
   output$multiplot <-renderPlot(
    plotmulti()
   )
   
   #tabmulti1<-reactive({
  #   if(input$mp1+input$mp2+input$mp3 ==1){
  #     X <- t(as.matrix(expand.grid(0:input$mN, 0:input$mN))); X <- X[, colSums(X) <= input$mN]
  #     X <- rbind(X, input$mN:input$mN - colSums(X))
  #     X<-data.frame(t(X))
  #     colnames(X)<-c('x_1','x_2','x_3')
  #     return(X)
  #   }
  #   }) 
   
  # tabmulti2<-reactive({
  #   if(input$mp1+input$mp2+input$mp3 ==1){
   #    X <- t(as.matrix(expand.grid(0:input$mN, 0:input$mN))); X <- X[, colSums(X) <= input$mN]
   #    X <- rbind(X, input$mN:input$mN - colSums(X))
   #    pp=apply(X,2,function(x) ifelse(x[3]>=0,dmultinom(x, input$mN,c(input$mp1,input$mp2,input$mp3)),0))
  #     pp<-data.frame(pp)
  #     colnames(pp)<-c('prob. teoret.')
  #     return(pp)
  #   }
   #}) 
   
   tabm<-reactive({
     if(input$mp1+input$mp2+input$mp3 ==1){
       X <- t(as.matrix(expand.grid(0:input$mN, 0:input$mN))); X <- X[, colSums(X) <= input$mN]
       X <- rbind(X, input$mN:input$mN - colSums(X))
       pp=apply(X,2,function(x) ifelse(x[3]>=0,dmultinom(x, input$mN,c(input$mp1,input$mp2,input$mp3)),0))
       X<-data.frame(t(X),round(pp,5))
       colnames(X)<-c('x_1','x_2','x_3','prob. teoret.')
       return(X)
     }
   }) 
   
   output$tabm <- DT::renderDataTable(
     tabm(), server=FALSE, rownames=FALSE,caption='Rozklad teoretyczny wektora losowego (X_1,X_2,X_3)'
   )
   
   #output$tabmulti1 <-renderTable(
  #   tabmulti1(),digits=0
  # )
   #output$tabmulti2 <-renderTable(
  #   tabmulti2(),digits=4
  # )
})
