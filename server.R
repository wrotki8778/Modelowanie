# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  #output$distPlot <- renderPlot({
  #  
  #  x    <- faithful$waiting
  #  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #  
  #  hist(x, breaks = bins, col = "#75AADB", border = "white",
  #       xlab = "Waiting time to next eruption (in mins)",
  #       main = "Histogram of waiting times")
  #  
  #})
  datasetInput <- reactive({
    switch(input$probka2,
           "macierz"=macierz,
           "x"=x,
           "y"=y)
  })
  output$naszPlot<-renderPlot({
    sd<-sqrt(input$sigma)
    mu<-input$mu
    x    <- rnorm(input$probka,mean=mu,sd=sd)
    x<-x[abs(x-mu)<4*sd]
    f<-ecdf(x)
    czasy<-seq(mu-4*sd,mu+4*sd,by=sd*2^(-4))
    breaks=seq(mu-4*sd,mu+4*sd,by=sd*2^(-2))
    height=1.2*(2*pi)^(-1/2)/sd
    hist(x,prob=TRUE,breaks=breaks,ylim=c(0,height))
    lines(czasy,dnorm(czasy,mu,sd),type='l',col='blue')
  })
  procedura<-reactive({
    require("car")
    ziarno=input$ziarno
    set.seed(ziarno)
    x=rnorm(input$probka2)
    set.seed(ziarno+10)
    y=rnorm(input$probka2)
    dataEllipse(x,y)
  })
  procedura_2<-reactive({
    require("car")
    ziarno=input$ziarno
    set.seed(ziarno)
    x=rnorm(input$probka2)
    set.seed(ziarno+10)
    y=rnorm(input$probka2)
    macierz<-cov(cbind(x,y))
    print(macierz)
  })
  output$macierz<-renderPlot({
   procedura() 
  })
  output$tabela<-renderPrint({
   procedura_2()
  })
}