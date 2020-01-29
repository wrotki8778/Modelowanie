
library(shiny)


shinyUI(navbarPage("Projekt 2",
                   tabPanel("Rozklad cosinus",
  # Application title
  titlePanel("Rozklad cosinus"),
  fluidPage(
    title = 'gestosc',
    withMathJax(),
  #  helpText
h4('Skonstruowac generator rozkladu o gestosci 
\\(f(x)=\\frac{\\pi}{2}\\cos \\left( \\pi \\left(x-\\frac12\\right) \\right)\\mathbb{1}_{[0,1]}\\)
             w oparciu o metode odwrotnej dystrybuanty.')
    )),
tabPanel("Dystrybuanta odwrotna",
titlePanel("Metoda odwrotnej dystrybuanty"),  
sidebarLayout(
  sidebarPanel(
    #title = 'dystrybuanta',
    withMathJax(),
    p('Dla zadanego rozkladu nalezy okreslic dystrybuante \\(F(x)\\) oraz wyznaczyc analitycznie dystrybuante odwrotna \\( \\color{red} {F^{-1}(x)}\\). Obie funkcje przedstawic na wykresie.')
    ),
  mainPanel(
    withMathJax(),
    p('Dystrybuanta \\(F(x)\\) oraz dystrybuanta odwrotna \\( \\color{red} {F^{-1}(x)}\\):'),
    plotOutput('dystr',width = "400px", height = "400px")
        ))),

tabPanel("Statystyki opisowe",
titlePanel("Statystyki opisowe"),  
sidebarLayout(
  sidebarPanel(
    withMathJax(),
    p('Wygenerowac ciag liczb pseudolosowych, z generatora rozkladu jednostajnego:'),
    code("runif()"),
    p('o dlugosci \\(n=10, 100, 1000\\). Metoda odwrotnej dystrybuanty wyznacz ciagi liczb pseudolosowych z zadanego rozkladu. Okresl statystyki opisowe i porownaj z parametrami rozkladu. Wyniki przedstaw w formie tabeli.')
  ),
  mainPanel()),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("NN", "Wielkosc probki:",
                         choiceNames = list("N=10","N=100","N=1000","N=10000"),
                         choiceValues = list(2,3,4,5),
                         inline=TRUE
                         )),
      mainPanel(
        withMathJax(),
        tableOutput("descTab")
      ))),

tabPanel("Histogramy",
titlePanel("Histogramy"),
sidebarLayout(
  sidebarPanel(
    withMathJax(),
    p('Dla wygenerowanych ciagow pseudolosowych narysowac histogramy wraz z wykresem gestosci rozkladu teoretycznego. Na histogramie przedstawiona ma byc gestosc:'),
    code("hist(...,prob=TRUE)")
  ),
  mainPanel()),

  fluidRow(
      column(4,wellPanel(checkboxGroupInput("NNp", "Wielkosc probki:",
                         choiceNames = list("N=10","N=100","N=1000","N=10000"),
                         choiceValues = list('x10','x100','x1000','x10000'),
                         inline=TRUE
      ))),
      column(4, wellPanel(sliderInput("bins",
                   "Liczba przedzialow:",
                   min = 1,
                   max = 50,
                   value = 5))),
      column(3,wellPanel(
       radioButtons("probb","Wartosc osi y:",
                    c("licznosci"="counts","gestosc"="dens")),
       numericInput('ymax','Ymax',0)
    ))),
fluidRow(
  uiOutput('ui_hist')
  )),

tabPanel("Dystrybuanta empiryczna",
titlePanel("Dystrybuanta empiryczna"),
sidebarLayout(
  sidebarPanel(
    withMathJax(),
    p('Dla wygenerowanych ciagow pseudolosowych narysowac wykres dystrybuanty empirycznej wraz z wykresem dystrybuanty rozkladu teoretycznego:'),
    code("ecdf()")
  ),
  mainPanel(
    withMathJax(),
    p('Dystrybuante empiryczna \\(F_n(x)\\) wyznaczamy na podstawie uporzadkowanej \\(n\\)-elementowej probki \\(x_{(1)}\\leq x_{(2)}\\leq\\ldots\\leq x_{(n)}\\) w sposob nastepujacy:'),
    withMathJax(p('$$F_n(x):= \\begin{cases}  0\\text{ dla }x<x_{(1)}, \\\\ \\frac k n \\text{ dla } x_{(k)}\\leq x < x_{k+1}, 1\\leq k\\leq n-1, \\\\ 1 \\text{ dla } x\\geq x_{(n)}. \\end{cases}$$'))
    )),


fluidRow(
  column(4,wellPanel(checkboxGroupInput("NNd", "Wielkosc probki:",
                                        choiceNames = list("N=10","N=100","N=1000","N=10000"),
                                        choiceValues = list('x10','x100','x1000','x10000'),
                                        inline=TRUE)))),
fluidRow(
  uiOutput('ui_ecdf')
)


),
tabPanel("Rozklady dyskretne",
         titlePanel("Rozklady dyskretne"),
         sidebarLayout(
           sidebarPanel(
             withMathJax(),
             p('W przypadku rozkladow typu dyskretnego, dla wygenerowanych ciagow pseudolosowych nalezy narysowac wykres czestosci wraz z wykresem teoretycznego rozkladu masy prawdopodobienstwa.'),
             code("table()")
           ),
           mainPanel()),
         
         sidebarLayout(
           sidebarPanel(
             h4('Rozklad geometryczny')),
           mainPanel(
             withMathJax(),
             p('Rozwazmy rozklad geometryczny z parametrem \\(p\\). Funkcja rozkladu masy prawdopodobienstwa dana jest poprzez:'),
             p('$$ p_k=(1-p)^{k-1}p, \\quad k=1,2,\\ldots $$'),
             p('Rozklad ten mozemy interpretowac jako czas oczekiwania na pierwszy sukces w ciagu prob Bernoulliego z prawdopodobienstwem sukcesu \\(p\\).')
           )),
         
         sidebarLayout(
           sidebarPanel(
             sliderInput("geomp",
                         "p (prawdopodobienstwo sukcesu)",
                         min = 0.01,
                         max = 1,
                         value = 0.2),
             numericInput('geomn','N (licznosc probki)',10,min=1),
             radioButtons("geomb","Wartosci:",
                          c("licznosci"="counts","czestosci"="frek"))
             ),
           mainPanel(
             textOutput("geompr1"),
             conditionalPanel("input.geomn <= 100",
                              textOutput("geompr2")),
             fluidRow(
               column(4,tableOutput("geomt")),
             column(8,plotOutput("geompl")))
           )),
         
         sidebarLayout(
           sidebarPanel(
             h4('Rozklad wielomianowy')),
           mainPanel(
             withMathJax(),
             p('Rozwazmy rozklad prawdopodobienstwa wynikow pewnego eksperymentu o \\(k\\in\\mathbb{N}\\) mozliwych rezultatach.'),
             p('Niech \\(k=3\\) bedzie ustalone i wzajemnie wykluczajace sie zdarzenia zachodza z prawdopodbienstwami \\(p_1, p_2,\\ldots, p_k\\) odpowiednio, gdzie \\(\\sum_{i=1}^kp_i=1\\).'),
             p('Jezeli \\(X_i, i=1,2,\\ldots,k\\) oznaczac bedzie liczbe otrzymanych rezultatow wyniku typu \\(i\\)-tego w \\(N\\) probach, wtedy wektor \\( \\left(X_1,X_2,\\ldots,X_k\\right)\\) ma rozklad wielomianowy z parametrami \\(N\\) i \\(p=(p_1,p_2,\\ldots,p_k)\\).')
           )),
         br(),
         sidebarLayout(
           sidebarPanel(
             withMathJax(),
             p('W przypadku rozkladu wielomianowego, \\( (k=3)\\), wykres czestosci mozna przedstawic w 3D wraz z wykresem teoretycznego rozkladu prawdopodbienstwa. Wyniki mozna rowniez przedstawic w formie tabeli czy macierzy.'),
             code("plot3D, hist3D")
             ),
           mainPanel(
             withMathJax(),
             p('\\( k=3\\)'),
             sliderInput("mN",
                         "N",
                         min = 1,
                         max = 10,
                         value = 10,step=1),
             p('Prawdopodobienstwa \\( p_i \\in [0,1] \\):'),
             flowLayout(
             numericInput('mp1','p_1',0.4,min=0,max=1,width='140px'),
             numericInput('mp2','p_2',0.3,min=0,max=1,width='140px'),
             numericInput('mp3','p_3',0.3,min=0,max=1,width='140px')
             ),
            h2(textOutput("blad")),
            plotOutput("multiplot"),
            #fluidRow(
              DT::dataTableOutput("tabm")
              #column(4,tableOutput("tabmulti1")),column(4,tableOutput("tabmulti2"))
             # )
           )
           )
        )
))
