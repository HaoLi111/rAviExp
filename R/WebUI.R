WebUI_Alpha_lin=function(){
  library(shiny)
  ui <- fluidPage(

    titlePanel("Angle of Attack Analysis"),


    sidebarLayout(
      sidebarPanel(
        sliderInput("rg", "Range of alpha:",
                    min = -15, max = 15,
                    value = c(-5,10)),
        textInput("Cla",
                  "Cla(Lift coefficient gradient)\n
                  [Deg^-1]",
                  value = '.1'),
        textInput('alpha0',
                  'alpha0(Zero-lift Angle of attack)',
                  value = '-5'),
        textInput('CdiF',
                  'Cdif(Lift-induced Drag Factor)',
                  value = '.0398'),
        textInput('Cd0',
                  'Cd0 (Zero-lift Drag coefficient)',
                  value = '.02')
      ),

      mainPanel(
        plotOutput("Plot"),
        tableOutput('Optim'),
        titlePanel('This UI is based on rAviExp package\n
                   for more informations,please visit\n
                   https://aviexptemp.weebly.com/ \n
                   https://github.com/HaoLi111/rAviExp')
        )
        )
        )

  server <- function(input, output) {
    output$Plot <- renderPlot({
      alpha<-list(Cla=eval(parse(text=input$Cla)),
                  alpha0=eval(parse(text=input$alpha0)),
                  CdiF=eval(parse(text=input$CdiF)),
                  Cd0=eval(parse(text=input$Cd0)))
      class(alpha)='Alpha_lin'
      alpha=create(alpha,alpha=seq(from=min(input$rg),to=max(input$rg),by=.1))
      lines(alpha)
    })
    output$Optim<-renderTable({
      alpha<-list(Cla=eval(parse(text=input$Cla)),
                  alpha0=eval(parse(text=input$alpha0)),
                  CdiF=eval(parse(text=input$CdiF)),
                  Cd0=eval(parse(text=input$Cd0)))
      class(alpha)='Alpha_lin'
      o=Optim(create(alpha,alpha=seq(from=min(input$rg),to=max(input$rg),by=.1)))
      rbind(o[[1]],o[[2]],o[[3]])
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)

}

WebUI_Theta=function(){

  library(shiny)
  #library(rAviExp)
  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
    titlePanel("Least climbing velocity ~ angle"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("Cl",
                    "Cl/100:",
                    min = 1,
                    max = 100,
                    value = 45),
        sliderInput("Cd",
                    "Cd/100:",
                    min = 1,
                    max = 100,
                    value = 2),
        sliderInput("FG",
                    "FG[N]",
                    min = 1,
                    max = 100,
                    value = 15),
        sliderInput("S",
                    "Lift Area(S)/100[m^2]:",
                    min = 1,
                    max = 100,
                    value = 70),
        sliderInput("F_max",
                    "F_max/100[N]",
                    min = 1,
                    max = 100,
                    value = 7),
        sliderInput("P_max",
                    "P_max[W]",
                    min = 1,
                    max = 300,
                    value = 95),
        width = 4
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$distPlot <- renderPlot({
      climbAnalysis = list(
        theta = (1:650)/10,
        state = list(Cl = input$Cl*100,
                     Cd = input$Cd*100,
                     FG = input$FG,
                     rho = 1.21,
                     S = input$S*100
        ),
        Pp = list(F_max = input$F_max*100,
                  P_max = input$P_max)
      )
      lines(create.Theta(climbAnalysis))
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

WebUI_ConvConcept=function(){


  library(shiny)

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
    titlePanel("Conventional Concept simplified"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        #WM
        sliderInput("Xcg",
                    "Xcg[cm]",
                    min=1,
                    max=120,
                    value = 25),
        sliderInput("WM.Span",
                    "WM.Span[cm]",
                    min = 1,
                    max = 200,
                    value =120),
        sliderInput("WM.Sweep",
                    "WM.Sweep[deg]",
                    min = 1,
                    max = 40,
                    value = 10),
        sliderInput("WM.ChordR",
                    "WM.ChordR[cm]",
                    min = 18,
                    max = 60,
                    value = 20),
        sliderInput("WM.ChordT",
                    "WM.ChordT[cm]",
                    min = 14,
                    max = 60,
                    value = 10),
        sliderInput("WM.Xf_C",
                    "WM.Xf_C[%]",
                    min = 1,
                    max = 100,
                    value = 25),
        sliderInput("WM.x",
                    "WM.x[cm]",
                    min = 1,
                    max = 200,
                    value = 20),
        sliderInput("WM.y",
                    "WM.y[cm]",
                    min = -100,
                    max = 100,
                    value = 0),
        sliderInput("WM.z",
                    "WM.z[cm]",
                    min = -100,
                    max = -100,
                    value = 0),#WH
        sliderInput("WH.Span",
                    "WH.Span[cm]",
                    min = 1,
                    max = 200,
                    value = 15),
        sliderInput("WH.Sweep",
                    "WH.Sweep[deg]",
                    min = 1,
                    max = 40,
                    value = 10),
        sliderInput("WH.ChordR",
                    "WH.ChordR[cm]",
                    min = 1,
                    max = 60,
                    value = 10),
        sliderInput("WH.ChordT",
                    "WH.ChordT[cm]",
                    min = 1,
                    max = 60,
                    value = 5),
        sliderInput("WH.Xf_C",
                    "WH.Xf_C[%]",
                    min = 1,
                    max = 100,
                    value = 25),
        sliderInput("WH.x",
                    "WH.x[cm]",
                    min = 1,
                    max = 200,
                    value = 80),
        sliderInput("WH.y",
                    "WH.y[cm]",
                    min = -100,
                    max = 100,
                    value = 0),
        sliderInput("WH.z",
                    "WH.z[cm]",
                    min = -100,
                    max = 100,
                    value = 0),#WV
        sliderInput("WV.Span",
                    "WV.Span[cm]",
                    min = 1,
                    max = 200,
                    value = 15),
        sliderInput("WV.Sweep",
                    "WV.Sweep[deg]",
                    min = 1,
                    max = 40,
                    value = 10),
        sliderInput("WV.ChordR",
                    "WV.ChordR[cm]",
                    min = 1,
                    max = 60,
                    value = 10),
        sliderInput("WV.ChordT",
                    "WV.ChordT[cm]",
                    min = 1,
                    max = 60,
                    value = 5),
        sliderInput("WV.Xf_C",
                    "WV.Xf_C[%]",
                    min = 1,
                    max = 100,
                    value = 25),
        sliderInput("WV.x",
                    "WV.x[cm]",
                    min = 1,
                    max = 200,
                    value = 80),
        sliderInput("WV.y",
                    "WV.y[cm]",
                    min = -100,
                    max = 100,
                    value = 0),
        sliderInput("WV.z",
                    "WV.z[cm]",
                    min = -100,
                    max = -100,
                    value = 0),
        textInput("Length","Length",value = '1.05'),
        textInput("r","r","c( 0.050,0.075,0.075,0.020,0.010,0.000)"),
        textInput("x","x","c(0.00,0.20,0.40,0.90,1.00,1.05)"),
        width =3
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"),
        tabsetPanel(type = "tabs",
                    tabPanel("Main Wing MAC", tableOutput("wm")),
                    tabPanel("Horz tail MAC", tableOutput('wh')),
                    tabPanel("Vert tail MAC", tableOutput('wv'))),
        tableOutput("SC")

      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {
    output$wm = renderTable({
      WM = list(Span = input$WM.Span/100,
                Sweep = input$WM.Sweep,
                ChordR = input$WM.ChordR/100,
                ChordT = input$WM.ChordT/100,
                Type = 0,
                Xf_C = input$WM.Xf_C/100,
                x = input$WM.x/100,
                y = input$WM.y/100,
                z = input$WM.z/100)
      class(WM) = 'wing'
      MAC(WM)
    })
    output$wh = renderTable({
      WH = list(Span = input$WH.Span/100,
                Sweep = input$WH.Sweep,
                ChordR = input$WH.ChordR/100,
                ChordT = input$WH.ChordT/100,
                Type = 0,
                Xf_C = input$WH.Xf_C/100,
                x = input$WH.x/100,
                y = input$WH.y/100,
                z = input$WH.z/100)
      class(WH) = 'wing'
      MAC(WH)
    })
    output$wv = renderTable({
      WV = list(Span = input$WV.Span/100,
                Sweep = input$WV.Sweep,
                ChordR = input$WV.ChordR/100,
                ChordT = input$WV.ChordT/100,
                Type = 0,
                Xf_C = input$WV.Xf_C/100,
                x = input$WV.x/100,
                y = input$WV.y/100,
                z = input$WV.z/100)
      class(WV) = 'wing'
      MAC(WV)
    })
    output$SC = renderTable({

      WM = list(Span = input$WM.Span/100,
                Sweep = input$WM.Sweep,
                ChordR = input$WM.ChordR/100,
                ChordT = input$WM.ChordT/100,
                Type = 0,
                Xf_C = input$WM.Xf_C/100,
                x = input$WM.x/100,
                y = input$WM.y/100,
                z = input$WM.z/100)
      class(WM) = 'wing'
      WH = list(Span = input$WH.Span/100,
                Sweep = input$WH.Sweep,
                ChordR = input$WH.ChordR/100,
                ChordT = input$WH.ChordT/100,
                Type = 0,
                Xf_C = input$WH.Xf_C/100,
                x = input$WH.x/100,
                y = input$WH.y/100,
                z = input$WH.z/100)
      class(WH) = 'wing'
      WV = list(Span = input$WV.Span/100,
                Sweep = input$WV.Sweep,
                ChordR = input$WV.ChordR/100,
                ChordT = input$WV.ChordT/100,
                Type = 1,
                Xf_C = input$WV.Xf_C/100,
                x = input$WV.x/100,
                y = input$WV.y/100,
                z = input$WV.z/100)
      class(WV) = 'wing'

      fuse = list(Length = as.numeric(input$Length),
                  x = eval(parse(text = (input$x))),
                  r = eval(parse(text = (input$r))))
      class(fuse) = "fuse"
      x = list(WM=WM,WH=WH,WV=WV,fuselage=fuse)
      class(x$WM) = 'wing'
      class(x$WH) = 'wing'
      class(x$WV) = 'wing'
      class(x) = "conventionalConcept"
      #plotxy(plane_init)
      s = SC_simp(x,input$Xcg/100)
      re = as.vector(c(unlist(s$From),unlist(s$Raw),unlist(s$Pos), unlist(s$Lever),unlist(s$Coef)))

      dim(re) = c(1,13)
      colnames(re) = c("From","Xcg","Xnp","Pos.xM","Pos.xH","Pos,xV",
                       "Lever","Lever.H","Lever.V",
                       "Coef.Vh","Coef.Vv","SM","M_de")
      re
    })
    output$distPlot <- renderPlot({
      WM = list(Span = input$WM.Span/100,
                Sweep = input$WM.Sweep,
                ChordR = input$WM.ChordR/100,
                ChordT = input$WM.ChordT/100,
                Type = 0,
                Xf_C = input$WM.Xf_C/100,
                x = input$WM.x/100,
                y = input$WM.y/100,
                z = input$WM.z/100)
      class(WM) = 'wing'
      WH = list(Span = input$WH.Span/100,
                Sweep = input$WH.Sweep,
                ChordR = input$WH.ChordR/100,
                ChordT = input$WH.ChordT/100,
                Type = 0,
                Xf_C = input$WH.Xf_C/100,
                x = input$WH.x/100,
                y = input$WH.y/100,
                z = input$WH.z/100)
      class(WH) = 'wing'
      WV = list(Span = input$WV.Span/100,
                Sweep = input$WV.Sweep,
                ChordR = input$WV.ChordR/100,
                ChordT = input$WV.ChordT/100,
                Type = 1,
                Xf_C = input$WV.Xf_C/100,
                x = input$WV.x/100,
                y = input$WV.y/100,
                z = input$WV.z/100)
      class(WV) = 'wing'

      fuse = list(Length = as.numeric(input$Length),
                  x = eval(parse(text = (input$x))),
                  r = eval(parse(text = (input$r))))
      class(fuse) = "fuse"
      x = list(WM=WM,WH=WH,WV=WV,fuselage=fuse)
      class(x$WM) = 'wing'
      class(x$WH) = 'wing'
      class(x$WV) = 'wing'
      class(x) = "conventionalConcept"
      #plotxy(plane_init)
      plotxy.conventionalConcept(x)#
      abline(v = input$Xcg/100,col = 'red')

      s = SC_simp(x,input$Xcg/100)
      re = as.vector(c(unlist(s$From),unlist(s$Raw),unlist(s$Pos), unlist(s$Lever),unlist(s$Coef)))

      dim(re) = c(1,13)
      colnames(re) = c("From","Xcg","Xnp","Pos.xM","Pos.xH","Pos,xV",
                       "Lever","Lever.H","Lever.V",
                       "Coef.Vh","Coef.Vv","SM","M_de")
      abline(v = re[1,3],col = "blue")
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
