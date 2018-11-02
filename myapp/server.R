
shinyServer(function(input, output) {
 
  csvfile <- eventReactive(input$fileInp, {
    if (is.null(file)) {
      return(NULL)
    }
    read.csv(input$fileInp$datapath,header=T)
  })
  
 ## ggplot(data=m,
   ##      aes(x=date, y=weight)) + xlab(input$xaxis)+ ylab(input$yaxis)+ggtitle(input$title)+
    ##theme(plot.title = element_text(hjust = 0.5,size=22,colour = "#7F3D17"))+
    #"geom_line(color="blue")
  #scale_x_date(limits = c(m$data[1], m$data[length(m$date)]))
  # scale_x_date(date_labels = "%b/%Y")
  
 plott<- function(){
  data = csvfile()
  m<-data.frame(data[,1],data[,input$select_col])
  colnames(m)[1]<-'date'
  colnames(m)[2]<-input$select_col

   m<-m[input$row[1]:input$row[2],]
 
    if(input$tabs == "X-Bar Chart"){
      q <- qcc(m, type="xbar",limits=c(input$LSL5,input$USL5),
               title=input$titleX,xlab=input$xaxisX,ylab=input$yaxisX)
      #q<-plot(q)
    }else{
      if(input$tabs == "S_Chart"){
        q <- qcc(m, type="S",limits=c(input$LSL1,input$USL1),title=input$titleS,xlab=input$xaxisS,ylab=input$yaxisS)
        #q<-plot(q)
        
      }else{
        if(input$tabs == "R_Chart"){
          q <- qcc(m, type="R",limits=c(input$LSL2,input$USL2),title=input$titleR,xlab=input$xaxisR,ylab=input$yaxisR)
          #q<-plot(q)
        }else{
          if(input$tabs == "Process Capability Studies"){
            data<-m
            q1 <- qcc(data, type="xbar", plot=FALSE)
            q<-process.capability(q1, spec.limits=c(input$LSL4,input$USL4))
            #q<-cp(m[,input$select_col], lsl = input$LSL4, usl = input$USL4,main=input$title,xlab=input$xaxis,ylab=input$yaxis)
          }else{
            if(input$tabs == "CUSUM"){##misy m[,2]
              q <- cusum(m, decision.interval = 4, se.shift = 1,std.dev=input$stddev3,limits=c(input$LSL3,input$USL3),title=input$titleCU,xlab=input$xaxisCU,ylab=input$yaxisCU)
              #q<-plot(q)
            }
          }}}}
  
  return(q)
}
  
  output$lep <- renderPlot({
    plott()
        })
 
 
  output$vbox <- renderValueBox({
    valueBox(length(plott()$sizes),"NUMBER GROUPS", icon = icon("columns"), color = 'light-blue', width = 1)
    })
  
  output$vbox1 <- renderValueBox({
    valueBox(round(plott()$center,1),"CENTER", icon = icon("align-center"),color = 'green', width = 1)
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(length(plott()$violations$beyond.limits),"OUTSIDE LIMIT", icon = icon("arrows-v"),color = 'orange', width = 1)
  })
  
  output$vbox3 <- renderValueBox({
    #factor<-fore[(input$years)*12,(input$risk)+1]
    #s4<-(input$amount)*factor
    valueBox(length(plott()$violations$violating.runs),"NUMBER OF VIOLATION",icon = icon("exclamation-triangle"),color = 'red', width = 1)
  })
  
  output$file <- renderUI({
  fileInput(inputId = "fileInp", label = "Input file:",multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"))
  

  })
 
  output$select_col <- renderUI({
    data = csvfile()
    fluidPage(
      #box(
        # height=480,width = 30, solidHeader = TRUE, status = "primary",
        #flowLayout(
      pickerInput("select_col","Select column", choices=names(data), selected = names(data)[2],options = list(`actions-box` = TRUE),multiple = T),
    #selectInput("select_col", "Select column", choices=names(data)[-1],selected = names(data)[2],multiple = TRUE),
    sliderInput("row", "Start-End row", 1, length(data[,1]), value = c(1, length(data[,1]))),
    
    conditionalPanel("input.tabs == 'S_Chart'",
                     #sliderInput("limitS", "LSL-USL", -10, 50, value = c(1, 20)),
                     textInput("titleS", "Title of S_Chart:", ""),
                     textInput("xaxisS", "X axis", ""),
                     textInput("yaxisS", "Y axis", "")
    ),
    
    conditionalPanel("input.tabs == 'R_Chart'",
                    # sliderInput("limitR", "LSL-USL", -10, 50, value = c(1, 20)),
                     textInput("titleR", "Title of R_Chart:", ""),
                     textInput("xaxisR", "X axis", ""),
                     textInput("yaxisR", "Y axis", "")
    ),
    conditionalPanel("input.tabs == 'CUSUM'",
                     #sliderInput("limitCU", "LSL-USL", -10, 50, value = c(1, 20)),
                     textInput("titleCU", "Title of CUSUM:", ""),
                     textInput("xaxisCU", "X axis", ""),
                     textInput("yaxisCU", "Y axis", "")
    ),
    conditionalPanel("input.tabs == 'Process Capability Studies'",
                     #sliderInput("limitPCS", "LSL-USL", -10, 50, value = c(1, 20)),
                     textInput("titlePCS", "Title of PCS:", ""),
                     textInput("xaxisPCS", "X axis", ""),
                     textInput("yaxisPCS", "Y axis", "")
    ),
    conditionalPanel("input.tabs == 'X-Bar Chart'",
                    # sliderInput("limitX", "LSL-USL", -10, 50, value = c(1, 20)),
                     textInput("titleX", "Title of X_BAR:", ""),
                     textInput("xaxisX", "X axis", ""),
                     textInput("yaxisX", "Y axis", "")
   # )
    
     #)
      ))
  })
  
 
  table<-function(){
    data = csvfile()
    #m<-data.frame(as.Date(data[,1],format="%m/%d/%Y"),data[,input$select_col])
    m<-data.frame(as.Date(data[,1],format="%m/%d/%Y"),data[,-1])
   # m<-data.frame(data[,-input$select_col],data[,input$select_col])
    colnames(m)[1]<-'Date'
    #colnames(m)[2]<-'weight'
    m<-m[input$row[1]:input$row[2],]
    return(m)
    
  }
  output$table <- DT::renderDataTable({
    data = csvfile()
    m<-data.frame(as.Date(data[,1],format="%m/%d/%Y"),data[,-1])
    colnames(m)[1]<-'date'
    colnames(m)[2]<-input$select_col
    m<-m[input$row[1]:input$row[2],]
    datatable(m,rownames=FALSE, escape = FALSE, selection = 'single',
    options = list(
      scrollX = '400px',
      scrollY = '450px',
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      
   ))
    
  })
   report1<-reactive({
    data = csvfile()
    m<-data.frame(data[,1],data[,input$select_col])
    colnames(m)[1]<-'date'
    colnames(m)[2]<-input$select_col
        m<-m[input$row[1]:input$row[2],]
        q <- qcc(m, type="S",limits=c(input$LSL1,input$USL1),title=input$titleS,xlab=input$xaxisS,ylab=input$yaxisS)  
       
        #return(q)
        combo <- list(q=q)
        combo
  })
  report2<-reactive({
    data = csvfile()
    m<-data.frame(data[,1],data[,input$select_col])
    colnames(m)[1]<-'date'
    colnames(m)[2]<-input$select_col
    m<-m[input$row[1]:input$row[2],]
    q <- qcc(m, type="R",limits=c(input$LSL2,input$USL2),title=input$titleR,xlab=input$xaxisR,ylab=input$yaxisR)
    #return(q)
    combo <- list(q=q)
    combo
  })
  
  report3<-reactive({
    data = csvfile()
    m<-data.frame(data[,1],data[,input$select_col])
    colnames(m)[1]<-'date'
    colnames(m)[2]<-input$select_col
    m<-m[input$row[1]:input$row[2],]
    q <- cusum(m, decision.interval = 4, se.shift = 1,std.dev=input$stddev3,limits=c(input$LSL3,input$USL3),title=input$titleCU,xlab=input$xaxisCU,ylab=input$yaxisCU)
 # return(q)
  combo <- list(q=q)
  combo
  })
  report4<-reactive({
    data = csvfile()
    m<-data.frame(data[,1],data[,input$select_col])
    colnames(m)[1]<-'date'
    colnames(m)[2]<-input$select_col
    m<-m[input$row[1]:input$row[2],]
    q1 <- qcc(data, type="xbar", plot=FALSE)
    q<-process.capability(q1, spec.limits=c(input$LSL4,input$USL4))
   
    #q<-cp(m[,input$select_col], lsl = input$LSL4, usl = input$USL4,main=input$title,xlab=input$xaxis,ylab=input$yaxis)
    #return(q)
    combo <- list(q=q)
    combo
  })
  
  report5<-reactive({
    data = csvfile()
    m<-data.frame(data[,1],data[,input$select_col])
    colnames(m)[1]<-'date'
    colnames(m)[2]<-input$select_col
    m<-m[input$row[1]:input$row[2],]
    q <- qcc(m, type="xbar",limits=c(input$LSL5,input$USL5),
             title=input$titleX,xlab=input$xaxisX,ylab=input$yaxisX)
    #return(q)
    combo <- list(q=q)
    combo
  })
  output$report<- downloadHandler(filename = "MembershipPlot.pdf",
                                         content = function(file) {
                                           pdf(file)
                                           report1()$combo
                                           report2()$combo
                                           report3()$combo
                                           report4()$combo
                                           report5()$combo
                                           #report2()
                                           #report3()
                                           #report4()
                                           #report5()
                                           dev.off()
                                         }
  )
  
  
  
  output$downloadplot <- downloadHandler(filename = "MembershipPlot.png",
                                         content = function(file) {
                                           png(file, type='cairo')
                                           plott()
                                           dev.off()
                                         },
                                         contentType = 'image/png'
  )
    
    
 # output$dto <- renderDataTable({thedata()})
  output$downloadData <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(table(), fname,row.names = F)
    }
  )

  
  
  #})#observe
 }
)
