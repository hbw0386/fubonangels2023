library(ggplot2)
library(dplyr)
library(DT)
a23fgb=read.csv("a23fgb2.csv")
a23.fg=read.csv("a23.fg2.csv")
fangels=c("卡卡","東東","慈妹","潔米","檸檬","朱朱","秀秀子","丹丹","蓁蓁","Tiffany",
          "Jessy","安娜","維心","沁沁","Kesha","奶昔")
zzz=read.csv("zzz.csv")

winp<-function(q){
  a23.fg%>%filter(grepl(q,Angels))->x
  nrow(x%>%filter(Result=="W"))
}

angels.info1<-function(){
  a23.fg->x
  a=nrow(x)
  b=0.433
  c=round(mean(x$HomeScore),2)
  e=round(mean(x$VisitingScore),2)
  d=round(mean(x$GameTime),1)
  cat("2023富邦悍將主場紀錄","\n","\n")
  cat("Fubon Angels:","全場次","\n")
  cat("出席場次:    ",a,"場","\n")
  cat("主場勝率:    ",b,"\n")
  cat("場均得分:    ",c,"\n")
  cat("場均失分:    ",e,"\n")
  cat("平均比賽時間:",d,"分鐘")
}
angels.info2<-function(q){
  a23.fg%>%filter(grepl(q,Angels))->x
  a=nrow(x)
  b=round(zzz$win[zzz$Name==q],3)
  c=round(mean(x$HomeScore),2)
  e=round(mean(x$VisitingScore),2)
  d=round(mean(x$GameTime),1)
  cat("2023富邦悍將主場紀錄","\n","\n")
  cat("Fubon Angels:",q,"\n")
  cat("出席場次:    ",a,"場","\n")
  cat("主場勝率:    ",b,"\n")
  cat("場均得分:    ",c,"\n")
  cat("場均失分:    ",e,"\n")
  cat("平均比賽時間:",d,"分鐘")
}

ui<-fluidPage(
  fluidRow(
    column(4,
           selectInput("Angels","富邦Angels:",c("All",fangels)
           ))
  ),
  verbatimTextOutput("summary"),
  DT::dataTableOutput("table")
)



server <- function(input, output) {
  
  output$summary<-renderPrint({

    if (input$Angels=="All"){angels.info1()}
    else{angels.info2(input$Angels)}
  })

  
  output$table <- DT::renderDataTable(DT::datatable({
    angels.split1<-function(){
      x=a23fgb
      #x%>%filter(grepl(y,Angels))->x
      x%>%group_by(HitterName)%>%
        summarize(G=n(),PA=sum(PlateAppearances,na.rm=TRUE), AB=sum(HitCnt,na.rm=TRUE), 
                  H=sum(HittingCnt,na.rm=TRUE), IBB=sum(IntentionalBasesONBallsCnt,na.rm=TRUE), 
                  BB=sum(BasesONBallsCnt,na.rm=TRUE), HBP=sum(HitBYPitchCnt,na.rm=TRUE),
                  SF=sum(SacrificeFlyCnt,na.rm=TRUE), BA=H/AB, OBP=(H+BB+HBP)/(AB+BB+HBP+SF),
                  SLG=sum(TotalBases,na.rm=TRUE)/AB, OPS=OBP+SLG, HR=sum(HomeRunCnt))->x
      x=x[c("HitterName","G","PA","AB","BB","H","HR","BA","OBP","SLG","OPS")]
      x%>%arrange(desc(OPS))->x
      x$BA%>%round(3)%>%format(nsmall=3)->x$BA
      x$OBP%>%round(3)%>%format(nsmall=3)->x$OBP
      x$SLG%>%round(3)%>%format(nsmall=3)->x$SLG
      x$OPS%>%round(3)%>%format(nsmall=3)->x$OPS
      x
    }
    angels.split2<-function(y){
      x=a23fgb
      x%>%filter(grepl(y,Angels))->x
      x%>%group_by(HitterName)%>%
        summarize(G=n(),PA=sum(PlateAppearances,na.rm=TRUE), AB=sum(HitCnt,na.rm=TRUE), 
                  H=sum(HittingCnt,na.rm=TRUE), IBB=sum(IntentionalBasesONBallsCnt,na.rm=TRUE), 
                  BB=sum(BasesONBallsCnt,na.rm=TRUE), HBP=sum(HitBYPitchCnt,na.rm=TRUE),
                  SF=sum(SacrificeFlyCnt,na.rm=TRUE), BA=H/AB, OBP=(H+BB+HBP)/(AB+BB+HBP+SF),
                  SLG=sum(TotalBases,na.rm=TRUE)/AB, OPS=OBP+SLG, HR=sum(HomeRunCnt))->x
      x=x[c("HitterName","G","PA","AB","BB","H","HR","BA","OBP","SLG","OPS")]
      x%>%arrange(desc(OPS))->x
      x$BA%>%round(3)%>%format(nsmall=3)->x$BA
      x$OBP%>%round(3)%>%format(nsmall=3)->x$OBP
      x$SLG%>%round(3)%>%format(nsmall=3)->x$SLG
      x$OPS%>%round(3)%>%format(nsmall=3)->x$OPS
      x
    }
    if (input$Angels =="All"){
      angels.split1()
      #cat(nrow(a23.fg))
      #b=round(nrow(a23.fg%>%filter(Result=="W"))/a,3)
      #cat("主場勝率:    ",b,"\n")
      }
    else{angels.split2(input$Angels)}
    
  }))
  


}

shinyApp(ui = ui, server = server)