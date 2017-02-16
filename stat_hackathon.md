[공통] 해커톤 12조 -Pb(납) 공간분포분석
======================================

# 1. 분석주제 선정 및 토론
## 1.1 분석주제의 조건
```
분석대상 : 비정형데이터 or Geometric 
분석대상의 조건 : 데이터크기
```

## 1.2 주제 후보군
```
1. 과연 한국은 E-Sport 강국인가?
2. 2017-18 시즌 전국 스키장 개장일은 과연 몇 일 일까?
3. 해외대학에 한국관련 학과개설과 한류의 상관분석
4. 김장시기는 어떻게 정해질까?
5. 대한민국의 알코올소비량과 대학생의 연관성
6. 로또명당은 과연 존재하는가?
```

## 1.3 후보군의 문제점
```
1. 강국의 기준의 모호함 - 게임만렙처럼 비교대상 정하기에 난항
2. 단순 내년 기상청 발표 기후자료이용 - 분석대상, 데이터크기 면에서 탈락
3. 대부분의 대학이 북미에 집중 - 한류의 대상인 중국과 일본, 동남아에 관련학과 미비
4. 단순 O/X - 최근 7일 평균기온이 4도 이하인가 아닌가
5. 주제의 모호함
6. 로또판매량 정보 부족으로 명당의 기준이 모호 (단순히 판매량이 다른 점포에 비해 2~3배 높을수도 있다)
```

## 1.4 분석주제 선정
```
당신의 건강은 안전하나요?
(다가오는 봄철 황사 얼마나 조심해야 하는가?)
```
공기중 납성분과 공기오염물질의 관계 + 공간구조

# 2. 데이터 수집
## 2.1 데이터수집장소
```
서울시데이터포털 - 서울열린데이터광장 (http://data.seoul.go.kr/index.jsp)
국가통계포털(KOSIS) (http://kosis.kr/)
```

## 2.2 데이터형식 및 구조
```
위치,장소,미세먼지농도,중금속(Pb) 등으로 구성된 csv파일
```

## 2.3 데이터 확장성
```
최근데이터 및 다른지역 데이터확장시 서울지역 뿐만 아니라 전국의 분포, 최근 상황 파악가능
```


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r cars}
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("상관분석결과"),
  
  fluidRow(
    column(4, wellPanel(
      radioButtons("picture", "picture",
                   c("number", "circle"))
    )),
    column(4,imageOutput("image2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # image1 creates a new PNG file each time Radius changes
  
  # image2 sends pre-rendered images
  output$image2 <- renderImage({
    
    if (input$picture == "circle") {
      return(list(
        src = "www/bbb.png",width=500,height=500,
        alt = "circle"
      ))
    } else if (input$picture == "number") {
      return(list(
        src="www/aaa.png",width=500,height=500,
        alt="number")
      )
    }
    
  },deleteFile=FALSE)
}
shinyApp(ui,server)
```



