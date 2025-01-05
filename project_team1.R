# 필요 라이브러리 로드 -------------------------------------------------------------
if (!require("BatchGetSymbols")) install.packages("BatchGetSymbols")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("shiny")) install.packages("shiny")
library(BatchGetSymbols)
library(ggplot2)
library(gridExtra)
library(shiny)

# 캔들차트 --------------------------------------------------------------------
# ggplot 패키지를 이용해서, 캔들 차트를 그려주는 함수 
# draw_candlestick_chart를 구현

draw_candlestick_chart <- function(stock_df){
  # stock_df <- BatchGetSymbols(tickers = ticker,
  #                             first.date = start_date,
  #                             last.date = end_date)
  
  x.index <- seq(length(stock_df$price.open))
  # 데이터에 제공된 날짜는, 공휴일이 빠져서
  # 실제로 그림을 그렸을 때 차트 사이에 빈 부분이 생김.
  # 이를 없애기 위해 새로 사용할 index 선언
  
  # (2024.12.15 수정)
  data_length <- length(stock_df$price.open)
  x.index.selected <- c(1, 
                        round(data_length * 1/5), 
                        round(data_length * 2/5), 
                        round(data_length * 3/5),
                        round(data_length * 4/5), 
                        data_length)
  date.label <- stock_df$ref.date[x.index.selected]
  
  candlestick_chart <- ggplot(stock_df, aes(x = x.index)) +
    geom_linerange(aes(ymin = price.low, ymax = price.high, color = price.close > price.open), size = 2) +
    # 캔들차트의 선은 그 날의 최고가(price.high), 최저가(price.low)로 결정
    geom_rect(aes(xmin = x.index - 0.4, xmax = x.index + 0.4, 
                  ymin = pmin(price.open, price.close), ymax = pmax(price.open, price.close), 
                  fill = price.close > price.open)) +
    # 캔들차트의 박스는 그 날의 시가(price.open), 종가(price.close)로 결정
    # 이때 색은, 그날의 시가 < 종가(현재가) 이면 초록색, 시가 > 종가(현재가)이면 빨간색입니다.
    # (나스닥이라 미국 주식의 차트 색을 그대로 따라했습니다)
    scale_fill_manual(values = c("TRUE" = "#26A69A", "FALSE" = "#EF5350")) +
    scale_color_manual(values = c("TRUE" = "#26A69A", "FALSE" = "#EF5350")) +
    labs(x = "Date", y = "Price($)") +
    theme_minimal() +
    scale_x_continuous(breaks = x.index.selected, labels = date.label) +
    # x축을 날짜 값으로 수정
    theme(legend.position = "none",
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15))
  
  return(candlestick_chart)# return 값으로 따로 plot을 저장할 수도 있게 해줌
}


# 거래량 차트 ------------------------------------------------------------------

draw_volume_chart <- function(stock_df){
  volume_chart <- ggplot(stock_df, 
                         aes(x = as.numeric(rownames(stock_df)), y = volume)) +
    geom_bar(stat = "identity", fill = "black") +
    # geom_bar를 이용해서 그릴 것임
    theme_minimal() +
    labs(title = "", x = "", y = "Volume") +
    theme(
      axis.text.x = element_blank(), # x축 텍스트 숨기기
      axis.ticks.x = element_blank(), # x축 눈금 숨기기
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15)
    )
  
  print(volume_chart)
  return(volume_chart)
}

# 추세선 ---------------------------------------------------------------------
calculate_moving_average <- function(stock_df, window_size, color) {
  mas <- numeric(length(stock_df$price.close)) # 이동 평균 값을 저장할 벡터
  mas[1:(window_size - 1)] <- NA # 초기 값은 NA로 설정
  
  for (index in seq(window_size, length(stock_df$price.close))) {
    # 윈도우 내 합계 계산
    window_sum <- 0
    for (semiIndex in seq_len(window_size)) {
      i <- semiIndex - 1
      window_sum <- window_sum + stock_df$price.close[index - i]
    }
    mas[index] <- window_sum / window_size # 평균 계산
  }
  
  result <- na.omit(data.frame(Index = 1:length(mas), ma = mas))
  # return(result)
  
  ma_line <- geom_line(data = result, 
                       aes(x = Index - (window_size - 1), y = ma), 
                       color = color, 
                       alpha = 0.8,
                       size = 2)
  return(ma_line)
}

# shiny -------------------------------------------------------------------
# UI
ui <- fluidPage(
  titlePanel("Interactive Candlestick Chart"),
  # 패널의 제목은, Interactive Candlestick Chart
  
  # 사이드바 구성
  sidebarLayout(
    sidebarPanel(
      # 종목을 입력 받는 칸
      textInput(
        "ticker",
        "Enter Stock Ticker:",
        value = "AAPL",
        placeholder = "Enter a stock ticker (e.g., AAPL, MSFT)"
      ),
      # 검색할 기간을 선택하는 칸
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 40,
        end = Sys.Date()
      ),
      
      tags$style(HTML("
      .ma-buttons {
      display: flex;
      justify-content: center;
      gap: 1px; /* 버튼 간의 간격 조절 */
      }
      #btn_5 { background-color: #1F77B4; color: white; }      /* 배경 빨강, 글자 흰색 */
      #btn_10 { background-color: #FF7F0E; color: white; }    /* 배경 파랑, 글자 흰색 */
      #btn_20 { background-color: #2CA02C; color: white; }   /* 배경 초록, 글자 흰색 */
      #btn_50 { background-color: #D62728; color: white; }  /* 배경 주황, 글자 흰색 */
      #btn_100 { background-color: #9A6FC0; color: white; } /* 배경 보라, 글자 흰색 */
      #btn_200 { background-color: #8C564B; color: white; }  /* 배경 갈색, 글자 흰색 */
                      ")),
      
      tags$style(HTML("
      .ma-all-buttons {
      display: flex;
      justify-content: center;
      gap: 1px; /* 버튼 간의 간격 조절 */
      }")),
      
      # 버튼 칸(2024.12.07 미완성)
      h4("Moving Average Lines (Days)"),
      # fluidRow
      div(class = "ma-buttons",
          actionButton("btn_5", "5"),
          actionButton("btn_10", "10"),
          actionButton("btn_20", "20"),
          actionButton("btn_50", "50"),
          actionButton("btn_100", "100"),
          actionButton("btn_200", "200")
      ),
      div(class = "ma-all-buttons",
          actionButton("draw_all", "Draw All"),
          actionButton("erase_all", "Erase All")
          )
    ),
    # 메인 패널에 뭐가 들어갈지
    mainPanel(
      plotOutput("candle_chart", width = "1000px", height = "800px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # BatchGetSymbols를 이용해서 데이터 가져오기
  stock_data <- reactive({
    validate(
      need(input$ticker != "", "Please input a stock")
    )
    stock_df <- BatchGetSymbols(
      tickers = input$ticker,
      first.date = input$date_range[1] - 300,
      # 200일 추세선 까지 그리기 위함
      # 입력 받은 날짜에서 -300일까지 확인
      last.date = input$date_range[2]
    )
    
    stock_df <- stock_df$df.tickers
    
    return(stock_df)
  })
  
  toggle_vars <- reactiveValues(
    toggle_5 = 0,
    toggle_10 = 0,
    toggle_20 = 0,
    toggle_50 = 0,
    toggle_100 = 0,
    toggle_200 = 0,
  )
  
  # 버튼 클릭 시 각 변수 값을 토글
  observeEvent(input$btn_5, {toggle_vars$toggle_5 <- ifelse(toggle_vars$toggle_5 == 0, 1, 0)})
  observeEvent(input$btn_10, { toggle_vars$toggle_10 <- ifelse(toggle_vars$toggle_10 == 0, 1, 0) })
  observeEvent(input$btn_20, { toggle_vars$toggle_20 <- ifelse(toggle_vars$toggle_20 == 0, 1, 0) })
  observeEvent(input$btn_50, { toggle_vars$toggle_50 <- ifelse(toggle_vars$toggle_50 == 0, 1, 0) })
  observeEvent(input$btn_100, { toggle_vars$toggle_100 <- ifelse(toggle_vars$toggle_100 == 0, 1, 0) })
  observeEvent(input$btn_200, { toggle_vars$toggle_200 <- ifelse(toggle_vars$toggle_200 == 0, 1, 0) })
  
  # draw_all, erase_all 버튼이 눌리면 모든 추세선을 그리거나 지우도록
  observeEvent(input$draw_all, {
    for (name in names(reactiveValuesToList(toggle_vars))) {
      toggle_vars[[name]] <- 1
    }
  })
  observeEvent(input$erase_all, {
    for (name in names(reactiveValuesToList(toggle_vars))) {
      toggle_vars[[name]] <- 0
    }
  })
  
  # 캔들차트 생성
  output$candle_chart <- renderPlot({
    
    stock_df <- stock_data()
    # NVDA[NVDA["ref.date"] >= "2024-12-03", ]
    
    chart_drawing_df <- stock_df[stock_df$ref.date >= input$date_range[1], ]
    candle_chart <- draw_candlestick_chart(chart_drawing_df)
    volume_chart <- draw_volume_chart(chart_drawing_df)
    
    # 여기는 추세선을 그리는 곳
    ma_settings <- list(
      "5"   = "#1F77B4",
      "10"  = "#FF7F0E",
      "20"  = "#2CA02C",
      "50"  = "#D62728",
      "100" = "#9A6FC0",
      "200" = "#8C564B"
    )
    
    for (period in names(ma_settings)) {
      toggle_var_name <- paste0("toggle_", period)
      
      if (toggle_vars[[toggle_var_name]] == 1) {
        start_d <- which(stock_df$ref.date == input$date_range[1]) - (as.numeric(period) - 1)
        color <- ma_settings[[period]]
        
        candle_chart <- candle_chart + calculate_moving_average(
          stock_df[start_d:length(stock_df$ref.date), ],
          as.numeric(period),
          color
        )
      }
    }
    
    grid.arrange(candle_chart, volume_chart,
                 nrow = 2, heights = c(4, 1))
  })
}

# Shiny 앱 실행
shinyApp(ui, server)

