library(shiny)
library(ggplot2)
library(DT)
ui <- fluidPage(
  titlePanel("EMI Calculator"),              
  tabsetPanel(
                tabPanel("Summary",
                fluidRow(
                column(3,
                inputPanel(
                h4("Enter the following Values"),
                br(),
                numericInput("principal","Principal Amount : ",NULL,0,1000000,100,240),
                numericInput("rate","Rate Of Intrest",NULL,0,100,0.01,240),
                numericInput("time","Period Of Loan In Months",NULL,0,1000,1,240),
                actionButton("Calc","Calculate")
                )),
                column(3,
                inputPanel(
                h4("Summary Data"),
                br(),
                tags$b("Total Amount"), "Payable :",
                tags$br(),
                textOutput("Total_amount_paid"),
                tags$br(),
                tags$b("Total Interest"), "Payable :",
                tags$br(),
                textOutput("Total_Interest"),
                tags$br(),
                tags$b("Principal :"),
                tags$br(),
                textOutput("Principal"),
                tags$br(),
                tags$b("Rate of Interest :"),
                tags$br(),
                textOutput("r")
                ),offset = 3)
                )),
                tabPanel("Charts",
                fluidRow(
                  column(4,
                 plotOutput("bar")
                ),
                column(4,
                       plotOutput("chart"), offset = 1
                )
                )),
                tabPanel("Monthwise Calculation",
                fluidRow(
                  column(12,
                         dataTableOutput("df"),
                         tags$head(tags$style("#df table {background-color: white;}", media="screen", type="text/css")),
                         tags$style(type="text/css", "#df th, td {border: medium solid #010101;text-align:center}")
                  )
                )),
                tabPanel("Documentation",
                  h1("What is", tags$b("EMI"), "?"),
                  tags$p("Equated Monthly Installment - EMI for short - is the amount payable every month to the bank or any other financial institution until the loan amount is fully paid off. It consists of the interest", br(),
                         "on loan as well as part of the principal amount to be repaid.", br(),
                         "The sum of principal amount and interest is divided by the tenure, i.e., number of months, in which the loan has to be repaid. This amount has to be paid monthly. The interest component of the EMI",br(),
                         "would be larger during the initial months and gradually reduce with each payment.", br(),
                         "The exact percentage allocated towards payment of the principal depends on the interest rate. Even though your monthly EMI payment won't change, the proportion of principal and interest components",br(),
                         "will change with time. With each successive payment, you'll pay more towards the principal and less in interest."),
                  tags$p("Here's the formula to calculate EMI:"),
                  img(src="http://emicalculator.net/wp-content/uploads/2011/06/emiformula.png?66d001"),
                  tags$p(
                    br(),
                    "where",
                    br(),
                    br(),
                    tags$b("E"),"is EMI",
                    br(),
                    br(),
                    tags$b("P"), "is Principal Loan Amount",
                    br(),
                    br(),
                    tags$b("r"), "is rate of interest calculated on monthly basis. (i.e., r = Rate of Annual interest/12/100. If rate of interest is 10.5% per annum, then r = 10.5/12/100=0.00875)",
                    br(),
                    br(),
                    tags$b("n"), "is loan term / tenure / duration in number of months"
                  ),
                  tags$p(
                    "Computing EMI for different combinations of principal loan amount, interest rates and loan term using", br(),
                    "the above EMI formula by hand is time consuming, complex and error prone. Our EMI calculator automates this calculation for you and gives you the result in a split second along with visual charts displaying",br(),
                    "payment schedule and the break-up of total payment."
                  )
                )
                )
)

server <- function(input, output) 
{
  observeEvent(input$Calc,{
    output$df <- renderDataTable({
      Rate <- rep.int((isolate(input$rate)/12),isolate(input$time))
      Monthly_rate <- rep.int((isolate(input$rate)/1200), isolate(input$time))
      Month <- c(1:isolate(input$time))
      EMI_num <- (isolate(input$principal))*(isolate(input$rate)/1200)*((1 + isolate(input$rate)/1200)^isolate(input$time))
      EMI_den <- ((1 + (isolate(input$rate)/1200))^isolate(input$time)) - 1
      EMI <- rep.int((EMI_num/EMI_den), isolate(input$time))
      princ <- c(isolate(input$principal), rep.int(0, (isolate(input$time) - 1)))
      for (i in (1:(isolate(input$time) - 1)))
      {
        princ[i + 1] <- (princ[i]*(1 + (isolate(input$rate)/1200))) - (EMI_num/EMI_den)
      }
      df <- data.frame(Month, Rate, EMI, princ, Monthly_rate)
      Interest <- c(df$princ * df$Monthly_rate)
      df <- data.frame(Month, Rate, EMI, princ, Interest)
      Amount <- c(df$princ + df$Interest)
      df <- data.frame(Month, Rate, Interest, princ, Amount, EMI)
      datatable(df) %>% formatStyle(
        columns = c("Amount","Interest"),
        target = "cell",
        backgroundColor = styleEqual(cbind(df$Amount,df$Interest), cbind(rep("skyblue", isolate(input$time)),rep("pink", isolate(input$time))))
      )
    })
  })
  observeEvent(input$Calc,{
    output$r <- renderText({input$rate})
  })
  observeEvent(input$Calc,{
    output$Principal <- renderText({input$principal})
  })
  observeEvent(input$Calc,{
    output$Total_interest_message <- renderText("The Total Interest You Pay :")
  })
  observeEvent(input$Calc,{
    output$Total_Interest <- renderText(
    {
    Rate <- rep.int((input$rate/12),input$time)
    Monthly_rate <- rep.int((input$rate/1200),input$time)
    Month <- c(1:input$time)
    EMI_num <- input$principal*(input$rate/1200)*((1 + input$rate/1200)^input$time)
    EMI_den <- ((1 + (input$rate/1200))^input$time) - 1
    EMI <- rep.int((EMI_num/EMI_den), input$time)
    princ <- c(input$principal, rep.int(0, (input$time - 1)))
    for (i in (1:(input$time - 1)))
    {
      princ[i + 1] <- (princ[i]*(1 + (input$rate/1200))) - (EMI_num/EMI_den)
    }
    df <- data.frame(princ, Monthly_rate)
    Interest <- c(df$princ * df$Monthly_rate)
    Total_Interest <- sum(Interest)
    })
  })
  observeEvent(input$Calc,{
    output$Total_amount_paid <- renderText(
    {
      Rate <- rep.int((input$rate/12),input$time)
      Monthly_rate <- rep.int((input$rate/1200),input$time)
      Month <- c(1:input$time)
      EMI_num <- input$principal*(input$rate/1200)*((1 + input$rate/1200)^input$time)
      EMI_den <- ((1 + (input$rate/1200))^input$time) - 1
      EMI <- rep.int((EMI_num/EMI_den), input$time)
      Total_EMI <- sum(EMI)
    })
  })
  observeEvent(input$Calc, {
    output$chart <- renderPlot({
    Rate <- rep.int((isolate(input$rate)/12),isolate(input$time))
    Monthly_rate <- rep.int((isolate(input$rate)/1200), isolate(input$time))
    Month <- c(1:isolate(input$time))
    EMI_num <- (isolate(input$principal))*(isolate(input$rate)/1200)*((1 + isolate(input$rate)/1200)^isolate(input$time))
    EMI_den <- ((1 + (isolate(input$rate)/1200))^isolate(input$time)) - 1
    EMI <- rep.int((EMI_num/EMI_den), isolate(input$time))
    princ <- c(isolate(input$principal), rep.int(0, (isolate(input$time) - 1)))
    for (i in (1:(isolate(input$time) - 1)))
    {
      princ[i + 1] <- (princ[i]*(1 + (isolate(input$rate)/1200))) - (EMI_num/EMI_den)
    }
    Total_EMI <- sum(EMI)
    df <- data.frame(princ, Monthly_rate)
    Interest <- c(df$princ * df$Monthly_rate)
    Total_Interest <- sum(Interest)
    difference <- (Total_EMI - Total_Interest)
    pie(c("Principal" = Total_EMI, "Total Interest Paid" = Total_Interest), main = "Break-Up of total payment", col = c("lightskyblue","pink"), radius = 1)
  })
  }
  )
  observeEvent(input$Calc, {
    output$bar <- renderPlot({
      Rate <- rep.int((isolate(input$rate)/12),isolate(input$time))
      Monthly_rate <- rep.int((isolate(input$rate)/1200), isolate(input$time))
      Month <- c(1:isolate(input$time))
      EMI_num <- (isolate(input$principal))*(isolate(input$rate)/1200)*((1 + isolate(input$rate)/1200)^isolate(input$time))
      EMI_den <- ((1 + (isolate(input$rate)/1200))^isolate(input$time)) - 1
      EMI <- rep.int((EMI_num/EMI_den), isolate(input$time))
      princ <- c(isolate(input$principal), rep.int(0, (isolate(input$time) - 1)))
      for (i in (1:(isolate(input$time) - 1)))
      {
        princ[i + 1] <- (princ[i]*(1 + (isolate(input$rate)/1200))) - (EMI_num/EMI_den)
      }
      df <- data.frame(Month, Rate, EMI, princ, Monthly_rate)
      Interest <- c(df$princ * df$Monthly_rate)
      df <- data.frame(Month, Rate, EMI, princ, Interest)
      Amount <- c(df$princ + df$Interest)
      df <- data.frame(Month, Rate, Interest, princ, Amount, EMI)
      barplot(height = rbind(df$Interest, df$EMI, df$princ), names.arg = c(1 : isolate(input$time)), beside = FALSE, main = "Summary", legend.text = c("Interest", "EMI", "Principal", "Amount Remaining"), col = c("red", "yellow", "blue", "white"), border = NA, xlab = "Month", ylab = "Rupees")
      lines(x = Month, y = df$Amount, type = "s", col = "white")
    })
  })
}

shinyApp(ui = ui, server = server)