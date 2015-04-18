#Assignment
#Formula from http://www.1728.org/loanform.htm
computePayment <- function(pv, rate, years) {
    per <- years * 12
    #     payment <- (rate + (rate / (((1+rate)^per) - 1) )) * pv
    payment <- (rate * pv) / (1 - (1+rate)^-per)
    return(payment)
}
computePaymentBreakdown <- function(pv,rate, years) {
    per <- years * 12
    pmt <- computePayment(pv, rate, years)
    df <- data.frame(year=numeric(per), period=numeric(per), 
                     repayment=numeric(per), 
                     #                      toInterest=numeric(per), 
                     toPrincipal=numeric(per), 
                     percentToPrincipal=numeric(per),
                     remainingPrincipal=numeric(per))
    df$repayment <- pmt
    remPrincipal <- pv
    #     for(i in 1:per) {
    #         df$year[i] <- format(ceiling(i/12),nsmall=0)
    #         df$period[i] <- format(i, nsmall=0)
    #         df$toPrincipal[i] <- P <- pmt-(remPrincipal * rate)
    #         df$percentToPrincipal[i] <- (P / pmt) * 100
    #         remPrincipal <- round(remPrincipal - P, 2)
    #         df$remainingPrincipal[i] <- remPrincipal
    #     }
    for(i in 1:per) {
        df$year[i] <- format(ceiling(i/12),nsmall=0)
        df$period[i] <- format(i, nsmall=0)
        df$toPrincipal[i] <- P <- pmt-(remPrincipal * rate)
        df$percentToPrincipal[i] <- (P / pmt) * 100
        remPrincipal <- remPrincipal - P
        df$remainingPrincipal[i] <- remPrincipal
    }
    #adjustment for last payment
    df$repayment[per]<-pmt+df$remainingPrincipal[per]
    #     df$toPrincipal[per]<-df$toPrincipal[per]+df$remainingPrincipal[per]
    df$remainingPrincipal[i] <- 0
    return(df)
}

library(shiny)
shinyServer( function(input, output) {
    output$pmt <- renderPrint({
        payment=computePayment(input$pv, (input$annualInterest)/ 1200,
                               (input$period))
        round(payment, digits=2)
    })
    
    output$contributionToInterest <- renderPrint({
        brkdwn<-computePaymentBreakdown(input$pv, (input$annualInterest)/1200,
                                        (input$period))
        interest = (sum(brkdwn$repayment) - sum(brkdwn$toPrincipal))
        round(interest, digits=2)
    })
    
    output$contributionToPrincipal <- renderPrint({
        brkdwn<-computePaymentBreakdown(input$pv, (input$annualInterest)/1200,
                                        (input$period))
        round(sum(brkdwn$toPrincipal), digits=2)
    })
    
    output$totalPmt <- renderPrint({
        brkdwn<-computePaymentBreakdown(input$pv, (input$annualInterest)/1200,
                                        (input$period))
        round(sum(brkdwn$repayment), digits=1)
    })
    
    #Percent plot
    output$paymentPlot2 <- renderPlot({
        bdw <- computePaymentBreakdown(input$pv, 
                                       (input$annualInterest)/1200,
                                       (input$period))
        totalInterest <- sum(bdw$repayment) - sum(bdw$toPrincipal)
        totalPayment <- sum(bdw$repayment)
        totalToPrincipal <- sum(bdw$toPrincipal)
        
        with(bdw, plot(period, toPrincipal, type="n", 
                       ylab="Contribution of repayment (%)",
                       xlab="Period (months)",
                       main="Repayment contribution to principal and interest",
                       ylim=c(0, 100)))
        #         with(bdw, lines(period, percentToPrincipal, col = "blue", lwd=5))
        #         with(bdw, lines(period, rep(100, dim(bdw)[1]), col="green", lwd=5))
        #Color the interest and principal areas
        polygon(c(1, bdw$period, dim(bdw)[1]), c(0, bdw$percentToPrincipal, 0),
                col="skyblue")
        polygon(c(bdw$period[1], bdw$period, bdw$period[length(bdw$period)]), 
                c(min(bdw$repayment), bdw$percentToPrincipal, min(bdw$repayment)),
                col="red")
        legend("bottomright", pch=15, col = c("red", "skyblue"), 
               legend = c("Pay towards interest", "Pay towards principal"))
        
        text((0.194)*as.numeric(bdw$period[length(bdw$period)]), 98, 
             paste0("Interest - ", round(((totalInterest/totalPayment)*100), digits=2), "%"))
        
        text((0.861)*as.numeric(bdw$period[length(bdw$period)]), 18, 
             paste0("Principal - ", round((totalToPrincipal/totalPayment)*100, digits=2) , "%"))
        
        text((0.5)*as.numeric(bdw$period[length(bdw$period)]), 50, 
             paste0("Total payment - ", "100%"))
    })
    
    #Value plot
    output$paymentPlot <- renderPlot({
        bdw <- computePaymentBreakdown(input$pv, 
                                       (input$annualInterest)/1200,
                                       (input$period))
        
        totalInterest <- sum(bdw$repayment) - sum(bdw$toPrincipal)
        totalPayment <- sum(bdw$repayment)
        totalToPrincipal <- sum(bdw$toPrincipal)
        
        with(bdw, plot(period, toPrincipal, type="n", 
                       ylab="Contribution of repayment ($ or currency entered)",
                       xlab="Period (months)",
                       main="Repayment contribution to principal and interest",
                       ylim=c(0, repayment[1])))
        #         with(bdw, lines(period, toPrincipal, col = "blue", lwd=5))
        #         with(bdw, lines(period, repayment, col="green", lwd=5))
        polygon(c(1, bdw$period, dim(bdw)[1]), c(0, bdw$toPrincipal, 0),
                col="skyblue")
        polygon(c(bdw$period[1], bdw$period, bdw$period[length(bdw$period)]), 
                c(min(bdw$repayment), bdw$toPrincipal, min(bdw$repayment)),
                col="red")
        legend("bottomright", pch=15, col = c("red", "skyblue"), 
               legend = c("Pay towards interest", "Pay towards principal"))
        
        text((0.194)*as.numeric(bdw$period[length(bdw$period)]), (0.955)*bdw$repayment, 
             paste0("Interest - $", format((totalInterest), nsmall=0)))
        
        text((0.861)*as.numeric(bdw$period[length(bdw$period)]), 
             (0.177)*bdw$repayment, paste0("Principal - $", 
                                           format(totalToPrincipal, nsmall=0)))
    
    text( (0.5)*as.numeric(bdw$period[length(bdw$period)]), 
          (0.5)*bdw$repayment, paste0("Total payment - $", format(totalPayment, nsmall=0)))
    })

output$bdown <- renderTable({computePaymentBreakdown(input$pv,
                                                     (input$annualInterest)/ 1200,
                                                     (input$period))})
} )