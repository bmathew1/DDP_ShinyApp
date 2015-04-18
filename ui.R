#Assignment
library(shiny)
shinyUI(fluidPage(
    titlePanel("Loan Repayment"),
    sidebarLayout(
        sidebarPanel(
            numericInput('pv', 'Loan amount ($ or other currency)', 150000, step=1000),
            sliderInput('annualInterest', 'Annual interest rate(%)',value = 8, 
                        min = 0.25, max = 20, step = 0.25),
            numericInput('period', 'Loan period (in years)', 30, min=1, max=30),
            h5("Show payment contribution to principal chart"),
            checkboxInput("checkbox", label = "yes", value = TRUE),
            conditionalPanel(condition = "input.checkbox == true",
                             radioButtons("grDisplay", label = h5("Graph y axis"),
                                          choices = list("Values" = 1, "Percent" = 2),
                                          selected = 1))
        ),
        
        mainPanel(
            p("This application allows you to compute monthly loan repayment. You have to enter the loan amount, the annual interest rate and the terms of the loan in years in the panel on the left side of this page."),
            p("The ouput below shows the monthly repayment amount, the total payment, total interest. In addition, a breakdown of the amount that will be paid towards the interest and to the principal is shown in graph and tabular format."),
            p("The chart is visible by default, however it can be hidden by removing the check 'Show payment contribution to principal chart'. If the chart is visible, you can view the repayment as values or as percent by selecting the options under 'Graph y axis'"),

            h4('Monthly repayment is'), verbatimTextOutput("pmt"),
            h4('Total Payment'), verbatimTextOutput("totalPmt"),
            h4('Contribution to Interest'), verbatimTextOutput("contributionToInterest"), 
#             h4('Contribution to Principal'), verbatimTextOutput("contributionToPrincipal"),
            conditionalPanel(condition = "input.checkbox == true & input.grDisplay == 1",
                             h4('Monthly repayment breakdown plot'),
                             helpText("The chart shows the contribution of the repayment that goes towards the principal and interest."), 
                             plotOutput("paymentPlot")),
            conditionalPanel(condition = "input.checkbox == true & input.grDisplay == 2",
                             h4('Monthly repayment breakdown plot'),
                             helpText("The chart shows the contribution of the repayment that goes towards the principal and interest."), 
                             plotOutput("paymentPlot2")),
            h4("Payment breakdown table"),
            p("The breakdown shows the contribution of the repayment towards principal along with the principal remaining after each repayment. The remaing amount from the repayment goes towards interest. The coloumns and what they contain are:"),
            helpText("year: The year of loan repayment. This will have values 1 to the term of the loan entered on the left panel"),
            helpText("period: The month of repayment"),
            helpText("repayment: The loan repayment amount in each period"),
            helpText("toPrincipal: The contribution of the repayment towards principal. Remaining part of the repayment goes to interest"),
            helpText("percentToPrincipal: Percentage of repayment that goes to principal"),
            helpText("remainingPrincipal: Remaining principal amount that is yet to be paid. ie. What has to be paid to the loan provider, if you were to close the loan at the given point."),
            tableOutput("bdown"),
            p(strong("Note:"), "The last repayment amount may be slightly adjusted to correct the minor variation towards the end.")
        ))
)
)