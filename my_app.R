#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")
#install.packages("tables")
library("ggplot2")
library("shiny")
library("shinydashboard")
#library(tables)
options(shiny.deprecation.messages=FALSE)
options(scipen = 999)
#ENTER filepath to data file from Dropbox below. Better solution coming soon.
BaseData = read.csv("~/my_app/Data2.csv")

ui = 
  
  dashboardPage(
    
    dashboardHeader(title = "SC Income Tax"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Personal Calculator", tabName = "calculator", icon = icon("calculator")),
        menuItem("Inputs", tabName = "inputs", icon = icon("edit")),
        menuItem("Optimizations", tabName = "optimizations", icon = icon("bar-chart"))
      )
    ),
    dashboardBody(
      
      tags$head(
        tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        
                        h1{
                        font-family: 'Arial';
                        font-weight: 500;
                        color: #333333;
                        }
                        #h2 {
                        #  font-family: 'Arial';
                        #  font-weight: 500;
                        #  color: #333333;
                        #}
                        #h3{
                        #  font-family: 'Arial';
                        #  font-weight: 500;
                        #  color: #333333
                        #}
                        h4{
                        font-family: 'Arial';
                        font-weight: 500;
                        color: #333333
                        }
                        
                        
                        "))
        ),
      
      tabItems(
        #tabItem(
        #  tabName = "welcome",
        #  h1("Hello")
        #),
        tabItem(
          tabName = "calculator",
          fluidRow(
            column(
              width = 12,
              box(
                title = "Basic Inputs & Calculation",  status = "primary", solidHeader = TRUE, width = 50,
                selectInput(inputId = "FilStatus", label = "Filing Status", choices = c("Single" = 1, "Head of Household" = 2, "Married Filing Jointly" = 3, "Married Filing Separately" = 4, "Surviving Spouse" = 5)),
                numericInput(inputId = "AdjGInc", label = "Adjusted Gross Income (AGI)", value = 0),
                conditionalPanel(condition = "input.FilStatus != 3", sliderInput(inputId = "PExempt", label = "Personal Exemptions", min = 1, max = 8, value = 1)),
                conditionalPanel(condition = "input.FilStatus == 3", sliderInput(inputId = "PExemptM", label = "Personal Exemptions", min = 2, max = 8, value = 1)),
                radioButtons(inputId = "sixtyfive", label = "65 or older?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                radioButtons(inputId = "Itemize", label = "Itemizing under Schedule A?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                conditionalPanel(condition = "input.Itemize == 'TRUE'", numericInput(inputId = "ItemizeValue", label = "Total itemized deductions", value = 0)),
                actionButton(inputId = "CalcButton", label = "Calculate Tax"),
                h4("Total South Carolina Taxable Income:", textOutput(outputId = "StateTaxInc1")),
                h4("Total South Carolina Individual Income Tax:", textOutput(outputId = "TotalStateTax")),
                h4("Effective Tax Rate (% of AGI):", textOutput(outputId = "EffRate"))
              )
            ),
            box(
              title = "Filing Status-Specific Options", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              conditionalPanel(
                condition = "input.FilStatus != 3",
                "Your Filing Status selection does not have any extra options."
              ),
              conditionalPanel(
                condition = "input.FilStatus == 4",
                radioButtons(inputId = "SepBool", label = "Is your separately-filing spouse claiming you as an exemption?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE)
              ),
              conditionalPanel(
                condition = "input.FilStatus == 3",
                radioButtons(inputId = "SpouseAge", label = "Is your jointly-filing spouse 65 or older?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                radioButtons(inputId = "SpouseBlind", label = "Is your jointly-filing spouse legally blind?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                radioButtons(inputId = "SpouseVol", label = "Does your spouse qualify for a volunteer deduction?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                uiOutput("DualSlider")
              )
            ),
            box(
              title = "State Additions to Taxable Income", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              numericInput(inputId = "NRLoss", label = "Enter your total out-of-state losses", value = 0),
              numericInput(inputId = "ResExp", label = "Enter your total expenses related to armed forces reserve income, if any", value = 0),
              numericInput(inputId = "NRInterest", label = "Enter the total interest you made on obligations outside of South Carolina", value = 0),
              numericInput(inputId = "OtherAdd", label = "Enter any other additions to federal taxable income", value = 0),
              conditionalPanel(
                condition = "input.Itemize == 'TRUE'",
                numericInput(inputId = "Addback", label = "Enter line 9 from the Federal 1040 Schedule A form", value = 0)
              )
            ),
            box(
              title= "State Deductions & Exemptions", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              numericInput(inputId = "StateRef", label = "Enter your State Tax Refund, included on federal return", value = 0),
              numericInput(inputId = "DisInc", label = "Enter your estimated total/permanent disability income", value = 0),
              numericInput(inputId = "NRInc", label = "Enter your total estimated income/gains from out-of-state", value = 0),
              numericInput(inputId = "CapGInc", label = "Enter your estimated income from capital gains", value = 0),
              radioButtons(inputId = "VolDed", label = "Are you a volunteer who qualifies for a volunteer deduction? (see SCDOR site for qualification)", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
              numericInput(inputId = "CollegeCont", label = "Enter amount contributed to a qualified 529 Plan through the SC College Investment Program", value = 0),
              numericInput(inputId = "ActBusInc", label = "Enter your estimated income from an active business or trade", value = 0),
              numericInput(inputId = "FedObInc", label = "Interest recieved from federal obligations", value = 0),
              numericInput(inputId = "NGorResInc", label = "Enter your estimated income from National Guard or armed forces annual training and weekend drills", value = 0),
              numericInput(inputId = "SSInc", label = "Enter your estimated income from Social Security or railroad retirement", value = 0),
              numericInput(inputId = "RetInc", label = "Enter your estimated Retirement Income", value = 0),
              numericInput(inputId = "NegFedInc", label = "Enter negative federal taxable income, if present", value = 0),
              radioButtons(inputId = "SupBool", label = "Did you have any subsistence allowance this year?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
              conditionalPanel(
                condition = "input.SupBool == 'TRUE'",
                sliderInput(inputId = "SupDaysInc", label = "How many days did you recieve subsistence income?", min = 1, max = 366, value = 1)
              ),
              radioButtons(inputId = "ChildBool", label = "Do you have any children age 6 or younger?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
              conditionalPanel(
                condition = "input.ChildBool == 'TRUE'",
                conditionalPanel(condition = "input.FilStatus != 3", uiOutput("ChildSlider")),
                conditionalPanel(condition = "input.FilStatus == 3", uiOutput("ChildSliderM")),
                h4("Note: slider is linked to main personal exemptions slider. Make sure you have accounted for enough personal exemptions before selecting value.")
              ),
              numericInput(inputId = "ConsProt", label = "Enter estimated income recieved from Consumer Protection Services", value = 0),
              numericInput(inputId = "OtherInc", label = "Enter all other income under possible subtractions (see SC 1040 Instructions)", value = 0),
              radioButtons(inputId = "Blind", label = "Are you legally blind?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE)
            ),
            box(
              title= "Primary State Credits", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              numericInput(inputId = "ChildCredit", label = "Enter your estimated childcare costs", value = 0),
              uiOutput("MultiChildren"),
              radioButtons(inputId = "NumNRBool", label = "Do you have income from any state other than South Carolina?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
              conditionalPanel(condition = "input.NumNRBool ==  'TRUE'", selectInput(inputId = "NumberNR", label = "How many states other than South Carolina did you have income from in the most recent tax year?", choices = c(0:6), selected = 0)),
              conditionalPanel(condition = "input.NumberNR >= 1 && input.NumNRBool == 'TRUE'", numericInput(inputId = "NR1", label = "Gross Income from state #1 that is taxable in SC", value = 0), numericInput(inputId = "NRT1", label = "Tax from other state on income taxable in that state #1 AND South Carolina", value = 0)),
              conditionalPanel(condition = "input.NumberNR >= 2 && input.NumNRBool == 'TRUE'", numericInput(inputId = "NR2", label = "Gross Income from state #2 that is taxable in SC", value = 0), numericInput(inputId = "NRT2", label = "Tax from other state on income taxable in that state #2 AND South Carolina", value = 0)),
              conditionalPanel(condition = "input.NumberNR >= 3 && input.NumNRBool == 'TRUE'", numericInput(inputId = "NR3", label = "Gross Income from state #3 that is taxable in SC", value = 0), numericInput(inputId = "NRT3", label = "Tax from other state on income taxable in that state #3 AND South Carolina", value = 0)),
              conditionalPanel(condition = "input.NumberNR >= 4 && input.NumNRBool == 'TRUE'", numericInput(inputId = "NR4", label = "Gross Income from state #4 that is taxable in SC", value = 0), numericInput(inputId = "NRT4", label = "Tax from other state on income taxable in that state #4 AND South Carolina", value = 0)),
              conditionalPanel(condition = "input.NumberNR >= 5 && input.NumNRBool == 'TRUE'", numericInput(inputId = "NR5", label = "Gross Income from state #5 that is taxable in SC", value = 0), numericInput(inputId = "NRT5", label = "Tax from other state on income taxable in that state #5 AND South Carolina", value = 0)),
              conditionalPanel(condition = "input.NumberNR >= 6 && input.NumNRBool == 'TRUE'", numericInput(inputId = "NR6", label = "Gross Income from state #6 that is taxable in SC", value = 0), numericInput(inputId = "NRT6", label = "Tax from other state on income taxable in that state #6 AND South Carolina", value = 0))
            ),
            box(
              title= "Other State Credits", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              radioButtons(inputId = "OtherBool", label = "Do you have any other credits you can claim? (See SC1040tc)", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
              conditionalPanel(
                condition = "input.OtherBool == 'TRUE'",
                numericInput(inputId = "Credit101", label = "Enter any carry-over of unused credits", value = 0),
                numericInput(inputId = "Credit44", label = "Enter your health insurance premium from the previous tax year", value = 0),
                numericInput(inputId = "Credit4", label = "Enter your estimated New Jobs Credit, if present (line 16 on your SC1040TC-4 form)", value = 0),
                numericInput(inputId = "Credit19", label = "Enter your estimated Land Donation for Conservation credit (line 8 on your SC1040TC-19 form)", value = 0),
                numericInput(inputId = "Credit1", label = "Enter your estimated Drip/Trickly Irrigation Systems Credit (line 10 on your SC1040TC-1 form)", value = 0),
                numericInput(inputId = "Credit2", label = "Enter your estimated contracting payemnts eligible for the Minority Business Credit (see SC1040TC-2 form)", value = 0),
                numericInput(inputId = "Credit3", label = "Enter your estimated costs for construction/restoration eligible for the Water Resources Credit (line 3 on your SC1040TC-3 form)", value = 0),
                numericInput(inputId = "Credit102", label = "Enter your total nursing home costs you have personally covered that are eligible for the Nursing Home Credit (see SC1040TC for more information)", value = 0),
                numericInput(inputId = "Credit4SB", label = "Enter your estimated Small Business Jobs Credit Total (line 14 on your SC1040TC-4SB form)", value = 0),
                numericInput(inputId = "Credit9", label = "Enter your estimated Childcare Program Credit (Part III, line 5 on your SC1040TC-9 form)", value = 0),
                numericInput(inputId = "Credit11", label = "Enter your estimated State Capital Investment Credit (line 10 on your SC1040TC-11 form)", value = 0),
                numericInput(inputId = "Credit12", label = "Enter your estimated Family Independence Payments Credit (line E5 on your SC1040TC-12 form)", value = 0),
                numericInput(inputId = "Credit14", label = "Enter your estimated Commmunity Development Credit (line 5 on your SC1040TC-14 form)", value = 0),
                numericInput(inputId = "Credit18", label = "Enter your estimated Research Expenses Tax Credit (line 4 on your SC1040TC-18 form)", value = 0),
                numericInput(inputId = "Credit20", label = "Enter your estimated Brownfields Cleanup Tax Credit (Part III, line 1 on your SC1040TC-20 form)", value = 0),
                numericInput(inputId = "Credit21", label = "Enter your estimated Historic Structure Tax Credit (line 5 on your SC1040TC-21 form)", value = 0),
                numericInput(inputId = "Credit22", label = "Enter your estimated Historic Residential Structure Tax Credit (line 5 on your SC1040TC-22 form)", value = 0),
                numericInput(inputId = "Credit23", label = "Enter your estimated Credit for Rehabilitated Textiles (line 5 on your SC1040TC-23 form)", value = 0),
                numericInput(inputId = "Credit24", label = "Enter your estimated Commercials Credit (line 4 on your SC1040TC-24 form)", value = 0),
                numericInput(inputId = "Credit25", label = "Enter your estimated Motion Pictures Credit", value = 0),
                numericInput(inputId = "Credit26", label = "Enter your estimated Venture Capital Investment Credit (line 3 on your SC1040TC-26 form)", value = 0),
                numericInput(inputId = "Credit27", label = "Enter any premiums paid this year for replacement health insurance coverage that falls under the Health Insurance Pool Credit", value = 0),
                numericInput(inputId = "Credit28", label = "Enter your estimated SC Quality Forum Credit (line 4 on your SC1040TC-28 form)", value = 0),
                numericInput(inputId = "Credit29", label = "Enter your estimated Qualified Retirement Plan Contribution Credit (line 6 on your SC1040TC-29 form)", value = 0),
                numericInput(inputId = "Credit30", label = "Enter your estimated Port Cargo Volume Increase Credit (line 3 on your SC1040TC-23 form)", value = 0),
                numericInput(inputId = "Credit31", label = "Enter your estimated Retail Facilities Revitalization Credit (line 5 on your SC1040TC-31 form)", value = 0),
                radioButtons(inputId = "Credit32", label = "Do you qualify for the Premarital Preparation Course Credit?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                numericInput(inputId = "Credit36", label = "Enter your estimated Alternative Motor Vehicle Credit (line 7 on your SC1040TC-35 form)", value = 0),
                numericInput(inputId = "Credit35", label = "Enter your estimated Industry Partnership Fund Credit (line 3 on your SC1040TC-36 form)", value = 0),
                numericInput(inputId = "Credit38", label = "Enter your estimated Solar Energy or Small Hydropower System Credit (line 7 on your SC1040TC-38 form)", value = 0),
                numericInput(inputId = "Credit40", label = "Enter your estimated Ethanol or Biodiesel Production Credit (line 1 on your SC1040TC-40 form)", value = 0),
                numericInput(inputId = "Credit41", label = "Enter your estimated Renewable Fuel Facility Credits (line 9 on your SC1040TC-41 form)", value = 0),
                numericInput(inputId = "Credit43", label = "Enter your estimated Residential Retrofit Credit (line 13 on your SC1040TC-43 form)", value = 0),
                numericInput(inputId = "Credit45", label = "Enter your estimated Apprenticeship Credit (line 6 on your SC1040TC-45 form)", value = 0),
                numericInput(inputId = "Credit48", label = "Enter your estimated Plug-In Hybrid Vehicle Credit (line 2 on your SC1040TC-48 form)", value = 0),
                selectInput(inputId  = "Credit51", label = "How many dear carcasses did you donate in the last year that qualify for the Venison for Charity Credit?", choices = 0:25),
                numericInput(inputId = "Credit52", label = "Enter your estimated Fire Sprinkler System Credit (line 3 on your SC1040TC-52 form)", value = 0),
                radioButtons(inputId = "Credit53", label = "Do you qualify for the Energy Efficient Manufactured Home Credit?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                numericInput(inputId = "Credit54", label = "Enter your estimated Credit for Manufacturing Renewable Energy Systems (line 3 on your SC1040TC-54 form)", value = 0),
                numericInput(inputId = "Credit55", label = "Enter your estimated Abandoned Buildings Revitalization Credit (line 11 on your SC1040TC-35 form)", value = 0),
                numericInput(inputId = "Credit56", label = "Enter your estimated Angel Investor Credit (line 13 on your SC1040TC-56 form)", value = 0),
                numericInput(inputId = "Credit57", label = "Enter your estimated Exceptional Needs Children Educational Credit (See SC1040TC-57 form)", value = 0)
              )
            )
          )
        ),
        tabItem(
          tabName = "inputs",
          fluidRow(
            tabBox(
              tabPanel(
                title = "Brackets", status = "primary", color = "navy", solidHeader = TRUE,
                selectInput(inputId = "FedChoose", label = "Which definition of income do you wish to use as a base for calculations?", choices = c("State Taxable Income" = 1, "Federal Taxable Income, No Non-Residents" = 2, "Federal Taxable Income" = 3)),
                selectInput(inputId = "BracketSelect", label = "Number of Brackets", choices = seq(1,6,1), selected = 6, width = '90%'),
                numericInput(inputId = "B1", label = "Bracket 1", value = 0, width = '90%'), numericInput(inputId = "R1", label = "Rate 1", value = 0, width = '90%'),
                conditionalPanel(condition = "input.BracketSelect >= 2", numericInput(inputId = "B2", label = "Bracket 2", value = 2880, width = '90%'), numericInput(inputId = "R2", label = "Rate 2", value = 3, width = '90%')),
                conditionalPanel(condition = "input.BracketSelect >= 3", numericInput(inputId = "B3", label = "Bracket 3", value = 5760, width = '90%'), numericInput(inputId = "R3", label = "Rate 3", value = 4, width = '90%')),
                conditionalPanel(condition = "input.BracketSelect >= 4", numericInput(inputId = "B4", label = "Bracket 4", value = 8640, width = '90%'), numericInput(inputId = "R4", label = "Rate 4", value = 5, width = '90%')),
                conditionalPanel(condition = "input.BracketSelect >= 5", numericInput(inputId = "B5", label = "Bracket 5", value = 11520, width = '90%'), numericInput(inputId = "R5", label = "Rate 5", value = 6, width = '90%')),
                conditionalPanel(condition = "input.BracketSelect >= 6", numericInput(inputId = "B6", label = "Bracket 6", value = 14400, width = '90%'), numericInput(inputId = "R6", label = "Rate 6", value = 7, width = '90%'))
              ),
              tabPanel(
                title = "Credit Add-Back",
                radioButtons(inputId = "CredBool", label = "Do you wish to add an add-back tax credit?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
                conditionalPanel(
                  condition = "input.CredBool == 'TRUE'",
                  #h3("FUNCTIONALITY COMING SOON!")
                  selectInput(inputId = "CredSelect", label = "Number of Add-Back Credit Brackets", choices = seq(1,8,1), selected = 8, width = '90%'),
                  numericInput(inputId = "C1", label = "Bracket 1", value = 0, width = '90%'), numericInput(inputId = "D1", label = "Rate 1", value = 87.5, width = '90%'),
                  conditionalPanel(condition = "input.CredSelect >= 2", numericInput(inputId = "C2", label = "Bracket 2", value = 5000, width = '90%'), numericInput(inputId = "D2", label = "Rate 2", value = 75, width = '90%')),
                  conditionalPanel(condition = "input.CredSelect >= 3", numericInput(inputId = "C3", label = "Bracket 3", value = 10000, width = '90%'), numericInput(inputId = "D3", label = "Rate 3", value = 62.5, width = '90%')),
                  conditionalPanel(condition = "input.CredSelect >= 4", numericInput(inputId = "C4", label = "Bracket 4", value = 15000, width = '90%'), numericInput(inputId = "D4", label = "Rate 4", value = 50, width = '90%')),
                  conditionalPanel(condition = "input.CredSelect >= 5", numericInput(inputId = "C5", label = "Bracket 5", value = 20000, width = '90%'), numericInput(inputId = "D5", label = "Rate 5", value = 37.5, width = '90%')),
                  conditionalPanel(condition = "input.CredSelect >= 6", numericInput(inputId = "C6", label = "Bracket 6", value = 25000, width = '90%'), numericInput(inputId = "D6", label = "Rate 6", value = 25, width = '90%')),
                  conditionalPanel(condition = "input.CredSelect >= 7", numericInput(inputId = "C7", label = "Bracket 7", value = 30000, width = '90%'), numericInput(inputId = "D7", label = "Rate 7", value = 12.5, width = '90%')),
                  conditionalPanel(condition = "input.CredSelect >= 8", numericInput(inputId = "C8", label = "Bracket 8", value = 35000, width = '90%'), numericInput(inputId = "D8", label = "Rate 8", value = 0, width = '90%')),
                  sliderInput(inputId = "AddExAmount", label = "Enter how much each personal exemption will add to add-back income", min = 0, max = 10000, value = 5000, step = 250)
                )
              )
            ),
            box(
              title = "Deductions & Exemptions for Optimization", status = "primary", color = "navy", solidHeader = TRUE,
              radioButtons(inputId = "ITEBool", label = "Do you wish to include itemized deductions?", choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE),
              sliderInput(inputId = "StdDeduction1", label = "Enter the base state standard deduction amount for Single filers", min = 0, max = 12600, value = 6300, step = 100),
              sliderInput(inputId = "StdDeduction2", label = "Enter the base state standard deduction amount for Head of Household filers", min = 0, max = 18500, value = 9250, step = 50),
              sliderInput(inputId = "StdDeduction3", label = "Enter the base state standard deduction amount for Married Filing Jointly filers", min = 0, max = 25200, value = 12600, step = 100),
              sliderInput(inputId = "StdDeduction4", label = "Enter the base state standard deduction amount for Married Filing Separately", min = 0, max = 12600, value = 6300, step = 100),
              sliderInput(inputId = "StdDeduction5", label = "Enter the base state standard deduction amount for Surviving Spouse filers", min = 0, max = 25200, value = 12600, step = 100),
              sliderInput(inputId = "PExAmount", label = "Enter the amount exempted for each personal exemption", min = 0, max = 8000, value = 4000, step = 100),
              sliderInput(inputId = "BaseCut", label = "Enter the multiplier for personal exemption cutoffs", min = 0, max = 1000000, value = 154950),
              radioButtons(inputId = "BusTaxBool", label = "Do you wish tax on an Active Business or Trade to be taxed as standard income?", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE)
            ),
            box(
              title = "Other Deductions & Exemptions", status = "primary", color = "navy", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              sliderInput(inputId = "RetDeduction1", label = "Enter the retirement add-on to standard deduction for Single filers", min = 0, max = 3100, value = 1550, step = 50),
              sliderInput(inputId = "RetDeduction2", label = "Enter the retirement add-on to standard deduction for Head of Household filers", min = 0, max = 3100, value = 1550, step = 50),
              sliderInput(inputId = "RetDeduction3", label = "Enter the retirement add-on to standard deduction for Married Filing Jointly filers", min = 0, max = 5000, value = 2500, step = 50),
              sliderInput(inputId = "RetDeduction4", label = "Enter the retirement add-on to standard deduction for Married Filing Separately filers", min = 0, max = 3100, value = 1550, step = 50),
              sliderInput(inputId = "RetDeduction5", label = "Enter the retirement add-on to standard deduction for Surviving Spouse filers", min = 0, max = 7500, value = 3750, step = 50),
              sliderInput(inputId = "MaxChildCredit", label = "Enter the cap for each child under the Childcare Costs Tax Credit", min = 0, max = 10000, value = 3000, step = 200),
              selectInput(inputId = "NumChildCredit", label = "Enter the maximum number of children allowed under the Childcare Costs Tax Credit", choices = c(1,2,3,4,5), selected = 2),
              sliderInput(inputId = "PerChildCredit", label = "Enter the percentage of childcare costs used to calculate the Childcare Costs Tax Credit", min = 0, max = .2, value = .07, step = .01),
              sliderInput(inputId = "DualIncRate", label = "Enter the percentage of the lesser wage earner's income to be eligible for the Dual Wage Earner Credit", min = 0, max = .03, value = .007, step = .001),
              sliderInput(inputId = "DualIncCap", label = "Enter a cap for the Dual Wage Earner Credit", min = 0, max = 1000, value = 210, step = 10)
            )
          )
        )
        ,
        tabItem(
          tabName = "optimizations",
          sidebarLayout(
            column(
              width = 4,
              sidebarPanel(
                width = 12,
                selectInput(inputId = "FedPlotChoose", label = "Which definition of income do you wish to order your graph by?", choices = c("State Taxable Income" = 1, "Federal Taxable Income" = 3, "Adjusted Gross Income (AGI)" = 4), width = 450),
                actionButton(inputId = "Optimize", label = "Calculate"),
                fluidRow(column(width = 8, h5("Original Revenue (2013):")), column(width = 4, h5("3407583078"))),
                fluidRow(column(width = 8, h5("Total South Carolina IIT Revenue:")), column(width = 4, h5(textOutput(outputId = "TaxOut")))),
                fluidRow(column(width = 8, h5("Percentage Change in Revenue:")), column(width = 4, h5(textOutput("RevSlider")))),
                fluidRow(column(width = 8, h5("Gross Change in Revenue:")), column(width = 4, h5(textOutput("DAmount")))),
                #fluidRow(column(width = 8, h5("NROW")), column(width = 4, h5(textOutput("testval")))),
                actionButton(inputId = "GraphReset", label = "Load/Reset Selected Visuals")
              ),
              sidebarPanel(
                width = 12,
                h4("Window for Additional Data & Visual Aids")
              )
            ),
            sidebarPanel(
              width = 8,
              plotOutput("CompPlot", width = "95%"),
              plotOutput("C2mpPlot", width = "95%"),
              plotOutput("C3mpPlot", width = "95%")
            )
          )
        )
      )
        )
        )

server = function(input, output) {
  
  output$ChildSlider = reactiveUI(function() {
    sliderInput("ChildSlider", label = "How many dependents under age 6 do you have?", min = 0,  max = (input$PExempt - 1), value = 0)
  })
  output$ChildSliderM = reactiveUI(function() {
    sliderInput("ChildSliderM", label = "How many dependents under age 6 do you have?", min = 0,  max = (input$PExemptM - 2), value = 0)
  })
  output$DualSlider = reactiveUI(function() {
    sliderInput(inputId = "DualSlider", label = "How much does the higher-earning spouse in your jointly-filing marriage make?", min = ((input$AdjGInc / 2) + 1), max = input$AdjGInc, value = ((input$AdjGInc / 2) + 1))
  })
  output$MultiChildren = reactiveUI(function() {
    selectInput(inputId = "MultiChildren", label = "", choices = 1:input$NumChildCredit, selected = 2)
  })
  
  LowerBoundFunc = function(input, bound){
    if(input >= bound){
      return(input)
    }
    else{
      return(bound)
    }
  }
  UpperBoundFunc = function(input, bound){
    if(input <= bound){
      return(input)
    }
    else{
      return(bound)
    }
  }
  RateFunctionShiny = function(ICN, B1, RR1, B2, RR2, B3, RR3, B4, RR4, B5, RR5, B6, RR6, ID){
    R1 = RR1 / 100
    R2 = RR2 / 100
    R3 = RR3 / 100
    R4 = RR4 / 100
    R5 = RR5 / 100
    R6 = RR6 / 100
    Rate = 0
    Tax = 0
    if(ICN >= B6){
      Rate = R6
      Tax = B1*R1 + (B2-B1)*R1 + (B3-B2)*R2 + (B4-B3)*R3 + (B5-B4)*R4 + (B6-B5)*R5 + (ICN - B6)*R6
    }
    else{
      if(ICN < B6 && ICN >= B5){
        Rate = R5
        Tax = B1*R1 + (B2-B1)*R1 + (B3-B2)*R2 + (B4-B3)*R3 + (B5-B4)*R4 + (ICN - B5)*R5
      }
      else{
        if(ICN < B5 && ICN >= B4){
          Rate = R4
          Tax = B1*R1 + (B2-B1)*R1 + (B3-B2)*R2 + (B4-B3)*R3 + (ICN - B4)*R4
        }
        else{
          if(ICN < B4 && ICN >= B3){
            Rate = R3
            Tax = B1*R1 + (B2-B1)*R1 + (B3-B2)*R2 + (ICN - B3)*R3
          }
          else{
            if(ICN < B3 && ICN >= B2){
              Rate = R2
              Tax = B1*R1 + (B2-B1)*R1 + (ICN - B2)*R2
            }
            else{
              if(ICN < B2 && ICN >= B1){
                Rate = R1
                Tax = B1*R1 + (ICN - B1)*R1
              }
            }
          }
        }
      }
    }  
    if(ID == 1){
      return(Rate)
    }
    else{
      return(Tax)
    }
  }
  TotalRateFunc = function(selectnum, STI){
    if(selectnum == 1){
      return(RateFunctionShiny(as.numeric(STI),input$B1,input$R1,1000000000,0,1000000001,0,10000000002,0,10000000003,0,10000000004,0,2))
    }
    if(selectnum == 2){
      return(RateFunctionShiny(as.numeric(STI),input$B1,input$R1,input$B2,input$R2,1000000000,0,10000000001,0,10000000002,0,10000000003,0,2))
    }
    if(selectnum == 3){
      return(RateFunctionShiny(as.numeric(STI),input$B1,input$R1,input$B2,input$R2,input$B3,input$R3,1000000000,0,10000000001,0,10000000002,0,2))
    }
    if(selectnum == 4){
      return(RateFunctionShiny(as.numeric(STI),input$B1,input$R1,input$B2,input$R2,input$B3,input$R3,input$B4,input$R4,1000000000,0,10000000001,0,2))
    }
    if(selectnum == 5){
      return(RateFunctionShiny(as.numeric(STI),input$B1,input$R1,input$B2,input$R2,input$B3,input$R3,input$B4,input$R4,input$B5,input$R5,1000000000,0,2))
    }
    if(selectnum == 6){
      return(RateFunctionShiny(as.numeric(STI),input$B1,input$R1,input$B2,input$R2,input$B3,input$R3,input$B4,input$R4,input$B5,input$R5,input$B6,input$R6,2))
    }
  }
  StandardDeductionFunc = function(filio, sixtyfive, SpouseAge, Blind, SpouseBlind, Itemize, ItemizeValue, StdDeduction1, StdDeduction2, StdDeduction3, StdDeduction4, StdDeduction5, RetDeduction1, RetDeduction2, RetDeduction3, RetDeduction4, RetDeduction5){
    
    BC = 0
    
    if(Itemize == FALSE){
      if(filio == 1){
        BC = BC + StdDeduction1
      }
      if(filio == 2){
        BC = BC + StdDeduction2
      }
      if(filio == 3){
        if(SpouseAge == TRUE){
          BC = BC + StdDeduction3
        }
        else{
          BC = BC + StdDeduction4
        }
      }
      if(filio == 4){
        if(SpouseAge == TRUE){
          BC = BC + (StdDeduction4 + RetDeduction4)
        }
        else{
          BC = BC + StdDeduction4
        }
      }
      if(filio == 5){
        BC = BC + StdDeduction5
      }
      
      
      if(sixtyfive == TRUE){
        if(filio == 1){
          BC = BC + RetDeduction1
        }
        if(filio == 2){
          BC = BC + RetDeduction2
        }
        if(filio == 3){
          if(SpouseAge == TRUE){
            BC = BC + RetDeduction3
          }
          else{
            BC = BC + (RetDeduction3 / 2)
          }
        }
        if(filio == 4){
          if(SpouseAge == TRUE){
            BC = BC + (RetDeduction4 * 2)
          }
          else{
            BC = BC + RetDeduction4
          }
        }
        if(filio == 5){
          BC = BC + RetDeduction5
        }
      }
      
      
      if(Blind == TRUE){
        if(filio == 1 | filio == 2){
          BC = BC + RetDeduction1
        }
        else{
          if(filio == 3 & SpouseBlind == TRUE){
            BC = BC + RetDeduction3
          }
          else{
            BC = BC + (RetDeduction3 / 2)
          }
        }
      }
      else{
        if(SpouseBlind == TRUE){
          BC = BC + RetDeduction4
        }
        else{
          BC = BC
        }
      }
    }
    if(Itemize == TRUE){
      BC = BC + ItemizeValue
    }
    
    
    
    return(BC)
  }
  ExemptFunc = function(PExAmount, AdjGInc, PExempt, PExemptM, FilStatus, BaseCut){
    
    DC = 0
    TempEx = PExAmount
    
    if(AdjGInc >= 258250 & FilStatus == 1){
      TempEx = LowerBoundFunc((PExAmount - (.02*PExAmount)*((AdjGInc - BaseCut*(5/3)) / 2500)), 0)
    }
    if(AdjGInc >= 284050 & FilStatus == 2){
      TempEx = LowerBoundFunc((PExAmount - (.02*PExAmount)*((AdjGInc - BaseCut*1.83317199) / 2500)), 0)
    }
    if(AdjGInc >= 309900 & FilStatus == 3){
      TempEx = LowerBoundFunc((PExAmount - (.02*PExAmount)*((AdjGInc - BaseCut*2) / 2500)), 0)
    }
    if(AdjGInc >= 154950 & FilStatus == 4){
      TempEx = LowerBoundFunc((PExAmount - (.02*PExAmount)*((AdjGInc - BaseCut) / 2500)), 0)
    }
    if(AdjGInc >= 309900 & FilStatus == 5){
      TempEx = LowerBoundFunc((PExAmount - (.02*PExAmount)*((AdjGInc - BaseCut*2) / 2500)), 0)
    }
    
    if(FilStatus != 3){
      DC = DC + TempEx*PExempt
    }
    if(FilStatus == 3){
      DC = DC + TempEx*PExemptM
    }
    
    return(DC)
    
  }
  FederalDeductionFunc = function(PExempt, PExemptM, sixtyfive, SpouseAge, FilStatus, Itemize, ItemizeValue, Blind, SpouseBlind, StdDeduction1, StdDeduction2, StdDeduction3, StdDeduction4, StdDeduction5, RetDeduction1, RetDeduction2, RetDeduction3, RetDeduction4, RetDeduction5, PExAmount){
    
    DC = 0
    
    #Standard Deduction
    if(Itemize == FALSE){
      DC = StandardDeductionFunc(FilStatus, sixtyfive, SpouseAge, Blind, SpouseBlind, Itemize, ItemizeValue, StdDeduction1, StdDeduction2, StdDeduction3, StdDeduction4, StdDeduction5, RetDeduction1, RetDeduction2, RetDeduction3, RetDeduction4, RetDeduction5)
    }
    
    #Personal Exemptions
    DC = DC + ExemptFunc(input$PExAmount, input$AdjGInc, input$PExempt, input$PExemptM, FilStatus, input$BaseCut)
    
    return(DC)
  }
  StateAdditionFunc = function(Addback, NRLoss, ResExp, NRInterest, OtherAdd, filio, Itemize, ItemizeValue, sixtyfive, SpouseAge, Blind, SpouseBlind, ID){
    
    BC = 0
    DC = 0
    
    #1040-1a: State tax addback if itemizing
    if(Itemize == TRUE){
      cat1 = ItemizeValue - StandardDeductionFunc(filio, sixtyfive, SpouseAge, Blind, SpouseBlind, Itemize, ItemizeValue)
      cat2 = Addback
      UpperBoundFunc(cat1, cat2)
    }
    else{
      DC = 0
    }
    
    #1040-1b: Out-of-state losses
    BC = BC + NRLoss
    DC = DC + NRLoss
    
    #1040-1c: Expenses related to armed forces reserve income
    DC = DC + ResExp
    
    #1040-1d: Total interest made on obligations in states other than South Carolina
    BC = BC + NRInterest
    DC = DC + NRInterest
    
    #1040-1e: Other additions to federal income
    DC = DC + OtherAdd
    
    if(ID == 0){
      return(DC)
    }
    if(ID == 1){
      return(BC)
    }
  }
  StateDeductionFunc = function(StateRef, DisInc, NRInc, CapGInc, VolDed, SpouseVol, CollegeCont, ActBusInc, FedObInc, NGorResInc, SSInc, filio, sixtyfive, SpouseAge, SepExempt, RetInc, NegFedInc, SupBool, SupDaysInc, ChildBool, ChildSlider, ChildSliderM, ConsProt, OtherInc, PExempt, PExemptM, Blind, SpouseBlind, ID){
    
    BC = 0
    DC = 0
    
    #1040-3f: State Tax Refund from federal return
    BC = BC + StateRef
    DC = DC + StateRef
    
    #1040-3g: Deduction for total disability retirement income
    BC = BC + DisInc
    DC = DC + DisInc
    
    #1040-3h: Out-of-state income/gain
    BC = BC + NRInc
    DC = DC + NRInc
    
    #1040-3i: Income from Capital Gains
    BC = BC + .44*CapGInc
    DC = DC + .44*CapGInc
    
    #1040-3j: Volunteer Deductions
    if(VolDed == TRUE){
      if(filio == 3){
        if(SpouseVol == TRUE){
          BC = BC + 6000
          DC = DC + 6000
        }
        else{
          BC = BC + 3000
          DC = DC + 3000
        }
      }
      else{
        BC = BC + 3000
        DC = DC + 3000
      }
    }
    else{
      if(filio == 3 & SpouseVol == TRUE){
        BC = BC + 3000
        DC = DC + 3000
      }
      else{
        BC = BC
        DC = DC
      }
    }
    
    #1040-3k: Contributions to the SC College Investment Program
    DC = DC + CollegeCont
    
    #1040-3l: Active Trade or Business Income
    DC = DC + ActBusInc
    
    #1040-3m: Interest Income from obligations of the US Government
    BC = BC + FedObInc
    DC = DC + FedObInc
    
    #1040-3n: Certain Nontaxable National Guard or Reserve Pay
    BC = BC + NGorResInc
    DC = DC + NGorResInc
    
    #1040-3o: Social Security and/or railroad retirement, if taxed on your federal return
    BC = BC + SSInc
    DC = DC + SSInc
    
    #1040-3p: Retirement Deduction
    #See Federal
    
    #1040-3q: 65 or older deduction
    if(sixtyfive == TRUE){
      if(filio == 3 & SpouseAge == TRUE){
        BC = BC + 30000
        DC = DC + 30000
      }
      else{
        BC = BC + 15000
        DC = DC + 15000
      }
    }
    else{
      if(SpouseAge == TRUE){
        BC = BC + 15000
        DC = DC + 15000
      }
      else{
        if(RetInc > 3000){
          BC = BC + 3000
          DC = DC + 3000
        }
        else{
          BC = BC + RetInc
          DC = DC + RetInc
        }
      }
    }
    
    #1040-3r: Negative Federal Income
    DC = DC + NegFedInc
    
    #1040-3s: Substience Allowance 
    if(SupBool == TRUE){
      BC = BC + 8*SupDaysInc
      DC = DC + 8*SupDaysInc
    }
    
    #1040-3t: Child under 6 additional deduction
    if(ChildBool == FALSE){
      DC = DC
    }
    else{
      if(ChildBool == TRUE & ChildSlider < PExempt & filio != 3){
        DC = DC + 4000*ChildSlider
      }
      else{
        if(ChildBool == TRUE & ChildSliderM < (PExemptM - 1) & filio == 3){
          DC = DC + 4000*ChildSliderM
        }
      }
    }
    
    #1040-3u: Consumer Protection Services
    DC = DC + ConsProt
    
    #1040-3v: Other Subtractions from State Taxable Income
    DC = DC + OtherInc
    
    if(ID == 0){
      return(DC)
    }
    else{
      return(BC)
    }
  }
  
  StateIncVal = eventReactive(input$CalcButton, {
    PreInc = (input$AdjGInc 
              + StateAdditionFunc(input$Addback, input$NRLoss, input$ResExp, input$NRInterest, input$OtherAdd, input$FilStatus, input$Itemize, input$ItemizeValue, input$sixtyfive, input$SpouseAge, input$Blind, input$SpouseBlind, 0) 
              - FederalDeductionFunc(input$PExempt, input$PExemptM, input$sixtyfive, input$SpouseAge, input$FilStatus, input$Itemize, input$ItemizeValue, input$Blind, input$SpouseBlind, input$StdDeduction1, input$StdDeduction2, input$StdDeduction3, input$StdDeduction4, input$StdDeduction5, input$RetDeduction1, input$RetDeduction2, input$RetDeduction3, input$RetDeduction4, input$RetDeduction5, input$PExAmount) 
              - StateDeductionFunc(input$StateRef, input$DisInc, input$NRInc, input$CapGInc, input$VolDed, input$SpouseVol, input$CollegeCont, input$ActBusInc, input$FedObInc, input$NGorResInc, input$SSInc, input$FilStatus, input$sixtyfive, input$SpouseAge, input$SepExempt, input$RetInc, input$NegFedInc, input$SupBool, input$SupDaysInc, input$ChildBool, input$ChildSlider, input$ChildSliderM, input$ConsProt, input$OtherInc, input$PExempt, input$PExemptM, input$Blind, input$SpouseBlind, 0)
    )
    LowerBoundFunc(PreInc, 0)
  })
  PreTaxVal = eventReactive(input$CalcButton, {
    PreTax = TotalRateFunc(input$BracketSelect, StateIncVal())
    LowerBoundFunc(PreTax, 0)
  })
  MultiStateVal = eventReactive(input$CalcButton, {
    PreMSVal = (input$AdjGInc 
                + StateAdditionFunc(input$Addback, input$NRLoss, input$ResExp, input$NRInterest, input$OtherAdd, input$FilStatus, input$Itemize, input$ItemizeValue, input$sixtyfive, input$SpouseAge, input$Blind, input$SpouseBlind, 1) 
                - FederalDeductionFunc(input$PExempt, input$PExemptM, input$sixtyfive, input$SpouseAge, input$FilStatus, input$Itemize, input$ItemizeValue, input$Blind, input$SpouseBlind, input$StdDeduction1, input$StdDeduction2, input$StdDeduction3, input$StdDeduction4, input$StdDeduction5, input$RetDeduction1, input$RetDeduction2, input$RetDeduction3, input$RetDeduction4, input$RetDeduction5, input$PExAmount) 
                - StateDeductionFunc(input$StateRef, input$DisInc, input$NRInc, input$CapGInc, input$VolDed, input$SpouseVol, input$CollegeCont, input$ActBusInc, input$FedObInc, input$NGorResInc, input$SSInc, input$FilStatus, input$sixtyfive, input$SpouseAge, input$SepExempt, input$RetInc, input$NegFedInc, input$SupBool, input$SupDaysInc, input$ChildBool, input$ChildSlider, input$ChildSliderM, input$ConsProt, input$OtherInc, input$PExempt, input$PExemptM, input$Blind, input$SpouseBlind, 1)
    )})
  
  PrimeCreditFunc = function(DualIncRate, DualIncCap, MultiChildren, ChildCredit, MaxChildCredit, PerChildCredit, FilStatus, DualSlider, NumberNR, NR1, NR2, NR3, NR4, NR5, NR6, NRT1, NRT2, NRT3, NRT4, NRT5, NRT6){
    
    DC = 0
    
    #1040-11: Childcare Credit
    PC = PerChildCredit
    MC = as.numeric(MaxChildCredit)
    CC = as.numeric(MultiChildren)
    ChildLimit = MC*CC
    if(ChildCredit >= ChildLimit){
      DC = DC + ChildLimit*PC
    }
    else{
      DC = DC + (PC*ChildCredit)
    }
    
    #1040-12: Dual Wage-Earner Credit
    #if(FilStatus == 3){
    #  DualCredit = DualIncRate*DualSlider
    #  if(DualCredit >= DualIncCap){
    #    DC = DC + DualIncCap
    #  }
    #  else{
    #    DC = DC + DualCredit
    #  }
    #}
    
    #1040TC Income in Multiple States Credit
    
    MSCred1 = c(1:6)
    MSCred2 = c(1:6)
    MSFinal = c(1:6)
    
    MSCred1[1] = ((NR1 / MultiStateVal())*PreTaxVal())
    MSCred1[2] = ((NR2 / MultiStateVal())*PreTaxVal())
    MSCred1[3] = ((NR3 / MultiStateVal())*PreTaxVal())
    MSCred1[4] = ((NR4 / MultiStateVal())*PreTaxVal())
    MSCred1[5] = ((NR5 / MultiStateVal())*PreTaxVal())
    MSCred1[6] = ((NR6 / MultiStateVal())*PreTaxVal())
    
    MSCred2[1] = NRT1
    MSCred2[2] = NRT2
    MSCred2[3] = NRT3
    MSCred2[4] = NRT4
    MSCred2[5] = NRT5
    MSCred2[6] = NRT6
    
    for(i in 1:6){
      if(MSCred1[i] >= MSCred2[i]){
        MSFinal[i] = MSCred2[i]
      }
      else{
        MSFinal[i] = MSCred1[i]
      }
    }
    DC = DC + sum(MSFinal)
    
    
    return(DC)
  }
  OtherCreditFunc = function(AdjGInc, NRInc, FilStatus, Credit101, Credit44, Credit4, Credit19, Credit1, Credit2, Credit3, Credit102, Credit4SB, Credit9, Credit11, Credit12, Credit14, Credit18, Credit20, Credit21, Credit22, Credit23, Credit24, Credit25, Credit26, Credit27, Credit28, Credit29, Credit30, Credit31, Credit32, Credit35, Credit36, Credit38, Credit40, Credit41, Credit43, Credit45, Credit48, Credit51, Credit52, Credit53, Credit54, Credit55, Credit56, Credit57){
    
    DC = 0
    
    #1040TC-101: Carried over tax credits
    DC = DC + Credit101
    
    #1040TC-44: Excessive insurance premiums
    DC = DC + LowerBoundFunc(UpperBoundFunc((Credit44 - (AdjGInc*.05)), 1250), 0)
    
    #1040TC-4: New Jobs Credit
    DC = DC + UpperBoundFunc(Credit4, (PreTaxVal()*.5))
    
    #1040TC-19: Land Donation for Conservation Credit
    DC = DC + UpperBoundFunc(Credit19, PreTaxVal())
    
    #1040TC-1: DRIP/TRICKLE IRRIGATION SYSTEMS CREDIT
    DC = DC + UpperBoundFunc(Credit1, PreTaxVal())
    
    #1040TC-2: State Contractors contracting with minority businesses
    DC = DC + UpperBoundFunc((Credit2*.04), 50000)
    
    #1040TC-3: Water Resources Credit
    DC = DC + UpperBoundFunc((Credit3*.25), 2500)
    
    #1040TC-102: Nursing Home Credit
    DC = DC + UpperBoundFunc((Credit102*.2), 300)
    
    #1040TC-104: Small Business Jobs Credit
    DC = DC + UpperBoundFunc(Credit4SB, (PreTaxVal()*.5))
    
    #1040TC-9: Childcare Program Credit
    DC = DC + UpperBoundFunc(Credit9, (.5*PreTaxVal()))
    
    #1040TC-11: Capital Investment Credit
    DC = DC + Credit11
    
    #1040TC-12: CREDIT FOR EMPLOYERS HIRING RECIPIENTS OF FAMILY INDEPENDENCE PAYMENTS
    DC = DC + Credit12
    
    #1040TC-14: Community Development Tax Credit
    DC = DC + Credit14
    
    #1040TC-18: Research Expenses Credit
    DC = DC + UpperBoundFunc(Credit18, (.5*PreTaxVal()))
    
    #1040TC-20: Brownfields Cleanup Credit
    DC = DC + UpperBoundFunc(Credit20, (.5*PreTaxVal()))
    
    #1040TC-21: Credit for a Certified Historic Structure
    DC = DC + UpperBoundFunc(Credit21, (PreTaxVal()))
    
    #1040TC-22: Credit for a Certified Historic Residential Structure
    DC = DC + UpperBoundFunc(Credit22, (PreTaxVal()))
    
    #1040TC-23: Textiles Rehabilitation Credit
    DC = DC + UpperBoundFunc(Credit23, (.5*PreTaxVal()))
    
    #1040TC-24: Commercials Credit
    DC = DC + UpperBoundFunc(Credit24, PreTaxVal())
    
    #1040TC-25: Motion Pictures Credits
    DC = DC + UpperBoundFunc(Credit25, (PreTaxVal()))
    
    #1040TC-26: Venture Capital Investment Credit
    DC = DC + UpperBoundFunc(Credit26, PreTaxVal())
    
    #1040TC-27: Health Insurance Pool Credit
    DC = DC + UpperBoundFunc(UpperBoundFunc((.5*Credit27), 3000) * (NRInc/(AdjGInc + .000001)), PreTaxVal())
    
    #1040TC-28: SC Quality Forum Credit
    DC = DC + Credit28
    
    #1040TC-29: Qualified Retirement Plan Contribution Credit
    DC = DC + Credit29
    
    #1040TC-30: Port Cargo Credit
    DC = DC + UpperBoundFunc(Credit30, (PreTaxVal()))
    
    #1040TC-31: Retail Facilities Revitalization Credit
    DC = DC + UpperBoundFunc(Credit31, (PreTaxVal()))
    
    #1040TC-32: Premarital Preparation Course Credit
    if(Credit32 == TRUE){
      if(FilStatus == 3){
        DC = DC + 50
      }
      else{
        DC = DC + 25
      }
    }
    
    #1040TC-35: Alternative Motor Vehicle Credit
    DC = DC + UpperBoundFunc(Credit35, (PreTaxVal()))
    
    #1040TC-36: Industry Partnership Credit
    DC = DC + UpperBoundFunc(Credit36, (PreTaxVal()))
    
    #1040TC-38: Solar Energy or Small Hydropower System Credit
    DC = DC + UpperBoundFunc(Credit38, (.5*PreTaxVal()))
    
    #1040TC-40: Ethanol or Biodiesel Production Credit
    DC = DC + UpperBoundFunc(Credit40, (PreTaxVal()))
    
    #1040TC-41: Renewable Fuel Facility Credit
    DC = DC + UpperBoundFunc(Credit41, (PreTaxVal()))
    
    #1040TC-43: Residential Retrofit Credit
    DC = DC + UpperBoundFunc(Credit43, (PreTaxVal()))
    
    #1040TC-45: Apprenticeship Credit
    DC = DC + UpperBoundFunc(Credit45, (PreTaxVal()))
    
    #1040TC-48: Plug-In Hybrid Vehicle Credit
    DC = DC + UpperBoundFunc(Credit48, (PreTaxVal()))
    
    #1040TC-51: Venison for Charity Credit
    DC = DC + 50*as.numeric(Credit51)
    
    #1040TC-52: Fire Sprinkler System Credit
    DC = DC + UpperBoundFunc(Credit52, (PreTaxVal()))
    
    #1040TC-53: Energy Efficient Manufactured Home Credit
    if(Credit53 == TRUE){
      DC = DC + 750
    }
    
    #1040TC-54: Clean Energy Credit
    DC = DC + UpperBoundFunc(UpperBoundFunc(Credit54, (PreTaxVal())), 500000)
    
    #1040TC-55: Abandoned Buildings Revitalization Credit
    DC = DC + UpperBoundFunc(Credit55, (.5*PreTaxVal()))
    
    #1040TC-56: Angel Investor Credit
    DC = DC + UpperBoundFunc(Credit56, (PreTaxVal()))
    
    #1040TC-57: Exceptional Needs Children Scholarship Credit
    DC = DC + Credit57
    
    return(DC)
  }                 
  AddCredFunc = function(Tax,AdjGInc,ExNum,AddExAmount,C1,C2,C3,C4,C5,C6,C7,C8,D1,D2,D3,D4,D5,D6,D7,D8){
    
    DD1 = D1 / 100
    DD2 = D2 / 100
    DD3 = D3 / 100
    DD4 = D4 / 100
    DD5 = D5 / 100
    DD6 = D6 / 100
    DD7 = D7 / 100
    DD8 = D8 / 100
    
    CC1 = C1 + ExNum*AddExAmount
    CC2 = C2 + ExNum*AddExAmount
    CC3 = C3 + ExNum*AddExAmount
    CC4 = C4 + ExNum*AddExAmount
    CC5 = C5 + ExNum*AddExAmount
    CC6 = C6 + ExNum*AddExAmount
    CC7 = C7 + ExNum*AddExAmount
    CC8 = C8 + ExNum*AddExAmount
    
    if(AdjGInc < CC2){
      Tax = Tax - Tax*DD1
    }
    else{
      if(AdjGInc < CC3){
        Tax = Tax - Tax*DD2
      }
      else{
        if(AdjGInc < CC4){
          Tax = Tax - Tax*DD3
        }
        else{
          if(AdjGInc < CC5){
            Tax = Tax - Tax*DD4
          }
          else{
            if(AdjGInc < CC6){
              Tax = Tax - Tax*DD5
            }
            else{
              if(AdjGInc < CC7){
                Tax = Tax - Tax*DD6
              }
              else{
                if(AdjGInc < CC8){
                  Tax = Tax - Tax*DD7
                }
                else{
                  Tax = Tax - Tax*DD8
                }
              }
            }
          }
        }
      }
    }
    
    return(Tax)
  }
  TotCredFunc = function(CredSelect, Tax, AdjGInc, ExNum, AddExAmount){
    
    TTax = 0
    
    if(CredSelect == 8){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,input$C8,input$D1,input$D2,input$D3,input$D4,input$D5,input$D6,input$D7,input$D8)
    }
    if(CredSelect == 7){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,input$C7,0,input$D1,input$D2,input$D3,input$D4,input$D5,input$D6,input$D7,0)
    }
    if(CredSelect == 6){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,input$C3,input$C4,input$C5,input$C6,0,0,input$D1,input$D2,input$D3,input$D4,input$D5,input$D6,0,0)
    }
    if(CredSelect == 5){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,input$C3,input$C4,input$C5,0,0,0,input$D1,input$D2,input$D3,input$D4,input$D5,0,0,0)
    }
    if(CredSelect == 4){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,input$C3,input$C4,0,0,0,0,input$D1,input$D2,input$D3,input$D4,0,0,0,0)
    }
    if(CredSelect == 3){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,input$C3,0,0,0,0,0,input$D1,input$D2,input$D3,0,0,0,0,0)
    }
    if(CredSelect == 2){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,input$C2,0,0,0,0,0,0,input$D1,input$D2,0,0,0,0,0,0)
    }
    if(CredSelect == 1){
      TTax = TTax + AddCredFunc(Tax,AdjGInc,ExNum,AddExAmount,input$C1,0,0,0,0,0,0,0,input$D1,0,0,0,0,0,0,0)
    }
    
    return(TTax)
    
  }
  TaxVal = eventReactive(input$CalcButton, {
    TempTax = (PreTaxVal() 
               + .03*input$ActBusInc 
               - PrimeCreditFunc(input$DualIncRate, input$DualIncCap, input$MultiChildren, input$ChildCredit, input$MaxChildCredit, input$PerChildCredit, input$FilStatus, input$DualSlider, input$NumberNR, input$NR1, input$NR2, input$NR3, input$NR4, input$NR5, input$NR6, input$NRT1, input$NRT2, input$NRT3, input$NRT4, input$NRT5, input$NRT6) 
               - OtherCreditFunc(input$AdjGInc, input$NRInc, input$FilStatus, input$Credit101, input$Credit44, input$Credit4, input$Credit19, input$Credit1, input$Credit2, input$Credit3, input$Credit102, input$Credit4SB, input$Credit9, input$Credit11, input$Credit12, input$Credit14, input$Credit18, input$Credit20, input$Credit21, input$Credit22, input$Credit23, input$Credit24, input$Credit25, input$Credit26, input$Credit27, input$Credit28, input$Credit29, input$Credit30, input$Credit31, input$Credit32, input$Credit35, input$Credit36, input$Credit38, input$Credit40, input$Credit41, input$Credit43, input$Credit45, input$Credit48, input$Credit51, input$Credit52, input$Credit53, input$Credit54, input$Credit55, input$Credit56, input$Credit57)  )
    
    if(input$FilStatus == 3){
      NumEx = input$PExemptM
    }
    else{
      NumEx = input$PExempt
    }
    
    if(input$CredBool == TRUE){
      TempTax = TotCredFunc(input$CredSelect, TempTax, input$AdjGInc, NumEx, input$AddExAmount)
    }
    else{
      TempTax = TempTax
    }
    LowerBoundFunc(TempTax, 0)
  })
  EffectivePRate = eventReactive(input$CalcButton, {
    round(((TaxVal() / input$AdjGInc)*100), digits = 2)
  })
  output$StateTaxInc1  = renderText({
    print(StateIncVal())
  })
  output$TotalStateTax = renderText({
    print(TaxVal())
  })
  output$EffRate       = renderText({
    EffectivePRate()
  })
  
  BusTaxFunc1 = function(BusTaxBool, IncVector, BusVector, ReturnsVector, FedChoose){
    if(BusTaxBool == TRUE & FedChoose == 1){
      TempVector = IncVector + ((BusVector / ReturnsVector) / .03)
    }
    else{
      TempVector = IncVector
    }
    return(TempVector)
  }
  BusTaxFunc2 = function(BusTaxBool, BusVector){
    if(BusTaxBool == TRUE){
      return(0)
    }
    else{
      return(BusVector)
    }
  }
  BusTaxFunc3 = function(BusTaxBool, IncVector, BusVector, ReturnsVector){
    if(BusTaxBool == FALSE){
      TempVector = IncVector + ((BusVector / ReturnsVector) / .03)
    }
    else{
      TempVector = IncVector
    }
    return(TempVector)
  }
  
  SCTaxFunc   = function(Database, BracketSelect, PExAmount, ITEBool, FedChoose){
    #starts with 15
    WorkFrame = data.frame(Database$NumberofReturns, Database$SC_Exemptions_Num, Database$Filing_Status, Database$Average.State.Taxable.Income, Database$Total_Tax_On_Act_Trade_Bus_Inc, Database$Total_Tax_On_Lump_Sum_Distrib, Database$Total_Tax_On_Excess_With_from_CSA, Database$Total_State_Tax_Credits_Claimed, Database$NumberWithSchedADeductions, Database$STILower, Database$BaseTax, Database$Total_Schedule_A_Deductions, Database$Total_Federal_Taxable_Income, Database$NumberOfSchedNR, Database$TotalFedTaxIncOnNR)
    WorkFrameTemp = data.frame(WorkFrame[,14], WorkFrame[,15])
    WorkFrameTemp$NonNRReturns = WorkFrame[,1] - WorkFrameTemp[,1]
    WorkFrameTemp$AvgNoNR = ((WorkFrame[,13] - WorkFrameTemp[,2]) / WorkFrameTemp$NonNRReturns)
    WKF = data.frame((Database$FED1040_Total_Adjusted_Gross_Income + Database$FED1040A_Total_Adjusted_Gross_Income + Database$FED1040EZ_Total_Adjusted_Gross_Income) / WorkFrame[,1])
    
    WorkFrame[,10] = factor(WorkFrame[,10])
    WorkFrame[,3] = as.character(WorkFrame[,3])
    for(i in 1:nrow(WorkFrame)){
      if(WorkFrame[i,3] == "Single"){
        WorkFrame[i,3] = 1
      }
      else{
        if(WorkFrame[i,3] == "Head of Household"){
          WorkFrame[i,3] = 2
        }
        else{
          if(WorkFrame[i,3] == "Married Filing Jointly"){
            WorkFrame[i,3] = 3
          }
          else{
            if(WorkFrame[i,3] == "Married Filing Separately"){
              WorkFrame[i,3] = 4
            }
            else{
              WorkFrame[i,3] = 5
            }
          }
        }
      }
    }
    WorkFrame[,3] = as.numeric(WorkFrame[,3])
    
    if(FedChoose == 3){
      IncGive = rep(0, nrow(WorkFrame))
      WorkFrame[,4] = (WorkFrame[,13] / WorkFrame[,1]) - BusTaxFunc3(input$BusTaxBool, IncGive, WorkFrame[,5], WorkFrame[,1])
    }
    if(FedChoose == 2){
      IncGive = rep(0, nrow(WorkFrame))
      WorkFrame[,4] = WorkFrameTemp$AvgNoNR - BusTaxFunc3(input$BusTaxBool, IncGive, WorkFrame[,5], WorkFrame[,1])
    }
    
    WorkFrame[,4] = BusTaxFunc1(input$BusTaxBool, WorkFrame[,4], WorkFrame[,5], WorkFrame[,1], input$FedChoose)
    
    #Col 16
    WorkFrame$PerCapItemize = 1:nrow(WorkFrame)
    
    if(ITEBool == FALSE){
      for(i in 1:nrow(WorkFrame)){
        WorkFrame$PerCapItemize[i] = (WorkFrame[i,12] / WorkFrame[i,9])*(WorkFrame[i,9] / WorkFrame[i,1])
      }
    }
    else{
      for(i in 1:nrow(WorkFrame)){
        WorkFrame$PerCapItemize[i] = 0
      }
    }
    
    for(i in 1:nrow(WorkFrame)){
      WorkFrame[i,4] = (WorkFrame[i,4] 
                        - (as.numeric(WorkFrame[i,2])*ExemptFunc(PExAmount, WorkFrame[i,4], 1, 1, WorkFrame[i,3], input$BaseCut) - as.numeric(WorkFrame[i,2])*ExemptFunc(4000, WorkFrame[i,4], 1, 1, WorkFrame[i,3], input$BaseCut))
                        - ((StandardDeductionFunc(WorkFrame[i,3], FALSE, FALSE, FALSE, FALSE, FALSE, input$ItemizeValue, input$StdDeduction1, input$StdDeduction2, input$StdDeduction3, input$StdDeduction4, input$StdDeduction5, input$RetDeduction1, input$RetDeduction2, input$RetDeduction3, input$RetDeduction4, input$RetDeduction5) - StandardDeductionFunc(WorkFrame[i,3], FALSE, FALSE, FALSE, FALSE, FALSE, input$ItemizeValue, 6300, 9250, 12600, 6300, 12600, 1550, 1550, 1250, 1250, 3750))*((WorkFrame[i,1] - WorkFrame[i,9]) / WorkFrame[i,1]))
                        + WorkFrame$PerCapItemize[i]
      )}
    
    #Col 17
    WorkFrame$SCTableTax = 1:nrow(WorkFrame)
    
    for(i in 1:nrow(WorkFrame)){
      WorkFrame$SCTableTax[i] = (TotalRateFunc(BracketSelect, WorkFrame[i,4]) * WorkFrame[i,1])
    }
    
    #Col 18
    WorkFrame$TotalTax = WorkFrame$SCTableTax + BusTaxFunc2(input$BusTaxBool, WorkFrame[,5]) + WorkFrame[,6] + WorkFrame[,7] - WorkFrame[,8]
    
    #RevBox = data.frame((WorkFrame[,4] / WorkFrame[,1])) 
    #if(CredBool == TRUE){
    #  for(i in 1:nrow(WorkFrame)){
    #    WorkFrame$IndivTax[i] = TotCredFunc(input$CredSelect, RevBox[i,1], AdjGInc, WorkFrame[i,2], input$AddExAmount)
    #  }
    #}
    #else{
    WorkFrame$IndivTax = (WorkFrame$TotalTax / WorkFrame[,1])     #RevBox[,1]   #Col 19
    #}
    
    #Col 20
    WorkFrame$BaseTTax = (WorkFrame[,11] * WorkFrame[,1]) + WorkFrame[,5] + WorkFrame[,6] + WorkFrame[,7] - WorkFrame[,8]
    
    #Col 21
    WorkFrame$BaseITax = WorkFrame$BaseTTax / WorkFrame[,1]
    
    #Col 22
    WorkFrame$FedPCap  = WorkFrame[,13] / WorkFrame[,1]
    
    #Col 23
    FTILowerFunc = function(InVector){
      for(i in (1:nrow(WorkFrame))){
        if(InVector[i] == 0){
          WorkFrame$FTILower[i] = 0
        }
        else{
          if(InVector[i] > 0 & InVector[i] <= 500){
            WorkFrame$FTILower[i] = 1
          }
          else{
            if(InVector[i] > 500 & InVector[i] <= 1000){
              WorkFrame$FTILower[i] = 500
            }
            else{
              if(InVector[i] > 1000 & InVector[i] <= 1500){
                WorkFrame$FTILower[i] = 1000
              }
              else{
                if(InVector[i] > 1500 & InVector[i] <= 2000){
                  WorkFrame$FTILower[i] = 1500
                }
                else{
                  if(InVector[i] > 2000 & InVector[i] <= 2500){
                    WorkFrame$FTILower[i] = 2000
                  }
                  else{
                    if(InVector[i] > 2500 & InVector[i] <= 3000){
                      WorkFrame$FTILower[i] = 2500
                    }
                    else{
                      if(InVector[i] > 3000 & InVector[i] <= 3500){
                        WorkFrame$FTILower[i] = 3000
                      }
                      else{
                        if(InVector[i] > 3500 & InVector[i] <= 4000){
                          WorkFrame$FTILower[i] = 3500
                        }
                        else{
                          if(InVector[i] > 4000 & InVector[i] <= 4500){
                            WorkFrame$FTILower[i] = 4000
                          }
                          else{
                            if(InVector[i] > 4500 & InVector[i] <= 5000){
                              WorkFrame$FTILower[i] = 4500
                            }
                            else{
                              if(InVector[i] > 5000 & InVector[i] <= 5500){
                                WorkFrame$FTILower[i] = 5000
                              }
                              else{
                                if(InVector[i] > 5500 & InVector[i] <= 6000){
                                  WorkFrame$FTILower[i] = 5500
                                }
                                else{
                                  if(InVector[i] > 6000 & InVector[i] <= 6500){
                                    WorkFrame$FTILower[i] = 6000
                                  }
                                  else{
                                    if(InVector[i] > 6500 & InVector[i] <= 7000){
                                      WorkFrame$FTILower[i] = 6500
                                    }
                                    else{
                                      if(InVector[i] > 7000 & InVector[i] <= 7500){
                                        WorkFrame$FTILower[i] = 7000
                                      }
                                      else{
                                        if(InVector[i] > 7500 & InVector[i] <= 8000){
                                          WorkFrame$FTILower[i] = 7500
                                        }
                                        else{
                                          if(InVector[i] > 8000 & InVector[i] <= 8500){
                                            WorkFrame$FTILower[i] = 8000
                                          }
                                          else{
                                            if(InVector[i] > 8500 & InVector[i] <= 9000){
                                              WorkFrame$FTILower[i] = 8500
                                            }
                                            else{
                                              if(InVector[i] > 9000 & InVector[i] <= 9500){
                                                WorkFrame$FTILower[i] = 9000
                                              }
                                              else{
                                                if(InVector[i] > 9500 & InVector[i] <= 10000){
                                                  WorkFrame$FTILower[i] = 9500
                                                }
                                                else{
                                                  if(InVector[i] > 10000 & InVector[i] <= 10500){
                                                    WorkFrame$FTILower[i] = 10000
                                                  }
                                                  else{
                                                    if(InVector[i] > 10500 & InVector[i] <= 11000){
                                                      WorkFrame$FTILower[i] = 10500
                                                    }
                                                    else{
                                                      if(InVector[i] > 11000 & InVector[i] <= 11500){
                                                        WorkFrame$FTILower[i] = 11000
                                                      }
                                                      else{
                                                        if(InVector[i] > 11500 & InVector[i] <= 12000){
                                                          WorkFrame$FTILower[i] = 11500
                                                        }
                                                        else{
                                                          if(InVector[i] > 12000 & InVector[i] <= 12500){
                                                            WorkFrame$FTILower[i] = 12000
                                                          }
                                                          else{
                                                            if(InVector[i] > 12500 & InVector[i] <= 13000){
                                                              WorkFrame$FTILower[i] = 12500
                                                            }
                                                            else{
                                                              if(InVector[i] > 13000 & InVector[i] <= 13500){
                                                                WorkFrame$FTILower[i] = 13000
                                                              }
                                                              else{
                                                                if(InVector[i] > 13500 & InVector[i] <= 14000){
                                                                  WorkFrame$FTILower[i] = 13500
                                                                }
                                                                else{
                                                                  if(InVector[i] > 14000 & InVector[i] <= 14500){
                                                                    WorkFrame$FTILower[i] = 14000
                                                                  }
                                                                  else{
                                                                    if(InVector[i] > 14500 & InVector[i] <= 15000){
                                                                      WorkFrame$FTILower[i] = 14500
                                                                    }
                                                                    else{
                                                                      if(InVector[i] > 15000 & InVector[i] <= 20000){
                                                                        WorkFrame$FTILower[i] = 15000
                                                                      }
                                                                      else{
                                                                        if(InVector[i] > 20000 & InVector[i] <= 25000){
                                                                          WorkFrame$FTILower[i] = 20000
                                                                        }
                                                                        else{
                                                                          if(InVector[i] > 25000 & InVector[i] <= 35000){
                                                                            WorkFrame$FTILower[i] = 25000
                                                                          }
                                                                          else{
                                                                            if(InVector[i] > 35000 & InVector[i] <= 50000){
                                                                              WorkFrame$FTILower[i] = 35000
                                                                            }
                                                                            else{
                                                                              if(InVector[i] > 50000 & InVector[i] <= 75000){
                                                                                WorkFrame$FTILower[i] = 50000
                                                                              }
                                                                              else{
                                                                                if(InVector[i] > 75000 & InVector[i] <= 100000){
                                                                                  WorkFrame$FTILower[i] = 75000
                                                                                }
                                                                                else{
                                                                                  if(InVector[i] > 100000 & InVector[i] <= 150000){
                                                                                    WorkFrame$FTILower[i] = 100000
                                                                                  }
                                                                                  else{
                                                                                    if(InVector[i] > 150000 & InVector[i] <= 200000){
                                                                                      WorkFrame$FTILower[i] = 150000
                                                                                    }
                                                                                    else{
                                                                                      if(InVector[i] > 200000 & InVector[i] <= 350000){
                                                                                        WorkFrame$FTILower[i] = 200000
                                                                                      }
                                                                                      else{
                                                                                        if(InVector[i] > 350000 & InVector[i] <= 500000){
                                                                                          WorkFrame$FTILower[i] = 350000
                                                                                        }
                                                                                        else{
                                                                                          if(InVector[i] > 500000 & InVector[i] <= 750000){
                                                                                            WorkFrame$FTILower[i] = 500000
                                                                                          }
                                                                                          else{
                                                                                            if(InVector[i] > 750000){
                                                                                              WorkFrame$FTILower[i] = 750000
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      return(WorkFrame$FTILower)
    }
    WorkFrame$FTILower = FTILowerFunc(WorkFrame$FedPCap)
    WorkFrame$FTILower = factor(WorkFrame$FTILower)
    WorkFrame$AGILower = FTILowerFunc(WKF[,1])
    
    return(WorkFrame)
  }
  
  PreVal     = eventReactive(input$Optimize, {
    withProgress(
      message = "Loading...",
      SCTaxFunc(BaseData, input$BracketSelect, input$PExAmount, input$ITEBool, input$FedChoose)
    )
  })
  
  TaxFrame = reactiveValues()
  TaxFrame$data = NULL
  TaxFrame$counter = 0
  observe({
    if(input$Optimize){
      print("here")
      TaxFrame$new = aggregate(PreVal()[,19] ~ PreVal()[,10], PreVal(), "median")
      TaxFrame$old = aggregate(PreVal()[,21] ~ PreVal()[,10], PreVal(), "median")
      TaxFrame$cnt = aggregate(PreVal()[,19] ~ PreVal()[,10], PreVal(), "length")
      
      TaxFrame$ne3 = aggregate(PreVal()[,19] ~ PreVal()[,23], PreVal(), "median")
      TaxFrame$ol3 = aggregate(PreVal()[,21] ~ PreVal()[,23], PreVal(), "median")
      TaxFrame$cn3 = aggregate(PreVal()[,19] ~ PreVal()[,23], PreVal(), "length")
      
      TaxFrame$ne4 = aggregate(PreVal()[,19] ~ PreVal()[,24], PreVal(), "median")
      TaxFrame$ol4 = aggregate(PreVal()[,21] ~ PreVal()[,24], PreVal(), "median")
      TaxFrame$cn4 = aggregate(PreVal()[,19] ~ PreVal()[,24], PreVal(), "length")
      
      TaxFrame$neu = sum(PreVal()[,18]) / sum(PreVal()[,20])
      TaxFrame$dif = sum(PreVal()[,18]) - sum(PreVal()[,20])
    }
  })
  CountVal   = eventReactive(input$Optimize, {
    TaxFrame$counter = 1
    return(TaxFrame$counter)
  })
  CompFrame  = eventReactive(input$GraphReset, {
    Drop1 = data.frame(TaxFrame$old[,1], TaxFrame$old[,2], TaxFrame$new[,2], TaxFrame$cnt[,2])
    for(i in 1:nrow(Drop1)){
      Drop1[i,2] = LowerBoundFunc(Drop1[i,2], 0)
    }
    for(i in 1:nrow(Drop1)){
      Drop1[i,3] = LowerBoundFunc(Drop1[i,3], 0)
    }
    return(Drop1)
  })
  FedFrame   = eventReactive(input$GraphReset, {
    Drop2 = data.frame(TaxFrame$ol3[,1], TaxFrame$ol3[,2], TaxFrame$ne3[,2], TaxFrame$cn3[,2])
    for(i in 1:nrow(Drop2)){
      Drop2[i,2] = LowerBoundFunc(Drop2[i,2], 0)
    }
    for(i in 1:nrow(Drop2)){
      Drop2[i,3] = LowerBoundFunc(Drop2[i,3], 0)
    }
    return(Drop2)
  })
  AFrame   = eventReactive(input$GraphReset, {
    Drop3 = data.frame(TaxFrame$ol4[,1], TaxFrame$ol4[,2], TaxFrame$ne4[,2], TaxFrame$cn4[,2])
    for(i in 1:nrow(Drop3)){
      Drop3[i,2] = LowerBoundFunc(Drop3[i,2], 0)
    }
    for(i in 1:nrow(Drop3)){
      Drop3[i,3] = LowerBoundFunc(Drop3[i,3], 0)
    }
    return(Drop3)
  })
  DubFrame   = eventReactive(input$GraphReset, {
    
    if(input$FedPlotChoose == 1){
      PreFrame = CompFrame()
    }
    else{
      if(input$FedPlotChoose == 3){
        PreFrame = FedFrame()
      }
      else{
        if(input$FedPlotChoose == 4){
          PreFrame = AFrame()
        }
      }
    }
    
    TempFrame1 = PreFrame[1:15,c(1,2)]
    TempFrame1[,3] = rep("Old", nrow(TempFrame1))
    TempFrame2 = PreFrame[1:15,c(1,2,3)]
    TempFrame2[,2] = TempFrame2[,3]
    TempFrame2 = TempFrame2[,c(1,2)]
    TempFrame2[,3] = rep("New", nrow(TempFrame2))
    
    TempFrame  = rbind(TempFrame1, TempFrame2)
    return(TempFrame)
  })
  NextFrame  = eventReactive(input$GraphReset, {
    
    if(input$FedPlotChoose == 1){
      PreFrame = CompFrame()
    }
    else{
      if(input$FedPlotChoose == 3){
        PreFrame = FedFrame()
      }
      else{
        if(input$FedPlotChoose == 4){
          PreFrame = AFrame()
        }
      }
    }
    
    TempFrame1 = PreFrame[16:30,c(1,2)]
    TempFrame1[,3] = rep("Old", nrow(TempFrame1))
    TempFrame2 = PreFrame[16:30,c(1,2,3)]
    TempFrame2[,2] = TempFrame2[,3]
    TempFrame2 = TempFrame2[,c(1,2)]
    TempFrame2[,3] = rep("New", nrow(TempFrame2))
    
    TempFrame  = rbind(TempFrame1, TempFrame2)
    return(TempFrame)
  })
  FineFrame  = eventReactive(input$GraphReset, {
    
    if(input$FedPlotChoose == 1){
      PreFrame = CompFrame()
    }
    else{
      if(input$FedPlotChoose == 3){
        PreFrame = FedFrame()
      }
      else{
        if(input$FedPlotChoose == 4){
          PreFrame = AFrame()
        }
      }
    }
    
    TempFrame1 = PreFrame[31:43,c(1,2)]
    TempFrame1[,3] = rep("Old", nrow(TempFrame1))
    TempFrame2 = PreFrame[31:43,c(1,2,3)]
    TempFrame2[,2] = TempFrame2[,3]
    TempFrame2 = TempFrame2[,c(1,2)]
    TempFrame2[,3] = rep("New", nrow(TempFrame2))
    
    TempFrame  = rbind(TempFrame1, TempFrame2)
    return(TempFrame)
  })
  RevenueVal = eventReactive(input$Optimize, {
    sum(PreVal()[,18])
  })
  PlotVal1   = eventReactive(input$GraphReset, {
    if(input$FedPlotChoose == 1){
      Tree1 = data.frame(CompFrame()[,3])
      Tree2 = data.frame(CompFrame()[,2])
    }
    if(input$FedPlotChoose == 3){
      Tree1 = data.frame(FedFrame()[,3])
      Tree2 = data.frame(FedFrame()[,2])
    }
    for(i in 1:nrow(Tree1)){
      if(Tree2[i,1] == 0){
        Tree1[i,1] = 1
        Tree2[i,1] = 1
      }
    }
    return((Tree1[,1] / Tree2[,1]) - 1)
  })
  PlotVal2   = eventReactive(input$GraphReset, {
    if(input$FedPlotChoose == 1){
      return(CompFrame()[,3] - CompFrame()[,2])
    }
    if(input$FedPlotChoose == 3){
      return(FedFrame()[,3] - FedFrame()[,2])
    }
  })
  PlotVal3   = eventReactive(input$GraphReset, {
    (ggplot(data = DubFrame(), aes(x = DubFrame()[,1], y = DubFrame()[,2], fill = DubFrame()[,3]))
     + theme(plot.background = element_rect(fill = "transparent",colour = NA))
     + ggtitle("Tax Liability Change, Lower Incomes")
     + labs(x = "Income Bracket", y = "Tax Liability")
     + scale_fill_manual(values=c("#999999", "#56B4E9"), name="Tax Policy")
     + geom_bar(stat="identity", position=position_dodge()))
  })
  PlotVal4   = eventReactive(input$GraphReset, {
    (ggplot(data = NextFrame(), aes(x = NextFrame()[,1], y = NextFrame()[,2], fill = NextFrame()[,3]))
     + theme(plot.background = element_rect(fill = "transparent",colour = NA))
     + ggtitle("Tax Liability Change, Middle Incomes")
     + labs(x = "Income Bracket", y = "Tax Liability")
     + scale_fill_manual(values=c("#999999", "#56B4E9"), name="Tax Policy")
     + geom_bar(stat="identity", position=position_dodge()))
  })
  PlotVal5   = eventReactive(input$GraphReset, {
    (ggplot(data = FineFrame(), aes(x = FineFrame()[,1], y = FineFrame()[,2], fill = FineFrame()[,3]))
     + theme(plot.background = element_rect(fill = "transparent",colour = NA))
     + ggtitle("Tax Liability Change, Higher Incomes")
     + labs(x = "Income Bracket", y = "Tax Liability")
     + scale_fill_manual(values=c("#999999", "#56B4E9"), name="Tax Policy")
     + geom_bar(stat="identity", position=position_dodge()))
  })
  TableVal   = eventReactive(input$GraphReset, {
    if(input$FedPlotChoose == 1){
      return(CompFrame())
    }
    if(input$FedPlotChoose == 3){
      return(FedFrame())
    }
  })
  TestVal    = eventReactive(input$Optimize, {
    nrow(PreVal())
  })
  BaseO      = eventReactive(input$Optimize, {
    sum(BaseData$BaseTax*BaseData$NumberofReturns)
  })
  
  output$TaxOut   = renderText({
    print(RevenueVal())
  })
  output$RevSlider= renderText({
    print(TaxFrame$neu)
  })
  output$DAmount  = renderText({
    print(TaxFrame$dif)
  })
  output$DistPlot = renderPlot({
    barplot(PlotVal1(), main = "Tax Burden Delta for Each Income Section, Percent Change", xlab = "Income Section", ylab = "Difference")
  })
  output$AddPlot  = renderPlot({
    barplot(PlotVal2(), main = "Tax Burden Delta for Each Income Section, Amount Change", xlab = "Income Section", ylab = "Difference")
  })
  output$CompPlot = renderPlot({
    PlotVal3()
  })
  output$C2mpPlot = renderPlot({
    PlotVal4()
  })
  output$C3mpPlot = renderPlot({
    PlotVal5()
  })
  output$oldtable = renderTable({
    TableVal()
  })
  output$testval  = renderText({
    TestVal()
  })
  output$BaseOut  = renderText({
    BaseO()
  })
  
}

shinyApp(ui = ui, server = server)