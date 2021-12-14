library(shiny)
# install.packages("shinydashboard")
library(shinydashboard)
library(bslib)
library(tidyverse)

activitySummaryAll = as_tibble(read.csv("ATUS_Activity_Summary_03_20.dat", header = TRUE, sep = ","))
activitySummary = activitySummaryAll %>% select( c(TUCASEID, 26:ncol(activitySummaryAll), TUDIARYDAY, TUYEAR, PEEDUCA, TRDPFTPT, TELFS, TEAGE, TESEX, PTDTRACE, GTMETSTA) )
# Category Codes - first 3 chars of the category code
categoryCodes = unique(substring(colnames(activitySummaryAll %>% select(26:ncol(activitySummaryAll))), first = 1, last = 3))
categoryDesc = c("Personal Care (Sleep)", "Household Activities", "Caring For Family", "Caring For Nonfamily", "Work Activities", "Education", "Consumer Purchases", "Professional Personal Care Services", "Household Services", "Government Services & Civic Obligations", "Eating & Drinking", "Socializing, Relaxing, & Leisure", "Sports, Exercise, & Recreation", "Religious & Spiritual Activities", "Volunteer Activities", "Telephone Calls", "Traveling", NA)
categoryColors = c("#0d353f","#243a35","#124940","#214650","#405752","#2a5a6e","#286a66","#597f7f","#3e82a2",
                   "#419faa","#3ea594","#4aa7d6","#84aead","#7ecaea","#59d6e2","#60e4d4","#b3e1e0")

# Category Tier Codes - first 5 chars of the category code
categoryTierCodes = unique(substring(colnames(activitySummaryAll %>% select(26:ncol(activitySummaryAll))), first = 1, last = 5))
categoryTierCodes = categoryTierCodes[!endsWith(categoryTierCodes, "99")] # remove NA
categoryTierDescriptions = c("Sleeping", "Grooming", "Health-related Self Care", "Personal Activities", "Personal Care Emergencies", "Housework", "Food Preperation and Clean-up", "Interior Maintenence, Repair, & Decoration", "Exterior Maintenance, Repair & Decoration", "Lawn, Garden, and Houseplants", "Animals and Pets", "Vehicles", " Appliances, Tools, and Toys", "Household Management", "Caring For & Helping HH Children", "Activities Related to HH Children's Education", "Activities Related to HH Children's Health", "Caring for Household Adults", "Helping Household Adults","Caring for & Helping Nonhousehold Children", "Activities Related to Nonhh Children's Education", "Activities Related to Nonhh Children's Health", "Caring For Nonhousehold Adults", "Helping Nonhousehold Adults", "Working", "Work-Related Activities", "Other Income-Generating Activities", "Job Search and Interviewing", "Taking Class", "Extracurricular School Activities (Except Sports)", "Research/Homework", "Registration/Administrative activities","Shopping (Store, Telephone, Internet)", "Researching Purchases", "Security Procedures Related to Consumer Purchases","Professional & Personal Care Services", "Financial Services and Banking", "Legal Services", "Medical and Care Services", "Personal Care Services", "Real Estate", "Veterinary Services (excluding grooming)", "Security Procedures Related to Professional/Personal Services","Household Services (not done by self)", "Home Maintenance/Repair/Décor/Construction (not done by self)", "Pet Services (not done by self, not vet)", "Lawn & Garden Services (not done by self)", "Vehicle Maintenance & Repair Services (not done by self)", "Using Government Services", "Civic Obligations & Participation", "Waiting Associated w/ Government Services or Civic Obligations", " Security Procedures Related to Government Services or Civic Obligations","Eating and Drinking", "Waiting associated with Eating & Drinking", "Socializing and Communicating", "Attending or Hosting Social Events","Relaxing and Leisure", "Arts and Entertainment (other than sports)", "Waiting associated with Socializing, Relaxing, and Leisure", "Participating in Sports, Exercise, and Recreation", "Attending Sports/Recreational Events", "Waiting Associated with Sports, Exercise, & Recreation", "Security Procedures Related to Sports, Exercise, & Recreation", "Religious/Spiritual Practices","Administrative & Support Activities", "Social Service & Care Activities (Except Medical)", "Indoor & Outdoor Maintenance, Building, & Clean-up Activities", "Participating in Performance & Cultural Activities", "Attending Meetings, Conferences, & Training", "Public Health & Safety Activities","Telephone Calls", "Travel Related to Personal Care", "Travel Related to Household Activities", "Travel Related to Caring For & Helping Household Members", "Travel Related to Caring For & Helping Nonhousehold Members", "Travel Related to Work", "Travel Related to Education", "Travel Related to Consumer Purchases", "Travel Related to Using Professional and Personal Care Services", "Travel Related to Using Household Services", "Travel Related to Using Government Services & Civic Obligations", "Travel Related to Eating and Drinking", "Travel Related to Socializing, Relaxing, and Leisure", "Travel Related to Sports, Exercise, and Recreation", "Travel Related to Religious/Spiritual Activities", "Travel Related to Volunteer Activities", "Travel Related to Telephone Calls", "Security Procedures Related to Traveling","Unable to Code")
years = as.character(c(2003:2020))

for (code in categoryCodes) {
  activitySummary = activitySummary %>%
    mutate(!!sym(code) := rowSums(across( colnames(activitySummary %>% select(starts_with(code))) )))
}

# Aggregate time spent by category tier and add representing columns to activity summary
for (code in categoryTierCodes) {
  activitySummary = activitySummary %>%
    mutate(!!sym(code) := rowSums(across( colnames(activitySummary %>% select(starts_with(code))) )))
}

# Create a new row in activity summary tibble
# employedAndWeekday = 0 if employed and weekday, 1 if not employed and weekday, 2 if employed and weekend, 3 if not employed and weekend
employedWeekday = function(emp, dow) {
  return (mapply(function(employed, day) {
    if ((employed == 1 | employed == 2) && (day == 2 | day == 3 || day == 4 | day == 5 | day == 6)) {
      return (0) # Employed and weekday
    }
    else if ((employed == 3 | employed == 4 | employed == 5) && (day == 2 | day == 3 | day == 4 | day == 5 | day == 6)) {
      return (1)  # Not employed and weekday
    }
    else if ((employed == 1 | employed == 2) && (day == 1 | day == 7)) {
      return (2) # Working and weekend
    }
    else {
      return (3) # not employed and weekend
    }
  }, emp, dow))
}
activitySummary = activitySummary %>% mutate(employedAndWeekday = employedWeekday(TELFS, TUDIARYDAY))

# Time spent by different age groups
# Add new column to activity summary tibble
# Age group = 0 if between 15 and 24, 1 if betwen 25 adn 64, and 2 if 65+
determineAgeGroup = function(ages) {
  return (sapply(ages, function(age) {
    if (age >= 15 & age <= 24) {
      return (0)  # Age group 0: [15 - 24]
    }
    else if (age >= 25 & age <= 64) {
      return (1)  # Age group 1: [25 - 64]
    }
    else {
      return (2)  # Age group 2: [65, +)
    }
  }))
}
# Add age group column to activity summary
activitySummary = activitySummary %>% mutate(AgeGroup = determineAgeGroup(TEAGE))


# UI Code
ui = navbarPage(title = "How People Spend Their Time", theme = bs_theme(bootswatch = "flatly"), tags$style(".fa-bed {color:#FFFFFF}"),
  tabPanel("Time Spent By Category", 
           sidebarLayout(
             sidebarPanel(
               h4("Select Activity Categories"),
               checkboxInput("t01", "Personal Care (Sleep)", TRUE),
               checkboxInput("t02", "Household Activities", TRUE),
               checkboxInput("t03", "Caring for Family", TRUE),
               checkboxInput("t04", "Caring for Nonfamily", TRUE),
               checkboxInput("t05", "Work Activities", TRUE),
               checkboxInput("t06", "Education", TRUE),
               checkboxInput("t07", "Consumer Purchases", TRUE),
               checkboxInput("t08", "Professional & Personal Care Services", TRUE),
               checkboxInput("t09", "Household Services", TRUE),
               checkboxInput("t10", "Government Services and Civic Obligations", TRUE),
               checkboxInput("t11", "Eating & Drinking", TRUE),
               checkboxInput("t12", "Socializing, Relaxing, & Leisure", TRUE),
               checkboxInput("t13", "Sports, Excercise, & Recreation", TRUE),
               checkboxInput("t14", "Religious & Spiritual Activities", TRUE),
               checkboxInput("t15", "Volunteer Activities", TRUE),
               checkboxInput("t16", "Telephone Calls", TRUE),
               checkboxInput("t18", "Traveling", TRUE)
               
             ),
             mainPanel(
               dashboardPage(
                 dashboardHeader(disable = TRUE), 
                 dashboardSidebar(disable = TRUE), 
                 dashboardBody(
                   fluidRow(
                     valueBox('9.6 Hours', "Time Sleeping", icon = icon("bed"), color = 'navy'),
                     valueBox('2 Hours', "Time Completing Chores", icon = icon("broom"), color = 'orange'),
                     valueBox('2.6 Hours', "Time Working", icon = icon("briefcase"), color = 'blue'),
                     valueBox('5 Hours', "Time Socializing Leisure", icon = icon("tv"), color = 'green'),
                     valueBox('1.2 Hours', "Time Traveling", icon = icon("plane"), color = 'light-blue'),
                     valueBox('0.25 Hours', "Time School", icon = icon("tv"), color = 'fuchsia')
                    ),
                   fluidRow(
                     plotOutput("overallTimePlot")
                   )
                 )
                )
              )
           )),
  
  tabPanel("Time Spent By Group",
           sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "groupBy",
                            label = "Time Spent By:",
                            choices = c("Employement Status and Day of Week", "Sex", "Age Group"))
              ),
              mainPanel(plotOutput("groupByPlot"))
            )),
  
  tabPanel("Time Spent During Covid-19",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "covidCategory",
                           label = "Activity Category:",
                           choices = c("Time Using Medical Care Service", "Time Traveling", "Time Socializing and Leisure", 
                                       "Time Exercising and Recreation", "Time on Phone Calls" ))
             ),
             mainPanel(plotOutput("covidPlot"))
           )),
  
  tabPanel("About", 
    h2("Data Source:"),
    h5("The American Time Use Survey (ATUS) provides national estimates of how, where, and with whom Americans spend their time. 
      ATUS is federally administered by the United States Census Bureau. The survey measures the amount of time people spend doing 
      various activities such as paid work, childcare, socializing, and recreational activities. The data set contains 219,368 
      observations from 2003 to 2020 with roughly 450 different activity types. Survey respondents were asked what activities 
      they participated in during the day and how long they spent doing each activity."),
    tags$a(href="https://www.bls.gov/tus/home.htm", "ATUS Link")
    
  )
  
)

# Server Code
server = function(input, output, session) {
  
  # Overall time plot
  output$overallTimePlot = renderPlot({
    categoriesToShow = c(input$t01, input$t02, input$t03, input$t04, input$t05, input$t06, input$t07, input$t08, input$t09, input$t10, input$t11, 
                       input$t12, input$t13, input$t14, input$t15, input$t16, input$t18, FALSE)
    codesToShow = categoryCodes[categoriesToShow]
    codesNotToShow = categoryCodes[!categoriesToShow]
    newLabels = categoryDesc[categoriesToShow]
    newColors = categoryColors[categoriesToShow]
    
    if (length(codesToShow != 0)) {
      # Find mean time by activity category for each year
      meanCategoryTimeByYear = activitySummary %>%
        group_by(TUYEAR) %>% summarize(across(categoryCodes, mean)) %>% select(-codesNotToShow) %>%
        gather(Category, meanTime, categoryCodes[categoriesToShow])
      
      # Plot mean time by activity category
      ggplot(meanCategoryTimeByYear, aes(x = TUYEAR, y = meanTime / 60, fill = Category)) +
        geom_bar(stat = "identity", color = "white", width = 1, lwd = 1.5) +
        labs(title = "Overall Time Spent By Activity Category", x = "Year", y = "Time Spent (Hours)") +
        scale_fill_discrete(name = "Activity Category", labels = newLabels) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = newColors, labels = newLabels)
    }
    
  })
  
  # Time grouping plots
  output$groupByPlot = renderPlot({
    if (input$groupBy == "Employement Status and Day of Week") {
      meanTimeByEmploymentAndWeekday =  activitySummary %>% group_by(employedAndWeekday) %>%
        summarise(across(categoryCodes, mean)) %>% gather(key = ActivityCategory, value = MeanTime, t01:t50) %>%
        filter(ActivityCategory != "t50")

      ggplot(meanTimeByEmploymentAndWeekday, aes(x = as.factor(ActivityCategory), y = MeanTime/60, group = employedAndWeekday, fill = as.factor(employedAndWeekday))) +
        geom_bar(position="dodge", stat = "identity") +
        labs(title = "Mean Time by Employment Status and Day", x = "", y = "Mean Time (Hours)") +
        scale_x_discrete(labels = categoryDesc) +
        scale_fill_manual(values = c("#008571","#61b8ff","#88db97","#19221f"), name = "Employment Status & DOW",
                          labels = c("Employed and Weekday", "Not Employed and Weekday", "Employed and Weekend",
                                     "Not Employed and Weekend")) +
        coord_flip()
    }
    else if (input$groupBy == "Sex") {
      meanTimeBySex = activitySummary %>% group_by(TESEX) %>% summarise(across(categoryCodes, mean)) %>%
        gather(key = ActivityCategory, value = MeanTime, t01:t50) %>% filter(ActivityCategory != "t50")

      ggplot(meanTimeBySex, aes(x = as.factor(ActivityCategory), y = MeanTime / 60, group = TESEX, fill = as.factor(TESEX))) +
        geom_bar(position="dodge", stat = "identity") +
        labs(title = "Mean Time Spent By Men and Women", x = "", y = "Mean Time (Hours)") +
        scale_x_discrete(labels = categoryDesc) +
        scale_fill_manual(values = c("#008571","#61b8ff"), name = "", labels = c("Men", "Women")) +
        coord_flip()
    }
    else {
      meanTimeByAgeGroup = activitySummary %>% group_by(AgeGroup) %>% summarise(across(categoryCodes, mean)) %>%
        gather(key = ActivityCategory, value = MeanTime, t01:t50) %>% filter(ActivityCategory != "t50")

      ggplot(meanTimeByAgeGroup, aes(x = as.factor(ActivityCategory), y = MeanTime / 60, group = AgeGroup, fill = as.factor(AgeGroup))) +
        geom_bar(position="dodge", stat = "identity") +
        labs(title = "Mean Time Spent By Age Group", x = "", y = "Mean Time (Hours)") +
        scale_x_discrete(labels = categoryDesc) +
        scale_fill_manual(values = c("#00e0aa","#3b5946","#5be5e8"), name = "Age Group", labels = c("15-24", "25-64", "65+")) +
        coord_flip()
    }
  })
  
  
  # Covid plots
  output$covidPlot = renderPlot({
    if (input$covidCategory == "Time Using Medical Care Service") {
      # Time spent using medical services - 0804
      healthCareServiceCodes = c("t080401", "t080402", "t080403")
      healthCareServiceDesc = c("Using Health Care Service Outside of Home", "Using In-Home Health Services", "Waiting Time Associated With Services")
      medicalServiceTimes = activitySummary %>% select(t0804, t080401, t080402, t080403, TUYEAR) %>% 
        group_by(TUYEAR) %>% summarise(across(healthCareServiceCodes, mean)) %>%
        gather(Category, meanTimes, healthCareServiceCodes)
      ungroup(activitySummary)
      
      ggplot(medicalServiceTimes, aes(x = TUYEAR, y = meanTimes, fill = Category)) + 
        geom_bar(stat = "identity", color = "white", width = 1, lwd = 1.5) + 
        labs(title = "Mean Time Spent Using Medical Care Services", x = "Year", y = "Time Spent (Minutes)") +
        scale_fill_discrete(name = "Activity Category", labels = healthCareServiceDesc) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#3ea594","#7ecaea","#286a66"), labels = healthCareServiceDesc) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
    else if (input$covidCategory == "Time Traveling") {
      # Travel time 
      travelTimecodes = c("t1801", "t1802", "t1805", "t1806", "t1807", "t1808", "t1811", "t1812", "t1813")
      travelDesc = c("Personal Care Travel", "Household  Activity Travel",  "Work Travel", "Education Travel", "Consumer Purchases Travel", "Using Professional & Personal Care Travel","Eating & Drinking Travel", "Socializing, Relaxing, and Leisure Travel","Sports, Exercise, & Recreation Travel")
      travelTimes = activitySummary %>% select(travelTimecodes, TUYEAR) %>%
        group_by(TUYEAR) %>%
        summarise(across(travelTimecodes, mean)) %>%
        gather(Category, meanTimes, travelTimecodes)
      ungroup(activitySummary)
      
      ggplot(travelTimes, aes(x = TUYEAR, y = meanTimes, fill = Category)) + 
        geom_bar(stat = "identity", color = "white", width = 1, lwd = 1.5) + 
        labs(title = "Time Spent Traveling", x = "Year", y = "Time Spent (Minutes)") +
        scale_fill_discrete(name = "Activity Category", labels = travelDesc) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#304248","#006595","#23464e","#427d9d","#00445f","#687886","#337684","#485861","#51687d"), labels = travelDesc) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
    else if (input$covidCategory == "Time Socializing and Leisure") {
      # Time spent socializing, relaxing, and leisure
      socialActivityCodes = categoryTierCodes[startsWith(categoryTierCodes, "t12")]
      socialActivityDesc = c("Socializing and Communicating", "Attending or Hosting Events", "Relaxing and Leisure", "Arts and Entertainment (Non sports)", "Waiting Associated with Social Activities")
      socialTimes = activitySummary %>% select(socialActivityCodes, TUYEAR) %>%
        group_by(TUYEAR) %>%
        summarise(across(socialActivityCodes, mean)) %>% 
        gather(Category, meanTimes, socialActivityCodes)
      ungroup(activitySummary)
      
      ggplot(socialTimes, aes(x = TUYEAR, y = meanTimes / 60, fill = Category)) + 
        geom_bar(stat = "identity", color = "white", width = 1, lwd = 1.5) +
        labs(title = "Time Spent Socializing, Relaxing, and Leisure", x = "Year", y = "Time Spent (Hours)") +
        scale_fill_discrete(name = "Activity Category", labels = socialActivityDesc) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#4c95b5","#002a33","#82c8e6","#1b404a","#376476"), labels = socialActivityDesc)
    }
    else if (input$covidCategory == "Time Exercising and Recreation") {
      # Time spent sports, exercise, and recreation
      sportActivityCodes = c("t1301", "t1302")
      sportactivityDesc = c("Participating in Sports Exercise and Recreation", "Attending Sports and Recreation Events")
      sportTimes = activitySummary %>% select(sportActivityCodes, TUYEAR) %>%
        group_by(TUYEAR) %>%
        summarise(across(sportActivityCodes, mean)) %>% 
        gather(Category, meanTimes, sportActivityCodes)
      ungroup(activitySummary)
      
      ggplot(sportTimes, aes(x = TUYEAR, y = meanTimes, fill = Category)) + 
        geom_bar(stat = "identity", color = "white", width = 1, lwd = 1.5) +
        labs(title = "Time Spent Sports, Exercise, and Recreation", x = "Year", y = "Time Spent (Minutes)") +
        scale_fill_discrete(name = "Activity Category", labels = sportactivityDesc) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#4c95b5", "#376476"), labels = sportactivityDesc)
    }
    else {
      # Telephone calls
      callActivityCodes = c("t160101", "t160102", "t160103", "t160104", "t160105", "t160106", "t160107", "t160108")
      callActivityDesc = c("Calls to Family", "Calls to Friends", "Calls to Education Services", "Calls to Salespeople", "Calls to Personal Care Providers", "Calls to Household Service Providers", "Calls to Child Care Providers", "Calls to Government Officials")
      callTimes = activitySummary %>% select(callActivityCodes, TUYEAR) %>%
        group_by(TUYEAR) %>%
        summarise(across(callActivityCodes, mean)) %>%
        gather(Category, meanTimes, callActivityCodes)
      ungroup(activitySummary)
      
      ggplot(callTimes, aes(x = TUYEAR, y = meanTimes, fill = Category)) + 
        geom_bar(stat = "identity", color = "white", width = 1, lwd = 1.5) +
        labs(title = "Time Spent on Telephone Calls", x = "Year", y = "Time Spent (Minutes)") +
        scale_fill_discrete(name = "Activity Category", labels = callActivityDesc) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#51b7e6","#215468","#91c8dc","#002a33","#4d8da9","#002a33","#326174","#1b404a"), labels = callActivityDesc)
    }
  })
}

shinyApp(ui, server)