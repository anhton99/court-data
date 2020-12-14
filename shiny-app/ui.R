#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tidycensus)
library(viridis)
library(leaflet)
library(stringr)
library(sf)
library(gt)
library(shinycssloaders)
library(scales)
library(rstanarm)
library(gtsummary)
library(broom.mixed)



shinyUI(navbarPage(theme = shinytheme("flatly"),
                   "Analysis of Prosecution Data in 
                   Middlesex, MA", 
                   
############## FIRST PAGE ##############   
 
tabPanel("Introduction",

# Centering paragraphs using column function (2-8-2)

         fluidRow(column(2), 
                  column(8,
                h2("Understanding the Efficacy and Racial Disparities 
                   in Prosecution", align = "center"),
                p(em("Middlesex County, Massachusetts"), align = "center"),
         br(),
         p("Most legal experts reach the consensus that prosecutors play the most 
         influential role in the criminal justice system. From deciding whether to 
         prosecute someone, which charges to bring, what sentences to recommend, 
         what plea deal to offer, prosecutors serve as gatekeepers to the courtroom 
         door and have a powerful say over the accused persons’ outcomes. Given that 
         prosecution is costly to everyone involved, and that the impact of prosecution 
         goes far beyond the courtroom for the accused people - especially for those who 
         (1) were eventually proved not guilty or were not seen by the court to negatively 
         impact community’s well-being and (2) belonged to historically disadvantaged 
         minority groups, it is the obligation of prosecutors, judges, and everyone 
         else in the community to study and assess prosecutorial decisions and their 
         consequences to improve fairness and social wellbeing."),
         br(),
         withSpinner(imageOutput("prosecutor", width = "100%", height = "100%")),
         p(em("\"Criminal Justice - The Kings of the Courtroom\". Source: The Economist"
         ), align = "center"),
         br(),
         p("This awareness serves as the main motivation behind this project, 
         which focuses particularly on the cases whose charges were brought against 
         by prosecutors and which were resolved by the courts in Middlesex County 
         during 2014-2019."),
         br(),
         p("In what follows, I will first present a summary of my research 
         analysis, key findings, data limitations, and proposals/takeaways for people 
         interested in understanding/improving the efficacy and racial impact of 
         prosecutorial decisions."),
         br(),
         br(),
         br()
        ))),

############## SECOND PAGE ##############   

   tabPanel("Summary",
         tabsetPanel(
            tabPanel("Report Summary",
                     fluidRow(column(2), 
                              column(8,
                                     br(),
                                     h3("Objective"),
                                     br(),
                                     p("This project examines the efficacy and racial disparities in prosecution
            in the 2014-2019", a("dataset", 
                                 href = "https://www.middlesexda.com/public-information/pages/prosecution-data-and-statistics"), 
                                 "that was made publicly accessible by the District 
            Attorney’s Office in Middlesex County, MA."),
                                 h3("Methodology and Analysis"),
                                 br(),
                                 p("First, I examine the", tags$b("demographics"), "to have some
            context of where the data is coming from. Given that Middlesex 
            is a predominantly white (75%), relatively wealthy neighborhood (median 
            income of ~ $97,000), I expect that the prosecution findings for Middlesex 
            County might not be representative of the country and cannot be easily
            generalized to other regions with different backgrounds in the US."),
                                 br(),
                                 br(),
                                 p("Then I move on to look at", tags$b("caseload management"), "with a focus on district 
            courts - where more than 90% of cases happened and got resolved. Specifically, 
            I look at the total number of cases that got resolved in the past 5 years 
            for each district court relative to its population served, the number of 
            days it took for cases to move from offense to filing to disposition dates, 
            and their disposition outcomes. 50% of total cases took at least 150-200 
            days to move from filing to disposition points, suggesting low court capacity. 
            Meanwhile, roughly 60% of total cases ended up getting acquitted or dismissed 
            without probationary terms, suggesting that the most accused persons were not 
            guilty, did not pose a threat to the community, or could not be convicted 
            given insufficient evidence. It is therefore worthwhile for prosecutors to 
            reassess the costs and benefits of prosecuting certain charges against persons 
            at the court, taking into account the tradeoff between court efficiency and 
            caseload."),
                                 br(),
                                 
# tag$b for bolded texts within paragraphs
                                 
                                 p("Third, I take a closer look at", tags$b("racial disparities"), "in the amount of cases 
            brought to court, types of charges, disposition outcomes, and sentence 
            descriptions by race, with a focus on Black and White people. 
            Due to the pre-existing disparities in racial population, the raw numbers 
            of cases and charges for Black people were much smaller compared to White 
            people, yet juxtaposing those numbers by racial population would reveal 
            a much higher likelihood for Black people to be charged and brought to 
            courts than White people, increasing racial inequality and inducing negative 
            racial impact on Black communities."),
                                 p("This prompts me to take a closer look at the 9 misdemeanor/low-offense 
            felony charge types identified by the American Civil Liberties Union (ACLU) 
            and other legal scholars as disproportionately affecting Black people, 
            remarkable among which are drug distribution with intent, resisting arrest, 
            trespassing and driving offenses. Using ACLU’s methodology, I find that 
            although the raw numbers independent of racial population suggest racially 
            neutral prosecutorial decisions, there is a large negative racial impact 
            because Black people were 4 to 9 times more likely than White people to 
            be prosecuted for what ACLU called “superfluous charges"),
                                 p("As for the sentence descriptions, I was able to categorize the offenses 
            that lead persons to serve their sentences in the House of Corrections and 
            look at the percentages for each offense type by race, using the methodology 
            in the recent Harvard Law School’s report on Racial Disparities in 
            Massachusetts Criminal Justice System. However, due to time constraint 
            and my inability to get a reply from the data owner for more details on 
            sentence length, I was not able to move further in my analysis and interpretation 
            of racial disparities in sentencing."),
                                 br(),
                                 p("All of my findings lead me to 2 concluding points."), 
                                 p("First, given the limited court capacity, low prosecution efficacy, 
            and the burden of financial and psychological costs of prosecution for 
            everyone involved - especially historically-disadvantaged minorities, 
            it would be beneficial for the community as a whole if prosecutors choose 
            to reexamine their practices and choose a minimal approach to prosecution. 
            The list of \"Decline To Prosecute\" proposed by the ACLU and endorsed by the DA 
            at Suffolk County, MA can be a good start."),
                                 p("Second, the DA Office in 
            realizing their stated commitment to transparency needs to improve the 
            accuracy and consistency in their data records. There are also inconsistencies
            in recording the dates of filing/offense/disposition (e.g. disposition date 
            happened before offense date, or cases that dated back to as old as 1986 for 
            “breaking and entering” offense which got filed in 2014 - this extraordinary 
            duration made me assumed is due to inaccurate reporting)"),
                                 br(),
                                 br()))
       ),
            tabPanel("Data Limitations",
                     br(),
                     br(),
                     fluidRow(column(2),
                              column(8,
                           h3("Data Limitations"),
                           br(),
                           p("1. This data only covers the cases that were prosecuted to a 
                           disposition. The number of offenses might not reflect the offenses 
                           that actually happened but rather the offenses that happened and got 
                           prosecuted. I might not be capturing the racial disparity at all if 
                           there’s a low rate of prosecution for white people. The question that
                           every prosecutor faces,  “Should I prosecute this person?”, is left
                           uninvestigated."),
                           p("2. Data inaccuracies in the original dataset lead to the elimination
                           of one-third (~ 33%) of total observations."),
                           p("3. It is unclear how race and ethnicity is reported.
                           Data on race and ethnicity are missing for about 10% of total 
                           qualified observations."),
                           p("There is no information on a person’s residence, his socioeconomic
                           status, and the length of sentence recommendations"))
         )),
         tabPanel("References",
                  br(),
                  br(),
                  fluidRow(column(2),
                           column(8,
                           h3("References"),
                           br(),
                                  p("Hall, R. & Eledroos, N.(2018),",
                                    em("Facts Over Fear - The benefits of declining to prosecute misdemeanor 
              and low-level felony offenses."), "American Civil Liberties Union.", 
                                    href = "https://www.aclum.org/en/news/facts-over-fear-benefits-declining-prosecute-misdemeanor-low-level-felonies"),
                                  p("Bishop, E., Hopkins, B., Obiofuma, C., Owusu, F.(2020)",
                                    em("Racial Disparities in the Massachusetts Criminal System."),
                                    "The Criminal Justice Policy Program, Harvard Law School.",
                                    href = "https://hls.harvard.edu/content/uploads/2020/11/Massachusetts-Racial-Disparity-Report-FINAL.pdf"),
                                  p("Davis, A. (2013)", 
                                    em("In Search of Racial Justice: The Role of the Prosecutor."), 
                                    "Articles in Law Reviews & Other Academic Journals. 1401.", 
                                    href = "https://digitalcommons.wcl.american.edu/facsch_lawrev/1401")
                           )))
         )),

############## THIRD PAGE ##############   

   tabPanel("Demographics",
            
# I want to sub-tabs, income and race

        tabsetPanel(
                
# 1st sub-tab, Race 
                
           tabPanel("Race",
                    br(),
                    br(),
                    p("Given that I am interested in learning about the 
                        racial disparities in Middlesex county and want to get 
                        a sense of how much I can generalize my findings to 
                        other regions in the US, I use data from the American 
                        Community Survey 2014-2018 to examine the racial 
                        population and median income for Middlesex county."),
                    br(),
                    p("Here, we see that Middlesex county is predominantly 
                    White. The pre-existing racial disparities in the population
                    have implications for the differential likelihood for people
                    of different races regarding their involvement with the 
                    justice system."),
                    p(em("Figure 1: Racial Population in Middlesex County, MA")),
                    imageOutput("middlesex_race")
                    ),
           
# 2nd sub-tab, Income 
           
           tabPanel("Income",
                    br(),
                    br(),
                    p("I choose to investigate income because it is an 
                    indicator of socioeconomic status and is associated with 
                    community safety and wellbeing. As we see here, the median income by 
                    census block groups show variations between $50,000 - $200,000. This
                    has implications for the number of crimes as well as the types of
                    crime committed. For example, given the inequality, I would predict 
                    that the proportion of property offense would be predominant among 
                    the types of crime committed. However, further information is needed 
                    to make more reliable predictions based on observations of trends, 
                    if any. At this point of time, I was not able to get more information 
                    from the DA’s Office regarding the defendants’ income status or the community
                    where he lives to factor income in my prediction model, which I will present
                    later."),
                p(em("Figure 2: Median Household Income Distribution 
                     in Middlesex County, MA")),
                imageOutput("middlesex_income_2")
                    )
                    )
           
            ),

############## FOURTH PAGE ##############   

   tabPanel("Caseload Management",
            
# I want 4 sub-tabs 

            tabsetPanel(
                    
# 1st sub-tab, Overview 
                    
              tabPanel("Overview",
                       br(),
                       br(),
                       p("My analysis shows that the Middlesex County District Attorney’s Office 
            (MCDAO)  prosecuted 88,651 unique cases between 2014-2019. 
              Here are more details of cases by types of courts:"),
                       p(em("Table 1. Total cases by Court types"), align = "center"),

# Add spinner to indicate pending 

                       withSpinner(gt::gt_output("middle_table")),
                       br(),

# split lay out to 70% and 30% 

                       splitLayout(cellWidths = c("70%", "30%"),
                                   p(em("Figure 3: Total cases in District Courts")),
                                   p(em("Figure 4: Total cases in Superior Courts"))),
                       splitLayout(cellWidths = c("70%", "30%"),
                                   withSpinner(plotOutput("mid_dis")),
                                   withSpinner(plotOutput("mid_sup"))),
                       br(),
                       br(),
                       br()
                       ),

# 2nd sub-tab, Case by Population

              tabPanel("Case by Population",
                       br(),
                       br(),
                       p("As over 90% of all cases happen in District Courts, this project will focus on 
            analyzing prosecution data at the district level. Middlesex has 12 District courts, 
            each serving a number of cities within the county (details can be found",
                         a("here", href = "https://www.middlesexda.com/about-us/pages/district-court-locations"),
                         ". Overall, for the past 6 years, the number of people involved with district courts 
              were relatively low to the total of the population that each court served. 
              50% of the district courts each received cases that comprise 3-5% 
              population of its assigned cities."),
                       p(em("Table 2: Summary of cases as % total population"), 
                         align = "center"),
                       gt_output(outputId = "sum_count"),
                       br(),
                       p("Figure 5 shows the total number of cases and the total population 
            that each district court served for the past 6 years (2014-2019). 
            Figure 6 transforms those numbers into percentage formats"),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   p(em("Figure 5: Total cases versus total population served")),
                                   p(em("Figure 6: Cases as % Total Population served"))
                                   ),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   plotOutput("total_sum_count"),
                                   plotOutput("sum_count_pt")
                                   ),
                       br(),
                       br(),
                       br()
                       ),

# 3rd sub-tab, Case duration 

               tabPanel("Case duration",
                        br(),
                        br(),
            p("There are 3 decision points as a case 
            moves through the system: the day an offense allegedly occurred, the day 
            criminal charges were officially documented, and the day the final outcome 
            of the case was determined. In the interactive plot below, we can see the 
            number of days it takes for each court to handle a case. While most cases 
            go through the first stage from offense date to filing date quickly 
            (~10-20 days), it takes much longer for the court to dispose of the case 
            (~ 150-200 days). The long delay from filing to disposition is indicative of
              limited court capacity"),
                        br(),
                        p(em("Figure 7: Moving through the system")),
            
# Interactive layout - choose courts 

                        sidebarLayout(sidebarPanel(
                                selectInput("district_court",
                                            label = "Choose a District Court:",
                                            choices = c("Ayer District Court",
                                                        "Cambridge District Court",
                                                        "Concord District Court",
                                                        "Framingham District Court",
                                                        "Lowell District Court",
                                                        "Malden District Court",
                                                        "Marlborough District Court",
                                                        "Natick District Court",
                                                        "Newton District Court",
                                                        "Somerville District Court",
                                                        "Waltham District Court",
                                                        "Woburn District Court"),
                                            selected = "Lowell District Court"),
                                selectInput("stage",
                                            label = "Choose a stage:",
                                            choices = c("offense to filing" = "filing_offense_diff",
                                                        "filing to disposition" = "dispo_filing_diff"),
                                            selected = "filing to disposition")
                        ), 
                        mainPanel(withSpinner(plotOutput("dispo_offense")))),
            br(),
            br()
               ),

# 4th sub-tab, Disposition 

            tabPanel("Disposition",
                     br(),
                     br(),
                     p("Using the methodology from the Harvard Law School’s and 
                     the ACLU’s reports, I was able to categorize disposition 
                     outcomes into 5 categories and reduce them further to 2 categories - 
                     adverse and non-adverse. A non-adverse outcome is that in which the case 
                     was dismissed without probationary terms , and an adverse outcome is that 
                     in which the defendant was either proved guilty or made to fulfill certain 
                     conditions prior to case dismissal. According to the ACLU, “if there were a 
                     significant public safety concern, an overwhelming majority of these cases 
                     would likely be prosecuted to an adverse disposition.” However, this is not 
                     the case for Middlesex county. Almost 60% of the cases ended up being in a 
                     non-adverse position, suggesting low prosecutorial efficacy."),
                     splitLayout(cellWidths = c("60%", "40%"),
                                 p(em("Figure 8: Disposition categories"), align = "center"),
                                 p(em("Figure 9: Adverse/Non-adverse outcomes"), align = "center")
                     ),
                     splitLayout(cellWidths = c("60%", "40%"),
                                 plotOutput("join_count_dispo"),
                                 plotOutput("join_adverse")
                     ),
                     br(),
                     br()
                     )
            )),

############## FIFTH PAGE ##############   

   tabPanel("Racial Disparities",

# 5 sub-tabs for each section of Racial Disparities

            tabsetPanel(

# 1st sub-tab, case by population, by race 
                    
              tabPanel("Case by Population",
                       br(),
                       br(),
                       fluidPage(
                               fluidRow(
                                        column(4,
                                               p("If we look at the whole district court system and consider the number of 
            cases by population by race, there are signs of racial disparity, As shown 
              in Figure 10, although White people account for 5.9% of Middlesex population, 
              they account for 13.8% total cases. Meanwhile, White people account for 
              78.2% of total population and 57.4% of total cases.")),
                                        column(8,
                                               p(em("Figure 10: Case by Population, by Race"), 
                                                 align = "center"),
                                               plotOutput("case_pop_race"))
                       )),
                       br(),
                       br()
                       ),
              
# 2nd sub-tab, Charge and Disposition, by race 

              tabPanel("Charge and Disposition",
                       br(),
                       br(),
                       p("The number of total charges per case exceeds the number of cases for people 
                       of all races. As we can see, on average, all cases in which the defendant is 
                       Black, White, or Hispanic/Latino were charged by the prosecutors with at least 2 criminal 
                       counts per case. The ratio between number of charges and number of cases were 
                       nonetheless higher for Black and Hispanic/Latino people than for White people. 
                       The more charges a prosecutor brings up against the defendant, the longer it 
                       takes for the defendant to defend himself against the accused charges. The practice 
                       of bringing up as many charges as possible also leverages the prosecutor’s 
                       position in plea bargaining - the process in which the prosecutor agrees to drop 
                       certain charges and/or recommend lesser sentences only if the defendant pleads 
                       guilty."),
                       p(em("Table 3. Total charges by case"), align = "center"),
                       withSpinner(gt::gt_output("summary_count_case")),
                       br(),
                       br(),
                       p("While Black people were slightly more likely than White people to be 
                       prosecuted with more charges, Black people were less likely than White 
                       people to be actually convicted or went on probation. The slightly higher
                       dismissal rates and overall more favorable disposition outcomes for Black
                       people suggests that Black people were more likely to be arrested and/or 
                       prosecuted for offenses that were not worth being brought to court."),
                       br(),
                       br(),
                       fluidRow(column(2),
                                column(8,
                                       p(em("Figure 11: Disposition categories, by Race"), align = "center"),
                                       plotOutput("dispo"))),
                       br(),
                       br()
                       ),

# 3rd sub-tab, Sentence Description, by race 

              tabPanel("Sentence Description",
                       br(),
                       br(),
                       p("For those who got sentenced to the House of Corrections (HOC), what offense 
                       did they commit? How did the offense types break down by race? Following the 
                       methodology used in the Harvard Law School’s report, I was able to categorize 
                       those offenses into 7 categories, as shown in Figure 12. In general, among those
                       who got sentenced to HOC, we can see that the proportions that Black and White people 
                       commit a certain type of offense relative to their same-race convicted groups
                       were almost the same, with some variations across individual courts. Analysis at 
                       individual district court level will be provided in another tab."),
                       br(),
                       fluidRow(column(2),
                                column(8,
                                       p(em("Figure 12: Offense types for HOC outcomes, by Race"), align = "center"),
                                       plotOutput("offense")))
                       ),

# 4th sub-tab, Decline to Prosecute, by race 

              tabPanel("Decline To Prosecute - DTP",
                       br(),
                       br(),
                       p("In recent years, many reform-minded legal experts agree that prosecuting people 
                       for any type of charges would not only add to the caseload and workload of the 
                       overburdened court, further reducing court capacity, but also are costly to the
                       accused people - most of whom suffer from poverty and/or other mental illnesses,
                       and therefore are much less effective and less humane compared to diverting these
                       people to other services in case where the person with the offenses do not pose
                       a danger to society that is significant enough for them to get convicted and 
                       locked up. In 2017, District Attorney Rollins from Suffolk County, Massachusetts
                       proposed a list of \"Decline To Prosecute\" - or DPT - including 15 misdemeanor 
                       offenses and low-level felonies, affirming “we will no longer criminalize poverty,
                       substance use disorder, and mental illness.” In the next year, the ACLU published 
                       a report studying racial disparities in prosecution in Suffolk County during 
                       2013-2014, showing significant disparities regarding the offenses mentioned in 
                       the DPT list and stating their support for Attorney Rollins’ policies."),
                       br(),
                       p("Based on their methodology, I was able to identify 9 offense categories from the DPT 
                       list in which the ACLU found significant racial disparities: trespassing, shoplifting, 
                       disorderly conduct, driving offense, breaking and entering, larceny under $250, 
                       destruction under $250, drug possession with intent, and resisting arrest. 
                       My findings are consistent with the ACLU’s findings in Suffolk county: 
                       adjusted for capita, Black and Hispanic/Latino people are more likely to be 
                       charged with offenses that - if committed - are primarily due to poverty and 
                       mental disorders. The interactive table below shows total charges per offense type, 
                       adjusted per capita and relative to White people:"),
                       br(),
                       p(em("Table 4: Charges, by Race"), align = "center"),
                       br(),
                       sidebarLayout(sidebarPanel(
                               selectInput("type",
                                           label = "Choose an offense type",
                                           choices = c("Trespassing" = "trespassing",
                                                       "Shoplifting" = "shoplifting",
                                                       "Disorderly Conduct" = "disorderly conduct",
                                                       "Driving Offense" = "driving offense",
                                                       "Break and Entering" = "breaking and entering",
                                                       "Larceny under $250" = "larceny under 250",
                                                       "Drug Possession w/ Intent" = "drug possession with intent",
                                                       "Destruction under $250" = "destruction under 250",
                                                       "Resisting Arrest" = "resisting arrest"),
                                           selected = "Trespassing")
                       ), 
                       mainPanel(withSpinner(gt_output("ratio_table")))),
                       br(),
                       br(),
                       br(),
                       h3("Outcome for DPT charges"),
                       p("Of all the DPT charges brought against Black and White people, the proportion of White
                       people with adverse disposition outcomes relative to their same-race defendant 
                       group was slightly higher than that of Black people. This means that when being 
                       charged with DPT offenses, Black people were more likely to have lesser outcomes than 
                       White people. Assuming judges are impartial, these outcomes show that Black people - when 
                       being arrested and prosecuted - are more likely to violate the law to a lesser
                       degree than White people such that their cases were more likely to be dismissed
                       without probationary terms. This indicates that police and prosecutors were
                       more likely to arrest and prosecute Black people even when there was not 
                       sufficient evidence for conviction."),
                       fluidRow(column(2),
                                column(8, 
                                       p(em("Figure 12: Disposition outcomes for DPT charges, by Race")),
                                       plotOutput("outcome_dpt")
                                )),
                       br(),
                       br(),
                       br()
              ),

# 5th sub-tab, Decline to Prosecute Model, by race 

              tabPanel("DTP Model",
                       br(),
                       br(),
                       p("Given the work that has been done by the ACLU and reform-minded leaders in the 
                       legal field, which lays solid ground for the argument for decriminalization of
                       people suffering from poverty, mental disorders, or the combination of both, 
                       I am interested in creating a model that focuses on prediction legal outcomes 
                       for people that are charged with these types of offense."),
                       p("The question I am interested in modeling is: among the people who got 
                       charged with at least one of the offenses on the “Decline to Prosecute” list
                       , do race and the court that handles their cases matter in terms of how many
                       charges they get filed against?"),
                       br(),
                       p("My model regressed the variables of 
                       race, court locations, and the interaction between the two onto the number of 
                       charges. Table 6 shows the results of my model. The intercept beta value 
                       represents the predicted average number of charges for White people in Ayer
                       District Court. Table 5 include the abbreviations that the Middelsex District 
                         Attorney's Office use for district courts' names"),
                       br(),
                       p(em("Table 5: Codes for District Courts Name from MCDAO official website",
                            ), align = "center"),
                       imageOutput("court_abbreviation", width = "100%", height = "100%"),
                       br(),
                       p(em("Table 6: Regression Summary Statistics"), align = "center"),
                       withSpinner(gt_output("model_1")),
                       br(),
                       p("The sum of the intercept beta value and the beta value for court location 
                       is the predicted average number of charges filed for White people in a given 
                       court. The beta value of the interaction term represents the change in the number of
                       charges
                       being brought against given a person's racial identity at one court versus another. 
                       However, 
                       if we add the beta values for the given district court location, the defendant's 
                       race, and the interaction term, we will get the predicted average number of 
                       charges filed for Black people in a given court location."),
                       br(),
                       h4("Graph Illustration"),
                       p("Based on this model, we can draw a posterior distribution for the estimated 
                       average number of charges that Black people would be filed against at a given 
                       court and even compare the probability between any two courts. For example, below 
                       is the posterior distribution for the number of charges black people receive in
                       Cambridge and Lowell District Courts"),
                       br(),
                       p(em("Figure 13: Posterior Distributions for the charges Black people receive in
                            Cambridge and Lowell district courts"), align = "center"),
                       fluidRow(column(2),
                                column(8, 
                                       plotOutput("posterior_1"))),
                       br(),
                       p("As we can see from the spread of the distribution, there is much uncertainty 
                       about the prediction. The two distributions also mostly overlap with each other, 
                       meaning the difference that exists between the two courts is minor."),
                       br(),
                       p("Figure 13 displays an interactive plot where we can choose two of any district 
                       courts to compare the probability that Black people will have a certain number of charges 
                       filed when their cases are served in one court versus 
                       another."),
                       br(),
                       p(em("Figure 14: Comparing the Posterior Distributions for the charges Black people receive in
                            two district courts"), align = "center"),
                       br(),
                       sidebarLayout(sidebarPanel(
                          selectInput("variable", 
                                         label = "Choose the first court:",
                                         choices = c(
                                                     "Cambridge District Court" = "CAM",
                                                     "Concord District Court" = "CON",
                                                     "Framingham District Court" = "FRA",
                                                     "Lowell District Court" = "LOW",
                                                     "Malden District Court" = "MAL",
                                                     "Marlborough District Court" = "MAR",
                                                     "Natick District Court" = "NAT",
                                                     "Newton District Court" = "NEW",
                                                     "Somerville District Court" = "SOM",
                                                     "Waltham District Court" = "WAL",
                                                     "Woburn District Court" = "WOB"),
                                         selected = "Lowell District Court"),
                          selectInput("variable_2",
                                         label = "Choose the second court:",
                                         choices = c(
                                            "Cambridge District Court" = "CAM",
                                            "Concord District Court" = "CON",
                                            "Framingham District Court" = "FRA",
                                            "Lowell District Court" = "LOW",
                                            "Malden District Court" = "MAL",
                                            "Marlborough District Court" = "MAR",
                                            "Natick District Court" = "NAT",
                                            "Newton District Court" = "NEW",
                                            "Somerville District Court" = "SOM",
                                            "Waltham District Court" = "WAL",
                                            "Woburn District Court" = "WOB"),
                                         selected = "Cambridge District Court")
                          
                       ), 
                       mainPanel(withSpinner(plotOutput("posterior_2"))))
                       )
            )),

############## SIXTH PAGE ##############   

   tabPanel("About",
            fluidRow(column(2),
                     column(8,
                            h3("Acknowledgement", align = "center"),
                            br(),
                            p(em("I would like to express my gratitude and warmest appreciation to 
         my Teaching Fellow Wyatt 
         Hurt - who supervised and advised me throughout every stage of the project, to 
         Disha Verma and Juan Palacio Moreno - who guided me with the legal 
         interpretations of my data, to Tyler Simko, Daniel Baissa, Shivani Aggarwal, 
         Professor Kane, and the whole course staff who have been so kind, helpful, 
         encouraging, and 
                                 dedicated to teaching me/us how to 
                                 do R and data science.")),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            p("About Me:"),
                            p("My name is Anh Ton. I am a student pursuing a joint concentration in 
          Sociology and Statistics at Harvard College."),
                            p("You can find my Github repository", a("here", 
                                                                     href = "https://github.com/anhton99/court-data")))
                     ))


))
     


  