library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tools)
library(ggrepel)
library(mapproj)



# load packages prior to running the app in order to have everything running in the app!!

ui <- fluidPage(theme= shinytheme("cosmo"),
                titlePanel("Lung Cancer Risk Assesment Tool"),    # Adds Title of the Website, hence Main Title
                # Adds Navigation Bar at the top of the Page
                # This is to have different tabs in the website
                navbarPage("Interactive Tools to Help You", 
                           #Creates a tab
                           tabPanel("Introduction",
                                    # Under this tab the main Panel depicts the paragraph below in the color
                                    mainPanel(span(style="color:black",
                                                   #Paragraph output bold
                                                   strong(p(""),
                                                          #break
                                                          br(),
                                                          #Paragraph output
                                                          # Paragrpah to introduce the app and explain the risk assesment tool
                                                          
                                                          p(
                                                            "Welcome to our website! With the incidence of cancer at an all-time high, 
                                                            it is imperative to learn about the associated risk factors in order to 
                                                            be better equipped to take proactive measures. According to 
                                                            the United States National Institute of Health (NIH), an estimated 1,735,350 
                                                            new cases of cancer will be diagnosed in the U.S. each year and 609,640 people will
                                                            die from the disease in 2018 alone. Cancer is the second leading cause of death in the U.S., 
                                                            and scientists predict that as the current population continues to age, these numbers will 
                                                            dramatically increase. This will have a major impact on many aspects of society, including economics. 
                                                            The NIH reported that in 2017, national expenditure for cancer care was $147.3 billion, 
                                                            and this number is also likely to increase in coming years. 
                                                            Given the unsettling implications of this data, action is required. 
                                                            The good news is that a substantial proportion of cancer risks can be significantly mitigated."),
                                                          #break
                                                          br(),
                                                          #Paragraph output
                                                          p(
                                                            "A study conducted by The World Cancer Research Fund concluded that up to one-third of cancer 
                                                            causes in developed countries are closely related to behavioral risk factors such as obesity, 
                                                            a sedentary lifestyle, smoking, and heavy drinking. Different cancers have distinct risk factors, 
                                                            however many lifestyle associated factors such as nutrition, exercise, alcohol and drug use have been 
                                                            significantly linked to specific cancer types, including lung cancer. However, 
                                                            important to note is that some risks factors such as one’s genetics/age cannot be altered and are
                                                            therefore not accounted for on our risk calculator. Our website offers user friendly, interactive features 
                                                            to calculate your risk of developing colorectal and lung cancer. It is our hope that by learning more you can 
                                                            adopt practices that minimize your risks. Ultimately, we seek to help you become more knowledgeable and proactive toward your health."),
                                                          br(),
                                                          
                                                          p("Disclaimer"),
                                                          
                                                          br(),
                                                          
                                                          p("All the information and analysis provided on this website is for educational purposes ONLY. 
                                                            The information is NOT intended to replace any clinical judgement in any matter. 
                                                            Please seek professional help from a qualified medical provider if you have any concerns about your health and/or cancer risk."),
                                                          
                                                          br(),
                                                          p("Let's get started!"))),
                                              #break
                                              br(),
                                              #Image is inserted
                                              img(src ="Prevention_pic.jpg", width="400"),
                                              br()
                                              
                                                          )),
##############################################################################################################################
## BMI tab
                           
                           tabPanel("Body Mass Index Calculator (BMI)",
                                    sidebarPanel(
                                      helpText("Body mass index (BMI) is a measure of body fat based on height and weight that applies to adult men and women. 
                                               The BMI is not always an accurate measure of health because it does not take into account muscle mass or body shape."),
                                      numericInput("num_height", label = h3("Height (in)"),value=60),
                                      numericInput("num_weight", label = h3("Weight (lbs)"),value=100),
                                      actionButton("action_calc", label = "Calculate")),
                                    # Create the individual tabs separatley!
                                    mainPanel(
                                    h2("Calculated values:"),div(textOutput("text_bmi"), style="font-weight: bold;"), 
                                    textOutput("text_type")),
                                    img(src="bmi-all.png", width=420,align="center")),
                                    
                            
                           
                           
                           
########################################################################################
# Lung cancer Plot                         
                           
tabPanel("Tobacco Use Increases Lung Cancer Risk",
                                    sidebarPanel(
                                      helpText("This plot shows a linear regression model of tobacco use and cancer risk. 
                                               The data used to make this plot was gathered from the CDC's website."),
                                      #radioButtons("Region", c("midwest", "northeast","south","west"), selected = "midwest")),
                                      selectInput("Region","Region:", 
                                                  choices=c("northeast", "south","midwest","west"))),
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      plotOutput("lungplot")
                                    )),

########################################################################################
# Lung cancer calculator tab
                           tabPanel("Lung Cancer Risk Calculator",
                                    sidebarPanel(
                                      helpText("This calculator is based on the LLP risk model: an individual risk prediction model for lung cancer. 
                                               It is a model-based approach that estimates your probability of developing lung cancer within a 5-year period. 
                                               It takes into account specific risks factors that have been strongly correlated with lung cancer such as tobacco use, 
                                               exposure to environmental contaminants, and a family history of lung cancer. Source: Cassidy et al., 2008. 
                                               British Journal of Cancer (2008) 98, 270-276."),
                                      # Adds buttons for selecting gender and metric system
                                      radioButtons(
                                        inputId  = "sex",
                                        label    = "Sex",
                                        choices  = c("Male" = 1, "Female" = 2),
                                        selected = 2
                                      ),
                                      
                                      sliderInput("age", "Age:",
                                                  min=40, max=84, value=40),
                                      
                                      numericInput("smoking",label=h4("Number of Years You Have Smoked"), value=1),
                                      
                                      radioButtons(
                                        inputId  = "pneumonia",
                                        label    = "Have You Ever Been Diagnosed with Pneumonia",
                                        choices  = c("Yes" = 1, "No" = 0),
                                        selected = 0
                                      ),
                                      
                                      radioButtons(
                                        inputId  = "asbestos",
                                        label    = "Have You Been Exposed to Asbestos",
                                        choices  = c("Yes" = 1, "No" = 0),
                                        selected = 0
                                      ),
                                      
                                      radioButtons(
                                        inputId  = "malignant_tumour",
                                        label    = "Prior Diagnosis of a Malignant Tumor",
                                        choices  = c("Yes" = 1, "No" = 0),
                                        selected = 0
                                      ),
                                      
                                      numericInput("family_history", label=h4("Prior Family History of Lung Cancer (Average Onset of Diagnosis)"),value=1)),
                                    mainPanel(span(style="color:black",
                                                   p(h4("Calculated values:")),
                                                   textOutput("text_risk"), style="font-weight: bold;")),
                                    
                                    img(src="table2.png", width=700,align="center")
                                    
                                    
                                    
                                    ),
# Application title
tabPanel("Distribution Map",
         
         
         # Sidebar with a slider input for number of bins 
         sidebarPanel(
           h1("Smoking and Lung Cancer"),
           p("There are many factors are associated with lung cancer.",
             span(strong("Smoking is the most important risk factor associated with lung cancer.")),
             "Below you can select different years to see how the distribution of smoking rate and lung cancer has changed across the United States."),
           
           
           radioButtons("year","Year" ,
                        c("2011" = "2011",
                          "2012" = "2012",
                          "2013" = "2013",
                          "2014" = "2014",
                          "2015" = "2015"))
         ),
         
         # Show a plot of the generated distribution
         mainPanel(
           
           plotOutput("smoking"),
           plotOutput("cancer")
           
         )),















###############################################################################################################################
## Recomendation Panel



tabPanel("Recommendations",
         mainPanel(span(style="color:black",
                        strong(p(""),
"The lungs are truly special organs within our bodies, they are responsible for oxygenating our blood and without we cannot live. 
Our lungs draw in air from the atmosphere and utilize it and distribute throughout our body. 
For these reasons, anything that we breathe in can impact our health and therefore our risk of developing lung cancer. 
After many years of research, scientists now unanimously agree that tobacco use is the most important and significant risk factor for lung cancer. 
In the U.S., over 87% of lung cancers are related to smoking and other forms of tobacco use. 
Smoking not only impacts the person who is smoking, but also those around them. 
Second-hand smoke is another concern, as those exposed are also at risk of developing lung cancer.",
br(),

p("Other environmental substances or exposures that can increase the risk of developing lung cancer include:"),

br(),

p("Asbestos, Radon", "Air-Pollution", "Industrial Substances (substances can include Arsenic, Uranium, Beryllium, Vinyl Chloride, Nickel Chromates, Coal Products, 
  Mustard Gas, Chloromethyl Ethers, Gasoline, and Diesel Exhaust), and Radiation Exposure"),

br(),

p("The CDC Recommends the Following Ways to Stop Smoking"),

br(),

p("Most former smokers quit without using one of the treatments that scientific research has shown can work. However, the following treatments are proven to be effective for smokers who want help to quit:
Brief help by a doctor (such as when a doctor takes 10 minutes or less to give a patient advice and assistance about quitting)
  Individual, group, or telephone counseling
  Behavioral therapies (such as training in problem solving)
  Treatments with more person-to-person contact and more intensity (such as more or longer counseling sessions)
  Programs to deliver treatments using mobile phones
  Medications for quitting that have been found to be effective include the following:
  Nicotine replacement products
  Over-the-counter (nicotine patch [which is also available by prescription], gum, lozenge)
  Prescription (nicotine patch, inhaler, nasal spray)
  Prescription non-nicotine medications: bupropion SR (Zyban®), varenicline tartrate (Chantix®)
  Counseling and medication are both effective for treating tobacco dependence, and using them together is more effective than using either one alone.
  More information is needed about quitting for people who smoke cigarettes and also use other types of tobacco."),

br(),
p("Helpful Resources"),
br(),
p("Call 1-800-QUIT-NOW (1-800-784-8669) if you want help quitting. 
  This is a free telephone support service that can help people who want to stop smoking or using tobacco.")
  
))),

img(src="free.jpg", width=420,align="right")

)


######################################################################################################################################################
## Do Not touch this part! This is to close the UI part
                           
                           ))
######################################################################################################################################################
# Setting up the server!!!!

server <- function(input, output,session) {
  values <- reactiveValues()
  
  ######################################################################################################################################################
  # BMI Function Server
  
  
  output$text_bmi <- renderText({
    input$action_calc
    values$bmi<-isolate({input$num_weight/((input$num_height)*(input$num_height))}*703)
    paste("BMI: ", isolate(round(values$bmi,digits=1))) # prints out only 1 decimal place
  })
  
  output$text_type <- renderText({
    input$action_calc
    values$bmi<-isolate({input$num_weight/((input$num_height)*(input$num_height))}*703)
    if(values$bmi<18.5){
      paste("Underweight")
      #img(src="underweight-bmi.png")
      
    }else
      if(values$bmi>=18.5 & values$bmi<20.0){
        paste("Healthy")
        #img(src="normal-weight-bmi.png")
        
      }else
        if(values$bmi>=20.0 & values$bmi<24.9){
          paste("Overweight")
          
        }else
          if(values$bmi>=24.9 & values$bmi<29.9){
            paste("Obesity")
            
            
          }else
            if(values$bmi>=29.9){
              paste("Morbid Obesity")}
    
  })
  
  lung <-read.csv("lung.csv")
  #lung
  
  
  output$lungplot <- renderPlot({
    ggplot(lung %>% filter(Region == input$Region) %>% mutate(Year=as.factor(Year)),
           aes(x=Smokerate, y=Cancerrate)) +
      geom_point(size=5, aes(color=Year)) +
      xlab("Smoking Rate") +
      ylab("Cancer Rate") +
      ggtitle("Smoking is Directly Correlated to Lung Cancer") +
      #facet_grid(~Region)+
      geom_smooth(method = "lm") +
      scale_color_brewer(palette="Dark2")+
      theme_bw()
  })
  
  
  
  # Lung Cancer Preduction Function
  
  output$text_risk <- renderText({
    source("lung_func.R")	
    #	as.numeric( )
    
    lung_cancer_risk(age = as.numeric(input$age), sex = as.numeric(input$sex), smoking = as.numeric(input$smoking),
                     pneumonia = as.numeric(input$pneumonia), asbestos = as.numeric(input$asbestos),
                     malignant_tumour = as.numeric(input$malignant_tumour), family_history = as.numeric(input$family_history))
    
#lung_cancer_risk(age=50,sex=1,smoking=1,pneumonia=1,asbestos=1,malignant_tumour=1,family_history=1)
    
  })

##################################################################################################
## Map
  
  states= map_data("state")
  states$region=toTitleCase(states$region)
  smokinglungcancer=read.csv("20112015smokingcancer.csv")
  state_distribution=inner_join(states, smokinglungcancer, by = "region")
  
  output$smoking <- renderPlot({
    smokingdata=state_distribution%>%
      filter(year==input$year)
    gg <- ggplot()
    gg <- gg + geom_map(data=states, map=states,
                        aes(long,lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)
    gg <- gg + geom_map(data=smokingdata, map=states,aes(fill=smoking, map_id=region),
                        color="#ffffff", size=0.15)
    gg <- gg + scale_fill_gradient(low='yellow', high='red', guide='colorbar',limits=c(9,30),name = "Smoking Rate(%)")
    gg <- gg + labs(x=NULL, y=NULL)
    gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45)#+geom_text_repel(data = smokingdata, aes(long, lat, label = region), size = 3,box.padding = unit(0.1, 'lines'), force = 0.5) 
    #gg <- gg + geom_text(aes(label=smoking))
    #gg <- gg + ggrepel::geom_label_repel(data=smokingdata, aes(x=long, y=lat, label=region))
    gg <- gg + theme(panel.border = element_blank())
    gg <- gg + theme(panel.background = element_blank())
    gg <- gg + theme(axis.ticks = element_blank())
    gg <- gg + theme(axis.text = element_blank())
    gg <- gg + ggtitle("Smoking Rate Distribution Accross U.S.") +theme(plot.title = element_text(size=35,face = "bold"))
    gg
    
    
    
    
    
    
    #ggplot(data=smoking,aes(x = long, y = lat,label=smoking, fill=smoking,group = region))+geom_polygon( color = "white") + coord_fixed(1.3)+scale_fill_gradient(low="blue", high="red",limits=c(9,30),name = "Smoking Rate(%)")+ geom_text_repel()
  })
  
  
  
  output$cancer <- renderPlot({
    cancerdata=state_distribution%>%filter(year==input$year)
    
    gg1 <- ggplot()
    gg1 <- gg1 + geom_map(data=states, map=states,
                          aes(x=long, y=lat, map_id=region),
                          fill="#ffffff", color="#ffffff", size=0.15)
    gg1 <- gg1 + geom_map(data=cancerdata, map=states,
                          aes(fill=cancer, map_id=region),
                          color="#ffffff", size=0.15)
    gg1 <- gg1 + scale_fill_gradient(low='blue', high='green', 
                                     guide='colorbar',limits=c(0.02,0.12),name = "Lung Cancer Rate(%)")
    gg1 <- gg1 + labs(x=NULL, y=NULL)
    gg1 <- gg1 + coord_map("albers", lat0 = 39, lat1 = 45) 
    gg1 <- gg1 + theme(panel.border = element_blank())
    gg1 <- gg1 + theme(panel.background = element_blank())
    gg1 <- gg1 + theme(axis.ticks = element_blank())
    gg1 <- gg1 + theme(axis.text = element_blank())
    gg1 <- gg1 + ggtitle("Lung Cancer Rate Distribution Across the U.S.")+theme(plot.title = element_text(size=35,face = "bold"))
    gg1    
    
    
    
    #ggplot(data = cancer,aes(x = long, y = lat, fill=smoking,group = region)) + geom_polygon( color = "white") + coord_fixed(1.3)+scale_fill_gradient(low="blue", high="red",limits=c(9,30),name = "Lung Cancer Rate(%)")
    
  })
  
  
  
  
  
  
  
  
  
  ######################################################################################################
  # Do not touch anything after this part!!!! This closes the server :)
}
shinyApp(ui, server)