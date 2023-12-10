library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Read the dataset
cyb_df <- read_csv("Cleaned-Data.csv")

tab_danger <- fluidPage(
  h3("How dangerous is each attack?"), 
  h5("Let's take a look at the different attack types and how their severity levels vary."),
  sidebarLayout(
    sidebarPanel(
      position = "left", 
      selectInput(
        inputId = "malw_type",
        label = "Attack Types",
        choices = c("SQL Injection", "Zero-Day Exploits", "Cross-Site Scripting", "Phishing", "Password Attacks", 
                    "DoS", "Ransomware", "Man-in-the-Middle", "DDoS"),
        selected = "Phishing"
      ),
      helpText(
        "Here, you can see that most attacks do have a higher level of severity, although there are some attack
       types that do have a majority of their attacks on the lower severity side."
      )
    ),
    
    mainPanel(
      #graph
      plotOutput(outputId = "malware_sign")
    )
  )
)

tab_anomaly <- fluidPage(
  h3("Is there a correlation between anomaly score and the severity of an attack?"),
  h5("Here, we will be focusing on Phishing attacks in particular, as it is the most common attack type and has the most high severity attacks."),
  h5("An Anomaly Score is the measure of how 'odd' the code of an attack is. That is, it is a measurement of how different or odd the system was acting 
  from its normal operating behavior."), 
  h5("Choose a severity level below to see the anomaly score distribution."),
  sidebarLayout(
    sidebarPanel(
      position = "right",
      radioButtons(
        inputId = "severe_type",
        label = "Severity Level of Attack",
        choices = c("Low", "Medium", "High"),
        selected = "Low"
      )
    ),
    mainPanel(
      plotOutput(outputId = "severe_corr")
    )
  )
)

tab_malware <- fluidPage(
  h3("How often is malware detected?"),
  h5("Are there factors of an attack that correlate to how often the system detects the malware? Here, we will 
     be looking at how the different factors of an attack, such as its anomaly score and packet size, affect
     the system's ability to detect it."),
  sidebarLayout(
    sidebarPanel(
      position = "right",
      sliderInput(
        inputId = "anom_score",
        label = "Choose a range of anomaly scores",
        min = 0,
        max = 100,
        value = c(0,100)
      ),
      sliderInput(
        inputId = "pckg.amt",
        label = "Choose the maximum package size",
        min = 96,
        max = 1260,
        value = 150
      )
    ),
    mainPanel(
      h3("Malware Detected based on Anomaly Score"),
      
      tableOutput(
        outputId = "phish_anom"
      ),
      
      h3("Malware Detected based on Package Size"),
      
      tableOutput(
        outputId = "phish_pckg"
      )
    )
  ),
  h5("Although the values are very close together, we can see that consistently malware is detected less often.")
)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Introduction", 
             h1("Securing the Digital Frontier: Unraveling Cyber Threats through Data Analysis"),
             p("By: Krishang Bairavarsu, Samuel Harris, Amara Herman"),
             
             # Add two images side by side
             div(
               img(src = "https://www.hurix.com/wp-content/uploads/2022/01/Cyber-security.jpg", 
                   width = 400, height = 300),
               img(src = "https://managex.ae/wp-content/uploads/2022/05/What-are-the-seven-types-of-cyber-security-threats-1536x864.jpg", 
                   width = 400, height = 300)
             ),
             
             h2("The Significance of Cybersecurity"),
             
             p("In an increasingly interconnected world driven by technological advancements, the significance of cybersecurity cannot be overlooked. While technology brings countless benefits and innovations to our lives, it also produces new risks and vulnerabilities that can jeopardize the safety and privacy of individuals and organizations."),
             
             p("In our ongoing journey through the digital age, our lives have become more intertwined with technology than ever before. As a result, it is important to safeguard the vast amount of data we generate and exchange daily."),
             
             p("Each one of us possesses data that holds significant value, known as Sensitive Personally Identifiable Information (SPII). This SPII encompasses a wide range of sensitive details, such as Social Security Numbers, credit card information, and medical records, among others. This information is stored on both local devices and the online platforms we rely on daily. While efforts have been made to secure this data on dedicated servers, the internet is teeming with individuals and groups seeking to exploit any vulnerabilities and gain unauthorized access."),
             
             p("These threat actors use various hacking techniques, including distributed denial of service (DDoS) attacks, phishing attempts, malware distribution, and more. Once they have this information, they can use it for a variety of illegal acts, such as financial extortion and identity theft. This is where the significance of cybersecurity and its protective measures truly comes to the forefront."),
             
             h2("Our Project"),
             
             p("Our project researches further into these cyber threats, delving into the various attacks targeting businesses and individuals alike. The goal of our project is to examine various datasets, gaining insights into the most common cyber attacks hackers employ and understanding their severity. By gaining a better understanding of these attack vectors, our goal is to not only educate others on potential cyber threats but to also effectively predict and mitigate potential risks in the ever-evolving realm of cybersecurity."),
             
             # Add a link for further exploration
             p("Explore our datasets in detail:"),
             p("Dataset 1:https://www.kaggle.com/datasets/zunxhisamniea/cyber-threat-data-for-new-malware-attacks."),
             p("Dataset 2:https://www.kaggle.com/datasets/teamincribo/cyber-security-attacks."
             )
    ),
    tabPanel("Bar Chart", 
             h3("About the Bar Charts:"),
             p("The bar charts below provide visual insights into the distribution of cyber attacks."),
             p("The first chart represents the top 3 most popular attack types in recent years, while the second chart displays other common attack types."),
             
             plotOutput(outputId = "bar_chart_attack_type"),
             plotOutput(outputId = "bar_chart_target_variable")
    ),
    tabPanel("How Dangerous is an Attack?", tab_danger),
    tabPanel("How Severe is Weird?", tab_anomaly),
    tabPanel("Malware Detection", tab_malware)
  )
)

server <- function(input, output) {
  output$bar_chart_attack_type <- renderPlot({
    count_df_attack_type <- as.data.frame(table(cyb_df$Attack.Type))
    
    ggplot(data = count_df_attack_type, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5, size = 3) +
      labs(x = "Top 3 Recorded Attack Types", y = "Number of recorded cases from 2020 - Present", fill = "Attack Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$bar_chart_target_variable <- renderPlot({
    count_df_target_variable <- as.data.frame(table(cyb_df$Target.Variable))
    
    ggplot(data = count_df_target_variable, aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Freq), vjust = -0.5, size = 3) +
      labs(x = "Other Common Attack Types", y = "Number of recorded cases from 2020 - Present", fill = "Attack Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
output$malware_sign <- renderPlot({
    malw_df <- filter(cyb_df, Target.Variable  == input$malw_type, Target.Variable != "NA") %>% group_by(Target.Variable) %>% count(Severity.Level)
    malw_df$Severity.Level <- factor(malw_df$Severity.Level, levels = c("Low", "Medium", "High"))  
    
    ggplot(data = malw_df) + 
      geom_bar(mapping = aes(x = Severity.Level, y = n, fill = Severity.Level), stat = "identity", position = "dodge") +
      geom_text(mapping = aes(x = Severity.Level, y = n, label = n), vjust = -0.75, size = 4) +
      labs(
        x = "Severity Level",
        y = "Number of Attacks",
        fill = "Severity Level",
        title = "Severity Level Trends Based on Attack Type") + 
      theme_classic()
  })
  
  output$severe_corr <- renderPlot({
    corr_df <- filter(cyb_df, Severity.Level == input$severe_type, Target.Variable == "Phishing")
    
    ggplot(corr_df) + 
      geom_histogram(mapping = aes(x = Anomaly.Scores), stat = "bin", position = "stack", bins = 10, color = "black") +
      labs(
        x = "Anomaly Scores",
        y = "Amount",
        title = "What is the Frequency of Anomaly Scores per Severity of Attack?") +
      theme_classic()
  })
  
  output$phish_anom <- renderTable({
    phish_df <- filter(cyb_df, Target.Variable == "Phishing", Anomaly.Scores >= input$anom_score[1], Anomaly.Scores <= input$anom_score[2]) %>%
      select(Malware.Indicators) %>%
      count(Malware.Indicators == "IoC Detected")
    colnames(phish_df) <- c("Malware.Detected", "Amount")
    phish_df$Malware.Detected[1] <- "Detected"
    phish_df$Malware.Detected[2] <- "Undetected"
    phish_df$Frequency[1] <- round( ( (phish_df$Amount[1]) / (phish_df$Amount[1] + phish_df$Amount[2]) ) * 100 )
    phish_df$Frequency[2] <- round( ( (phish_df$Amount[2]) / (phish_df$Amount[1] + phish_df$Amount[2]) ) * 100 )
    
    phish_df
  })
  
  output$phish_pckg <- renderTable({
    phish_df2 <- filter(cyb_df, Target.Variable == "Phishing", AvgPacketPerEvent <= input$pckg.amt) %>%
      select(Malware.Indicators) %>%
      count(Malware.Indicators == "IoC Detected")
    colnames(phish_df2) <- c("Malware.Detected", "Amount")
    phish_df2$Malware.Detected[1] <- "Detected"
    phish_df2$Malware.Detected[2] <- "Undetected"
    phish_df2$Frequency[1] <- round( ( (phish_df2$Amount[1]) / (phish_df2$Amount[1] + phish_df2$Amount[2]) ) * 100 )
    phish_df2$Frequency[2] <- round( ( (phish_df2$Amount[2]) / (phish_df2$Amount[1] + phish_df2$Amount[2]) ) * 100 )
    
    phish_df2
  })
  
}

shinyApp(ui = ui, server = server)
