
# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(viridis)
library(gifski)
library(av)
library(extrafont)
library(imager)

font_import()

grp = function(ini.stake,p,win.amt)
{
  
  # ini.stake :: The Initial Stake with which the gambler enters the game.
  # p :: win probability for each round for the gambler, 0<p<1.
  # win.amt :: The amount the gambler wants to win from the game Utlimately.
  
  round.money = array(dim=1) # Money with the gambler in each round of the game.
  
  round.money[1] = ini.stake # Storing the money for the first round, which is
  # indeed the initial capital.
  
  ct = 1 # A counter keeping the position count of the array round.money[].
  
  while(round.money[ct] > 0) # Until going broke
  {
    ct = ct + 1            # Number of Rounds being played.
    
    if(rbinom(1,1,p) == 1) # The gambler wins the round.
    {
      round.money[ct] = round.money[ct-1] + 1
    }else                  # The gambler loses the round.
    {
      round.money[ct] = round.money[ct-1] - 1
    }
    
    if(round.money[ct] == win.amt) # If the gambler reaches the winning amount,
      # the gambler stops playing.
    {
      break
    }
  }
  
  if(p == 0.5)   # The gambler playing a fair/unbiased game.
  {
    # Probability of winning the entire fair game.
    
    win.prob = ini.stake/win.amt
    
  }else          # The gambler playing a biased game.
  {
    # Probability of winning the entire biased game.
    
    win.prob = (1-(((1-p)/p)^ini.stake))/(1-(((1-p)/p)^win.amt))
  }
  
  # A Data Frame for storing the data, which is to be plotted.
  df = data.frame(capital = round.money, rounds = 1:ct)
  
  if(round.money[length(round.money)] == 0) # Ultimately losing the game
    # ---> going broke.
  {
    ggplot(df, aes(x = rounds,y = capital,color=capital)) +
      theme_ipsum(base_family = "Roboto Condensed") + ylab("Capital in Each Round") +
      ggtitle(paste("Overall Probability of winning this entire game\nis ",win.prob
                    ,"\nInitial stake as ",ini.stake,"; Winning Amount = ",win.amt,
                    "\nWin probability in each round is p = ",p,
                    "\nBut Sorry you lost! :( ",
                    "\nNumber Of Rounds Played = ",ct)) +
      geom_line() + theme(legend.position="None") +
      xlab("Rounds of the Game") + transition_reveal(df$rounds) +
      scale_color_viridis(discrete = F) +
      geom_point(size=4,color="#8E44AD")
    
  }else  # Ultimately winning the game ---> acquiring the winning amount.
  {
    ggplot(df, aes(x = rounds,y = capital,color=capital)) +
      theme_ipsum(base_family = "Roboto Condensed") + ylab("Capital in Each Round") +
      ggtitle(paste("Overall Probability of winning this entire game\nis ",win.prob,
                    "\nInitial stake = ",ini.stake,"; Winning Amount = ",win.amt,
                    "\nWin probability in each round is p = ",p,
                    "\nCongratulations You Win! :) ",
                    "\nNumber Of Rounds Played = ",ct)) +
      geom_line() + theme(legend.position="None")+#color = "#00008B",size=1,alpha=0.7) +
      xlab("Rounds of the Game") + transition_reveal(df$rounds) +
      scale_color_viridis(discrete = F) +
      geom_point(size=4,color="#8E44AD")
    
  }
  
  
}


# Define UI
ui <- shinyUI(fluidPage(theme = shinytheme("darkly"),
                navbarPage(
                  # theme = "darkly",  # <--- To use a theme, uncomment this
                  "Gambler's Ruin Simulator",
                  tabPanel("The Simulator",
                           sidebarPanel(
                             tags$h3("User Inputs:"),
                             numericInput("ini.stake", "Money you have", 4),
                             numericInput("win.amt", "Money you want to win", 50),
                             numericInput("p", "p - Win Probability of each round",0.50,
                                          max=1,min=0,step=0.01),
                             
                             submitButton("Play"),
                             p("Wait for a few moments at the beginning
                               and after you hit play, for the simulation
                               to run !!",style="font-size:20px"),
                             p("Please be a little ambitious and enter a greater
                               sum you want to win than the existing capital you have !!",
                               style = "font-size:20px")
                           ),
                           
                           mainPanel(
                             h1("The Results - Your Game Trajectory"),
                             h4("Illustration of One Dimensional Random Walk"),
                             plotOutput("res_plot",width=1080)
                             
                             
                            # mainPanel
                  )
                  ),
                  tabPanel("Associated Concepts",
                           "The simulation of the game is hosted under the framework
                           of the Gambler's Ruin Problem, which is often given reference
                           to while illustrating the key concepts of Statistics and Probability,
                           viz. Stochastic Processes, Random Walks and Markov Chains.
                           The problem being simulated is often used to demonstrate the above 
                           mentioned concepts, as the case, where the Simulator in this app simulates 
                           a particular game for the choice of the input parameters mentioned by the user,
                           which can henceforth be used an illustrative example of a 1-D Random Walk.",style = "font-size:25px"),
                  tabPanel("Game Details", 
                           "The game considers the situation of a gambler, who enters to gamble with an initial
                           amount of money as denoted by 'ini.stake'. The gambler wishes to play until he attains 
                           a particular amount of money from the game, which is denoted by 'win.amt'. Hence the gambler 
                           eventually leaves the game and the game stops under two specific circumstances, i.e., once when 
                           all the money gets finished, making the gambler bankrupt or when the gambler reaches his desired 
                           amount of money. The probability that the gambler wins each round of the game being played is 'p'.
                           The wagered amount considered at every round of the game is 1 unit of money getting deducted, if the 
                           gambler loses the round, or 1 unit of money added, if the gambler wins the round. The entire game 
                           trajectory along with the final result ---> whether the gambler loses or wins the game being played, is 
                           demonstrated through the animated plot, which also contains the long-run probability that the gambler
                           wins the entire game being played."
                           ,style = "font-size:25px"),
                  tabPanel("Developer Info",
                           p("Somjit Roy",style="font-size:30px"),
                           imageOutput("my_image"),
                           p("e-mail: roy.sxcstsa@gmail.com",style = "font-size:20px"),
                           p("A Statistics and Data Science Enthusiast. This app is a part of my research on 
                             the topic 'The Gambler's Ruin Problem - Introducing Games of Chance', during the 
                             completion of my undergraduate degree in Statistics from St.Xavier's College, Kolkata. The 
                             research entails a detailed description about the problem at hand and the associated concepts. 
                             Modelling Gambler's Ruin Problem through One-Dimensional Random Walks and Markov Chains, Discussing 
                             the solution of the problem through simulations, difference equations and transition probability matrices, 
                             solving the most asked query about the Expected Duration of Play, Giving a Machine Learning aspect to the 
                             problem and developing an R-Package for simulating the game are few among many of the prospects covered 
                             in the research."
                             ,style = "font-size:25px"),
                           p("Any feedback regarding further development of the app, or any ideas to be shared are highly appreciated - For that please
                             contact me through the email id as mentioned above. Also I am available on LinkedIn and GitHub.",
                             style = "font-size:25px"),
                           p(a("Somjit Roy - GitHub ID", href="https://github.com/Roy-SR-007", target="_blank"),
                             style = "font-size:25px"),
                           p(a("Somjit Roy - LinkedIn Profile ID", href="https://www.linkedin.com/in/somjit-r-6b0965114/", 
                               target="_blank"),style = "font-size:25px"),)
                  
                 # navbarPage
  )

)# fluidPage
)


# Define server function  
shinyServer(
server <- function(input, output) 
{
  
  
  output$res_plot <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
    
    p <- grp(input$ini.stake,input$p,input$win.amt)
    
    anim_save("outfile.gif", animate(p)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$my_image<-renderImage({
    list(src = "ProfilePhoto.jpg",
         contentType = 'image/jpg',
         width = 300,
         height = 300
         # alt = "This is alternate text"
    )}, deleteFile = FALSE)
  
  
  }
# server
)


# Create Shiny object
shinyApp(ui = ui, server = server)


