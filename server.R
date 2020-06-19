#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library("shiny")
library("tm")
library("stringr")
library("quanteda")
library("wordcloud")
options(warn=-1)   #Suppress warnings
load("GramDataMid.RData")

#Part 2. build prediction model
#Cut the n last character in the sentence
cutstring <- function(String, LastN){
  Split <- unlist(str_split(String, " "))
  Len <- length(Split)
  LastString <- Split[(Len-LastN+1):Len]
  return(LastString)
}

#Search model
#Search from 4-gram model, return a word list, or just nothing
Search4Gram <- function(LastThreeString){
  Result <- subset(FourGram, FourGram$FristWord == LastThreeString[1] & FourGram$SecondWord==LastThreeString[2] & FourGram$ThirdWord == LastThreeString[3])
  if(dim(Result)[1] == 0){
    return(0)
  }else(
    return(as.data.frame(Result[,c("ForthWord", "fourgramsum_sorted")]))
  )
}
Search3Gram <- function(LastTwoString){
  Result <- subset(ThreeGram, ThreeGram$FristWord == LastTwoString[1] & ThreeGram$SecondWord==LastTwoString[2])
  if(dim(Result)[1] == 0){
    return(0)
  }else(
    return(as.data.frame(Result[,c("ThirdWord", "threegramsum_sorted")]))
  )
}
Search2Gram <- function(LastString){
  Result <- subset(TwoGram, TwoGram$FristWord == LastString)
  if(dim(Result)[1] == 0){
    return(0)
  }else(
    return(as.data.frame(Result[,c("SecondWord", "twogramsum_sorted")]))
  )
}
ramdomword <- "this"

#Main function
PredictNext <- function(realstring){
  newstring <- tolower(gsub("[^0-9A-Za-z'!@#$%^&*() ]","" , realstring, ignore.case = TRUE))
  print(newstring)
  stringsplit <- unlist(strsplit(newstring, " "))
  print(stringsplit)
  stringlen <- length(stringsplit)
  if (stringlen >= 3){
    test4 <- cutstring(realstring, 3)
    Search <- Search4Gram(test4)
    if (Search == 0){
      test3 <- cutstring(realstring, 2)
      Search <- Search3Gram(test3)
      if (Search == 0){
        test2 <- cutstring(realstring, 1)
        Search <- Search2Gram(test2)
        if (Search == 0){
          print("Based on random prediction")
          Final <- ramdomword
        }else{
          #Got something
          print("Based on 2-gram search")
          Final <- Search
        }
      }else{
        #Got something
        print("Based on 3-gram search")
        Final <- Search
      }
    }else{
      #Got something
      print("Based on 4-gram search")
      Final <- Search
    }
  }else if(stringlen == 2){
    test3 <- cutstring(realstring, 2)
    Search <- Search3Gram(test3)
    if (Search == 0){
      test2 <- cutstring(realstring, 1)
      Search <- Search2Gram(test2)
      if (Search == 0){
        print("Based on random prediction")
        Final <- ramdomword
      }else{
        #Got something
        print("Based on 2-gram search")
        Final <- Search
      }
    }else{
      #Got something
      print("Based on 3-gram search")
      Final <- Search
    }
  }else if(stringlen == 1){
    test2 <- cutstring(realstring, 1)
    Search <- Search2Gram(test2)
    if (Search == 0){
      print("Based on random prediction")
      Final <- ramdomword
    }else{
      #Got something
      print("Based on 2-gram search")
      Final <- Search
    }
  }
  if(length(Final) > 1){
    if (dim(Final)[1] >= 5){
      Final <- Final[1:5,]
    }
  }
  return(Final)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$mostlikelyword = renderText({
    results <- PredictNext(input$text)
    if (length(results)[1] > 1){
      finalword <- results[1,1]
      paste("The next word is: ", finalword)
    }else{
      paste("The next word is: ", ramdomword)
    }

    })
  
  output$othertitle = renderText({paste("Other possible words: ")})
  
  output$otherword = renderText({
    results <- PredictNext(input$text)
    if (length(results)[1] > 1){
      if (dim(results)[1] > 5){
        otheroptions <- results[2:5,1]
      }else if(dim(results)[1] >= 2){
        otheroptions <- results[2:dim(results)[1],1]
      }else{
        otheroptions <- "NA"
      }
    }else{
      otheroptions <- "NA"
    }
    paste(otheroptions, sep = ',')
    })

  output$table =  renderDataTable({
    results <- PredictNext(input$text)
    Records <- length(results)[1]
    if (Records > 1){
      colnames(results) <- c("Next Word", "Score")
      if (dim(results)[1] > input$outputnum){
        head(results, n=input$outputnum)
      }else{
        head(results, n=dim(results)[1])
      }
    }
    })
  
  output$wordcloudplot <- renderPlot({
    results <- PredictNext(input$text)
    Records <- length(results)[1]
    if (Records > 1){
      temp <- unlist(strsplit(row.names(results),"_"))
      Length <- length(unlist(strsplit(row.names(results)[1],"_")))
      Possiblewords <- temp[seq(Length,length(temp),Length)]
      wordcloud(words = Possiblewords, freq = results[,2], min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    }else{
      wordcloud(words = "this", freq = 1)
    }
  })


})
