getscore <- function(filename="FinalExam.Rmd"){
  
    tmp <- readLines(filename)
    tmp1 <- grep("## Question", tmp, value=TRUE)
    tmp2 <- sapply(tmp1, function(x) strsplit(x, "[{]|[}]")[[1]][2])
    names(tmp2) <- 1:25
    tmp2 <- gsub("[ ]*","",tolower(tmp2))
    if(length(grep("[a-d]", tmp2))<25){
      message <- "Not all questions Answered"
    } 
    tmp3 <-                c(Q1="c",
                             Q2="c",
                             Q3="a",
                             Q4="d",
                             Q5="c",
                             Q6="c",
                             Q7="a",
                             Q8="b",
                             Q9="b",
                             Q10="c",
                             Q11="d",
                             Q12="d",
                             Q13="a",
                             Q14="c",
                             Q15="b",
                             Q16="c",
                             Q17="a",
                             Q18="d",
                             Q19="c",
                             Q20="b",
                             Q21="d",
                             Q22="b",
                             Q23="c",
                             Q24="d",
                             Q25="a")
      
    message <- paste(sum(tmp3 == tmp2), "correct")
    
    list(score = sum(tmp3 == tmp2)*4,
         answerdetail = cbind(paste("Q",1:25), tmp2, ifelse(tmp2==tmp3, "Correct","Wrong")))
}