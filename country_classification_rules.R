classify_country <-function(lifeExp,expectedCases){
  if(lifeExp>=70){
    if(expectedCases<0.01){
      return(2)
    }else if(expectedCases<0.1){
      return(3)
    }
  }else{
    return(4)
  }
  return(4)
  
}

classify_country_trust <-function(lifeExp){
  if(lifeExp>=70){
    return("trustworthy")
  }else{
    return("untrustworthy")
  }
  
}