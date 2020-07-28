classify_country <-function(lifeExp,expectedCases){
  if(lifeExp>=70){
    if(expectedCases<0.05){
      return(2)
      #high trust, very low prevalence
    }else if(expectedCases<1){
      return(3)
      #high trust, low to moderate prevalence
    }else{
      return(4)
      #high trust, high prevalence
    }
  }else{
    return(4) #low trust.
  }
  return(NULL)
  
}

classify_country_trust <-function(lifeExp, country_name){
  if(lifeExp>=70){
    return("trustworthy")
  }else if (country_name %in% c("Samoa","Cook Islands")){
    return("trustworthy")
  } else{
    return("untrustworthy")
  }
  
}

country_classification_names <- function(country_classification){
  if (country_classification==2){
    return("safe")
  }else if (country_classification==3){
    return("low to moderate risk")
  }else if(country_classification==4){
    return("high risk")
  }else{
    return("unknown risk")
  }
}