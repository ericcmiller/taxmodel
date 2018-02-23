Test2014 = subset(BaseData1, BaseData1$Taxyear == 2014)
Test2014$Average.State.Taxable.Income = Test2014$Total_State_Taxable_Income / Test2014$NumberofReturns
LowerBoundFunc = function(input, bound){
  if(input >= bound){
    return(input)
  }
  else{
    return(bound)
  }
}
Test2014$AverageAGI = (Test2014$FED1040_Total_Adjusted_Gross_Income + Test2014$FED1040A_Total_Adjusted_Gross_Income + Test2014$FED1040EZ_Total_Adjusted_Gross_Income) / Test2014$NumberofReturns
for(i in 1:nrow(Test2014)){
  Test2014$BaseTax[i] = RFS(Test2014$Average.State.Taxable.Income[i],0,0,2920,3,5840,4,8760,5,11680,6,14600,7,0)
  Test2014$AverageAGI[i] = LowerBoundFunc(Test2014$AverageAGI[i],0)
}
write.csv(Test2014, file = "Data2.csv")


#To do:
#
#   1. Condense graph/sorting
#   2. Include a window for graph explanation
#   3. Table to display counts per sorting
#   4. Clear up add-back credit and implement
#   5. Debug non-resident federal taxable income calculation
#   6. Look over alternative 2014 dataset in Clicdata


RFS = function(ICN, B1, RR1, B2, RR2, B3, RR3, B4, RR4, B5, RR5, B6, RR6, ID){
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

