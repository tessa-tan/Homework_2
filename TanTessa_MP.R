#1. Define an R function that removes NA values from a vector.
removesNAvals <- function(x) {
  return(x[!is.na(x)])
}

#2 Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
fact <- function(a) {
  prod = 1 
  if(a<0) {print("Please enter integer >0")}
  else if (a==0) {print(1)}
  else {
    for(i in 1:a)
      {prod = prod * i} 
    print(prod)
  }
}

#Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.

day_of_week <- function(date_input) {
  z <- unclass(as.Date(as.character(date_input))) #unclass date
  modval <- z %% 7
  dayofweek = c("Thursday","Friday","Saturday","Sunday","Monday","Tuesday","Wednesday")
  return(dayofweek[modval+1])
  
}


#Create a function to compute for your net pay at work. 

Net_Pay <- function(monthly_basic_rate,days_absent) { 
  daily_rate = monthly_basic_rate * 12 / 261 #261 working days in a year
  
  #SSS deduction: Basis: Monthly Basic Rate
  if (monthly_basic_rate < 1000) {
    sss_deduct = 0
  } else if(monthly_basic_rate < 15750) {
    realbracket = 0 #which row in the SSS table is the comp
    while(monthly_basic_rate > 1249.99 + 500 * realbracket) {
      realbracket = realbracket + 1
    }
    #18.1 - number of times we have to add 18.1 [based on pattern]
    add18.1 = floor(realbracket / 3)
    
    #18.2 - number of times we have to add 18.2 [based on pattern]
    add18.2 = realbracket - add18.1
    
    sss_deduct = 36.3 + (18.2 * add18.2) + (18.1* add18.1)
  }
  else {sss_deduct = 581.3}
  cat("SSS Deduction: ",sss_deduct, "\n")
  
  #PhilHealth Deduction: Basis: Monthly Basic Rate
  if(monthly_basic_rate <= 10000) {
    philhealth_deduct = 137.5
  } else if (monthly_basic_rate < 40000) {
      philhealth_deduct = .0275 * monthly_basic_rate / 2
  } else {philhealth_deduct = 550}
  #print(philhealth_deduct)
  cat("PhilHealth Deduction: ", philhealth_deduct, "\n")
  
  #Pag-Ibig Deduction: Basis: Monthly Basic Rate
  if(monthly_basic_rate <= 1500) {
    pagibig_deduct = monthly_basic_rate * .01
    } else if (monthly_basic_rate <= 5000) {
      pagibig_deduct = monthly_basic_rate * .02
    } else {
        pagibig_deduct = 100}
  #print(pagibig_deduct)
  cat("PAG-IBIG Deduction: ", pagibig_deduct, "\n")

  #Taxable Income
  taxable_income = monthly_basic_rate - (sss_deduct + philhealth_deduct + pagibig_deduct + daily_rate * days_absent)
  
  #Withholding Tax
  if(taxable_income < 20833.33) {
    tax = 0
  } else if(taxable_income < 33333) {
    tax = (taxable_income - 20833.33)*.2
  } else if(taxable_income < 66667) {
    tax = 2500 + .25 * (taxable_income - 33333)
  } else if(taxable_income < 166667) {
    tax = 10833.33 + .3 * (taxable_income - 66667)
  } else if(taxable_income < 666667) {
    tax = 40833.33 + .32 * (taxable_income - 166667)
  } else {
    tax = 200833.33 + .35 * (taxable_income - 666667)}
  total_deduct = sss_deduct + philhealth_deduct + pagibig_deduct + tax
  cat("Withholding Tax: ", tax, "\nTotal Deductions: ",total_deduct,"\n")
  
  netpay = taxable_income - tax
  cat("Net Pay: ", netpay, "\n")
  }
    
  

#Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
cpd_int <- function(rate,time_yrs,principal,numcompounded) { #rate=interest rate [in decimal form eg 0.05 ]; time=number of yrs money is invested;numcompounded = number of times interest is compounded per year 
  if(rate > 1){
    print("Input rate in decimal form.")
  } else { #calculated step by step
    A = 1 + rate/numcompounded
    B = A^(numcompounded * time_yrs)
    C= B * principal
    cpd_interest = C - principal
    return(cpd_interest)
  }
}


#Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.
isPrime <- function(n) {
  ctr = 0
  for (i in 1:n) {
    if (n%%i == 0) {
      ctr = ctr + 1
    }
  }
  if (ctr >= 3) {
    print(FALSE) 
    }
    else {
      print(TRUE)
    }
}

