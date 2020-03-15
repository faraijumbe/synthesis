﻿module Synthesis

let abelar input = input > 12 && input < 3097 && input % 12 = 0 
    //failwith "Not implemented"

let area length height = 
    match length >= 0.0 && height >= 0.0 with
    |true -> length / 2.0 * height
    |false -> failwith "Noooo"
    

let zollo input =
    match input >= 0 with
    |true -> input * 2
    |false -> input * -1 
    //failwith "Not implemented"

let min num1 num2 =
    match num1 > num2 with 
    |true -> num2
    |false -> num1

let max num1 num2 =
    match num1 > num2 with
    |true -> num1
    |false -> num2

let ofTime h m s = h * 3600 + m * 60 + s

let toTime input =  
    match input < 0 with 
    |true -> (0,0,0)
    |false -> 
    let h = input / 3600
    let m = input % 3600 / 60
    let s = input % 3600 % 60
    (h, m, s)
    

let digits input =
    let rec count a acc= 
        match  (a < 10 && a > -10) with
        |true -> acc
        |false -> count (a / 10) (acc + 1)
    count input 1
   

let minmax (a,b,c,d) =
    min (min a b) (min c d), max (max a b) (max c d)
    

let isLeap year = match (year >= 1582)  with
                  |true -> match  (not(year % 100 = 0)|| year % 400 = 0) && (year % 4 = 0) with
                           |true -> true
                           |false -> false
                  |false -> failwith "not leap year"

let month = function 
        |1 ->("January", 31 ) 
        |2 ->("February", 28)
        |3 ->("March", 31 ) 
        |4 ->("April", 30)
        |5 ->("May",31)
        |6 ->("June", 30)
        |7 ->("July", 31)
        |8 ->("August", 31)
        |9 ->("September", 30)
        |10 ->("October",31)
        |11 ->("November", 30)
        |12 ->("December", 31)
        |_ -> failwith "not a month"
    


let toBinary input =
    match input >= 0 with
    |true ->
        let rec binary num store =
            match num = 0 with 
            |true -> match store = "" with 
                     |true -> "0"
                     |_ -> store
            |false -> binary (num / 2) (string(num % 2) + store)
        binary input ""
    |false -> failwith "no negative numbers allowed"
    

let bizFuzz input =
    
    let rec biz num (a, b, c) =
        match num < 1 with
        |true -> (a, b, c)
        |false -> 
            match num % 3 = 0 && num % 5 = 0  with 
            |true -> biz (num - 1) (a + 1, b + 1, c + 1)
            |false -> 
                    match num % 3 = 0 with
                    |true -> biz (num - 1) (a + 1, b, c)
                    |false -> match num % 5 = 0 with
                                |true -> biz (num - 1) (a, b + 1, c)
                                |false -> biz (num - 1) (a, b, c)
         
    biz input (0, 0, 0)
        


let monthDay d y =
    failwith "Not implemented"
    

let coord _ =
    failwith "Not implemented"