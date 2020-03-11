module Synthesis

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
   

let minmax _ =
    failwith "Not implemented"

let isLeap year = match (year >= 1582)  with
                  |true -> match  (not(year % 100 = 0)|| year % 400 = 0) && (year % 4 = 0) with
                           |true -> true
                           |false -> false
                  |false -> failwith "not leap year"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"