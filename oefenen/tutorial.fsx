let prefix prefixStr baseStr = 
    printfn "%s" (prefixStr + ", " + baseStr)


let names = ["Nathan"; "Alice"; "Bob"]
let exclamation e = e + "!"

let mutable a = 6

printfn "%d" a

a <- 1

printfn "%d" a





names 
|> List.map exclamation
|> List.iter (prefix "Hello")

let weather temperature =
    if temperature < 0 then printfn "Freezing"
    elif temperature < 10 then printfn "Cold"
    elif temperature < 20 then printfn "Mild"
    elif temperature < 30 then printfn "Warm"
    else printfn "Hot"

weather 150