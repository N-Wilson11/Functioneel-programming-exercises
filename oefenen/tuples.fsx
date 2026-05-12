//Print a student with tuples
let display tuple1 =
    match tuple1 with
    | (studentName, studentEmail, studentNumber) -> printfn "Student naam: %s\nEmail: %s\nNummer: %d" studentName studentEmail studentNumber

display ("Nathan Wilson", "nathan@gmail.com", 12345)

printfn "Hello, %A" (fst ("Nathan", "Kishawn"))
printfn "Hello, %A" (snd ("Nathan", "Kishawn"))