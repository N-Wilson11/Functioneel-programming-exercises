//Simple Tic-Tac-Toe game in F#

printfn "Welcome to Tic-Tac-Toe"

type Square = Empty | Cross | Nought
type Row = Square * Square * Square
type Board = Row * Row * Row

type TripleIndex = 
    |First 
    |Second
    |Third

let updateSquareOfRow(r: Row)(i: TripleIndex)(s: Square) : Row =
    let s1, s2, s3 = r
    match i with
    | First -> (s, s2, s3)
    | Second -> (s1, s, s3)
    | Third -> (s1, s2, s)

let updateRowOfBoard(b: Board)(i: TripleIndex)(r: Row) : Board =
    let r1, r2, r3 = b
    match i with
    | First -> (r, r2, r3)
    | Second -> (r1, r, r3)
    | Third -> (r1, r2, r)

let getRowOfBoard(b: Board)(rowIndex: TripleIndex) : Row =
    let r1, r2, r3 = b
    match rowIndex with
    | First -> r1
    | Second -> r2
    | Third -> r3

let updateSquareOfBoard(b: Board)(squareIndex: TripleIndex)(rowIndex: TripleIndex)(s: Square) : Board =  
    let rowToUpdate = getRowOfBoard b rowIndex
    let updatedRow = updateSquareOfRow rowToUpdate squareIndex s
    updateRowOfBoard b rowIndex updatedRow


let squareToString(s: Square) : string =
    match s with
    | Empty -> " "
    | Cross -> "X"
    | Nought -> "O"

let drawRow(r: Row) : string =
    let s1, s2, s3 = r
    sprintf " %s | %s | %s " (squareToString s1) (squareToString s2) (squareToString s3)

let drawBoard(b: Board) : unit =
    let r1, r2, r3 = b
    printfn "\n"
    printfn "%s" (drawRow r1)
    printfn "-----------"
    printfn "%s" (drawRow r2)
    printfn "-----------"
    printfn "%s" (drawRow r3)
    printfn "\n"

let emptyBoard = (
    (Empty, Empty, Empty),
    (Empty, Empty, Empty),
    (Empty, Empty, Empty)
)

let parseIndex (input: string) : TripleIndex option =
    match input.Trim().ToLower() with
    | "1" | "first"  -> Some First
    | "2" | "second" -> Some Second
    | "3" | "third"  -> Some Third
    | _              -> None

//let squareIsEmpty (b: Board) (squareIndex: TripleIndex) (rowIndex: TripleIndex) : bool =
    
    

let rec gameLoop (board: Board) (currentPlayer: Square) (counterP1: int) (counterP2: int) : unit =
    drawBoard board
    let playerStr = if currentPlayer = Cross then "X" else "O"
    printfn "Player %s's turn" playerStr
    printf "Enter row (1/2/3): "
    let rowInput = System.Console.ReadLine()
    printf "Enter column (1/2/3): "
    let colInput = System.Console.ReadLine()

   

    match parseIndex rowInput, parseIndex colInput with
    | Some row, Some col ->
        let newBoard = updateSquareOfBoard board col row currentPlayer
        let newCounterP1 = if currentPlayer = Cross then counterP1 + 1 else counterP1
        let newCounterP2 = if currentPlayer = Nought then counterP2 + 1 else counterP2
        if currentPlayer = Cross && newCounterP1 = 3 then
            printfn "Counter: %d" newCounterP1
            printfn "Player X wins!"
            drawBoard newBoard
        else if currentPlayer = Nought && newCounterP2 = 3 then
            printfn "Counter: %d" newCounterP2
            printfn "Player O wins!"
            drawBoard newBoard
        else
            printfn "Counter: %d" (if currentPlayer = Cross then newCounterP1 else newCounterP2)
            let nextPlayer = if currentPlayer = Cross then Nought else Cross
            gameLoop newBoard nextPlayer newCounterP1 newCounterP2
    | _ ->
        printfn "Ongeldige invoer, probeer opnieuw."
        gameLoop board currentPlayer counterP1 counterP2

gameLoop emptyBoard Cross 0 0







