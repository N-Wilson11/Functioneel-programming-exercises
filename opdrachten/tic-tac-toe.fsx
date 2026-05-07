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

let square = Cross
let row = (squareToString square, squareToString Empty, squareToString Empty)
let board = (row, row, row)


printfn "%A\n" board



