printfn "Welcome to Tic-Tac-Toe!"

// ======================================================
// 1. Domain model
// ======================================================

type Square =
    | Empty
    | Cross
    | Nought

type Row = Square * Square * Square
type Board = Row * Row * Row

type TripleIndex =
    | First
    | Second
    | Third

// ======================================================
// 2. Updating rows and boards
// ======================================================

let updateSquareOfRow (r: Row) (i: TripleIndex) (s: Square) : Row =
    let s1, s2, s3 = r

    match i with
    | First -> (s, s2, s3)
    | Second -> (s1, s, s3)
    | Third -> (s1, s2, s)

let updateRowOfBoard (b: Board) (i: TripleIndex) (r: Row) : Board =
    let r1, r2, r3 = b

    match i with
    | First -> (r, r2, r3)
    | Second -> (r1, r, r3)
    | Third -> (r1, r2, r)

let getRowOfBoard (b: Board) (rowIndex: TripleIndex) : Row =
    let r1, r2, r3 = b

    match rowIndex with
    | First -> r1
    | Second -> r2
    | Third -> r3

let updateSquareOfBoard
    (b: Board)
    (squareIndex: TripleIndex)
    (rowIndex: TripleIndex)
    (s: Square)
    : Board =

    let rowToUpdate = getRowOfBoard b rowIndex
    let updatedRow = updateSquareOfRow rowToUpdate squareIndex s
    updateRowOfBoard b rowIndex updatedRow

// ======================================================
// 3. Empty board
// ======================================================

let emptyRow : Row =
    (Empty, Empty, Empty)

let emptyBoard : Board =
    (emptyRow, emptyRow, emptyRow)

// Example board with one move already played.
let exampleBoard =
    updateSquareOfBoard emptyBoard First First Cross

// ======================================================
// 4. Turning the board into text
// ======================================================

let symbolOfSquare (s: Square) : string =
    match s with
    | Empty -> " "
    | Cross -> "X"
    | Nought -> "O"

let stringOfRow (r: Row) : string =
    let f, m, b = r

    let fs = symbolOfSquare f
    let ms = symbolOfSquare m
    let bs = symbolOfSquare b

    $"| %s{fs} | %s{ms} | %s{bs} |"

let stringOfBoard (b: Board) : string =
    let border = "-------------"

    match b with
    | (r1, r2, r3) ->
        let sr1 = stringOfRow r1
        let sr2 = stringOfRow r2
        let sr3 = stringOfRow r3

        $"%s{border}\n%s{sr1}\n%s{sr2}\n%s{sr3}\n%s{border}"

let printBoard (b: Board) : unit =
    let sb = stringOfBoard b
    printfn "%s" sb

// Try the board printing.
printBoard exampleBoard