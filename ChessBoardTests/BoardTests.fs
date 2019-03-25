namespace BoardTests

module BoardTests =
    open Xunit
    open ChessBoard // lib under test
    open System

    let q x y = Board.createQueen x y
    let diagonalQueens = seq{for x in 1 .. 3 -> Board.createQueen x x}

    [<Fact>]
    let ``No horizontal conflicts on 3 by 3 board with diagonal queens`` () =
        Assert.Equal(0,Board.evaluateHorizontally diagonalQueens)

    [<Fact>]
    let ``No vertical conflicts on 3 by 3 board with diagonal queens`` () =
        Assert.Equal(0,Board.evaluateVertically diagonalQueens)

    [<Fact>]
    let ``3 diagonal conflicts on 3 by 3 board with diagonal queens`` () =
        Assert.Equal(3,Board.evaluateDiagonal diagonalQueens)

    [<Fact>]
    let ``no diagonal conflictsqueens vertical neighbor`` () =
        Assert.Equal(0,Board.evaluateDiagonal [|q 1 1;q 1 2; |])

    [<Fact>]
    let ``no diagonal conflictsqueens horiziontal neighbor`` () =
        Assert.Equal(0,Board.evaluateDiagonal [|q 1 1;q 2 1; |])

    let testData () =
        seq{
                yield [| [|q 1 1|]:> Object; 0 :>Object |] // no conflicts on 1 queen obviously
                yield [| [|q 1 1;q 1 2|]:> Object; 2 :>Object |]
                yield [| [|q 1 1;q 2 1|]:> Object; 2 :>Object |]
                yield [| [|q 1 1;q 2 2|]:> Object; 2 :>Object |]
        }

    [<Theory>]
    [<MemberData("testData")>]
    let `` Simple number of conflicts test `` queens expectedConflicts =
        Assert.Equal(expectedConflicts,Board.evaluate queens 1)