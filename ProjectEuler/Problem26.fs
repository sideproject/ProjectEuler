// Learn more about F# at http://fsharp.net

let buildDiagList start stop skip =
    [for i in start .. skip .. stop -> (i) ].Tail //don't double count first element

let getSkipFactor input = 
    let x = input |> float |> sqrt |> int
    x - 2

let getPreviousUpperRightPoint input = 
    let x = getSkipFactor input
    x * x

let getDiagSum lst2 = 
    let rec getDiagSum_kernel lst2 (acc:int) =
        if List.isEmpty lst2 then 
            acc
        else     
            let h = lst2.Head
            let stop = h
            let start = getPreviousUpperRightPoint h
            let skip = getSkipFactor h
    
            let diagList = buildDiagList start stop (skip + 1)
            let summary = List.sum diagList  
                  
            getDiagSum_kernel lst2.Tail (acc + summary)

    (getDiagSum_kernel lst2 0) + 1

let square = [for i in 3 .. 2.. 1001 -> (i*i)]
let q = getDiagSum square

