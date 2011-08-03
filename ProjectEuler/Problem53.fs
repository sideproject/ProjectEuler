//module Problem53

let fact (i:bigint) = 
    let rec fact_k (i:bigint) (acc:bigint) =        
        if i = 1I then acc
        else
           fact_k (i - 1I) (acc * (i - 1I))
        
    if i = 0I then 1I
    else fact_k i i

let calc (n:bigint) (r:bigint) =
    (fact n) / ((fact r) * (fact(n - r)))

let calc2 (n:bigint) rlst = 
    let rec calc2_k (n:bigint) rlst count =
        if List.isEmpty rlst || rlst.Head > n then count
        else
            let sum = calc n (rlst.Head)
            if sum > 1000000I then
                calc2_k n rlst.Tail (count + 1)
            else
                calc2_k n rlst.Tail count
    calc2_k n rlst 0

let lst = [1I..100I]
lst |> List.map (fun x -> (calc2 x lst))
    |> List.sum
    |> printfn "Answer: %d"