namespace impl

module QuickFind =

    let areConnected (id: array<int>)  i j =
        id.[i] = id.[j]

    let connect (id: array<int>)  i j =
        let iVal = id.[i]
        let jVal = id.[j]
        id |> Array.map (fun e -> if e = iVal then jVal else e)


module UnionFind =

    let rec getRoot (id: array<int>) i =
        if id.[i] = i then i else getRoot id id.[i]

    let areConnected (id: array<int>)  i j =
        getRoot id i = getRoot id j

    let union (id: array<int>)  i j =
        let ri = getRoot id i
        let rj = getRoot id j
        Array.set id ri rj
        id


module WeightedUnionFind =

    let initialize n =
        [|0..n-1|], Array.create n 1

    let rec getRoot (id: array<int>) i =
        if id.[i] = i then i else getRoot id id.[i]

    let areConnected (id: array<int>)  i j =
        getRoot id i = getRoot id j

    let replace ri rj (id: array<int>) (sz: array<int>) =
        Array.set id ri rj
        Array.set sz rj (sz.[rj] + sz.[ri])
        id, sz

    let union i j (id: array<int>, sz: array<int>) =
        let ri = getRoot id i
        let rj = getRoot id j

        if ri = rj 
        then id, sz
        elif sz.[ri] < sz.[rj]
        then replace ri rj id sz
        else replace rj ri id sz