module QuickFindTests

open NUnit.Framework
open FsUnit
open impl.QuickFind
open impl.UnionFind


[<Test>]
let ``given [|1; 1; 2|] expect 0 and 1 are connected`` () =
    impl.QuickFind.areConnected [|1; 1; 2|] 0 1 |> should be True


[<Test>]
let ``when connect [|0; 1; 2|] 0 1 expect [|1; 1; 2|] `` () =
    impl.QuickFind.connect [|0; 1; 2|] 0 1 |> should equal [|1; 1; 2|]


[<Test>]
let ``given [|0; 1; 3; 5; 4; 4|] expect root of 2 is 4`` () =
    impl.UnionFind.getRoot [|0; 1; 3; 5; 4; 4|] 2 |> should equal 4

[<Test>]
let ``given [|0; 1; 3; 5; 4; 4|] expect 2 and 3 are connected`` () =
    impl.UnionFind.areConnected [|0; 1; 3; 5; 4; 4|] 2 3 |> should be True


[<Test>]
let ``when union [|0; 1; 3; 5; 4; 4|] 1 2 expect [|0; 4; 3; 5; 4; 4|]`` () =
    impl.UnionFind.union [|0; 1; 3; 5; 4; 4|] 1 2 |> should equal [|0; 4; 3; 5; 4; 4|]


[<Test>]
let ``many weighted unions`` () =
    let id0, sz0 = impl.WeightedUnionFind.initialize 10
    let union = impl.WeightedUnionFind.union
    let id1, sz1 =
        (id0, sz0)
        |> union 4 3
        |> union 3 8
        |> union 6 5
        |> union 9 4
        |> union 2 1
        |> union 5 0
        |> union 7 2
        |> union 6 1
        |> union 7 3
    id1 |> should equal [|6; 2; 6; 4; 6; 6; 6; 2; 4; 4|]

