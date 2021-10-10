module Common

let L = 10

type private Ising2DContext = {
    AdjacencyMatrix : (int * int)[,,]
}
with
    static member singleton =
        let adj =
            Array3D.init<int*int> L L 4 (fun x y -> function
                | 0 -> ((x + 1) % L), (y)
                | 1 -> ((x + L - 1) % L), (y)
                | 2 -> (x), ((y + 1) % L)
                | 3 -> (x), ((y + L - 1) % L)
                | _ -> failwith "how?")
        { AdjacencyMatrix = adj }

    override this.ToString() =
        let sb = System.Text.StringBuilder()

        ignore <| sb.AppendLine("Ising2DContext : \n{")
        for y in 0 .. (L - 1) do
            for x in 0 .. (L - 1) do
                ignore <| sb.Append($"\t ({x}, {y}) -> [ ")
                for p in 0..3 do
                    let (a, b) = this.AdjacencyMatrix.[x,y,p]
                    ignore <| sb.Append($"({a}, {b}) ")
                ignore <| sb.AppendLine($"] ")
        ignore <| sb.AppendLine("}")

        sb.ToString()

let private context = Ising2DContext.singleton
let private Jij = 1.0

let interactionEnergy flip (spins : bool[,]) x y =
    let inline spinEnergy s1 s2 = Jij * if (s1 = s2) then 1.0 else -1.0

    let spin' = spins.[x, y]
    let spin = if flip then not spin' else spin'

    let mutable energy = 0.0
    for p in 0..3 do
        let (nx, ny) = context.AdjacencyMatrix.[x, y, p]
        energy <- energy - spinEnergy spin (spins.[nx, ny])
    energy


type Ising2D = {
    Spins : bool[,]
    H : float
}
with
    static member Apply spins h = { Spins = spins; H = h }
    static member Random () =
        let spins = Array2D.init<bool> L L (fun _ _ -> match System.Random.Shared.Next(2) with | 0 -> false | _ -> true)

        let mutable h = 0.0
        for y in 0 .. (L - 1) do
            for x in 0 .. (L - 1) do
                h <- h + interactionEnergy false spins x y

        { Spins = spins; H = h }

    override this.ToString() =
        let sb = System.Text.StringBuilder()
        ignore <| sb.AppendLine("Ising2D : \n{")
        ignore <| sb.AppendLine("spins : \n\t{")
        for y in 0 .. (L - 1) do
            ignore <| sb.Append("\t\t[ ")
            for x in 0 .. (L - 1) do
                ignore <| sb.Append (if this.Spins.[x, y] then "+ " else "0 ")
            ignore <| sb.AppendLine("]")
        ignore <| sb.AppendLine("\t}")
        ignore <| sb.AppendLine($"ham : {this.H}")
        ignore <| sb.AppendLine("}")

        sb.ToString()