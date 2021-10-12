module Common

let L = 20

let inline private neighbour (x, y, p) =
    match p with
    | 0 -> ((x + 1) % L), (y)
    | 1 -> ((x + L - 1) % L), (y)
    | 2 -> (x), ((y + 1) % L)
    | 3 -> (x), ((y + L - 1) % L)
    | _ -> failwith "how?"

type private JijMatrix = JijMatrix of float[,,]
with
    member inline this.Unapply = match this with | JijMatrix x -> x

    static member ConstantFerromagnet =
        Array3D.init<float> L L 4 (fun x y -> function
            | 0 -> 1.0 //((x + 1) % L), (y)
            | 1 -> 1.0 //((x + L - 1) % L), (y)
            | 2 -> 1.0 //(x), ((y + 1) % L)
            | 3 -> 1.0 //(x), ((y + L - 1) % L)
            | _ -> failwith "how?")
        |> JijMatrix

    override this.ToString() =
        let jij = this.Unapply
        let sb = System.Text.StringBuilder()

        ignore <| sb.AppendLine("Ising2DContext : \n{")
        for y in 0 .. (L - 1) do
            for x in 0 .. (L - 1) do
                ignore <| sb.Append($"\t ({x}, {y}) -> [ ")
                for p in 0..3 do
                    let (a, b) = neighbour(x, y, p)
                    let f = jij.[x,y,p]
                    ignore <| sb.Append($"({a}, {b}) [{f}]")
                ignore <| sb.AppendLine($"] ")
        ignore <| sb.AppendLine("}")

        sb.ToString()

let private Jij = JijMatrix.ConstantFerromagnet.Unapply

let interactionEnergy flip (spins : bool[,]) x y =
    let spin' = spins.[x, y]
    let spin = if flip then not spin' else spin'

    let mutable energy = 0.0
    for p in 0..3 do
        let (nx, ny) = neighbour(x, y, p)
        let neighbour_spin = spins.[nx, ny]
        let j = Jij.[x, y, p]
        energy <- if spin = neighbour_spin then energy - j else energy + j
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

        Ising2D.Apply spins h

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