module Ising2D_MC

let private L = 256
let mutable flips = 0

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

let private Jij = 1.0

let private context = Ising2DContext.singleton

let inline private interactionEnergy flip (spins : bool[,]) (x, y) =
    let inline spinEnergy s1 s2 = Jij * if (s1 = s2) then 1.0 else -1.0

    let spin = if flip then not spins.[x, y] else spins.[x, y]

    let mutable energy = 0.0
    for p in 0..3 do
        let (nx, ny) = context.AdjacencyMatrix.[x, y, p]
        energy <- energy - spinEnergy spin (spins.[nx, ny])
    energy

type private Ising2D = {
    Spins : bool[,]
    H : float
}
with
    static member Random () =
        let spins = Array2D.init<bool> L L (fun _ _ -> match System.Random.Shared.Next(2) with | 0 -> false | _ -> true)

        let mutable h = 0.0
        for y in 0 .. (L - 1) do
            for x in 0 .. (L - 1) do
                h <- h + interactionEnergy false spins (x, y)

        { Spins = spins; H = h }

    override this.ToString() =
        let sb = System.Text.StringBuilder()
        ignore <| sb.AppendLine("Ising2D : \n{")
        ignore <| sb.AppendLine("spins : \n\t{")
        for y in 0 .. (L - 1) do
            ignore <| sb.Append("\t\t[")
            for x in 0 .. (L - 1) do
                ignore <| sb.Append (if this.Spins.[x, y] then "+ " else "0 ")
            ignore <| sb.AppendLine("]")
        ignore <| sb.AppendLine("\t}")
        ignore <| sb.AppendLine($"ham : {this.H}")
        ignore <| sb.AppendLine("}")

        sb.ToString()

let SolveMC (temp : float) (num_iterations : int) =
    let mutable lattice = Ising2D.Random()

    let evolve cell =
        let before = interactionEnergy false lattice.Spins cell
        let after  = interactionEnergy true  lattice.Spins cell
        let dE = after - before
        let accept =
            if dE <= 0.0
            then true
            else
                do flips <- flips + 1
                let probability = exp(-dE/temp)
                let random = System.Random.Shared.NextDouble()
                probability >= random

        if accept then
            let (x, y) = cell
            lattice.Spins.[x, y] <- not lattice.Spins.[x, y]
            lattice <- { lattice with H = lattice.H + dE }

    let start = System.DateTime.Now
    printfn "Before : \n%O : %s" lattice (start.ToString("r"))
    for _ in 0..num_iterations do
        let x = System.Random.Shared.Next(L)
        let y = System.Random.Shared.Next(L)
        evolve (x, y)
    let finish = System.DateTime.Now
    printfn "After : \n%O : %s" lattice (finish.ToString("r"))
    let duration = finish - start
    printfn $"F# - Solving Ising2D {L} x {L} ({num_iterations} iterations) at {temp}K took {duration.TotalMilliseconds} ms. {flips} spins were flipped."