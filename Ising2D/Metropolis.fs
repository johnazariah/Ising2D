module Ising2D_MC
open Common

let SolveMetropolis (temp : float) (num_iterations : int) =
    let mutable lattice = Ising2D.Random()
    let mutable flips = 0

    let evolve x y =
        let before = interactionEnergy false lattice.Spins x y
        let after  = interactionEnergy true  lattice.Spins x y
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
            lattice.Spins.[x, y] <- not lattice.Spins.[x, y]
            lattice <- { lattice with H = lattice.H + dE }

    let start = System.DateTime.Now
    printfn "Before : \n%O : %s" lattice (start.ToString("r"))

    for _ in 0..num_iterations do
        let (x, y) = (System.Random.Shared.Next(L), System.Random.Shared.Next(L))
        evolve x y

    let finish = System.DateTime.Now
    printfn "After : \n%O : %s" lattice (finish.ToString("r"))

    let duration = finish - start
    printfn $"F# - Solving Ising2D {L} x {L} ({num_iterations} iterations) at {temp}K took {duration.TotalMilliseconds} ms. {flips} spins were flipped."