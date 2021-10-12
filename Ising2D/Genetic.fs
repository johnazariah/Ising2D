module Ising2D_Genetic
open Common
let L = Common.L

let synthesize _ =
    let i = Ising2D.Random ()
    (i, i.H)

let inline flipSpin (spins : bool[,] byref) (h : double byref) x y =
    let before = interactionEnergy false spins x y
    let after  = interactionEnergy true  spins x y
    let dE = after - before
    h <- h + dE
    spins.[x, y] <- not spins.[x, y]

let mutate (mutationPct : double) (input, _) =
    let num_mutations = BRKGA.proportion (L * L) mutationPct
    let inputSpins = input.Spins
    let mutable spins = Array2D.copy<bool> inputSpins
    let mutable h = input.H

    for _ in 0..(num_mutations - 1) do
        let x = System.Random.Shared.Next(L)
        let y = System.Random.Shared.Next(L)
        flipSpin (&spins) (&h) x y

    (Ising2D.Apply spins h, h)

let crossover (eliteBias : double) ((eliteParent, _), (otherParent, _)) =
    let num_crossovers = BRKGA.proportion (L * L) (1.0 - eliteBias)
    let otherParentSpins = otherParent.Spins
    let mutable spins = Array2D.copy eliteParent.Spins
    let mutable h = eliteParent.H

    for _ in 0..(num_crossovers - 1) do
        let x = System.Random.Shared.Next(L)
        let y = System.Random.Shared.Next(L)
        if (spins.[x, y] <> otherParentSpins.[x, y]) then
            flipSpin (&spins) (&h) x y

    (Ising2D.Apply spins h, h)

let crossover_block (eliteBias : double) ((eliteParent, _), (otherParent, _)) =
    let num_crossovers = BRKGA.proportion L (1.0 - eliteBias)
    let otherParentSpins = otherParent.Spins
    let mutable spins = Array2D.copy eliteParent.Spins
    let mutable h = eliteParent.H

    let block_size = BRKGA.proportion L 0.1

    for _ in 0..(num_crossovers - 1) do
        let (x,y) = (System.Random.Shared.Next(L), System.Random.Shared.Next(L))
        let inline clamp j = ((x + j + L - 1) % L, (y + j + L - 1) % L)

        for j in -block_size..block_size do
            let (x', y') = clamp j
            if (spins.[x', y] <> otherParentSpins.[x', y]) then
                flipSpin (&spins) (&h) x' y
            if (spins.[x, y'] <> otherParentSpins.[x, y']) then
                flipSpin (&spins) (&h) x y'

    (Ising2D.Apply spins h, h)


let PopulationParameters : BRKGA.PopulationParameters<_,_> = {
    Comparer = {| first_is_better = (<=); worst_fitness = System.Double.MaxValue |}
    InitialPopulationCount = 100
    SynthesisFunction = synthesize
    MutationFunction = mutate
    CrossoverFunction = crossover
}

let EvolutionParameters = BRKGA.EvolutionParameters.Default

let inline Solve num_iterations = BRKGA.Solve  $"Ising2D_{L}_{num_iterations}" PopulationParameters EvolutionParameters num_iterations
