module Ising2D_Genetic
open Common
let L = Common.L

let synthesize _ =
        let i = Ising2D.Random ()
        (i, i.H)

let mutate (mutationPct : double) (input, _) =
        let num_mutations = BRKGA.proportion (L * L) mutationPct
        let mutable spins = Array2D.copy<bool> input.Spins
        let mutable h = input.H
        let spins' = &spins

        for _ in 0..(num_mutations - 1) do
            let (x, y) = (System.Random.Shared.Next(L), System.Random.Shared.Next(L))
            let before = interactionEnergy false spins x y
            let after  = interactionEnergy true  spins x y
            h <- h - before + after
            spins.[x, y] <- not spins.[x, y]

        (Ising2D.Apply spins h, h)

let crossover (eliteBias : double) ((eliteParent, _), (otherParent, _)) =
        let num_crossovers_into_elite = L - (BRKGA.proportion (L * L) eliteBias)
        let mutable spins = Array2D.copy eliteParent.Spins
        let mutable h = eliteParent.H
        for _ in 0..(num_crossovers_into_elite - 1) do
            let (x, y) = (System.Random.Shared.Next(L), System.Random.Shared.Next(L))
            let eliteParentSpin = eliteParent.Spins.[x, y]
            let otherParentSpin = otherParent.Spins.[x, y]
            if (eliteParentSpin <> otherParentSpin) then
                let before = interactionEnergy false spins x y
                let after  = interactionEnergy true  spins x y
                h <- h - before + after
                spins.[x, y] <- not spins.[x, y]

        (Ising2D.Apply spins h, h)

let PopulationParameters : BRKGA.PopulationParameters<_,_> = {
    Comparer = {| first_is_better = (<=); worst_fitness = System.Double.MaxValue |}
    InitialPopulationCount = 50
    SynthesisFunction = synthesize
    MutationFunction = mutate
    CrossoverFunction = crossover
}

let EvolutionParameters = BRKGA.EvolutionParameters.Default

let Solve = BRKGA.Solve PopulationParameters EvolutionParameters
