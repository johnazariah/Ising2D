module BRKGA
open System

type FitnessFunction<'t,   'f when 'f : comparison> = 't -> 'f
type SynthesisFunction<'t, 'f when 'f : comparison> = int -> ('t * 'f)
type MutationFunction<'t,  'f when 'f : comparison> = double -> ('t * 'f) -> ('t * 'f)
type CrossoverFunction<'t, 'f when 'f : comparison> = double -> ('t * 'f) * ('t * 'f) -> ('t * 'f)

type PopulationParameters<'t, 'f when 'f : comparison> = {
    InitialPopulationCount : int
    FitnessFunction   : FitnessFunction<'t, 'f>
    SynthesisFunction : SynthesisFunction<'t, 'f>
    MutationFunction  : MutationFunction<'t, 'f>
    CrossoverFunction : CrossoverFunction<'t, 'f>
}

type EvolutionParameters = private {
    ElitePercentage : double
    MutantPercentage : double
    MutationBias : double
    EliteBias : double
}
with
    static member Initialize elitePercentage mutantPercentage mutationBias eliteBias = {
        // TODO: validate parameters
        ElitePercentage = elitePercentage
        MutantPercentage = mutantPercentage
        MutationBias = mutationBias
        EliteBias = eliteBias
    }
    static member Default = EvolutionParameters.Initialize 0.3 0.2 0.3 0.75

let DefaultEvolutionParameters = EvolutionParameters.Default

let inline proportion total percentage = percentage * float total |> Math.Ceiling |> int

type Population<'t, 'f when 'f : comparison> = private | Population of ('t * 'f)[]
with
    member this.Unapply = match this with Population x -> x
    static member Apply (individuals : ('t *'f)[]) =
        individuals
        |> Array.sortBy snd
        |> Population

    static member Random (pp : PopulationParameters<'t, 'f>) =
        seq { 0..(pp.InitialPopulationCount - 1) }
        |> Seq.map pp.SynthesisFunction
        |> Seq.toArray
        |> Population.Apply

    member inline this.Best = this.Unapply.[0]

    member this.Evolve (pp : PopulationParameters<'t, 'f>) (ep : EvolutionParameters) =
        let members = this.Unapply
        let populationCount = members.Length
        let partial = proportion populationCount
        let (eliteCount, mutantCount) = (partial ep.ElitePercentage, partial ep.MutantPercentage)
        let plebianCount = populationCount - eliteCount
        let childrenCount = plebianCount - mutantCount

        let inline pickRandom (min, max) = members.[System.Random.Shared.Next(min, max)]

        let elites =
            members
            |> Array.take eliteCount

        let mutants =
            [| for _ in 0 .. (mutantCount) -> pickRandom (0, eliteCount) |]
            |> Array.map (pp.MutationFunction ep.MutationBias)

        let children =
            [| for _ in 0 .. (childrenCount) -> (pickRandom (0, eliteCount), pickRandom(eliteCount, populationCount)) |]
            |> Array.map (pp.CrossoverFunction ep.EliteBias)

        [|
            yield! elites
            yield! mutants
            yield! children
        |]
        |> Population.Apply

let Solve (populationParameters : PopulationParameters<_, _>) (evolutionParameters : EvolutionParameters) (num_iterations : int) =
    let outerTime, innerTime = System.Diagnostics.Stopwatch(), System.Diagnostics.Stopwatch();
    outerTime.Start()

    let mutable population = Population.Random populationParameters
    let results =
        [|
            for i in 0..num_iterations ->
                innerTime.Restart()
                population <- population.Evolve populationParameters evolutionParameters
                innerTime.Stop()
                {| Iteration = i; IterationTime = innerTime.ElapsedMilliseconds; BestFitness = snd population.Best |}
        |]

    outerTime.Stop()
    printfn $"F# - Evolved Population {num_iterations} times in {outerTime.ElapsedMilliseconds} ms."

    {| TotalTime = outerTime.ElapsedMilliseconds; Results = results |}
