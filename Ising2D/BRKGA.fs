module BRKGA
open System

type SynthesisFunction<'t, 'f when 'f : comparison> = int -> ('t * 'f)
type MutationFunction<'t,  'f when 'f : comparison> = double -> ('t * 'f) -> ('t * 'f)
type CrossoverFunction<'t, 'f when 'f : comparison> = double -> ('t * 'f) * ('t * 'f) -> ('t * 'f)

type PopulationParameters<'t, 'f when 'f : comparison> = {
    Comparer : {| first_is_better : 'f -> 'f -> bool; worst_fitness : 'f |}
    InitialPopulationCount : int
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

let printArray name (rg : ('t * 'f)[]) =
    rg
    |> Array.map (fun (_, f) -> sprintf "%O" f)
    |> (fun rg -> String.Join(',', rg))
    |> printfn "%s : %s" name

let merge3<'t, 'f when 'f : comparison> (first_is_better : 'f -> 'f -> bool, worst_fitness : 'f) (rg1 : ('t * 'f)[]) (rg2 : ('t * 'f)[]) (rg3 : ('t * 'f)[]) =
    let mutable i1 = 0
    let mutable i2 = 0
    let mutable i3 = 0

    let result =
        [|
            let mutable f1 = if i1 < rg1.Length then snd rg1.[i1] else worst_fitness
            let mutable f2 = if i2 < rg2.Length then snd rg2.[i2] else worst_fitness
            let mutable f3 = if i3 < rg3.Length then snd rg3.[i3] else worst_fitness

            while not ((f1 = worst_fitness) && (f2 = worst_fitness) && (f3 = worst_fitness)) do
                if (first_is_better f1 f2) then
                    if (first_is_better f1 f3) then
                        yield rg1.[i1]
                        i1 <- i1 + 1
                        f1 <- if i1 = rg1.Length then worst_fitness else snd rg1.[i1]
                    else
                        yield rg3.[i3]
                        i3 <- i3 + 1
                        f3 <- if i3 = rg3.Length then worst_fitness else snd rg3.[i3]
                else
                    if (first_is_better f2 f3) then
                        yield rg2.[i2]
                        i2 <- i2 + 1
                        f2 <- if i2 = rg2.Length then worst_fitness else snd rg2.[i2]
                    else
                        yield rg3.[i3]
                        i3 <- i3 + 1
                        f3 <- if i3 = rg3.Length then worst_fitness else snd rg3.[i3]
        |]

    let sortCheck (_, f) (sorted, prev) =
        (sorted && (first_is_better f prev), f)

    match Array.foldBack sortCheck result (true, worst_fitness) with
    | (true, _) -> result
    | _ ->
        printfn "merge inputs are not sorted - falling back and sorting"
        rg1 |> printArray "rg1"
        rg2 |> printArray "rg2"
        rg3 |> printArray "rg3"
        result |> printArray "result"
        result |> Array.sortBy snd

type Population<'t, 'f when 'f : comparison> = private | Population of ('t * 'f)[]
with
    member this.Unapply = match this with Population x -> x
    static member Apply (individuals : ('t *'f)[]) =
        individuals
        |> Population

    static member Random (pp : PopulationParameters<'t, 'f>) =
        seq { 0..(pp.InitialPopulationCount - 1) }
        |> Seq.map pp.SynthesisFunction
        |> Seq.toArray
        |> Array.sortBy snd
        |> Population.Apply

    member inline this.Best = this.Unapply.[0]

    member this.Evolve (pp : PopulationParameters<'t, 'f>) (ep : EvolutionParameters) =
        let members = this.Unapply
        let populationCount = members.Length
        let partial = proportion populationCount
        let (eliteCount, mutantCount) = (partial ep.ElitePercentage, partial ep.MutantPercentage)
        let plebianCount = populationCount - eliteCount
        let childrenCount = plebianCount - mutantCount

        printfn $"population : {populationCount}; elites : {eliteCount}; mutants : {mutantCount}; plebians : {plebianCount}"

        let inline pickRandom (min, max) = members.[System.Random.Shared.Next(min, max)]

        let elites =
            members
            |> Array.take eliteCount
            //|> Array.sortBy snd

        let mutants =
            [| for _ in 0 .. (mutantCount) -> pickRandom (0, eliteCount) |]
            |> Array.map (pp.MutationFunction ep.MutationBias)
            |> Array.sortBy snd

        let children =
            [| for _ in 0 .. (childrenCount) -> (pickRandom (0, eliteCount), pickRandom(0, populationCount)) |]
            |> Array.map (pp.CrossoverFunction ep.EliteBias)
            |> Array.sortBy snd

        merge3 (pp.Comparer.first_is_better, pp.Comparer.worst_fitness) elites mutants children
        |> Population.Apply

let Solve (populationParameters : PopulationParameters<_, _>) (evolutionParameters : EvolutionParameters) (num_iterations : int) =
    let outerTime, innerTime = System.Diagnostics.Stopwatch(), System.Diagnostics.Stopwatch();
    outerTime.Start()

    let mutable population = Population.Random populationParameters
    let initial = population.Best
    let results =
        [|
            for i in 0..num_iterations ->
                if i % 1000 = 0 then printf "."
                innerTime.Restart()
                population <- population.Evolve populationParameters evolutionParameters
                innerTime.Stop()
                {| Iteration = i; IterationTime = innerTime.ElapsedMilliseconds; BestFitness = snd population.Best |}
        |]
    printfn ""
    outerTime.Stop()
    {| TotalTime = outerTime.ElapsedMilliseconds; (* Results = results; *) Initial = initial; Final = population.Best |}
