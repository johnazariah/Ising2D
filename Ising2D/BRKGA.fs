module BRKGA
open System
open System.IO

type 't``[]`` with
    member this.Print (elementSelector : 't -> 'a) (elementFormatter : 'a -> string) name =
        this
        |> Array.map (elementSelector >> elementFormatter)
        |> (fun rg -> System.String.Join(',', rg))
        |> printfn "%s : %s" name

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

let inline proportion total percentage = percentage * float total |> Math.Ceiling |> int

type EvolutionParameters = private {
    ElitePercentage : double
    MutantPercentage : double
    MutationBias : double
    EliteBias : double
}
with
    static member Initialize elitePercentage mutantPercentage mutationBias eliteBias =
        let inline ensureClamped f n = if (f >= 0.0) && f < (1.0) then f else failwith $"{n} is not clamped to [0.0, 1.0)"
        {
            ElitePercentage  = ensureClamped elitePercentage "elitePercentage"
            MutantPercentage = ensureClamped mutantPercentage "mutantPercentage"
            MutationBias     = ensureClamped mutationBias "mutationBias"
            EliteBias        = ensureClamped eliteBias "eliteBias"
        }

    static member Default = EvolutionParameters.Initialize 0.3 0.4 0.4 0.8

let DefaultEvolutionParameters = EvolutionParameters.Default


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

    static member inline SortFirstElements (first_is_better : 'f -> 'f -> bool, worst_fitness : 'f) (sort_count : int) (rg1 : ('t * 'f)[]) (rg2 : ('t * 'f)[]) (rg3 : ('t * 'f)[]) =
        let mutable i1 = 0;
        let mutable i2 = 0;
        let mutable i3 = 0;
        let mutable count = sort_count

        let result =
            [|
                while (count > 0) && ((i1 < rg1.Length) || (i2 < rg2.Length) || (i3 < rg3.Length)) do
                    let f1 = if i1 < rg1.Length then snd rg1.[i1] else worst_fitness
                    let f2 = if i2 < rg2.Length then snd rg2.[i2] else worst_fitness
                    let f3 = if i3 < rg3.Length then snd rg3.[i3] else worst_fitness

                    if (first_is_better f1 f2) then
                        if (first_is_better f1 f3) then
                            yield rg1.[i1]; do i1 <- i1 + 1;
                        else
                            yield rg3.[i3]; do i3 <- i3 + 1
                    else
                        if (first_is_better f2 f3) then
                            yield rg2.[i2]; do i2 <- i2 + 1
                        else
                            yield rg3.[i3]; do i3 <- i3 + 1
                    count <- count - 1

                for x in i1 .. (rg1.Length - 1) -> rg1.[x]
                for x in i2 .. (rg2.Length - 1) -> rg2.[x]
                for x in i3 .. (rg3.Length - 1) -> rg3.[x]
            |]

        let inline sortCheck (_, f) (sorted, prev) = (sorted && (first_is_better f prev), f)
        Array.foldBack sortCheck (Array.take sort_count result) (true, worst_fitness)
        |> function
            | (true, _) -> result
            | _ ->
                printfn "WARNING: Merge Inputs NOT Sorted! Fall-back Sorting!"
                rg1.Print snd (sprintf "%O") "rg1"
                rg2.Print snd (sprintf "%O") "rg2"
                rg3.Print snd (sprintf "%O") "rg3"
                result.Print snd (sprintf "%O") "result"
                result |> Array.sortBy snd

    member this.Evolve (pp : PopulationParameters<'t, 'f>) (ep : EvolutionParameters) =
        let members          = this.Unapply
        let populationCount  = members.Length
        let inline partial f = proportion populationCount f

        let eliteCount       = partial ep.ElitePercentage
        let mutantCount      = partial ep.MutantPercentage
        let plebianCount     = populationCount - eliteCount
        let childrenCount    = plebianCount - mutantCount

        let inline pickRandom (min, max) = members.[System.Random.Shared.Next(min, max)]

        let elites =
            members
            |> Array.take eliteCount

        let mutants =
            [| for _ in 0 .. (mutantCount - 1) -> pickRandom (0, eliteCount) |]
            |> Array.Parallel.map (pp.MutationFunction ep.MutationBias)
            |> Array.sortBy snd

        let children =
            [| for _ in 0 .. (childrenCount - 1) -> (pickRandom (0, eliteCount), pickRandom(0, populationCount)) |]
            |> Array.Parallel.map (pp.CrossoverFunction ep.EliteBias)
            |> Array.sortBy snd

        Population.SortFirstElements (pp.Comparer.first_is_better, pp.Comparer.worst_fitness) eliteCount elites mutants children
        |> Population.Apply

let inline private SolveInternal (populationParameters : PopulationParameters<'t, 'f>) (evolutionParameters : EvolutionParameters) (num_iterations : int) =
    let dot = num_iterations / 100
    let outerTime, innerTime = System.Diagnostics.Stopwatch(), System.Diagnostics.Stopwatch();
    outerTime.Start()

    let mutable population = Population.Random populationParameters
    let initial = population.Best
    let results =
        [|
            for i in 0..num_iterations ->
                if i % dot = 0 then printf "."

                innerTime.Restart()
                population <- population.Evolve populationParameters evolutionParameters
                innerTime.Stop()
                {| Iteration = i; IterationTime = innerTime.ElapsedMilliseconds; BestFitness = snd population.Best |}
        |]
    printfn ""
    outerTime.Stop()

    printfn $"Initial: {initial}"
    printfn $"Final: {population.Best}"
    {| TotalTime = outerTime.ElapsedMilliseconds; Results = results; Initial = initial; Final = population.Best |}

let Solve<'t, 'f when 'f : comparison> (experimentName : string) (populationParameters : PopulationParameters<'t, 'f>) (evolutionParameters : EvolutionParameters) (num_iterations : int) =
    let results = SolveInternal populationParameters evolutionParameters num_iterations

    let fileName =
        sprintf "BRKGA_%s_%s_%u.csv" (typeof<'f>.Name) experimentName System.DateTime.Now.Ticks
        |> FileInfo
        |> (fun fi -> fi.FullName)

    let output = new StreamWriter(fileName)
    try
        $"# {{ Experiment : {experimentName}; Iterations : {num_iterations}; Runtime (ms) : {results.TotalTime}; }}"
        |> output.WriteLine
        $"# {{ Population : {populationParameters.InitialPopulationCount}; InitialFitness : {snd results.Initial}; FinalFitness : {snd results.Final} }}"
        |> output.WriteLine
        $"# {{ Elite %% : {evolutionParameters.ElitePercentage}; Mutant %% : {evolutionParameters.MutantPercentage}; EliteCrossover %% : {evolutionParameters.EliteBias}; Mutation %% : {evolutionParameters.MutationBias} }}"
        |> output.WriteLine

        "Iteration, Fitness, Time"
        |> output.WriteLine

        for r in results.Results |> Array.sortBy (fun t -> t.Iteration) do
            $"{r.Iteration}, {r.BestFitness}, {r.IterationTime}"
            |> output.WriteLine

        printfn $"Wrote results to {fileName}"
    finally
        output.Flush ()
        output.Close ()