

// For more information see https://aka.ms/fsharp-console-apps
let results = Ising2D_Genetic.Solve 5000
printfn "%s" <| results.Initial.ToString()
printfn "%s" <| results.Final.ToString()
printfn $"{results.TotalTime} ms"