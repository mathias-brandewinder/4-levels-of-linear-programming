#load "data.fsx"
open Data

// 563.0
countries
|> Array.sumBy (fun x -> x.Population)

let factory1 = countries.[ 0 .. 9 ] |> Array.map (fun x -> x.Name)
let factory2 = countries.[ 0 .. 4 ] |> Array.map (fun x -> x.Name)
let factory3 = countries.[ 10 .. ] |> Array.map (fun x -> x.Name)
let factory4 = countries.[ 10 .. 14 ] |> Array.map (fun x -> x.Name)
let factory5 = countries.[ 13 .. 14 ] |> Array.map (fun x -> x.Name)

let factories =
    [
        "FACTORY 1", factory1
        "FACTORY 2", factory2
        "FACTORY 3", factory3
        "FACTORY 4", factory4
        "FACTORY 5", factory5
    ]
    |> Map.ofList

let capacity = 150.0
// total capacity: 150 * 5 = 750 > demand = 563


#r "nuget: Google.OrTools, Version=9.7.2996"
open Google.OrTools.LinearSolver

// Pure Linear Programming solver
let solver = Solver.CreateSolver("GLOP")

// constraint: each factory can ship only up to its capacity
// constraint: each country must receive its demand

// variables: shipments (origin, destination)
type Shipment = {
    Origin: string
    Destination: string
    }

let variables = 
    factories
    |> Seq.collect (fun kv ->
        let factory = kv.Key
        let destinations = kv.Value
        destinations
        |> Seq.map (fun country ->
            { Origin = factory; Destination = country },
            solver.MakeNumVar(0.0, capacity, $"{factory}-{country}")
            )
        )
    |> Map.ofSeq

// production capacity
factories
|> Map.iter (fun factory destinations ->
    let c = solver.MakeConstraint(0.0, capacity, $"Capacity {factory}")
    variables
    |> Map.filter (fun shipment variable ->
        shipment.Origin = factory
        )
    |> Map.iter (fun shipment variable ->
        c.SetCoefficient(variable, 1.0)
        )
    )

// demand
countries
|> Array.iter (fun country ->
    let c = 
        solver.MakeConstraint(
            country.Population,
            country.Population, 
            $"Demand {country}"
            )
    variables
    |> Map.filter (fun shipment variable ->
        shipment.Destination = country.Name
        )
    |> Map.iter (fun shipment variable ->
        c.SetCoefficient(variable, 1.0)
        )
    )

// ... and solve
let solution = solver.Solve()

// At what capacity does it become infeasible / feasible?