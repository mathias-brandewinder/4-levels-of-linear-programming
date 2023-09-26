#load "data.fsx"
open Data

// 563.0
countries
|> Array.sumBy (fun x -> x.Population)

// units per factory per unit of time
let capacity = 150.0

// NEW: MODIFIED FROM PREVIOUS SCRIPT
let unitSalePrice = 1.0 // price per unit sold
// large distance ~ 3000 kms
let transportationCost = 1.0 / 2500.

// countries served
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

// NEW: MODIFIED FROM PREVIOUS SCRIPT
// Each factory is located in a country
let factoryLocations = 
    [
        "FACTORY 1", countries.[0]
        "FACTORY 2", countries.[4]
        "FACTORY 3", countries.[10]
        "FACTORY 4", countries.[14]
        "FACTORY 5", countries.[13]
    ]
    |> Map.ofList

#r "nuget: Google.OrTools, Version=9.7.2996"
open Google.OrTools.LinearSolver

// Pure Linear Programming solver
let solver = Solver.CreateSolver("GLOP")

// variables: shipments (origin, destination)
// -----------------------------------------------------------------------------

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

// production capacity constraint
// constraint: each factory can ship only up to its capacity
// -----------------------------------------------------------------------------

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

// demand constraint
// constraint: each country can receive up to its demand
// -----------------------------------------------------------------------------

countries
|> Array.iter (fun country ->
    let c = 
        solver.MakeConstraint(
            // NEW: MODIFIED FROM PREVIOUS SCRIPT
            0.0,
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

// Objective
// We want to maximize profit
// -----------------------------------------------------------------------------

let objective = solver.Objective()
objective.SetMaximization()

variables
|> Map.iter (fun shipment variable ->
    // country where the shipment comes from
    let origin = 
        countries 
        |> Array.find (fun x -> 
            x.Name = factoryLocations.[shipment.Origin].Name
            )
    // country where the shipment goes to
    let dest = 
        countries 
        |> Array.find (fun x -> 
            x.Name = shipment.Destination
            )
    let travelDistance = distance origin.Coords dest.Coords
    // profit per unit shipped
    let profitPerUnit =
        unitSalePrice
        -
        travelDistance * transportationCost
    objective.SetCoefficient(variable, profitPerUnit)
    )

// ... and solve
let solution = solver.Solve()

// We can observe how big each shipment should be
variables
|> Map.iter (fun k v -> printfn $"{k}: {v.SolutionValue()}")

// Which factories are fully utilized? Which are barely utilized?
// Are we shipping all we could ship?
// Which countries get / don't get all they wanted? Why?

variables
|> Seq.groupBy (fun kv -> kv.Key.Destination)
|> Seq.map (fun (k, v) -> 
    k, 
    v 
    |> Seq.sumBy (fun x -> x.Value.SolutionValue())
    )
|> Seq.toArray

variables
|> Seq.groupBy (fun kv -> kv.Key.Origin)
|> Seq.map (fun (k, v) -> 
    k, 
    v 
    |> Seq.sumBy (fun x -> x.Value.SolutionValue())
    )
|> Seq.toArray

variables
|> Seq.sumBy (fun kv -> kv.Value.SolutionValue())

countries
|> Seq.sumBy (fun kv -> kv.Population)
