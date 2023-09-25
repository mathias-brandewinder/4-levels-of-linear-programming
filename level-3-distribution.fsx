#load "data.fsx"
open Data

// 563.0
countries
|> Array.sumBy (fun x -> x.Population)

// units per factory per unit of time
let capacity = 200.0
let machineCapacity = 50.0
let machineCost = 10.0

let unitSalePrice = 1.0 // price per unit sold
// large distance ~ 3000 kms
let transportationCost = 1.0 / 2500.

type CountryID = | CountryID of string
type FactoryID = | FactoryID of string

type Factory = {
    Location: string //CountryID
    // Capacity: float
    CountriesServed: CountryID []
    // ProductionCostPerUnit: float
    // CostPerUnitPerKilometer: float
    }

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

let factoryLocations = 
    [
        "FACTORY 1", countries.[0]
        "FACTORY 2", countries.[4]
        "FACTORY 3", countries.[10]
        "FACTORY 4", countries.[14]
        "FACTORY 5", countries.[13]
    ]
    |> Map.ofList

(*
Austria  
Cyprus
Germany
Italy
Ireland
*)

#r "nuget: Google.OrTools, Version=9.7.2996"
open Google.OrTools.LinearSolver

// Mixed Integer Linear Programming solver
let solver = Solver.CreateSolver("SCIP")

// constraint: each factory can ship only up to its capacity
// constraint: each country can receive only up to its demand

// variables: shipments (origin, destination)
type Shipment = {
    Origin: string
    Destination: string
    }

// we can ship from anywhere to anywhere
let variables = 
    factories
    |> Seq.collect (fun kv ->
        let factory = kv.Key
        let destinations =
            countries
            |> Array.map (fun c -> c.Name)
            //kv.Value
        destinations
        |> Seq.map (fun country ->
            { Origin = factory; Destination = country },
            // not bounded by capacity anymore
            solver.MakeNumVar(0.0, infinity, $"{factory}-{country}")
            )
        )
    |> Map.ofSeq

// how many machines do we have in a factory
let factorySizes =
    factories
    |> Map.map (fun name _ ->
        solver.MakeIntVar(0, 10, $"SIZE {name}")
        )

// production capacity: we can ship only what we have machines for
// sum shipments <= machines * machine capacity
// capacity - shipments >= 0
factories
|> Map.iter (fun factory destinations ->
    let c = solver.MakeConstraint($"Capacity {factory}")
    c.SetLb(0.0)
    
    let factorySize = factorySizes.[factory]
    c.SetCoefficient(factorySize, machineCapacity)
    
    variables
    |> Map.filter (fun shipment variable ->
        shipment.Origin = factory
        )
    |> Map.iter (fun shipment variable ->
        c.SetCoefficient(variable, -1.0)
        )
    )

// demand
countries
|> Array.iter (fun country ->
    let c = 
        solver.MakeConstraint(
            // CHANGE FROM PREVIOUS MODEL
            // country.Population,
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

// objective

let objective = solver.Objective()
objective.SetMaximization()

// profit
variables
|> Map.iter (fun shipment variable ->
    let origin = 
        countries 
        |> Array.find (fun x -> x.Name = factoryLocations.[shipment.Origin].Name)
    let dest = countries |> Array.find (fun x -> x.Name = shipment.Destination)
    let travelDistance = distance origin.Coords dest.Coords
    let profitPerUnit =
        unitSalePrice
        -
        travelDistance * transportationCost
    objective.SetCoefficient(variable, profitPerUnit)
    )

// machine cost
factorySizes
|> Map.iter (fun factory size ->
    objective.SetCoefficient(size, - machineCost)
    )

// ... and solve
let solution = solver.Solve()

factorySizes
|> Map.iter (fun factory size ->
    printfn $"{factory}: {size.SolutionValue()}"
    )

variables
|> Map.iter (fun k v -> printfn $"{k}: {v.SolutionValue()}")

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

variables
|> Seq.groupBy (fun kv -> kv.Key.Origin)
|> Seq.iter (fun (k,kv) ->
    printfn $"{k}"
    kv 
    |> Seq.filter (fun x -> x.Value.SolutionValue() > 0.0)
    |> Seq.iter (fun x -> printfn $"  {x.Key.Destination}")
    )
