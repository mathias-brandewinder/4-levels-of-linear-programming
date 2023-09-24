#r "nuget: Google.OrTools, Version=9.7.2996"
open Google.OrTools.LinearSolver

// MILP
let solver = Solver.CreateSolver("SCIP")

type LocationID =
    | LocationID of string

type Move = {
    Origin: LocationID
    Destination: LocationID
    }

// setup
// -----------------------------------------------------------------------------

#load "data.fsx"
open Data

let locations =
    countries
    |> Array.map (fun c -> c.Capital |> LocationID)
//[| 'A' .. 'Z' |] |> Array.map (string >> LocationID)

let n = locations |> Array.length

let moves =
    [|
        for i in 0 .. (n - 1) do
            for j in 0 .. (n - 1) do
                if i <> j
                then 
                    yield 
                        { 
                            Origin = locations.[i]
                            Destination = locations.[j]
                        },
                        solver.MakeBoolVar($"X_{i}_{j}")
                        
    |]
    |> Map.ofArray

// fake for now
let rng = System.Random 0
let costs =
    moves
    |> Map.map (fun move _ -> 
        let origin = countries |> Array.find (fun x -> LocationID x.Capital = move.Origin)
        let dest = countries |> Array.find (fun x -> LocationID x.Capital = move.Destination)
        let cost = distance origin.Coords dest.Coords
        cost
        //rng.NextDouble()
        )

// constraints
// -----------------------------------------------------------------------------

// every city is visited exactly once:
// each city is entered exactly once.
locations
|> Array.iter (fun location ->
    // create constraint
    let c = solver.MakeConstraint($"Enter {location}")
    c.SetBounds(1.0, 1.0)
    moves
    |> Map.filter (fun move _ -> move.Destination = location)
    // sum = 1
    |> Map.iter (fun _ variable -> c.SetCoefficient(variable, 1.0))
    )

// every city is visited exactly once:
// each city is exited exactly once.
locations
|> Array.iter (fun location ->
    // create constraint
    let c = solver.MakeConstraint($"Leave {location}")
    c.SetBounds(1.0, 1.0)
    moves
    |> Map.filter (fun move _ -> move.Origin = location)
    // sum = 1
    |> Map.iter (fun _ variable -> c.SetCoefficient(variable, 1.0))
    )

// no sub-cycles
// create variables

type Order = | Order of LocationID

// TODO eliminate location 0
let orders = 
    locations
    // ignore the first in the list: by convention, starting point
    |> Array.skip 1
    |> Array.map (fun location ->
        Order location,
        solver.MakeIntVar(1, n, $"Order Variable {location}")
        )
    |> Map.ofArray

// u_i - u_j + (n - 1) * x_i,j <= (n - 2) , 2 <= i <> j <= n

orders
|> Map.iter (fun origin originVariable ->
    orders
    |> Map.iter (fun destination destinationVariable ->
        let o = match origin with | Order o -> o
        let d = match destination with | Order d -> d
        if o <> d
        then
            let c = solver.MakeConstraint($"Cycle {origin} {destination}")
            c.SetCoefficient(originVariable, 1)
            c.SetCoefficient(destinationVariable, -1)

            c.SetCoefficient(moves.[ { Origin = o; Destination = d } ], float (n - 1))
            c.SetUb(float (n - 2))
        )
    )

let objective = solver.Objective()
moves
|> Map.iter (fun move variable ->
    let cost = costs.[move]
    objective.SetCoefficient(variable, cost)
    )

objective.SetMinimization()

let result = solver.Solve()

moves
|> Map.iter (fun move variable ->
    printfn $"{move}: {variable.SolutionValue()}"
    )

orders
|> Map.iter (fun order variable ->
    printfn $"{order}: {variable.SolutionValue()}")

orders
|> Seq.sortBy (fun kv -> kv.Value.SolutionValue())
|> Seq.map (fun kv -> kv.Key)
|> Seq.iter (fun x -> printfn $"{x}")