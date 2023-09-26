// https://www.distancelatlong.com/all/countries/

open System
open System.IO

type Coordinates = {
    Lat: float
    Lon: float
    }

type Country = {
    Name: string
    Population: float
    Capital: string
    Coords: Coordinates
    }

let countries =
    "data.csv"
    |> File.ReadAllLines
    |> Array.map (fun line ->
        printfn $"{line}"
        let block = line.Split '\t'
        let x = block.[0].IndexOf("(")
        let y = block.[0].IndexOf(")")
        let name = block.[0].[0 .. x - 1].Trim()
        let population = block.[0].[x + 1 .. y - 1] |> float
        {
            Name = name
            Population = population
            Capital = block.[1]
            Coords = {
                Lat = block.[2] |> float
                Lon = block.[3] |> float
                }
        }
        )

let distance (coords1: Coordinates) (coords2: Coordinates) =

    let lat1 = coords1.Lat
    let lon1 = coords1.Lon

    let lat2 = coords2.Lat
    let lon2 = coords2.Lon

    let r = 6371.0
    let p = Math.PI / 180.0

    let a = 
        0.5
        - 
        cos((lat2 - lat1) * p) / 2.0
        + 
        cos(lat1 * p) * cos(lat2 * p) * (1.0 - cos((lon2 - lon1) * p)) / 2.0

    2.0 * r * asin(sqrt(a))

let c1 = countries.[0]
let c2 = countries.[1]
let f = countries |> Array.find (fun x -> x.Name = "France")
let g = countries |> Array.find (fun x -> x.Name = "Germany")

distance (c1.Coords) (c2.Coords)
distance (c1.Coords) (f.Coords)
distance (f.Coords) (g.Coords)

countries
|> Array.map (fun x ->
    x.Capital, distance g.Coords x.Coords
    )

// long ~ 3000 kms
countries
|> Array.collect (fun origin ->
    countries
    |> Array.map (fun dest -> 
        (origin.Name, dest.Name),
        distance origin.Coords dest.Coords
        )
    )
|> Array.sortByDescending (fun (_, d) -> d)
|> Array.take 50
