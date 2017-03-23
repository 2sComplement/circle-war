module CounterWs.ServerModel

open Messages

type Model =
    { playerRegistry: Map<PlayerId,bool>
      circles: PlayerCircles }

let mutable model = 
    { playerRegistry = Map.empty
      circles = Map.empty }
      
// Returns player id
let addPlayer() =
    let add pid =
        model <- { model with playerRegistry = model.playerRegistry |> Map.add pid true }
        pid
    match model.playerRegistry |> Map.tryFindKey (fun pid isRegistered -> not isRegistered) with
    | Some unregistered -> unregistered |> add
    | None -> model.playerRegistry.Count + 1 |> add

let removePlayer pid =
    model <- 
        { model with 
            playerRegistry = model.playerRegistry |> Map.add pid false
            circles = model.circles |> Map.add pid Array.empty }

let addCircle (pid,x,y) = model <- { model with circles = model.circles |> addCircle(pid,x,y) }
let deleteCircle (pid,x,y) = model <- { model with circles = model.circles |> deleteCircle(pid,x,y) }

let players() = model.playerRegistry |> Map.toArray |> Array.filter (fun (p,r) -> r) |> Array.map fst
let circles() = model.circles