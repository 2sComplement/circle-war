module CounterWs.ServerModel

open Messages

type Model =
    { playerRegistry: Map<PlayerId,bool>
      shapes: Map<PlayerId,Coordinate array> }

let mutable model = 
    { playerRegistry = Map.empty
      shapes = Map.empty }
      
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
            shapes = model.shapes |> Map.add pid Array.empty }
