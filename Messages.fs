module CounterWs.Messages

open Fable.Core.JsInterop

type PlayerId = int

type Coordinate = float * float

type PlayerCircles = Map<PlayerId,Coordinate array>

type WsMessage =
    | IdPlayer of pid:PlayerId
    | PlayerJoined of pid:PlayerId
    | PlayerLeft of pid:PlayerId
    | DeleteCircle of pid:PlayerId * x:float * y:float
    | AddCircle of pid:PlayerId * x:float * y:float

let addCircle (pid,x,y) (circles:PlayerCircles) =
    match circles |> Map.tryFind pid with
    | Some existing -> circles |> Map.add pid (existing |> Array.append [|x,y|])
    | None -> circles |> Map.add pid [|x,y|]

let deleteCircle (pid,x,y) (circles:PlayerCircles) =
    circles |> Map.map (fun p coords -> if pid = p then coords |> Array.except [|x,y|] else coords)
