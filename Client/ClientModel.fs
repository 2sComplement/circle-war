﻿module CounterWs.ClientModel

open Fable.Core
open Fable.Import
open Elmish
open Fable.Import.Browser
open Messages
open Fable.Core.JsInterop
open Fable.Import

// TYPES

type RelativeClick =
    abstract left: float
    abstract top: float

type ClickEvent =
    inherit EventTarget
    abstract getBoundingClientRect: unit -> RelativeClick

type Model =
    { connected: bool
      playerId: PlayerId option
      otherPlayers: PlayerId list
      circles: Map<PlayerId,Coordinate list> }

type Msg =
    | Noop of unit
    | Error of exn
    | Connected of bool
    | GetPlayers
    | GotPlayers of PlayerId []
    | GetCircles
    | GotCircles of Map<PlayerId,Coordinate array>
    | Send of WsMessage
    | Rcv of WsMessage

let ws = WebSocket.Create("ws://localhost:8080")
console.log(ws.readyState);

let onMessage dispatch =
    fun (msg: MessageEvent) ->
        let msg' = msg.data |> string |> JS.JSON.parse :?> WsMessage
        match msg' with
        | NewCircle _
        | NewPlayer _
        | DeleteCircle _
        | IdPlayer _ as msg -> Rcv msg |> dispatch
        | _ -> console.log(sprintf "Not handling unknown message: %s" (string msg.data))
        unbox None

let onOpen dispatch = fun _ -> Connected true |> dispatch
let onClose dispatch = fun _ -> Connected false |> dispatch

let subscription dispatch =
    ws.onmessage <- unbox (onMessage dispatch)
    ws.onopen <- unbox (onOpen dispatch)
    ws.onclose <- unbox (onClose dispatch)

let subscribe model = Cmd.ofSub subscription

ws.onopen <- fun _ ->
    console.log("ws open")

    unbox None

let send msg =
    let m = JS.JSON.stringify msg
    ws.send m

let init () = { connected = false; playerId = None; otherPlayers = []; circles = Map.empty }, Cmd.batch [ Cmd.ofMsg GetPlayers; Cmd.ofMsg GetCircles ]

let colours = [ "#009688";"#F44336";"#673AB7";"#2196F3";"#4CAF50";"#9C27B0";"#3F51B5";"#03A9F4";"#E91E63";"#8BC34A" ]

// UPDATE

//let outputCircles model = console.log(model.circles |> List.map (fun (pid,(x,y)) -> sprintf "%d (%f,%f)" pid x y) |> List.toArray)

let update (msg:Msg) model =
    match msg with
    | Noop _ -> model, Cmd.none
    | Send m -> model, Cmd.ofFunc send m Noop Error
    | Error ex ->
        console.error("Error: ", ex)
        model, Cmd.none
    | Rcv (NewCircle(pid,x,y)) -> 
        match model.circles |> Map.tryFind pid with
        | Some existing -> { model with circles = model.circles |> Map.add pid ((x,y) :: existing) }, Cmd.none
        | None -> { model with circles = model.circles |> Map.add pid [x,y] }, Cmd.none
    | Rcv (IdPlayer pid) -> { model with playerId = Some pid }, Cmd.none
    | Rcv (NewPlayer pid) -> { model with otherPlayers = model.otherPlayers |> List.append [ pid ]}, Cmd.none 
    | Rcv (DeleteCircle(pid,x,y)) -> { model with circles = model.circles |> Map.map (fun p coords -> if pid = p then coords |> List.except [x,y] else coords) }, Cmd.none
    | Rcv _ -> model, Cmd.none
    | Connected c -> { model with connected = c }, Cmd.none
    | GetPlayers -> model, Cmd.ofPromise (fun _ -> ClientApi.get<PlayerId[]> "/api/players") () GotPlayers Error
    | GotPlayers players -> { model with otherPlayers = Array.toList players |> List.filter (fun p -> p <> model.playerId.Value) |> List.distinct }, Cmd.none
    | GetCircles -> model, Cmd.ofPromise (fun _ -> ClientApi.get<Map<PlayerId,Coordinate array>> "/api/circles") () GotCircles Error
    | GotCircles circles -> { model with circles = circles |> Map.map (fun _ coords -> List.ofArray coords) }, Cmd.none