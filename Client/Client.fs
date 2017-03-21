module CounterWs.Client

open Fable.Core
open Fable.Import
open Elmish
open Fable.Import.Browser
open Messages
open Fable.Core.JsInterop
open Fable.Import.JS

// MODEL

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
      circles: (PlayerId * Coordinate) list }

type Msg =
    | Noop of unit
    | Error of exn
    | Send of WsMessage
    | Rcv of WsMessage
    | Connected of bool

let ws = WebSocket.Create("ws://localhost:8080")
console.log(ws.readyState);

let onMessage dispatch =
    fun (msg: MessageEvent) ->
        let msg' = msg.data |> string |> JSON.parse :?> WsMessage
        match msg' with
        | NewCircle _
        | NewPlayer _
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
    let m = JSON.stringify msg
    ws.send m

let init () = { connected = false; playerId = None; otherPlayers = []; circles = [] }, Cmd.none

// UPDATE

let update (msg:Msg) model =
    match msg with
    | Noop _ -> model, Cmd.none
    | Send m -> model, Cmd.ofFunc send m Noop Error
    | Error ex ->
        console.error("Error: ", ex)
        model, Cmd.none
    | Rcv (NewCircle(pid,x,y)) -> { model with circles = model.circles |> List.append [ pid, (x,y) ] } , Cmd.none
    | Rcv (IdPlayer pid) -> { model with playerId = Some pid }, Cmd.none
    | Rcv (NewPlayer pid) -> { model with otherPlayers = model.otherPlayers |> List.append [ pid ]}, Cmd.none 
    | Rcv _ -> model, Cmd.none
    | Connected c -> { model with connected = c }, Cmd.none

// rendering views with React
module R = Fable.Helpers.React
open Fable.Helpers.React.Props

let view model dispatch =
    let onClick suppress =
        OnClick <| fun e -> 
            let dim = (e.target :?> ClickEvent).getBoundingClientRect()
            let x = e.clientX - dim.left
            let y = e.clientY - dim.top

            if suppress then e.stopPropagation()

            PostCircle(model.playerId.Value, x, y) |> Send |> dispatch

    let disable = not model.connected
    let drawCirc x y = R.circle [ Cx <| unbox x; Cy <| unbox y; R <| unbox 50; Fill "green"; onClick true |> unbox ] []
    let circles = model.circles |> List.map (fun (pid,(x,y)) -> drawCirc x y)
    R.div [ Style [ TextAlign "center" ]] [
        R.div [] [ unbox (match model.playerId with | Some pid -> sprintf "Player%d" pid | None -> "waiting for server") ]
        R.svg [ Style [ Border "1px solid red"; CSSProp.Width 800; Height 600 ]; onClick false |> unbox ] circles
    ]

open Elmish.React

// App
Program.mkProgram init update view
|> Program.withSubscription subscribe
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run