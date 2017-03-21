module CounterWs.Server

open Fable.Core
open Fable.Import
open Elmish
open Fable.Import.Browser
open Fable.Helpers.Ws
open Fable.Core.JsInterop
open Fable.Import.Node
open Messages
open Fable.Import.JS
open Fable.Import.express
open ServerModel

// Helper types for running an express server

type HttpServer =
    inherit http_types.Server
    abstract on: string * obj -> unit

type HttpServerFactory =
    abstract createServer: unit -> HttpServer

// Create Express server
let serv = importDefault<HttpServerFactory>("http").createServer()
let app = express.Invoke()

let path = __dirname + "/../../"

console.log(sprintf "Server content from %s" path)

app.``use``(express.``static``.Invoke(path)) |> ignore

// Create Websocket server
let opts = [ ServerOptions.Server <| unbox serv ]
let wsServer = Server.createServer(unbox opts)

let send (ws:Ws.WebSocket) msg =
    msg |> JSON.stringify |> ws.send

let broadcastAll msg =
    wsServer.clients
    |> Seq.iter (fun c -> if c.readyState = ws.OPEN then send c msg)

let broadcastExcept ws msg =
    wsServer.clients
    |> Seq.iter (fun c -> if c <> ws && c.readyState = ws.OPEN then send c msg)

wsServer.on_connection(fun ws ->
    let pid = addPlayer()

    if pid > 10 then
        console.log("Max players reached")
    else
        console.log(sprintf "Assigned new player id %d" pid)

        // Identify the player that joined
        IdPlayer(pid) |> send ws

        // Let everyone else know that a player has joined
        NewPlayer(pid) |> broadcastExcept ws
    
    ws.on_message <| fun msg ->
        let msg' = JSON.parse (string msg) :?> WsMessage
        match msg' with
        | PostCircle (pid,x,y) -> NewCircle(pid,x,y) |> broadcastAll
        | DeleteCircle (pid,x,y) -> DeleteCircle(pid,x,y) |> broadcastAll
        | _ -> ()

    ws.on_close(fun _ ->
        console.log(sprintf "Player%d left, removing shapes" pid)
        removePlayer pid
    )
)

serv.on("request", app)
serv.listen(8080, unbox <| fun () -> console.log("Listening on http://localhost:8080")) |> ignore
