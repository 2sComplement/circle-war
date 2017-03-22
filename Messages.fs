module CounterWs.Messages

open Fable.Core.JsInterop

type PlayerId = int

type Coordinate = float * float

type WsMessage =
    | IdPlayer of pid:PlayerId
    | PlayerJoined of pid:PlayerId
    | PlayerLeft of pid:PlayerId
    | PostCircle of pid:PlayerId * x:float * y:float
    | DeleteCircle of pid:PlayerId * x:float * y:float
    | NewCircle of pid:PlayerId * x:float * y:float