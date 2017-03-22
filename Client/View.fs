module CounterWs.ClientView

open Fable.Core
open Fable.Import
open Elmish
open Fable.Import.Browser
open Messages
open Fable.Core.JsInterop
open Fable.Import
open ClientModel

module R = Fable.Helpers.React
open Fable.Helpers.React.Props

let view model dispatch =

    let validatePlayer f =
        match model.playerId with
        | Some pid ->
            f pid
        | None -> OnClick ignore
    
    let postCircle =
        fun pid ->
            OnClick <| fun e -> 
                let dim = (e.target :?> ClickEvent).getBoundingClientRect()
                let x = e.clientX - dim.left
                let y = e.clientY - dim.top
                PostCircle(pid, x, y) |> Send |> dispatch
        |> validatePlayer

    let deleteCircle (pid, x, y) =
        fun _ ->
            OnClick <| fun e ->
                e.stopPropagation()
                DeleteCircle(pid, x, y) |> Send |> dispatch
        |> validatePlayer
        
    let disable = not model.connected
    let drawCirc pid x y = 
        let onCircleClick = 
            if pid = model.playerId.Value then OnClick <| fun e -> e.stopPropagation()
            else deleteCircle(pid,x,y)
            
        R.circle [ Cx <| unbox x; Cy <| unbox y; R <| unbox 50; Fill colours.[pid % colours.Length]; unbox onCircleClick ] []
            
    let circles = model.circles |> Map.toList |> List.collect (fun (pid,coords) -> coords |> List.map (fun (x,y) -> drawCirc pid x y))
    let currentColour = match model.playerId with | Some pid -> colours.[pid % colours.Length] | None -> "#000000"
    let otherPlayers = []
        //model.otherPlayers |> List.sort |> List.map (fun pid -> console.log(pid); R.div [ Style [ Color colours.[pid % colours.Length]; MarginLeft 5 ]] [ unbox <| sprintf "Player%d" pid ])
    R.div [] [
        R.div [ Style [ Display "flex"; FlexDirection "row" ]] [
            R.div [ Style [ Color currentColour ]] [ unbox (match model.playerId with | Some pid -> sprintf "You are: Player%d" pid | None -> "waiting for server") ]
            R.div [ Style [ Flex <| unbox 100 ]] []
            R.div [ Style [ Display "flex"; FlexDirection "row" ]] ((R.div [] [ unbox "Others: " ]) :: otherPlayers)
        ]
        R.div [ Style [ TextAlign "center" ]] [
            R.svg [ Style [ sprintf "1px solid %s" currentColour |> unbox |> Border; CSSProp.Width 800; Height 600 ]; unbox postCircle ] circles
        
        ]
    ]

open Elmish.React

// App
Program.mkProgram init update view
|> Program.withSubscription subscribe
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run