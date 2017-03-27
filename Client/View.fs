module CounterWs.ClientView

open Fable.Core
open Fable.Import
open Elmish
open Messages
open Fable.Core.JsInterop
open Fable.Import
open ClientModel

module R = Fable.Helpers.React
open Fable.Helpers.React.Props

let colours = [ "#673AB7";"#2196F3";"#009688";"#F44336";"#4CAF50";"#9C27B0";"#3F51B5";"#FFC107";"#E91E63";"#8BC34A" ]
let maxPlayers = colours.Length

let view model dispatch =

    let circleRadius = 30
    let spectatorColour = "#000000"

    let validatePlayer f =
        match model.playerId with
        | Some pid ->
            f pid
        | None -> OnClick ignore
    
    let addCircle pid =
        OnClick <| fun e -> 
            let dim = (e.target :?> ClickEvent).getBoundingClientRect()
            let x = e.clientX - dim.left
            let y = e.clientY - dim.top
            AddCircle(pid, x, y) |> Send |> dispatch

    let deleteCircle (pid, x, y) =
        OnClick <| fun e ->
            e.stopPropagation()
            DeleteCircle(pid, x, y) |> Send |> dispatch

    let noClick _ = OnClick ignore
    
    let numCircles pid = match model.circles |> Map.tryFind pid with | Some c -> c.Length | None -> 0
    let getColour pid = if pid < colours.Length then colours.[pid] else spectatorColour
    let playerDesc, onClickAdd, onClickDelete, currentColour =
        match model.playerId with
        | Some pid when pid < maxPlayers -> 
            [ numCircles pid, sprintf "Player%d (you)" pid, getColour pid ], addCircle pid, deleteCircle, getColour pid
        | Some _ | None -> [ -1, "Spectator (you)", spectatorColour ], noClick(), noClick, spectatorColour

    let drawCircle pid x y = 
        let onCircleClick = 
            if pid = model.playerId.Value then OnClick <| fun e -> e.stopPropagation()
            else onClickDelete(pid,x,y)
            
        R.circle [ Cx <| unbox x; Cy <| unbox y; R <| unbox circleRadius; Fill (getColour pid); unbox onCircleClick ] []
            
    let circles = model.circles |> Map.toList |> List.collect (fun (pid,coords) -> coords |> Array.toList |> List.map (fun (x,y) -> drawCircle pid x y))
    let counts,players =
        let div colour = unbox >> List.singleton >> R.div [ Style [ Margin 2; Color colour ]]

        (model.otherPlayers |> List.map (fun pid -> if pid < maxPlayers then numCircles pid, sprintf "Player%d" pid, getColour pid else -1, "Spectator", spectatorColour))
        @ playerDesc
        |> List.sortByDescending (fun (circs,_,_) -> circs)
        |> List.map (fun (circs,name,colour) -> (if circs >= 0 then div colour circs else div colour ""), div colour name)
        |> List.unzip
            
    R.div [] [
        R.div [ Style [ Display "flex"; FlexDirection "row" ]] [
            R.div [ Style [ TextAlign "center" ]] [
                R.svg [ Style [ sprintf "5px solid %s" currentColour |> unbox |> Border; CSSProp.Width 800; Height 600 ]; unbox onClickAdd ] circles
            ]
            R.div [ Style [ Flex <| unbox 100 ]] [
                R.div [ Style [ Display "flex"; FlexDirection "row" ]] [
                    R.div [ Style [ Flex <| unbox 100; TextAlign "right" ]] counts
                    R.div [ Style [ Flex <| unbox 100; TextAlign "left" ]] players
                ]
            ]
        ]
    ]

open Elmish.React

// App
Program.mkProgram init update view
|> Program.withSubscription subscribe
//|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run