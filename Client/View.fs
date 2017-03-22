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

let colours = [ "#673AB7";"#2196F3";"#009688";"#F44336";"#4CAF50";"#9C27B0";"#3F51B5";"#03A9F4";"#E91E63";"#8BC34A" ]

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
    let numCircles pid = match model.circles |> Map.tryFind pid with | Some c -> c.Length | None -> 0
    let counts,players =
        let div pid = unbox >> List.singleton >> R.div [ Style [ Margin 2; Color colours.[pid % colours.Length]]]
        (numCircles model.playerId.Value, sprintf "Player%d (you)" model.playerId.Value, model.playerId.Value) :: (model.otherPlayers |> List.map (fun pid -> numCircles pid, sprintf "Player%d" pid, pid))
        |> List.sortByDescending (fun (c,_,_) -> c)
        |> List.map (fun (circs,name,pid) -> div pid circs, div pid name)
        |> List.unzip
        //|> List.map (fun pid -> R.div [ Style [ Color colours.[pid % colours.Length]]] [ unbox <| if pid = model.playerId.Value then sprintf "Player%d (you)" pid else sprintf "Player%d" pid])
    //console.log(Array.ofList players)
        //model.otherPlayers |> List.sort |> List.map (fun pid -> console.log(pid); R.div [ Style [ Color colours.[pid % colours.Length]; MarginLeft 5 ]] [ unbox <| sprintf "Player%d - %d" (numCircles pid) ])
    R.div [] [
        R.div [ Style [ Color currentColour ]] [ unbox (match model.playerId with | Some pid -> sprintf "You are: Player%d" pid | None -> "waiting for server") ]
        R.div [ Style [ Display "flex"; FlexDirection "row" ]] [
            R.div [ Style [ TextAlign "center" ]] [
                R.svg [ Style [ sprintf "1px solid %s" currentColour |> unbox |> Border; CSSProp.Width 800; Height 600 ]; unbox postCircle ] circles
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
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run