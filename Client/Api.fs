module CounterWs.ClientApi

open Fable.Import
open Fable.Core
open Fable.Import.Browser
open Fable.Core.JsInterop
open Fable.PowerPack.Fetch
open System
open Fable.PowerPack.Fetch.Fetch_types
open Fable.PowerPack

JsInterop.importAll "whatwg-fetch"

let private operation op = 
    promise { 
        let headers = [ HttpRequestHeaders.ContentType "application/json" ]
        let! res = op headers
        return res
    }

let internal request<'T> (f : HttpRequestHeaders list -> JS.Promise<Response>) = 
    promise { 
        let! resp = operation f
        if not resp.Ok then failwith <| sprintf "Request failed with %d" resp.Status
    }

let [<PassGenerics>] get<'T> url : JS.Promise<'T> = 
    operation (fun headers -> fetchAs url [ RequestProperties.Headers headers
                                            RequestProperties.Credentials RequestCredentials.Include ])