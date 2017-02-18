module FsAutoComplete.Suave

open System.IO
open Suave
open Suave.Http
open Suave.Operators
open Suave.Web
open Suave.WebPart
open Suave.WebSocket
open Suave.Sockets.Control
open Suave.Filters
open Suave.Logging

open Newtonsoft.Json
open Microsoft.FSharp.Compiler

open FsAutoComplete
open FsAutoComplete.JsonSerializer

[<AutoOpen>]
module Contract =
    type ParseRequest = { FileName : string; IsAsync : bool; Lines : string[]; Version : int }
    type ProjectRequest = { FileName : string;}
    type DeclarationsRequest = {FileName : string; Version : int}
    type HelptextRequest = {Symbol : string}
    type CompletionRequest = {FileName : string; SourceLine : string; Line : int; Column : int; Filter : string; IncludeKeywords : bool;}
    type PositionRequest = {FileName : string; Line : int; Column : int; Filter : string}
    type LintRequest = {FileName : string}

[<AutoOpen>]
module internal Utils =
    let private fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

    [<Literal>]
    let RequestIdName = "requestId"
    let getRequestIdFromReq (req : HttpRequest) = req.Item RequestIdName

let fsacToSuaveLogLevel = function
    | Utils.Logging.Debug -> Suave.Logging.Info // elevate FSAC Debug events to Suave Info level
    | Utils.Logging.Info -> Suave.Logging.Info
    | Utils.Logging.Warn -> Suave.Logging.Warn
    | Utils.Logging.Error -> Suave.Logging.Error

/// Forwards FSAC log messages to the Suave logger. The advantage is that we can reuse the Suave
/// Logging (i.e. Logary.Facade) which can be pretty-printed or output to more serious log targets
/// like file loggers and log servers, etc.
type FsacToSuaveLogger (suaveLogger: Suave.Logging.Logger, fields: Utils.Logging.Value list) =
    let requestIdTemplatePrefix = "[{" + RequestIdName + "}] "
    interface FsAutoComplete.Utils.Logging.ILogger with
        member this.Log level template args =
            let suaveLogLevel = fsacToSuaveLogLevel level
            suaveLogger.log suaveLogLevel (fun _ ->
                let mutable suaveEvent = Suave.Logging.Message.event suaveLogLevel (requestIdTemplatePrefix + template)
                let addNameValueToSuave name value = suaveEvent <- suaveEvent |> Suave.Logging.Message.setField name value
                let addExnToSuave exn = suaveEvent <- suaveEvent |> Suave.Logging.Message.addExn exn
                let addValueToSuaveEvent = function
                    | Logging.Field (name, value) -> addNameValueToSuave name (string value)
                    | Logging.Exception exn -> addExnToSuave exn
                // Note: appending args *after* fields is important; any template fields (args) will overwrite
                // the 'ambiant' logger fields.
                fields |> Seq.append args |> Seq.iter addValueToSuaveEvent
                suaveEvent
            )
            // Immediately ask Suave to queue the log event
            |> Async.RunSynchronously

        member this.With field = FsacToSuaveLogger(suaveLogger, List.Cons (field, fields)) :> Utils.Logging.ILogger

[<EntryPoint>]
let main argv =
    let mutable client : WebSocket option  = None

    let coreCommandsSuaveLogger = Suave.Logging.Log.create "FSAC.Core.Commands"
    // TODO: figure out how to flow the RequestId through. It looks like this wont work
    let coreComamndsLogger = FsacToSuaveLogger (coreCommandsSuaveLogger, [ Utils.Logging.Field(RequestIdName, "TODO") ])
    let commands = Commands(writeJson, coreComamndsLogger)

    System.Threading.ThreadPool.SetMinThreads(8, 8) |> ignore
    let originalFs = AbstractIL.Internal.Library.Shim.FileSystem
    let fs = new FileSystem(originalFs, commands.Files.TryFind)
    AbstractIL.Internal.Library.Shim.FileSystem <- fs

    // commands.FileChecked
    // |> Event.add (fun response ->
    //     client |> Option.iter (fun socket ->
    //         async {
    //             let! res = response

    //             let cnt = res |> List.toArray |> Json.toJson
    //             return! socket.send Text cnt true
    //         } |> Async.Ignore |> Async.Start ))

    let handler f : WebPart = fun (r : HttpContext) -> async {
          let data = r.request |> getResourceFromReq
          let reqId = r.request |> getRequestIdFromReq
          let! res = Async.Catch (f reqId data)
          match res with
          | Choice1Of2 res ->
             let res' = res |> List.toArray |> Json.toJson
             return! Response.response HttpCode.HTTP_200 res' r
          | Choice2Of2 e ->
            coreCommandsSuaveLogger.errorWithBP
                (Suave.Logging.Message.eventX "The handler of request at {requestUrlPathAndQuery} crashed"
                >> Suave.Logging.Message.setField "requestUrlPathAndQuery" r.request.url.PathAndQuery
                >> Suave.Logging.Message.addExn e
                >> Suave.Logging.Message.setField "SuaveRequest" r.request)
            |> Async.RunSynchronously

            return! Response.response HttpCode.HTTP_500 (Json.toJson e) r
        }

    let positionHandler (f : PositionRequest -> ParseAndCheckResults -> string -> string [] -> Async<string list>) : WebPart = fun (r : HttpContext) ->
        async {
            let data = r.request |> getResourceFromReq<PositionRequest>
            let file = Path.GetFullPath data.FileName
            let! res =
                match commands.TryGetFileCheckerOptionsWithLinesAndLineStr(file, { Line = data.Line; Col = data.Column }) with
                | Failure s -> async.Return ([CommandResponse.error writeJson s])
                | Success (options, lines, lineStr) ->
                  // TODO: Should sometimes pass options.Source in here to force a reparse
                  //       for completions e.g. `(some typed expr).$`
                  try
                    let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                    match tyResOpt with
                    | None -> async.Return [CommandResponse.info writeJson "Cached typecheck results not yet available"]
                    | Some tyRes ->
                        async {
                            let! r = Async.Catch (f data tyRes lineStr lines)
                            match r with
                            | Choice1Of2 r -> return r
                            | Choice2Of2 e -> return [CommandResponse.error writeJson e.Message]
                        }
                  with e -> async.Return [CommandResponse.error writeJson e.Message]
            let res' = res |> List.toArray |> Json.toJson
            return! Response.response HttpCode.HTTP_200 res' r
        }

    let echo (webSocket : WebSocket) =
        fun cx ->
            client <- Some webSocket
            socket {
                let loop = ref true
                while !loop do
                    let! msg = webSocket.read()
                    match msg with
                    | (Ping, _, _) -> do! webSocket.send Pong (Sockets.ByteSegment [||]) true
                    | (Close, _, _) ->
                        do! webSocket.send Close (Sockets.ByteSegment [||]) true
                        client <- None
                        loop := false
                    | _ -> ()
                }

    let fsacRequestLogger (ctx: HttpContext) =
        let fieldMap : Map<string, obj> =
            [
                "requestMethod", box (ctx.request.``method``)
                "requestUrlPathAndQuery", box (ctx.request.url.PathAndQuery)
                "httpStatusCode", box ctx.response.status.code
                "requestForm", box ctx.request.form
            ] |> Map
        "{requestMethod} {requestUrlPathAndQuery} {requestForm}", fieldMap

    let app =
        logWithLevelStructured Suave.Logging.Info coreCommandsSuaveLogger fsacRequestLogger
        >=>
        choose [
            // path "/notify" >=> handShake echo
            path "/parse" >=> handler (fun reqId (data : ParseRequest) -> commands.Parse data.FileName data.Lines data.Version)
            path "/parseProjects" >=> handler (fun reqId (data : ProjectRequest) ->
                failwith "hah testing"
                commands.ParseProjectsForFile data.FileName)
            //TODO: Add filewatcher
            path "/parseProjectsInBackground" >=> handler (fun reqId (data : ProjectRequest) -> commands.ParseAndCheckProjectsInBackgroundForFile data.FileName)
            path "/project" >=> handler (fun reqId (data : ProjectRequest) -> commands.Project data.FileName false ignore)
            path "/declarations" >=> handler (fun reqId (data : DeclarationsRequest) -> commands.Declarations data.FileName (Some data.Version) )
            path "/declarationsProjects" >=> fun httpCtx ->
                async {
                    let! errors = commands.DeclarationsInProjects ()
                    let res = errors |> List.toArray |> Json.toJson
                    return! Response.response HttpCode.HTTP_200 res httpCtx
                }
            path "/helptext" >=> handler (fun reqId (data : HelptextRequest) -> commands.Helptext data.Symbol |> async.Return)
            path "/completion" >=> handler (fun reqId (data : CompletionRequest) -> async {
                let file = Path.GetFullPath data.FileName
                match commands.TryGetFileCheckerOptionsWithLines file with
                | Failure s -> return [CommandResponse.error writeJson s]
                | Success (options, lines) ->
                    let line = data.Line
                    let col = data.Column
                    let lineStr = data.SourceLine
                    let ok = line <= lines.Length && line >= 1 && col <= lineStr.Length + 1 && col >= 1
                    if not ok then
                        return [CommandResponse.error writeJson "Position is out of range"]
                    else
                        let tyResOpt = commands.TryGetRecentTypeCheckResultsForFile(file, options)
                        match tyResOpt with
                        | None -> return [ CommandResponse.info writeJson "Cached typecheck results not yet available"]
                        | Some tyRes -> return! commands.Completion tyRes { Line = data.Line; Col = data.Column } lineStr (Some data.Filter) data.IncludeKeywords
                })
            path "/tooltip" >=> positionHandler (fun data tyRes lineStr _ -> commands.ToolTip tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/signature" >=> positionHandler (fun data tyRes lineStr _ -> commands.Typesig tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/symboluseproject" >=> positionHandler (fun data tyRes lineStr _ -> commands.SymbolUseProject tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/symboluse" >=> positionHandler (fun data tyRes lineStr _ -> commands.SymbolUse tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/finddeclaration" >=> positionHandler (fun data tyRes lineStr _ -> commands.FindDeclarations tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/methods" >=> positionHandler (fun data tyRes _ lines   -> commands.Methods tyRes { Line = data.Line; Col = data.Column } lines)
            path "/help" >=> positionHandler (fun data tyRes line _   -> commands.Help tyRes { Line = data.Line; Col = data.Column } line)
            path "/compilerlocation" >=> fun httpCtx ->
                async {
                    let res = commands.CompilerLocation() |> List.toArray |> Json.toJson
                    return! Response.response HttpCode.HTTP_200 res httpCtx
                }
            path "/lint" >=> handler (fun reqId (data: LintRequest) -> commands.Lint data.FileName)
            path "/namespaces" >=> positionHandler (fun data tyRes lineStr _   -> commands.GetNamespaceSuggestions tyRes { Line = data.Line; Col = data.Column } lineStr)
            path "/unionCaseGenerator" >=> positionHandler (fun data tyRes lineStr lines   -> commands.GetUnionPatternMatchCases tyRes { Line = data.Line; Col = data.Column } lines lineStr)
        ] >=> logWithLevelStructured Info coreCommandsSuaveLogger logFormatStructured


    let port =
        try
            int argv.[0]
        with
        _ -> 8088

    let defaultBinding = defaultConfig.bindings.[0]
    let withPort = { defaultBinding.socketBinding with port = uint16 port }
    let serverConfig =
        { defaultConfig with bindings = [{ defaultBinding with socketBinding = withPort }]
                             // Note we use a different logger for suave itself
                             logger = Logging.Targets.create Info [|"Suave"|] }

    startWebServer serverConfig app
    0
