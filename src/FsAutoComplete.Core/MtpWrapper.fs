namespace FsAutoComplete.TestServer

open System
open System.Collections.Generic
open System.Threading.Tasks
open FsAutoComplete.TestingPlatform.Client

/// Drives Microsoft.Testing.Platform applications over the server-mode protocol. Each application
/// is its own test runner, so there is no separate runner to locate.
module MtpWrapper =
  /// Path to a dll or executable that speaks the server-mode protocol.
  type TestApplication = string

  /// A node of a test tree, paired with the application that reported it.
  type DiscoveredNode = TestApplication * TestNodeUpdate

  type TestDiscoveryUpdate =
    | Progress of DiscoveredNode list
    | LogMessage of ClientLogLevel * string

  let private clientOptions =
    { MtpClientOptions.Default with
        ClientName = "FsAutoComplete" }

  /// Collects every node an application reports, notifying as the batches arrive.
  let private discoverFromAsync (notify: TestDiscoveryUpdate -> unit) (application: TestApplication) =
    async {
      let discovered = ResizeArray<DiscoveredNode>()

      let! client = MtpClient.LaunchAsync(application, clientOptions) |> Async.AwaitTask
      use client = client

      use _ =
        client.TestNodesUpdated.Subscribe(fun batch ->
          let nodes = batch.Updates |> List.map (fun node -> application, node)
          discovered.AddRange nodes
          notify (Progress nodes))

      use _ =
        client.LogReceived.Subscribe(fun log -> notify (LogMessage(log.Level, log.Message)))

      let! _capabilities = client.InitializeAsync() |> Async.AwaitTask
      do! client.DiscoverTestsAsync() |> Async.AwaitTask
      do! client.ExitAsync() |> Async.AwaitTask

      return List.ofSeq discovered
    }

  /// A node reported while running, paired with the application that reported it. A test is
  /// reported more than once: once as it starts, and again with its outcome.
  type RunNode = TestApplication * TestNodeUpdate

  type TestRunUpdate =
    | Progress of RunNode list
    | LogMessage of ClientLogLevel * string

  [<RequireQualifiedAccess>]
  type TestSelection =
    | All
    | Uids of string list

  /// Each application runs either all tests or an explicit set of discovered test uids.
  type RunRequest = TestApplication * TestSelection

  type ProcessId = int
  type DidDebuggerAttach = bool

  /// Answers the server's request to have a debugger attached. Unlike VSTest, where the client
  /// launches the test host itself to get ahead of it, the platform runs its own host and asks
  /// the client to attach to it; the run continues once this answers.
  let attachDebuggerHandler (onAttachDebugger: ProcessId -> DidDebuggerAttach) : ServerRequestHandler =
    fun name parameters _cancellationToken ->
      if name <> "client/attachDebugger" then
        Task.FromResult None
      else
        let processId =
          parameters
          |> Option.bind (fun p ->
            match p.TryGetValue "processId" with
            | true, processId -> Some(Convert.ToInt32 processId)
            | _ -> None)

        let attached = processId |> Option.map onAttachDebugger |> Option.defaultValue false

        Dictionary<string, obj>(dict [ "attached", box attached ]) :> IReadOnlyDictionary<string, obj>
        |> Some
        |> Task.FromResult

  /// Runs the requested tests of one application, notifying as the outcomes arrive.
  let private runOnAsync
    (notify: TestRunUpdate -> unit)
    (onAttachDebugger: (ProcessId -> DidDebuggerAttach) option)
    ((application, selection): RunRequest)
    =
    async {
      let reported = ResizeArray<RunNode>()

      let! client = MtpClient.LaunchAsync(application, clientOptions) |> Async.AwaitTask
      use client = client

      use _ =
        client.TestNodesUpdated.Subscribe(fun batch ->
          let nodes = batch.Updates |> List.map (fun node -> application, node)
          reported.AddRange nodes
          notify (Progress nodes))

      use _ =
        client.LogReceived.Subscribe(fun log -> notify (LogMessage(log.Level, log.Message)))

      onAttachDebugger
      |> Option.iter (fun onAttachDebugger ->
        client.ServerRequestHandler <- Some(attachDebuggerHandler onAttachDebugger))

      let! _capabilities = client.InitializeAsync() |> Async.AwaitTask

      let! _result =
        match selection with
        | TestSelection.All -> client.RunTestsAsync()
        | TestSelection.Uids uids -> client.RunTestsAsync(uids)
        |> Async.AwaitTask

      do! client.ExitAsync() |> Async.AwaitTask

      return List.ofSeq reported
    }

  /// Runs the requested tests of every given application. A debugger is attached only where the
  /// application asks for one, which it does when the run was started under a debugger.
  let runTestsWithDebuggerAsync
    (notify: TestRunUpdate -> unit)
    (onAttachDebugger: (ProcessId -> DidDebuggerAttach) option)
    (requests: RunRequest list)
    : Async<RunNode list> =
    async {
      // The platform treats an empty uid collection as "run all". An explicit empty
      // selection must therefore never be sent to an application.
      let! perApplication =
        requests
        |> List.filter (fun (_, selection) -> selection <> TestSelection.Uids [])
        |> List.map (runOnAsync notify onAttachDebugger)
        |> Async.Sequential

      return perApplication |> List.concat
    }

  /// Runs the requested tests of every given application, undebugged.
  let runTestsAsync (notify: TestRunUpdate -> unit) (requests: RunRequest list) : Async<RunNode list> =
    runTestsWithDebuggerAsync notify None requests

  /// Discovers the tests of every given application.
  let discoverTestsAsync
    (notify: TestDiscoveryUpdate -> unit)
    (applications: TestApplication list)
    : Async<DiscoveredNode list> =
    async {
      let! perApplication = applications |> List.map (discoverFromAsync notify) |> Async.Sequential

      return perApplication |> List.concat
    }
