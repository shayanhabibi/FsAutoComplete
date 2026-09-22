namespace FsAutoComplete.TestServer

open Partas.TestingPlatform.Client

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

  /// The tests of an application to run, named by the uid the platform gave them. An empty list
  /// runs every test the application has.
  type RunRequest = TestApplication * string list

  /// Runs the requested tests of one application, notifying as the outcomes arrive.
  let private runOnAsync (notify: TestRunUpdate -> unit) ((application, uids): RunRequest) =
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

      let! _capabilities = client.InitializeAsync() |> Async.AwaitTask

      let! _result =
        match uids with
        | [] -> client.RunTestsAsync()
        | uids -> client.RunTestsAsync(uids)
        |> Async.AwaitTask

      do! client.ExitAsync() |> Async.AwaitTask

      return List.ofSeq reported
    }

  /// Runs the requested tests of every given application.
  let runTestsAsync (notify: TestRunUpdate -> unit) (requests: RunRequest list) : Async<RunNode list> =
    async {
      let! perApplication = requests |> List.map (runOnAsync notify) |> Async.Sequential

      return perApplication |> List.concat
    }

  /// Discovers the tests of every given application.
  let discoverTestsAsync
    (notify: TestDiscoveryUpdate -> unit)
    (applications: TestApplication list)
    : Async<DiscoveredNode list> =
    async {
      let! perApplication = applications |> List.map (discoverFromAsync notify) |> Async.Sequential

      return perApplication |> List.concat
    }
