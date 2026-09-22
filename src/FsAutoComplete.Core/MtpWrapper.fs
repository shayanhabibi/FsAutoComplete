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

  /// Discovers the tests of every given application.
  let discoverTestsAsync
    (notify: TestDiscoveryUpdate -> unit)
    (applications: TestApplication list)
    : Async<DiscoveredNode list> =
    async {
      let! perApplication = applications |> List.map (discoverFromAsync notify) |> Async.Sequential

      return perApplication |> List.concat
    }
