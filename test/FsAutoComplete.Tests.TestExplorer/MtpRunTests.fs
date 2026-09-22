module MtpRunTests

open Expecto
open System.IO
open FsAutoComplete.TestServer
open Partas.TestingPlatform.Client

let private sampleApp =
  Path.Combine(ResourceLocators.sampleProjectsRootDir, "Mtp.XUnit/bin/Debug/net8.0/Mtp.XUnit.dll")

let private nameOf (node: TestNodeUpdate) = node.DisplayName |> Option.defaultValue node.Uid

let private runAll () = MtpWrapper.runTestsAsync ignore [ sampleApp, [] ]

let private byName name (results: MtpWrapper.RunNode list) =
  results |> List.find (fun (_, node) -> nameOf node = name) |> snd

[<Tests>]
let tests =
  testList
    "MtpWrapper Test Runs"
    [ testCaseAsync "runs nothing when no application is named"
      <| async {
        let! actual = MtpWrapper.runTestsAsync ignore []
        Expect.isEmpty actual "no application runs no tests"
      }

      testCaseAsync "reports a passing test as passed"
      <| async {
        let! results = runAll ()

        Expect.equal (byName "Tests.My test" results).ExecutionState (Some ExecutionState.Passed) "the test passed"
      }

      testCaseAsync "reports a failing test as failed"
      <| async {
        let! results = runAll ()

        Expect.equal (byName "Tests.Fails" results).ExecutionState (Some ExecutionState.Failed) "the test failed"
      }

      testCaseAsync "explains a failure"
      <| async {
        let! results = runAll ()
        let failed = byName "Tests.Fails" results

        Expect.isSome failed.Error "the server says why the test failed"
      }

      testCaseAsync "reports a skipped test as skipped"
      <| async {
        let! results = runAll ()

        Expect.equal
          (byName "Tests.Skipped" results).ExecutionState
          (Some ExecutionState.Skipped)
          "the test was skipped"
      }

      testCaseAsync "pairs every result with the application it came from"
      <| async {
        let! results = runAll ()

        Expect.all results (fun (source, _) -> source = sampleApp) "each result names its application"
      }

      testCaseAsync "reports progress as the server runs"
      <| async {
        let seen = ResizeArray()

        let! results =
          MtpWrapper.runTestsAsync
            (function
            | MtpWrapper.TestRunUpdate.Progress nodes -> seen.AddRange nodes
            | MtpWrapper.TestRunUpdate.LogMessage _ -> ())
            [ sampleApp, [] ]

        Expect.equal (List.ofSeq seen) results "progress reports the same nodes as the result"
      }

      testCaseAsync "runs only the tests it is asked to run"
      <| async {
        let! discovered = MtpWrapper.discoverTestsAsync ignore [ sampleApp ]

        let uid =
          discovered
          |> List.find (fun (_, node) -> nameOf node = "Tests.Fails")
          |> snd
          |> _.Uid

        let! results = MtpWrapper.runTestsAsync ignore [ sampleApp, [ uid ] ]

        let names =
          results
          |> List.filter (fun (_, node) -> node.ExecutionState <> Some ExecutionState.InProgress)
          |> List.map (snd >> nameOf)
          |> List.distinct

        Expect.equal names [ "Tests.Fails" ] "only the named test ran"
      } ]
