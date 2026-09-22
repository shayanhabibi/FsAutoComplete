module MtpMappingTests

open Expecto
open System.Collections.Generic
open FsAutoComplete.TestServer
open Partas.TestingPlatform.Client

let private project = "/repo/Tests.fsproj"
let private framework = "net8.0"

let private node uid : TestNodeUpdate =
  { Uid = uid
    DisplayName = None
    NodeType = Some NodeType.Action
    ExecutionState = None
    ParentUid = None
    Location = None
    Duration = None
    Error = None
    StandardOutput = None
    StandardError = None
    Raw = Dictionary<string, obj>() }

let private map (n: TestNodeUpdate) = TestItem.ofMtpNode project framework n

[<Tests>]
let mtpNodeTests =
  testList
    "TestItem.ofMtpNode"
    [ testCase "the uid identifies the node within its project and framework"
      <| fun _ ->
        let actual = map (node "a3f9")

        Expect.equal actual.Id (TestItem.idOf project framework "a3f9") "the id is the uid scoped to the project"

      testCase "the uid addresses the test to the platform"
      <| fun _ ->
        let actual = map (node "a3f9")

        Expect.equal actual.PlatformUid (Some "a3f9") "the platform is asked to run the test by uid"

      testCase "the display name is the full name"
      <| fun _ ->
        let actual =
          map
            { node "a3f9" with
                DisplayName = Some "Tests.Adds(a: 1)" }

        Expect.equal actual.FullName "Tests.Adds(a: 1)" "the name the server displays names the test"

      testCase "the full name falls back to the uid"
      <| fun _ ->
        let actual = map (node "a3f9")

        Expect.equal actual.FullName "a3f9" "the uid names the node when the server sends no display name"

      testCase "a parent uid is scoped the same way as the node's own id"
      <| fun _ ->
        let actual =
          map
            { node "a3f9" with
                ParentUid = Some "b1c2" }

        Expect.equal
          actual.ParentId
          (Some(TestItem.idOf project framework "b1c2"))
          "the parent id matches the parent's id"

      testCase "a node without a parent sits at the root"
      <| fun _ ->
        let actual = map (node "Tests")

        Expect.isNone actual.ParentId "a root node has no parent"

      testCase "an empty parent uid is no parent"
      <| fun _ ->
        let actual = map { node "a3f9" with ParentUid = Some "" }

        Expect.isNone actual.ParentId "an empty uid names no node"

      testCase "an action node is runnable"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                NodeType = Some NodeType.Action }

        Expect.isTrue actual.IsLeaf "an action is a test"

      testCase "a group node is not runnable"
      <| fun _ ->
        let actual =
          map
            { node "Tests" with
                NodeType = Some NodeType.Group }

        Expect.isFalse actual.IsLeaf "a group holds tests"

      testCase "a node of unreported type is runnable"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                NodeType = None }

        Expect.isTrue actual.IsLeaf "only a group is excluded from running"

      testCase "the display name falls back to the uid"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                DisplayName = None }

        Expect.equal actual.DisplayName "Tests.Adds" "the uid names the node when the server sends no display name"

      testCase "a reported display name is kept"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                DisplayName = Some "Adds two numbers" }

        Expect.equal actual.DisplayName "Adds two numbers" "the server names the node"

      testCase "a location becomes a code file and range"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                Location =
                  Some
                    { File = "/repo/Tests.fs"
                      LineStart = Some 12
                      LineEnd = Some 20 } }

        Expect.equal actual.CodeFilePath (Some "/repo/Tests.fs") "the file is carried over"

        Expect.equal
          actual.CodeLocationRange
          (Some { StartLine = 12; EndLine = 20 })
          "the reported lines bound the test"

      testCase "a location without an end line covers one line"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                Location =
                  Some
                    { File = "/repo/Tests.fs"
                      LineStart = Some 12
                      LineEnd = None } }

        Expect.equal actual.CodeLocationRange (Some { StartLine = 12; EndLine = 12 }) "the start line bounds both ends"

      testCase "a location without lines gives a file and no range"
      <| fun _ ->
        let actual =
          map
            { node "Tests.Adds" with
                Location =
                  Some
                    { File = "/repo/Tests.fs"
                      LineStart = None
                      LineEnd = None } }

        Expect.equal actual.CodeFilePath (Some "/repo/Tests.fs") "the file is still known"
        Expect.isNone actual.CodeLocationRange "no line was reported"

      testCase "a node without a location has neither file nor range"
      <| fun _ ->
        let actual = map (node "Tests.Adds")

        Expect.isNone actual.CodeFilePath "no file was reported"
        Expect.isNone actual.CodeLocationRange "no range was reported"

      testCase "the node carries the project it was discovered in"
      <| fun _ ->
        let actual = map (node "Tests.Adds")

        Expect.equal actual.ProjectFilePath project "the project is carried over"
        Expect.equal actual.TargetFramework framework "the framework is carried over"

      testCase "the executor uri names no VSTest adapter"
      <| fun _ ->
        let actual = map (node "Tests.Adds")

        Expect.isNone
          (TestFrameworkId.tryOfExecutorUri actual.ExecutorUri)
          "name breakdown by adapter does not apply to a platform that reports its own tree" ]

[<Tests>]
let mtpHierarchyTests =
  let leaf id fullName parentId : TestItem =
    { Id = id
      ParentId = parentId
      IsLeaf = true
      FullName = fullName
      DisplayName = fullName
      ExecutorUri = TestItem.mtpExecutorUri
      ProjectFilePath = project
      TargetFramework = framework
      PlatformUid = Some id
      CodeFilePath = None
      CodeLocationRange = None }

  testList
    "TestHierarchy.withHierarchy"
    [ testCase "a tree the server linked itself is kept as reported"
      <| fun _ ->
        let group =
          { leaf "b1c2" "Tests" None with
              IsLeaf = false
              PlatformUid = Some "b1c2" }

        let child = leaf "a3f9" "Tests.Adds" (Some "b1c2")

        let actual = TestHierarchy.withHierarchy [ group; child ]

        Expect.equal actual [ group; child ] "the server already linked the nodes"

      testCase "a flat tree is grouped by the segments of its names"
      <| fun _ ->
        let actual = TestHierarchy.withHierarchy [ leaf "a3f9" "Tests.Adds" None ]

        let names = actual |> List.map _.FullName |> List.sort
        Expect.equal names [ "Tests"; "Tests.Adds" ] "the name segment becomes a group"

      testCase "a grouped leaf keeps the uid that addresses it"
      <| fun _ ->
        let actual =
          TestHierarchy.withHierarchy [ leaf "a3f9" "Tests.Adds" None ]
          |> List.find _.IsLeaf

        Expect.equal actual.Id "a3f9" "the id the server gave the node survives grouping"
        Expect.equal actual.PlatformUid (Some "a3f9") "the test can still be run"

      testCase "a synthesised group addresses no test"
      <| fun _ ->
        let actual =
          TestHierarchy.withHierarchy [ leaf "a3f9" "Tests.Adds" None ]
          |> List.find (_.IsLeaf >> not)

        Expect.isNone actual.PlatformUid "a group is not a test the platform can run"

      testCase "a grouped leaf is linked to its group"
      <| fun _ ->
        let actual = TestHierarchy.withHierarchy [ leaf "a3f9" "Tests.Adds" None ]
        let group = actual |> List.find (_.IsLeaf >> not)
        let child = actual |> List.find _.IsLeaf

        Expect.equal child.ParentId (Some group.Id) "the leaf hangs off the group"

      testCase "each project is linked on its own"
      <| fun _ ->
        let linkedProject =
          [ { leaf "b1c2" "Linked" None with
                IsLeaf = false
                ProjectFilePath = "/repo/Linked.fsproj" }
            { leaf "a3f9" "Linked.Adds" (Some "b1c2") with
                ProjectFilePath = "/repo/Linked.fsproj" } ]

        let flatProject =
          [ { leaf "c7d8" "Flat.Adds" None with
                ProjectFilePath = "/repo/Flat.fsproj" } ]

        let actual = TestHierarchy.withHierarchy (linkedProject @ flatProject)

        Expect.equal
          (actual |> List.filter (fun node -> node.ProjectFilePath = "/repo/Linked.fsproj"))
          linkedProject
          "the linked project is kept as reported"

        Expect.equal
          (actual
           |> List.filter (fun node -> node.ProjectFilePath = "/repo/Flat.fsproj")
           |> List.map _.FullName
           |> List.sort)
          [ "Flat"; "Flat.Adds" ]
          "the flat project is grouped by its names" ]
