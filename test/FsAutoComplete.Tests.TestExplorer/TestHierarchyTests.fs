module TestHierarchyTests

open Expecto
open FsAutoComplete.TestServer

let private leaf fullName : TestItem =
  { Id = ""
    ParentId = None
    IsLeaf = true
    FullName = fullName
    DisplayName = fullName
    ExecutorUri = "executor://xunit/VsTestRunner2/netcoreapp"
    ProjectFilePath = "/repo/Tests.fsproj"
    TargetFramework = "net8.0"
    CodeFilePath = None
    CodeLocationRange = None }

let private byFullName (items: TestItem list) = items |> List.map (fun i -> i.FullName) |> List.sort

[<Tests>]
let tests =
  testList
    "TestHierarchy"
    [ testCase "a leaf with an unsegmented name has no parent"
      <| fun _ ->
        let actual = TestHierarchy.withInferredGroupings [ leaf "MyTest" ]

        Expect.hasLength actual 1 "the leaf is the whole tree"
        Expect.isNone actual.Head.ParentId "an unsegmented name is rooted"
        Expect.isTrue actual.Head.IsLeaf "the node is runnable"

      testCase "leaves sharing a prefix gain one grouping node"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.First"; leaf "Tests.Second" ]

        Expect.equal (byFullName actual) [ "Tests"; "Tests.First"; "Tests.Second" ] "the shared prefix becomes a node"

      testCase "a grouping node is not runnable"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.First"; leaf "Tests.Second" ]

        let grouping = actual |> List.find (fun i -> i.FullName = "Tests")

        Expect.isFalse grouping.IsLeaf "a grouping node has no test to run"

      testCase "a leaf points at its grouping node"
      <| fun _ ->
        let actual =
          TestHierarchy.withInferredGroupings [ leaf "Tests.First"; leaf "Tests.Second" ]

        let grouping = actual |> List.find (fun i -> i.FullName = "Tests")
        let first = actual |> List.find (fun i -> i.FullName = "Tests.First")

        Expect.equal first.ParentId (Some grouping.Id) "the leaf's parent is the grouping node"

      testCase "nested type separators nest"
      <| fun _ ->
        let actual = TestHierarchy.withInferredGroupings [ leaf "Tests+Nested.Test 1" ]

        Expect.equal
          (byFullName actual)
          [ "Tests"; "Tests+Nested"; "Tests+Nested.Test 1" ]
          "'+' separates a nested type from its parent"

      testCase "identical names in different projects stay distinct"
      <| fun _ ->
        let other =
          { leaf "Tests.First" with
              ProjectFilePath = "/repo/Other.fsproj" }

        let actual = TestHierarchy.withInferredGroupings [ leaf "Tests.First"; other ]
        let ids = actual |> List.map _.Id |> List.distinct

        Expect.hasLength ids 4 "two leaves and two groupings, none shared across projects" ]
