namespace FsAutoComplete.TestServer

open System

type TestFileRange = { StartLine: int; EndLine: int }

type TestItem =
  {
    /// Distinguishes this node from every other node reported by the server.
    Id: string
    /// The `Id` of the node one level up, or `None` at the root of a project.
    ParentId: string option
    /// A runnable test. `false` marks a grouping node.
    IsLeaf: bool
    FullName: string
    DisplayName: string
    /// Identifies the test adapter that ran the tests
    /// Example: executor://xunit/VsTestRunner2/netcoreapp
    /// Used for determining the test library, which effects how tests names are broken down
    ExecutorUri: string
    ProjectFilePath: string
    TargetFramework: string
    CodeFilePath: string option
    CodeLocationRange: TestFileRange option
  }

module TestItem =
  /// Unique within a server session: a name repeated across projects or target frameworks
  /// yields a different id per project and framework.
  let idOf (projFilePath: string) (targetFramework: string) (fullName: string) =
    $"{projFilePath}|{targetFramework}|{fullName}"

  let ofVsTestCase
    (projFilePath: string)
    (targetFramework: string)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem =
    { Id = idOf projFilePath targetFramework testCase.FullyQualifiedName
      ParentId = None
      IsLeaf = true
      FullName = testCase.FullyQualifiedName
      DisplayName = testCase.DisplayName
      ExecutorUri = testCase.ExecutorUri |> string
      ProjectFilePath = projFilePath
      TargetFramework = targetFramework
      CodeFilePath = Some testCase.CodeFilePath
      CodeLocationRange =
        Some
          { StartLine = testCase.LineNumber
            EndLine = testCase.LineNumber } }

  let tryTestCaseToDTO
    (projectLookup: string -> Ionide.ProjInfo.Types.ProjectOptions option)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem option =
    match projectLookup testCase.Source with
    | None -> None // this should never happen. We pass VsTest the list of executables to test, so all the possible sources should be known to us
    | Some project -> ofVsTestCase project.ProjectFileName project.TargetFramework testCase |> Some

/// Builds the test tree for platforms that report runnable tests alone.
module TestHierarchy =
  open System.Text.RegularExpressions

  type private Segment =
    { Text: string
      SeparatorBefore: string }

  let private segmentRegex = Regex(@"([+\.]?)([^+\.]+)", RegexOptions.Compiled)

  let private splitSegments (fullName: string) =
    [ for m in segmentRegex.Matches(fullName) ->
        { Text = m.Groups[2].Value
          SeparatorBefore = m.Groups[1].Value } ]

  /// The ancestor names of a fully-qualified test name, outermost first.
  let private ancestorNames (fullName: string) =
    splitSegments fullName
    |> List.scan (fun path segment -> $"{path}{segment.SeparatorBefore}{segment.Text}") ""
    |> List.filter (fun name -> name <> "" && name <> fullName)

  let private groupingNode (template: TestItem) (fullName: string) =
    { template with
        Id = TestItem.idOf template.ProjectFilePath template.TargetFramework fullName
        ParentId = None
        IsLeaf = false
        FullName = fullName
        DisplayName = (splitSegments fullName |> List.last).Text
        CodeFilePath = None
        CodeLocationRange = None }

  /// Returns the given tests plus a grouping node per name segment they share, each node
  /// linked to its parent. Ids are assigned here, so a caller may leave them unset. A leaf
  /// keeps its own identity where a grouping name collides with it.
  let withInferredGroupings (tests: TestItem list) : TestItem list =
    let parentOf (item: TestItem) =
      ancestorNames item.FullName
      |> List.tryLast
      |> Option.map (TestItem.idOf item.ProjectFilePath item.TargetFramework)

    let leaves =
      tests
      |> List.map (fun leaf ->
        { leaf with
            Id = TestItem.idOf leaf.ProjectFilePath leaf.TargetFramework leaf.FullName })

    let groupings =
      leaves
      |> List.collect (fun leaf -> ancestorNames leaf.FullName |> List.map (groupingNode leaf))

    let leafIds = leaves |> List.map _.Id |> Set.ofList

    leaves @ groupings
    |> List.filter (fun node -> node.IsLeaf || not (leafIds.Contains node.Id))
    |> List.distinctBy _.Id
    |> List.map (fun node -> { node with ParentId = parentOf node })

[<RequireQualifiedAccess>]
type TestOutcome =
  | Failed = 0
  | Passed = 1
  | Skipped = 2
  | None = 3
  | NotFound = 4

module TestOutcome =
  type VSTestOutcome = Microsoft.VisualStudio.TestPlatform.ObjectModel.TestOutcome

  let ofVSTestOutcome (vsTestOutcome: VSTestOutcome) =
    match vsTestOutcome with
    | VSTestOutcome.Passed -> TestOutcome.Passed
    | VSTestOutcome.Failed -> TestOutcome.Failed
    | VSTestOutcome.Skipped -> TestOutcome.Skipped
    | VSTestOutcome.NotFound -> TestOutcome.NotFound
    | VSTestOutcome.None -> TestOutcome.None
    | _ -> TestOutcome.None

type TestResult =
  { TestItem: TestItem
    Outcome: TestOutcome
    ErrorMessage: string option
    ErrorStackTrace: string option
    AdditionalOutput: string option
    Duration: TimeSpan }

module TestResult =
  type VSTestResult = Microsoft.VisualStudio.TestPlatform.ObjectModel.TestResult

  let ofVsTestResult (projFilePath: string) (targetFramework: string) (vsTestResult: VSTestResult) : TestResult =
    let stringToOption (text: string) = if String.IsNullOrEmpty(text) then None else Some text

    { Outcome = TestOutcome.ofVSTestOutcome vsTestResult.Outcome
      ErrorMessage = vsTestResult.ErrorMessage |> stringToOption
      ErrorStackTrace = vsTestResult.ErrorStackTrace |> stringToOption
      AdditionalOutput =
        match vsTestResult.Messages |> Seq.toList with
        | [] -> None
        | messages -> messages |> List.map _.Text |> String.concat Environment.NewLine |> Some
      Duration = vsTestResult.Duration
      TestItem = TestItem.ofVsTestCase projFilePath targetFramework vsTestResult.TestCase }
