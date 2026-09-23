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
    /// Addresses a runnable test to Microsoft.Testing.Platform, which identifies tests by opaque
    /// uid. A grouping node and a test run under VSTest carry `None`.
    PlatformUid: string option
    ProjectFilePath: string
    TargetFramework: string
    CodeFilePath: string option
    CodeLocationRange: TestFileRange option
  }

[<RequireQualifiedAccess>]
type TestFrameworkId =
  | NUnit
  | MsTest
  | XUnit
  | Expecto

module TestFrameworkId =
  let tryOfExecutorUri (executorUri: string) =
    let startsWith (prefix: string) = executorUri.StartsWith(prefix, StringComparison.Ordinal)

    if startsWith "executor://nunit" then
      Some TestFrameworkId.NUnit
    elif startsWith "executor://mstest" then
      Some TestFrameworkId.MsTest
    elif startsWith "executor://xunit" then
      Some TestFrameworkId.XUnit
    elif startsWith "executor://yolodev" then
      Some TestFrameworkId.Expecto
    else
      None

module TestItem =
  /// Unique within a server session: a name repeated across projects or target frameworks
  /// yields a different id per project and framework.
  let idOf (projFilePath: string) (targetFramework: string) (fullName: string) =
    $"{projFilePath}|{targetFramework}|{fullName}"

  /// The name that identifies a single test case. xUnit and MSTest report every case of a
  /// parameterised test under one fully-qualified name and vary only the display name, so the
  /// case data is appended to keep the cases apart.
  let fullNameWithParameterisedCases (executorUri: string) (fullName: string) (displayName: string) =
    match TestFrameworkId.tryOfExecutorUri executorUri with
    | Some TestFrameworkId.MsTest ->
      if fullName.EndsWith(displayName, StringComparison.Ordinal) then
        fullName
      else
        $"{fullName}.{displayName}"
    | Some TestFrameworkId.XUnit ->
      // xUnit repeats the fully-qualified name inside the display name and appends the case
      // parameters to it rather than nesting them.
      if displayName <> fullName then
        let caseFragment = displayName.Split('.') |> Array.last
        $"{fullName}.{caseFragment}"
      else
        fullName
    | _ -> fullName

  let ofVsTestCase
    (projFilePath: string)
    (targetFramework: string)
    (testCase: Microsoft.VisualStudio.TestPlatform.ObjectModel.TestCase)
    : TestItem =
    let fullName =
      fullNameWithParameterisedCases (string testCase.ExecutorUri) testCase.FullyQualifiedName testCase.DisplayName

    { Id = idOf projFilePath targetFramework fullName
      ParentId = None
      IsLeaf = true
      FullName = fullName
      DisplayName = testCase.DisplayName
      ExecutorUri = testCase.ExecutorUri |> string
      PlatformUid = None
      ProjectFilePath = projFilePath
      TargetFramework = targetFramework
      CodeFilePath = Some testCase.CodeFilePath
      CodeLocationRange =
        Some
          { StartLine = testCase.LineNumber
            EndLine = testCase.LineNumber } }

  /// Stands in for the VSTest adapter uri on a platform that reports its own tree, where no
  /// adapter-specific name breakdown applies.
  [<Literal>]
  let mtpExecutorUri = "microsoft.testing.platform"

  /// Maps a node of a Microsoft.Testing.Platform test tree onto the shape the clients consume.
  /// The platform reports parent links and grouping nodes itself, so the result needs no pass
  /// through `TestHierarchy.withInferredGroupings`.
  let ofMtpNode
    (projFilePath: string)
    (targetFramework: string)
    (node: FsAutoComplete.TestingPlatform.Client.TestNodeUpdate)
    : TestItem =
    let idOfUid = idOf projFilePath targetFramework

    let range (location: FsAutoComplete.TestingPlatform.Client.SourceLocation) =
      location.LineStart
      |> Option.map (fun startLine ->
        { StartLine = startLine
          EndLine = location.LineEnd |> Option.defaultValue startLine })

    let isLeaf = node.NodeType <> Some FsAutoComplete.TestingPlatform.Client.NodeType.Group

    { Id = idOfUid node.Uid
      ParentId =
        node.ParentUid
        |> Option.filter (String.IsNullOrEmpty >> not)
        |> Option.map idOfUid
      IsLeaf = isLeaf
      FullName = node.DisplayName |> Option.defaultValue node.Uid
      DisplayName = node.DisplayName |> Option.defaultValue node.Uid
      ExecutorUri = mtpExecutorUri
      PlatformUid = (if isLeaf then Some node.Uid else None)
      ProjectFilePath = projFilePath
      TargetFramework = targetFramework
      CodeFilePath = node.Location |> Option.map _.File
      CodeLocationRange = node.Location |> Option.bind range }

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
        PlatformUid = None
        CodeFilePath = None
        CodeLocationRange = None }

  /// Returns the given tests plus a grouping node per name segment they share, each node
  /// linked to its parent. A leaf without an id is given one derived from its name, and keeps
  /// its own identity where a grouping name collides with it.
  let withInferredGroupings (tests: TestItem list) : TestItem list =
    let parentOf (item: TestItem) =
      ancestorNames item.FullName
      |> List.tryLast
      |> Option.map (TestItem.idOf item.ProjectFilePath item.TargetFramework)

    let leaves =
      tests
      |> List.map (fun leaf ->
        if String.IsNullOrEmpty leaf.Id then
          { leaf with
              Id = TestItem.idOf leaf.ProjectFilePath leaf.TargetFramework leaf.FullName }
        else
          leaf)

    let groupings =
      leaves
      |> List.collect (fun leaf -> ancestorNames leaf.FullName |> List.map (groupingNode leaf))

    let leafIds = leaves |> List.map _.Id |> Set.ofList

    leaves @ groupings
    |> List.filter (fun node -> node.IsLeaf || not (leafIds.Contains node.Id))
    |> List.distinctBy _.Id
    |> List.map (fun node -> { node with ParentId = parentOf node })

  /// Returns the nodes as a linked tree, one project at a time. A project whose platform reports
  /// its own parent links is taken at its word; a project reported without any is grouped by the
  /// segments of its test names.
  let withHierarchy (nodes: TestItem list) : TestItem list =
    nodes
    |> List.groupBy (fun node -> node.ProjectFilePath, node.TargetFramework)
    |> List.collect (fun (_, projectNodes) ->
      if projectNodes |> List.exists (fun node -> node.ParentId.IsSome) then
        projectNodes
      else
        withInferredGroupings projectNodes)

[<RequireQualifiedAccess>]
type TestPlatformKind =
  | VSTest
  | Mtp

module TestProject =
  [<Literal>]
  let private isTestingPlatformApplication = "IsTestingPlatformApplication"

  /// MSBuild property names the workspace loader must retain for `classify` to read.
  let requiredCustomProperties = [ isTestingPlatformApplication ]

  let private hasVsTestPackages (project: Ionide.ProjInfo.Types.ProjectOptions) =
    let indicators = set [ "Microsoft.TestPlatform.TestHost"; "Microsoft.NET.Test.Sdk" ]

    project.PackageReferences
    |> List.exists (fun pr -> Set.contains pr.Name indicators)

  let private optsIntoTestingPlatform (project: Ionide.ProjInfo.Types.ProjectOptions) =
    project.CustomProperties
    |> List.exists (fun p ->
      p.Name = isTestingPlatformApplication
      && p.Value.Equals("true", StringComparison.OrdinalIgnoreCase))

  /// The platform that will run a project's tests, or `None` for a project carrying none.
  /// A project opts into Microsoft.Testing.Platform through an MSBuild property alone, so it
  /// need not reference the VSTest packages; every other test project runs under VSTest.
  let classify (project: Ionide.ProjInfo.Types.ProjectOptions) : TestPlatformKind option =
    if optsIntoTestingPlatform project then
      Some TestPlatformKind.Mtp
    elif hasVsTestPackages project then
      Some TestPlatformKind.VSTest
    else
      None

  /// The platform a project's tests will actually be run on. With Microsoft.Testing.Platform
  /// disabled, a project that opts into it falls back to VSTest where it references the VSTest
  /// packages, and is otherwise unreachable.
  let platformFor (mtpEnabled: bool) (project: Ionide.ProjInfo.Types.ProjectOptions) : TestPlatformKind option =
    match classify project with
    | Some TestPlatformKind.Mtp when not mtpEnabled ->
      if hasVsTestPackages project then
        Some TestPlatformKind.VSTest
      else
        None
    | platform -> platform

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

  /// Reads the verdict out of the state a Microsoft.Testing.Platform node reports. A state that
  /// records progress rather than a verdict, and one this client does not know, leave the test
  /// unjudged.
  let ofMtpExecutionState (state: FsAutoComplete.TestingPlatform.Client.ExecutionState option) =
    match state with
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Passed -> TestOutcome.Passed
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Skipped -> TestOutcome.Skipped
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Failed
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.Error
    | Some FsAutoComplete.TestingPlatform.Client.ExecutionState.TimedOut -> TestOutcome.Failed
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

  /// Reads the outcome of a run out of a Microsoft.Testing.Platform node. The platform reports a
  /// result as a further update to the node that was discovered, so the test it belongs to is the
  /// node itself.
  let ofMtpNode
    (projFilePath: string)
    (targetFramework: string)
    (node: FsAutoComplete.TestingPlatform.Client.TestNodeUpdate)
    : TestResult =
    let output =
      [ node.StandardOutput; node.StandardError ]
      |> List.choose id
      |> function
        | [] -> None
        | streams -> streams |> String.concat Environment.NewLine |> Some

    { Outcome = TestOutcome.ofMtpExecutionState node.ExecutionState
      ErrorMessage = node.Error |> Option.bind _.Message
      ErrorStackTrace = node.Error |> Option.bind _.StackTrace
      AdditionalOutput = output
      Duration = node.Duration |> Option.defaultValue TimeSpan.Zero
      TestItem = TestItem.ofMtpNode projFilePath targetFramework node }
