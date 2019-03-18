/* tslint:disable:object-literal-sort-keys */

export const Syntax = {
    CobolSourceProgram: 'CobolSourceProgram',
    CobolSourceProgramNested: 'CobolSourceProgramNested',
    ProgramId: 'ProgramId',
    ProgramIdNested: 'ProgramIdNested',

    // TODO: mögl. splitten
    IdentificationDivisionContent: 'IdentificationDivisionContent',
    EnvironmentDivisionContent: 'EnvironmentDivisionContent',
    ConfigurationSection: 'ConfigurationSection',

    // ConfigurationSectionParagraphs
    SourceComputerParagraph: 'SourceComputerParagraph',
    ObjectComputerParagraph: 'ObjectComputerParagraph',
    SpecialNamesParagraph: 'SpecialNamesParagraph',

    ObjectComputerParagraphSequencePhrase: 'ObjectComputerParagraphSequencePhrase',
    SpecialNamesParagraphStatusPhrase: 'SpecialNamesParagraphStatusPhrase',

    // SpecialNamesParagraphClauses
    AlphabetClause: 'AlphabetClause',
    SymbolicCharactersClause: 'SymbolicCharactersClause',
    ClassClause: 'ClassClause',
    CurrencySignClause: 'CurrencySignClause',
    DecimalPointClause: 'DecimalPointClause',

    FileControlParagraph: 'FileControlParagraph',
    FileControlEntry: 'FileControlEntry',
    SelectClause: 'SelectClause',
    AssignClause: 'AssignClause',
    ReserveClause: 'ReserveClause',
    PaddingCharacterClause: 'PaddingCharacterClause',
    RecordDelimiterClause: 'RecordDelimiterClause',
    RecordKeyClause: 'RecordKeyClause',
    AlternateRecordKeyClause: 'AlternateRecordKeyClause',
    RelativeKeyClause: 'RelativeKeyClause',
    PasswordClause: 'PasswordClause',
    FileStatusClause: 'FileStatusClause',

    IOControlParagraph: 'IOControlParagraph',
    QsamOrSamIOControlEntry: 'QsamOrSamIOControlEntry',
    VsamIOControlEntry: 'VsamIOControlEntry',
    SortMergeIOControlEntry: 'SortMergeIOControlEntry',

    // TODO: mögl. splitten
    DataDivisionContent: 'DataDivisionContent',
    DataItemDescriptionEntry: 'DataItemDescriptionEntry',
    RecordDescriptionEntry: 'RecordDescriptionEntry',
    FileAndSortDescriptionEntry: 'FileAndSortDescriptionEntry',

    // FileAndSortDescriptionEntryClauses
    ExternalClause: 'ExternalClause',
    GlobalClause: 'GlobalClause',
    BlockContainsClause: 'BlockContainsClause',
    RecordClause: 'RecordClause', RecordVaryingPhrase: 'RecordVaryingPhrase',
    LabelRecordsClause: 'LabelRecordsClause',
    ValueOfClause: 'ValueOfClause',
    DataRecordsClause: 'DataRecordsClause',
    LinageClause: 'LinageClause', LinageFootingPhrase: 'LinageFootingPhrase',
    RecordingModeClause: 'RecordingModeClause',
    CodeSetClause: 'CodeSetClause',

    DataDescriptionEntry: 'dataDescriptionEntry',

    // dataDescriptionEntryClauses
    RedefinesClause: 'redefinesClause',
    BlankWhenZeroClause: 'BlankWhenZeroClause',
    JustifiedClause: 'JustifiedClause',
    OccursClause: 'OccursClause',
    PictureClause: 'PictureClause',
    SignClause: 'SignClause',
    SyncronizedClause: 'SyncronizedClause',
    UsageClause: 'UsageClause',
    ConditionValueClause: 'ConditionValueClause',
    DataValueClause: 'DataValueClause',
    RenamesClause: 'RenamesClause',

    ProcedureDivision: 'ProcedureDivision',
    SectionHeader: 'SectionHeader',
    Paragraph: 'Paragraph',
    Sentence: 'Sentence',
    StatementList: 'StatementList',

    // Statement
    AcceptStatement: 'AcceptStatement',
    AddStatement: 'AddStatement',
    AlterStatement: 'AlterStatement',
    CallStatement: 'CallStatement',
    CancelStatement: 'CancelStatement',
    CloseStatement: 'CloseStatement',
    ComputeStatement: 'ComputeStatement',
    ContinueStatement: 'ContinueStatement',
    DeleteStatement: 'DeleteStatement',
    DisplayStatement: 'DisplayStatement',
    DivideStatement: 'DivideStatement',
    EntryStatement: 'EntryStatement',
    EvaluateStatement: 'EvaluateStatement', EvaluatePhrase: 'EvaluatePhrase',
    ExitStatement: 'ExitStatement',
    ExitProgramStatement: 'ExitProgramStatement',
    GoBackStatement: 'GoBackStatement',
    GoToStatement: 'GoToStatement',
    AlteredGoToStatement: 'AlteredGoToStatement',
    IfStatement: 'IfStatement',
    InitializeStatement: 'InitializeStatement',
    InspectStatement: 'InspectStatement', BeforeAfterPhrase: 'BeforeAfterPhrase',
    MergeStatement: 'MergeStatement',
    MoveStatement: 'MoveStatement',
    MultiplyStatement: 'MultiplyStatement',
    OpenStatement: 'OpenStatement',
    PerformStatement: 'PerformStatement',
    PerformUntilStatement: 'PerformUntilStatement',
    PerformVaryingStatement: 'PerformVaryingStatement',
    PerformAfterStatement: 'PerformAfterStatement',
    ReadStatement: 'ReadStatement',
    ReleaseStatement: 'ReleaseStatement',
    ReturnStatement: 'ReturnStatement',
    RewriteStatement: 'RewriteStatement',
    SearchStatement: 'SearchStatement',
    SetStatement: 'SetStatement',
    SortStatement: 'SortStatement',
    StartStatement: 'StartStatement',
    StopStatement: 'StopStatement',
    StringStatement: 'StringStatement',
    SubtractStatement: 'SubtractStatement',
    UnstringStatement: 'UnstringStatement',
    WriteStatement: 'WriteStatement', WriteStatementPhrase: 'WriteStatementPhrase',

    // CompilerDirectingStatement
    BasisStatement: 'BasisStatement',
    CblProcessStatement: 'CblProcessStatement',
    ControlCblStatement: 'ControlCblStatement',
    CopyStatement: 'CopyStatement',
    DeleteCompilerDirectingStatement: 'DeleteCompilerDirectingStatement',
    EjectStatement: 'EjectStatement',
    EnterStatement: 'EnterStatement',
    InsertStatement: 'InsertStatement',
    ReadyOrResetTraceStatement: 'ReadyOrResetTraceStatement',
    ReplaceStatement: 'ReplaceStatement',
    ServiceLabelStatement: 'ServiceLabelStatement',
    ServiceReloadStatement: 'ServiceReloadStatement',
    SkipStatement: 'SkipStatement',
    TitleStatement: 'TitleStatement',
    UseStatement: 'UseStatement',

    // Condition
    CombinableCondition: 'CombinableCondition',
    CombinedCondition: 'CombinedCondition',

    // SimpleCondition
    ClassCondition: 'ClassCondition',
    ConditionNameCondition: 'ConditionNameCondition',
    RelationCondition: 'RelationCondition', RelationalOperator: 'RelationalOperator',
    SignCondition: 'SignCondition',
    SwitchStatusCondition: 'SwitchStatusCondition',
    NegatedSimpleCondition: 'NegatedSimpleCondition',

    AbbreviatedCombinedRelationConditions: 'AbbreviatedCombinedRelationConditions',
    AbbreviationRest: 'AbbreviationRest',
    AbbreviationLeaf: 'AbbreviationLeaf',

    ProcedureName: 'ProcedureName',
    Identifier: 'Identifier',
    QualifiedDataName: 'QualifiedDataName',
    Length: 'Length',
    LeftMostCharacterPosition: 'LeftMostCharacterPosition',
    ConditionNameReference: 'ConditionNameReference',
    Subscript: 'Subscript',
    ArithmeticExpression: 'ArithmeticExpression',
    TimesDiv: 'TimesDiv',
    Power: 'Power',
    Basis: 'Basis',
    CopyOperand: 'CopyOperand',
    Mode: 'Mode',

    // Name
    AlphabetName: 'AlphabetName',
    ClassName: 'ClassName',
    ConditionName: 'DataName',
    FileName: 'FileName',
    IndexName: 'IndexName',
    MnemonicName: 'MnemonicName',
    RecordName: 'RecordName',
    RoutineName: 'RoutineName',
    SymbolicCharacter: 'SymbolicCharacter',
    LibraryName: 'LibraryName',
    ProgramName: 'ProgramName',
    TextName: 'TextName',
    ParagraphName: 'ParagraphName',
    SectionName: 'SectionName',
    ComputerName: 'ComputerName',
    LanguageName: 'LanguageName',
    EnvironmentName: 'EnvironmentName',
    AssignmentName: 'AssignmentName',
    BasisName: 'BasisName',

    FigurativeConstant: 'FigurativeConstant',
    Literal: 'Literal',
    SpecialRegister: 'SpecialRegister'

















}