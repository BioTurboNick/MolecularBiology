Imports System.Math
Imports System.Collections.ObjectModel
Imports Microsoft.SDK.Samples.VistaBridge.Library
Imports MolecularBiology2.General
Imports MolecularBiology2.Application

Friend NotInheritable Class Mathematics
    Friend Const Tau As Double = 2 * PI

    Friend Shared Function PolarToCartesian(ByVal Angle As Double, ByVal Radius As Double) As Point
        Dim Theta As Double = (Tau / 360) * (Angle - 90)

        Dim x As Double = Radius * Cos(Theta)
        Dim y As Double = Radius * Sin(Theta)

        Return New Point(x, y)
    End Function

    Friend Shared Function ComputeAngle(ByVal Length As Double, ByVal Radius As Double) As Double
        Return Length / (Tau * Radius) * 360
    End Function

    Friend Shared Function VectorToAngle(ByVal Location As Point) As Double
        Dim Theta As Double = Atan2(Location.Y, Location.X)
        Dim Angle As Double = Theta * 360 / Tau + 90

        If Angle < 0 Then Angle += 360

        Return Angle
    End Function

    Friend Shared Function FractionToAngle(ByVal Fraction As Double, Optional ByVal Normalize As Boolean = False) As Double
        Dim Angle As Double

        Angle = Fraction * 360

        If Normalize Then Angle = NormalizeAngle(Angle)

        Return Angle
    End Function

    Friend Shared Function NormalizeAngle(ByVal Angle As Double) As Double
        'Return values from 0 inclusive to 360 exclusive
        Angle -= 360 * Int(Angle / 360)

        Return Angle
    End Function

    Friend Shared Function AngleToRadians(ByVal Angle As Double) As Double
        Dim Radians As Double = Angle / 360 * Tau

        Return Radians
    End Function

    Friend Shared Function RadiansToAngle(ByVal Radians As Double) As Double
        Dim Angle As Double = Radians / Tau * 360

        Return Angle
    End Function

    Friend Shared Function FindMagnitude(ByVal Value As Double) As Double
        Dim Magnitude As Integer

        Select Case Value
            Case 0, 1
                Magnitude = 0
            Case Is >= 10
                Do While Value >= 10
                    Value /= 10
                    Magnitude += 1
                Loop
            Case Is < 1
                Do While Value < 1
                    Value *= 10
                    Magnitude -= 1
                Loop
        End Select

        Return Magnitude
    End Function

End Class

Friend NotInheritable Class General
    Friend Shared Sub Swap(ByRef Value1 As Integer, ByRef Value2 As Integer)
        Dim Value1Store As Integer = Value1
        Value1 = Value2
        Value2 = Value1Store
    End Sub

    Friend Shared Sub Swap(ByRef Value1 As Boolean, ByRef Value2 As Boolean)
        Dim Value1Store As Boolean = Value1
        Value1 = Value2
        Value2 = Value1Store
    End Sub
End Class

Friend Class SequenceTools
    Friend Shared Function GetComplement(ByVal Sequence As String, Optional ByVal GetReverse As Boolean = True) As String
        'Defaults to returning the reverse complement (5'-3'); setting GetReverse to False will return the forward complement (3'-5')
        Dim ComplementStringBuilder As New Text.StringBuilder
        Dim SequenceBases As IEnumerable(Of Char)

        If GetReverse Then
            SequenceBases = Sequence.Reverse
        Else
            SequenceBases = DirectCast(Sequence, IEnumerable(Of Char))
        End If

        For Each Base As Char In SequenceBases
            Dim NewBase As Char
            Dim BadChar As Boolean = False
            Select Case Base 'Change to use ToUpperInvariant
                Case CChar("A"), CChar("a")
                    NewBase = CChar("T")
                Case CChar("C"), CChar("c")
                    NewBase = CChar("G")
                Case CChar("G"), CChar("g")
                    NewBase = CChar("C")
                Case CChar("T"), CChar("t")
                    NewBase = CChar("A")
                Case CChar("R"), CChar("r") 'Any purine (A or G)
                    NewBase = CChar("Y")
                Case CChar("Y"), CChar("y") 'Any pyrimidine (C or T)
                    NewBase = CChar("R")
                Case CChar("M"), CChar("m") 'A or C
                    NewBase = CChar("K")
                Case CChar("K"), CChar("k") 'G or T
                    NewBase = CChar("M")
                Case CChar("S"), CChar("s") 'C or G
                    NewBase = CChar("S")
                Case CChar("W"), CChar("w") 'A or T
                    NewBase = CChar("W")
                Case CChar("B"), CChar("b") 'Not A
                    NewBase = CChar("V")
                Case CChar("D"), CChar("d") 'Not C
                    NewBase = CChar("H")
                Case CChar("H"), CChar("h") 'Not G
                    NewBase = CChar("D")
                Case CChar("V"), CChar("v") 'Not T
                    NewBase = CChar("B")
                Case CChar("N"), CChar("n") 'Any base
                    NewBase = CChar("N")
                Case Else
                    BadChar = True 'Ignore any unrecognized characters
            End Select

            If Not BadChar Then
                ComplementStringBuilder.Append(NewBase)
            End If

        Next

        Return ComplementStringBuilder.ToString
    End Function

    'Friend Shared Function FindSequence(ByVal Sequence As String, ByVal IsCircular As Boolean, ByVal SearchSequence As String) As List(Of Integer)
    '    ' Finds a sequence within a larger sequence and returns a List object containing the StartBase of the search sequence
    '    Dim ForwardMatchesLists As List(Of Integer) = FindSequenceSingleStrand(Sequence, IsCircular, SearchSequence)
    '    Dim ReverseMatchesLists As List(Of Integer) = FindSequenceSingleStrand(GetReverseComplement(Sequence), IsCircular, SearchSequence)

    '    Dim MatchesLists As New List(Of Integer)

    '    For Each Match As Integer In ForwardMatchesLists
    '        MatchesLists.Add(Match)
    '    Next

    '    'Invert the numbers for MatchesLists(1) to match the orientation of the input sequence and make negative so the function returns a single list
    '    For Each Match As Integer In ReverseMatchesLists
    '        Match -= Sequence.Length + 1
    '        MatchesLists.Add(Match)
    '    Next

    '    Return MatchesLists
    'End Function

    'Friend Shared Function FindSequenceLoose(ByVal Sequence As String, ByVal IsCircular As Boolean, ByVal SearchSequence As String) As Tuple(Of Integer, Boolean())
    '    ' Finds a sequence within a larger sequence and returns only the best match's 3' end location. Not set up to handle potentially wrong primers.
    '    Dim Matches As New List(Of Tuple(Of Integer, Boolean()))
    '    Matches.Add(FindSequenceSingleStrandLoose(Sequence, IsCircular, SearchSequence))
    '    Matches.Add(FindSequenceSingleStrandLoose(GetReverseComplement(Sequence), IsCircular, SearchSequence))

    '    'Invert the numbers for Matches(1) to match the orientation of the input sequence and make negative so the function returns a single list
    '    Dim MatchSequenceID As Integer
    '    MatchSequenceID = Matches(1).Item1 - (Sequence.Length + 1)
    '    Matches(1) = New Tuple(Of Integer, Boolean())(MatchSequenceID, Matches(1).Item2)

    '    Dim TopMatchScore As Integer = SimpleMatchScore(Matches(0).Item2)
    '    Dim BottomMatchScore As Integer = SimpleMatchScore(Matches(1).Item2)

    '    If TopMatchScore > BottomMatchScore Then
    '        Return Matches(0)
    '    Else
    '        Return Matches(1)
    '    End If

    'End Function

    'Friend Shared Function SearchSequenceJumpTable(ByVal SearchSequence As String) As Dictionary(Of Char, Integer)
    '    'Create JumpTable for the sequence
    '    Dim JumpTable As New Dictionary(Of Char, Integer)
    '    Dim SearchSequenceLastIndex = SearchSequence.Length - 1
    '    Dim BaseList() As Char = {CChar("A"), CChar("C"), CChar("G"), CChar("T")}
    '    For Each Base In BaseList
    '        Dim PossibleMatch(0 To 7) As Integer
    '        Dim SearchSequenceStringTemp As String = SearchSequence.Remove(SearchSequenceLastIndex)
    '        Select Case Base
    '            'A=A,R,M,W,D,H,V,N; C=C,Y,M,S,B,H,V,N; G=G,R,K,S,B,D,V,N; T=T,Y,K,W,B,D,H,N
    '            Case CChar("A")
    '                PossibleMatch(0) = SearchSequenceStringTemp.LastIndexOf("A")
    '                PossibleMatch(1) = SearchSequenceStringTemp.LastIndexOf("R")
    '                PossibleMatch(2) = SearchSequenceStringTemp.LastIndexOf("M")
    '                PossibleMatch(3) = SearchSequenceStringTemp.LastIndexOf("W")
    '                PossibleMatch(4) = SearchSequenceStringTemp.LastIndexOf("D")
    '                PossibleMatch(5) = SearchSequenceStringTemp.LastIndexOf("H")
    '                PossibleMatch(6) = SearchSequenceStringTemp.LastIndexOf("V")
    '                PossibleMatch(7) = SearchSequenceStringTemp.LastIndexOf("N")
    '            Case CChar("C")
    '                PossibleMatch(0) = SearchSequenceStringTemp.LastIndexOf("C")
    '                PossibleMatch(1) = SearchSequenceStringTemp.LastIndexOf("Y")
    '                PossibleMatch(2) = SearchSequenceStringTemp.LastIndexOf("M")
    '                PossibleMatch(3) = SearchSequenceStringTemp.LastIndexOf("S")
    '                PossibleMatch(4) = SearchSequenceStringTemp.LastIndexOf("B")
    '                PossibleMatch(5) = SearchSequenceStringTemp.LastIndexOf("H")
    '                PossibleMatch(6) = SearchSequenceStringTemp.LastIndexOf("V")
    '                PossibleMatch(7) = SearchSequenceStringTemp.LastIndexOf("N")
    '            Case CChar("G")
    '                PossibleMatch(0) = SearchSequenceStringTemp.LastIndexOf("G")
    '                PossibleMatch(1) = SearchSequenceStringTemp.LastIndexOf("R")
    '                PossibleMatch(2) = SearchSequenceStringTemp.LastIndexOf("K")
    '                PossibleMatch(3) = SearchSequenceStringTemp.LastIndexOf("S")
    '                PossibleMatch(4) = SearchSequenceStringTemp.LastIndexOf("B")
    '                PossibleMatch(5) = SearchSequenceStringTemp.LastIndexOf("D")
    '                PossibleMatch(6) = SearchSequenceStringTemp.LastIndexOf("V")
    '                PossibleMatch(7) = SearchSequenceStringTemp.LastIndexOf("N")
    '            Case CChar("T")
    '                PossibleMatch(0) = SearchSequenceStringTemp.LastIndexOf("T")
    '                PossibleMatch(1) = SearchSequenceStringTemp.LastIndexOf("Y")
    '                PossibleMatch(2) = SearchSequenceStringTemp.LastIndexOf("K")
    '                PossibleMatch(3) = SearchSequenceStringTemp.LastIndexOf("W")
    '                PossibleMatch(4) = SearchSequenceStringTemp.LastIndexOf("B")
    '                PossibleMatch(5) = SearchSequenceStringTemp.LastIndexOf("D")
    '                PossibleMatch(6) = SearchSequenceStringTemp.LastIndexOf("H")
    '                PossibleMatch(7) = SearchSequenceStringTemp.LastIndexOf("N")
    '        End Select

    '        Dim JumpValue As Integer = SearchSequenceLastIndex - PossibleMatch.Max
    '        JumpTable.Add(Base, JumpValue)
    '    Next

    '    Return JumpTable
    'End Function

    'Friend Shared Function FindSequenceSingleStrand(ByVal Sequence As String, ByVal IsCircular As Boolean, ByVal SearchSequence As String) As List(Of Integer)
    '    Dim SequenceBaseID As Integer
    '    Dim SearchSequenceLastIndex As Integer = SearchSequence.Length - 1
    '    Dim MatchesList As New List(Of Integer)

    '    Dim JumpTable As Dictionary(Of Char, Integer) = SearchSequenceJumpTable(SearchSequence)

    '    If IsCircular Then
    '        SequenceBaseID = 0
    '    Else
    '        SequenceBaseID = SearchSequenceLastIndex
    '    End If

    '    Do While SequenceBaseID < Sequence.Length
    '        Dim StartBaseID As Integer = SequenceBaseID

    '        For SiteBaseID As Integer = SearchSequenceLastIndex To 0 Step -1
    '            Dim SequenceBase As Char = Sequence(SequenceBaseID)
    '            Dim IsMatched As Boolean = MatchBase(SequenceBase, SearchSequence(SiteBaseID))

    '            If Not IsMatched Then
    '                'Jump to next possible match
    '                SequenceBaseID = StartBaseID + JumpTable(Sequence(StartBaseID))
    '                Exit For
    '            ElseIf SiteBaseID = 0 Then
    '                MatchesList.Add(SequenceBaseID)

    '                'Move to the end and look for more next round
    '                SequenceBaseID = StartBaseID + JumpTable(Sequence(StartBaseID))
    '            Else
    '                'Move to the next-lower base to check it against the sequence
    '                SequenceBaseID -= 1
    '                If IsCircular AndAlso SequenceBaseID < 0 Then
    '                    SequenceBaseID += Sequence.Length
    '                End If
    '            End If
    '        Next
    '    Loop

    '    Return MatchesList
    'End Function

    'Friend Shared Function FindSequenceSingleStrandLoose(ByVal Sequence As String, ByVal IsCircular As Boolean, ByVal SearchSequence As String) As Tuple(Of Integer, Boolean())
    '    Dim MatchesList As New List(Of Tuple(Of Integer, Boolean()))
    '    Dim MatchArray(0 To SearchSequence.Length - 1) As Boolean
    '    'Not a rigorous matching algorithm. Returns only the best result

    '    For SequenceIndex As Integer = 0 To Sequence.Length - 1

    '        For SearchSequenceIndex As Integer = SearchSequence.Length - 1 To 0
    '            Dim SequencePositionIndex As Integer

    '            If Not IsCircular AndAlso SequencePositionIndex < 0 Then
    '                Exit For
    '            ElseIf Not IsCircular Then
    '                SequencePositionIndex = SequenceIndex - (SearchSequence.Length - SearchSequenceIndex)
    '            Else
    '                SequencePositionIndex = SequenceIndex - (SearchSequence.Length - SearchSequenceIndex)
    '                If SequencePositionIndex < 0 Then SequencePositionIndex += Sequence.Length
    '            End If

    '            MatchArray(SearchSequenceIndex) = MatchBase(Sequence(SequencePositionIndex), SearchSequence(SearchSequenceIndex))

    '        Next


    '        MatchesList.Add(New Tuple(Of Integer, Boolean())(SequenceIndex, MatchArray))

    '    Next

    '    Dim BestMatch As Tuple(Of Integer, Boolean())
    '    Dim MaxScore As Integer

    '    For Each Match As Tuple(Of Integer, Boolean()) In MatchesList
    '        Dim NewScore As Integer = SimpleMatchScore(Match.Item2)

    '        If NewScore > MaxScore Then
    '            MaxScore = NewScore
    '            BestMatch = Match
    '        End If


    '    Next

    '    Return MatchesList(0)
    'End Function

    'Shared Function SimpleMatchScore(ByVal MatchArray As Boolean()) As Integer
    '    Dim MatchScore As Integer

    '    For Index As Integer = 0 To MatchArray.GetUpperBound(0)
    '        If MatchArray(Index) = True Then
    '            MatchScore += 1

    '            If Index > 0 AndAlso MatchArray(Index - 1) = True Then MatchScore += 1

    '            If Index < MatchArray.GetUpperBound(0) AndAlso MatchArray(Index + 1) = True Then MatchScore += 1
    '        End If



    '    Next
    'End Function

    'Friend Shared Function MoveOrigin(ByVal CurrentMacromolecule As Macromolecule, ByVal NewStartBase As Integer) As Macromolecule
    '    Dim ReorientedMacromolecule As Macromolecule = CurrentMacromolecule.Clone

    '    With ReorientedMacromolecule
    '        .Sequence = .Sequence.Substring(NewStartBase - 1) & .Sequence.Remove(NewStartBase - 1)

    '        For Each Feature As Macromolecule.Feature In .Features
    '            With Feature
    '                If .SpansOrigin Then
    '                    If (.StartBase >= NewStartBase AndAlso .EndBase > NewStartBase) OrElse (.StartBase > NewStartBase AndAlso .EndBase >= NewStartBase) OrElse (.StartBase < NewStartBase AndAlso .EndBase < NewStartBase) Then
    '                        .SpansOrigin = True
    '                    Else
    '                        .SpansOrigin = False
    '                    End If
    '                Else
    '                    If (.StartBase < NewStartBase AndAlso .EndBase >= NewStartBase) OrElse (.StartBase >= NewStartBase AndAlso .EndBase < NewStartBase) Then
    '                        .SpansOrigin = True
    '                    Else
    '                        .SpansOrigin = False
    '                    End If
    '                End If
    '            End With

    '            Reposition(Feature.StartBase, NewStartBase, .Size)
    '            Reposition(Feature.EndBase, NewStartBase, .Size)

    '        Next

    '        For Each Site As Macromolecule.RestrictionSite In .RestrictionSites
    '            Reposition(Site.StartBase, NewStartBase, .Size)
    '        Next

    '    End With

    '    Return ReorientedMacromolecule
    'End Function

    'Friend Shared Sub Reposition(ByRef BasePosition As Integer, ByVal BaseShift As Integer, ByVal TotalBases As Integer)
    '    BasePosition -= BaseShift - 1

    '    If BasePosition < 1 Then BasePosition += TotalBases
    'End Sub

    'Shared Function Invert(ByVal InputMacromolecule As Macromolecule) As Macromolecule
    '    Dim InvertedMacromolecule As New Macromolecule

    '    With InputMacromolecule
    '        InvertedMacromolecule.Name = .Name
    '        InvertedMacromolecule.Sequence = SequenceTools.GetReverseComplement(.Sequence)
    '        InvertedMacromolecule.IsCircular = .IsCircular
    '        InvertedMacromolecule.StartOverhang = .EndOverhang
    '        InvertedMacromolecule.EndOverhang = .StartOverhang
    '        InvertedMacromolecule.Comments = .Comments

    '        For Each Feature As Macromolecule.Feature In .Features
    '            Dim InvertedFeature As New Macromolecule.Feature
    '            With Feature
    '                InvertedFeature.Name = .Name
    '                InvertedFeature.SpansOrigin = .SpansOrigin
    '                InvertedFeature.Type = .Type
    '                InvertedFeature.Completeness = .Completeness
    '                InvertedFeature.StartBase = InputMacromolecule.Size - .StartBase + 1
    '                InvertedFeature.EndBase = InputMacromolecule.Size - .EndBase + 1
    '                InvertedFeature.Comments = .Comments
    '            End With
    '            InvertedMacromolecule.Features.Add(InvertedFeature)
    '        Next

    '        For Each RestrictionSite As Macromolecule.RestrictionSite In .RestrictionSites
    '            Dim InvertedRestrictionSite As New Macromolecule.RestrictionSite
    '            With RestrictionSite
    '                InvertedRestrictionSite.EnzymeName = .EnzymeName
    '                InvertedRestrictionSite.StartBase = InputMacromolecule.Size - .StartBase + 1
    '                InvertedRestrictionSite.TopStrandCleavageOffset = .BottomStrandCleavageOffset
    '                InvertedRestrictionSite.BottomStrandCleavageOffset = .TopStrandCleavageOffset
    '            End With
    '            InvertedMacromolecule.RestrictionSites.Add(InvertedRestrictionSite)
    '        Next
    '    End With

    '    Return InvertedMacromolecule
    'End Function

    'Shared Function MatchBase(ByVal SequenceBase As Char, ByVal SearchBase As Char) As Boolean
    '    Dim IsMatched As Boolean

    '    Select Case SearchBase
    '        Case CChar("A"), CChar("C"), CChar("G"), CChar("T")
    '            IsMatched = SequenceBase = SearchBase
    '        Case CChar("R") 'Any purine (A or G)                        
    '            IsMatched = SequenceBase = CChar("A") OrElse SequenceBase = CChar("G")
    '        Case CChar("Y") 'Any pyrimidine (C or T)
    '            IsMatched = SequenceBase = CChar("C") OrElse SequenceBase = CChar("T")
    '        Case CChar("M") 'A or C
    '            IsMatched = SequenceBase = CChar("A") OrElse SequenceBase = CChar("C")
    '        Case CChar("K") 'G or T
    '            IsMatched = SequenceBase = CChar("G") OrElse SequenceBase = CChar("T")
    '        Case CChar("S") 'C or G
    '            IsMatched = SequenceBase = CChar("C") OrElse SequenceBase = CChar("G")
    '        Case CChar("W") 'A or T
    '            IsMatched = SequenceBase = CChar("A") OrElse SequenceBase = CChar("T")
    '        Case CChar("B") 'Not A
    '            IsMatched = SequenceBase <> CChar("A")
    '        Case CChar("D") 'Not C
    '            IsMatched = SequenceBase <> CChar("C")
    '        Case CChar("H") 'Not G
    '            IsMatched = SequenceBase <> CChar("G")
    '        Case CChar("V") 'Not T
    '            IsMatched = SequenceBase <> CChar("T")
    '        Case CChar("N") 'Any base
    '            IsMatched = True
    '    End Select

    '    Return IsMatched
    'End Function

End Class

'Friend Class RestrictionAnalysis
'    Friend Shared EnzymeList As Dictionary(Of String, RestrictionEnzyme)

'    Friend Shared Sub LoadRestrictionEnzymeDatabase()
'        EnzymeList = New Dictionary(Of String, RestrictionEnzyme)

'        'Read file
'        Dim OpenFileStream As New IO.FileStream(My.Settings.RestrictionEnzymeDatabaseLocation, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
'        Dim OpenFileReader As New IO.StreamReader(OpenFileStream)
'        Dim FileLineList As New List(Of String)

'        Do
'            FileLineList.Add(OpenFileReader.ReadLine.TrimEnd())
'        Loop Until OpenFileReader.EndOfStream

'        OpenFileReader.Close()
'        OpenFileStream.Close()

'        For FileLineID As Integer = 0 To FileLineList.Count - 1
'            If FileLineList(FileLineID).StartsWith("<1") AndAlso Not FileLineList(FileLineID + 4).Contains("?") AndAlso Not FileLineList(FileLineID + 4).Contains(",") AndAlso (FileLineList(FileLineID + 4).Contains("^") OrElse FileLineList(FileLineID + 4).Contains("(")) AndAlso FileLineList(FileLineID + 5).Contains("(") Then
'                ' Doesn't load enzymes with unknown restriction sites, including known target sites with unknown cleavage sites, or with multiple recognition sites (all of these have undefined cleavage sites), or those that require methylation
'                Dim Enzyme As New RestrictionEnzyme

'                Enzyme.Name = FileLineList(FileLineID).Substring(3)
'                Enzyme.Prototype = FileLineList(FileLineID + 1).Substring(3)
'                Enzyme.SourceOrganism = FileLineList(FileLineID + 2).Substring(3)
'                Dim RestrictionSites() As String = FileLineList(FileLineID + 4).Substring(3).Split(New Char() {CChar(",")})

'                For Each RestrictionSiteString As String In RestrictionSites
'                    Dim RestrictionSite As New RestrictionEnzyme.RestrictionSite
'                    With RestrictionSite
'                        If RestrictionSiteString.Contains("^") Then
'                            .TopStrandCleavageSite = RestrictionSiteString.IndexOf("^") - 1

'                            RestrictionSiteString = RestrictionSiteString.Remove(.TopStrandCleavageSite + 1, 1)

'                            .BottomStrandCleavageSite = RestrictionSiteString.Length - .TopStrandCleavageSite - 2

'                            Enzyme.HasInternalCleavageSites = True
'                            Enzyme.IsPallandromic = True
'                        ElseIf RestrictionSiteString.Contains("(") Then
'                            Dim LeftExtension(0 To 1) As String
'                            Enzyme.IsPallandromic = False

'                            If RestrictionSiteString.StartsWith("(") Then
'                                ' This will only exist if there are two sites; though I should maybe rewrite it to be general 
'                                LeftExtension = RestrictionSiteString.Substring(1, RestrictionSiteString.IndexOf(CChar(")")) - 1).Split(New Char() {CChar("/")})

'                                Enzyme.HasSecondCleavageSite = True

'                                RestrictionSiteString = RestrictionSiteString.Substring(RestrictionSiteString.IndexOf(")") + 1)
'                            End If

'                            Dim RightExtension() As String = RestrictionSiteString.Substring(RestrictionSiteString.IndexOf("(") + 1).TrimEnd(CChar(")")).Split(New Char() {CChar("/")})

'                            RestrictionSiteString = RestrictionSiteString.Substring(0, RestrictionSiteString.IndexOf("("))

'                            Dim RestrictionSequenceLastIndex = RestrictionSiteString.Length - 1

'                            .TopStrandCleavageSite = RestrictionSequenceLastIndex + CInt(RightExtension(0))
'                            .BottomStrandCleavageSite = RestrictionSequenceLastIndex + CInt(RightExtension(1))

'                            If Enzyme.HasSecondCleavageSite Then
'                                .TopStrandSecondCleavageSite = -CInt(LeftExtension(0)) - 1
'                                .BottomStrandSecondCleavageSite = -CInt(LeftExtension(1)) - 1
'                                If RestrictionSiteString = SequenceTools.GetReverseComplement(RestrictionSiteString) AndAlso .TopStrandCleavageSite - RestrictionSequenceLastIndex = -.BottomStrandSecondCleavageSite - 1 AndAlso .BottomStrandCleavageSite - RestrictionSequenceLastIndex = -.TopStrandSecondCleavageSite - 1 Then
'                                    Enzyme.IsPallandromic = True
'                                End If
'                            End If

'                            If .TopStrandCleavageSite >= 0 AndAlso .TopStrandCleavageSite < RestrictionSequenceLastIndex AndAlso .BottomStrandCleavageSite >= 0 AndAlso .BottomStrandCleavageSite < RestrictionSequenceLastIndex Then
'                                'If the site is inside the recognition sequence
'                                If Enzyme.HasSecondCleavageSite Then
'                                    If Not (.TopStrandSecondCleavageSite >= 0 AndAlso .TopStrandSecondCleavageSite < RestrictionSequenceLastIndex AndAlso .BottomStrandSecondCleavageSite >= 0 AndAlso .BottomStrandSecondCleavageSite < RestrictionSequenceLastIndex) Then
'                                        Exit For
'                                    End If
'                                End If

'                                Enzyme.HasInternalCleavageSites = True
'                            End If
'                        End If

'                        .Sequence = RestrictionSiteString

'                        With .Sequence
'                            If .Contains("N") OrElse
'                               .Contains("R") OrElse .Contains("Y") OrElse
'                               .Contains("M") OrElse .Contains("K") OrElse
'                               .Contains("S") OrElse .Contains("W") OrElse
'                               .Contains("B") OrElse .Contains("D") OrElse
'                               .Contains("H") OrElse .Contains("V") Then

'                                Enzyme.HasDegeneracy = True

'                            End If
'                        End With


'                    End With

'                    Enzyme.RestrictionSites.Add(RestrictionSite)

'                    If Not Enzyme.IsPallandromic Then
'                        Dim ReverseRestrictionSite As New RestrictionEnzyme.RestrictionSite
'                        With ReverseRestrictionSite
'                            .Sequence = SequenceTools.GetReverseComplement(RestrictionSite.Sequence)
'                            If Enzyme.HasSecondCleavageSite Then
'                                .TopStrandCleavageSite = -RestrictionSite.BottomStrandSecondCleavageSite + .Sequence.Length - 2
'                                .BottomStrandCleavageSite = -RestrictionSite.TopStrandSecondCleavageSite + .Sequence.Length - 2
'                                .TopStrandSecondCleavageSite = -RestrictionSite.BottomStrandCleavageSite + .Sequence.Length - 2
'                                .BottomStrandSecondCleavageSite = -RestrictionSite.TopStrandCleavageSite + .Sequence.Length - 2
'                            Else
'                                .TopStrandCleavageSite = -RestrictionSite.BottomStrandCleavageSite + .Sequence.Length - 2
'                                .BottomStrandCleavageSite = -RestrictionSite.TopStrandCleavageSite + .Sequence.Length - 2
'                            End If
'                        End With

'                        Enzyme.RestrictionSites.Add(ReverseRestrictionSite)
'                    End If

'                Next

'                Enzyme.CommercialSources = FileLineList(FileLineID + 6).Substring(3)

'                FileLineID += 8

'                EnzymeList.Add(Enzyme.Name, Enzyme)
'            End If

'            If FileLineList(FileLineID).StartsWith("References") Then Exit For
'        Next
'    End Sub

'    Friend Shared Sub FindRestrictionSites(ByRef CurrentMacromolecule As Macromolecule)
'        Dim FoundRestrictionSites As New List(Of Macromolecule.RestrictionSite)

'        'Use Boyer and Moore-type search algorithm to find restriction sites in the sequence
'        For Each Enzyme As RestrictionEnzyme In EnzymeList.Values

'            If Enzyme.Name = "AclI" Or Enzyme.Name = "BalI" Then
'            Else
'                Continue For
'            End If

'            For Each RestrictionSite In Enzyme.RestrictionSites
'                With RestrictionSite
'                    Dim RestrictionSiteStartBaseIDList As List(Of Integer) = SequenceTools.FindSequenceSingleStrand(CurrentMacromolecule.Sequence, CurrentMacromolecule.IsCircular, .Sequence)

'                    'Probably isn't the best way to do this, but this checks that there is enough flanking sequence for non-internal cleavage sites
'                    ' Need to make sure that it keeps double-cutters where only one set is within the sequence
'                    Dim RestrictionSiteStartBaseIDToRemoveList As New List(Of Integer)
'                    If Not CurrentMacromolecule.IsCircular AndAlso Not Enzyme.HasInternalCleavageSites Then
'                        For Each StartBaseID As Integer In RestrictionSiteStartBaseIDList
'                            If StartBaseID + .TopStrandCleavageSite < 0 OrElse StartBaseID + .TopStrandCleavageSite >= CurrentMacromolecule.Size - 1 OrElse StartBaseID + .BottomStrandCleavageSite < 0 OrElse StartBaseID + .BottomStrandCleavageSite >= CurrentMacromolecule.Size - 1 Then
'                                RestrictionSiteStartBaseIDToRemoveList.Add(StartBaseID)
'                            ElseIf Enzyme.HasSecondCleavageSite Then
'                                If StartBaseID + .TopStrandSecondCleavageSite < 0 OrElse StartBaseID + .TopStrandSecondCleavageSite >= CurrentMacromolecule.Size - 1 OrElse StartBaseID + .BottomStrandSecondCleavageSite < 0 OrElse StartBaseID + .BottomStrandSecondCleavageSite >= CurrentMacromolecule.Size - 1 Then
'                                    RestrictionSiteStartBaseIDToRemoveList.Add(StartBaseID)
'                                End If
'                            End If
'                        Next
'                    End If

'                    For Each StartBaseID As Integer In RestrictionSiteStartBaseIDToRemoveList
'                        RestrictionSiteStartBaseIDList.Remove(StartBaseID)
'                    Next

'                    For Each StartBaseID As Integer In RestrictionSiteStartBaseIDList
'                        'Add the site to the list
'                        Dim FoundRestrictionSite As New Macromolecule.RestrictionSite

'                        FoundRestrictionSite.EnzymeName = Enzyme.Name
'                        FoundRestrictionSite.StartBase = StartBaseID + 1
'                        FoundRestrictionSite.TopStrandCleavageOffset = .TopStrandCleavageSite
'                        FoundRestrictionSite.BottomStrandCleavageOffset = .BottomStrandCleavageSite
'                        FoundRestrictionSites.Add(FoundRestrictionSite)

'                        Debug.Print(CurrentMacromolecule.Sequence.Substring(FoundRestrictionSite.StartBase - 5, 20))

'                        If Enzyme.HasSecondCleavageSite Then
'                            Dim FoundSecondRestrictionSite As New Macromolecule.RestrictionSite

'                            FoundSecondRestrictionSite.EnzymeName = Enzyme.Name
'                            FoundSecondRestrictionSite.StartBase = StartBaseID + 1
'                            FoundSecondRestrictionSite.TopStrandCleavageOffset = .TopStrandSecondCleavageSite
'                            FoundSecondRestrictionSite.BottomStrandCleavageOffset = .BottomStrandSecondCleavageSite

'                            FoundRestrictionSites.Add(FoundSecondRestrictionSite)
'                        End If
'                    Next
'                End With
'            Next
'        Next

'        FoundRestrictionSites.Sort()
'        CurrentMacromolecule.RestrictionSites = FoundRestrictionSites

'    End Sub

'    Friend Shared Function Digest(ByVal Parent As Macromolecule, ByVal Sites As Queue(Of Macromolecule.RestrictionSite)) As ObservableCollection(Of Macromolecule)
'        Dim DigestionProducts As New ObservableCollection(Of Macromolecule)

'        If Parent.IsCircular Then
'            Parent = Linearize(Parent, Sites.Peek)

'            'Reposition sites
'            Dim NewBasePosition As Integer = Sites.Peek.MinSite
'            For Each Site As Macromolecule.RestrictionSite In Sites
'                SequenceTools.Reposition(Site.StartBase, NewBasePosition, Parent.Size)
'            Next

'            Sites.Dequeue()
'        End If

'        If Sites.Count > 0 Then
'            DigestionProducts.Add(ConstructFragment(Parent, Nothing, Sites.Peek))

'            While Sites.Count > 1
'                DigestionProducts.Add(ConstructFragment(Parent, Sites.Dequeue, Sites.Peek))
'            End While

'            DigestionProducts.Add(ConstructFragment(Parent, Sites.Dequeue, Nothing))
'        Else
'            DigestionProducts.Add(Parent)
'        End If


'        Return DigestionProducts
'    End Function

'    Friend Shared Function Ligate(ByVal Fragments As ObservableCollection(Of Macromolecule), ByVal IgnoreBluntEnds As Boolean) As ObservableCollection(Of Macromolecule)
'        'Should have detection for multiple products, and output them?
'        'Also, should standardize the list/collection objects I'm using

'        Dim LigatedMacromolecules As New ObservableCollection(Of Macromolecule)


'        Dim FragmentPairings As New List(Of Pairing)

'        'Load Fragments into a Queue
'        Dim FragmentsQueue As New Queue(Of Macromolecule)
'        For Each Fragment As Macromolecule In Fragments
'            FragmentsQueue.Enqueue(Fragment)
'        Next

'        'Find all the pairing arrangements.
'        While FragmentsQueue.Count > 1
'            Dim CurrentFragment As Macromolecule = FragmentsQueue.Dequeue
'            Dim StartOverhang As String
'            Dim EndOverhang As String

'            With CurrentFragment
'                StartOverhang = .Sequence.Substring(0, Abs(.StartOverhang))
'                EndOverhang = .Sequence.Substring(.Sequence.Length - Abs(.EndOverhang))
'            End With

'            For Each OtherFragment As Macromolecule In FragmentsQueue

'                'May not be the most efficient way to do this
'                With OtherFragment
'                    If Sign(CurrentFragment.StartOverhang) = Sign(.StartOverhang) Then
'                        Dim IsMatched As Boolean

'                        If Not IgnoreBluntEnds AndAlso CurrentFragment.StartOverhang = 0 Then
'                            IsMatched = True
'                        ElseIf CurrentFragment.StartOverhang <> 0 Then
'                            Dim OtherStartOverhang As String = .Sequence.Substring(0, Abs(.StartOverhang))
'                            If StartOverhang = OtherStartOverhang Then
'                                IsMatched = True
'                            End If
'                        End If

'                        If IsMatched Then
'                            FragmentPairings.Add(New Pairing(Fragments.IndexOf(CurrentFragment) + 1, Fragments.IndexOf(OtherFragment) + 1, PairingType.StartToStart))
'                        End If
'                    End If

'                    If Sign(CurrentFragment.StartOverhang) = Sign(.EndOverhang) Then
'                        Dim IsMatched As Boolean

'                        If Not IgnoreBluntEnds AndAlso CurrentFragment.StartOverhang = 0 Then
'                            IsMatched = True
'                        ElseIf CurrentFragment.StartOverhang <> 0 Then
'                            Dim OtherEndOverhang As String = SequenceTools.GetReverseComplement(.Sequence.Substring(.Sequence.Length - Abs(.EndOverhang)))
'                            If StartOverhang = OtherEndOverhang Then
'                                IsMatched = True
'                            End If
'                        End If

'                        If IsMatched Then
'                            FragmentPairings.Add(New Pairing(Fragments.IndexOf(CurrentFragment) + 1, Fragments.IndexOf(OtherFragment) + 1, PairingType.StartToEnd))
'                        End If
'                    End If

'                    If Sign(CurrentFragment.EndOverhang) = Sign(.StartOverhang) Then
'                        Dim IsMatched As Boolean

'                        If Not IgnoreBluntEnds AndAlso CurrentFragment.EndOverhang = 0 Then
'                            IsMatched = True
'                        ElseIf CurrentFragment.EndOverhang <> 0 Then
'                            Dim OtherStartOverhang As String = SequenceTools.GetReverseComplement(.Sequence.Substring(0, Abs(.StartOverhang)))
'                            If EndOverhang = OtherStartOverhang Then
'                                IsMatched = True
'                            End If
'                        End If

'                        If IsMatched Then
'                            FragmentPairings.Add(New Pairing(Fragments.IndexOf(CurrentFragment) + 1, Fragments.IndexOf(OtherFragment) + 1, PairingType.EndToStart))
'                        End If
'                    End If

'                    If Sign(CurrentFragment.EndOverhang) = Sign(.EndOverhang) Then
'                        Dim IsMatched As Boolean

'                        If Not IgnoreBluntEnds AndAlso CurrentFragment.EndOverhang = 0 Then
'                            IsMatched = True
'                        ElseIf CurrentFragment.EndOverhang <> 0 Then
'                            Dim OtherStartOverhang As String = OtherFragment.Sequence.Substring(.Sequence.Length - Abs(.EndOverhang))
'                            If EndOverhang = OtherStartOverhang Then
'                                IsMatched = True
'                            End If
'                        End If

'                        If IsMatched Then
'                            FragmentPairings.Add(New Pairing(Fragments.IndexOf(CurrentFragment) + 1, Fragments.IndexOf(OtherFragment) + 1, PairingType.EndToEnd))
'                        End If
'                    End If
'                End With
'            Next
'        End While

'        'If no pairings are found, return the unligated fragments
'        If FragmentPairings.Count = 0 Then
'            LigatedMacromolecules = Fragments
'            Return LigatedMacromolecules
'        End If

'        'Abort if any fragment can pair with multiple fragments; currently unable to handle those
'        For Each Fragment As Macromolecule In Fragments
'            Dim FragmentIndex As Integer = Fragments.IndexOf(Fragment)
'            If FragmentPairings.Where(Function(Pair As Pairing) (Pair.MacromoleculeIndex1 = FragmentIndex OrElse Pair.MacromoleculeIndex2 = FragmentIndex)).Count > 2 Then
'                MsgBox("Warning: Ligation algorithm currently cannot handle multiple possible pairs. Aborting.")
'                Return LigatedMacromolecules
'            End If

'        Next



'        'Go through pairings to produce complete Macromolecules. This method should allow for multiple ligation products (except for multimers of the same molecule)

'        'Read fragments into a list for the starting fragments. This is a list instead of a queue in order to allow internal items to be removed.
'        Dim FragmentsStartList As New List(Of Macromolecule)
'        For Each Fragment As Macromolecule In Fragments
'            FragmentsStartList.Add(Fragment)
'        Next

'        'Sequentially build up macromolecules
'        'Overview of algorithm:
'        '1 Iterate through the build list.
'        '2 For each build in the list, add the next fragment.
'        '3 If more than one possible fragment, create a copy and add it to the build list.
'        '4 If added fragment has no other possible pairs at connected end, remove it from the FragmentsStartList
'        '5 If added fragment is the same as the starting macromolecule in the same orientation, add the build to MacromoleculeBuilderList and remove it from the current build list. (plasmid formed)
'        '6 If added fragment is a duplicate of an internal macromolecule in the same orientation, add the build to MacromoleculeBuilderList and remove it from the current build list. (infinite linear chain)
'        '7 If there are no possible fragments, start working backward from the first fragment in the build, implementing 2, 3, 4

'        Dim MacromoleculeBuilderList As New List(Of List(Of Integer))
'        While FragmentsStartList.Count > 0
'            'Grab the first item in the FragmentsStartList, find its position in Fragments, and remove it
'            Dim CurrentMacromoleculeIndex As Integer = Fragments.IndexOf(FragmentsStartList(0)) + 1
'            FragmentsStartList.RemoveAt(0)
'            Dim CurrentBuilderQueue As New Queue(Of List(Of Integer))
'            Dim OriginalBuild As New List(Of Integer)
'            OriginalBuild.Add(CurrentMacromoleculeIndex)
'            CurrentBuilderQueue.Enqueue(OriginalBuild)
'            'Iterate through the current builder list, which is being moved off to 
'            While CurrentBuilderQueue.Count > 0
'                Dim CurrentBuild As List(Of Integer) = CurrentBuilderQueue.Peek

'                'Loop through the current build going to the right
'                Do
'                    Dim TerminalFragmentSignedIndex As Integer = CurrentBuild.Last
'                    Dim EndPairs As New List(Of Pairing)(FragmentPairings.Where(Function(Pair As Pairing) (Pair.MacromoleculeIndex1 = Abs(TerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.EndToEnd OrElse Pair.PairingType = PairingType.EndToStart) OrElse (Pair.MacromoleculeIndex2 = Abs(TerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.StartToEnd OrElse Pair.PairingType = PairingType.EndToEnd)))))
'                    Dim NewBuilds As New List(Of List(Of Integer))

'                    'For every additional EndPair, create a new build, copy the current build to it, and add it to the end of a temporary list
'                    For Counter As Integer = 1 To EndPairs.Count - 1
'                        Dim NewBuild As New List(Of Integer)(CurrentBuild)
'                        NewBuilds.Add(NewBuild)
'                    Next

'                    For Each EndPair As Pairing In EndPairs
'                        Dim TempBuild As List(Of Integer)
'                        Dim EndPairIndex As Integer = EndPairs.IndexOf(EndPair)

'                        'Select the right item to build from
'                        If EndPairIndex = 0 Then
'                            TempBuild = CurrentBuild
'                        Else
'                            TempBuild = NewBuilds(EndPairIndex - 1)
'                        End If

'                        With EndPair
'                            Select Case .PairingType
'                                Case PairingType.StartToEnd
'                                    'Only occurs in pairings where the starting macromolecule is in the second position
'                                    TempBuild.Add(.MacromoleculeIndex1)
'                                Case PairingType.EndToStart
'                                    'Only occurs pairings where the starting macromolecule is in the first position
'                                    TempBuild.Add(.MacromoleculeIndex2)
'                                Case PairingType.EndToEnd
'                                    If CurrentMacromoleculeIndex = .MacromoleculeIndex2 Then
'                                        TempBuild.Add(-.MacromoleculeIndex1)
'                                    Else
'                                        TempBuild.Add(-.MacromoleculeIndex2)
'                                    End If
'                            End Select
'                        End With

'                        'Do some checking of the added fragment
'                        Dim NewTerminalFragmentSignedIndex As Integer = TempBuild.Last

'                        'If there is only one pairing at the start (connected end) of the added fragment, which would be to the same fragment its already connected to, remove it from the FragmentsStartList
'                        Dim AddedFragmentStartPairs As New List(Of Pairing)(FragmentPairings.Where(Function(Pair As Pairing) (Pair.MacromoleculeIndex1 = Abs(NewTerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.StartToStart OrElse Pair.PairingType = PairingType.StartToEnd) OrElse (Pair.MacromoleculeIndex2 = Abs(NewTerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.StartToStart OrElse Pair.PairingType = PairingType.EndToStart)))))
'                        If AddedFragmentStartPairs.Count = 1 Then
'                            FragmentsStartList.Remove(Fragments(Abs(NewTerminalFragmentSignedIndex) - 1))
'                        End If

'                        'If the added fragment is a duplicate of the CurrentMacromoleculeIndex (plasmid) or if it is a duplicate of an internal macromolecule in the same orientation (start of infinite linear chain), add the build to the MacromoleculeBuilderList and remove it from the current build list, and move to the next build.
'                        If TempBuild.IndexOf(NewTerminalFragmentSignedIndex) < TempBuild.Count - 1 Then
'                            MacromoleculeBuilderList.Add(TempBuild)
'                            If TempBuild Is CurrentBuild Then CurrentBuilderQueue.Dequeue()
'                            Continue While
'                        End If

'                        'If using a new build and it is incomplete, add it to the end of the CurrentBuilderQueue; because TempBuild points to the original Build, the current build in the queue is directly updated
'                        If EndPairIndex > 0 Then
'                            CurrentBuilderQueue.Enqueue(TempBuild)
'                        End If
'                    Next

'                    'If there are no more end pairs, exit the loop
'                    If EndPairs.Count = 0 Then Exit Do
'                Loop

'                'Loop through the current build going to the left
'                Do
'                    Dim TerminalFragmentSignedIndex As Integer = CurrentBuild.First
'                    Dim StartPairs As New List(Of Pairing)(FragmentPairings.Where(Function(Pair As Pairing) (Pair.MacromoleculeIndex1 = Abs(TerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.StartToStart OrElse Pair.PairingType = PairingType.StartToEnd) OrElse (Pair.MacromoleculeIndex2 = Abs(TerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.StartToStart OrElse Pair.PairingType = PairingType.EndToStart)))))
'                    Dim NewBuilds As New List(Of List(Of Integer))

'                    'For every additional StartPair, create a new build, copy the current build to it, and add it to the end of a temporary list
'                    For Counter As Integer = 1 To StartPairs.Count - 1
'                        Dim NewBuild As New List(Of Integer)(CurrentBuild)
'                        NewBuilds.Add(NewBuild)
'                    Next

'                    For Each StartPair As Pairing In StartPairs
'                        Dim TempBuild As List(Of Integer)
'                        Dim StartPairIndex As Integer = StartPairs.IndexOf(StartPair)

'                        'Select the right item to build from
'                        If StartPairIndex = 0 Then
'                            TempBuild = CurrentBuild
'                        Else
'                            TempBuild = NewBuilds(StartPairIndex - 1)
'                        End If

'                        With StartPair
'                            Select Case .PairingType
'                                Case PairingType.EndToStart
'                                    'Only occurs in pairings where the starting macromolecule is in the second position
'                                    TempBuild.Insert(0, .MacromoleculeIndex1)
'                                Case PairingType.StartToEnd
'                                    'Only occurs pairings where the starting macromolecule is in the first position
'                                    TempBuild.Insert(0, .MacromoleculeIndex2)
'                                Case PairingType.StartToStart
'                                    If CurrentMacromoleculeIndex = .MacromoleculeIndex2 Then
'                                        TempBuild.Insert(0, -.MacromoleculeIndex1)
'                                    Else
'                                        TempBuild.Insert(0, -.MacromoleculeIndex2)
'                                    End If
'                            End Select
'                        End With

'                        'Do some checking of the added fragment
'                        Dim NewTerminalFragmentSignedIndex As Integer = TempBuild.First

'                        'If there is only one pairing at the end (connected end) of the added fragment, which would be to the same fragment its already connected to, remove it from the FragmentsStartList
'                        Dim AddedFragmentEndPairs As New List(Of Pairing)(FragmentPairings.Where(Function(Pair As Pairing) (Pair.MacromoleculeIndex1 = Abs(NewTerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.EndToEnd OrElse Pair.PairingType = PairingType.EndToStart) OrElse (Pair.MacromoleculeIndex2 = Abs(NewTerminalFragmentSignedIndex) AndAlso (Pair.PairingType = PairingType.StartToEnd OrElse Pair.PairingType = PairingType.EndToEnd)))))
'                        If AddedFragmentEndPairs.Count = 1 Then
'                            FragmentsStartList.Remove(Fragments(Abs(NewTerminalFragmentSignedIndex) - 1))
'                        End If

'                        'If using a new build, add any incomplete new builds to the end of the CurrentBuilderQueue; because TempBuild points to the original Build, the current build in the queue is directly updated
'                        If StartPairIndex > 0 Then
'                            CurrentBuilderQueue.Enqueue(TempBuild)
'                        End If

'                    Next

'                    'If there are no more start pairs, exit the loop
'                    If StartPairs.Count = 0 Then Exit Do
'                Loop

'                'If the fragment makes it here, it's done (linear molecule)
'                MacromoleculeBuilderList.Add(CurrentBuilderQueue.Dequeue)
'            End While
'        End While

'        'Use MacromoleculeBuilderList to stitch together the fragments into full macromolecules
'        For Each MacromoleculeBuilder As List(Of Integer) In MacromoleculeBuilderList
'            Dim StartFragmentSignedIndex As Integer = MacromoleculeBuilder(0)
'            Dim LigatedMacromolecule As New Macromolecule

'            Select Case StartFragmentSignedIndex
'                Case Is > 0
'                    LigatedMacromolecule = Fragments(Abs(StartFragmentSignedIndex) - 1)
'                Case Is < 0
'                    LigatedMacromolecule = SequenceTools.Invert(Fragments(Abs(StartFragmentSignedIndex) - 1))
'            End Select

'            MacromoleculeBuilder.RemoveAt(0)

'            For Each SignedIndex As Integer In MacromoleculeBuilder
'                Dim NewFragment As New Macromolecule

'                'If the index is identical to the start index, circularize it (should only happen with final segment). Otherwise, ligate it.
'                If StartFragmentSignedIndex = SignedIndex Then
'                    LigatedMacromolecule = Circularize(LigatedMacromolecule)
'                Else
'                    Select Case SignedIndex
'                        Case Is > 0
'                            NewFragment = Fragments(Abs(SignedIndex) - 1)
'                        Case Is < 0
'                            NewFragment = SequenceTools.Invert(Fragments(Abs(SignedIndex) - 1))
'                    End Select

'                    LigatedMacromolecule = LigateFragments(LigatedMacromolecule, NewFragment)
'                End If

'                LigatedMacromolecules.Add(LigatedMacromolecule)

'            Next
'        Next

'        'Find any unpaired fragments and return them as well
'        Dim UnusedFragments As New List(Of Macromolecule)(Fragments)
'        For Each Pair As Pairing In FragmentPairings
'            UnusedFragments.Remove(Fragments(Pair.MacromoleculeIndex1 - 1))
'            UnusedFragments.Remove(Fragments(Pair.MacromoleculeIndex2 - 1))
'        Next

'        For Each Fragment As Macromolecule In UnusedFragments
'            LigatedMacromolecules.Add(Fragment)
'        Next

'        Return LigatedMacromolecules
'    End Function

'    Private Enum PairingType
'        StartToStart
'        StartToEnd
'        EndToStart
'        EndToEnd
'    End Enum

'    Private Class Pairing
'        Friend MacromoleculeIndex1 As Integer
'        Friend MacromoleculeIndex2 As Integer
'        Friend PairingType As PairingType

'        Sub New()

'        End Sub

'        Sub New(ByVal Pair1 As Integer, ByVal Pair2 As Integer, ByVal PairType As PairingType)
'            MacromoleculeIndex1 = Pair1
'            MacromoleculeIndex2 = Pair2
'            PairingType = PairType
'        End Sub

'    End Class

'    Shared Function ConstructFragment(ByVal Parent As Macromolecule, ByVal StartSite As Macromolecule.RestrictionSite, ByVal EndSite As Macromolecule.RestrictionSite) As Macromolecule
'        Dim Fragment As New Macromolecule
'        Dim StartBase As Integer
'        Dim EndBase As Integer

'        If StartSite IsNot Nothing Then
'            With StartSite
'                StartBase = .MinSite
'                Fragment.StartOverhang = .Overhang
'            End With
'        Else
'            StartBase = 1
'            Fragment.StartOverhang = Parent.StartOverhang
'        End If

'        If EndSite IsNot Nothing Then
'            With EndSite
'                EndBase = .MaxSite
'                Fragment.EndOverhang = .Overhang
'            End With
'        Else
'            EndBase = Parent.Size
'            Fragment.EndOverhang = Parent.EndOverhang
'        End If

'        Fragment.Sequence = Parent.Sequence.Substring(StartBase - 1, EndBase - StartBase + 1)
'        Fragment.Name = Parent.Name & " (" & StartBase & "-" & EndBase & ")"
'        Fragment.IsCircular = False

'        For Each Feature As Macromolecule.Feature In Parent.Features
'            Dim ModFeature As Macromolecule.Feature = Feature.Clone

'            With ModFeature
'                If (.StartBase < StartBase AndAlso .EndBase < StartBase) OrElse (.StartBase > EndBase AndAlso .EndBase > EndBase) Then
'                Else
'                    If Min(.StartBase, .EndBase) >= StartBase AndAlso Max(.EndBase, .StartBase) <= EndBase Then
'                        .StartBase = .StartBase - StartBase + 1
'                        .EndBase = .EndBase - StartBase + 1
'                    ElseIf .StartBase < StartBase AndAlso .EndBase > EndBase Then
'                        .StartBase = 1
'                        .EndBase = EndBase - StartBase + 1
'                        .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                    ElseIf .EndBase < StartBase AndAlso .StartBase > EndBase Then
'                        .StartBase = EndBase - StartBase + 1
'                        .EndBase = 1
'                        .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                    Else
'                        If .StartBase < StartBase Then
'                            .StartBase = 1
'                            .EndBase = .EndBase - StartBase + 1
'                            .Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                        ElseIf .EndBase < StartBase Then
'                            .StartBase = .StartBase - StartBase + 1
'                            .EndBase = 1
'                            .Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                        ElseIf .StartBase > EndBase Then
'                            .StartBase = EndBase - StartBase + 1
'                            .EndBase = .EndBase - StartBase + 1
'                            .Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                        ElseIf .EndBase > EndBase Then
'                            .StartBase = .StartBase - StartBase + 1
'                            .EndBase = EndBase - StartBase + 1
'                            .Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                        End If

'                        If Feature.Completeness <> .Completeness Then .Completeness = Macromolecule.Feature.CompletenessTypes.Internal

'                    End If
'                    Fragment.Features.Add(ModFeature)
'                End If
'            End With
'        Next

'        Return Fragment
'    End Function

'    Shared Function LigateFragments(ByVal Fragment1 As Macromolecule, ByVal Fragment2 As Macromolecule) As Macromolecule 'Assumes that the ligation occurs at the end of Fragment 1 and the start of Fragment 2
'        Dim LigatedMacromolecule As New Macromolecule

'        LigatedMacromolecule.Name = Fragment1.Name & "-" & Fragment2.Name
'        LigatedMacromolecule.IsCircular = False
'        LigatedMacromolecule.Sequence = Fragment1.Sequence
'        LigatedMacromolecule.Features = Fragment1.Features
'        LigatedMacromolecule.StartOverhang = Fragment1.StartOverhang
'        LigatedMacromolecule.EndOverhang = Fragment2.EndOverhang
'        LigatedMacromolecule.RestrictionSites = Fragment1.RestrictionSites
'        LigatedMacromolecule.Comments = Fragment1.Comments & vbCrLf & Fragment2.Comments

'        'Append Fragment2 sequence, adjusting for overhang
'        LigatedMacromolecule.Sequence &= Fragment2.Sequence.Substring(Math.Abs(Fragment2.StartOverhang))

'        'Reposition features and restriction sites on Fragment2
'        For Each Feature As Macromolecule.Feature In Fragment2.Features
'            Feature.StartBase += Fragment1.Size - Fragment2.StartOverhang
'            Feature.EndBase += Fragment1.Size - Fragment2.StartOverhang
'            LigatedMacromolecule.Features.Add(Feature)
'        Next

'        For Each Site As Macromolecule.RestrictionSite In Fragment2.RestrictionSites
'            Site.StartBase += Fragment1.Size - Fragment2.StartOverhang
'            LigatedMacromolecule.RestrictionSites.Add(Site)
'        Next

'        Return LigatedMacromolecule
'    End Function

'    Shared Function Circularize(ByVal LinearMacromolecule As Macromolecule) As Macromolecule
'        Dim CircularMacromolecule As New Macromolecule

'        CircularMacromolecule = LinearMacromolecule
'        CircularMacromolecule.IsCircular = True
'        CircularMacromolecule.Sequence = LinearMacromolecule.Sequence.Substring(0, LinearMacromolecule.Size - Math.Abs(LinearMacromolecule.EndOverhang))
'        CircularMacromolecule.StartOverhang = 0
'        CircularMacromolecule.EndOverhang = 0

'        'Check for features and restriction sites that overlap the end overhang and adjust accordingly
'        For Each Feature As Macromolecule.Feature In CircularMacromolecule.Features
'            'If it starts or ends in the overhang, but not both, it spans the origin
'            If Feature.StartBase > CircularMacromolecule.Size Xor Feature.EndBase > CircularMacromolecule.Size Then
'                Feature.SpansOrigin = True
'            End If

'            If Feature.StartBase > CircularMacromolecule.Size Then
'                Feature.StartBase -= CircularMacromolecule.Size
'            End If

'            If Feature.EndBase > CircularMacromolecule.Size Then
'                Feature.EndBase -= CircularMacromolecule.Size
'            End If
'        Next

'        For Each Site As Macromolecule.RestrictionSite In CircularMacromolecule.RestrictionSites
'            If Site.StartBase > CircularMacromolecule.Size Then
'                Site.StartBase -= CircularMacromolecule.Size
'            End If
'        Next

'        Return CircularMacromolecule
'    End Function

'    Shared Function Linearize(ByVal CircularMacromolecule As Macromolecule, ByVal Site As Macromolecule.RestrictionSite) As Macromolecule
'        Dim LinearizedMacromolecule As Macromolecule

'        'Append everything that occurs prior to the restriction site to the end of the molecule
'        Dim NewStartBase As Integer = Site.MinSite 'Need handling for if the cleavage site spans the origin.

'        LinearizedMacromolecule = SequenceTools.MoveOrigin(CircularMacromolecule, NewStartBase)

'        With LinearizedMacromolecule
'            .IsCircular = False
'            .Sequence &= LinearizedMacromolecule.Sequence.Remove(Site.OverhangSize)
'            .StartOverhang = Site.Overhang
'            .EndOverhang = Site.Overhang

'            Dim RemovedFeatures As New List(Of Macromolecule.Feature)

'            Dim AddedFeatures As New List(Of Macromolecule.Feature)

'            For Each Feature As Macromolecule.Feature In .Features.Where(Function(TestFeature As Macromolecule.Feature) TestFeature.SpansOrigin)
'                RemovedFeatures.Add(Feature)

'                'Split features spanning the origin into two
'                Dim HalfFeature1 As New Macromolecule.Feature
'                With HalfFeature1
'                    .Name = Feature.Name

'                    If Feature.StartBase < Feature.EndBase Then
'                        .StartBase = Feature.StartBase
'                        .EndBase = 1
'                    Else
'                        .StartBase = 1
'                        .EndBase = Feature.EndBase
'                    End If
'                    .Comments = Feature.Comments
'                    .Type = Feature.Type

'                    Select Case Feature.Completeness
'                        Case Macromolecule.Feature.CompletenessTypes.Full
'                            If Feature.StartBase < Feature.EndBase Then
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                            Else
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                            End If
'                        Case Macromolecule.Feature.CompletenessTypes.StartOnly
'                            If Feature.StartBase < Feature.EndBase Then
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                            Else
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                            End If
'                        Case Macromolecule.Feature.CompletenessTypes.StopOnly
'                            If Feature.StartBase < Feature.EndBase Then
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                            Else
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                            End If
'                        Case Macromolecule.Feature.CompletenessTypes.Internal
'                            .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                    End Select
'                End With
'                AddedFeatures.Add(HalfFeature1)

'                Dim HalfFeature2 As New Macromolecule.Feature
'                With HalfFeature2
'                    Dim OverhangCompensation As Integer

'                    .Name = Feature.Name

'                    'If the feature ends within the overhang, compensate
'                    If Min(Feature.StartBase, Feature.EndBase) <= Site.Overhang Then
'                        OverhangCompensation = Site.Overhang - Min(Feature.StartBase, Feature.EndBase)
'                    End If

'                    If Feature.StartBase < Feature.EndBase Then
'                        .StartBase = LinearizedMacromolecule.Size - OverhangCompensation
'                        .EndBase = Feature.EndBase
'                    Else
'                        .StartBase = Feature.StartBase
'                        .EndBase = LinearizedMacromolecule.Size - OverhangCompensation
'                    End If

'                    .Comments = Feature.Comments
'                    .Type = Feature.Type

'                    If OverhangCompensation > 0 Then

'                        Select Case Feature.Completeness
'                            Case Macromolecule.Feature.CompletenessTypes.Full
'                                If Feature.StartBase < Feature.EndBase Then
'                                    .Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                                Else
'                                    .Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                                End If
'                            Case Macromolecule.Feature.CompletenessTypes.StartOnly
'                                If Feature.StartBase < Feature.EndBase Then
'                                    .Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                                Else
'                                    .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                                End If
'                            Case Macromolecule.Feature.CompletenessTypes.StopOnly
'                                If Feature.StartBase < Feature.EndBase Then
'                                    .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                                Else
'                                    .Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                                End If
'                            Case Macromolecule.Feature.CompletenessTypes.Internal
'                                .Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                        End Select
'                    Else
'                        .Completeness = Feature.Completeness
'                    End If
'                End With
'                AddedFeatures.Add(HalfFeature2)

'            Next

'            For Each Feature As Macromolecule.Feature In RemovedFeatures
'                .Features.Remove(Feature)
'            Next

'            For Each Feature As Macromolecule.Feature In AddedFeatures
'                .Features.Add(Feature)
'            Next

'        End With

'        Return LinearizedMacromolecule
'    End Function

'End Class

'Friend Class PCR
'    Friend Shared Function PCR(ByVal Primer1 As Primer, ByVal Primer2 As Primer, ByVal Templates As ObservableCollection(Of Macromolecule)) As Macromolecule
'        Dim AmplifiedMacromolecule As New Macromolecule

'        'For single-template PCR:
'        '   1) Find the location on the template that is most identical to one of the primers
'        '   2) From the location of the first primer in the direction indicated by it, find the location of the second primer
'        '   3) Construct a sequence that uses the primer sequences instead of the template where there is a region of overlap
'        '   4) Allow for C or G to mismatch an A or T at the 3' position to allow for GC clamp
'        'For double-template PCR:
'        '   Treat the first and last segments (20 bp?) of each template as additional primers
'        'To start, this will only work with exact matches at the 3' end of primers

'        If Templates.Count = 1 Then
'            Dim Template As Macromolecule = Templates(0)
'            Dim Primer1Match As Tuple(Of Integer, Boolean()) = SequenceTools.FindSequenceLoose(Template.Sequence, Template.IsCircular, Primer1.Sequence)
'            Dim Primer2Match As Tuple(Of Integer, Boolean()) = SequenceTools.FindSequenceLoose(Template.Sequence, Template.IsCircular, Primer2.Sequence)

'            Dim PCRProduct As New System.Text.StringBuilder

'            Dim ForwardPrimer As New Primer
'            Dim ForwardPrimerMatchPosition As Integer
'            Dim ForwardPrimerMatchArray As Boolean()
'            Dim ForwardPrimer5PrimeMatchEndPosition As Integer
'            Dim ReversePrimer As New Primer
'            Dim ReversePrimerMatchPosition As Integer
'            Dim ReversePrimerMatchArray As Boolean()
'            Dim ReversePrimer5PrimeMatchEndPosition As Integer

'            If Primer1Match.Item1 > Primer2Match.Item1 Then
'                ForwardPrimer = Primer1
'                ForwardPrimerMatchPosition = Primer1Match.Item1
'                ForwardPrimerMatchArray = Primer1Match.Item2
'                ReversePrimer = Primer2
'                ReversePrimerMatchPosition = Primer2Match.Item1
'                ReversePrimerMatchArray = Primer2Match.Item2
'            ElseIf Primer1Match.Item1 < Primer2Match.Item1 Then
'                ForwardPrimer = Primer2
'                ForwardPrimerMatchPosition = Primer2Match.Item1
'                ForwardPrimerMatchArray = Primer2Match.Item2
'                ReversePrimer = Primer1
'                ReversePrimerMatchPosition = Primer1Match.Item1
'                ReversePrimerMatchArray = Primer1Match.Item2
'            End If

'            'Find a the earliest match in the primer. Allows up to 3 mismatches per gap
'            For Index As Integer = ForwardPrimer.Sequence.Length - 1 To 0
'                Dim MatchPosition As Integer = ForwardPrimerMatchPosition - (ForwardPrimer.Sequence.Length - Index - 1)
'                Dim MismatchCounter As Integer
'                Dim GapCounter As Integer

'                If ForwardPrimerMatchArray(Index) Then
'                    ForwardPrimer5PrimeMatchEndPosition = MatchPosition
'                Else
'                    MismatchCounter += 1
'                    If Index < ForwardPrimer.Sequence.Length - 1 AndAlso ForwardPrimerMatchArray(Index + 1) Then
'                        GapCounter += 1
'                    End If
'                End If

'                If MatchPosition / GapCounter > 3 Then Exit For
'            Next

'            'Find a the earliest match in the primer. Allows up to 3 mismatches per gap
'            For Index As Integer = 0 To ReversePrimer.Sequence.Length - 1
'                Dim MatchPosition As Integer = ReversePrimerMatchPosition + Index
'                Dim MismatchCounter As Integer
'                Dim GapCounter As Integer

'                If ReversePrimerMatchArray(Index) Then
'                    ReversePrimer5PrimeMatchEndPosition = MatchPosition
'                Else
'                    MismatchCounter += 1
'                    If Index > 0 AndAlso ReversePrimerMatchArray(Index - 1) Then
'                        GapCounter += 1
'                    End If
'                End If

'                If MatchPosition / GapCounter > 3 Then Exit For
'            Next

'            If ForwardPrimerMatchPosition < Abs(ReversePrimerMatchPosition) Then
'                PCRProduct.Append(ForwardPrimer.Sequence)
'                PCRProduct.Append(Template.Sequence.Substring(ForwardPrimerMatchPosition, Abs(ReversePrimerMatchPosition) - ForwardPrimerMatchPosition - 1))
'                PCRProduct.Append(SequenceTools.GetReverseComplement(ReversePrimer.Sequence))
'            ElseIf Template.IsCircular Then
'                If ForwardPrimerMatchPosition > Abs(ReversePrimerMatchPosition) Then
'                    SequenceTools.MoveOrigin(Template, ForwardPrimerMatchPosition)
'                    SequenceTools.Reposition(ReversePrimerMatchPosition, ForwardPrimerMatchPosition, Template.Size)
'                    ForwardPrimerMatchPosition = 0
'                    PCRProduct.Append(ForwardPrimer.Sequence)
'                    PCRProduct.Append(Template.Sequence.Substring(0, Abs(ReversePrimerMatchPosition) - 1))
'                    PCRProduct.Append(SequenceTools.GetReverseComplement(ReversePrimer.Sequence))
'                End If
'            End If

'            AmplifiedMacromolecule.Name = Template.Name & " (" & Primer1.Name & ", " & Primer2.Name & ")"
'            AmplifiedMacromolecule.Sequence = PCRProduct.ToString
'            AmplifiedMacromolecule.Comments = Template.Comments

'            'Can use primer matches (Item2) to determine how to keep features.**********************************
'            For Each Feature As Macromolecule.Feature In Template.Features
'                Dim NewFeature As Macromolecule.Feature = Feature.Clone

'                'Skip features that are completely outside the primer positions, though it unfortunately loses info 
'                If (Feature.StartBase < ForwardPrimer5PrimeMatchEndPosition AndAlso Feature.EndBase < ForwardPrimer5PrimeMatchEndPosition) OrElse
'                    (Feature.StartBase > ForwardPrimer5PrimeMatchEndPosition AndAlso Feature.EndBase > ForwardPrimer5PrimeMatchEndPosition) Then Continue For

'                If (Feature.StartBase < ForwardPrimer5PrimeMatchEndPosition AndAlso Feature.EndBase <= ReversePrimer5PrimeMatchEndPosition) OrElse
'                    (Feature.StartBase > ReversePrimer5PrimeMatchEndPosition AndAlso Feature.EndBase >= ForwardPrimer5PrimeMatchEndPosition) Then
'                    'Trim Start from feature
'                    Select Case Feature.Completeness
'                        Case Macromolecule.Feature.CompletenessTypes.Full
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                        Case Macromolecule.Feature.CompletenessTypes.Internal
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                        Case Macromolecule.Feature.CompletenessTypes.StartOnly
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                        Case Macromolecule.Feature.CompletenessTypes.StopOnly
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.StopOnly
'                    End Select
'                End If

'                If (Feature.StartBase >= ForwardPrimer5PrimeMatchEndPosition AndAlso Feature.EndBase > ReversePrimer5PrimeMatchEndPosition) OrElse
'                    (Feature.StartBase <= ReversePrimer5PrimeMatchEndPosition AndAlso Feature.EndBase < ForwardPrimer5PrimeMatchEndPosition) Then
'                    'Trim End from feature
'                    Select Case Feature.Completeness
'                        Case Macromolecule.Feature.CompletenessTypes.Full
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                        Case Macromolecule.Feature.CompletenessTypes.Internal
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                        Case Macromolecule.Feature.CompletenessTypes.StartOnly
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.StartOnly
'                        Case Macromolecule.Feature.CompletenessTypes.StopOnly
'                            Feature.Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                    End Select
'                End If

'                If (Feature.StartBase < ForwardPrimer5PrimeMatchEndPosition AndAlso Feature.EndBase > ReversePrimer5PrimeMatchEndPosition) OrElse
'                    (Feature.StartBase > ReversePrimer5PrimeMatchEndPosition AndAlso Feature.EndBase < ForwardPrimer5PrimeMatchEndPosition) Then
'                    'Trim Start and End from feature
'                    Feature.Completeness = Macromolecule.Feature.CompletenessTypes.Internal
'                End If

'                If (Feature.StartBase >= ForwardPrimer5PrimeMatchEndPosition AndAlso Feature.EndBase <= ReversePrimer5PrimeMatchEndPosition) OrElse
'                    (Feature.StartBase <= ReversePrimer5PrimeMatchEndPosition AndAlso Feature.EndBase >= ForwardPrimer5PrimeMatchEndPosition) Then
'                    'Retain full feature
'                    NewFeature.Completeness = Feature.Completeness
'                End If

'                NewFeature.Name = Feature.Name
'                NewFeature.Type = Feature.Type
'                NewFeature.StartBase = Feature.StartBase - ForwardPrimer5PrimeMatchEndPosition - 1
'                NewFeature.EndBase = Feature.EndBase - ForwardPrimer5PrimeMatchEndPosition - 1

'                AmplifiedMacromolecule.Features.Add(NewFeature)
'            Next

'            RestrictionAnalysis.FindRestrictionSites(AmplifiedMacromolecule)
'        End If

'        Return AmplifiedMacromolecule
'    End Function
'End Class

'***********Next step: Test new Routines to make sure they work correctly
'***********To do: change unnecessary queues in the entire program back to Lists.

Friend NotInheritable Class FileOperations
    Friend Shared Function OpenFile(FileName As String) As Queue(Of Macromolecule)
        Dim OpenFileStream As New IO.FileStream(FileName, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
        Dim OpenFileReader As New IO.StreamReader(OpenFileStream)

        Dim FileLinesQueue As New Queue(Of String)

        Do
            Dim NewLine As String = OpenFileReader.ReadLine.TrimEnd()
            FileLinesQueue.Enqueue(NewLine)
        Loop Until OpenFileReader.EndOfStream

        OpenFileReader.Close()

        Dim LoadedMacromolecules As New Queue(Of Macromolecule)
        LoadedMacromolecules = FileReaders.Read(FileLinesQueue)

        Return LoadedMacromolecules
    End Function

    Friend Shared Sub SaveFile(ByVal SavedMacromolecules As List(Of Macromolecule))
        Dim FileText As String = FileWriters.Write(SavedMacromolecules)
        Dim SaveFileWriter As IO.TextWriter = New IO.StreamWriter("pRS314-saved.txt")
        SaveFileWriter.Write(FileText)
        SaveFileWriter.Close()
    End Sub

    Friend Class FileWriters
        Friend Shared Function Write(MacromoleculesToSave As List(Of Macromolecule)) As String
            Dim FileToSave As String

            FileToSave = GenBankFlatFile.Write(MacromoleculesToSave)
            'FileToSave = FASTAFile.Write(MacromoleculesToSave)

            Return FileToSave
        End Function

        Private Class GenBankFlatFile
            Private Const FlatFileWidth As Integer = 79
            Private Const FlatFileColumn1 As Integer = 12
            Private Const FlatFileColumn2 As Integer = FlatFileWidth - FlatFileColumn1
            Private Const SequenceNumberColumn As Integer = 9
            Private Const FeatureTableColumn1 As Integer = 5
            Private Const FeatureTableColumn2 As Integer = 16
            Private Const FeatureTableColumn12 As Integer = FeatureTableColumn1 + FeatureTableColumn2
            Private Const FeatureTableColumn3 As Integer = FlatFileWidth - FeatureTableColumn1 - FeatureTableColumn2

            Shared Function Write(ByVal MacromoleculesToSave As List(Of Macromolecule)) As String
                'Writes an output file conforming to current NCBI-GenBank Flat File Release 185.0
                'ftp://ftp.ncbi.nih.gov/genbank/gbrel.txt

                Dim FileBuilder As New Text.StringBuilder

                For Each SavingMacromolecule As Macromolecule In MacromoleculesToSave
                    WriteLocus(SavingMacromolecule, FileBuilder)
                    WriteDefinition(SavingMacromolecule, FileBuilder)
                    WriteAccession(SavingMacromolecule, FileBuilder)
                    WriteVersion(SavingMacromolecule, FileBuilder)
                    WriteDBLink(SavingMacromolecule, FileBuilder)
                    WriteKeywords(SavingMacromolecule, FileBuilder)
                    WriteSourceOrganism(SavingMacromolecule, FileBuilder)
                    WriteReferences(SavingMacromolecule, FileBuilder)
                    WritePrimary(SavingMacromolecule, FileBuilder)
                    WriteComment(SavingMacromolecule, FileBuilder)
                    WriteFeatureTable(SavingMacromolecule, FileBuilder)
                    WriteOrigin(SavingMacromolecule, FileBuilder)
                    WriteSequence(SavingMacromolecule, FileBuilder)

                    'Build Terminator; mandatory
                    FileBuilder.AppendLine("//")
                Next

                Return FileBuilder.ToString
            End Function

            Private Shared Sub WriteLocus(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build LOCUS line; mandatory
                With SavingMacromolecule
                    Dim SequenceLength As String = CStr(.SequenceLength)
                    FileBuilder.Append("LOCUS       ")
                    'May want to ask for LocusName if one isn't existing? At some point...
                    FileBuilder.Append(.LocusName)
                    FileBuilder.Append(CChar(" "), 16 - .LocusName.Length)
                    FileBuilder.Append(" ")
                    FileBuilder.Append(CChar(" "), 11 - SequenceLength.Length)
                    FileBuilder.Append(SequenceLength)
                    If .Type = Macromolecule.MacromoleculeTypes.Protein Then
                        FileBuilder.Append(" aa ")
                    Else
                        FileBuilder.Append(" bp ")
                    End If
                    Select Case .Strandedness
                        Case Macromolecule.StrandednessTypes.None
                            FileBuilder.Append("   ")
                        Case Macromolecule.StrandednessTypes.DoubleStranded
                            FileBuilder.Append("ds-")
                        Case Macromolecule.StrandednessTypes.MixedStranded
                            FileBuilder.Append("ms-")
                        Case Macromolecule.StrandednessTypes.SingleStranded
                            FileBuilder.Append("ss-")
                    End Select
                    Select Case SavingMacromolecule.Type
                        Case Macromolecule.MacromoleculeTypes.NA
                            FileBuilder.Append("NA     ")
                            'Though I'm not sure of any examples of this...
                        Case Macromolecule.MacromoleculeTypes.RNA
                            Select Case .TypeModifier
                                Case Macromolecule.MacromoleculeTypeModifiers.Messenger
                                    FileBuilder.Append("m")
                                Case Macromolecule.MacromoleculeTypeModifiers.Ribosomal
                                    FileBuilder.Append("r")
                                Case Macromolecule.MacromoleculeTypeModifiers.Translating
                                    FileBuilder.Append("t")
                                Case Macromolecule.MacromoleculeTypeModifiers.Viral
                                    FileBuilder.Append("c")
                                Case Macromolecule.MacromoleculeTypeModifiers.Transcribed
                                    FileBuilder.Append("u")
                            End Select
                            FileBuilder.Append("RNA    ")
                            If .TypeModifier = Macromolecule.MacromoleculeTypeModifiers.None Then
                                FileBuilder.Append(" ")
                            End If
                        Case Macromolecule.MacromoleculeTypes.DNA
                            FileBuilder.Append("DNA     ")
                        Case Macromolecule.MacromoleculeTypes.Protein
                            FileBuilder.Append("        ")
                    End Select
                    If .IsCircular Then
                        FileBuilder.Append("circular ")
                    Else
                        FileBuilder.Append("linear   ")
                    End If
                    If .VolatileInformation IsNot Nothing AndAlso .VolatileInformation.Division <> "" Then
                        FileBuilder.Append(.VolatileInformation.Division)
                    Else
                        FileBuilder.Append("   ")
                    End If
                    FileBuilder.Append(" ")
                    FileBuilder.Append(String.Format("{0:dd-MMM-yyyy}", .DateModified).ToUpper)
                    FileBuilder.AppendLine()
                End With
            End Sub

            Private Shared Sub WriteDefinition(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build DEFINITION line; mandatory
                FileBuilder.Append("DEFINITION  ")
                Dim Definition As String = SavingMacromolecule.Description
                If Definition.Length > FlatFileColumn2 Then
                    Dim SplitDefinitionLines As List(Of String) = SplitStringByLength(Definition, FlatFileColumn2, {CChar(" "), CChar(";"), CChar(","), CChar(":"), CChar("-")})

                    FileBuilder.AppendLine(SplitDefinitionLines(0))

                    For LineCount As Integer = 1 To SplitDefinitionLines.Count - 1
                        FileBuilder.Append(CChar(" "), FlatFileColumn1)
                        FileBuilder.AppendLine(SplitDefinitionLines(LineCount))
                    Next

                Else
                    FileBuilder.AppendLine(Definition)
                End If
            End Sub

            Private Shared Sub WriteAccession(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build ACCESSION line; mandatory, but would be empty if modified
                With SavingMacromolecule
                    FileBuilder.Append("ACCESSION   ")
                    Dim AccesionBuilder As New Text.StringBuilder
                    If .VolatileInformation IsNot Nothing AndAlso .VolatileInformation.AccessionNumbers.Count > 0 Then
                        With .VolatileInformation
                            AccesionBuilder.Append(.AccessionNumbers(0))

                            If .AccessionNumbers.Count > 1 Then
                                For Counter = 1 To .AccessionNumbers.Count - 1
                                    Dim AccessionLettersIndex As Integer
                                    Dim AccessionNumberStartNumeric As Integer
                                    Dim AccessionNumberStartLetters As String
                                    Dim AccessionNumberStart As String = .AccessionNumbers(Counter)

                                    With AccessionNumberStart
                                        AccessionLettersIndex = .IndexOfAny({CChar("0"), CChar("1"), CChar("2"), CChar("3"), CChar("4"), CChar("5"), CChar("6"), CChar("7"), CChar("8"), CChar("9")})
                                        AccessionNumberStartNumeric = CInt(.Substring(AccessionLettersIndex))
                                        AccessionNumberStartLetters = .Substring(0, AccessionLettersIndex)
                                    End With

                                    Do
                                        Dim SecondCounter As Integer
                                        Dim AccessionNumber1, AccessionNumber2 As String
                                        Dim AccessionNumber1Numeric, AccessionNumber2Numeric As Integer
                                        Dim AccessionNumber1Letters, AccessionNumber2Letters As String

                                        If SecondCounter = 0 Then
                                            AccessionNumber1 = AccessionNumberStart
                                            AccessionNumber1Numeric = AccessionNumberStartNumeric
                                            AccessionNumber1Letters = AccessionNumberStartLetters
                                        Else
                                            AccessionNumber1 = AccessionNumber2
                                            AccessionNumber1Numeric = AccessionNumber2Numeric
                                            AccessionNumber1Letters = AccessionNumber2Letters
                                        End If

                                        If Counter + SecondCounter + 1 <= .AccessionNumbers.Count - 1 Then
                                            AccessionNumber2 = .AccessionNumbers(Counter + SecondCounter + 1)
                                            With AccessionNumber2
                                                AccessionLettersIndex = .IndexOfAny({CChar("0"), CChar("1"), CChar("2"), CChar("3"), CChar("4"), CChar("5"), CChar("6"), CChar("7"), CChar("8"), CChar("9")})
                                                AccessionNumber2Numeric = CInt(.Substring(AccessionLettersIndex))
                                                AccessionNumber2Letters = .Substring(0, AccessionLettersIndex)
                                            End With
                                        Else
                                            AccessionNumber2 = ""
                                            AccessionNumber2Numeric = 0
                                            AccessionNumber2Letters = ""
                                        End If

                                        'Check that the next number is next in the series. If it is, continue on. If it isn't, commit and move on.
                                        With AccesionBuilder
                                            If Not (AccessionNumber1Letters = AccessionNumber2Letters AndAlso AccessionNumber1Numeric + 1 = AccessionNumber2Numeric) Then
                                                .Append(" ")
                                                .Append(AccessionNumberStart)
                                                If AccessionNumber1Numeric <> AccessionNumberStartNumeric Then
                                                    .Append("-")
                                                    .Append(AccessionNumber1)
                                                End If
                                                Counter += SecondCounter
                                                Exit Do
                                            End If
                                        End With

                                        SecondCounter += 1
                                    Loop
                                Next
                            End If
                        End With

                        Dim AccessionNumbers As String = AccesionBuilder.ToString

                        If AccessionNumbers.Length > FlatFileColumn2 Then
                            Dim SplitAccessionLines As List(Of String) = SplitStringByLength(AccessionNumbers, FlatFileColumn2, {CChar(" ")})

                            FileBuilder.AppendLine(SplitAccessionLines(0))

                            For LineCount As Integer = 1 To SplitAccessionLines.Count - 1
                                FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                FileBuilder.AppendLine(SplitAccessionLines(LineCount))
                            Next

                        Else
                            FileBuilder.AppendLine(AccessionNumbers)
                        End If

                    Else
                        FileBuilder.AppendLine()
                    End If

                End With
            End Sub

            Private Shared Sub WriteVersion(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build VERSION line; mandatory, but would be empty if modified
                With SavingMacromolecule
                    FileBuilder.Append("VERSION     ")
                    If .VolatileInformation IsNot Nothing AndAlso .VolatileInformation.AccessionNumbers.Count > 0 AndAlso .VolatileInformation.Version > 0 Then
                        With .VolatileInformation
                            FileBuilder.Append(.AccessionNumbers(0))
                            FileBuilder.Append(".")
                            FileBuilder.Append(CStr(.Version))
                            If .GenInfoIdentifier <> 0 Then
                                FileBuilder.Append(" GI:")
                                FileBuilder.Append(CStr(.GenInfoIdentifier))
                            End If
                        End With
                    End If
                    FileBuilder.AppendLine()
                End With
            End Sub

            Private Shared Sub WriteDBLink(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build DBLINK line
                With SavingMacromolecule
                    If .VolatileInformation IsNot Nothing AndAlso .VolatileInformation.DatabaseCrossReferences.Count > 0 Then
                        FileBuilder.Append("DBLINK      ")
                        With .VolatileInformation
                            Dim Databases As New List(Of String)

                            'Collect databases
                            For Each DBLink As Tuple(Of String, String, String) In .DatabaseCrossReferences
                                If Not Databases.Contains(DBLink.Item1) Then Databases.Add(DBLink.Item1)
                            Next

                            For Each Database As String In Databases
                                Dim DBLinkBuilder As New Text.StringBuilder
                                Dim DatabaseCrossReferences As List(Of Tuple(Of String, String, String)) = .DatabaseCrossReferences
                                Dim SplitDBLinkLines As List(Of String)

                                With DBLinkBuilder
                                    .Append(Database)
                                    .Append(":")
                                    For Each DBLink As Tuple(Of String, String, String) In DatabaseCrossReferences
                                        If DBLink.Item1 = Database Then
                                            .Append(DBLink.Item2)
                                            .Append(",")
                                        End If
                                    Next
                                    .Remove(.Length - 1, 1)

                                    SplitDBLinkLines = SplitStringByLength(.ToString, FlatFileColumn2, {CChar(",")})
                                End With

                                Dim LineStartIndex As Integer

                                If Databases.IndexOf(Database) = 0 Then
                                    LineStartIndex = 1
                                Else
                                    FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                End If

                                FileBuilder.AppendLine(SplitDBLinkLines(0))

                                For LineCount As Integer = LineStartIndex To SplitDBLinkLines.Count - 1
                                    FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                    FileBuilder.AppendLine(SplitDBLinkLines(LineCount))
                                Next
                            Next
                        End With
                    End If
                End With
            End Sub

            Private Shared Sub WriteKeywords(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build KEYWORDS line; mandatory
                FileBuilder.Append("KEYWORDS    ")
                Dim KeywordsBuilder As New Text.StringBuilder
                Dim Keywords As String
                Dim KeywordsList As List(Of String) = SavingMacromolecule.Keywords

                With KeywordsBuilder
                    For Each Keyword As String In KeywordsList
                        .Append(Keyword)
                        .Append(";")
                    Next
                    If .Length > 0 Then .Remove(.Length - 1, 1)

                    .Append(".")

                    Keywords = .ToString
                End With

                If Keywords.Length > FlatFileColumn2 Then
                    Dim SplitKeywordsLines As List(Of String) = SplitStringByLength(Keywords, FlatFileColumn2, {CChar(";")})

                    FileBuilder.AppendLine(SplitKeywordsLines(0))

                    For LineCount As Integer = 1 To SplitKeywordsLines.Count - 1
                        FileBuilder.Append(CChar(" "), FlatFileColumn1)
                        FileBuilder.AppendLine(SplitKeywordsLines(LineCount))
                    Next

                Else
                    FileBuilder.AppendLine(Keywords)
                End If
            End Sub

            Private Shared Sub WriteSourceOrganism(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build SOURCE and ORGANISM lines; mandatory
                Dim TaxonomyBuilder As New Text.StringBuilder
                Dim TaxonomyList As List(Of String)
                Dim Taxonomy As String

                With SavingMacromolecule
                    FileBuilder.Append("SOURCE      ")
                    FileBuilder.AppendLine(.Source)
                    FileBuilder.Append("  ORGANISM  ")
                    TaxonomyList = .Taxonomy
                End With

                With TaxonomyBuilder
                    For Each TaxonomyItem As String In TaxonomyList
                        .Append(TaxonomyItem)
                        .Append("; ")
                    Next

                    If .Length > 0 Then
                        .Remove(.Length - 2, 2)
                    End If

                    .Append(".")

                    Taxonomy = TaxonomyBuilder.ToString

                End With

                If Taxonomy.Length > FlatFileColumn2 Then
                    Dim SplitKeywordsLines As List(Of String) = SplitStringByLength(Taxonomy, FlatFileColumn2, {CChar(";")})

                    FileBuilder.AppendLine(SplitKeywordsLines(0))

                    For LineCount As Integer = 1 To SplitKeywordsLines.Count - 1
                        FileBuilder.Append(CChar(" "), FlatFileColumn1)
                        FileBuilder.AppendLine(SplitKeywordsLines(LineCount))
                    Next

                Else
                    FileBuilder.AppendLine(Taxonomy)
                End If
            End Sub

            Private Shared Sub WriteComment(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build COMMENT lines
                Dim Comment As String = SavingMacromolecule.Comments

                If Comment <> "" Then
                    FileBuilder.Append("COMMENT     ")
                    Dim CommentLines As New List(Of String)

                    For Counter As Integer = 0 To Comment.Length - 1
                        Dim StartCounter As Integer
                        If Comment(Counter) = CChar(vbCrLf) Then
                            CommentLines.Add(Comment.Substring(StartCounter, Counter - StartCounter))
                            Counter += 1
                            StartCounter = Counter + 1
                        ElseIf Counter = Comment.Length - 1 Then
                            CommentLines.Add(Comment.Substring(StartCounter))
                        End If
                    Next

                    For Each CommentLine As String In CommentLines
                        If CommentLine.Length > FlatFileColumn2 Then
                            Dim SplitCommentLines As List(Of String) = SplitStringByLength(CommentLine, FlatFileColumn2, {CChar(" "), CChar(";"), CChar(","), CChar(":"), CChar("-")})

                            FileBuilder.AppendLine(SplitCommentLines(0))

                            For LineCount As Integer = 1 To SplitCommentLines.Count - 1
                                FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                FileBuilder.AppendLine(SplitCommentLines(LineCount))
                            Next

                        Else
                            If CommentLines.IndexOf(CommentLine) <> 0 Then
                                FileBuilder.Append(CChar(" "), FlatFileColumn1)
                            End If
                            FileBuilder.AppendLine(CommentLine)
                        End If
                    Next
                Else
                    FileBuilder.AppendLine()
                End If

            End Sub

            Private Shared Sub WriteFeatureTable(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build FEATURES lines
                FileBuilder.AppendLine("FEATURES             Location/Qualifiers")

                'Combine all feature collections first
                Dim CombinedFeatures As New Macromolecule.FeatureObservableCollection

                For Each SourceFeature As Macromolecule.Feature In SavingMacromolecule.SourceFeatures
                    CombinedFeatures.Add(SourceFeature)
                Next

                For Each InfoFeature As Macromolecule.Feature In SavingMacromolecule.InfoFeatures
                    CombinedFeatures.Add(InfoFeature)
                Next

                For Each Feature As Macromolecule.Feature In SavingMacromolecule.Features
                    CombinedFeatures.Add(Feature)
                Next

                'Should sort features by the first starting location...

                For Each Feature As Macromolecule.Feature In SavingMacromolecule.Features
                    With Feature
                        FileBuilder.Append("     ")
                        FileBuilder.Append(.Type)
                        FileBuilder.Append(CChar(" "), FeatureTableColumn2 - .Type.Length)

                        Dim Locations As New List(Of Macromolecule.Feature.Location)

                        For Each Location As Macromolecule.Feature.Location In .SequenceLocations
                            Locations.Add(Location)

                            With Location
                                If .SpansOrigin Then
                                    Dim SecondLocation As New Macromolecule.Feature.Location

                                    '900-100, spans; 100-900, spans, complement
                                    SecondLocation.EndBase = .EndBase
                                    .SpansOrigin = False
                                    If .StartBase > .EndBase Then
                                        SecondLocation.StartBase = 1
                                        .EndBase = SavingMacromolecule.SequenceLength
                                    Else
                                        SecondLocation.StartBase = SavingMacromolecule.SequenceLength
                                        .EndBase = 1
                                    End If

                                    Locations.Add(SecondLocation)
                                End If
                            End With

                        Next

                        Dim LocationString As String = EncodeFeatureLocation(Locations, .IsJoined)

                        If LocationString.Length > FeatureTableColumn3 Then
                            Dim SplitLocationLines As List(Of String) = SplitStringByLength(LocationString, FeatureTableColumn3, {CChar(",")})

                            FileBuilder.AppendLine(SplitLocationLines(0))

                            For LineCount As Integer = 1 To SplitLocationLines.Count - 1
                                FileBuilder.Append(CChar(" "), FeatureTableColumn12)
                                FileBuilder.AppendLine(SplitLocationLines(LineCount))
                            Next
                        Else
                            FileBuilder.AppendLine(LocationString)
                        End If

                        For Each QualifierItem As Macromolecule.Feature.Qualifier In .Qualifiers
                            With QualifierItem
                                FileBuilder.Append(CChar(" "), FeatureTableColumn12)

                                Dim QualifierLineBuilder As New Text.StringBuilder
                                QualifierLineBuilder.Append("/")
                                QualifierLineBuilder.Append(.Type)
                                QualifierLineBuilder.Append("=")

                                If IsNumeric(.Data) Then
                                    FileBuilder.Append(.Data)
                                Else
                                    QualifierLineBuilder.Append("""")
                                    QualifierLineBuilder.Append(.Data.Replace("""", """"""))
                                    QualifierLineBuilder.Append("""")

                                    Dim QualifierLine As String = QualifierLineBuilder.ToString

                                    If QualifierLine.Length > FeatureTableColumn3 Then
                                        Dim SplitQualifierLines As List(Of String) = SplitStringByLength(QualifierLine, FeatureTableColumn3, {CChar(" ")})

                                        FileBuilder.AppendLine(SplitQualifierLines(0))

                                        For LineCount As Integer = 1 To SplitQualifierLines.Count - 1
                                            FileBuilder.Append(CChar(" "), FeatureTableColumn12)
                                            FileBuilder.AppendLine(SplitQualifierLines(LineCount))
                                        Next

                                    Else
                                        FileBuilder.AppendLine(QualifierLine)
                                    End If
                                End If
                            End With
                        Next
                    End With
                Next
            End Sub

            Private Shared Function EncodeFeatureLocation(Locations As List(Of Macromolecule.Feature.Location), IsJoined As Boolean) As String
                'Basic idea: find strings of same-strandedness, join them and then complement them, and iterate.
                Dim LastLocationsIndex As Integer = Locations.Count - 1
                Dim LocationStringBuilder As New Text.StringBuilder
                With LocationStringBuilder

                    For Counter As Integer = 0 To LastLocationsIndex
                        Dim StartIndex As Integer = Counter
                        Dim EndIndex As Integer = StartIndex
                        Dim StartIsComplement As Boolean = Locations(Counter).IsComplement
                        Dim IsGrouped As Boolean
                        Dim IsGlobalGrouped As Boolean

                        'Find adjacent IsComplement status
                        If Counter < LastLocationsIndex Then
                            For NextCounter As Integer = Counter + 1 To LastLocationsIndex
                                If Locations(NextCounter).IsComplement <> StartIsComplement Then
                                    EndIndex = NextCounter - 1
                                    Counter = EndIndex
                                    Exit For
                                ElseIf NextCounter = LastLocationsIndex Then
                                    EndIndex = NextCounter
                                End If
                            Next

                            If StartIndex <> EndIndex Then
                                IsGrouped = True
                            Else
                                IsGrouped = False
                            End If

                            If Counter = 0 AndAlso EndIndex <> LastLocationsIndex Then
                                If IsJoined Then
                                    .Append("join(")
                                Else
                                    .Append("order(")
                                End If
                                IsGlobalGrouped = True
                            End If
                        Else
                            IsGrouped = False
                        End If

                        If StartIsComplement Then
                            Swap(StartIndex, EndIndex)
                            .Append("complement(")
                        End If

                        If IsGrouped Then
                            If IsJoined Then
                                .Append("join(")
                            Else
                                .Append("order(")
                            End If
                        End If

                        Dim IndexStep As Integer = 1

                        If StartIsComplement Then
                            IndexStep = -1
                        End If

                        For Index As Integer = StartIndex To EndIndex Step IndexStep
                            Dim StartBase As Integer
                            Dim StartIsBeyond As Boolean
                            Dim EndBase As Integer
                            Dim EndIsBeyond As Boolean
                            Dim IsRemote As Boolean
                            Dim RemoteAccession As String
                            Dim IsBetween As Boolean

                            With Locations(Index)
                                StartBase = .StartBase
                                StartIsBeyond = .StartIsBeyond
                                EndBase = .EndBase
                                EndIsBeyond = .EndIsBeyond
                                IsRemote = .IsRemote
                                IsBetween = .IsBetween
                                RemoteAccession = .RemoteAccession
                            End With

                            If StartBase <> EndBase Then
                                If IsRemote Then
                                    .Append(RemoteAccession)
                                    .Append(":")
                                End If

                                If StartIsComplement Then
                                    Swap(StartBase, EndBase)
                                    Swap(StartIsBeyond, EndIsBeyond)
                                End If

                                If StartIsBeyond Then .Append("<")
                                .Append(StartBase)
                                If IsBetween Then
                                    .Append("^")
                                Else
                                    .Append("..")
                                End If
                                If EndIsBeyond Then .Append(">")
                                .Append(EndBase)

                                If EndIndex <> StartIndex AndAlso Index <> EndIndex Then
                                    .Append(",")
                                End If
                            Else
                                .Append(StartBase)
                            End If
                        Next

                        If StartIsComplement Then .Append(")")
                        If IsGrouped Then .Append(")")

                        Counter = Math.Max(StartIndex, EndIndex)

                        If Counter < LastLocationsIndex Then
                            .Append(",")
                        ElseIf Counter = LastLocationsIndex AndAlso IsGlobalGrouped Then
                            .Append(")")
                        End If

                    Next

                End With

                Return LocationStringBuilder.ToString

            End Function

            Private Shared Sub WriteOrigin(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                'Build ORIGIN line; mandatory
                FileBuilder.Append("ORIGIN")
                With SavingMacromolecule
                    If .VolatileInformation IsNot Nothing AndAlso .VolatileInformation.OriginData <> "" Then
                        FileBuilder.Append("      ")
                        FileBuilder.Append(.VolatileInformation.OriginData)
                    End If
                End With
                FileBuilder.AppendLine()
            End Sub

            Private Shared Sub WriteSequence(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                With SavingMacromolecule.Sequence
                    Dim SequenceIndex As Integer
                    Do
                        Dim SequenceLineBuilder As New Text.StringBuilder
                        Dim SequencePosition As String = CStr(SequenceIndex + 1)
                        FileBuilder.Append(CChar(" "), SequenceNumberColumn - SequencePosition.Length - 1)
                        FileBuilder.Append(SequencePosition)

                        If SequenceIndex + 60 < .Length - 1 Then
                            For Counter As Integer = 0 To 5
                                FileBuilder.Append(" ")
                                FileBuilder.Append(.Substring(SequenceIndex + Counter * 10, 10).ToLower)
                            Next
                        Else
                            For Counter As Integer = 0 To 5
                                Dim SequenceIndexAdvance As Integer = SequenceIndex + (Counter + 1) * 10
                                FileBuilder.Append(" ")
                                If SequenceIndexAdvance < .Length - 1 Then
                                    FileBuilder.Append(.Substring(SequenceIndex + Counter * 10, 10).ToLower)
                                Else
                                    FileBuilder.Append(.Substring(SequenceIndex + Counter * 10).ToLower)
                                    Exit For
                                End If
                            Next
                        End If

                        FileBuilder.AppendLine()
                        SequenceIndex += 60

                    Loop Until SequenceIndex > .Length - 1
                End With


            End Sub

            Private Shared Sub WriteReferences(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                If SavingMacromolecule.References.Count > 0 Then
                    For Each Reference As KeyValuePair(Of Integer, Macromolecule.Reference) In SavingMacromolecule.References
                        FileBuilder.Append("REFERENCE   ")
                        With Reference
                            Dim ReferenceNumber As String = CStr(Reference.Key)
                            FileBuilder.Append(ReferenceNumber)
                            If ReferenceNumber.Length = 1 Then
                                FileBuilder.Append("  ")
                            Else
                                FileBuilder.Append(" ")
                            End If
                            FileBuilder.Append("(bases ")

                            With .Value
                                'Find the minimum and maximum sequence locations in case of multiple entries
                                Dim MinBase As Integer = SavingMacromolecule.SequenceLength
                                Dim MaxBase As Integer
                                For Each Location As Tuple(Of Integer, Integer) In .RelatedSequenceLocations
                                    If Location.Item1 < MinBase Then MinBase = Location.Item1
                                    If Location.Item2 > MaxBase Then MaxBase = Location.Item2
                                Next

                                FileBuilder.Append(CStr(MinBase))
                                FileBuilder.Append(" to ")
                                FileBuilder.Append(CStr(MaxBase))
                                FileBuilder.AppendLine(")")

                                If .Authors.Count > 0 Then
                                    FileBuilder.Append("  AUTHORS   ")
                                    Dim AuthorsBuilder As New Text.StringBuilder
                                    For Each Author As Macromolecule.Reference.Author In .Authors
                                        With Author
                                            AuthorsBuilder.Append(.LastName)
                                            AuthorsBuilder.Append(",")
                                            For Each Initial As String In .FirstInitials
                                                AuthorsBuilder.Append(Initial)
                                                AuthorsBuilder.Append(".")
                                            Next

                                            If .Suffix <> "" Then
                                                AuthorsBuilder.Append(" ")
                                                AuthorsBuilder.Append(.Suffix)
                                            End If

                                            AuthorsBuilder.Append(", ")
                                        End With
                                    Next
                                    AuthorsBuilder.Remove(AuthorsBuilder.Length - 2, 2)

                                    Dim Authors As String = AuthorsBuilder.ToString

                                    If Not Authors.EndsWith(".") Then Authors &= "."

                                    If Authors.Length > FlatFileColumn2 Then
                                        Dim SplitAuthorsLines As List(Of String) = SplitStringByLength(Authors, FlatFileColumn2, {CChar(" ")})

                                        FileBuilder.AppendLine(SplitAuthorsLines(0))

                                        For LineCount As Integer = 1 To SplitAuthorsLines.Count - 1
                                            FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                            FileBuilder.AppendLine(SplitAuthorsLines(LineCount))
                                        Next
                                    Else
                                        FileBuilder.AppendLine(Authors)
                                    End If
                                End If

                                If .Consortium <> "" Then
                                    FileBuilder.Append("  CONSRTM   ")

                                    If .Consortium.Length > FlatFileColumn2 Then
                                        Dim SplitConsortiumLines As List(Of String) = SplitStringByLength(.Consortium, FlatFileColumn2, {CChar(" ")})
                                        FileBuilder.AppendLine(SplitConsortiumLines(0))

                                        For LineCount As Integer = 1 To SplitConsortiumLines.Count - 1
                                            FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                            FileBuilder.AppendLine(SplitConsortiumLines(LineCount))
                                        Next
                                    Else
                                        FileBuilder.AppendLine(.Consortium)
                                    End If
                                End If

                                If .Title <> "" Then
                                    FileBuilder.Append("  TITLE     ")

                                    If .Title.Length > FlatFileColumn2 Then
                                        Dim SplitTitleLines As List(Of String) = SplitStringByLength(.Title, FlatFileColumn2, {CChar(" ")})
                                        FileBuilder.AppendLine(SplitTitleLines(0))

                                        For LineCount As Integer = 1 To SplitTitleLines.Count - 1
                                            FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                            FileBuilder.AppendLine(SplitTitleLines(LineCount))
                                        Next
                                    Else
                                        FileBuilder.AppendLine(.Title)
                                    End If
                                End If

                                If .Publication <> "" Then
                                    FileBuilder.Append("  JOURNAL   ")

                                    Dim JournalLines As New List(Of String)

                                    For Counter As Integer = 0 To .Publication.Length - 1
                                        Dim StartCounter As Integer
                                        If .Publication(Counter) = CChar(vbCrLf) Then
                                            Dim JournalLine As String = .Publication.Substring(StartCounter, Counter - StartCounter) & ";"
                                            JournalLines.Add(JournalLine)
                                            Counter += 1
                                            StartCounter = Counter + 1
                                        ElseIf Counter = .Publication.Length - 1 Then
                                            JournalLines.Add(.Publication.Substring(StartCounter))
                                        End If
                                    Next

                                    For Each JournalLine As String In JournalLines
                                        If JournalLine.Length > FlatFileColumn2 Then
                                            Dim SplitJournalLines As List(Of String) = SplitStringByLength(JournalLine, FlatFileColumn2, {CChar(" "), CChar(";"), CChar(","), CChar(":"), CChar("-")})

                                            FileBuilder.AppendLine(SplitJournalLines(0))

                                            For LineCount As Integer = 1 To SplitJournalLines.Count - 1
                                                FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                                FileBuilder.AppendLine(SplitJournalLines(LineCount))
                                            Next

                                        Else
                                            If JournalLines.IndexOf(JournalLine) <> 0 Then
                                                FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                            End If
                                            FileBuilder.AppendLine(JournalLine)
                                        End If
                                    Next

                                    If .ResourceCrossReferences.Count > 0 Then
                                        For Each CrossReference As Tuple(Of String, String) In .ResourceCrossReferences
                                            With CrossReference
                                                Select Case .Item1
                                                    Case "Medline"
                                                        FileBuilder.Append("  MEDLINE   ")
                                                    Case "PubMed"
                                                        FileBuilder.Append("   PUBMED   ")
                                                End Select

                                                FileBuilder.AppendLine(.Item2)
                                            End With
                                        Next
                                    End If

                                    If .Remark <> "" Then
                                        FileBuilder.Append("  REMARK    ")

                                        Dim RemarkLines As New List(Of String)

                                        For Counter As Integer = 0 To .Remark.Length - 1
                                            Dim StartCounter As Integer
                                            If .Remark(Counter) = CChar(vbCrLf) Then
                                                Dim RemarkLine As String = .Remark.Substring(StartCounter, Counter - StartCounter)
                                                RemarkLines.Add(RemarkLine)
                                                Counter += 1
                                                StartCounter = Counter + 1
                                            ElseIf Counter = .Remark.Length - 1 Then
                                                RemarkLines.Add(.Remark.Substring(StartCounter))
                                            End If
                                        Next

                                        For Each RemarkLine As String In RemarkLines
                                            If RemarkLine.Length > FlatFileColumn2 Then
                                                Dim SplitRemarkLines As List(Of String) = SplitStringByLength(RemarkLine, FlatFileColumn2, {CChar(" "), CChar(";"), CChar(","), CChar(":"), CChar("-")})

                                                FileBuilder.AppendLine(SplitRemarkLines(0))

                                                For LineCount As Integer = 1 To SplitRemarkLines.Count - 1
                                                    FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                                    FileBuilder.AppendLine(SplitRemarkLines(LineCount))
                                                Next

                                            Else
                                                If RemarkLines.IndexOf(RemarkLine) <> 0 Then
                                                    FileBuilder.Append(CChar(" "), FlatFileColumn1)
                                                End If
                                                FileBuilder.AppendLine(RemarkLine)
                                            End If
                                        Next
                                    End If

                                End If
                            End With

                        End With

                    Next
                End If
            End Sub

            Private Shared Sub WritePrimary(SavingMacromolecule As Macromolecule, FileBuilder As Text.StringBuilder)
                With SavingMacromolecule
                    If .VolatileInformation IsNot Nothing AndAlso .VolatileInformation.Assembly.Count > 0 Then
                        FileBuilder.AppendLine("PRIMARY     TPA_SPAN            PRIMARY_IDENTIFIER PRIMARY_SPAN        COMP")
                        For Each Primary As Tuple(Of Integer, Integer, String, Integer, Integer) In SavingMacromolecule.VolatileInformation.Assembly
                            FileBuilder.Append(CChar(" "), FlatFileColumn1)

                            With Primary
                                Dim TPASpan As String = .Item1 & "-" & .Item2
                                Dim RemoteAccessionNumber As String = .Item3
                                Dim PrimarySpan As String
                                Dim IsComplement As Boolean

                                If Primary.Item4 < Primary.Item5 Then
                                    PrimarySpan = .Item4 & "-" & .Item5
                                    IsComplement = False
                                Else
                                    PrimarySpan = .Item5 & "-" & .Item4
                                    IsComplement = True
                                End If

                                FileBuilder.Append(TPASpan)
                                FileBuilder.Append(CChar(" "), 20 - TPASpan.Length)
                                FileBuilder.Append(RemoteAccessionNumber)
                                FileBuilder.Append(CChar(" "), 19 - RemoteAccessionNumber.Length)
                                FileBuilder.Append(PrimarySpan)
                                If IsComplement Then
                                    FileBuilder.Append(CChar(" "), 20 - PrimarySpan.Length)
                                    FileBuilder.Append("c")
                                End If
                                FileBuilder.AppendLine()

                            End With
                        Next
                    End If
                End With
            End Sub
        End Class

        Private Class FASTAFile
            Shared Function Write(ByVal MacromoleculesToSave As List(Of Macromolecule)) As String
                'Might want to have the ability for the user to define the preferred format of the description line
                Dim FileBuilder As New Text.StringBuilder

                For Each SavingMacromolecule As Macromolecule In MacromoleculesToSave
                    With SavingMacromolecule
                        FileBuilder.Append(">")
                        FileBuilder.AppendLine(.Description)
                        FileBuilder.AppendLine(.Sequence)
                    End With
                Next

                Return FileBuilder.ToString
            End Function
        End Class

        Friend Shared Function SplitStringByLength(ByVal InputString As String, ByVal SplitLength As Integer, ByVal ParamArray SplitCharacters As Char()) As List(Of String)
            'Splits a string to the specified lengths, using the given characters as preferred wrap breaks
            Dim SplitString As New List(Of String)
            Dim SplitCharactersWithoutSpace(SplitCharacters.Length - 2) As Char
            Dim TestSpace As Boolean = SplitCharacters.Contains(CChar(" "))

            If TestSpace AndAlso SplitCharacters.Count > 1 Then
                Dim SpaceCharIndex As Integer = Array.IndexOf(SplitCharacters, CChar(" "))

                For Index As Integer = 0 To SplitCharacters.Length - 1
                    Dim TargetIndex As Integer
                    Select Case Index
                        Case Is < SpaceCharIndex
                            TargetIndex = Index
                        Case SpaceCharIndex
                            Continue For
                        Case Is > SpaceCharIndex
                            TargetIndex = Index - 1
                    End Select

                    SplitCharactersWithoutSpace(TargetIndex) = SplitCharacters(Index)
                Next
            End If

            Do
                If InputString.Length > SplitLength Then
                    Dim SplitIndex As Integer

                    If TestSpace Then
                        SplitIndex = InputString.Remove(SplitLength).LastIndexOfAny(SplitCharactersWithoutSpace)

                        For Counter As Integer = SplitLength - 1 To 0 Step -1
                            If InputString(Counter + 1) = " " AndAlso InputString(Counter) <> " " Then
                                If Counter > SplitIndex Then SplitIndex = Counter
                                Exit For
                            End If
                        Next

                    Else
                        SplitIndex = InputString.Remove(SplitLength + 1).LastIndexOfAny(SplitCharacters)
                    End If

                    'If the above doesn't find any matches, it will return "-1"
                    If SplitIndex <= 0 Then
                        SplitIndex = SplitLength - 1
                    End If

                    SplitString.Add(InputString.Remove(SplitIndex + 1))
                    InputString = InputString.Substring(SplitIndex + 1).TrimStart()

                Else
                    SplitString.Add(InputString)
                    Exit Do
                End If
            Loop

            Return SplitString

        End Function

    End Class

    Friend Class FileReaders

        Friend Shared Function Read(ByVal FileLines As Queue(Of String)) As Queue(Of Macromolecule)
            Dim LoadedMacromolecules As New Queue(Of Macromolecule)

            Try
                Select Case DetectFileType(FileLines)
                    Case FileTypes.GenBank_DDBJ
                        LoadedMacromolecules = GenBankFlatFile.Read(FileLines)
                    Case FileTypes.EMBL

                    Case FileTypes.FASTA
                        LoadedMacromolecules = FASTAFile.Read(FileLines)
                End Select
            Catch ex As Exception
                'Have the person send me the offending file and some information so I can fix it.
                MessageBox.Show("Error reading file")
            End Try

            Return LoadedMacromolecules
        End Function

        Private Class GenBankFlatFile
            Shared Function Read(ByVal FileLines As Queue(Of String)) As Queue(Of Macromolecule)
                'Intended to be flexible as to spacing, order, and existence of elements of the input file
                Dim LoadingMacromolecule As New Macromolecule 'This and the next line are only here to stop the designer from complaining about the object not set to an instance.
                LoadingMacromolecule.VolatileInformation = New Macromolecule.Volatile
                Dim LoadedMacromolecules As New Queue(Of Macromolecule)
                Do
                    Dim IsStartLine As Boolean = True

                    Select Case True
                        Case FileLines.Peek.StartsWith("LOCUS")
                            ReadLocus(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("DEFINITION")
                            ReadDefinition(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("ACCESSION")
                            ReadAccession(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("VERSION")
                            ReadVersion(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("NID")
                            ReadNID(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("PROJECT")
                            ReadProject(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("DBLINK")
                            ReadDBLink(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("PRIMARY")
                            FileLines.Dequeue()
                            ReadPrimary(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("KEYWORDS")
                            ReadKeywords(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("SEGMENT")
                            ReadSegment(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("SOURCE")
                            ReadSource(FileLines, LoadingMacromolecule)
                            ReadOrganism(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("REFERENCE")
                            ReadReference(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("COMMENT")
                            ReadComment(FileLines, LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("FEATURES")
                            FileLines.Dequeue()
                            ReadFeatureTable(FileLines, LoadingMacromolecule)
                            FindMolType(LoadingMacromolecule)
                        Case FileLines.Peek.StartsWith("BASE COUNT")
                            'Obselete GenBank line type. Provided by DDBJ files.
                            FileLines.Dequeue()
                        Case FileLines.Peek.StartsWith("ORIGIN")
                            ReadOrigin(FileLines, LoadingMacromolecule)
                            If FileLines.Peek.StartsWith("CONTIG") Then
                                ReadContig(FileLines, LoadingMacromolecule)
                            ElseIf Not FileLines.Peek.StartsWith("//") Then
                                ReadSequence(FileLines, LoadingMacromolecule)
                            End If
                        Case FileLines.Peek.StartsWith("//")
                            'Terminator
                            FileLines.Dequeue()
                            LoadedMacromolecules.Enqueue(LoadingMacromolecule)
                            LoadingMacromolecule = New Macromolecule
                        Case Else
                            Debug.Print("Unknown Line: " & FileLines.Dequeue)
                    End Select
                Loop While FileLines.Count > 0

                'If terminator was not included, last-read Macromolecule might not be added.
                With LoadedMacromolecules
                    If .Count = 0 OrElse ((.Last IsNot LoadingMacromolecule) AndAlso LoadingMacromolecule.Equals(New Macromolecule)) Then .Enqueue(LoadingMacromolecule)
                End With

                Return LoadedMacromolecules

            End Function

            Private Shared Sub ReadLocus(ByVal FileLines As Queue(Of String), ByVal LoadingMacromolecule As Macromolecule)
                With LoadingMacromolecule
                    Dim MissingFieldAdjustment As Integer = 0
                    Dim LocusLineElements() As String = FileLines.Dequeue.Substring(5).TrimStart().Split({CChar(" ")}, StringSplitOptions.RemoveEmptyEntries)
                    .LocusName = LocusLineElements(0)
                    .SequenceLength = CInt(LocusLineElements(1))

                    If LocusLineElements(2) = "aa" Then
                        .Type = Macromolecule.MacromoleculeTypes.Protein
                    Else
                        Dim MoleculeTypeElement As String
                        If LocusLineElements(3).Contains("-") Then
                            Dim StrandednessTypeElement As String = LocusLineElements(3).Substring(0, LocusLineElements(3).IndexOf(CChar("-")))
                            MoleculeTypeElement = LocusLineElements(3).Substring(LocusLineElements(3).IndexOf(CChar("-")) + 1)
                            Select Case StrandednessTypeElement
                                Case "ss"
                                    .Strandedness = Macromolecule.StrandednessTypes.SingleStranded
                                Case "ds"
                                    .Strandedness = Macromolecule.StrandednessTypes.DoubleStranded
                                Case "ms"
                                    .Strandedness = Macromolecule.StrandednessTypes.MixedStranded
                            End Select
                        Else
                            MoleculeTypeElement = LocusLineElements(3)
                            .Strandedness = Macromolecule.StrandednessTypes.None
                        End If
                        Select Case MoleculeTypeElement
                            'The "source" feature's "mol_type" is more specific and modifies this first result.
                            Case "NA"
                                .Type = Macromolecule.MacromoleculeTypes.NA
                            Case "DNA"
                                .Type = Macromolecule.MacromoleculeTypes.DNA
                            Case "RNA"
                                .Type = Macromolecule.MacromoleculeTypes.RNA
                            Case "tRNA"
                                .Type = Macromolecule.MacromoleculeTypes.RNA
                                .TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Translating
                            Case "rRNA"
                                .Type = Macromolecule.MacromoleculeTypes.RNA
                                .TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Ribosomal
                            Case "mRNA"
                                .Type = Macromolecule.MacromoleculeTypes.RNA
                                .TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Messenger
                            Case "uRNA"
                                .Type = Macromolecule.MacromoleculeTypes.RNA
                                .TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Transcribed
                            Case "cRNA"
                                .Type = Macromolecule.MacromoleculeTypes.RNA
                                .TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Viral
                            Case Else
                                Debug.Print("No Macromolecule Type found!")
                        End Select
                    End If

                    Select Case LocusLineElements(4)
                        Case "circular"
                            .IsCircular = True
                        Case "linear"
                            .IsCircular = False
                        Case Else
                            MissingFieldAdjustment += 1

                            Select Case MessageBox.Show("This sequence file does not specify whether it is linear or circular. Is this a circular sequence?", "Circular?", MessageBoxButton.YesNo, MessageBoxImage.Question, MessageBoxResult.No)
                                Case MessageBoxResult.Yes
                                    .IsCircular = True
                                Case MessageBoxResult.No
                                    .IsCircular = False
                                Case Else
                                    Debug.Print("Circularity not defined!")
                            End Select
                    End Select

                    .VolatileInformation = New Macromolecule.Volatile

                    .VolatileInformation.Division = LocusLineElements(5 - MissingFieldAdjustment)

                    .DateModified = CDate(LocusLineElements(6 - MissingFieldAdjustment))

                End With

            End Sub

            Private Shared Sub ReadDefinition(ByVal FileLines As Queue(Of String), ByVal LoadingMacromolecule As Macromolecule)
                Dim DefinitionBuilder As New Text.StringBuilder
                Dim IsStartLine As Boolean = True

                Do
                    Dim DefinitionLine As String = FileLines.Dequeue

                    With DefinitionLine
                        If IsStartLine Then
                            If .Length > 10 Then
                                DefinitionLine = .Substring(10)
                            Else
                                Exit Do
                            End If
                        Else
                            DefinitionBuilder.Append(" ")
                        End If
                    End With

                    DefinitionBuilder.Append(DefinitionLine.TrimStart())

                    IsStartLine = False
                Loop While FileLines.Peek.StartsWith(" ")

                LoadingMacromolecule.Description = DefinitionBuilder.ToString
            End Sub

            Private Shared Sub ReadAccession(ByVal FileLines As Queue(Of String), ByVal LoadingMacromolecule As Macromolecule)
                Dim AccessionLine As String = FileLines.Dequeue
                Dim IsStartLine As Boolean = True

                Do
                    Dim AccessionNumberElements As String()

                    With AccessionLine
                        If IsStartLine Then
                            If .Length > 9 Then
                                AccessionLine = .Substring(9)
                            Else
                                Exit Do
                            End If
                        End If
                    End With

                    AccessionNumberElements = AccessionLine.TrimStart().Split({CChar(" ")})

                    With LoadingMacromolecule

                        If .VolatileInformation Is Nothing Then .VolatileInformation = New Macromolecule.Volatile

                        With .VolatileInformation
                            For Each AccessionNumberElement As String In AccessionNumberElements

                                If AccessionNumberElement.Contains("-") Then
                                    Dim AccessionLettersNumber As Integer
                                    Dim AccessionStart As Integer
                                    Dim AccessionEnd As Integer
                                    Dim AccessionLetters As String

                                    With AccessionNumberElement
                                        Dim HyphenIndex As Integer = .IndexOf({CChar("-")})
                                        AccessionLettersNumber = .IndexOfAny({CChar("0"), CChar("1"), CChar("2"), CChar("3"), CChar("4"), CChar("5"), CChar("6"), CChar("7"), CChar("8"), CChar("9")})
                                        AccessionStart = CInt(.Substring(AccessionLettersNumber, HyphenIndex - AccessionLettersNumber))
                                        AccessionEnd = CInt(.Substring(HyphenIndex + 1 + AccessionLettersNumber))
                                        AccessionLetters = .Substring(0, AccessionLettersNumber)
                                    End With

                                    For Counter As Integer = 0 To AccessionEnd - AccessionStart
                                        .AccessionNumbers.Add(AccessionLetters & CStr(AccessionStart + Counter))
                                    Next
                                Else
                                    .AccessionNumbers.Add(AccessionNumberElement)
                                End If
                            Next
                        End With
                    End With
                    IsStartLine = False
                Loop While FileLines.Peek.StartsWith(" ")
            End Sub

            Private Shared Sub ReadVersion(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim VersionLine As String = FileLines.Dequeue
                If VersionLine.Length > 7 Then
                    VersionLine = VersionLine.Substring(VersionLine.IndexOf(CChar(".")) + 1)
                    With LoadingMacromolecule
                        If .VolatileInformation Is Nothing Then LoadingMacromolecule.VolatileInformation = New Macromolecule.Volatile
                        With .VolatileInformation
                            If VersionLine.Contains("GI:") Then
                                .Version = CInt(VersionLine.Substring(0, VersionLine.IndexOf(CChar(" "))))
                                .GenInfoIdentifier = CInt(VersionLine.Substring(VersionLine.IndexOf(CChar(":")) + 1))
                            Else
                                .Version = CInt(VersionLine)
                            End If
                        End With
                    End With
                End If
            End Sub

            Private Shared Sub ReadNID(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                'Deprecated keyword, alternate presentation for GI found in Version
                Dim NIDLine As String = FileLines.Dequeue.Substring(3).TrimStart()
                With LoadingMacromolecule
                    If .VolatileInformation Is Nothing Then .VolatileInformation = New Macromolecule.Volatile
                    .VolatileInformation.GenInfoIdentifier = CInt(NIDLine)
                End With
            End Sub

            Private Shared Sub ReadProject(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                'Deprecated keyword, contains information now found in DBLINK
                Dim IsStartLine As Boolean = True
                Do
                    Dim ProjectLine As String = FileLines.Dequeue
                    Dim ProjectIDs As String()

                    With ProjectLine
                        If IsStartLine Then
                            If ProjectLine.Length > 7 Then
                                ProjectLine = .Substring(7)
                            Else
                                Exit Do
                            End If
                        End If
                    End With

                    ProjectLine = ProjectLine.TrimStart()
                    ProjectIDs = ProjectLine.Substring(ProjectLine.IndexOf(CChar(":")) + 1).Split({CChar(",")})

                    Dim ProjectName As String
                    Dim LinkWraps As Boolean

                    With LoadingMacromolecule
                        If .VolatileInformation Is Nothing Then .VolatileInformation = New Macromolecule.Volatile

                        With .VolatileInformation.DatabaseCrossReferences

                            If ProjectLine.Contains(":") Then
                                ProjectName = ProjectLine.Remove(ProjectLine.IndexOf(CChar(":")))
                                If ProjectName = "GenomeProject" Then ProjectName = "Project"
                            Else

                                ProjectName = .Last.Item1
                                If LinkWraps Then
                                    Dim PreviousDBCrossLink As Tuple(Of String, String, String) = .Last
                                    Dim PreviousAccession As String = PreviousDBCrossLink.Item2
                                    .Remove(PreviousDBCrossLink)
                                    .Add(New Tuple(Of String, String, String)(ProjectName, PreviousAccession & ProjectIDs(0), ""))
                                End If
                            End If

                            If ProjectLine.Last <> CChar(",") Then
                                LinkWraps = True
                            End If

                            For Each ID As String In ProjectIDs
                                If LinkWraps AndAlso Array.IndexOf(ProjectIDs, ID) = 0 Then Continue For
                                .Add(New Tuple(Of String, String, String)(ProjectName, ID, ""))
                            Next

                        End With
                    End With

                    IsStartLine = False
                Loop While FileLines.Peek.StartsWith(" ")
            End Sub

            Private Shared Sub ReadDBLink(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim IsStartLine As Boolean = True

                Do
                    Dim DBLinkLine As String = FileLines.Dequeue
                    Dim DBLinkIDs As String()

                    With DBLinkLine
                        If IsStartLine Then
                            If .Length > 6 Then
                                DBLinkLine = .Substring(6)
                            Else
                                Exit Do
                            End If
                        End If
                    End With

                    DBLinkLine = DBLinkLine.TrimStart()
                    DBLinkIDs = DBLinkLine.Substring(DBLinkLine.IndexOf(CChar(":")) + 1).Split({CChar(",")})

                    Dim DBLinkName As String
                    Dim LinkWraps As Boolean

                    With LoadingMacromolecule
                        If .VolatileInformation Is Nothing Then .VolatileInformation = New Macromolecule.Volatile

                        With .VolatileInformation.DatabaseCrossReferences

                            If DBLinkLine.Contains(":") Then
                                DBLinkName = DBLinkLine.Remove(DBLinkLine.IndexOf(CChar(":")))
                            Else

                                DBLinkName = .Last.Item1
                                If LinkWraps Then
                                    Dim PreviousDBCrossLink As Tuple(Of String, String, String) = .Last
                                    Dim PreviousAccession As String = PreviousDBCrossLink.Item2
                                    .Remove(PreviousDBCrossLink)
                                    .Add(New Tuple(Of String, String, String)(DBLinkName, PreviousAccession & DBLinkIDs(0), ""))
                                End If

                            End If

                            'Check if the last character of the line doesn't end with a comma. If it does, next time it will append the beginning number to the end of the last one
                            If DBLinkLine.Last <> CChar(",") Then
                                LinkWraps = True
                            End If

                            For Each ID As String In DBLinkIDs
                                If LinkWraps AndAlso Array.IndexOf(DBLinkIDs, ID) = 0 Then Continue For
                                .Add(New Tuple(Of String, String, String)(DBLinkName, ID, ""))
                            Next

                        End With
                    End With

                    IsStartLine = False
                Loop While FileLines.Peek.StartsWith(" ")

            End Sub

            Private Shared Sub ReadPrimary(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Do
                    Dim PrimaryElements() As String = FileLines.Dequeue.TrimStart().Split({CChar(" "), CChar("-")}, StringSplitOptions.RemoveEmptyEntries)

                    Dim LocalBaseStart As Integer = CInt(PrimaryElements(0))
                    Dim LocalBaseEnd As Integer = CInt(PrimaryElements(1))
                    Dim RemoteAccessionNumber As String = PrimaryElements(2)
                    Dim RemoteBaseStart As Integer = CInt(PrimaryElements(3))
                    Dim RemoteBaseEnd As Integer = CInt(PrimaryElements(4))
                    Dim IsComplement As Boolean
                    If PrimaryElements.Count > 5 Then
                        IsComplement = PrimaryElements(5) = "c"
                    End If

                    If IsComplement Then
                        Swap(RemoteBaseStart, RemoteBaseEnd)
                    End If

                    Dim Primary As New Tuple(Of Integer, Integer, String, Integer, Integer)(LocalBaseStart, LocalBaseEnd, RemoteAccessionNumber, RemoteBaseStart, RemoteBaseEnd)

                    LoadingMacromolecule.VolatileInformation.Assembly.Add(Primary)

                Loop While FileLines.Peek.StartsWith(" ")
            End Sub

            Private Shared Sub ReadKeywords(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim IsStartLine As Boolean = True
                Do
                    Dim KeywordsLine As String = FileLines.Dequeue
                    Dim Keywords As String()

                    With KeywordsLine
                        If IsStartLine Then
                            If .Length > 8 Then
                                KeywordsLine = .Substring(8)
                            Else
                                Exit Do
                            End If
                        End If
                    End With

                    Keywords = KeywordsLine.TrimStart().TrimEnd({CChar(".")}).Split({CChar(";")})

                    For Each Keyword As String In Keywords
                        LoadingMacromolecule.Keywords.Add(Keyword)
                    Next

                    IsStartLine = False
                Loop While FileLines.Peek.StartsWith(" ")
            End Sub

            Private Shared Sub ReadSegment(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                '                Dim SegmentLine As String = FileLines.Dequeue.Substring(7).TrimStart()
                '                Dim SegmentElements As String() = SegmentLine.Split({CChar(" ")})
                '                DecodedFile.Segment = New Tuple(Of Integer, Integer)(CInt(SegmentElements(0)), CInt(SegmentElements(2)))

                'Should present dialog asking for other segments to be loaded. Should also check first that other segments are not in the same file. This section will otherwise be discarded.
                Throw New NotImplementedException
            End Sub

            Private Shared Sub ReadSource(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim IsStartLine As Boolean = True
                Dim SourceBuilder As New Text.StringBuilder

                Do
                    Dim SourceLine As String = FileLines.Dequeue

                    With SourceLine
                        If IsStartLine Then
                            If .Length > 6 Then
                                SourceLine = .Substring(6)
                            Else
                                Exit Do
                            End If
                        Else
                            SourceBuilder.Append(" ")
                        End If
                    End With

                    SourceBuilder.Append(SourceLine.TrimStart())

                    IsStartLine = False

                    'If SourceLine.EndsWith(".") OrElse FileLines.Peek.StartsWith("  O") Then
                    '    SetEnded = True
                    'End If
                Loop While FileLines.Peek.StartsWith("   ")

                LoadingMacromolecule.Source = SourceBuilder.ToString
            End Sub

            Private Shared Sub ReadOrganism(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim IsStartLine As Boolean = True
                Dim GenusSpecies(1) As String
                Do
                    Dim OrganismLine As String = FileLines.Dequeue
                    Dim OrganismElements As String()

                    With OrganismLine
                        If IsStartLine Then
                            If .Length > 10 Then
                                OrganismLine = .Substring(10)
                            Else
                                Exit Do
                            End If
                        End If

                    End With

                    OrganismLine = OrganismLine.TrimStart()

                    If IsStartLine AndAlso Not OrganismLine.Contains(";") Then
                        Dim GenusSpeciesElements As String() = OrganismLine.Split({CChar(" ")})
                        GenusSpecies(0) = GenusSpeciesElements(0)
                        For Index As Integer = 1 To GenusSpeciesElements.Count - 1
                            If Index > 1 Then GenusSpecies(1) &= " "
                            GenusSpecies(1) &= GenusSpeciesElements(Index)
                        Next

                        GenusSpecies(1).TrimEnd({CChar(".")})
                    Else
                        If OrganismLine.EndsWith(".") Then
                            OrganismLine = OrganismLine.TrimEnd({CChar(".")})
                        End If

                        OrganismElements = OrganismLine.Split({CChar(";")}, StringSplitOptions.RemoveEmptyEntries)

                        For Each OrganismElement As String In OrganismElements
                            LoadingMacromolecule.Taxonomy.Add(OrganismElement.TrimStart())
                        Next
                    End If

                    IsStartLine = False

                Loop While FileLines.Peek.StartsWith(" ")

                With LoadingMacromolecule.Taxonomy
                    If GenusSpecies(0) <> "" AndAlso GenusSpecies(1) <> "" AndAlso .Count > 0 Then
                        If .Last <> GenusSpecies(0) Then .Add(GenusSpecies(0))
                        .Add(GenusSpecies(1))
                    End If
                End With
            End Sub

            Private Shared Sub ReadComment(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim CommentBuilder As New Text.StringBuilder
                Dim IsStartLine As Boolean = True

                Do
                    Dim CommentLine As String = FileLines.Dequeue

                    With CommentLine
                        If IsStartLine Then
                            If .Length > 7 Then
                                CommentLine = .Substring(7)
                            Else
                                Exit Do
                            End If
                        Else
                            CommentBuilder.Append(vbCrLf)
                        End If
                    End With

                    CommentBuilder.Append(CommentLine.TrimStart())

                    IsStartLine = False
                Loop While FileLines.Peek.StartsWith(" ") OrElse FileLines.Peek = ""

                LoadingMacromolecule.Comments = CommentBuilder.ToString
            End Sub

            Private Shared Sub ReadFeatureTable(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim IsStartLine As Boolean = True
                Dim FeaturesTemp As New List(Of Macromolecule.Feature)
                Dim InfoFeaturesTemp As New List(Of Macromolecule.Feature)
                Dim SourceFeaturesTemp As New List(Of Macromolecule.Feature)

                Do
                    Dim Feature As New Macromolecule.Feature

                    Dim FeatureKeyLine As String = FileLines.Dequeue.TrimStart()
                    Dim FeatureKey As String

                    With FeatureKeyLine
                        FeatureKey = .Substring(0, .IndexOf(CChar(" ")))
                        Feature.Type = FeatureKey
                        Dim FeatureKeyLineBuilder As New Text.StringBuilder
                        FeatureKeyLineBuilder.Append(.Substring(FeatureKey.Length).TrimStart())
                        Do Until FileLines.Peek.StartsWith("                     /") OrElse Not FileLines.Peek.StartsWith("      ")
                            FeatureKeyLineBuilder.Append(FileLines.Dequeue.TrimStart())
                        Loop
                        FeatureKeyLine = FeatureKeyLineBuilder.ToString
                    End With

                    'Unwrap feature locations
                    Dim FeatureLocationsPackage As Tuple(Of List(Of Macromolecule.Feature.Location), Boolean) = DecodeFeatureLocation(FeatureKeyLine)

                    With Feature
                        .SequenceLocations = FeatureLocationsPackage.Item1
                        .IsJoined = FeatureLocationsPackage.Item2

                        For Each Location As Macromolecule.Feature.Location In .SequenceLocations
                            If Not .HasRemoteParts AndAlso Location.RemoteAccession <> "" Then .HasRemoteParts = True
                        Next

                        'Detect an origin-spanning feature
                        If LoadingMacromolecule.IsCircular AndAlso .IsJoined AndAlso .SequenceLocations.Count > 1 Then
                            For Index As Integer = 0 To .SequenceLocations.Count - 2
                                Dim Location1 As Macromolecule.Feature.Location = .SequenceLocations(Index)
                                Dim Location2 As Macromolecule.Feature.Location = .SequenceLocations(Index + 1)

                                'Possibilities for a 200-bp feature on a 1000-bp plasmid:
                                '900-1000, 1-100
                                '100-1,1000-900

                                If Location1.EndBase = LoadingMacromolecule.SequenceLength AndAlso Location2.StartBase = 1 Then
                                    Location1.EndBase = Location2.EndBase
                                ElseIf Location2.StartBase = LoadingMacromolecule.SequenceLength AndAlso Location1.EndBase = 1 Then
                                    Location1.EndBase = Location2.EndBase
                                End If

                                If Location1.EndBase = Location2.EndBase Then
                                    Location1.SpansOrigin = True
                                    .SequenceLocations.Remove(Location2)
                                    .ParentSequenceLength = LoadingMacromolecule.SequenceLength
                                    If .SequenceLocations.Count = 1 Then .IsJoined = False
                                End If
                            Next
                        End If

                        Do While FileLines.Peek.StartsWith("      ")
                            Dim QualifierLine As String = FileLines.Dequeue
                            Dim QualifierType As String = ""
                            Dim QualifierData As String = ""
                            Dim QualifierDataBuilder As New Text.StringBuilder
                            IsStartLine = True

                            QualifierLine = QualifierLine.TrimStart()

                            If QualifierLine.Contains("=") Then
                                QualifierType = QualifierLine.Substring(1, QualifierLine.IndexOf(CChar("=")) - 1)
                                QualifierLine = QualifierLine.Substring(QualifierLine.IndexOf(CChar("=")) + 1)

                                Do
                                    If Not IsStartLine Then
                                        QualifierLine = FileLines.Dequeue.TrimStart()
                                        QualifierDataBuilder.Append(" ")
                                    End If

                                    QualifierDataBuilder.Append(QualifierLine)

                                    IsStartLine = False
                                Loop Until FileLines.Peek.TrimStart().StartsWith("/") OrElse Not FileLines.Peek.StartsWith("       ")

                                QualifierData = QualifierDataBuilder.ToString

                                If QualifierData.StartsWith("""") AndAlso QualifierData.EndsWith("""") Then
                                    QualifierData = QualifierData.Substring(1, QualifierData.Length - 2)
                                End If
                            Else
                                QualifierData = ""
                            End If

                            Dim QualifierItem As New Macromolecule.Feature.Qualifier
                            QualifierItem.Type = QualifierType
                            QualifierItem.Data = QualifierData

                            .Qualifiers.Add(QualifierItem)

                        Loop
                    End With

                    'Store the source and information-type features in separate collections
                    If My.Settings.SourceFeatureKey = Feature.Type Then
                        SourceFeaturesTemp.Add(Feature)
                    ElseIf My.Settings.InfoFeatureKeys.Contains(Feature.Type) Then
                        InfoFeaturesTemp.Add(Feature)
                    Else
                        FeaturesTemp.Add(Feature)
                    End If

                Loop While FileLines.Peek.StartsWith(" ")

                LoadingMacromolecule.Features.ParentSequenceLength = LoadingMacromolecule.SequenceLength
                LoadingMacromolecule.Features.AddRange(FeaturesTemp)
                LoadingMacromolecule.InfoFeatures.ParentSequenceLength = LoadingMacromolecule.SequenceLength
                LoadingMacromolecule.InfoFeatures.AddRange(InfoFeaturesTemp)
                LoadingMacromolecule.SourceFeatures.ParentSequenceLength = LoadingMacromolecule.SequenceLength
                LoadingMacromolecule.SourceFeatures.AddRange(SourceFeaturesTemp)
            End Sub

            Private Shared Sub FindMolType(LoadingMacromolecule As Macromolecule)
                'Scan feature qualifiers for the first "mol_type" line. All "mol_type" entries in a file are required to be the same
                For Each Feature As Macromolecule.Feature In LoadingMacromolecule.Features
                    For Each QualifierItem As Macromolecule.Feature.Qualifier In Feature.Qualifiers
                        With QualifierItem
                            If .Type = "mol_type" Then
                                Dim TypeModifier As Macromolecule.MacromoleculeTypeModifiers
                                Select Case .Data.Remove(.Data.Length - 3) 'Get the prefix
                                    Case "genomic "
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Genomic
                                    Case "m"
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Messenger
                                    Case "t"
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Translating
                                    Case "r"
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Ribosomal
                                    Case "other "
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Other
                                    Case "transcribed "
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Transcribed
                                    Case "viral "
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Viral
                                    Case "unassigned "
                                        TypeModifier = Macromolecule.MacromoleculeTypeModifiers.Unassigned
                                End Select
                            End If
                        End With
                    Next
                Next
            End Sub

            Private Shared Sub ReadOrigin(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim OriginLine As String = FileLines.Dequeue()

                If OriginLine.Length > 6 Then
                    OriginLine = OriginLine.Substring(6)

                    With LoadingMacromolecule
                        If .VolatileInformation Is Nothing Then .VolatileInformation = New Macromolecule.Volatile
                        .VolatileInformation.OriginData = OriginLine.TrimEnd({CChar(".")})
                    End With
                End If
            End Sub

            Private Shared Sub ReadSequence(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim SequenceBuilder As New Text.StringBuilder

                Do
                    Dim SequenceLine As String = FileLines.Dequeue
                    SequenceLine = ExtractNucleicAcidSequence(SequenceLine)
                    SequenceLine = SequenceLine.ToUpper() 'Make all the letters uppercase
                    SequenceBuilder.Append(SequenceLine)
                Loop While FileLines.Peek.StartsWith(" ")

                LoadingMacromolecule.Sequence = SequenceBuilder.ToString
            End Sub

            Private Shared Sub ReadContig(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                'Found only in CON entries for stitching together other GenBank files.
                Throw New NotImplementedException
            End Sub

            Private Shared Sub ReadReference(FileLines As Queue(Of String), LoadingMacromolecule As Macromolecule)
                Dim ReferenceLine As String = FileLines.Dequeue.Substring(9).TrimStart()
                Dim NewReference As New Macromolecule.Reference
                Dim IsStartLine As Boolean = True

                Dim ReferenceElements As String() = ReferenceLine.Split({CChar(" ")}, StringSplitOptions.RemoveEmptyEntries)
                Dim ReferenceNumber As Integer = CInt(ReferenceElements(0))
                Dim StartBase As Integer = CInt(ReferenceElements(2))
                Dim EndBase As Integer = CInt(ReferenceElements(4).Remove(ReferenceElements(4).Length - 1))

                With NewReference
                    .RelatedSequenceLocations.Add(New Tuple(Of Integer, Integer)(StartBase, EndBase))

                    Do
                        IsStartLine = True
                        Select Case True
                            Case FileLines.Peek.StartsWith("  AUTHORS")
                                Do
                                    Dim AuthorLine As String = FileLines.Dequeue
                                    Dim AuthorElements As String()

                                    With AuthorLine
                                        If IsStartLine Then
                                            If .Length > 9 Then
                                                AuthorLine = .Substring(9)
                                            Else
                                                Exit Do
                                            End If
                                        End If
                                    End With

                                    AuthorElements = AuthorLine.TrimStart().Split({CChar(" ")})

                                    For Each AuthorElement As String In AuthorElements

                                        If AuthorElement = "and" Then
                                            Continue For
                                        ElseIf Not AuthorElement.Remove(AuthorElement.Length - 1).Contains(",") Then
                                            'Treat as a suffix of the previous author. This detection may be buggy.
                                            .Authors.Last.Suffix &= AuthorElement.TrimEnd({CChar(",")})
                                            Continue For
                                        End If

                                        Dim NameElements As String() = AuthorElement.Split({CChar(",")}, StringSplitOptions.RemoveEmptyEntries)

                                        Dim NewAuthor As New Macromolecule.Reference.Author

                                        With NewAuthor
                                            .LastName = NameElements(0)
                                            Dim FirstInitialsElements As String() = NameElements(1).Split({CChar(".")}, StringSplitOptions.RemoveEmptyEntries)

                                            For Each Initial As String In FirstInitialsElements
                                                .FirstInitials.Add(Initial)
                                            Next
                                        End With

                                        .Authors.Add(NewAuthor)
                                    Next

                                    IsStartLine = False
                                Loop While FileLines.Peek.StartsWith("   ")

                            Case FileLines.Peek.StartsWith("  CONSRTM")
                                Dim ConsortiumBuilder As New Text.StringBuilder

                                Do
                                    Dim ConsortiumLine As String = FileLines.Dequeue

                                    With ConsortiumLine
                                        If IsStartLine Then
                                            If .Length > 9 Then
                                                ConsortiumLine = .Substring(9)
                                            Else
                                                Exit Do
                                            End If
                                        Else
                                            ConsortiumBuilder.Append(" ")
                                        End If
                                    End With

                                    ConsortiumBuilder.Append(ConsortiumLine.TrimStart())

                                    IsStartLine = False
                                Loop While FileLines.Peek.StartsWith("   ")

                                .Consortium = ConsortiumBuilder.ToString

                            Case FileLines.Peek.StartsWith("  TITLE")
                                Dim TitleBuilder As New Text.StringBuilder

                                Do
                                    Dim TitleLine As String = FileLines.Dequeue

                                    With TitleLine
                                        If IsStartLine Then
                                            If .Length > 9 Then
                                                TitleLine = .Substring(9)
                                            Else
                                                Exit Do
                                            End If
                                        Else
                                            TitleBuilder.Append(" ")
                                        End If
                                    End With

                                    TitleBuilder.Append(TitleLine.TrimStart())

                                    IsStartLine = False
                                Loop While FileLines.Peek.StartsWith("   ")

                                .Title = TitleBuilder.ToString

                            Case FileLines.Peek.StartsWith("  JOURNAL")
                                Dim JournalBuilder As New Text.StringBuilder

                                Do
                                    Dim JournalLine As String = FileLines.Dequeue

                                    With JournalLine
                                        If IsStartLine Then
                                            If .Length > 9 Then
                                                JournalLine = .Substring(9)
                                            Else
                                                Exit Do
                                            End If
                                        Else
                                            If JournalLine.EndsWith(";") Then
                                                JournalBuilder.Append(vbCrLf)
                                            Else
                                                JournalBuilder.Append(" ")
                                            End If

                                        End If
                                    End With

                                    JournalBuilder.Append(JournalLine.TrimStart().TrimEnd({CChar(";")}))

                                    IsStartLine = False
                                Loop While FileLines.Peek.StartsWith("   ")

                                .Publication = JournalBuilder.ToString

                            Case FileLines.Peek.StartsWith("  MEDLINE")
                                Dim MedlineLine As String = FileLines.Dequeue.Substring(9).TrimStart()
                                .ResourceCrossReferences.Add(New Tuple(Of String, String)("Medline", MedlineLine))

                            Case FileLines.Peek.StartsWith("   PUBMED")
                                Dim PubMedLine As String = FileLines.Dequeue.Substring(9).TrimStart()
                                .ResourceCrossReferences.Add(New Tuple(Of String, String)("PubMed", PubMedLine))

                            Case FileLines.Peek.StartsWith("  REMARK")
                                Dim RemarkBuilder As New Text.StringBuilder

                                Do
                                    Dim RemarkLine As String = FileLines.Dequeue

                                    With RemarkLine
                                        If IsStartLine Then
                                            If .Length > 8 Then
                                                RemarkLine = .Substring(8)
                                            Else
                                                Exit Do
                                            End If
                                        Else
                                            RemarkBuilder.Append(vbCrLf)
                                        End If
                                    End With

                                    RemarkBuilder.Append(RemarkLine.TrimStart())

                                    IsStartLine = False
                                Loop While FileLines.Peek.StartsWith("   ")

                                .Remark = RemarkBuilder.ToString
                        End Select

                    Loop While FileLines.Peek.StartsWith(" ")
                End With

                LoadingMacromolecule.References.Add(ReferenceNumber, NewReference)
            End Sub

            Private Shared Function DecodeFeatureLocation(ByVal LocationString As String) As Tuple(Of List(Of Macromolecule.Feature.Location), Boolean)
                Dim IsJoined As Boolean
                Dim LocationsList As New List(Of Macromolecule.Feature.Location)

                'Detect the presence of an outer wrapper and process it.
                If LocationString.Contains("(") Then
                    'Wrapper
                    If LocationString.StartsWith("complement") Then
                        'Remove wrapper, invert "IsComplement" and pass back to this function

                        LocationString = LocationString.Substring(LocationString.IndexOf(CChar("(")) + 1)
                        LocationString = LocationString.Remove(LocationString.Length - 1)

                        Dim PartLocationsPackage As Tuple(Of List(Of Macromolecule.Feature.Location), Boolean) = DecodeFeatureLocation(LocationString)

                        If Not IsJoined Then IsJoined = PartLocationsPackage.Item2

                        PartLocationsPackage.Item1.Reverse()

                        For Each Location As Macromolecule.Feature.Location In PartLocationsPackage.Item1
                            Swap(Location.StartBase, Location.EndBase)
                            Swap(Location.StartIsBeyond, Location.EndIsBeyond)
                            LocationsList.Add(Location)
                        Next

                    ElseIf LocationString.StartsWith("join") OrElse LocationString.StartsWith("order") Then
                        'Remove wrapper, set "IsJoined" if appropriate, and pass each joined segment back to this function
                        If Not IsJoined AndAlso LocationString.StartsWith("join") Then IsJoined = True

                        LocationString = LocationString.Substring(LocationString.IndexOf(CChar("(")) + 1)
                        LocationString = LocationString.Remove(LocationString.Length - 1)

                        'Split the string along commas not contained within parentheses
                        For Index As Integer = 0 To LocationString.Length - 1
                            Dim LeftParensCount As Integer
                            Dim RightParensCount As Integer
                            Dim StartIndex As Integer
                            Dim Trigger As Boolean
                            Dim LocationPart As String = ""

                            Select Case LocationString(Index)
                                Case CChar("(")
                                    LeftParensCount += 1
                                Case CChar(")")
                                    RightParensCount += 1
                                Case CChar(",")
                                    Trigger = LeftParensCount = RightParensCount
                                    LocationPart = LocationString.Substring(StartIndex, Index - StartIndex)
                            End Select

                            If Not Trigger AndAlso Index = LocationString.Length - 1 Then
                                Trigger = True
                                LocationPart = LocationString.Substring(StartIndex)
                            End If

                            If Trigger Then
                                Dim PartLocationsPackage As Tuple(Of List(Of Macromolecule.Feature.Location), Boolean) = DecodeFeatureLocation(LocationPart)

                                For Each Location As Macromolecule.Feature.Location In PartLocationsPackage.Item1
                                    LocationsList.Add(Location)
                                Next

                                StartIndex = Index + 1
                                Trigger = False
                            End If
                        Next

                    End If
                Else
                    'No wrapper, decode string
                    Dim UnpackedLocation As New Macromolecule.Feature.Location

                    With UnpackedLocation
                        If LocationString.Contains(".") OrElse LocationString.Contains("^") Then
                            If LocationString.Contains(":") Then
                                'This is a remote location
                                .RemoteAccession = LocationString.Substring(0, LocationString.IndexOf(":"))

                                'Remove Remote Accession
                                LocationString = LocationString.Substring(LocationString.IndexOf(CChar(":")) + 1)
                            End If

                            If LocationString.Contains("^") Then
                                .IsBetween = True
                                .StartBase = CInt(LocationString.Substring(0, LocationString.IndexOf(CChar("^"))))
                                .EndBase = CInt(LocationString.Substring(LocationString.IndexOf(CChar("^")) + 1))
                                If Math.Abs(.StartBase - .EndBase) <> 1 Then
                                    Throw New ApplicationException("One of the features is cited as located between two non-adjacent bases.")
                                End If
                            Else
                                If LocationString.StartsWith("<") Then
                                    .StartIsBeyond = True
                                    LocationString = LocationString.Substring(1)
                                End If

                                .StartBase = CInt(LocationString.Substring(0, LocationString.IndexOf(CChar("."))))

                                LocationString = LocationString.Substring(LocationString.LastIndexOf(CChar(".")) + 1)

                                If LocationString.StartsWith(">") Then
                                    .EndIsBeyond = True
                                    LocationString = LocationString.Substring(1)
                                End If

                                .EndBase = CInt(LocationString)
                            End If

                        Else
                            .StartBase = CInt(LocationString)
                            .EndBase = CInt(LocationString)
                        End If
                    End With

                    LocationsList.Add(UnpackedLocation)
                End If

                Return New Tuple(Of List(Of Macromolecule.Feature.Location), Boolean)(LocationsList, IsJoined)
            End Function

        End Class

        Private Class FASTAFile
            Shared Function Read(ByVal FileLines As Queue(Of String)) As Queue(Of Macromolecule)
                Dim LoadingMacromolecule As New Macromolecule 'This and the next line are only here to stop the designer from complaining about the object not set to an instance.
                Dim LoadedMacromolecules As New Queue(Of Macromolecule)

                Do
                    Select Case True
                        Case FileLines.Peek.StartsWith(">")
                            LoadingMacromolecule = New Macromolecule
                            Dim DescriptionLine As String = FileLines.Dequeue.Substring(1)
                            LoadingMacromolecule.Description = DescriptionLine
                        Case Else
                            Dim SequenceBuilder As New Text.StringBuilder

                            Do
                                Dim SequenceLine As String = FileLines.Dequeue
                                SequenceLine = SequenceLine.ToUpper() 'Make all the letters uppercase
                                SequenceBuilder.Append(SequenceLine)
                            Loop Until FileLines.Count = 0 OrElse FileLines.Peek.StartsWith(">")

                            LoadingMacromolecule.Sequence = SequenceBuilder.ToString

                            LoadedMacromolecules.Enqueue(LoadingMacromolecule)
                    End Select
                Loop While FileLines.Count > 0

                Return LoadedMacromolecules
            End Function
        End Class

        Private Shared Function ExtractNucleicAcidSequence(ByVal InputText As String) As String
            'Remove all non-nucleic acid characters from a blob of text
            Dim SequenceBuilder As New Text.StringBuilder

            For Each Character As Char In InputText
                If NucleicAcidCharacters.Contains(Character) Then
                    SequenceBuilder.Append(Character)
                End If
            Next

            Return SequenceBuilder.ToString
        End Function

        Private Shared Function ExtractAminoAcidSequence(ByVal InputText As String) As String
            'Remove all non-amino acid characters from a blob of text
            Dim SequenceBuilder As New Text.StringBuilder

            For Each Character As Char In InputText
                If AminoAcidCharacters.Contains(Character) Then
                    SequenceBuilder.Append(Character)
                End If
            Next

            Return SequenceBuilder.ToString
        End Function

        Private Shared Function DetectFileType(ByVal FileLines As Queue(Of String)) As FileTypes
            Select Case True
                Case FileLines.Peek.StartsWith("LOCUS ")
                    Return FileTypes.GenBank_DDBJ
                Case FileLines.Peek.StartsWith("ID ")
                    Return FileTypes.EMBL
                Case FileLines.Peek.StartsWith(">")
                    If FileLines.Peek.StartsWith(">DL;") OrElse FileLines(0).StartsWith(">P1;") Then
                        Return FileTypes.NBRF_PIR
                    Else
                        Return FileTypes.FASTA
                    End If
                Case Else
                    MessageBox.Show("Unrecognized file type!")
                    Return FileTypes.Raw
            End Select
        End Function

    End Class
End Class


'Can some of these tasks be made multi-core aware?