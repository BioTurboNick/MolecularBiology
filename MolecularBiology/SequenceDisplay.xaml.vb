Imports System.Math
Imports MolecularBiology2.Mathematics

Imports System.Collections.ObjectModel

Friend Class SequenceDisplay
    Inherits UserControl

#Region "Properties"
    Private DisplacementLevelMin As Double
    Private DisplacementLevelMax As Double

    Public Shared ReadOnly CurrentMacromoleculeProperty As DependencyProperty = DependencyProperty.Register("CurrentMacromolecule", GetType(Macromolecule), GetType(SequenceDisplay), New FrameworkPropertyMetadata(New Macromolecule, AddressOf OnCurrentMacromoleculeChanged))
    Public Shared ReadOnly DisplacementIncrementProperty As DependencyProperty = DependencyProperty.Register("DisplacementIncrement", GetType(Double), GetType(SequenceDisplay), New FrameworkPropertyMetadata(0.0))

    Friend Property CurrentMacromolecule As Macromolecule
        Get
            Return DirectCast(GetValue(CurrentMacromoleculeProperty), Macromolecule)
        End Get
        Set(value As Macromolecule)
            SetValue(CurrentMacromoleculeProperty, value)
        End Set
    End Property

    Friend Property DisplacementIncrement As Double
        Get
            Return CDbl(GetValue(DisplacementIncrementProperty))
        End Get
        Set(value As Double)
            SetValue(DisplacementIncrementProperty, value)
        End Set
    End Property

#End Region

#Region "Property Callbacks"
    Private Shared Sub OnDataContextChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, SequenceDisplay).OnDataContextChanged()
    End Sub

    Private Sub OnDataContextChanged()
        'Assign the DataContext to the CurrentMacromolecule property
        CurrentMacromolecule = DirectCast(DataContext, Macromolecule)
    End Sub

    Shared Sub OnCurrentMacromoleculeChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        Dim OldMacromolecule As Macromolecule = DirectCast(e.OldValue, Macromolecule)
        Dim NewMacromolecule As Macromolecule = DirectCast(e.NewValue, Macromolecule)
        Dim FeaturesChanged, IsCircularChanged As Boolean

        If (OldMacromolecule.Features IsNot NewMacromolecule.Features OrElse
            OldMacromolecule.InfoFeatures IsNot NewMacromolecule.InfoFeatures) Then FeaturesChanged = True

        If OldMacromolecule.IsCircular <> NewMacromolecule.IsCircular Then IsCircularChanged = True

        DirectCast(d, SequenceDisplay).OnCurrentMacromoleculeChanged(FeaturesChanged, IsCircularChanged)

    End Sub

    Private Sub OnCurrentMacromoleculeChanged(ByVal FeaturesChanged As Boolean, ByVal IsCircularChanged As Boolean)
        Dim DisplacementChanged As Boolean

        ''If Features are different, check for changes in the displacement range
        'If FeaturesChanged Then
        '    Dim DisplacementLevelMinTemp As Double
        '    Dim DisplacementLevelMaxTemp As Double

        '    'As formulated, only Features will extend themselves inward/lower, so find the MaxDisplacement
        '    For Each FeatureItem As Macromolecule.Feature In CurrentMacromolecule.Features
        '        If FeatureItem.OverlapIndex > DisplacementLevelMaxTemp Then DisplacementLevelMaxTemp = FeatureItem.OverlapIndex
        '    Next

        '    'As formulated, only InfoFeatures will extend themselves outward/upper, so find the MinDisplacement
        '    'Invert and extend 1 more than their OverlapIndex states
        '    For Each InfoFeatureItem As Macromolecule.Feature In CurrentMacromolecule.InfoFeatures
        '        If -InfoFeatureItem.OverlapIndex - 1 < DisplacementLevelMinTemp Then DisplacementLevelMinTemp = -InfoFeatureItem.OverlapIndex - 1
        '    Next

        '    'Check if the levels have changed, then compute the new CenterY and Radius values.
        '    If DisplacementLevelMax <> DisplacementLevelMaxTemp OrElse DisplacementLevelMin <> DisplacementLevelMinTemp Then
        '        DisplacementLevelMax = DisplacementLevelMaxTemp
        '        DisplacementLevelMin = DisplacementLevelMinTemp

        '        If DisplacementLevelMax < 0 Then
        '            DisplacementLevelMax /= 2
        '        End If

        '        If DisplacementLevelMin < 0 Then
        '            DisplacementLevelMin /= 2
        '        End If

        '        DisplacementChanged = True
        '    End If
        'End If

        DrawSequenceDisplay()
    End Sub

    Private Sub OnSizeChanged() Handles Me.SizeChanged
        DrawSequenceDisplay()
    End Sub

#End Region

    Shared Sub New()
        DataContextProperty.OverrideMetadata(GetType(SequenceDisplay), New FrameworkPropertyMetadata(New Macromolecule, AddressOf OnDataContextChanged))
    End Sub

    Private Sub DrawSequenceDisplay()
        If Me.ActualWidth = 0 Then Exit Sub
        'How many bases can we fit in the control at one time?
        Dim X As New FormattedText(CurrentMacromolecule.Sequence, System.Globalization.CultureInfo.CurrentCulture, Windows.FlowDirection.LeftToRight, New Typeface("Consolas"), 13.0, Brushes.Black)
        Dim BaseCharWidth = X.Width / X.Text.Length
        Dim LineBasesLimit As Integer = CInt(Int((SequenceLineContainer.ActualWidth - 60) / BaseCharWidth))
        Dim NumLines As Integer = CInt(Ceiling(CurrentMacromolecule.SequenceLength / LineBasesLimit))

        'Break sequence up into lines
        Dim SequenceLines As New Queue(Of String)
        Dim MarkerShapes As New Queue(Of Path)
        Dim LineStartBases As New Queue(Of String)
        Dim LineEndBases As New Queue(Of String)
        Dim LineFeatures As New Queue(Of Queue(Of Macromolecule.Feature))
        For LineIndex As Integer = 0 To NumLines - 1
            Dim LineStartBaseIndex As Integer = LineIndex * LineBasesLimit
            Dim LineLength As Integer
            If LineIndex < NumLines - 1 Then
                LineLength = LineBasesLimit
            Else
                LineLength = CurrentMacromolecule.SequenceLength - LineStartBaseIndex
            End If

            Dim NewLine As String = CurrentMacromolecule.Sequence.Substring(LineStartBaseIndex, LineLength)
            Dim NewMarkerPath As Path = DrawMarkerLine(LineStartBaseIndex + 1, LineLength, BaseCharWidth)

            SequenceLines.Enqueue(NewLine)
            MarkerShapes.Enqueue(NewMarkerPath)
            LineStartBases.Enqueue(CStr(LineStartBaseIndex + 1))
            LineEndBases.Enqueue(CStr(LineStartBaseIndex + LineBasesLimit))

            'Find features within the line
            Dim LineStartBase As Integer = LineStartBaseIndex + 1
            Dim LineEndBase As Integer = LineStartBase + LineLength
            Dim CurrentLineFeatures As New Queue(Of Macromolecule.Feature)
            For Each Feature As Macromolecule.Feature In CurrentMacromolecule.Features
                Dim LineFeature As New Macromolecule.Feature
                Dim LineSequenceLocations As New List(Of Macromolecule.Feature.Location)
                For Each SequenceLocation In Feature.SequenceLocations
                    If SequenceLocation.IsRemote Then Continue For
                    Dim LineSequenceLocation As New Macromolecule.Feature.Location
                    Dim StartInterior As Boolean = False
                    Dim EndInterior As Boolean = False
                    Dim FoundLocation As Boolean = False
                    Dim PreviousLocation As Macromolecule.Feature.Location
                    If SequenceLocation.StartBase >= LineStartBase AndAlso SequenceLocation.StartBase <= LineEndBase Then StartInterior = True
                    If SequenceLocation.EndBase >= LineStartBase AndAlso SequenceLocation.EndBase <= LineEndBase Then EndInterior = True

                    If StartInterior AndAlso EndInterior Then
                        'Both endpoints are within the region
                        If SequenceLocation.SpansOrigin Then
                            ' ====|----|====
                            Dim LineSequenceLocation2 As New Macromolecule.Feature.Location
                            If SequenceLocation.StartBase < SequenceLocation.EndBase Then
                                LineSequenceLocation.EndBase = SequenceLocation.EndBase
                                LineSequenceLocation.EndIsBeyond = SequenceLocation.EndIsBeyond
                                LineSequenceLocation.StartBase = LineEndBase
                                LineSequenceLocation.StartIsBeyond = True

                                LineSequenceLocation2.StartBase = SequenceLocation.StartBase
                                LineSequenceLocation2.StartIsBeyond = SequenceLocation.StartIsBeyond
                                LineSequenceLocation2.EndBase = LineStartBase
                                LineSequenceLocation2.EndIsBeyond = True
                            Else
                                LineSequenceLocation.StartBase = SequenceLocation.StartBase
                                LineSequenceLocation.StartIsBeyond = SequenceLocation.StartIsBeyond
                                LineSequenceLocation.EndBase = LineStartBase
                                LineSequenceLocation.EndIsBeyond = True

                                LineSequenceLocation2.EndBase = SequenceLocation.EndBase
                                LineSequenceLocation2.EndIsBeyond = SequenceLocation.EndIsBeyond
                                LineSequenceLocation2.StartBase = LineEndBase
                                LineSequenceLocation2.StartIsBeyond = True
                            End If
                            LineSequenceLocations.Add(LineSequenceLocation2)
                        Else
                            '----|====|----
                            LineSequenceLocation = SequenceLocation
                        End If
                        FoundLocation = True
                    ElseIf StartInterior Then
                        '---------|==== or ====|---------
                        LineSequenceLocation.StartBase = SequenceLocation.StartBase
                        LineSequenceLocation.StartIsBeyond = SequenceLocation.StartIsBeyond
                        LineSequenceLocation.EndIsBeyond = True

                        If SequenceLocation.StartBase < SequenceLocation.EndBase Xor SequenceLocation.SpansOrigin Then
                            LineSequenceLocation.EndBase = LineStartBase
                        Else
                            LineSequenceLocation.EndBase = LineEndBase
                        End If
                        FoundLocation = True
                    ElseIf EndInterior Then
                        '---------|==== or ====|---------
                        LineSequenceLocation.EndBase = SequenceLocation.EndBase
                        LineSequenceLocation.EndIsBeyond = SequenceLocation.EndIsBeyond
                        LineSequenceLocation.StartIsBeyond = True

                        If SequenceLocation.StartBase < SequenceLocation.EndBase Xor SequenceLocation.SpansOrigin Then
                            LineSequenceLocation.StartBase = LineStartBase
                        Else
                            LineSequenceLocation.StartBase = LineEndBase
                        End If
                        FoundLocation = True
                    Else
                        If (SequenceLocation.StartBase < LineStartBase AndAlso SequenceLocation.EndBase > LineEndBase) Xor SequenceLocation.SpansOrigin Then
                            '==============
                            LineSequenceLocation.StartBase = LineStartBase
                            LineSequenceLocation.EndBase = LineEndBase
                            FoundLocation = True
                        ElseIf (SequenceLocation.StartBase > LineEndBase AndAlso SequenceLocation.EndBase < LineStartBase) Xor SequenceLocation.SpansOrigin Then
                            '==============
                            LineSequenceLocation.EndBase = LineStartBase
                            LineSequenceLocation.StartBase = LineEndBase
                            FoundLocation = True
                        End If
                        '*****************What to do with a feature joined between a previous line and this line, or a future line.
                    End If

                    If FoundLocation Then LineSequenceLocations.Add(LineSequenceLocation)
                Next
                If LineSequenceLocations.Count > 0 Then
                    LineFeature.Type = Feature.Type
                    LineFeature.Qualifiers = Feature.Qualifiers
                    LineFeature.IsJoined = Feature.IsJoined
                    LineFeature.Completeness = Feature.Completeness
                    LineFeature.SequenceLocations = LineSequenceLocations
                    CurrentLineFeatures.Enqueue(LineFeature)
                End If
            Next
            LineFeatures.Enqueue(CurrentLineFeatures)
        Next

        'Clear the original sequence lines
        SequenceLineContainer.Children.Clear()

        'Load the sequence lines into TextBlocks and add them to the panel
        For I = 1 To NumLines
            Dim Z As New Grid
            Dim Z1, Z2, Z3 As New ColumnDefinition
            Z1.Width = New GridLength(30)
            Z3.Width = New GridLength(BaseCharWidth * LineBasesLimit)
            Z.ColumnDefinitions.Add(Z1)
            Z.ColumnDefinitions.Add(Z3)
            Z.ColumnDefinitions.Add(New ColumnDefinition)
            Z.RowDefinitions.Add(New RowDefinition)
            Z.RowDefinitions.Add(New RowDefinition)
            Z.RowDefinitions.Add(New RowDefinition)
            For Count As Integer = 1 To LineFeatures.Peek.Count
                Z.RowDefinitions.Add(New RowDefinition)
            Next

            'Add sequence
            Dim Y As New TextBlock
            Y.Text = SequenceLines.Dequeue
            Y.FontFamily = New FontFamily("Consolas")
            Y.FontSize = 13
            Grid.SetColumn(Y, 1)
            Z.Children.Add(Y)

            Dim Y1 As New TextBlock
            Y1.Text = SequenceTools.GetComplement(Y.Text, False)
            Y1.FontFamily = New FontFamily("Consolas")
            Y1.FontSize = 13
            Grid.SetColumn(Y1, 1)
            Grid.SetRow(Y1, 2)
            Z.Children.Add(Y1)

            '*********Idea for selction box: select each text separately or together

            'Add position markers
            Dim D As Path = MarkerShapes.Dequeue
            Grid.SetColumn(D, 1)
            Grid.SetRow(D, 1)
            Z.Children.Add(D)

            'Add left and right position indicators
            Dim E As New TextBlock
            E.Text = LineStartBases.Dequeue
            E.FontFamily = New FontFamily("Segoe UI")
            E.FontSize = 11
            E.TextAlignment = TextAlignment.Right
            E.Margin = New Thickness(0, -1, 5, 0)
            E.VerticalAlignment = Windows.VerticalAlignment.Center
            Grid.SetRow(E, 0)
            Grid.SetColumn(E, 0)
            Grid.SetRowSpan(E, 3)
            Z.Children.Add(E)

            Dim F As New TextBlock
            F.Text = LineEndBases.Dequeue
            F.FontFamily = New FontFamily("Segoe UI")
            F.FontSize = 11
            F.TextAlignment = TextAlignment.Left
            F.Margin = New Thickness(5, -1, 0, 0)
            F.VerticalAlignment = Windows.VerticalAlignment.Center
            Grid.SetRow(F, 0)
            Grid.SetColumn(F, 2)
            Grid.SetRowSpan(F, 3)
            Z.Children.Add(F)

            'Add Features
            If LineFeatures.Count > 0 Then
                Dim CurrentLineFeatures As Queue(Of Macromolecule.Feature) = LineFeatures.Dequeue
                For FeatureRow As Integer = 3 To CurrentLineFeatures.Count + 2
                    Dim Feature As Macromolecule.Feature = CurrentLineFeatures.Dequeue
                    'Dim G As Shape = DrawFeature(Feature.SequenceLocations, BaseCharWidth)
                    Dim G As New Rectangle
                    G.Width = 25
                    G.Height = 10
                    G.Stroke = Brushes.Black
                    G.HorizontalAlignment = Windows.HorizontalAlignment.Left
                    G.Margin = New Thickness(0, 5, 0, 0)
                    Grid.SetRow(G, FeatureRow)
                    Grid.SetColumn(G, 1)
                    Z.Children.Add(G)
                Next
            End If

            Z.Margin = New Thickness(0, 0, 0, 25)
            SequenceLineContainer.Children.Add(Z)
        Next

    End Sub

    Private Function DrawMarkerLine(ByVal LineStartBase As Integer, ByVal LineBaseLength As Integer, ByVal BaseCharWidth As Double) As Path
        Dim MarkerInterval As Integer = 10

        Dim A As New Path
        Dim A1 As New StreamGeometry
        Using Context As StreamGeometryContext = A1.Open()
            Dim Baseline As Double = 5
            Dim MinorTickLength As Double = 2.5
            Dim MajorTickLength As Double = 5

            Context.BeginFigure(New Point(0, Baseline), False, False)

            For BaseIndex As Integer = 0 To LineBaseLength - 1
                Dim LowerTickPosition As Double = Baseline
                Dim UpperTickPosition As Double = Baseline

                If (LineStartBase + BaseIndex) / MarkerInterval = Int((LineStartBase + BaseIndex) / MarkerInterval) Then
                    LowerTickPosition += MajorTickLength
                    UpperTickPosition -= MajorTickLength
                Else
                    LowerTickPosition += MinorTickLength
                    UpperTickPosition -= MinorTickLength
                End If

                Context.LineTo(New Point((BaseIndex + 0.5) * BaseCharWidth, Baseline), True, True)
                Context.LineTo(New Point((BaseIndex + 0.5) * BaseCharWidth, LowerTickPosition), True, True)
                Context.LineTo(New Point((BaseIndex + 0.5) * BaseCharWidth, UpperTickPosition), True, True)
                Context.LineTo(New Point((BaseIndex + 0.5) * BaseCharWidth, Baseline), False, True)
            Next

            Context.LineTo(New Point(LineBaseLength * BaseCharWidth, Baseline), True, True)
        End Using

        ' Freeze the geometry for performance benefits
        A1.Freeze()

        A.Data = A1
        A.Stroke = Brushes.Gray
        A.StrokeThickness = 1

        Return A
    End Function

    Private Function DrawFeature(ByVal SequenceLocations As List(Of Macromolecule.Feature.Location), ByVal BaseCharWidth As Double) As Shape
        Return New Rectangle
    End Function

End Class
