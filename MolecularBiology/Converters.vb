Imports System.Math
Imports System.Collections.ObjectModel

Friend NotInheritable Class SequenceLocationsConverter
    Implements IMultiValueConverter

    Public Function Convert(values() As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IMultiValueConverter.Convert
        If values(0) Is DependencyProperty.UnsetValue OrElse
           values(1) Is DependencyProperty.UnsetValue Then
            Return New List(Of FeatureShape.Span)
        End If

        Dim SequenceLocations As List(Of Macromolecule.Feature.Location) = DirectCast(values(0), List(Of Macromolecule.Feature.Location))
        Dim SequenceLength As Integer = CInt(values(1))

        Dim ShapeSpans As New List(Of FeatureShape.Span)

        For Each Location As Macromolecule.Feature.Location In SequenceLocations
            Dim Span As New FeatureShape.Span

            With Location
                Dim BaseStartAdjustment As Double
                Dim BaseEndAdjustment As Double

                If .IsBetween Then
                    BaseEndAdjustment = -1
                Else
                    Select Case .EndBase - .StartBase
                        Case Is > 0
                            BaseStartAdjustment = -1
                            BaseEndAdjustment = 0
                        Case Is < 0
                            BaseStartAdjustment = 0
                            BaseEndAdjustment = -1
                        Case 0
                            BaseStartAdjustment = -0.5
                            BaseEndAdjustment = 0.5
                    End Select
                End If


                Span.StartFraction = (.StartBase + BaseStartAdjustment) / SequenceLength
                Span.EndFraction = (.EndBase + BaseEndAdjustment) / SequenceLength

                Span.UncertainStart = .StartIsBeyond
                Span.UncertainEnd = .EndIsBeyond
                Span.SpansOrigin = .SpansOrigin

                ShapeSpans.Add(Span)
            End With
        Next

        Return ShapeSpans
    End Function

    Public Function ConvertBack(value As Object, targetTypes() As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object() Implements System.Windows.Data.IMultiValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Friend NotInheritable Class MinHeightConverter
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then
            Return 0
        End If

        Dim IsCircular As Boolean = CBool(value)

        If IsCircular Then
            Return 120
        Else
            Return 30
        End If
    End Function

    Public Function ConvertBack(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class ScalingConverter
    Implements IValueConverter

    Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then
            Return CDbl(0)
        End If

        Dim Length As Double = CDbl(parameter)
        Dim ScalingFactor As Double = CDbl(value)

        Return Length * ScalingFactor

    End Function

    Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class DivideValueConverter
    Implements IValueConverter

    Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue OrElse
            parameter Is DependencyProperty.UnsetValue Then
            Return CDbl(0)
        End If

        Dim FullValue As Double = CDbl(value)
        Dim Divisor As Double = CDbl(parameter)
        Dim FinalValue As Double

        FinalValue = FullValue / Divisor

        Return FinalValue

    End Function

    Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class MajorTickIntervalConverter
    Implements IMultiValueConverter

    Public Function Convert(ByVal values() As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IMultiValueConverter.Convert
        If values(0) Is DependencyProperty.UnsetValue OrElse
           values(1) Is DependencyProperty.UnsetValue OrElse values.Count < 3 OrElse
           values(2) Is DependencyProperty.UnsetValue Then
            Return CInt(0)
        End If

        Dim Size As Double = CDbl(values(0))
        Dim HalfScalingFactor As Double = CDbl(values(1)) / 2
        Dim ZoomFactor As Double = CDbl(values(2))
        Dim SizeMagnitude As Double = Pow(10, Mathematics.FindMagnitude(Size / ZoomFactor))
        Dim MajorTickInterval As Integer

        If ZoomFactor > 700 Then
            Dim x As Double = 3
        End If

        If (Size / ZoomFactor) / HalfScalingFactor / SizeMagnitude >= 5 Then
            MajorTickInterval = CInt(SizeMagnitude)
        ElseIf (Size / ZoomFactor) / HalfScalingFactor / SizeMagnitude >= 2.5 Then
            MajorTickInterval = CInt(SizeMagnitude / 2)
        ElseIf SizeMagnitude > 1 Then
            MajorTickInterval = CInt(SizeMagnitude / 4)
        End If

        If MajorTickInterval < 1 Then MajorTickInterval = 1

        Return MajorTickInterval

    End Function

    Public Function ConvertBack(ByVal value As Object, ByVal targetTypes() As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object() Implements System.Windows.Data.IMultiValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class BooleanToVisibilityConverter
    Implements IValueConverter

    Public Function Convert(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then Return 0

        Dim BooleanValue As Boolean = CBool(value)

        If BooleanValue Then
            Return Visibility.Visible
        Else
            Return Visibility.Collapsed
        End If
    End Function

    Public Function ConvertBack(ByVal value As Object, ByVal targetType As System.Type, ByVal parameter As Object, ByVal culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class ArrowConverter
    Implements IMultiValueConverter

    Public Function Convert(values() As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IMultiValueConverter.Convert
        If values(0) Is DependencyProperty.UnsetValue OrElse
            values(1) Is DependencyProperty.UnsetValue Then
            Return FeatureShape.ArrowTypes.None
        End If

        Dim FeatureType As String = CStr(values(0))
        Dim FeatureQualifiers As ObservableCollection(Of Macromolecule.Feature.Qualifier) = DirectCast(values(1), ObservableCollection(Of Macromolecule.Feature.Qualifier))

        Select Case FeatureType
            Case "CDS", "mat_peptide", "misc_RNA", "mRNA", "ncRNA", "rRNA", "tmRNA", "tRNA"
                Return FeatureShape.ArrowTypes.EndArrow
            Case "rep_origin", "oriT"
                For Each QualifierItem As Macromolecule.Feature.Qualifier In FeatureQualifiers
                    If QualifierItem.Type = "direction" Then
                        Select Case QualifierItem.Data
                            Case "RIGHT"
                                Return FeatureShape.ArrowTypes.EndArrow
                            Case "LEFT"
                                Return FeatureShape.ArrowTypes.StartArrow
                            Case "BOTH"
                                Return FeatureShape.ArrowTypes.BothArrow
                        End Select
                    End If
                Next
                Return FeatureShape.ArrowTypes.BothArrow
            Case Else
                Return FeatureShape.ArrowTypes.None
        End Select

    End Function

    Public Function ConvertBack(value As Object, targetTypes() As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object() Implements System.Windows.Data.IMultiValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class FeatureLabelConverter
    Implements IMultiValueConverter

    Public Function Convert(values() As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IMultiValueConverter.Convert
        If values(0) Is DependencyProperty.UnsetValue OrElse
            values(1) Is DependencyProperty.UnsetValue Then
            Return ""
        End If

        Dim FeatureType As String = CStr(values(0))
        Dim FeatureQualifiers As ObservableCollection(Of Macromolecule.Feature.Qualifier) = DirectCast(values(1), ObservableCollection(Of Macromolecule.Feature.Qualifier))

        Dim PossibleNameValues As New Dictionary(Of String, String)

        For Each QualifierItem As Macromolecule.Feature.Qualifier In FeatureQualifiers
            Select Case QualifierItem.Type
                Case "label", "gene", "allele"
                    PossibleNameValues.Add(QualifierItem.Type, QualifierItem.Data)
            End Select
        Next

        If PossibleNameValues.ContainsKey("label") Then
            Return PossibleNameValues("label")
        ElseIf PossibleNameValues.ContainsKey("gene") Then
            Dim NameText As String
            NameText = PossibleNameValues("gene") & " " & FeatureType
            If PossibleNameValues.ContainsKey("allele") Then
                NameText &= " (" & PossibleNameValues("allele") & ")"
            End If

            Return NameText
        Else
            Return FeatureType
        End If

        Return ""
    End Function

    Public Function ConvertBack(value As Object, targetTypes() As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object() Implements System.Windows.Data.IMultiValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class RadiusConverter
    Implements IMultiValueConverter

    Public Function Convert(values() As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IMultiValueConverter.Convert
        If values(0) Is DependencyProperty.UnsetValue OrElse
            values(1) Is DependencyProperty.UnsetValue OrElse
            values(2) Is DependencyProperty.UnsetValue OrElse
            values(3) Is DependencyProperty.UnsetValue Then
            Return 0.0
        End If

        Dim BaseRadius As Double = CDbl(values(0))
        Dim DisplacementLevel As Double = CDbl(values(1))
        Dim DisplacementIncrement As Double = CDbl(values(2))
        Dim IsCircular As Boolean = CBool(values(3))
        Dim FinalRadius As Double = BaseRadius

        If IsCircular Then
            If DisplacementLevel < 0 Then DisplacementIncrement /= 2

            FinalRadius -= DisplacementIncrement * DisplacementLevel
        End If

        Return FinalRadius

    End Function

    Public Function ConvertBack(value As Object, targetTypes() As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object() Implements System.Windows.Data.IMultiValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class CenterYConverter
    Implements IMultiValueConverter

    Public Function Convert(values() As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IMultiValueConverter.Convert
        If values(0) Is DependencyProperty.UnsetValue OrElse
            values(1) Is DependencyProperty.UnsetValue OrElse
            values(2) Is DependencyProperty.UnsetValue OrElse
            values(3) Is DependencyProperty.UnsetValue Then
            Return 0.0
        End If

        Dim BaseCenterY As Double = CDbl(values(0))
        Dim DisplacementLevel As Double = CDbl(values(1))
        Dim DisplacementIncrement As Double = CDbl(values(2))
        Dim IsCircular As Boolean = CBool(values(3))
        Dim FinalCenterY As Double = BaseCenterY

        If Not IsCircular Then
            If DisplacementLevel < 0 Then
                DisplacementIncrement /= 2
            End If

            FinalCenterY += DisplacementIncrement * DisplacementLevel
        End If

        Return FinalCenterY

    End Function

    Public Function ConvertBack(value As Object, targetTypes() As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object() Implements System.Windows.Data.IMultiValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class InvertValueConverter
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then
            Return 0
        End If

        Dim OriginalValue As Double = CDbl(value)
        Dim InvertedValue As Double
        Dim ValueAdjustment As Double = CDbl(parameter)

        InvertedValue = -OriginalValue + ValueAdjustment

        Return InvertedValue

    End Function

    Public Function ConvertBack(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class SequenceStringToCollectionConverter
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then
            Return New Dictionary(Of Integer, Char)
        End If

        'Convert the sequence into a dictionary with the base position in the Key position

        Dim Sequence As String = CStr(value)
        Dim SequenceBaseDictionary As New Dictionary(Of Integer, Char)

        For Index As Integer = 0 To Sequence.Length - 1
            SequenceBaseDictionary.Add(Index + 1, Sequence(Index))
        Next
        Return SequenceBaseDictionary

    End Function

    Public Function ConvertBack(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class SequencePositionVisibilityConverter
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then
            Return Visibility.Collapsed
        End If

        Dim BasePosition As Integer = CInt(value)

        Dim Remainder As Integer

        DivRem(BasePosition, 50, Remainder)

        If Remainder = 0 OrElse BasePosition = 1 Then
            Return Visibility.Visible
        Else
            Return Visibility.Hidden
        End If
    End Function

    Public Function ConvertBack(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class

Public NotInheritable Class SequenceTickLengthConverter
    Implements IValueConverter

    Public Function Convert(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.Convert
        If value Is DependencyProperty.UnsetValue Then
            Return 0
        End If

        Dim BasePosition As Integer = CInt(value)

        Dim Remainder As Integer

        DivRem(BasePosition, 10, Remainder)

        If Remainder = 0 OrElse BasePosition = 1 Then
            Return 5
        Else
            Return 2.5
        End If
    End Function

    Public Function ConvertBack(value As Object, targetType As System.Type, parameter As Object, culture As System.Globalization.CultureInfo) As Object Implements System.Windows.Data.IValueConverter.ConvertBack
        Throw New NotImplementedException
    End Function
End Class