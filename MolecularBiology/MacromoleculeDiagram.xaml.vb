Imports System.Math
Imports MolecularBiology2.Mathematics

Friend Class MacromoleculeDiagram
    Inherits UserControl

#Region "Properties"
    Private DisplacementLevelMin As Double
    Private DisplacementLevelMax As Double

    Public Shared ReadOnly CurrentMacromoleculeProperty As DependencyProperty = DependencyProperty.Register("CurrentMacromolecule", GetType(Macromolecule), GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(New Macromolecule, AddressOf OnCurrentMacromoleculeChanged))
    Public Shared ReadOnly RadiusProperty As DependencyProperty = DependencyProperty.Register("Radius", GetType(Double), GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(0.0, AddressOf OnMeasureChanged, AddressOf OnCoerceMeasureChanged))
    Public Shared ReadOnly CenterXProperty As DependencyProperty = DependencyProperty.Register("CenterX", GetType(Double), GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(0.0, AddressOf OnMeasureChanged, AddressOf OnCoerceMeasureChanged))
    Public Shared ReadOnly CenterYProperty As DependencyProperty = DependencyProperty.Register("CenterY", GetType(Double), GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(0.0, AddressOf OnMeasureChanged, AddressOf OnCoerceMeasureChanged))
    Public Shared ReadOnly DisplacementIncrementProperty As DependencyProperty = DependencyProperty.Register("DisplacementIncrement", GetType(Double), GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(0.0))
    Public Shared ReadOnly ScaleFactorProperty As DependencyProperty = DependencyProperty.Register("ScaleFactor", GetType(Double), GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(100.0))

    Friend Property CurrentMacromolecule As Macromolecule
        Get
            Return DirectCast(GetValue(CurrentMacromoleculeProperty), Macromolecule)
        End Get
        Set(value As Macromolecule)
            SetValue(CurrentMacromoleculeProperty, value)
        End Set
    End Property

    Friend Property Radius As Double
        Get
            Return CDbl(GetValue(RadiusProperty))
        End Get
        Set(value As Double)
            SetValue(RadiusProperty, value)
        End Set
    End Property

    Friend Property CenterX As Double
        Get
            Return CDbl(GetValue(CenterXProperty))
        End Get
        Set(value As Double)
            SetValue(CenterXProperty, value)
        End Set
    End Property

    Friend Property CenterY As Double
        Get
            Return CDbl(GetValue(CenterYProperty))
        End Get
        Set(value As Double)
            SetValue(CenterYProperty, value)
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

    Friend Property ScaleFactor As Double
        Get
            Return CDbl(GetValue(ScaleFactorProperty))
        End Get
        Set(value As Double)
            SetValue(ScaleFactorProperty, value)
        End Set
    End Property
#End Region

#Region "Property Callbacks"
    Private Shared Sub OnDataContextChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, MacromoleculeDiagram).OnDataContextChanged()
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

        DirectCast(d, MacromoleculeDiagram).OnCurrentMacromoleculeChanged(FeaturesChanged, IsCircularChanged)

    End Sub

    Private Sub OnCurrentMacromoleculeChanged(ByVal FeaturesChanged As Boolean, ByVal IsCircularChanged As Boolean)
        Dim DisplacementChanged As Boolean

        'If Features are different, check for changes in the displacement range
        If FeaturesChanged Then
            Dim DisplacementLevelMinTemp As Double
            Dim DisplacementLevelMaxTemp As Double

            'As formulated, only Features will extend themselves inward/lower, so find the MaxDisplacement
            For Each FeatureItem As Macromolecule.Feature In CurrentMacromolecule.Features
                If FeatureItem.OverlapIndex > DisplacementLevelMaxTemp Then DisplacementLevelMaxTemp = FeatureItem.OverlapIndex
            Next

            'As formulated, only InfoFeatures will extend themselves outward/upper, so find the MinDisplacement
            'Invert and extend 1 more than their OverlapIndex states
            For Each InfoFeatureItem As Macromolecule.Feature In CurrentMacromolecule.InfoFeatures
                If -InfoFeatureItem.OverlapIndex - 1 < DisplacementLevelMinTemp Then DisplacementLevelMinTemp = -InfoFeatureItem.OverlapIndex - 1
            Next

            'Check if the levels have changed, then compute the new CenterY and Radius values.
            If DisplacementLevelMax <> DisplacementLevelMaxTemp OrElse DisplacementLevelMin <> DisplacementLevelMinTemp Then
                DisplacementLevelMax = DisplacementLevelMaxTemp
                DisplacementLevelMin = DisplacementLevelMinTemp

                If DisplacementLevelMax < 0 Then
                    DisplacementLevelMax /= 2
                End If

                If DisplacementLevelMin < 0 Then
                    DisplacementLevelMin /= 2
                End If

                DisplacementChanged = True
            End If
        End If

        If DisplacementChanged OrElse IsCircularChanged Then
            CalculateCenterY()
            CalculateRadius()
        End If
    End Sub

    'Private Sub OnSizeChanged() Handles Me.SizeChanged

    '    'If DataContext Is Nothing OrElse ActualHeight = 0 OrElse ActualWidth = 0 Then Exit Sub

    '    Dim InheritParameters As Boolean = False
    '    Dim ZoomFactor As Double = 1
    '    Dim RotationAngleFinal As Double = 0
    '    Dim RotationAngle As Double = 0

    '    If CurrentMacromolecule.IsCircular Then
    '        CenterY = ActualHeight / 2
    '    Else
    '        CenterY = (ActualHeight - DisplacementIncrement * (DisplacementLevelMax - DisplacementLevelMin)) / 2
    '    End If

    '    If Not InheritParameters Then

    '        With CurrentMacromolecule
    '            'If .IsCircular AndAlso ActualWidth < 120 Then Width = 120

    '            CenterX = ActualWidth / 2

    '            If .IsCircular Then
    '                Radius = Math.Min(ActualWidth, ActualHeight) / 2 - DisplacementBase
    '            Else
    '                Radius = ActualWidth / 2 - DisplacementBase
    '            End If

    '            Radius *= ZoomFactor

    '            If .IsCircular AndAlso ZoomFactor > 1 Then
    '                Dim SelectionChord As Double = Min(ActualWidth, ActualHeight)
    '                Dim SelectionSagitta As Double = (2 * Radius) - Sqrt((2 * Radius) ^ 2 - (SelectionChord / 2) ^ 2)

    '                CenterY += (Radius - SelectionSagitta) * Cos(AngleToRadians(RotationAngleFinal - RotationAngle))
    '                CenterX -= (Radius - SelectionSagitta) * Sin(AngleToRadians(RotationAngleFinal - RotationAngle))
    '            End If
    '        End With

    '    End If
    'End Sub

    Private Sub OnSizeChanged(ByVal sender As Object, ByVal e As SizeChangedEventArgs) Handles Me.SizeChanged
        ScaleFactor = Log10((ActualHeight + ActualWidth) / 2) * 100
        DisplacementIncrement = 0.15 * ScaleFactor

        If e.WidthChanged OrElse e.HeightChanged Then
            'CenterX only changes when the width of the control changes
            If e.WidthChanged Then
                CenterX = ActualWidth / 2
            End If

            'CenterY only changes when the height of the control changes
            If e.HeightChanged Then
                CalculateCenterY()
            End If

            'Radius only changes when the minimum of the height and width of the control changes
            If CurrentMacromolecule.IsCircular AndAlso (Math.Min(e.NewSize.Height, e.NewSize.Width) <> Math.Min(e.PreviousSize.Height, e.PreviousSize.Width)) OrElse
                Not CurrentMacromolecule.IsCircular AndAlso e.WidthChanged Then
                CalculateRadius()
            End If
        End If
    End Sub

    Private Shared Sub OnMeasureChanged(ByVal d As Object, ByVal e As DependencyPropertyChangedEventArgs)
    End Sub

    Private Shared Function OnCoerceMeasureChanged(ByVal d As DependencyObject, ByVal baseValue As Object) As Double
        Dim NewValue As Double = CDbl(baseValue)
        If NewValue < 0 Then
            Return 0
        Else
            Return NewValue
        End If
    End Function
#End Region

    Shared Sub New()
        DataContextProperty.OverrideMetadata(GetType(MacromoleculeDiagram), New FrameworkPropertyMetadata(New Macromolecule, AddressOf OnDataContextChanged))
    End Sub

    Private Sub CalculateCenterY()
        Dim YOffset, CenterYTemp As Double

        CenterYTemp = ActualHeight / 2

        If Not CurrentMacromolecule.IsCircular Then
            'The goal is to center the linear display.
            'Find the average displacement level on either side; If balanced, = 0
            'Negative offset = upwards shift, but negative displacement level = downwards shift
            YOffset = -(DisplacementLevelMax + DisplacementLevelMin) / 2 * DisplacementIncrement
            CenterYTemp += YOffset
        End If

        CenterY = CenterYTemp
    End Sub

    Private Sub CalculateRadius()
        Dim RadiusOffset, RadiusTemp As Double

        If CurrentMacromolecule.IsCircular Then
            RadiusTemp = Math.Min(ActualHeight, ActualWidth) / 2
            'Adjust the radius to accomodate all features displayed.
            'Take the minimum displacement level, negate it, and shift the radius by that amount.
            RadiusOffset = (DisplacementLevelMin - 1) * DisplacementIncrement
        Else
            RadiusTemp = ActualWidth / 2
            RadiusOffset = -DisplacementIncrement
        End If

        RadiusTemp += RadiusOffset
        Radius = RadiusTemp
    End Sub
End Class
