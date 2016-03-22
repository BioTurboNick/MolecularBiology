Imports MolecularBiology2.Application

Friend MustInherit Class DiagramShapeBase
    Inherits Shape

#Region "Properties"
    Public Shared ReadOnly RadiusProperty As DependencyProperty = DependencyProperty.Register("Radius", GetType(Double), GetType(DiagramShapeBase), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender, AddressOf OnMeasureChanged, AddressOf OnCoerceMeasureChanged))
    Public Shared ReadOnly CenterXProperty As DependencyProperty = DependencyProperty.Register("CenterX", GetType(Double), GetType(DiagramShapeBase), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender, AddressOf OnMeasureChanged, AddressOf OnCoerceMeasureChanged))
    Public Shared ReadOnly CenterYProperty As DependencyProperty = DependencyProperty.Register("CenterY", GetType(Double), GetType(DiagramShapeBase), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender, AddressOf OnMeasureChanged, AddressOf OnCoerceMeasureChanged))
    Public Shared ReadOnly IsCircularProperty As DependencyProperty = DependencyProperty.Register("IsCircular", GetType(Boolean), GetType(DiagramShapeBase), New FrameworkPropertyMetadata(False, FrameworkPropertyMetadataOptions.AffectsRender))

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

    Friend Property IsCircular As Boolean
        Get
            Return CBool(GetValue(IsCircularProperty))
        End Get
        Set(value As Boolean)
            SetValue(IsCircularProperty, value)
        End Set
    End Property

#End Region

    Protected Shared Sub OnMeasureChanged(ByVal d As Object, ByVal e As DependencyPropertyChangedEventArgs)
    End Sub

    Protected Shared Function OnCoerceMeasureChanged(ByVal d As DependencyObject, ByVal baseValue As Object) As Double
        Dim NewValue As Double = CDbl(baseValue)

        If NewValue < 0 Then
            Return 0
        Else
            Return NewValue
        End If
    End Function

    Protected Overrides ReadOnly Property DefiningGeometry As System.Windows.Media.Geometry
        Get
            Return Nothing
        End Get
    End Property
End Class