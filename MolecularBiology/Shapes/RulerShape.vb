Imports MolecularBiology2.Mathematics
Imports System.Math
Imports System.Globalization

Friend NotInheritable Class RulerShape
    Inherits DiagramShapeBase
    Private Typeface As Typeface
    Private Labels As New List(Of List(Of FormattedText))

#Region "Dependency Properties"
    Public Shared ReadOnly MacromoleculeSizeProperty As DependencyProperty = DependencyProperty.Register("MacromoleculeSize", GetType(Integer), GetType(RulerShape), New FrameworkPropertyMetadata(0, FrameworkPropertyMetadataOptions.AffectsRender, AddressOf AffectsIntervals))
    Public Shared ReadOnly MajorTickIntervalProperty As DependencyProperty = DependencyProperty.Register("MajorTickInterval", GetType(Integer), GetType(RulerShape), New FrameworkPropertyMetadata(0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly MajorTickLengthProperty As DependencyProperty = DependencyProperty.Register("MajorTickLength", GetType(Double), GetType(RulerShape), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly MinorTickIntervalProperty As DependencyProperty = DependencyProperty.Register("MinorTickInterval", GetType(Integer), GetType(RulerShape), New FrameworkPropertyMetadata(0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly MinorTickLengthProperty As DependencyProperty = DependencyProperty.Register("MinorTickLength", GetType(Double), GetType(RulerShape), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly ShowMinorTicksProperty As DependencyProperty = DependencyProperty.Register("ShowMinorTicks", GetType(Boolean), GetType(RulerShape), New FrameworkPropertyMetadata(False, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly FontFamilyProperty As DependencyProperty = TextElement.FontFamilyProperty.AddOwner(GetType(RulerShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontStyleProperty As DependencyProperty = TextElement.FontStyleProperty.AddOwner(GetType(RulerShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontWeightProperty As DependencyProperty = TextElement.FontWeightProperty.AddOwner(GetType(RulerShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontStretchProperty As DependencyProperty = TextElement.FontStretchProperty.AddOwner(GetType(RulerShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontSizeProperty As DependencyProperty = TextElement.FontSizeProperty.AddOwner(GetType(RulerShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))

    Friend Property MacromoleculeSize() As Integer
        Get
            Return CInt(MyBase.GetValue(MacromoleculeSizeProperty))
        End Get
        Set(ByVal value As Integer)
            MyBase.SetValue(MacromoleculeSizeProperty, value)
        End Set
    End Property

    Friend Property MajorTickInterval() As Integer
        Get
            Return CInt(MyBase.GetValue(MajorTickIntervalProperty))
        End Get
        Set(ByVal value As Integer)
            MyBase.SetValue(MajorTickIntervalProperty, value)
        End Set
    End Property

    Friend Property MajorTickLength() As Double
        Get
            Return CInt(MyBase.GetValue(MajorTickLengthProperty))
        End Get
        Set(ByVal value As Double)
            MyBase.SetValue(MajorTickLengthProperty, value)
        End Set
    End Property

    Friend Property MinorTickInterval() As Integer
        Get
            Return CInt(MyBase.GetValue(MinorTickIntervalProperty))
        End Get
        Set(ByVal value As Integer)
            MyBase.SetValue(MinorTickIntervalProperty, value)
        End Set
    End Property

    Friend Property MinorTickLength() As Double
        Get
            Return CInt(MyBase.GetValue(MinorTickLengthProperty))
        End Get
        Set(ByVal value As Double)
            MyBase.SetValue(MinorTickLengthProperty, value)
        End Set
    End Property

    Friend Property ShowMinorTicks() As Boolean
        Get
            Return CBool(MyBase.GetValue(ShowMinorTicksProperty))
        End Get
        Set(ByVal value As Boolean)
            MyBase.SetValue(ShowMinorTicksProperty, value)
        End Set
    End Property

    Friend Property FontFamily() As FontFamily
        Get
            Return DirectCast(MyBase.GetValue(FontFamilyProperty), FontFamily)
        End Get
        Set(ByVal value As FontFamily)
            MyBase.SetValue(FontFamilyProperty, value)
        End Set
    End Property

    Friend Property FontStyle() As FontStyle
        Get
            Return DirectCast(MyBase.GetValue(FontStyleProperty), FontStyle)
        End Get
        Set(ByVal value As FontStyle)
            MyBase.SetValue(FontStyleProperty, value)
        End Set
    End Property

    Friend Property FontWeight() As FontWeight
        Get
            Return DirectCast(MyBase.GetValue(FontWeightProperty), FontWeight)
        End Get
        Set(ByVal value As FontWeight)
            MyBase.SetValue(FontWeightProperty, value)
        End Set
    End Property

    Friend Property FontStretch() As FontStretch
        Get
            Return DirectCast(MyBase.GetValue(FontStretchProperty), FontStretch)
        End Get
        Set(ByVal value As FontStretch)
            MyBase.SetValue(FontStretchProperty, value)
        End Set
    End Property

    Friend Property FontSize() As Double
        Get
            Return CDbl(MyBase.GetValue(FontSizeProperty))
        End Get
        Set(ByVal value As Double)
            MyBase.SetValue(FontSizeProperty, value)
        End Set
    End Property

    Friend ReadOnly Property Foreground() As Brush
        Get
            Return DirectCast(MyBase.GetValue(StrokeProperty), Brush)
        End Get
    End Property

#End Region

    Public Sub New()
        Typeface = New Typeface(FontFamily, FontStyle, FontWeight, FontStretch)
    End Sub

    Private Shared Sub OnFontChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, RulerShape).OnFontChanged(d)
    End Sub

    Private Sub OnFontChanged(ByVal d As DependencyObject)
        Typeface = New Typeface(FontFamily, FontStyle, FontWeight, FontStretch)
        InvalidateVisual()
    End Sub

    Private Shared Sub AffectsIntervals(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, RulerShape).AffectsIntervals(d)
    End Sub

    Private Sub AffectsIntervals(ByVal d As DependencyObject)
        'Make a list of all the Major Tick label FormattedText objects
        Dim CurrentBase As Integer = MajorTickInterval
        Labels.Clear()

        Do While CurrentBase < MacromoleculeSize
            Dim LabelText As String
            Dim FormattedChars As New List(Of FormattedText)
            Dim MegaCurrentPosition As Double = CurrentBase / 1000000
            Dim KiloCurrentPosition As Double = CurrentBase / 1000

            If MegaCurrentPosition >= 1 Then
                Dim MegaTest As Integer
                DivRem(CurrentBase, 1000000, MegaTest)

                If MegaTest = 0 Then
                    LabelText = CStr(MegaCurrentPosition) & ".0 Mb"
                Else
                    LabelText = CStr(MegaCurrentPosition) & " Mb"
                End If
            ElseIf KiloCurrentPosition >= 1 Then
                Dim KiloTest As Integer
                DivRem(CurrentBase, 1000, KiloTest)

                If KiloTest = 0 Then
                    LabelText = CStr(KiloCurrentPosition) & ".0 kb"
                Else
                    LabelText = CStr(KiloCurrentPosition) & " kb"
                End If
            Else
                LabelText = CStr(CurrentBase) & " bp"
            End If

            For Each Character As Char In LabelText
                Dim FormattedChar As New FormattedText(Character, CultureInfo.CurrentCulture, FlowDirection.LeftToRight, Typeface, FontSize, Foreground)
                FormattedChars.Add(FormattedChar)
            Next

            Labels.Add(FormattedChars)

            CurrentBase += MajorTickInterval
        Loop

        InvalidateMeasure()
        InvalidateVisual()
    End Sub

    Protected Overrides ReadOnly Property DefiningGeometry() As System.Windows.Media.Geometry
        Get
            Dim Geometry As New StreamGeometry

            Using Context As StreamGeometryContext = Geometry.Open()
                InternalDrawGeometry(Context)
            End Using

            ' Freeze the geometry for performance benefits
            Geometry.Freeze()

            Return Geometry
        End Get
    End Property

    Private Sub InternalDrawGeometry(ByVal Context As StreamGeometryContext)
        Dim CurrentBase As Integer
        Dim TickBasePoint As Point
        Dim TickEndPoint As Point

        If MacromoleculeSize = 0 OrElse Radius = 0 Then Exit Sub

        If IsCircular Then
            Dim OffsetRadius As Double = Radius
            Dim CircleSize As New Size(OffsetRadius, OffsetRadius)
            Dim BaseAngle As Double = 360 / MacromoleculeSize
            Dim MajorTickAngle As Double = MajorTickInterval * BaseAngle
            Dim MajorTickEndRadius As Double = OffsetRadius - MajorTickLength
            Dim IsLargeArc As Boolean
            Dim CurrentAngle As Double

            Dim StartPoint As Point = PolarToCartesian(0, OffsetRadius)
            Dim OriginTickPoint As Point = PolarToCartesian(0, MajorTickEndRadius - MajorTickLength)

            StartPoint.Offset(CenterX, CenterY)
            OriginTickPoint.Offset(CenterX, CenterY)

            Context.BeginFigure(StartPoint, False, True)
            Context.LineTo(OriginTickPoint, True, True)
            Context.LineTo(StartPoint, True, True)

            If ShowMinorTicks AndAlso MajorTickInterval >= 5 Then
                Dim MinorTickAngle As Double = MinorTickInterval * BaseAngle
                IsLargeArc = MinorTickAngle > 180
                CurrentAngle = MinorTickAngle - BaseAngle / 2
                CurrentBase = MinorTickInterval
                Dim MinorTickEndRadius As Double = OffsetRadius - MinorTickLength

                Do While CurrentAngle < 360
                    TickBasePoint = PolarToCartesian(CurrentAngle, OffsetRadius)
                    TickBasePoint.Offset(CenterX, CenterY)

                    Dim MajorTest As Integer
                    DivRem(CurrentBase, MajorTickInterval, MajorTest)

                    Dim MinorTest As Integer
                    DivRem(CurrentBase, MinorTickInterval, MinorTest)

                    If MajorTest = 0 Then
                        TickEndPoint = PolarToCartesian(CurrentAngle, MajorTickEndRadius)
                    ElseIf MinorTest = 0 Then
                        TickEndPoint = PolarToCartesian(CurrentAngle, MinorTickEndRadius)
                    End If

                    TickEndPoint.Offset(CenterX, CenterY)

                    If MajorTest = 0 OrElse MinorTest = 0 Then
                        Context.ArcTo(TickBasePoint, CircleSize, 0, IsLargeArc, SweepDirection.Clockwise, True, True)
                        Context.LineTo(TickEndPoint, True, True)
                        Context.LineTo(TickBasePoint, True, True)
                    End If

                    CurrentAngle += BaseAngle
                    CurrentBase += 1
                Loop
            Else
                IsLargeArc = MajorTickAngle > 180
                CurrentAngle = MajorTickAngle - BaseAngle / 2

                Do While CurrentAngle < 360
                    TickBasePoint = PolarToCartesian(CurrentAngle, OffsetRadius)
                    TickEndPoint = PolarToCartesian(CurrentAngle, MajorTickEndRadius)

                    TickBasePoint.Offset(CenterX, CenterY)
                    TickEndPoint.Offset(CenterX, CenterY)

                    Context.ArcTo(TickBasePoint, CircleSize, 0, IsLargeArc, SweepDirection.Clockwise, True, True)
                    Context.LineTo(TickEndPoint, True, True)
                    Context.LineTo(TickBasePoint, True, True)

                    CurrentAngle += MajorTickAngle
                Loop
            End If

            IsLargeArc = 360 - CurrentAngle > 180

            Context.ArcTo(StartPoint, CircleSize, 0, IsLargeArc, SweepDirection.Clockwise, True, True)
        Else
            Dim BaseSpan As Double = 2 * Radius / MacromoleculeSize
            Dim MajorTickSpan As Double = MajorTickInterval * BaseSpan
            Dim CurrentPosition As Double = -Radius
            Dim XOffset As Double = CenterX
            Dim YOffset As Double = CenterY

            Dim StartPoint As New Point(-Radius, 0)
            Dim EndPoint As New Point(Radius, 0)

            StartPoint.Offset(XOffset, YOffset)
            EndPoint.Offset(XOffset, YOffset)

            Context.BeginFigure(StartPoint, False, False)

            Dim OriginTickPoint As New Point(-Radius, 2 * MajorTickLength)
            OriginTickPoint.Offset(XOffset, YOffset)
            Context.LineTo(OriginTickPoint, True, True)
            Context.LineTo(StartPoint, True, True)

            If ShowMinorTicks AndAlso MajorTickInterval >= 5 Then
                Dim MinorTickSpan As Double = MinorTickInterval * BaseSpan
                CurrentPosition += MinorTickSpan - BaseSpan / 2
                CurrentBase = MinorTickInterval

                Do While CurrentBase <= MacromoleculeSize

                    TickBasePoint = New Point(CurrentPosition, 0)

                    Dim MajorTest As Integer
                    DivRem(CurrentBase, MajorTickInterval, MajorTest)

                    Dim MinorTest As Integer
                    DivRem(CurrentBase, MinorTickInterval, MinorTest)

                    If MajorTest = 0 Then
                        TickEndPoint = New Point(CurrentPosition, MajorTickLength)
                    ElseIf MinorTest = 0 Then
                        TickEndPoint = New Point(CurrentPosition, MinorTickLength)
                    End If

                    TickBasePoint.Offset(XOffset, YOffset)
                    TickEndPoint.Offset(XOffset, YOffset)

                    If MajorTest = 0 OrElse MinorTest = 0 Then
                        Context.LineTo(TickBasePoint, True, True)
                        Context.LineTo(TickEndPoint, True, True)
                        Context.LineTo(TickBasePoint, True, True)
                    End If

                    CurrentPosition += BaseSpan
                    CurrentBase += 1
                Loop
            Else
                CurrentPosition += MajorTickSpan - BaseSpan / 2

                Do While CurrentBase <= MacromoleculeSize
                    TickBasePoint = New Point(CurrentPosition, 0)
                    TickEndPoint = New Point(CurrentPosition, MajorTickLength)

                    TickBasePoint.Offset(XOffset, YOffset)
                    TickEndPoint.Offset(XOffset, YOffset)

                    Context.LineTo(TickBasePoint, True, True)
                    Context.LineTo(TickEndPoint, True, True)
                    Context.LineTo(TickBasePoint, True, True)

                    CurrentPosition += MajorTickSpan
                    CurrentBase += MajorTickInterval
                Loop
            End If

            Context.LineTo(EndPoint, True, True)

        End If
    End Sub

    Protected Overloads Overrides Sub OnRender(ByVal RenderDrawingContext As DrawingContext)
        'Required in order to draw the geometry before drawing the text elements
        MyBase.OnRender(RenderDrawingContext)

        DrawLabels(RenderDrawingContext)
    End Sub

    Private Sub DrawLabels(ByVal RenderDrawingContext As DrawingContext)
        Dim CurrentBase As Integer = MajorTickInterval
        If Labels.Count = 0 Then Exit Sub

        If IsCircular Then
            Dim OffsetRadius = Radius - MajorTickLength - Labels(0)(0).Baseline
            Dim BaseAngle As Double = 360 / MacromoleculeSize
            Dim MajorTickAngle As Double = MajorTickInterval / MacromoleculeSize * 360
            Dim CurrentAngle As Double = MajorTickAngle - BaseAngle / 2

            For Each Label As List(Of FormattedText) In Labels
                Dim LabelWidth As Double = 0
                Dim InvertText As Boolean = False
                Dim Progress As Double = 0
                Dim TestAngle As Double = NormalizeAngle(CurrentAngle)

                CurrentAngle = NormalizeAngle(CurrentAngle)

                If TestAngle > 90 AndAlso TestAngle < 270 Then InvertText = True

                For Each Character As FormattedText In Label
                    LabelWidth += Character.WidthIncludingTrailingWhitespace
                Next

                For Each Character As FormattedText In Label
                    Dim HalfCharWidth As Double = Character.WidthIncludingTrailingWhitespace / 2
                    Dim CharBaseline As Double
                    Dim Angle As Double

                    Progress += HalfCharWidth / LabelWidth

                    Dim LabelArcFraction = LabelWidth / (OffsetRadius * PI) * 360
                    Dim AngleOffset = -(LabelArcFraction / 2) * (0.5 - Progress)

                    If InvertText Then
                        CharBaseline = Character.Height - Character.Baseline
                        Angle = CurrentAngle - AngleOffset
                    Else
                        CharBaseline = Character.Baseline
                        Angle = CurrentAngle + AngleOffset
                    End If

                    Dim CharPoint As Point = PolarToCartesian(Angle, OffsetRadius)

                    CharPoint.Offset(CenterX, CenterY)

                    RenderDrawingContext.PushTransform(New TranslateTransform(CharPoint.X - HalfCharWidth, CharPoint.Y - CharBaseline))

                    If InvertText Then Angle -= 180

                    RenderDrawingContext.PushTransform(New RotateTransform(Angle, HalfCharWidth, CharBaseline))
                    RenderDrawingContext.DrawText(Character, New Point(0, 0))
                    RenderDrawingContext.Pop()
                    RenderDrawingContext.Pop()

                    Progress += HalfCharWidth / LabelWidth
                Next

                CurrentAngle += MajorTickAngle
            Next
        Else
            Dim MajorTickSpan As Double = MajorTickInterval / MacromoleculeSize * 2 * Radius
            Dim BaseSpan As Double = 2 * Radius / MacromoleculeSize
            Dim CurrentPosition As Double = -Radius

            Dim XOffset As Double = CenterX
            Dim YOffset As Double = CenterY + MajorTickLength - Labels(0)(0).Baseline

            CurrentPosition += MajorTickSpan - BaseSpan / 2

            For Each Label As List(Of FormattedText) In Labels
                Dim LabelWidth As Double = 0
                Dim Progress As Double = 0

                For Each Character As FormattedText In Label
                    LabelWidth += Character.WidthIncludingTrailingWhitespace
                Next

                For Each Character As FormattedText In Label
                    Dim HalfCharWidth = Character.WidthIncludingTrailingWhitespace / 2
                    Dim CharBaseline = Character.Baseline

                    Progress += HalfCharWidth / LabelWidth

                    Dim Offset As Double = LabelWidth * (Progress - 0.5)

                    Dim CharPoint As New Point(CurrentPosition + Offset, CharBaseline)

                    CharPoint.Offset(XOffset, YOffset)

                    RenderDrawingContext.PushTransform(New TranslateTransform(CharPoint.X - HalfCharWidth, CharPoint.Y))
                    RenderDrawingContext.DrawText(Character, New Point(0, 0))
                    RenderDrawingContext.Pop()

                    Progress += HalfCharWidth / LabelWidth
                Next

                CurrentPosition += MajorTickSpan
            Next

        End If
    End Sub
End Class