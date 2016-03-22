Imports MolecularBiology2.Mathematics
Imports System.Math
Imports System.Globalization

Friend NotInheritable Class FeatureShape
    Inherits DiagramShapeBase
    Private Typeface As Typeface
    Private FormattedChars As New List(Of FormattedText)
    Private TextLength As Double
    Private InvertText As Boolean

    Public Sub New()
        Typeface = New Typeface(FontFamily, FontStyle, FontWeight, FontStretch)
    End Sub

#Region "Properties"
    Public Shared ReadOnly TextProperty As DependencyProperty = TextBlock.TextProperty.AddOwner(GetType(FeatureShape), New FrameworkPropertyMetadata("", New PropertyChangedCallback(AddressOf OnTextChanged)))
    Public Shared ReadOnly FontFamilyProperty As DependencyProperty = TextElement.FontFamilyProperty.AddOwner(GetType(FeatureShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontStyleProperty As DependencyProperty = TextElement.FontStyleProperty.AddOwner(GetType(FeatureShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontWeightProperty As DependencyProperty = TextElement.FontWeightProperty.AddOwner(GetType(FeatureShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontStretchProperty As DependencyProperty = TextElement.FontStretchProperty.AddOwner(GetType(FeatureShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly FontSizeProperty As DependencyProperty = TextElement.FontSizeProperty.AddOwner(GetType(FeatureShape), New FrameworkPropertyMetadata(New PropertyChangedCallback(AddressOf OnFontChanged)))
    Public Shared ReadOnly SpansProperty As DependencyProperty = DependencyProperty.Register("Spans", GetType(List(Of Span)), GetType(FeatureShape), New FrameworkPropertyMetadata(New List(Of Span), FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly ShaftWidthProperty As DependencyProperty = DependencyProperty.Register("ShaftWidth", GetType(Double), GetType(FeatureShape), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly IsJoinedProperty As DependencyProperty = DependencyProperty.Register("IsJoined", GetType(Boolean), GetType(FeatureShape), New FrameworkPropertyMetadata(False, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly JoinExtensionProperty As DependencyProperty = DependencyProperty.Register("JoinExtension", GetType(Double), GetType(FeatureShape), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly ShapeTypeProperty As DependencyProperty = DependencyProperty.Register("ShapeType", GetType(ShapeTypes), GetType(FeatureShape), New FrameworkPropertyMetadata(ShapeTypes.Block, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly HeadExtensionProperty As DependencyProperty = DependencyProperty.Register("HeadExtension", GetType(Double), GetType(FeatureShape), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly HeadLengthProperty As DependencyProperty = DependencyProperty.Register("HeadLength", GetType(Double), GetType(FeatureShape), New FrameworkPropertyMetadata(0.0, FrameworkPropertyMetadataOptions.AffectsRender))
    Public Shared ReadOnly ArrowTypeProperty As DependencyProperty = DependencyProperty.Register("ArrowType", GetType(ArrowTypes), GetType(FeatureShape), New FrameworkPropertyMetadata(ArrowTypes.None, FrameworkPropertyMetadataOptions.AffectsRender))

    Public Property Text() As String
        Get
            Return CStr(MyBase.GetValue(TextProperty))
        End Get
        Set(ByVal value As String)
            MyBase.SetValue(TextProperty, value)
        End Set
    End Property

    Public Property FontFamily() As FontFamily
        Get
            Return DirectCast(MyBase.GetValue(FontFamilyProperty), FontFamily)
        End Get
        Set(ByVal value As FontFamily)
            MyBase.SetValue(FontFamilyProperty, value)
        End Set
    End Property

    Public Property FontStyle() As FontStyle
        Get
            Return DirectCast(MyBase.GetValue(FontStyleProperty), FontStyle)
        End Get
        Set(ByVal value As FontStyle)
            MyBase.SetValue(FontStyleProperty, value)
        End Set
    End Property

    Public Property FontWeight() As FontWeight
        Get
            Return DirectCast(MyBase.GetValue(FontWeightProperty), FontWeight)
        End Get
        Set(ByVal value As FontWeight)
            MyBase.SetValue(FontWeightProperty, value)
        End Set
    End Property

    Public Property FontStretch() As FontStretch
        Get
            Return DirectCast(MyBase.GetValue(FontStretchProperty), FontStretch)
        End Get
        Set(ByVal value As FontStretch)
            MyBase.SetValue(FontStretchProperty, value)
        End Set
    End Property

    Public Property FontSize() As Double
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

    Friend Property Spans As List(Of Span)
        Get
            Return DirectCast(GetValue(SpansProperty), List(Of Span))
        End Get
        Set(value As List(Of Span))
            SetValue(SpansProperty, value)
        End Set
    End Property

    Friend Property ShaftWidth As Double
        Get
            Return CDbl(GetValue(ShaftWidthProperty))
        End Get
        Set(value As Double)
            SetValue(ShaftWidthProperty, value)
        End Set
    End Property

    Friend Property IsJoined As Boolean
        Get
            Return CBool(GetValue(IsJoinedProperty))
        End Get
        Set(value As Boolean)
            SetValue(IsJoinedProperty, value)
        End Set
    End Property

    Friend Property JoinExtension As Double
        Get
            Return CDbl(GetValue(JoinExtensionProperty))
        End Get
        Set(value As Double)
            SetValue(JoinExtensionProperty, value)
        End Set
    End Property

    Friend Property ShapeType As ShapeTypes
        Get
            Return DirectCast(GetValue(ShapeTypeProperty), ShapeTypes)
        End Get
        Set(value As ShapeTypes)
            SetValue(ShapeTypeProperty, value)
        End Set
    End Property

    Friend Property ArrowType As ArrowTypes
        Get
            Return DirectCast(GetValue(ArrowTypeProperty), ArrowTypes)
        End Get
        Set(value As ArrowTypes)
            SetValue(ArrowTypeProperty, value)
        End Set
    End Property

    Friend Property HeadExtension As Double
        Get
            Return CDbl(GetValue(HeadExtensionProperty))
        End Get
        Set(value As Double)
            SetValue(HeadExtensionProperty, value)
        End Set
    End Property

    Friend Property HeadLength As Double
        Get
            Return CDbl(GetValue(HeadLengthProperty))
        End Get
        Set(value As Double)
            SetValue(HeadLengthProperty, value)
        End Set
    End Property
#End Region

    Protected Overrides ReadOnly Property DefiningGeometry As System.Windows.Media.Geometry
        Get
            Dim Geometry As New StreamGeometry

            Using Context As StreamGeometryContext = Geometry.Open()
                InternalDrawShapeGeometry(Context)
            End Using

            ' Freeze the geometry for performance benefits
            Geometry.Freeze()

            Return Geometry
        End Get
    End Property

    Private Sub InternalDrawShapeGeometry(Context As StreamGeometryContext)
        Select Case ShapeType
            Case ShapeTypes.Block
                DrawBlock(Context)
            Case ShapeTypes.Bracket
                DrawBracket(Context)
            Case ShapeTypes.Astrix
            Case ShapeTypes.Line
                'DrawLine(Context)
        End Select
    End Sub

    Protected ReadOnly Property GuidePathGeometry() As PathGeometry
        Get
            Dim GuidePathGeometryTemp As New PathGeometry
            Dim GuidePathFigure As New PathFigure

            If Spans.Count = 0 Then Return GuidePathGeometryTemp

            Dim MaxSpanIndex As Integer

            'Find the largest segment to place the text over
            For Index As Integer = 0 To Spans.Count - 1
                Dim PrevMaxSpanFraction As Double
                Dim CurrentSpanFraction As Double

                With Spans(Index)
                    If .SpansOrigin Then
                        CurrentSpanFraction = .StartFraction + 1 - .EndFraction
                    Else
                        CurrentSpanFraction = .EndFraction - .StartFraction
                    End If

                    If CurrentSpanFraction > PrevMaxSpanFraction Then
                        PrevMaxSpanFraction = CurrentSpanFraction
                        MaxSpanIndex = Index
                    End If
                End With
            Next

            With Spans(MaxSpanIndex)
                Dim StartPosition, MidPosition, EndPosition, SpanLength, TextSpanLength, Offset As Double
                Dim StartPoint, MidPoint, EndPoint As Point
                Dim IsLargeArc As Boolean
                Dim GuidePathSegment1, GuidePathSegment2 As PathSegment
                Dim Sweep As SweepDirection

                Dim IsCircularSign As Double

                If IsCircular Then
                    StartPosition = FractionToAngle(.StartFraction)
                    EndPosition = FractionToAngle(.EndFraction)
                    IsCircularSign = 1
                Else
                    Dim Length As Double = Radius * 2
                    StartPosition = Length * (.StartFraction - 0.5)
                    EndPosition = Length * (.EndFraction - 0.5)
                    IsCircularSign = -1
                End If

                Dim IsStartArrow As Boolean = ArrowType = ArrowTypes.StartArrow OrElse ArrowType = ArrowTypes.BothArrow
                Dim IsEndArrow As Boolean = ArrowType = ArrowTypes.EndArrow OrElse ArrowType = ArrowTypes.BothArrow

                SpanLength = EndPosition - StartPosition

                Dim SpanLengthSign As Double = Sign(SpanLength)

                If IsStartArrow AndAlso MaxSpanIndex = 0 Then
                    If IsCircular Then
                        Dim HeadAngle As Double = ComputeAngle(HeadLength, Radius)
                        If .SpansOrigin Then
                            HeadAngle *= -1
                        End If
                        StartPosition += SpanLengthSign * HeadAngle
                    Else
                        StartPosition += SpanLengthSign * HeadLength
                    End If
                End If

                If IsEndArrow AndAlso MaxSpanIndex = Spans.Count - 1 Then
                    If IsCircular Then
                        Dim HeadAngle As Double = ComputeAngle(HeadLength, Radius)
                        If .SpansOrigin Then
                            HeadAngle *= -1
                        End If
                        EndPosition -= SpanLengthSign * HeadAngle
                    Else
                        EndPosition -= SpanLengthSign * HeadLength
                    End If
                End If

                SpanLength = EndPosition - StartPosition

                'Calculate IsCircular-specific variables
                If IsCircular Then
                    Sweep = SweepDirection.Clockwise

                    If .SpansOrigin Then
                        SpanLength = -Sign(SpanLength) * (360 - Abs(SpanLength))
                    End If

                    Offset = Radius

                    If ShapeType = ShapeTypes.Bracket Then
                        Offset += ShaftWidth + 2
                    End If

                    TextSpanLength = ComputeAngle(TextLength, Offset)
                Else
                    If ShapeType = ShapeTypes.Bracket Then
                        Offset -= 2 * ShaftWidth + 2
                    End If

                    TextSpanLength = TextLength
                End If

                If TextSpanLength > 4 * Abs(SpanLength) Then Return GuidePathGeometryTemp

                If IsCircular Then
                    MidPosition = NormalizeAngle(StartPosition + SpanLength / 2)

                    If MidPosition > 90 AndAlso MidPosition < 270 Then
                        InvertText = True
                    Else
                        InvertText = False
                    End If
                End If

                If Not ShapeType = ShapeTypes.Bracket AndAlso Abs(SpanLength) < TextSpanLength Then
                    Offset += IsCircularSign * (ShaftWidth + 2)
                End If

                'Move the start and end positions of the span to the start and endpoints of the text
                Dim HalfTextSpanRemainder As Double = (SpanLength - TextSpanLength) / 2
                StartPosition += HalfTextSpanRemainder
                EndPosition -= HalfTextSpanRemainder

                If IsCircular Then
                    StartPosition = NormalizeAngle(StartPosition)
                    EndPosition = NormalizeAngle(EndPosition)

                    IsLargeArc = Abs(TextSpanLength) / 2 > 180

                    StartPoint = PolarToCartesian(StartPosition, Offset)
                    MidPoint = PolarToCartesian(MidPosition, Offset)
                    EndPoint = PolarToCartesian(EndPosition, Offset)
                Else
                    StartPoint = New Point(StartPosition, Offset)
                    EndPoint = New Point(EndPosition, Offset)
                End If

                StartPoint.Offset(CenterX, CenterY)
                If IsCircular Then
                    MidPoint.Offset(CenterX, CenterY)
                End If
                EndPoint.Offset(CenterX, CenterY)

                If IsCircular Then
                    GuidePathSegment1 = New ArcSegment(MidPoint, New Size(Offset, Offset), 0, IsLargeArc, Sweep, False)
                    GuidePathSegment2 = New ArcSegment(EndPoint, New Size(Offset, Offset), 0, IsLargeArc, Sweep, False)
                Else
                    GuidePathSegment1 = New LineSegment(EndPoint, False)
                End If

                GuidePathFigure.StartPoint = StartPoint
                GuidePathFigure.Segments.Add(GuidePathSegment1)
                If IsCircular Then
                    GuidePathFigure.Segments.Add(GuidePathSegment2)
                End If
            End With

            GuidePathGeometryTemp.Figures.Add(GuidePathFigure)

            Return GuidePathGeometryTemp

        End Get
    End Property

    Private Shared Sub OnFontChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, FeatureShape).OnFontChanged(d)
    End Sub

    Private Sub OnFontChanged(ByVal d As DependencyObject)
        Typeface = New Typeface(FontFamily, FontStyle, FontWeight, FontStretch)

        OnTextChanged(d)
    End Sub

    Private Shared Sub OnTextChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, FeatureShape).OnTextChanged(d)
    End Sub

    Private Sub OnTextChanged(ByVal d As DependencyObject)
        FormattedChars.Clear()
        TextLength = 0

        If Text Is Nothing Then Exit Sub

        For Each Character As Char In Text
            Dim FormattedChar As New FormattedText(Character.ToString(), CultureInfo.CurrentCulture, FlowDirection.LeftToRight, Typeface, FontSize, Foreground)
            FormattedChars.Add(FormattedChar)
            TextLength += FormattedChar.WidthIncludingTrailingWhitespace
        Next

        InvalidateMeasure()
        InvalidateVisual()

    End Sub

    Protected Overloads Overrides Function MeasureOverride(ByVal AvailableSize As Size) As Size
        Dim BoundingRectangle As Rect = GuidePathGeometry.Bounds

        Dim MeasurementSize As Size = CType(BoundingRectangle.BottomRight, Size)

        If MeasurementSize.Width = Double.PositiveInfinity Then MeasurementSize.Width = Double.MaxValue
        If MeasurementSize.Height = Double.PositiveInfinity Then MeasurementSize.Height = Double.MaxValue

        Return MeasurementSize

    End Function

    Protected Overloads Overrides Sub OnRender(ByVal RenderDrawingContext As DrawingContext)
        If Radius = 0 Then Exit Sub

        MyBase.OnRender(RenderDrawingContext)

        Dim GuidePathGeometryTemp As PathGeometry = GuidePathGeometry
        Dim Progress As Double

        If TextLength = 0 OrElse GuidePathGeometry.Figures.Count = 0 Then
            Exit Sub
        End If

        If InvertText Then
            Progress = 1
        Else
            Progress = 0
        End If

        For Each Character As FormattedText In FormattedChars
            Dim HalfCharWidth As Double = Character.WidthIncludingTrailingWhitespace / 2
            Dim HalfCharHeight As Double = Character.Height / 2
            Dim ProgressIncrement As Double = HalfCharWidth / TextLength
            Dim CharPoint As Point
            Dim TangentPoint As Point
            Dim TangentValue As Double

            If InvertText Then
                Progress -= ProgressIncrement
            Else
                Progress += ProgressIncrement
            End If

            GuidePathGeometryTemp.GetPointAtFractionLength(Progress, CharPoint, TangentPoint)

            If InvertText Then
                TangentValue = Atan2(-TangentPoint.Y, -TangentPoint.X)
            Else
                TangentValue = Atan2(TangentPoint.Y, TangentPoint.X)
            End If

            RenderDrawingContext.PushTransform(New TranslateTransform(CharPoint.X - HalfCharWidth, CharPoint.Y - HalfCharHeight))
            If IsCircular Then
                RenderDrawingContext.PushTransform(New RotateTransform(RadiansToAngle(TangentValue), HalfCharWidth, HalfCharHeight))
            End If
            RenderDrawingContext.DrawText(Character, New Point(0, 0))
            RenderDrawingContext.Pop()
            If IsCircular Then
                RenderDrawingContext.Pop()
            End If

            If InvertText Then
                Progress -= ProgressIncrement
            Else
                Progress += ProgressIncrement
            End If
        Next
    End Sub

    Private Sub DrawBlock(Context As StreamGeometryContext)
        'Save the upper points and join points so that the shape can be closed at the end.
        Dim RetracePoints As New Stack(Of Point)

        Dim IsStartArrow As Boolean = ArrowType = ArrowTypes.StartArrow OrElse ArrowType = ArrowTypes.BothArrow
        Dim IsEndArrow As Boolean = ArrowType = ArrowTypes.EndArrow OrElse ArrowType = ArrowTypes.BothArrow
        Dim IsArrow As Boolean = IsStartArrow OrElse IsEndArrow

        Dim UpperShaft, LowerShaft, UpperHead, LowerHead, Join, IsCircularSign As Double

        Dim HalfShaftWidth As Double = ShaftWidth / 2

        If IsCircular Then
            UpperShaft = Radius
            LowerShaft = Radius
            IsCircularSign = 1
        Else
            IsCircularSign = -1
        End If

        UpperShaft += IsCircularSign * HalfShaftWidth
        LowerShaft -= IsCircularSign * HalfShaftWidth

        If IsArrow Then
            UpperHead = UpperShaft + IsCircularSign * HeadExtension
            LowerHead = LowerShaft - IsCircularSign * HeadExtension
        End If

        If IsJoined Then
            Join = UpperShaft + IsCircularSign * JoinExtension
        End If

        'For each spanitem, draw the shape and its connecting parts
        For Each SpanItem As Span In Spans
            With SpanItem
                Dim StartPosition, EndPosition, SpanLength, StartHeadSpan, EndHeadSpan, PreviousEndPosition, StartHeadPosition, EndHeadPosition As Double
                Dim EndHeadShaftUpperPoint, EndHeadPoint, EndHeadShaftLowerPoint, StartHeadShaftUpperPoint, StartHeadPoint, StartHeadShaftLowerPoint, MidUpperPoint, MidLowerPoint As Point
                Dim Sweep1, Sweep2 As SweepDirection
                Dim IsLargeArc As Boolean

                'Convert the fractions into positions based on angles or positions. The linear display is 0 in the center.
                If IsCircular Then
                    StartPosition = FractionToAngle(.StartFraction)
                    EndPosition = FractionToAngle(.EndFraction)
                Else
                    Dim Length As Double = Radius * 2
                    StartPosition = Length * (.StartFraction - 0.5)
                    EndPosition = Length * (.EndFraction - 0.5)
                End If

                SpanLength = EndPosition - StartPosition

                'Calculate IsCircular-specific variables
                If IsCircular Then
                    If SpanLength > 0 Xor .SpansOrigin Then
                        Sweep1 = SweepDirection.Clockwise
                        Sweep2 = SweepDirection.Counterclockwise
                    Else
                        Sweep1 = SweepDirection.Counterclockwise
                        Sweep2 = SweepDirection.Clockwise
                    End If

                    If .SpansOrigin Then
                        SpanLength = -Sign(SpanLength) * (360 - Abs(SpanLength))
                    End If
                End If

                'If the end should have an arrow, calculate variables
                If IsEndArrow AndAlso SpanItem Is Spans.Last Then
                    If IsCircular Then
                        EndHeadSpan = ComputeAngle(HeadLength, Radius)
                    Else
                        EndHeadSpan = HeadLength
                    End If

                    'If the shape is too small for the normal arrow size, adjust
                    If IsStartArrow AndAlso EndHeadSpan > Abs(SpanLength / 2) Then
                        'If the other end has an arrow too, half the distance
                        EndHeadSpan = Abs(SpanLength) / 2
                    ElseIf EndHeadSpan > Abs(SpanLength) Then
                        EndHeadSpan = Abs(SpanLength)
                    End If

                    If SpanLength < 0 Then
                        EndHeadSpan *= -1
                    End If
                Else
                    EndHeadSpan = 0
                End If

                'If the start should have an arrow, calculate variables
                If IsStartArrow AndAlso SpanItem Is Spans.First Then
                    If IsCircular Then
                        StartHeadSpan = ComputeAngle(HeadLength, Radius)
                    Else
                        StartHeadSpan = HeadLength
                    End If

                    'If the shape is too small for the normal arrow size, adjust
                    If IsEndArrow AndAlso StartHeadSpan > Abs(SpanLength / 2) Then
                        'If the other end has an arrow too, half the distance
                        StartHeadSpan = Abs(SpanLength) / 2
                    ElseIf StartHeadSpan > Abs(SpanLength) Then
                        StartHeadSpan = Abs(SpanLength)
                    End If

                    If SpanLength < 0 Then
                        StartHeadSpan *= -1
                    End If
                Else
                    StartHeadSpan = 0
                End If

                'Need to fix this small-size bleedthrough problem.***************************

                SpanLength -= Sign(SpanLength) * (Abs(StartHeadSpan) + Abs(EndHeadSpan))

                'Compute the points necessary to draw the shapes
                If IsCircular Then
                    IsLargeArc = Abs(SpanLength) / 2 > 180
                    StartHeadPosition = NormalizeAngle(StartPosition + StartHeadSpan)
                    EndHeadPosition = NormalizeAngle(EndPosition - EndHeadSpan)
                    Dim MidAngle As Double = NormalizeAngle(StartPosition + StartHeadSpan + SpanLength / 2)

                    'Midpoints are necessary to allow for arcs > half the circle
                    StartHeadShaftUpperPoint = PolarToCartesian(StartHeadPosition, UpperShaft)
                    StartHeadShaftLowerPoint = PolarToCartesian(StartHeadPosition, LowerShaft)
                    MidUpperPoint = PolarToCartesian(MidAngle, UpperShaft)
                    MidLowerPoint = PolarToCartesian(MidAngle, LowerShaft)
                    EndHeadShaftUpperPoint = PolarToCartesian(EndHeadPosition, UpperShaft)
                    EndHeadShaftLowerPoint = PolarToCartesian(EndHeadPosition, LowerShaft)
                Else
                    StartHeadPosition = StartPosition + StartHeadSpan
                    EndHeadPosition = EndPosition - EndHeadSpan
                    EndHeadShaftUpperPoint = New Point(EndHeadPosition, UpperShaft)
                    EndHeadShaftLowerPoint = New Point(EndHeadPosition, LowerShaft)
                    StartHeadShaftUpperPoint = New Point(StartHeadPosition, UpperShaft)
                    StartHeadShaftLowerPoint = New Point(StartHeadPosition, LowerShaft)
                End If

                'Position the points correctly
                EndHeadShaftUpperPoint.Offset(CenterX, CenterY)
                EndHeadPoint.Offset(CenterX, CenterY)
                EndHeadShaftLowerPoint.Offset(CenterX, CenterY)
                StartHeadShaftUpperPoint.Offset(CenterX, CenterY)
                StartHeadShaftLowerPoint.Offset(CenterX, CenterY)
                If IsCircular Then
                    MidUpperPoint.Offset(CenterX, CenterY)
                    MidLowerPoint.Offset(CenterX, CenterY)
                End If

                'Draw the points, starting with the potential join from the previous span.
                'If this isn't the first shape, draw the connecting line (only visible if IsJoined = True), else start the figure
                If Spans.IndexOf(SpanItem) > 0 Then
                    If IsJoined Then
                        Dim JoinPoint As Point
                        If IsCircular Then
                            JoinPoint = PolarToCartesian(PreviousEndPosition + (StartPosition - PreviousEndPosition) / 2, UpperShaft + JoinExtension)
                        Else
                            JoinPoint = New Point(PreviousEndPosition + (StartPosition - PreviousEndPosition) / 2, -(HalfShaftWidth + JoinExtension))
                        End If
                        JoinPoint.Offset(CenterX, CenterY)
                        RetracePoints.Push(JoinPoint)
                        Context.LineTo(JoinPoint, True, True)
                    End If
                    Context.LineTo(StartHeadShaftUpperPoint, IsJoined, IsJoined)
                Else
                    Context.BeginFigure(StartHeadShaftUpperPoint, True, True)
                End If

                RetracePoints.Push(StartHeadShaftUpperPoint)
                If IsCircular Then
                    RetracePoints.Push(MidUpperPoint)
                End If
                RetracePoints.Push(EndHeadShaftUpperPoint)

                'Draw the starting arrow points
                If IsStartArrow AndAlso SpanItem Is Spans.First Then
                    Dim HeadUpperPoint, HeadLowerPoint As Point
                    If IsCircular Then
                        HeadUpperPoint = PolarToCartesian(StartHeadPosition, UpperHead)
                        StartHeadPoint = PolarToCartesian(StartPosition, Radius)
                        HeadLowerPoint = PolarToCartesian(StartHeadPosition, LowerHead)
                    Else
                        Dim HalfHeadWidth = HalfShaftWidth + HeadExtension
                        HeadUpperPoint = New Point(StartHeadPosition, UpperHead)
                        StartHeadPoint = New Point(StartPosition, 0)
                        HeadLowerPoint = New Point(StartHeadPosition, LowerHead)
                    End If

                    HeadUpperPoint.Offset(CenterX, CenterY)
                    StartHeadPoint.Offset(CenterX, CenterY)
                    HeadLowerPoint.Offset(CenterX, CenterY)

                    Context.LineTo(HeadUpperPoint, True, True)
                    Context.LineTo(StartHeadPoint, True, True)
                    Context.LineTo(HeadLowerPoint, True, True)
                End If

                Context.LineTo(StartHeadShaftLowerPoint, True, True)

                'Draw the ending arrow points
                If IsCircular Then
                    Context.ArcTo(MidLowerPoint, New Size(LowerShaft, LowerShaft), 0, IsLargeArc, Sweep1, True, True)
                    Context.ArcTo(EndHeadShaftLowerPoint, New Size(LowerShaft, LowerShaft), 0, IsLargeArc, Sweep1, True, True)
                Else
                    Context.LineTo(EndHeadShaftLowerPoint, True, True)
                End If

                If IsEndArrow AndAlso SpanItem Is Spans.Last Then
                    Dim HeadUpperPoint, HeadLowerPoint As Point
                    If IsCircular Then
                        HeadUpperPoint = PolarToCartesian(EndHeadPosition, UpperHead)
                        EndHeadPoint = PolarToCartesian(EndPosition, Radius)
                        HeadLowerPoint = PolarToCartesian(EndHeadPosition, LowerHead)
                    Else
                        HeadUpperPoint = New Point(EndHeadPosition, UpperHead)
                        EndHeadPoint = New Point(EndPosition, 0)
                        HeadLowerPoint = New Point(EndHeadPosition, LowerHead)
                    End If

                    HeadUpperPoint.Offset(CenterX, CenterY)
                    EndHeadPoint.Offset(CenterX, CenterY)
                    HeadLowerPoint.Offset(CenterX, CenterY)

                    Context.LineTo(HeadLowerPoint, True, True)
                    Context.LineTo(EndHeadPoint, True, True)
                    Context.LineTo(HeadUpperPoint, True, True)

                End If

                Context.LineTo(EndHeadShaftUpperPoint, True, True)

                'Step back through stored points
                If Spans.IndexOf(SpanItem) = Spans.Count - 1 Then
                    RetracePoints.Pop()
                    Dim MidPoint, StartPoint, JoinPoint, PreviousStartPoint As Point

                    If Spans.Count > 1 Then
                        Do
                            If IsCircular Then
                                MidPoint = RetracePoints.Pop
                            End If
                            StartPoint = RetracePoints.Pop
                            If IsJoined Then
                                JoinPoint = RetracePoints.Pop
                            End If
                            PreviousStartPoint = RetracePoints.Pop

                            If IsCircular Then
                                Context.ArcTo(MidPoint, New Size(UpperShaft, UpperShaft), 0, IsLargeArc, Sweep2, True, True)
                                Context.ArcTo(StartPoint, New Size(UpperShaft, UpperShaft), 0, IsLargeArc, Sweep2, True, True)
                            Else
                                Context.LineTo(StartPoint, True, True)
                            End If

                            If IsJoined Then
                                Context.LineTo(JoinPoint, False, False)
                            End If

                            Context.LineTo(PreviousStartPoint, False, False)

                        Loop Until (IsCircular AndAlso RetracePoints.Count = 2) OrElse (Not IsCircular AndAlso RetracePoints.Count = 1)
                    End If
                    If IsCircular Then
                        Context.ArcTo(RetracePoints.Pop, New Size(UpperShaft, UpperShaft), 0, IsLargeArc, Sweep2, True, True)
                        Context.ArcTo(RetracePoints.Pop, New Size(UpperShaft, UpperShaft), 0, IsLargeArc, Sweep2, True, True)
                    Else
                        Context.LineTo(RetracePoints.Pop, True, True)
                    End If
                Else
                    PreviousEndPosition = EndPosition
                End If
            End With
        Next
    End Sub

    Private Sub DrawBracket(Context As StreamGeometryContext)
        If Radius = 0 Then Exit Sub

        If IsCircular Then
            Dim IsLargeArc As Boolean

            For Each Span As Span In Spans
                With Span
                    Dim StartAngle As Double = FractionToAngle(.StartFraction)
                    Dim EndAngle As Double = FractionToAngle(.EndFraction)
                    Dim MidAngle As Double
                    Dim SpanAngle As Double = EndAngle - StartAngle

                    If .SpansOrigin Then
                        SpanAngle = -Sign(SpanAngle) * (360 - Abs(SpanAngle))
                        MidAngle = NormalizeAngle(StartAngle + SpanAngle / 2)
                    Else
                        MidAngle = StartAngle + SpanAngle / 2
                    End If

                    Dim Sweep As SweepDirection

                    If SpanAngle > 0 Then
                        Sweep = SweepDirection.Clockwise
                    Else
                        Sweep = SweepDirection.Counterclockwise
                    End If

                    IsLargeArc = Abs(SpanAngle) / 2 > 180

                    Dim OuterShaftRadius As Double = Radius
                    Dim InnerShaftRadius As Double = Radius - ShaftWidth

                    'Midpoints are necessary to allow for arcs > half the circle
                    Dim StartOuterPoint As Point = PolarToCartesian(StartAngle, OuterShaftRadius)
                    Dim StartInnerPoint As Point = PolarToCartesian(StartAngle, InnerShaftRadius)
                    Dim MidOuterPoint As Point = PolarToCartesian(MidAngle, OuterShaftRadius)
                    Dim EndOuterPoint As Point = PolarToCartesian(EndAngle, OuterShaftRadius)
                    Dim EndInnerPoint As Point = PolarToCartesian(EndAngle, InnerShaftRadius)

                    StartOuterPoint.Offset(CenterX, CenterY)
                    StartInnerPoint.Offset(CenterX, CenterY)
                    MidOuterPoint.Offset(CenterX, CenterY)
                    EndOuterPoint.Offset(CenterX, CenterY)
                    EndInnerPoint.Offset(CenterX, CenterY)

                    If Spans.IndexOf(Span) > 0 Then
                        Context.LineTo(StartInnerPoint, False, False)
                    Else
                        Context.BeginFigure(StartInnerPoint, False, False)
                    End If

                    Context.LineTo(StartOuterPoint, True, True)
                    Context.ArcTo(MidOuterPoint, New Size(OuterShaftRadius, OuterShaftRadius), 0, IsLargeArc, Sweep, True, True)
                    Context.ArcTo(EndOuterPoint, New Size(OuterShaftRadius, OuterShaftRadius), 0, IsLargeArc, Sweep, True, True)
                    Context.LineTo(EndInnerPoint, True, True)
                End With
            Next
        Else
            For Each Span As Span In Spans
                Dim Length As Double = Radius * 2
                With Span
                    Dim StartPosition As Double = Length * (.StartFraction - 0.5)
                    Dim EndPosition As Double = Length * (.EndFraction - 0.5)
                    Dim SpanPosition As Double = EndPosition - StartPosition

                    Dim StartUpperPoint As New Point(StartPosition, -ShaftWidth)
                    Dim StartLowerPoint As New Point(StartPosition, 0)
                    Dim EndUpperPoint As New Point(EndPosition, -ShaftWidth)
                    Dim EndLowerPoint As New Point(EndPosition, 0)

                    StartUpperPoint.Offset(CenterX, CenterY)
                    StartLowerPoint.Offset(CenterX, CenterY)
                    EndUpperPoint.Offset(CenterX, CenterY)
                    EndLowerPoint.Offset(CenterX, CenterY)

                    If Spans.IndexOf(Span) > 0 Then
                        Context.LineTo(StartLowerPoint, False, False)
                    Else
                        Context.BeginFigure(StartLowerPoint, False, False)
                    End If

                    Context.LineTo(StartUpperPoint, True, True)
                    Context.LineTo(EndUpperPoint, True, True)
                    Context.LineTo(EndLowerPoint, True, True)
                End With
            Next
        End If
    End Sub

    'Private Sub DrawLine(Context As StreamGeometryContext)
    '    IsArrow = True
    '    If IsCircular Then
    '        Dim IsLargeArc As Boolean

    '        For Each Span As Span In Spans
    '            With Span
    '                Dim StartAngle As Double = FractionToAngle(.StartFraction)
    '                Dim EndAngle As Double = FractionToAngle(.EndFraction)
    '                Dim SpanAngle As Double = EndAngle - StartAngle

    '                Dim HeadSpanAngle As Double
    '                Dim Sweep As SweepDirection

    '                If SpanAngle > 0 Then
    '                    Sweep = SweepDirection.Clockwise
    '                Else
    '                    Sweep = SweepDirection.Counterclockwise
    '                End If

    '                If .SpansOrigin Then
    '                    SpanAngle = -Sign(SpanAngle) * (360 - Abs(SpanAngle))
    '                End If

    '                If IsArrow AndAlso Span Is Spans.Last Then
    '                    HeadSpanAngle = ComputeAngle(HeadLength, Radius)

    '                    If HeadSpanAngle > Abs(SpanAngle) Then
    '                        HeadSpanAngle = SpanAngle
    '                    ElseIf SpanAngle < 0 Then
    '                        HeadSpanAngle *= -1
    '                    End If
    '                Else
    '                    HeadSpanAngle = 0
    '                End If

    '                IsLargeArc = Abs(SpanAngle) / 2 > 180

    '                Dim LineRadius As Double = Radius + ShaftWidth + HeadExtension + Displacement
    '                Dim HeadRadius As Double = LineRadius + HeadExtension

    '                Dim HeadAngle As Double = NormalizeAngle(StartAngle + SpanAngle - HeadSpanAngle)
    '                Dim MidAngle As Double = NormalizeAngle(StartAngle + SpanAngle / 2)

    '                'Midpoints are necessary to allow for arcs > half the circle

    '                Dim EndPoint As Point = PolarToCartesian(EndAngle, LineRadius)
    '                Dim StartPoint As Point = PolarToCartesian(StartAngle, LineRadius)
    '                Dim MidPoint As Point = PolarToCartesian(MidAngle, LineRadius)

    '                EndPoint.Offset(CenterXOffset, CenterYOffset)
    '                StartPoint.Offset(CenterXOffset, CenterYOffset)
    '                MidPoint.Offset(CenterXOffset, CenterYOffset)

    '                'If this isn't the first shape, draw the connecting line (only visible if IsJoined = True), else start the figure
    '                If Spans.IndexOf(Span) > 0 Then
    '                    Context.LineTo(StartPoint, False, False)
    '                Else
    '                    Context.BeginFigure(StartPoint, False, False)
    '                End If

    '                Context.ArcTo(MidPoint, New Size(LineRadius, LineRadius), 0, IsLargeArc, Sweep, True, True)
    '                Context.ArcTo(EndPoint, New Size(LineRadius, LineRadius), 0, IsLargeArc, Sweep, True, True)

    '                If IsArrow AndAlso Span Is Spans.Last Then
    '                    Dim HeadPoint As Point = PolarToCartesian(HeadAngle, HeadRadius)

    '                    HeadPoint.Offset(CenterXOffset, CenterYOffset)

    '                    Context.LineTo(HeadPoint, True, True)
    '                End If

    '            End With
    '        Next
    '    Else
    '        'Note that negative Y values are up, positive Y values are down

    '        For Each Span As Span In Spans
    '            Dim Length As Double = Radius * 2
    '            With Span
    '                Dim StartPosition As Double = Length * .StartFraction
    '                Dim EndPosition As Double = Length * .EndFraction
    '                Dim SpanPosition As Double = EndPosition - StartPosition
    '                Dim HeadSpan As Double

    '                If IsArrow AndAlso Span Is Spans.Last Then
    '                    HeadSpan = HeadLength
    '                    If HeadSpan > Abs(SpanPosition) Then
    '                        HeadSpan = SpanPosition
    '                    ElseIf SpanPosition < 0 Then
    '                        HeadSpan *= -1
    '                    End If
    '                Else
    '                    HeadSpan = 0
    '                End If

    '                Dim HeadPosition = StartPosition + SpanPosition - HeadSpan

    '                Dim EndPoint As New Point(EndPosition, -ShaftWidth - HeadExtension)
    '                Dim StartPoint As New Point(StartPosition, -ShaftWidth - HeadExtension)

    '                EndPoint.Offset(CenterXOffset, CenterYOffset)
    '                StartPoint.Offset(CenterXOffset, CenterYOffset)

    '                'If this isn't the first shape, draw the connecting line (only visible if IsJoined = True), else start the figure
    '                If Spans.IndexOf(Span) > 0 Then
    '                    Context.LineTo(StartPoint, False, False)
    '                Else
    '                    Context.BeginFigure(StartPoint, False, False)
    '                End If

    '                Context.LineTo(EndPoint, True, True)

    '                If IsArrow AndAlso Span Is Spans.Last Then
    '                    Dim HeadPoint As New Point(HeadPosition, -2 * HeadExtension - ShaftWidth)

    '                    HeadPoint.Offset(CenterXOffset, CenterYOffset)

    '                    Context.LineTo(HeadPoint, True, True)
    '                End If

    '            End With
    '        Next
    '    End If
    'End Sub

    Friend Class Span
        Friend StartFraction As Double
        Friend EndFraction As Double
        Friend UncertainStart As Boolean
        Friend UncertainEnd As Boolean
        Friend FragmentStart As Boolean
        Friend FragmentEnd As Boolean
        Friend SpansOrigin As Boolean
    End Class

    Friend Enum ShapeTypes
        'None
        Block
        'BlockArrow
        Bracket
        Squiggle
        Astrix
        Line
    End Enum

    Friend Enum ArrowTypes
        None
        StartArrow
        EndArrow
        BothArrow
    End Enum
End Class