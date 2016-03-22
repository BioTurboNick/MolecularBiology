Friend Class CollapsableCircle
    Inherits DiagramShapeBase

    Protected Overrides ReadOnly Property DefiningGeometry As System.Windows.Media.Geometry
        Get
            Dim Geometry As Geometry

            If IsCircular Then
                Geometry = New EllipseGeometry(New Point(CenterX, CenterY), Radius, Radius)
            Else
                Dim StartPoint As New Point(-Radius, 0)
                Dim EndPoint As New Point(Radius, 0)

                StartPoint.Offset(CenterX, CenterY)
                EndPoint.Offset(CenterX, CenterY)

                Geometry = New LineGeometry(StartPoint, EndPoint)
            End If

            Geometry.Freeze()

            Return Geometry
        End Get
    End Property

End Class