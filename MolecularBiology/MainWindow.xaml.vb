Imports MolecularBiology2.Application

Friend Class MainWindow
    Public Shared ReadOnly CurrentMacromoleculeProperty As DependencyProperty = DependencyProperty.Register("CurrentMacromolecule", GetType(Macromolecule), GetType(MainWindow), New FrameworkPropertyMetadata(New Macromolecule, AddressOf OnCurrentMacromoleculeChanged))

    Friend Property CurrentMacromolecule As Macromolecule
        Get
            Return DirectCast(GetValue(CurrentMacromoleculeProperty), Macromolecule)
        End Get
        Set(value As Macromolecule)
            SetValue(CurrentMacromoleculeProperty, value)
        End Set
    End Property

    Private Shared Sub OnCurrentMacromoleculeChanged(ByVal d As DependencyObject, ByVal e As DependencyPropertyChangedEventArgs)
        If e.NewValue Is e.OldValue OrElse e.NewValue Is Nothing Then
            Exit Sub
        End If

        DirectCast(d, MainWindow).OnCurrentMacromoleculeChanged(d)
    End Sub

    Private Sub OnCurrentMacromoleculeChanged(ByVal d As DependencyObject)
        DataContext = CurrentMacromolecule
    End Sub

    Friend Sub New()

        InitializeComponent()

        Dim FileName As String = "..\..\pRS314.txt" '"..\..\testDNA.txt" '"..\..\ecoli.gb" '"..\..\GDA.gb" 
        Dim LoadingMacromolecules As Queue(Of Macromolecule) = FileOperations.OpenFile(FileName)

        Do
            OpenMacromolecules.Add(LoadingMacromolecules.Dequeue)
        Loop Until LoadingMacromolecules.Count = 0

        'OpenMacromolecules(0).IsCircular = False

        CurrentMacromolecule = OpenMacromolecules(0)

    End Sub

    Private Sub MainWindow_Closed(sender As Object, e As System.EventArgs) Handles Me.Closed
        OpenMacromolecules.Remove(CurrentMacromolecule)
    End Sub

End Class
