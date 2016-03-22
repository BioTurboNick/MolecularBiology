Imports System.Collections.ObjectModel
Imports System.ComponentModel
Imports System.Collections.Specialized

Friend Class Macromolecule
    Implements INotifyPropertyChanged
#Region "Properties"
    Private LocusNamePrivate As String = "" 'GenBank; Limited to 16 characters
    Private SequenceLengthPrivate As Integer
    Private TypePrivate As MacromoleculeTypes
    Private TypeModifierPrivate As MacromoleculeTypeModifiers
    Private StrandednessPrivate As StrandednessTypes
    Private IsCircularPrivate As Boolean
    Private DateModifiedPrivate As DateTime
    Private DescriptionPrivate As String '"Definition" in GenBank
    Private KeywordsPrivate As New List(Of String)
    Private SourcePrivate As String = ""
    Private TaxonomyPrivate As New List(Of String)
    Private ReferencesPrivate As New Dictionary(Of Integer, Reference)
    Private FeaturesPrivate As New FeatureObservableCollection
    Private InfoFeaturesPrivate As New FeatureObservableCollection
    Private SourceFeaturesPrivate As New FeatureObservableCollection
    Private CommentsPrivate As String = ""
    Private SequencePrivate As String = ""
    Private VolatileInformationPrivate As Volatile 'This contains information in a GenBank file that would become invalid if the molecule were modified.
    'Segment and Contig information will be used only to stitch together multiple files into a single Macromolecule
    'DDBJ format is essentially identical to GenBank except that it retains the BASE COUNT line.

    Public Property LocusName As String
        Get
            Return LocusNamePrivate
        End Get
        Set(value As String)
            LocusNamePrivate = value
            NotifyPropertyChanged("LocusName")
        End Set
    End Property

    Public Property SequenceLength As Integer
        Get
            Return SequenceLengthPrivate
        End Get
        Set(value As Integer)
            SequenceLengthPrivate = value
            NotifyPropertyChanged("SequenceLength")
        End Set
    End Property

    Public Property Type As MacromoleculeTypes
        Get
            Return TypePrivate
        End Get
        Set(value As MacromoleculeTypes)
            TypePrivate = value
            NotifyPropertyChanged("Type")
        End Set
    End Property

    Public Property TypeModifier As MacromoleculeTypeModifiers
        Get
            Return TypeModifierPrivate
        End Get
        Set(value As MacromoleculeTypeModifiers)
            TypeModifierPrivate = value
            NotifyPropertyChanged("TypeModifier")
        End Set
    End Property

    Public Property Strandedness As StrandednessTypes
        Get
            Return StrandednessPrivate
        End Get
        Set(value As StrandednessTypes)
            StrandednessPrivate = value
            NotifyPropertyChanged("Strandedness")
        End Set
    End Property

    Public Property IsCircular As Boolean
        Get
            Return IsCircularPrivate
        End Get
        Set(value As Boolean)
            IsCircularPrivate = value
            NotifyPropertyChanged("IsCircular")
        End Set
    End Property

    Public Property DateModified As DateTime
        Get
            Return DateModifiedPrivate
        End Get
        Set(value As DateTime)
            DateModifiedPrivate = value
            NotifyPropertyChanged("DateModified")
        End Set
    End Property

    Public Property Description As String
        Get
            Return DescriptionPrivate
        End Get
        Set(value As String)
            DescriptionPrivate = value
            NotifyPropertyChanged("Description")
        End Set
    End Property

    Public Property Keywords As List(Of String)
        Get
            Return KeywordsPrivate
        End Get
        Set(value As List(Of String))
            KeywordsPrivate = value
            NotifyPropertyChanged("Keywords")
        End Set
    End Property

    Public Property Source As String
        Get
            Return SourcePrivate
        End Get
        Set(value As String)
            SourcePrivate = value
            NotifyPropertyChanged("Source")
        End Set
    End Property

    Public Property Taxonomy As List(Of String)
        Get
            Return TaxonomyPrivate
        End Get
        Set(value As List(Of String))
            TaxonomyPrivate = value
            NotifyPropertyChanged("Taxonomy")
        End Set
    End Property

    Public Property References As Dictionary(Of Integer, Reference)
        Get
            Return ReferencesPrivate
        End Get
        Set(value As Dictionary(Of Integer, Reference))
            ReferencesPrivate = value
            NotifyPropertyChanged("References")
        End Set
    End Property

    Public Property Features As FeatureObservableCollection
        Get
            Return FeaturesPrivate
        End Get
        Set(value As FeatureObservableCollection)
            FeaturesPrivate = value
            NotifyPropertyChanged("Features")
        End Set
    End Property

    Public Property InfoFeatures As FeatureObservableCollection
        Get
            Return InfoFeaturesPrivate
        End Get
        Set(value As FeatureObservableCollection)
            InfoFeaturesPrivate = value
            NotifyPropertyChanged("InfoFeatures")
        End Set
    End Property

    Public Property SourceFeatures As FeatureObservableCollection
        Get
            Return SourceFeaturesPrivate
        End Get
        Set(value As FeatureObservableCollection)
            SourceFeaturesPrivate = value
            NotifyPropertyChanged("SourceFeatures")
        End Set
    End Property

    Public Property Comments As String
        Get
            Return CommentsPrivate
        End Get
        Set(value As String)
            CommentsPrivate = value
            NotifyPropertyChanged("Comments")
        End Set
    End Property

    Public Property Sequence As String
        Get
            Return SequencePrivate
        End Get
        Set(value As String)
            SequencePrivate = value
            NotifyPropertyChanged("Sequence")
        End Set
    End Property

    Public Property VolatileInformation As Volatile
        Get
            Return VolatileInformationPrivate
        End Get
        Set(value As Volatile)
            VolatileInformationPrivate = value
            NotifyPropertyChanged("VolatileInformation")
        End Set
    End Property
#End Region

    Friend Class Reference
        Implements INotifyPropertyChanged

        Private RelatedSequenceLocationsPrivate As New List(Of Tuple(Of Integer, Integer))
        Private AuthorsPrivate As New List(Of Author)
        Private TitlePrivate As String = ""
        Private PublicationPrivate As String = "" '"Journal" in GenBank; "Location" in EMBL
        Private ResourceCrossReferencesPrivate As New List(Of Tuple(Of String, String)) 'Pubmed; Medline (deprecated); DOI or Agricola (EMBL)
        Private ConsortiumPrivate As String = "" '"Group" in EMBL
        Private RemarkPrivate As String = "" '"Comment" in EMBL

        Public Property RelatedSequenceLocations As List(Of Tuple(Of Integer, Integer))
            Get
                Return RelatedSequenceLocationsPrivate
            End Get
            Set(value As List(Of Tuple(Of Integer, Integer)))
                RelatedSequenceLocationsPrivate = value
                NotifyPropertyChanged("RelatedSequenceLocations")
            End Set
        End Property

        Public Property Authors As List(Of Author)
            Get
                Return AuthorsPrivate
            End Get
            Set(value As List(Of Author))
                AuthorsPrivate = value
                NotifyPropertyChanged("Authors")
            End Set
        End Property

        Public Property Title As String
            Get
                Return TitlePrivate
            End Get
            Set(value As String)
                TitlePrivate = value
                NotifyPropertyChanged("Title")
            End Set
        End Property

        Public Property Publication As String
            Get
                Return PublicationPrivate
            End Get
            Set(value As String)
                PublicationPrivate = value
                NotifyPropertyChanged("Publication")
            End Set
        End Property

        Public Property ResourceCrossReferences As List(Of Tuple(Of String, String))
            Get
                Return ResourceCrossReferencesPrivate
            End Get
            Set(value As List(Of Tuple(Of String, String)))
                ResourceCrossReferencesPrivate = value
                NotifyPropertyChanged("ResourceCrossReferences")
            End Set
        End Property

        Public Property Consortium As String
            Get
                Return ConsortiumPrivate
            End Get
            Set(value As String)
                ConsortiumPrivate = value
                NotifyPropertyChanged("Consortium")
            End Set
        End Property

        Public Property Remark As String
            Get
                Return RemarkPrivate
            End Get
            Set(value As String)
                RemarkPrivate = value
                NotifyPropertyChanged("Remark")
            End Set
        End Property

        Friend Class Author
            Friend LastName As String = ""
            Friend FirstInitials As New List(Of String)
            Friend Suffix As String = ""
        End Class

        Friend Event PropertyChanged(sender As Object, e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

        Private Sub NotifyPropertyChanged(ByVal propertyName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
        End Sub
    End Class

    Friend Class Feature
        Implements INotifyPropertyChanged

#Region "Properties"
        Private TypePrivate As String
        Private IsJoinedPrivate As Boolean
        Friend HasRemoteParts As Boolean
        Private SequenceLocationsPrivate As New List(Of Location)
        Private CompletenessPrivate As CompletenessTypes
        Private QualifiersPrivate As New ObservableCollection(Of Qualifier)
        Private OverlapIndexPrivate As Integer
        Private ParentSequenceLengthPrivate As Integer

        Public Property Type As String
            Get
                Return TypePrivate
            End Get
            Set(value As String)
                TypePrivate = value
                NotifyPropertyChanged("Type")
            End Set
        End Property

        Public Property IsJoined As Boolean
            Get
                Return IsJoinedPrivate
            End Get
            Set(value As Boolean)
                IsJoinedPrivate = value
                NotifyPropertyChanged("IsJoined")
            End Set
        End Property

        Public Property SequenceLocations As List(Of Location)
            Get
                Return SequenceLocationsPrivate
            End Get
            Set(value As List(Of Location))
                SequenceLocationsPrivate = value
                NotifyPropertyChanged("SequenceLocations")
            End Set
        End Property

        Public Property Completeness As CompletenessTypes
            Get
                Return CompletenessPrivate
            End Get
            Set(value As CompletenessTypes)
                CompletenessPrivate = value
                NotifyPropertyChanged("Completeness")
            End Set
        End Property

        Public Property Qualifiers As ObservableCollection(Of Qualifier)
            Get
                Return QualifiersPrivate
            End Get
            Set(value As ObservableCollection(Of Qualifier))
                QualifiersPrivate = value
                NotifyPropertyChanged("Qualifiers")
            End Set
        End Property

        Public Property OverlapIndex As Integer
            Get
                Return OverlapIndexPrivate
            End Get
            Set(value As Integer)
                OverlapIndexPrivate = value
                NotifyPropertyChanged("OverlapIndex")
            End Set
        End Property

        Public Property ParentSequenceLength As Integer
            Get
                Return ParentSequenceLengthPrivate
            End Get
            Set(value As Integer)
                ParentSequenceLengthPrivate = value
                NotifyPropertyChanged("FeatureLength")
            End Set
        End Property

        Public ReadOnly Property FeatureLength As Integer
            Get
                Dim FeatureLengthPrivate As Integer = 0

                For Each LocationItem As Location In SequenceLocations
                    With LocationItem
                        If .SpansOrigin Then
                            FeatureLengthPrivate += ParentSequenceLengthPrivate - Math.Max(.StartBase, .EndBase) - Math.Min(.StartBase, .EndBase) + 2
                        Else
                            FeatureLengthPrivate += Math.Max(.StartBase, .EndBase) - Math.Min(.StartBase, .EndBase) + 1
                        End If
                    End With
                Next

                Return FeatureLengthPrivate
            End Get
        End Property

#End Region

        Friend Class Qualifier
            Implements INotifyPropertyChanged

            Private TypePrivate As String = ""
            Private DataPrivate As String = ""

            Friend Property Type As String
                Get
                    Return TypePrivate
                End Get
                Set(value As String)
                    TypePrivate = value
                    NotifyPropertyChanged("Type")
                End Set
            End Property

            Friend Property Data As String
                Get
                    Return DataPrivate
                End Get
                Set(value As String)
                    DataPrivate = value
                    NotifyPropertyChanged("Data")
                End Set
            End Property

            Friend Event PropertyChanged(sender As Object, e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

            Private Sub NotifyPropertyChanged(ByVal propertyName As String)
                RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
            End Sub
        End Class

        Friend Class Location
            Implements INotifyPropertyChanged

            Private IsBetweenPrivate As Boolean
            Private SpansOriginPrivate As Boolean
            Private RemoteAccessionPrivate As String = ""
            Private StartIsBeyondPrivate As Boolean
            Private EndIsBeyondPrivate As Boolean
            Private IsComplementPrivate As Boolean
            Private StartBasePrivate As Integer
            Private EndBasePrivate As Integer
            Private IsRemotePrivate As Boolean

            Public Property IsBetween As Boolean
                Get
                    Return IsBetweenPrivate
                End Get
                Set(value As Boolean)
                    IsBetweenPrivate = value
                End Set
            End Property

            Public Property StartBase As Integer
                Get
                    Return StartBasePrivate
                End Get
                Set(value As Integer)
                    StartBasePrivate = value
                    NotifyPropertyChanged("StartBase")
                    OnBasesChanged()
                End Set
            End Property

            Public Property StartIsBeyond As Boolean
                Get
                    Return StartIsBeyondPrivate
                End Get
                Set(value As Boolean)
                    StartIsBeyondPrivate = value
                    NotifyPropertyChanged("StartIsBeyond")
                End Set
            End Property

            Public Property EndBase As Integer
                Get
                    Return EndBasePrivate
                End Get
                Set(value As Integer)
                    EndBasePrivate = value
                    NotifyPropertyChanged("EndBase")
                    OnBasesChanged()
                End Set
            End Property

            Public Property EndIsBeyond As Boolean
                Get
                    Return EndIsBeyondPrivate
                End Get
                Set(value As Boolean)
                    EndIsBeyondPrivate = value
                    NotifyPropertyChanged("EndIsBeyond")
                End Set
            End Property

            Public Property SpansOrigin As Boolean
                Get
                    Return SpansOriginPrivate
                End Get
                Set(value As Boolean)
                    SpansOriginPrivate = value
                    NotifyPropertyChanged("SpansOrigin")
                    OnBasesChanged()
                End Set
            End Property

            Friend ReadOnly Property IsComplement As Boolean
                Get
                    Return IsComplementPrivate
                End Get
            End Property

            Public Property RemoteAccession As String
                Get
                    Return RemoteAccessionPrivate
                End Get
                Set(value As String)
                    RemoteAccessionPrivate = value
                    OnRemoteAccessionSet()
                    NotifyPropertyChanged("RemoteAccession")
                End Set
            End Property

            Friend ReadOnly Property IsRemote As Boolean
                Get
                    Return IsRemotePrivate
                End Get
            End Property

            Private Sub OnBasesChanged()
                Dim OldIsComplement As Boolean = IsComplementPrivate

                IsComplementPrivate = (EndBasePrivate < StartBasePrivate) Xor SpansOriginPrivate

                If OldIsComplement <> IsBetweenPrivate Then
                    NotifyPropertyChanged("IsComplement")
                End If
            End Sub

            Private Sub OnRemoteAccessionSet()
                IsRemotePrivate = RemoteAccession <> ""
            End Sub

            Friend Event PropertyChanged(sender As Object, e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

            Private Sub NotifyPropertyChanged(ByVal propertyName As String)
                RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
            End Sub
        End Class

        Enum CompletenessTypes As Byte
            Full
            StartOnly
            EndOnly
            Internal
        End Enum

        Friend Event PropertyChanged(sender As Object, e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

        Private Sub NotifyPropertyChanged(ByVal propertyName As String)
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
        End Sub

    End Class

    Friend Class FeatureObservableCollection
        Inherits ObservableCollection(Of Feature)
        Private ParentSequenceLengthPrivate As Integer

        Public Property ParentSequenceLength As Integer
            Get
                Return ParentSequenceLengthPrivate
            End Get
            Set(value As Integer)
                ParentSequenceLengthPrivate = value
            End Set
        End Property

        Private SuppressNotification As Boolean = False

        Protected Overrides Sub OnCollectionChanged(e As System.Collections.Specialized.NotifyCollectionChangedEventArgs)

            If SuppressNotification Then Exit Sub

            'I need to make sure this can function correctly regardless of the order of the features

            MyBase.OnCollectionChanged(e)

            'Reset all OverrideIndicies and recalculate
            Dim Features As IList(Of Feature) = MyBase.Items

            For Each FeatureItem As Feature In Features
                FeatureItem.OverlapIndex = 0
            Next

            Dim OverlappingFeatures As New List(Of List(Of Integer))

            Dim LengthExtension As Integer = CInt(Me.ParentSequenceLength * 0.001)

            For Each FeatureItem1 As Feature In Features
                Dim FeatureItem1Overlaps As New List(Of Integer)
                For Index As Integer = Features.IndexOf(FeatureItem1) + 1 To Features.Count - 1
                    Dim FeatureItem2 As Feature = Features(Index)
                    Dim IsOverlapping As Boolean
                    Dim MinBase1, MaxBase1 As Integer

                    For Each Location1 As Feature.Location In FeatureItem1.SequenceLocations
                        With Location1
                            MinBase1 = Math.Min(.StartBase, .EndBase)
                            MaxBase1 = Math.Max(.StartBase, .EndBase)

                            If .SpansOrigin Then
                                MinBase1 += LengthExtension
                                MaxBase1 -= LengthExtension
                            Else
                                MinBase1 -= LengthExtension
                                MaxBase1 += LengthExtension
                            End If
                        End With

                        For Each Location2 As Feature.Location In FeatureItem2.SequenceLocations
                            Dim MinBase2, MaxBase2 As Integer
                            With Location2
                                MinBase2 = Math.Min(Location2.StartBase, Location2.EndBase)
                                MaxBase2 = Math.Max(Location2.StartBase, Location2.EndBase)

                                If Location2.SpansOrigin Then
                                    MinBase2 += LengthExtension
                                    MaxBase2 -= LengthExtension
                                Else
                                    MinBase2 -= LengthExtension
                                    MaxBase2 += LengthExtension
                                End If
                            End With

                            If Location1.SpansOrigin Then
                                If (MinBase2 >= MaxBase1 OrElse MinBase2 <= MinBase1) OrElse (MaxBase2 >= MaxBase1 OrElse MaxBase2 <= MinBase1) Then
                                    IsOverlapping = True
                                End If
                            ElseIf (MinBase2 >= MinBase1 AndAlso MinBase2 <= MaxBase1) OrElse (MaxBase2 >= MinBase1 AndAlso MaxBase2 <= MaxBase1) Then
                                IsOverlapping = True
                            End If

                            If IsOverlapping Then Exit For

                        Next

                        If IsOverlapping Then Exit For

                    Next

                    If IsOverlapping Then
                        FeatureItem1Overlaps.Add(Index)
                        IsOverlapping = False
                    End If
                Next

                If FeatureItem1Overlaps.Count > 0 Then
                    FeatureItem1Overlaps.Insert(0, Features.IndexOf(FeatureItem1))
                    OverlappingFeatures.Add(FeatureItem1Overlaps)
                End If
            Next

            For Each OverlapList As List(Of Integer) In OverlappingFeatures
                Dim FeatureItem1 As Feature = Features(OverlapList(0))

                For Index As Integer = 1 To OverlapList.Count - 1
                    Dim FeatureItem2 As Feature = Features(OverlapList(Index))
                    If FeatureItem1.OverlapIndex = FeatureItem2.OverlapIndex Then
                        If FeatureItem1.FeatureLength > FeatureItem2.FeatureLength Then
                            FeatureItem2.OverlapIndex += 1
                        ElseIf FeatureItem1.FeatureLength < FeatureItem2.FeatureLength Then
                            FeatureItem1.OverlapIndex += 1
                        Else
                            Dim FullMinBase1, FullMinBase2 As Integer

                            For Each Location1 As Feature.Location In FeatureItem1.SequenceLocations
                                FullMinBase1 = Math.Min(FullMinBase1, Math.Min(Location1.StartBase, Location1.EndBase))
                            Next

                            For Each Location2 As Feature.Location In FeatureItem1.SequenceLocations
                                FullMinBase2 = Math.Min(FullMinBase2, Math.Min(Location2.StartBase, Location2.EndBase))
                            Next

                            If FullMinBase1 < FullMinBase2 Then
                                FeatureItem2.OverlapIndex += 1
                            ElseIf FullMinBase1 > FullMinBase2 Then
                                FeatureItem1.OverlapIndex += 1
                            Else
                                Dim FullMaxBase1, FullMaxBase2 As Integer

                                For Each Location1 As Feature.Location In FeatureItem1.SequenceLocations
                                    FullMaxBase1 = Math.Min(FullMaxBase1, Math.Max(Location1.StartBase, Location1.EndBase))
                                Next

                                For Each Location2 As Feature.Location In FeatureItem1.SequenceLocations
                                    FullMaxBase2 = Math.Min(FullMaxBase2, Math.Max(Location2.StartBase, Location2.EndBase))
                                Next

                                If FullMaxBase1 > FullMaxBase2 Then
                                    FeatureItem2.OverlapIndex += 1
                                ElseIf FullMaxBase1 < FullMaxBase2 Then
                                    FeatureItem1.OverlapIndex += 1
                                Else
                                    FeatureItem2.OverlapIndex += 1
                                End If
                            End If
                        End If
                    End If

                Next
            Next


            MyBase.OnCollectionChanged(e)
        End Sub

        Friend Sub AddRange(Items As IEnumerable(Of Macromolecule.Feature))
            SuppressNotification = True

            For Each Item As Macromolecule.Feature In Items
                Add(Item)
            Next

            SuppressNotification = False

            OnCollectionChanged(New NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset))
        End Sub
    End Class

    Friend Class Volatile
        Friend AccessionNumbers As New List(Of String)
        Friend Version As Integer
        Friend GenInfoIdentifier As Integer 'GenBank
        Friend Division As String = ""
        Friend DataClass As String = "" 'EMBL
        Friend DateCreated As DateTime 'EMBL
        Friend DatabaseCrossReferences As New List(Of Tuple(Of String, String, String)) 'database identifier, accession number, (EMBL) secondary accession
        Friend Assembly As New List(Of Tuple(Of Integer, Integer, String, Integer, Integer)) '"Primary" in GenBank; Third Party Annotation and Transcriptome Shotgun Assembly records; local start, local end, remote accession number, remote start, remote end
        Friend OriginData As String = ""
    End Class

    Enum MacromoleculeTypes As Byte
        NA
        DNA
        RNA
        Protein
    End Enum

    Enum MacromoleculeTypeModifiers As Byte
        None
        Genomic 'Includes plasmids
        Translating 'tRNA
        Ribosomal 'rRNA
        Messenger 'mRNA
        Viral 'Viral RNA transcribed from cDNA
        Other 'Used for synthetic molecules
        Unassigned 'Used for hypothetical molecules not observed
        Transcribed 'RNA that isn't t, r, m, or c, but is known to be transcribed (e.g. miRNA, snRNA)
    End Enum

    Enum StrandednessTypes As Byte
        None
        SingleStranded
        DoubleStranded
        MixedStranded
    End Enum

    Friend Event PropertyChanged(sender As Object, e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged

    Private Sub NotifyPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

End Class


Friend Enum FileTypes As Byte
    'http://www.hiv.lanl.gov/content/sequence/HelpDocs/SEQsamples.html
    Raw
    FASTA
    GCG
    GenBank_DDBJ 'DDBJ will also fall under this
    EMBL
    NBRF_PIR

    'Other: NEXUS, Clustal, Phylip, MSF, ASN.1, IG, MASE, GDE, Mega, RSF
End Enum