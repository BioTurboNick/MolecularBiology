Class Application
    Friend Shared NucleicAcidCharacters As Char() = {CChar("G"), CChar("A"), CChar("T"), CChar("C"), CChar("R"), CChar("Y"), CChar("M"), CChar("K"), CChar("S"), CChar("W"), CChar("H"), CChar("B"), CChar("V"), CChar("D"), CChar("N"),
                                      CChar("g"), CChar("a"), CChar("t"), CChar("c"), CChar("r"), CChar("y"), CChar("m"), CChar("k"), CChar("s"), CChar("w"), CChar("h"), CChar("b"), CChar("v"), CChar("d"), CChar("n")}
    Friend Shared AminoAcidCharacters As Char() = {CChar("A"), CChar("V"), CChar("L"), CChar("I"), CChar("G"), CChar("P"), CChar("T"), CChar("S"), CChar("Y"), CChar("H"), CChar("N"), CChar("Q"), CChar("D"), CChar("E"), CChar("K"), CChar("R"), CChar("F"), CChar("W"), CChar("C"), CChar("M"), CChar("X"), CChar("*"),
                                            CChar("a"), CChar("v"), CChar("l"), CChar("i"), CChar("g"), CChar("p"), CChar("t"), CChar("s"), CChar("y"), CChar("h"), CChar("n"), CChar("q"), CChar("d"), CChar("e"), CChar("k"), CChar("r"), CChar("f"), CChar("w"), CChar("c"), CChar("m"), CChar("x")}
    Friend Shared OpenMacromolecules As New List(Of Macromolecule)
    ' Application-level events, such as Startup, Exit, and DispatcherUnhandledException
    ' can be handled in this file.


End Class
