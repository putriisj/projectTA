Public Class algoritma
    Public V1 As Double 'V adalah tegangan setiap sensor
    Public V2 As Double
    Public V3 As Double
    Public V4 As Double
    Public Var_Cost(3) As Double
    Public GBD(3) As Double
    Public Beban1 As Double
    Public Beban2 As Double
    Public Beban3 As Double
    Public Beban_total As Double
    Public Num(3) As Double
    Public pembagi_Var As Double
    Public sisa(4) As Double
    Public SK1 As Boolean
    Public SK2 As Boolean
    Public SK3 As Boolean

    Public Function hitungV(S1 As Integer, S2 As Integer, S3 As Integer, S4 As Integer) As Double
        V1 = S1 / 1200 'S adalah masukan sensor
        V2 = S2 / 1200
        V3 = S3 / 1400
        V4 = S4 / 1200
        Return V1
        Return V2
        Return V3
        Return V4
    End Function
    Public Function hitung_VarCost() As Double
        Var_Cost(1) = ((-0.0216 * V2 * V2) + (0.7552 * V2) - 0.7334) * 220 'satuan watt
        Var_Cost(2) = ((-0.0523 * V3 * V3) + (1.6733 * V3) - 1.4349) * 220
        Var_Cost(3) = ((-0.0403 * V4 * V4) + (0.7552 * V4) - 1.2077) * 220
        Return Var_Cost(1)
        Return Var_Cost(2)
        Return Var_Cost(3)
    End Function
    Public Function hitung_GBD() As Double
        GBD(1) = 80.0 / Var_Cost(1) 'Greedy By Density
        GBD(2) = 35 / Var_Cost(2)
        GBD(3) = 45 / Var_Cost(3)

        Dim i As Integer
        For i = 1 To 3
            If Var_Cost(i) < 55.0 Then
                Var_Cost(i) = 0.0
                GBD(i) = 0.0
            End If
        Next
        Return GBD(1)
        Return GBD(2)
        Return GBD(3)
    End Function
    Public Function hitung_beban() As Double
        Beban1 = Var_Cost(1)
        Beban2 = Var_Cost(2)
        Beban3 = Var_Cost(3)
        Beban_total = ((-0.0021 * V1 * V1) + (0.9563 * V1) - 0.6055) * 220

        If Beban_total < 30.0 Then
            Beban_total = 0.0
        End If

        Return Beban1
        Return Beban2
        Return Beban3
        Return Beban_total

    End Function
    Public Function hitung_Num() As Double
        Num(1) = GBD(1)
        Num(2) = GBD(2)
        Num(3) = GBD(3)

        Dim i As Integer
        Dim j As Integer
        Dim temp As Double
        For i = 1 To 29
            For j = i + 2 To 3
                If Num(i) < Num(j) Then
                    temp = Num(i)
                    Num(i) = Num(j)
                    Num(j) = temp
                End If
            Next
        Next
        For i = 1 To 3
            If Num(i) = GBD(1) Then
                Num(i) = Var_Cost(1) / 12
            ElseIf Num(i) = GBD(2) Then
                Num(i) = Var_Cost(2) / pembagi_Var
            ElseIf Num(i) = GBD(3) Then
                Num(i) = Var_Cost(3)
            End If
        Next

        Return Num(1)
        Return Num(2)
        Return Num(3)
    End Function
    Public Function hitung_sisa(KWH As Double) As Double
        sisa(0) = KWH * 1000
        Return sisa(0)
    End Function
    Sub isi()
        If Num(1) < sisa(0) Then
            sisa(1) = sisa(0) - Num(1)

            If Num(1) = Var_Cost(1) Then
                SK1 = False
                SK2 = True
                SK3 = True
                If Num(2) < sisa(1) Then
                    sisa(2) = sisa(1) - Num(2)
                    If Num(2) = Var_Cost(2) Then
                        SK2 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK3 = False
                        End If
                    ElseIf Num(2) = Var_Cost(3) Then
                        SK3 = False

                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK2 = False
                        End If
                    End If
                End If

            ElseIf Num(1) = Var_Cost(2) Then
                SK1 = True
                SK2 = False
                SK3 = True
                If Num(2) < sisa(1) Then
                    sisa(2) = sisa(1) - Num(2)
                    If Num(2) = Var_Cost(1) Then
                        SK1 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK3 = False
                        End If
                    ElseIf Num(2) = Var_Cost(3) Then
                        SK3 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK1 = False
                        End If
                    End If
                End If

            ElseIf Num(1) = Var_Cost(3) Then
                SK1 = True
                SK2 = True
                SK3 = False
                If Num(2) < sisa(1) Then
                    sisa(2) = sisa(1) - Num(2)
                    If Num(2) = Var_Cost(1) Then
                        SK1 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK1 = False
                        End If
                    ElseIf Num(2) = Var_Cost(2) Then
                        SK2 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK1 = False
                        End If
                    End If
                End If
            End If
        Else
            SK1 = True
            SK2 = True
            SK3 = True
        End If

        If SK2 = True Then
            pembagi_Var = pembagi_Var * 2.0
        End If
    End Sub
End Class
