Public Class algoritma
    Public V1 As Double
    Public V2 As Double
    Public V3 As Double
    Public Var_Cost(3) As Double
    Public GBD(3) As Double
    Public Beban1 As Double
    Public Beban2 As Double
    Public Beban3 As Double
    Public Beban_total As Double
    Public Num(3) As Double
    Public pembagi_Var As Double
    Public sisa(3) As Double
    Public SK1 As Boolean
    Public SK2 As Boolean
    Public SK3 As Boolean

    Public Function hitung_beban(S1 As Integer, S2 As Integer, S3 As Integer, S4 As Integer, S5 As Integer) As Double
        'hitung tegangan setiap sensor
        V1 = S1 * 5 / 1023 'S1 adalah masukan sensor 1 
        V2 = S2 * 5 / 1023 'S2 adalah masukan sensor 2 
        V3 = S3 * 5 / 1023 'S3 adalah masukan sensor 3 

        'convert sensor ke kwh
        Var_Cost(1) = '((-0.0216 * V1 * V1) + (0.7552 * V1) - 0.7334) * 220 'ditentukan dari pengujian sensor
        Var_Cost(2) = '((-0.0523 * V2 * V2) + (1.6733 * V2) - 1.4349) * 220
        Var_Cost(3) = '((-0.0403 * V3 * V3) + (0.7552 * V3) - 1.2077) * 220


        'cari nilai Greedy By Density
        GBD(1) = 80.0 / Var_Cost(1)
        GBD(2) = 35.0 / Var_Cost(2)
        GBD(3) = 45.0 / Var_Cost(3)

        Dim i As Integer
        For i = 1 To 5
            If Var_Cost(i) < 55.0 Then
                Var_Cost(i) = 0.0
                GBD(i) = 0.0
            End If
        Next

        'konvert var_cost ke kwh
        Beban1 = Var_Cost(1)
        Beban2 = Var_Cost(2)
        Beban3 = Var_Cost(3)
        Beban_total = Beban1 + Beban2 + Beban3

        If Beban_total < 30.0 Then
            Beban_total = 0.0
        End If

        Return Beban1
        Return Beban2
        Return Beban3
        Return Beban_total

    End Function
    Public Function cari_kondisi(KWH As Double) As Double
        Num(1) = GBD(1)
        Num(2) = GBD(2)
        Num(3) = GBD(3)

        Dim i As Integer
        Dim j As Integer
        Dim temp As Double
        'sorting 
        For i = 1 To 2
            For j = i + 1 To 3
                If Num(i) < Num(j) Then
                    temp = Num(i)
                    Num(i) = Num(j)
                    Num(j) = temp
                End If
            Next
        Next

        For i = 1 To 3
            If Num(i) = GBD(1) Then
                Num(i) = Var_Cost(1)
            ElseIf Num(i) = GBD(2) Then
                Num(i) = Var_Cost(2) 
            ElseIf Num(i) = GBD(3) Then
                Num(i) = Var_Cost(3)
            End If
        Next
        sisa(0) = KWH * 1000

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
                        Else SK3 = True
                        End If

                    ElseIf Num(2) = Var_Cost(3) Then
                        SK3 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK2 = False
                        Else SK2 = True
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
                        Else SK3 = True
                        End If

                    ElseIf Num(2) = Var_Cost(3) Then
                        SK3 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK1 = False
                        Else SK1 = True
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
                            SK2 = False
                        Else SK2 = True
                        End If

                    ElseIf Num(2) = Var_Cost(2) Then
                        SK2 = False
                        If Num(3) < sisa(2) Then
                            sisa(3) = sisa(2) - Num(3)
                            SK1 = False
                        Else SK1 = True
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

        Return SK1
        Return SK2
        Return SK3

    End Function
End Class
