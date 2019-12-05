' Mortgage.vb
' Week 3 Assignment
' David C. Gibbons
' POS/402 - Visual Basic
' Aron Kuppersmith
' January 27, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
'    2.00 | 2007-01-27 | Updated for week 3 assignment.
' --------+------------+-----------------------------------------------
'    1.00 | 2007-01-20 | Initial version.
' --------+------------+-----------------------------------------------

'
' The Mortgage class provides financial calculations for mortgages.
' 
Public Class Mortgage
    '
    ' Constant for the number of months per year (only to make formulas
    ' easier to read.
    '
    Public Const MONTHS_PER_YEAR = 12

    '
    ' The starting PrincipalAmount of the mortgage.
    '
    Private PrincipalAmount As Decimal

    '
    ' The MonthlyRate of the mortgage (as opposed to the yearly rate).
    '
    Private MonthlyRate As Double

    '
    ' The NumPayments of the mortgate.
    '
    Private NumPayments As Double

    '
    ' The MonthlyPayment value is a public property that contains the
    ' currently calculated mortgage payment amount.
    '
    Public MonthlyPayment As Decimal

    '
    ' A constructor that requires the specific principal, annual interest rate, 
    ' and mortgage term.
    '
    Public Sub New(ByVal principal As Decimal, ByVal rate As Double, ByVal term As Integer)

        ' save the starting principal amount and mortgage term
        PrincipalAmount = principal
        NumPayments = term

        ' calculate the monthly interet rate
        MonthlyRate = rate / MONTHS_PER_YEAR

        ' calculate the mortgage payment 
        MonthlyPayment = CalculatePayment()
    End Sub

    '
    ' Calculates the monthly mortgage payment using a standard financial
    ' calculation.
    '
    Private Function CalculatePayment() As Double
        Dim Payment As Decimal
        Payment = PrincipalAmount * (MonthlyRate / (1 - Math.Pow(1 + MonthlyRate, -NumPayments)))
        Return Payment
    End Function

End Class
