' Mortgage.vb
' Week 4 Assignment
' David C. Gibbons
' POS/402 - Visual Basic
' Aron Kuppersmith
' February 5, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
'    3.00 | 2007-02-5  | Updated for week 4 assignment.
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
    Public ReadOnly PrincipalAmount As Decimal

    '
    ' The mortgage interest rate
    '
    Public ReadOnly InterestRate As Double

    '
    ' The MonthlyRate of the mortgage (as opposed to the yearly rate).
    '
    Public ReadOnly MonthlyRate As Double

    '
    ' The Term of the mortgage (years).
    '
    Public ReadOnly Term As Integer

    '
    ' The NumPayments of the mortgate.
    '
    Public ReadOnly NumPayments As Integer

    '
    ' The friendly description of the mortgage terms
    '
    Public ReadOnly Description As String

    '
    ' The MonthlyPayment value contains the currently calculated 
    ' mortgage payment amount.
    '
    Public ReadOnly MonthlyPayment As Decimal

    '
    ' A constructor that requires the specific principal, annual interest rate, 
    ' and mortgage term.
    '
    Public Sub New(ByVal principal As Decimal, ByVal rate As Double, ByVal term As Integer)

        ' save the starting principal amount and mortgage term
        PrincipalAmount = principal
        Me.Term = term
        NumPayments = term * MONTHS_PER_YEAR

        ' calculate the monthly interet rate
        InterestRate = rate
        MonthlyRate = InterestRate / MONTHS_PER_YEAR

        ' build the description
        Description = String.Format("{0:c} @ {1:p} for {2:d} years", _
            PrincipalAmount, InterestRate, term)

        ' calculate the mortgage payment 
        MonthlyPayment = CalculatePayment()

    End Sub

    '
    ' The ToString function provides a friendly description of the Mortgage terms
    '
    Overrides Function ToString() As String
        Return Description
    End Function

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
