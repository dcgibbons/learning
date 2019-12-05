' Mortgage.vb
' Week 5 Assignment
' David C. Gibbons
' POS/402 - Visual Basic
' Aron Kuppersmith
' February 12, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
'    4.10 | 2007-02-12 | Changed properties to use Property syntax 
'         |            | based upon week 4 assignment feedback.
' --------+------------+-----------------------------------------------
'    4.00 | 2007-02-09 | Updated for week 5 assignment.
' --------+------------+-----------------------------------------------
'    3.00 | 2007-02-05 | Updated for week 4 assignment.
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
    Private thePrincipalAmount As Decimal

    '
    ' The mortgage interest rate
    '
    Private theInterestRate As Double

    '
    ' The MonthlyRate of the mortgage (as opposed to the yearly rate).
    '
    Private theMonthlyRate As Double

    '
    ' The Term of the mortgage (years).
    '
    Private theTerm As Integer

    '
    ' The NumPayments of the mortgate.
    '
    Private theNumPayments As Integer

    '
    ' The friendly description of the mortgage terms
    '
    Private theDescription As String

    '
    ' The MonthlyPayment value contains the currently calculated 
    ' mortgage payment amount.
    '
    Private theMonthlyPayment As Decimal

    '
    ' A constructor that requires the specific principal, annual interest rate, 
    ' and mortgage term.
    '
    Public Sub New(ByVal principal As Decimal, ByVal rate As Double, ByVal term As Integer)

        ' save the starting principal amount and mortgage term
        thePrincipalAmount = principal
        theTerm = term
        theNumPayments = term * MONTHS_PER_YEAR

        ' calculate the monthly interet rate
        theInterestRate = rate
        theMonthlyRate = theInterestRate / MONTHS_PER_YEAR

        ' build the description
        theDescription = String.Format("{0:c} @ {1:p} for {2:d} years", _
            thePrincipalAmount, theInterestRate, theTerm)

        ' calculate the mortgage payment 
        theMonthlyPayment = CalculatePayment()

    End Sub

    '
    ' PrincipalAmount property
    '
    Public ReadOnly Property PrincipalAmount() As Decimal
        Get
            PrincipalAmount = thePrincipalAmount
        End Get
    End Property

    '
    ' InterestRate property
    '
    Public ReadOnly Property InterestRate() As Double
        Get
            InterestRate = theInterestRate
        End Get
    End Property

    '
    ' MonthlyRate property
    '
    Public ReadOnly Property MonthlyRate() As Double
        Get
            MonthlyRate = theMonthlyRate
        End Get
    End Property


    '
    ' Term property
    '
    Public ReadOnly Property Term() As Integer
        Get
            Term = theTerm
        End Get
    End Property

    '
    ' NumPayments property
    '
    Public ReadOnly Property NumPayments() As Integer
        Get
            NumPayments = theNumPayments
        End Get
    End Property

    '
    ' Description property
    '
    Public ReadOnly Property Description() As String
        Get
            Description = theDescription
        End Get
    End Property

    '
    ' MonthlyPayment property
    '
    Public ReadOnly Property MonthlyPayment() As String
        Get
            MonthlyPayment = theMonthlyPayment
        End Get
    End Property

    '
    ' The ToString function provides a friendly description of the Mortgage terms
    '
    Overrides Function ToString() As String
        Return theDescription
    End Function

    '
    ' Calculates the monthly mortgage payment using a standard financial
    ' calculation.
    '
    Private Function CalculatePayment() As Double
        Dim Payment As Decimal
        Payment = thePrincipalAmount * (theMonthlyRate / (1 - Math.Pow(1 + theMonthlyRate, -theNumPayments)))
        Return Payment
    End Function

End Class
