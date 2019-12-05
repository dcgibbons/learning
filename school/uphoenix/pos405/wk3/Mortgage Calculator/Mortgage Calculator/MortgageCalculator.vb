' MortgageCalculator.vb
' Week 3 Assignment
' David C. Gibbons
' POS/405 - Advanced Visual Basic
' Don McPherson
' April 16, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
' | 2.00  | 2007-04-16 | Changes to satisfy change request # 21.
' --------+------------+-----------------------------------------------
' | 1.00  | 2007-04-09 | Initial version to satisfy change request # 
' |       |            | 20.
' --------+------------+-----------------------------------------------
'

Option Explicit On      ' Require all variables to be declared
Option Strict On        ' Require strict data type declarations and conversions

'
' Imports
'
Imports System.Globalization    ' used for numeric field conversion

'
' MortgageCalculator class is linked to a Windows Form. It processes input 
' from the form
Public Class MortgageCalculator
    Private PrincipalTotal As Decimal
    Private InterestRate As Double
    Private TermInYears As Integer
    Private PaymentAmount As Decimal

    '
    ' In this version, the mortgage terms and rates are hard-coded and the user
    ' selects from a menu of available choices. Here we represent the term and
    ' interest rate as two arrays. Each index of both arrays will be used to 
    ' construct one mortgage choice.
    '
    Dim MAX_MORTGAGE_LOANS As Integer = 3
    Private MortgageTerms(MAX_MORTGAGE_LOANS) As Integer
    Private MortgageRates(MAX_MORTGAGE_LOANS) As Double

    '
    ' Prepopulate the tooltips and form data whenever the form loads
    '
    Private Sub MortgageCalculator_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        ToolTip.SetToolTip(txtPrincipalAmount, _
            "Enter the principal amount in dollars and cents, e.g.: $250,000.00")

        ToolTip.SetToolTip(boxMortgages, _
            "Shows the breakdown of interest and principal paid for each payment of of the mortgage")

        ToolTip.SetToolTip(txtPayment, _
            "Displays the mortgage payment after " & vbCrLf & _
            "all input fields have valid input")

        '
        ' populate the arrays with the available predefined mortgages
        '
        Dim mortgage As Integer = 1
        MortgageTerms(mortgage) = 7
        MortgageRates(mortgage) = 5.35 / 100.0

        mortgage = mortgage + 1
        MortgageTerms(mortgage) = 15
        MortgageRates(mortgage) = 5.5 / 100.0

        mortgage = mortgage + 1
        MortgageTerms(mortgage) = 30
        MortgageRates(mortgage) = 5.75 / 100.0

        '
        ' populate the combbox with each of the available mortgages
        '
        boxMortgages.Items.Clear()
        boxMortgages.Items.Add("") ' emtpy choice by default
        For mortgage = 1 To MortgageTerms.Length - 1
            ' build the description
            Dim description As String
            description = String.Format("{0:d} years @ {1:p}", _
                MortgageTerms(mortgage), MortgageRates(mortgage))
            boxMortgages.Items.Add(description)
        Next

        '
        ' select the first mortgage choice by default
        '
        boxMortgages.SelectedIndex = 0
    End Sub

    '
    ' Calculate's the current mortgage payment information based upon
    ' the currently input mortgage values, whether user input or retrieved
    ' from the list of predefined mortgages
    '
    Private Sub calculateMortgagePayment()
        ' clear any previous calculation output
        txtPayment.Clear()
        paymentsList.Clear()

        ' clear all previous error indicators
        ErrorProvider.Clear()

        ' only try and validate the fields if all of them have data
        If AllInputAvailable() Then
            ' validate all the input
            Dim inputValid As Boolean
            inputValid = validatePrincipalAmount()
            If Not inputValid Then
                ErrorProvider.SetError(txtPrincipalAmount, "Amount is not a valid number greater than 0.")
            End If

            ' Calculate the monthly mortgage payment and display it
            If inputValid Then
                Dim mortgageCalc As New Mortgage(PrincipalTotal, InterestRate, TermInYears)
                PaymentAmount = mortgageCalc.MonthlyPayment
                txtPayment.Text = Format(PaymentAmount, "currency")
                createPaymentReport()
            End If
        End If
    End Sub

    '
    ' Creates a detailed breakdown of the mortgage, including the remaining balance
    ' at each month, and the interest and principal payment breakdown for each month
    '
    Private Sub createPaymentReport()
        ' suspend the listview's layout so that the user gets a seamless update
        paymentsList.SuspendLayout()

        ' reset the column views each time the list is recalculated
        paymentsList.Columns.Clear()
        paymentsList.Columns.Add("Pmt #", -2, HorizontalAlignment.Right)
        paymentsList.Columns.Add("Balance", -2, HorizontalAlignment.Right)
        paymentsList.Columns.Add("Interest", -2, HorizontalAlignment.Right)
        paymentsList.Columns.Add("Principal", -2, HorizontalAlignment.Right)

        Dim rateMonthly As Decimal = CDec(InterestRate / Mortgage.MONTHS_PER_YEAR)
        Dim currentPrincipal As Decimal = PrincipalTotal

        ' loop through every month
        For month As Integer = 1 To (TermInYears * Mortgage.MONTHS_PER_YEAR)
            ' calculate the interest and principal portions of this month's payment
            Dim pmtInterest As Decimal = rateMonthly * currentPrincipal
            Dim pmtPrincipal As Decimal = PaymentAmount - pmtInterest

            ' calculate the remaining balance
            currentPrincipal = (currentPrincipal - pmtPrincipal)

            ' add a new item to the list view for each of the column values
            Dim item As New ListViewItem()
            item.Text = String.Format("{0,5}", month)
            item.SubItems.Add(String.Format("{0,13:c}", currentPrincipal))
            item.SubItems.Add(String.Format("{0,13:c}", pmtInterest))
            item.SubItems.Add(String.Format("{0,13:c}", pmtPrincipal))

            paymentsList.Items.Add(item)
        Next

        ' change the column headers to auto-resize and the resume the list view layout
        ' so the user sees a quick display refresh
        paymentsList.Columns.Item(0).AutoResize(ColumnHeaderAutoResizeStyle.HeaderSize)
        paymentsList.Columns.Item(1).AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent)
        paymentsList.Columns.Item(2).AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent)
        paymentsList.Columns.Item(3).AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent)
        paymentsList.ResumeLayout()
    End Sub

    '
    ' Determines if the PrincipalAmount input field is valid. If so, then the
    ' PrincipalTotal member variable will be set to the decimal equivalent of
    ' the value.
    '
    Private Function validatePrincipalAmount() As Boolean
        Dim inputValid As Boolean
        Dim principal As Decimal
        Try
            principal = Decimal.Parse(txtPrincipalAmount.Text, NumberStyles.Currency, CultureInfo.CurrentCulture)
            If principal > 0.0 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As FormatException
            inputValid = False
        End Try

        If inputValid Then
            PrincipalTotal = principal
        End If

        validatePrincipalAmount = inputValid
    End Function

    '
    ' Determines if all the input fields from the user have at least some 
    ' input, valid or not
    '
    Public Function AllInputAvailable() As Boolean
        AllInputAvailable = (txtPrincipalAmount.Text.Trim.Length > 0)
        AllInputAvailable = AllInputAvailable AndAlso (TermInYears > 0)
        AllInputAvailable = AllInputAvailable AndAlso (InterestRate > 0.0)
    End Function

    '
    ' When the user presses the close button, it's time to end the application!
    '
    Private Sub closeButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles closeButton.Click
        End
    End Sub

    '
    ' When the selected item in the mortgages combox changes then a new
    ' set of mortgage terms should be displayed on screen and the payment
    ' recalculated
    '
    Private Sub boxMortgages_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) _
    Handles boxMortgages.SelectedIndexChanged
        If boxMortgages.SelectedIndex >= 1 Then
            ' retrieve the Mortgage object from the array of Mortgages
            Dim mortgage As Integer = boxMortgages.SelectedIndex

            ' populate the fields with the mortgage information and 
            ' calculate the new payment
            TermInYears = MortgageTerms(mortgage)
            InterestRate = MortgageRates(mortgage)
        Else
            TermInYears = 0
            InterestRate = 0.0
        End If
        resetPayment()
    End Sub

    '
    ' If the user clicks the Clear button then reset all of the fields on the form
    ' to their default values
    Private Sub clearButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles clearButton.Click
        TermInYears = 0
        InterestRate = 0.0
        txtPrincipalAmount.Text = ""
        boxMortgages.SelectedIndex = 0
        resetPayment()
    End Sub

    '
    ' Calculate the mortgage payment when the user presses the Calculate button.
    '
    Private Sub btnCalc_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCalc.Click
        calculateMortgagePayment()
    End Sub

    '
    ' Check if the calculate button should be enabled or disabled when the user changes
    ' the principal amount field
    '
    Private Sub txtPrincipalAmount_TextChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtPrincipalAmount.TextChanged
        checkCalcButton()
        resetPayment()
    End Sub

    '
    ' Resets the payment and payment list fields
    '
    Private Sub resetPayment()
        txtPayment.Text = ""
        paymentsList.Clear()
        checkCalcButton()
    End Sub

    '
    ' Enables or disables the calculate button based on current user input
    '
    Private Sub checkCalcButton()
        btnCalc.Enabled = AllInputAvailable() AndAlso validatePrincipalAmount()
    End Sub
End Class
