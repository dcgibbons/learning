' MortgageCalculator.vb
' Week 2 Assignment
' David C. Gibbons
' POS/405 - Advanced Visual Basic
' Don McPherson
' April 9, 2007
'
' Version | Date       | Description
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
    ' Prepopulate the tooltips whenever the form loads
    '
    Private Sub MortgageCalculator_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        ToolTip.SetToolTip(txtPrincipalAmount, _
            "Enter the principal amount in dollars in cents, e.g.: $250,000.00")

        ToolTip.SetToolTip(txtInterestRate, _
            "Enter the interest rate as a percentage, e.g.: 8.25%")

        ToolTip.SetToolTip(txtTerm, _
            "Enter the mortgage term as a whole number of years between 1 and 50")

        ToolTip.SetToolTip(txtPayment, _
            "Displays the mortgage payment after " & vbCrLf & _
            "all input fields have valid input")
    End Sub

    '
    ' Calculate's the current mortgage payment information based upon
    ' the currently input mortgage values, whether user input or retrieved
    ' from the list of predefined mortgages
    '
    Private Sub calculateMortgagePayment()
        ' clear any previous calculation output
        txtPayment.Clear()

        ' clear all previous error indicators
        ErrorProvider.Clear()

        ' only try and validate the fields if all of them have data
        If AllInputAvailable() Then
            ' validate all the input
            Dim inputValid As Boolean
            inputValid = validatePrincipalAmount()
            inputValid = inputValid AndAlso validateInterestRate()
            inputValid = inputValid AndAlso validateTerm()

            ' Calculate the monthly mortgage payment and display it
            If inputValid Then
                Dim mortgageCalc As New Mortgage(PrincipalTotal, InterestRate, TermInYears)
                PaymentAmount = mortgageCalc.MonthlyPayment
                txtPayment.Text = Format(PaymentAmount, "currency")
            End If
        End If
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

        If Not inputValid Then
            ErrorProvider.SetError(txtPrincipalAmount, "Amount is not a valid number greater than 0.")
        Else
            PrincipalTotal = principal
        End If

        validatePrincipalAmount = inputValid
    End Function

    '
    ' Determines if the InterestRate input field is valid. If so, then the
    ' InterestRate member variable will be set to the decimal equivalent of
    ' the value.
    '
    Private Function validateInterestRate() As Boolean
        Dim inputValid As Boolean
        Dim rate As Double

        Try
            ' remove any percent sign and spacing in the input text
            Dim s As String = txtInterestRate.Text.Trim()
            Dim n As Integer = s.IndexOf("%")
            If (n > -1) Then
                s = s.Substring(0, n).Trim()
            End If

            rate = Decimal.Parse(s, NumberStyles.AllowDecimalPoint, CultureInfo.CurrentCulture) / 100D ' divide to convert to percentage
            If rate > 0.0 And rate < 100.0 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As Exception
            inputValid = False
        End Try

        If Not inputValid Then
            ErrorProvider.SetError(txtInterestRate, "Interest Rate is not a valid percentage.")
        Else
            InterestRate = rate
        End If

        validateInterestRate = inputValid
    End Function

    '
    ' Determines if the Term input field is valid. If so, then the 
    ' TermInYears member variable will be set to the decimal equivalent of 
    ' the value.
    '
    Private Function validateTerm() As Boolean
        Dim inputValid As Boolean
        Dim term As Integer

        ' Attempt to parse the mortgage term; display an error if parsing fails.
        Try
            term = Integer.Parse(txtTerm.Text, NumberStyles.Integer, CultureInfo.CurrentCulture)
            If term > 0 And term < 50 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As Exception
            inputValid = False
        End Try

        If Not inputValid Then
            ErrorProvider.SetError(txtTerm, "Mortgage term is not a valid whole number between 1 and 50 (years).")
        Else
            TermInYears = term
        End If

        validateTerm = inputValid
    End Function

    '
    ' Determines if all the input fields from the user have at least some 
    ' input, valid or not
    '
    Public Function AllInputAvailable() As Boolean
        AllInputAvailable = (txtPrincipalAmount.Text.Trim.Length > 0) _
            AndAlso (txtInterestRate.Text.Trim.Length > 0) _
            AndAlso (txtTerm.Text.Trim.Length > 0)
    End Function

    '
    ' Attempts to calculate the mortgage whenever the user presses Enter in
    ' the principal amount field
    '
    Private Sub txtPrincipalAmount_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtPrincipalAmount.KeyPress
        ' The keypressed method uses the KeyChar property to check 
        ' whether the ENTER key is pressed. 

        ' If the ENTER key is pressed, the Handled property is set to true, 
        ' to indicate the event is handled.
        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Return) Then
            e.Handled = True
            calculateMortgagePayment()
        End If
    End Sub

    '
    ' Attempts to calculate the mortgage whenever the user leaves the principal
    ' amount field.
    '
    Private Sub txtPrincipalAmount_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtPrincipalAmount.Leave
        calculateMortgagePayment()
    End Sub

    '
    ' Attempts to calculate the mortgage whenever the user presses Enter in
    ' the interest rate field
    '
    Private Sub txtInterestRate_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtInterestRate.KeyPress
        ' The keypressed method uses the KeyChar property to check 
        ' whether the ENTER key is pressed. 

        ' If the ENTER key is pressed, the Handled property is set to true, 
        ' to indicate the event is handled.
        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Return) Then
            e.Handled = True
            calculateMortgagePayment()
        End If
    End Sub

    '
    ' Attempts to calculate the mortgage whenever the user leaves the interest 
    ' rate field.
    '
    Private Sub txtInterestRate_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtInterestRate.Leave
        calculateMortgagePayment()
    End Sub

    '
    ' Attempts to calculate the mortgage whenever the user presses Enter in
    ' the mortgage term field
    '
    Private Sub txtTerm_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtTerm.KeyPress
        ' The keypressed method uses the KeyChar property to check 
        ' whether the ENTER key is pressed. 

        ' If the ENTER key is pressed, the Handled property is set to true, 
        ' to indicate the event is handled.
        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Return) Then
            e.Handled = True
            calculateMortgagePayment()
        End If
    End Sub

    '
    ' Attempts to calculate the mortgage whenever the user leaves the mortgage 
    ' term field.
    '
    Private Sub txtTerm_Leave(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtTerm.Leave
        calculateMortgagePayment()
    End Sub

    '
    ' When the user presses the close button, it's time to end the application!
    '
    Private Sub closeButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles closeButton.Click
        End
    End Sub
End Class
