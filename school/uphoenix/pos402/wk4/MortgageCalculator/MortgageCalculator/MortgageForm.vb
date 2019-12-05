' MortgageForm.vb
' Week 4 Assignment
' David C. Gibbons
' POS/402 - Visual Basic
' Aron Kuppersmith
' February 4, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
'    3.00 | 2007-01-04 | Added predefined mortgage terms in an array 
'         |            | data structure that the user may pick from.
' --------+------------+-----------------------------------------------
'    2.00 | 2007-01-27 | Added user input of mortgage terms and detailed
'         |            | mortgage payment report breakdown.
' --------+------------+-----------------------------------------------
'    1.00 | 2007-01-20 | Initial version.
' --------+------------+-----------------------------------------------

' Include the globalization namespace by default
Imports System.Globalization

'
' The MortgageForm class provides the applications main user interface and
' controls the entire application lifecycle.
'
Public Class MortgageForm
    Private Mortgages(2) As Mortgage
    Private PrincipalTotal As Decimal
    Private InterestRate As Decimal
    Private Term As Integer
    Private PaymentAmount As Decimal

    '
    ' When the form is loaded we need to populate the combobox that contains
    ' the predefined list of mortgages that can be selected by the user.
    '
    Private Sub MortgageForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) _
    Handles Me.Load
        '
        ' populate the array with the available predefined mortgages
        '
        Mortgages(0) = New Mortgage(200000.0, 5.35 / 100.0, 7)
        Mortgages(1) = New Mortgage(200000.0, 5.5 / 100.0, 15)
        Mortgages(2) = New Mortgage(200000.0, 5.75 / 100.0, 30)

        '
        ' populate the combbox with each of the available mortgages
        '
        boxMortgages.Items.Clear()
        Dim mortgage As Mortgage
        For Each mortgage In Mortgages
            ' add the Mortgage object directly into the combobox
            boxMortgages.Items.Add(mortgage)
        Next

        '
        ' select the first mortgage by default - this causes an automatic
        ' calculation of that mortgage's payment
        '
        boxMortgages.SelectedIndex = 0
    End Sub

    '
    ' When the user clicks the calculate button the mortgage payment is
    ' calculated and added to the payment text control for display.
    '
    Private Sub btnCalculate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) _
    Handles btnCalculate.Click
        calculateMortgagePayment()
    End Sub

    '
    ' Calculate's the current mortgage payment information based upon
    ' the currently input mortgage values, whether user input or retrieved
    ' from the list of predefined mortgages
    '
    Private Sub calculateMortgagePayment()
        ' clear any previous calculation output
        boxPayments.Clear()
        txtPayment.Clear()

        Dim inputValid As Boolean

        ' Attempt to parse the principal amount; display an error if parsing fails.
        Try
            PrincipalTotal = Decimal.Parse(txtPrincipalAmount.Text, _
                NumberStyles.Currency, _
                CultureInfo.CurrentCulture)
            If PrincipalTotal > 0.0 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As FormatException
            inputValid = False
        End Try

        If Not inputValid Then
            MsgBox(txtPrincipalAmount.Text & " is not valid. Try again.", _
                MsgBoxStyle.Exclamation, "Invalid Principal Amount")
            txtPrincipalAmount.Focus()
            Return
        End If

        ' Attempt to parse the interest rate; display an error if parsing fails.
        Try
            ' remove any percent sign and spacing in the input text
            Dim s As String = txtInterestRate.Text.Trim()
            Dim n = s.IndexOf("%")
            If n > -1 Then
                s = s.Substring(0, n).Trim()
            End If

            InterestRate = Decimal.Parse(s, _
                NumberStyles.AllowDecimalPoint, _
                CultureInfo.CurrentCulture) / 100D ' divide to convert to percentage
            If InterestRate > 0.0 And InterestRate < 100.0 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As Exception
            inputValid = False
        End Try

        If Not inputValid Then
            MsgBox(txtInterestRate.Text & " is not valid. Try again.", _
                MsgBoxStyle.Exclamation, "Invalid Interest Rate")
            txtInterestRate.Focus()
            Return
        End If

        ' Attempt to parse the mortgage term; display an error if parsing fails.
        Try
            Term = Integer.Parse(txtTerm.Text, _
                NumberStyles.Integer, _
                CultureInfo.CurrentCulture)
            If Term > 0 And Term < 600 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As Exception
            inputValid = False
        End Try

        If Not inputValid Then
            MsgBox(txtTerm.Text & " is not valid. Try again.", _
                MsgBoxStyle.Exclamation, "Invalid Mortgage Term")
            txtTerm.Focus()
            Return
        End If

        ' Calculate the monthly mortgage payment and display it in the text box
        Dim mortgageCalc As New Mortgage(PrincipalTotal, interestRate, Term)
        PaymentAmount = mortgageCalc.MonthlyPayment
        txtPayment.Text = Format(paymentAmount, "currency")

        ' Disable the calculate button and display a payment report breakdown;
        ' once the calculation is complete the calculate button is reenabled
        '
        ' TODO: uncomment for the week 5 assignment.
        ' btnCalculate.Enabled = False
        ' createPaymentReport()
        ' btnCalculate.Enabled = True
    End Sub

    '
    ' Creates a detailed breakdown of the mortgage, including the remaining balance
    ' at each month, and the interest and principal payment breakdown for each month
    '
    ' NOTE: unused in the week 4 assignment
    '
    Private Sub createPaymentReport()
        ' create the header and display it initially
        Dim header1 = String.Format("{0,5} {1,13} {2,13} {3,13}", _
            "Month", "Balance", "Interest", "Principal")
        Dim header2 = String.Format("{0,5} {1,13} {2,13} {3,13}", _
            "-----", "-------", "--------", "---------")
        boxPayments.AppendText(header1 & Environment.NewLine & _
            header2 & Environment.NewLine)

        Dim month As Integer
        Dim rateMonthly As Decimal = InterestRate / Mortgage.MONTHS_PER_YEAR
        Dim currentPrincipal As Decimal = PrincipalTotal

        ' loop through every month
        For month = 1 To Term
            ' calculate the interest and principal portions of this month's payment
            Dim pmtInterest As Decimal = rateMonthly * currentPrincipal
            Dim pmtPrincipal As Decimal = PaymentAmount - pmtInterest

            ' calculate the remaining balance
            currentPrincipal = Math.Abs(currentPrincipal - pmtPrincipal)

            ' format the report line and add it to the output box
            Dim line = String.Format("{0,5} {1,13:C} {2,13:C} {3,13:C}", _
                month, currentPrincipal, pmtInterest, pmtPrincipal)
            boxPayments.AppendText(line & Environment.NewLine)

            ' give the UI a chance to respond to any events
            Application.DoEvents()

            ' every year, pause a moment
            If month Mod Mortgage.MONTHS_PER_YEAR = 0 Then
                Threading.Thread.Sleep(1000)

                ' give the UI a chance to respond to any events
                Application.DoEvents()

                ' display a header for the next year if there is one
                If month < Term Then
                    boxPayments.AppendText(Environment.NewLine & header1 & Environment.NewLine & _
                        header2 & Environment.NewLine)
                End If
            End If
        Next
    End Sub

    '
    ' When the user clicks the close button the application will immediately
    ' exit.
    '
    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) _
    Handles btnClose.Click
        End
    End Sub

    '
    ' When the txtPrincipalAmount field gets focus automatically select all the text for
    ' the ease of the user
    '
    Private Sub txtPrincipalAmount_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtPrincipalAmount.GotFocus
        txtPrincipalAmount.SelectAll()
    End Sub

    '
    ' When the txtInterestRate field gets focus automatically select all the text for
    ' the ease of the user
    '
    Private Sub txtInterestRate_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtInterestRate.GotFocus
        txtInterestRate.SelectAll()
    End Sub

    '
    ' When the txtTerm field gets focus automatically select all the text for
    ' the ease of the user
    '
    Private Sub txtTerm_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtTerm.GotFocus
        txtTerm.SelectAll()
    End Sub

    '
    ' When the selected item in the mortgages combox changes then a new
    ' set of mortgage terms should be displayed on screen and the payment
    ' recalculated
    '
    Private Sub boxMortgages_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) _
    Handles boxMortgages.SelectedIndexChanged
        If boxMortgages.SelectedIndex >= 0 Then
            ' retrieve the Mortgage object from the combobox
            Dim mortgage As Mortgage = boxMortgages.SelectedItem

            ' populate the fields with the mortgage information and 
            ' calculate the new payment
            txtPrincipalAmount.Text = mortgage.PrincipalAmount.ToString("c")
            txtInterestRate.Text = mortgage.InterestRate.ToString("p")
            txtTerm.Text = mortgage.Term.ToString("d")
            calculateMortgagePayment()
        Else
            txtPrincipalAmount.Text = ""
            txtInterestRate.Text = ""
            txtTerm.Text = ""
            txtPayment.Text = ""
        End If
    End Sub
End Class
