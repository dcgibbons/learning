' MortgageForm.vb
' Week 3 Assignment
' David C. Gibbons
' POS/402 - Visual Basic
' Aron Kuppersmith
' January 27, 2007
'
' Version | Date       | Description
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
    Private principalTotal As Decimal
    Private interestRate As Decimal
    Private termTotal As Integer
    Private paymentAmount As Decimal

    '
    ' When the user clicks the calculate button the mortgage payment is
    ' calculated and added to the payment text control for display.
    '
    Private Sub btnCalculate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) _
    Handles btnCalculate.Click
        ' clear any previous calculation output
        boxPayments.Clear()
        txtPayment.Clear()

        Dim inputValid As Boolean

        ' Attempt to parse the principal amount; display an error if parsing fails.
        Try
            principalTotal = Decimal.Parse(txtPrincipalAmount.Text, _
                NumberStyles.Currency, _
                CultureInfo.CurrentCulture)
            If principalTotal > 0.0 Then
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
            interestRate = Decimal.Parse(txtInterestRate.Text, _
                NumberStyles.AllowDecimalPoint, _
                CultureInfo.CurrentCulture) / 100D ' divide to convert to percentage
            If interestRate > 0.0 And interestRate < 100.0 Then
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
            termTotal = Integer.Parse(txtTerm.Text, _
                NumberStyles.Integer, _
                CultureInfo.CurrentCulture) * Mortgage.MONTHS_PER_YEAR
            If termTotal > 0 And termTotal < 600 Then
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
        Dim mortgageCalc As New Mortgage(principalTotal, interestRate, termTotal)
        paymentAmount = mortgageCalc.MonthlyPayment
        txtPayment.Text = Format(paymentAmount, "currency")

        ' Disable the calculate button and display a payment report breakdown;
        ' once the calculation is complete the calculate button is reenabled
        btnCalculate.Enabled = False
        createPaymentReport()
        btnCalculate.Enabled = True
    End Sub

    '
    ' Creates a detailed breakdown of the mortgage, including the remaining balance
    ' at each month, and the interest and principal payment breakdown for each month
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
        Dim rateMonthly As Decimal = interestRate / Mortgage.MONTHS_PER_YEAR
        Dim currentPrincipal As Decimal = principalTotal

        ' loop through every month
        For month = 1 To termTotal
            ' calculate the interest and principal portions of this month's payment
            Dim pmtInterest As Decimal = rateMonthly * currentPrincipal
            Dim pmtPrincipal As Decimal = paymentAmount - pmtInterest

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
                If month < termTotal Then
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
End Class
