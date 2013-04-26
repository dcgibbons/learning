' MortgageForm.vb
' Week 5 Assignment
' David C. Gibbons
' POS/402 - Visual Basic
' Aron Kuppersmith
' February 12, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
'    4.10 | 2007-02-12 | Modified logic to use Mortgages array directly
'         |            | instead of the cached value from the form.
' --------+------------+-----------------------------------------------
'    4.00 | 2007-02-09 | Added calculation detail for predefined 
'         |            | mortgages using a ListView control.
' --------+------------+-----------------------------------------------
'    3.00 | 2007-02-04 | Added predefined mortgage terms in an array 
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
    ' Calculate's the current mortgage payment information based upon
    ' the currently input mortgage values, whether user input or retrieved
    ' from the list of predefined mortgages
    '
    Private Sub calculateMortgagePayment()
        ' clear any previous calculation output
        paymentsList.Clear()
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

        ' now calculate the payment detail report
        createPaymentReport()
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

        Dim month As Integer
        Dim rateMonthly As Decimal = InterestRate / Mortgage.MONTHS_PER_YEAR
        Dim currentPrincipal As Decimal = PrincipalTotal

        ' loop through every month
        For month = 1 To Term * Mortgage.MONTHS_PER_YEAR
            ' calculate the interest and principal portions of this month's payment
            Dim pmtInterest As Decimal = rateMonthly * currentPrincipal
            Dim pmtPrincipal As Decimal = PaymentAmount - pmtInterest

            ' calculate the remaining balance
            currentPrincipal = Math.Abs(currentPrincipal - pmtPrincipal)

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
            ' retrieve the Mortgage object from the array of Mortgages
            Dim mortgage As Mortgage = Mortgages(boxMortgages.SelectedIndex)

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
