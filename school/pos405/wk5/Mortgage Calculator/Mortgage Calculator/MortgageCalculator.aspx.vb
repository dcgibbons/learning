' MortgageCalculator.aspx.vb
' Week 5 Assignment
' David C. Gibbons
' POS/405 - Advanced Visual Basic
' Don McPherson
' April 30, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
' | 4.00  | 2007-04-30 | Changes to satisfy change request # 23. UI is
' |       |            | now an Web UI using ASP.NET with a VB backend.
' --------+------------+-----------------------------------------------
' | 3.00  | 2007-04-23 | Changes to satisfy change request # 22.
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
Imports System.Drawing
Imports System.IO               ' used for file I/O
Imports System.Globalization    ' used for numeric field conversion

Partial Class _Default
    Inherits System.Web.UI.Page

    Private PrincipalTotal As Decimal
    Private InterestRate As Decimal
    Private TermInYears As Integer
    Private PaymentAmount As Decimal

    Const CurrencyFormat As String = "{0,13:c}"
    Const RateFormat As String = "{0:p}"

    '
    ' In this version, the mortgage terms and rates are dynamically loaded and the user
    ' selects from a menu of available choices. Here we represent the term and
    ' interest rate as two arrays. Each index of both arrays will be used to 
    ' construct one mortgage choice.
    '
    Const MAX_MORTGAGES As Integer = 10
    Private MortgageTerms() As Integer
    Private MortgageRates() As Decimal

    Protected Sub Page_Init(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Init
        '
        ' populate the arrays with the available predefined mortgages
        '
        loadMortgages()

        '
        ' populate the combbox with each of the available mortgages
        '
        listTerms.Items.Clear()
        listTerms.Items.Add("") ' emtpy choice by default
        For mortgage As Integer = 1 To MortgageTerms.Length - 1
            ' build the description
            Dim description As String
            description = String.Format("{0:d} years @ {1:p}", _
                MortgageTerms(mortgage), MortgageRates(mortgage))
            listTerms.Items.Add(description)
        Next
    End Sub

    '
    ' Loads the mortgage terms and rates from a data file. If the data file isn't
    ' found in the current working directory then the user is prompted to select
    ' a data file.
    '
    Private Sub loadMortgages()
        ' find the actual server-side path of the App_Data directory and our mortgages
        ' data file
        Dim fileName As String = HttpContext.Current.ApplicationInstance.Server.MapPath("~/App_Data/mortgages.txt")

        ' let any thrown exceptions terminate our server-side script
        ReDim MortgageTerms(MAX_MORTGAGES)
        ReDim MortgageRates(MAX_MORTGAGES)
        Dim mortgage As Integer = 0
        Dim line As String
        Dim reader As StreamReader = File.OpenText(fileName)

        ' read until the file is exhausted or our mortgage data is full
        Do Until reader.EndOfStream Or mortgage = MAX_MORTGAGES
            line = reader.ReadLine()

            ' skip any line that begins with a comment character
            If line.StartsWith("#") Then
                Continue Do
            End If

            ' split the line into two parts that contain the mortgage term and interest rate
            Dim split As String() = line.Split(New [Char]() {","c})
            Dim term As Integer = Integer.Parse(split(0))
            Dim interestRate As Decimal = Decimal.Parse(split(1)) / 100D

            ' add the mortgage to the next location in the mortgages arrays
            mortgage = mortgage + 1
            MortgageTerms(mortgage) = term
            MortgageRates(mortgage) = interestRate
        Loop
        reader.Close()

        ' resize the dynamic arrays to be just the size of the data loaded
        ReDim Preserve MortgageTerms(mortgage)
        ReDim Preserve MortgageRates(mortgage)
    End Sub

    '
    ' Calculate a new mortgage whenever the user presses calculate
    ' Validation will prevent this event handler from being called if any
    ' fields fail validation
    '
    Protected Sub btnCalc_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnCalc.Click
        ' the drop-down list of pre-defined mortgages always wins over the
        ' custom loan input fields
        If listTerms.SelectedIndex > 0 Then
            InterestRate = MortgageRates(listTerms.SelectedIndex)
            TermInYears = MortgageTerms(listTerms.SelectedIndex)
            txtInterestRate.Text = String.Format(RateFormat, InterestRate)
            txtTerm.Text = TermInYears.ToString
        End If

        Dim mortgageCalc As New Mortgage(PrincipalTotal, InterestRate, TermInYears)
        PaymentAmount = mortgageCalc.MonthlyPayment
        txtPayment.Text = Format(PaymentAmount, "currency")
        createPaymentReport()

        ' show the results panel so all of the calculations magically appear
        resultsPanel.Visible = True
    End Sub

    '
    ' Clear all of the input values and hide the results panel if the user 
    ' presses Clear
    '
    Protected Sub btnClear_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnClear.Click
        txtPrincipalAmount.Text = ""
        txtTerm.Text = ""
        txtInterestRate.Text = ""
        txtPayment.Text = ""
        listTerms.SelectedIndex = 0
        resultsPanel.Visible = False
    End Sub

    '
    ' If the user's browser supports JavaScript, then changing the selection
    ' of predefined mortgages will give us a chance to dynamically update the
    ' manual input fields to keep things synchronized
    '
    Protected Sub listTerms_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles listTerms.SelectedIndexChanged
        If listTerms.SelectedIndex > 0 Then
            InterestRate = MortgageRates(listTerms.SelectedIndex)
            TermInYears = MortgageTerms(listTerms.SelectedIndex)
            txtInterestRate.Text = String.Format(RateFormat, InterestRate)
            txtTerm.Text = TermInYears.ToString
        Else
            TermInYears = 0
            InterestRate = 0D
            txtInterestRate.Text = ""
            txtTerm.Text = ""
        End If
        resultsPanel.Visible = False
    End Sub

    '
    ' Custom validation method for the principal amount field
    '
    Protected Sub validatorPrincipal_ServerValidate(ByVal source As Object, ByVal args As System.Web.UI.WebControls.ServerValidateEventArgs) Handles validatorPrincipal.ServerValidate
        Dim inputValid As Boolean
        Dim principal As Decimal
        Try
            principal = Decimal.Parse(args.Value, NumberStyles.Currency, CultureInfo.CurrentCulture)
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

        args.IsValid = inputValid
    End Sub

    '
    ' Custom validation method for the term field
    '
    Protected Sub validatorTerm_ServerValidate(ByVal source As Object, ByVal args As System.Web.UI.WebControls.ServerValidateEventArgs) Handles validatorTerm.ServerValidate
        Dim inputValid As Boolean
        Dim term As Integer
        Try
            term = Integer.Parse(args.Value, NumberStyles.Integer, CultureInfo.CurrentCulture)
            If term > 0 And term < 50 Then
                inputValid = True
            Else
                inputValid = False
            End If
        Catch ex As Exception
            inputValid = False
        End Try

        If inputValid Then
            TermInYears = term
        End If

        args.IsValid = inputValid
    End Sub

    '
    ' Custom validation method for the interest rate field
    '
    Protected Sub validatorInterestRate_ServerValidate(ByVal source As Object, ByVal args As System.Web.UI.WebControls.ServerValidateEventArgs) Handles validatorInterestRate.ServerValidate
        Dim inputValid As Boolean
        Dim rate As Decimal
        Try
            ' remove any percent sign and spacing in the input text
            Dim s As String = args.Value.Trim()
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

        If inputValid Then
            InterestRate = rate
        End If

        args.IsValid = inputValid
    End Sub

    '
    ' Creates a detailed breakdown of the mortgage, including the remaining balance
    ' at each month, and the interest and principal payment breakdown for each month
    '
    Private Sub createPaymentReport()
        tblPayments.Rows.Clear()

        ' add the table header row
        Dim row As TableRow = New TableRow
        row.TableSection = TableRowSection.TableHeader

        Dim cell As TableCell = New TableCell
        cell.Text = "Pmt #"
        row.Cells.Add(cell)

        cell = New TableCell
        cell.Text = "Balance"
        row.Cells.Add(cell)

        cell = New TableCell
        cell.Text = "Interest"
        row.Cells.Add(cell)

        cell = New TableCell
        cell.Text = "Principal"
        row.Cells.Add(cell)

        tblPayments.Rows.Add(row)

        ' determine the monthly rate and begin calculating each month's payment breakdown
        Dim rateMonthly As Decimal = CDec(InterestRate / Mortgage.MONTHS_PER_YEAR)
        Dim currentPrincipal As Decimal = PrincipalTotal
        Dim interestTotal As Decimal = 0D

        ' loop through every month
        For month As Integer = 1 To (TermInYears * Mortgage.MONTHS_PER_YEAR)
            ' calculate the interest and principal portions of this month's payment
            Dim pmtInterest As Decimal = rateMonthly * currentPrincipal
            Dim pmtPrincipal As Decimal = PaymentAmount - pmtInterest

            ' calculate the total interest paid so far and remaining balance
            interestTotal = interestTotal + pmtInterest
            currentPrincipal = currentPrincipal - pmtPrincipal

            ' add a new row to the table with the payment breakdown
            row = New TableRow
            row.TableSection = TableRowSection.TableBody

            cell = New TableCell
            cell.Text = String.Format("{0,5}", month)
            row.Cells.Add(cell)

            cell = New TableCell
            cell.Text = String.Format(CurrencyFormat, currentPrincipal)
            row.Cells.Add(cell)

            cell = New TableCell
            cell.Text = String.Format(CurrencyFormat, pmtInterest)
            row.Cells.Add(cell)

            cell = New TableCell
            cell.Text = String.Format(CurrencyFormat, pmtPrincipal)
            row.Cells.Add(cell)

            tblPayments.Rows.Add(row)
        Next

        ' add one final row to represent the totals
        row = New TableRow
        row.TableSection = TableRowSection.TableFooter

        cell = New TableCell
        cell.ColumnSpan = 2
        cell.Text = "Total Paid:"
        row.Cells.Add(cell)

        cell = New TableCell
        cell.Text = String.Format(CurrencyFormat, interestTotal)
        row.Cells.Add(cell)

        cell = New TableCell
        cell.Text = String.Format(CurrencyFormat, PrincipalTotal)
        row.Cells.Add(cell)

        tblPayments.Rows.Add(row)

        ' setup the pie chart to show the breakdown between interest and principal paid
        PieChart1.Clear()
        PieChart1.ChartTitle = "Breakdown"
        PieChart1.ValueFormatText = CurrencyFormat
        PieChart1.AddData("Total Interest", interestTotal)
        PieChart1.AddData("Total Principal", PrincipalTotal)

    End Sub

End Class
