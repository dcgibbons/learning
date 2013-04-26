<%@ Page Language="VB" AutoEventWireup="false" CodeFile="MortgageCalculator.aspx.vb" Inherits="_Default" %>

<%@ Register Src="PieChart.ascx" TagName="PieChart" TagPrefix="uc1" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Mortgage Calculator - DCG-POS405-WK5</title>
    <link rel="stylesheet" type="text/css" href="StyleSheet.css" />
</head>
<body>
    <div id="titleInfo">
        <h1>Mortgage Calculator</h1>
        <h2>Written by David C. Gibbons</h2>
        <h3>POS/405 - Advanced Visual Basic</h3>
    </div>

    <form id="form1" runat="server">

        <div id="validationSummary">
            <asp:ValidationSummary ID="ValidationSummary1" runat="server" />
        </div>

        <fieldset>
            <legend>Loan Information</legend>
            
            <!--<asp:Label ID="Label1" runat="server" Text="Principal Amount:"></asp:Label>-->
            <label for="txtPrincipalAmount" accesskey="P">Principal Amount:</label>
            <asp:TextBox ID="txtPrincipalAmount" runat="server"></asp:TextBox>
            &nbsp;
            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ControlToValidate="txtPrincipalAmount"
                Display="None" ErrorMessage="Principal Amount is required."></asp:RequiredFieldValidator>
            <asp:CustomValidator ID="validatorPrincipal" runat="server" ControlToValidate="txtPrincipalAmount"
                Display="None" ErrorMessage="Principal amount must be a valid dollar amount."
                SetFocusOnError="True"></asp:CustomValidator>
            <br />

            <label for="listTerms">Terms:</label>
            <asp:DropDownList ID="listTerms" runat="server" AutoPostBack="True"/>
            <br />

            <label for="txtTerm">Term In Years:</label>
            <asp:TextBox ID="txtTerm" runat="server" CausesValidation="True"></asp:TextBox>
            &nbsp;
            <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ControlToValidate="txtTerm"
                Display="None" ErrorMessage="Term is required."></asp:RequiredFieldValidator>
            <asp:CustomValidator ID="validatorTerm" runat="server" ErrorMessage="CustomValidator" ControlToValidate="txtTerm"></asp:CustomValidator>
            <br />
            
            <label for="txtInterestRate">Interest Rate:</label>
            <asp:TextBox ID="txtInterestRate" runat="server" CausesValidation="True"></asp:TextBox>
            &nbsp;
            <asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ControlToValidate="txtInterestRate"
                Display="None" ErrorMessage="Interest Rate is required."></asp:RequiredFieldValidator>
            <asp:CustomValidator ID="validatorInterestRate" runat="server" ErrorMessage="CustomValidator" ControlToValidate="txtInterestRate"></asp:CustomValidator>
            <br />
        </fieldset>

        <div id="buttons">
            <asp:Button ID="btnCalc" runat="server" CommandName="calculate" Text="Calculate" />
            <asp:Button ID="btnClear" runat="server" CausesValidation="False" CommandName="clear" Text="Clear" />
            <br />
            <br />
        </div>

        <div id="results">
            <asp:Panel ID="resultsPanel" runat="server" Visible="False">
                <fieldset>
                    <legend>Mortgage Payment Details</legend>
                    <label for="txtPayment">Payment Amount:</label>
                    <asp:TextBox ID="txtPayment" runat="server" Enabled="False"></asp:TextBox>
                    <br />
                    
                    <uc1:PieChart ID="PieChart1" runat="server" />
                    <asp:Table ID="tblPayments" runat="server"/>
                </fieldset>
            </asp:Panel>
        </div>

    </form>
   
</body>
</html>
