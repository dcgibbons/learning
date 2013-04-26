<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MortgageForm
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.lblPrincipalAmount = New System.Windows.Forms.Label
        Me.lblInterestRate = New System.Windows.Forms.Label
        Me.lblTerm = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.Label5 = New System.Windows.Forms.Label
        Me.lblPayment = New System.Windows.Forms.Label
        Me.txtPayment = New System.Windows.Forms.TextBox
        Me.GroupBox1 = New System.Windows.Forms.GroupBox
        Me.txtTerm = New System.Windows.Forms.TextBox
        Me.txtInterestRate = New System.Windows.Forms.TextBox
        Me.txtPrincipalAmount = New System.Windows.Forms.TextBox
        Me.btnClose = New System.Windows.Forms.Button
        Me.btnCalculate = New System.Windows.Forms.Button
        Me.boxPayments = New System.Windows.Forms.TextBox
        Me.Label1 = New System.Windows.Forms.Label
        Me.boxMortgages = New System.Windows.Forms.ComboBox
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'lblPrincipalAmount
        '
        Me.lblPrincipalAmount.AutoSize = True
        Me.lblPrincipalAmount.Location = New System.Drawing.Point(38, 19)
        Me.lblPrincipalAmount.Name = "lblPrincipalAmount"
        Me.lblPrincipalAmount.Size = New System.Drawing.Size(50, 13)
        Me.lblPrincipalAmount.TabIndex = 0
        Me.lblPrincipalAmount.Text = "Principal:"
        '
        'lblInterestRate
        '
        Me.lblInterestRate.AutoSize = True
        Me.lblInterestRate.Location = New System.Drawing.Point(17, 56)
        Me.lblInterestRate.Name = "lblInterestRate"
        Me.lblInterestRate.Size = New System.Drawing.Size(71, 13)
        Me.lblInterestRate.TabIndex = 1
        Me.lblInterestRate.Text = "Interest Rate:"
        '
        'lblTerm
        '
        Me.lblTerm.AutoSize = True
        Me.lblTerm.Location = New System.Drawing.Point(5, 94)
        Me.lblTerm.Name = "lblTerm"
        Me.lblTerm.Size = New System.Drawing.Size(75, 13)
        Me.lblTerm.TabIndex = 2
        Me.lblTerm.Text = "Term in Years:"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Tahoma", 14.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(111, 7)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(204, 23)
        Me.Label4.TabIndex = 6
        Me.Label4.Text = "Mortgage Calculator"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Tahoma", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(101, 30)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(237, 19)
        Me.Label5.TabIndex = 7
        Me.Label5.Text = "Written by David C. Gibbons"
        Me.Label5.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'lblPayment
        '
        Me.lblPayment.AutoSize = True
        Me.lblPayment.Location = New System.Drawing.Point(138, 227)
        Me.lblPayment.Name = "lblPayment"
        Me.lblPayment.Size = New System.Drawing.Size(88, 13)
        Me.lblPayment.TabIndex = 8
        Me.lblPayment.Text = "Monthly Payment"
        '
        'txtPayment
        '
        Me.txtPayment.Location = New System.Drawing.Point(232, 227)
        Me.txtPayment.Name = "txtPayment"
        Me.txtPayment.ReadOnly = True
        Me.txtPayment.Size = New System.Drawing.Size(100, 20)
        Me.txtPayment.TabIndex = 6
        Me.txtPayment.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.txtTerm)
        Me.GroupBox1.Controls.Add(Me.txtInterestRate)
        Me.GroupBox1.Controls.Add(Me.txtPrincipalAmount)
        Me.GroupBox1.Controls.Add(Me.lblPrincipalAmount)
        Me.GroupBox1.Controls.Add(Me.lblInterestRate)
        Me.GroupBox1.Controls.Add(Me.lblTerm)
        Me.GroupBox1.Location = New System.Drawing.Point(138, 92)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(200, 129)
        Me.GroupBox1.TabIndex = 10
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Mortgage Terms"
        '
        'txtTerm
        '
        Me.txtTerm.Location = New System.Drawing.Point(94, 91)
        Me.txtTerm.Name = "txtTerm"
        Me.txtTerm.ReadOnly = True
        Me.txtTerm.Size = New System.Drawing.Size(100, 20)
        Me.txtTerm.TabIndex = 3
        '
        'txtInterestRate
        '
        Me.txtInterestRate.Location = New System.Drawing.Point(94, 53)
        Me.txtInterestRate.Name = "txtInterestRate"
        Me.txtInterestRate.ReadOnly = True
        Me.txtInterestRate.Size = New System.Drawing.Size(100, 20)
        Me.txtInterestRate.TabIndex = 2
        '
        'txtPrincipalAmount
        '
        Me.txtPrincipalAmount.Location = New System.Drawing.Point(94, 16)
        Me.txtPrincipalAmount.Name = "txtPrincipalAmount"
        Me.txtPrincipalAmount.ReadOnly = True
        Me.txtPrincipalAmount.Size = New System.Drawing.Size(100, 20)
        Me.txtPrincipalAmount.TabIndex = 1
        '
        'btnClose
        '
        Me.btnClose.Location = New System.Drawing.Point(403, 275)
        Me.btnClose.Name = "btnClose"
        Me.btnClose.Size = New System.Drawing.Size(75, 23)
        Me.btnClose.TabIndex = 5
        Me.btnClose.Text = "Close"
        Me.btnClose.UseVisualStyleBackColor = True
        '
        'btnCalculate
        '
        Me.btnCalculate.Location = New System.Drawing.Point(338, 227)
        Me.btnCalculate.Name = "btnCalculate"
        Me.btnCalculate.Size = New System.Drawing.Size(75, 23)
        Me.btnCalculate.TabIndex = 4
        Me.btnCalculate.Text = "Calculate"
        Me.btnCalculate.UseVisualStyleBackColor = True
        Me.btnCalculate.Visible = False
        '
        'boxPayments
        '
        Me.boxPayments.Font = New System.Drawing.Font("Lucida Console", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.boxPayments.Location = New System.Drawing.Point(35, 253)
        Me.boxPayments.Multiline = True
        Me.boxPayments.Name = "boxPayments"
        Me.boxPayments.ReadOnly = True
        Me.boxPayments.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.boxPayments.Size = New System.Drawing.Size(80, 42)
        Me.boxPayments.TabIndex = 7
        Me.boxPayments.Visible = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(102, 68)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(55, 13)
        Me.Label1.TabIndex = 11
        Me.Label1.Text = "Mortgage:"
        '
        'boxMortgages
        '
        Me.boxMortgages.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.boxMortgages.FormattingEnabled = True
        Me.boxMortgages.Location = New System.Drawing.Point(158, 65)
        Me.boxMortgages.Name = "boxMortgages"
        Me.boxMortgages.Size = New System.Drawing.Size(180, 21)
        Me.boxMortgages.TabIndex = 12
        '
        'MortgageForm
        '
        Me.AcceptButton = Me.btnClose
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.AutoSize = True
        Me.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.ClientSize = New System.Drawing.Size(490, 310)
        Me.Controls.Add(Me.boxMortgages)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.boxPayments)
        Me.Controls.Add(Me.btnCalculate)
        Me.Controls.Add(Me.btnClose)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.txtPayment)
        Me.Controls.Add(Me.lblPayment)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label4)
        Me.Name = "MortgageForm"
        Me.Text = "Mortgage Calculator"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lblPrincipalAmount As System.Windows.Forms.Label
    Friend WithEvents lblInterestRate As System.Windows.Forms.Label
    Friend WithEvents lblTerm As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents lblPayment As System.Windows.Forms.Label
    Friend WithEvents txtPayment As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents btnClose As System.Windows.Forms.Button
    Friend WithEvents btnCalculate As System.Windows.Forms.Button
    Friend WithEvents boxPayments As System.Windows.Forms.TextBox
    Friend WithEvents txtPrincipalAmount As System.Windows.Forms.TextBox
    Friend WithEvents txtInterestRate As System.Windows.Forms.TextBox
    Friend WithEvents txtTerm As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents boxMortgages As System.Windows.Forms.ComboBox

End Class
