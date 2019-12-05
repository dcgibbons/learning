<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MortgageCalculator
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
        Me.components = New System.ComponentModel.Container
        Me.txtPayment = New System.Windows.Forms.TextBox
        Me.lblPayment = New System.Windows.Forms.Label
        Me.ErrorProvider = New System.Windows.Forms.ErrorProvider(Me.components)
        Me.lblPrincipalAmount = New System.Windows.Forms.Label
        Me.txtPrincipalAmount = New System.Windows.Forms.TextBox
        Me.ToolTip = New System.Windows.Forms.ToolTip(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.closeButton = New System.Windows.Forms.Button
        Me.Label3 = New System.Windows.Forms.Label
        Me.Label4 = New System.Windows.Forms.Label
        Me.boxMortgages = New System.Windows.Forms.ComboBox
        Me.paymentsList = New System.Windows.Forms.ListView
        Me.Label5 = New System.Windows.Forms.Label
        Me.clearButton = New System.Windows.Forms.Button
        Me.btnCalc = New System.Windows.Forms.Button
        Me.txtTerm = New System.Windows.Forms.TextBox
        Me.txtInterestRate = New System.Windows.Forms.TextBox
        Me.Label6 = New System.Windows.Forms.Label
        Me.Label7 = New System.Windows.Forms.Label
        CType(Me.ErrorProvider, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'txtPayment
        '
        Me.txtPayment.Location = New System.Drawing.Point(241, 203)
        Me.txtPayment.Name = "txtPayment"
        Me.txtPayment.ReadOnly = True
        Me.txtPayment.Size = New System.Drawing.Size(121, 20)
        Me.txtPayment.TabIndex = 11
        Me.txtPayment.TabStop = False
        Me.txtPayment.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lblPayment
        '
        Me.lblPayment.AutoSize = True
        Me.lblPayment.Location = New System.Drawing.Point(144, 206)
        Me.lblPayment.Name = "lblPayment"
        Me.lblPayment.Size = New System.Drawing.Size(91, 13)
        Me.lblPayment.TabIndex = 12
        Me.lblPayment.Text = "Monthly Payment:"
        '
        'ErrorProvider
        '
        Me.ErrorProvider.ContainerControl = Me
        '
        'lblPrincipalAmount
        '
        Me.lblPrincipalAmount.AutoSize = True
        Me.lblPrincipalAmount.Location = New System.Drawing.Point(185, 100)
        Me.lblPrincipalAmount.Name = "lblPrincipalAmount"
        Me.lblPrincipalAmount.Size = New System.Drawing.Size(50, 13)
        Me.lblPrincipalAmount.TabIndex = 0
        Me.lblPrincipalAmount.Text = "Principal:"
        '
        'txtPrincipalAmount
        '
        Me.txtPrincipalAmount.Location = New System.Drawing.Point(241, 97)
        Me.txtPrincipalAmount.Name = "txtPrincipalAmount"
        Me.txtPrincipalAmount.Size = New System.Drawing.Size(121, 20)
        Me.txtPrincipalAmount.TabIndex = 1
        Me.txtPrincipalAmount.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 18.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(130, 9)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(249, 29)
        Me.Label1.TabIndex = 13
        Me.Label1.Text = "Mortgage Calculator"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(153, 38)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(202, 16)
        Me.Label2.TabIndex = 14
        Me.Label2.Text = "Written by David C. Gibbons"
        '
        'closeButton
        '
        Me.closeButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.closeButton.Location = New System.Drawing.Point(421, 402)
        Me.closeButton.Name = "closeButton"
        Me.closeButton.Size = New System.Drawing.Size(75, 23)
        Me.closeButton.TabIndex = 5
        Me.closeButton.Text = "Close"
        Me.closeButton.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(169, 61)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(170, 13)
        Me.Label3.TabIndex = 16
        Me.Label3.Text = "POS/405 - Advanced Visual Basic"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(196, 126)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(39, 13)
        Me.Label4.TabIndex = 17
        Me.Label4.Text = "Terms:"
        '
        'boxMortgages
        '
        Me.boxMortgages.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.boxMortgages.FormattingEnabled = True
        Me.boxMortgages.Location = New System.Drawing.Point(241, 123)
        Me.boxMortgages.Name = "boxMortgages"
        Me.boxMortgages.Size = New System.Drawing.Size(121, 21)
        Me.boxMortgages.TabIndex = 2
        '
        'paymentsList
        '
        Me.paymentsList.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.paymentsList.FullRowSelect = True
        Me.paymentsList.GridLines = True
        Me.paymentsList.Location = New System.Drawing.Point(86, 262)
        Me.paymentsList.Name = "paymentsList"
        Me.paymentsList.Size = New System.Drawing.Size(336, 133)
        Me.paymentsList.TabIndex = 20
        Me.paymentsList.UseCompatibleStateImageBehavior = False
        Me.paymentsList.View = System.Windows.Forms.View.Details
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(83, 246)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(81, 13)
        Me.Label5.TabIndex = 19
        Me.Label5.Text = "Payment Detail:"
        '
        'clearButton
        '
        Me.clearButton.Location = New System.Drawing.Point(378, 126)
        Me.clearButton.Name = "clearButton"
        Me.clearButton.Size = New System.Drawing.Size(75, 23)
        Me.clearButton.TabIndex = 4
        Me.clearButton.Text = "C&lear"
        Me.clearButton.UseVisualStyleBackColor = True
        '
        'btnCalc
        '
        Me.btnCalc.Location = New System.Drawing.Point(378, 97)
        Me.btnCalc.Name = "btnCalc"
        Me.btnCalc.Size = New System.Drawing.Size(75, 23)
        Me.btnCalc.TabIndex = 3
        Me.btnCalc.Text = "&Calculate"
        Me.btnCalc.UseVisualStyleBackColor = True
        '
        'txtTerm
        '
        Me.txtTerm.Location = New System.Drawing.Point(241, 151)
        Me.txtTerm.Name = "txtTerm"
        Me.txtTerm.Size = New System.Drawing.Size(121, 20)
        Me.txtTerm.TabIndex = 3
        Me.txtTerm.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtInterestRate
        '
        Me.txtInterestRate.Location = New System.Drawing.Point(241, 177)
        Me.txtInterestRate.Name = "txtInterestRate"
        Me.txtInterestRate.Size = New System.Drawing.Size(121, 20)
        Me.txtInterestRate.TabIndex = 4
        Me.txtInterestRate.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(164, 180)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(71, 13)
        Me.Label6.TabIndex = 23
        Me.Label6.Text = "Interest Rate:"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(160, 154)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(75, 13)
        Me.Label7.TabIndex = 24
        Me.Label7.Text = "Term in Years:"
        '
        'MortgageCalculator
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(508, 437)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.txtInterestRate)
        Me.Controls.Add(Me.txtTerm)
        Me.Controls.Add(Me.btnCalc)
        Me.Controls.Add(Me.clearButton)
        Me.Controls.Add(Me.paymentsList)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.boxMortgages)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.closeButton)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.txtPayment)
        Me.Controls.Add(Me.txtPrincipalAmount)
        Me.Controls.Add(Me.lblPayment)
        Me.Controls.Add(Me.lblPrincipalAmount)
        Me.Name = "MortgageCalculator"
        Me.Text = "Mortgage Calculator"
        CType(Me.ErrorProvider, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents txtPayment As System.Windows.Forms.TextBox
    Friend WithEvents lblPayment As System.Windows.Forms.Label
    Friend WithEvents ErrorProvider As System.Windows.Forms.ErrorProvider
    Friend WithEvents txtPrincipalAmount As System.Windows.Forms.TextBox
    Friend WithEvents lblPrincipalAmount As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ToolTip As System.Windows.Forms.ToolTip
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents closeButton As System.Windows.Forms.Button
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents boxMortgages As System.Windows.Forms.ComboBox
    Friend WithEvents paymentsList As System.Windows.Forms.ListView
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents clearButton As System.Windows.Forms.Button
    Friend WithEvents btnCalc As System.Windows.Forms.Button
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents txtInterestRate As System.Windows.Forms.TextBox
    Friend WithEvents txtTerm As System.Windows.Forms.TextBox
    Friend WithEvents Label7 As System.Windows.Forms.Label

End Class
