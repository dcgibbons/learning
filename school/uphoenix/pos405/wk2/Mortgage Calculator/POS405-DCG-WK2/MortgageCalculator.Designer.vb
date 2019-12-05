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
        Me.lblTerm = New System.Windows.Forms.Label
        Me.lblInterestRate = New System.Windows.Forms.Label
        Me.lblPrincipalAmount = New System.Windows.Forms.Label
        Me.txtPrincipalAmount = New System.Windows.Forms.TextBox
        Me.txtInterestRate = New System.Windows.Forms.TextBox
        Me.txtTerm = New System.Windows.Forms.TextBox
        Me.ToolTip = New System.Windows.Forms.ToolTip(Me.components)
        Me.Label1 = New System.Windows.Forms.Label
        Me.Label2 = New System.Windows.Forms.Label
        Me.closeButton = New System.Windows.Forms.Button
        Me.Label3 = New System.Windows.Forms.Label
        CType(Me.ErrorProvider, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'txtPayment
        '
        Me.txtPayment.Location = New System.Drawing.Point(174, 217)
        Me.txtPayment.Name = "txtPayment"
        Me.txtPayment.ReadOnly = True
        Me.txtPayment.Size = New System.Drawing.Size(100, 20)
        Me.txtPayment.TabIndex = 11
        Me.txtPayment.TabStop = False
        Me.txtPayment.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lblPayment
        '
        Me.lblPayment.AutoSize = True
        Me.lblPayment.Location = New System.Drawing.Point(80, 217)
        Me.lblPayment.Name = "lblPayment"
        Me.lblPayment.Size = New System.Drawing.Size(88, 13)
        Me.lblPayment.TabIndex = 12
        Me.lblPayment.Text = "Monthly Payment"
        '
        'ErrorProvider
        '
        Me.ErrorProvider.ContainerControl = Me
        '
        'lblTerm
        '
        Me.lblTerm.AutoSize = True
        Me.lblTerm.Location = New System.Drawing.Point(85, 175)
        Me.lblTerm.Name = "lblTerm"
        Me.lblTerm.Size = New System.Drawing.Size(75, 13)
        Me.lblTerm.TabIndex = 2
        Me.lblTerm.Text = "Term in Years:"
        '
        'lblInterestRate
        '
        Me.lblInterestRate.AutoSize = True
        Me.lblInterestRate.Location = New System.Drawing.Point(97, 137)
        Me.lblInterestRate.Name = "lblInterestRate"
        Me.lblInterestRate.Size = New System.Drawing.Size(71, 13)
        Me.lblInterestRate.TabIndex = 1
        Me.lblInterestRate.Text = "Interest Rate:"
        '
        'lblPrincipalAmount
        '
        Me.lblPrincipalAmount.AutoSize = True
        Me.lblPrincipalAmount.Location = New System.Drawing.Point(118, 100)
        Me.lblPrincipalAmount.Name = "lblPrincipalAmount"
        Me.lblPrincipalAmount.Size = New System.Drawing.Size(50, 13)
        Me.lblPrincipalAmount.TabIndex = 0
        Me.lblPrincipalAmount.Text = "Principal:"
        '
        'txtPrincipalAmount
        '
        Me.txtPrincipalAmount.Location = New System.Drawing.Point(174, 97)
        Me.txtPrincipalAmount.Name = "txtPrincipalAmount"
        Me.txtPrincipalAmount.Size = New System.Drawing.Size(100, 20)
        Me.txtPrincipalAmount.TabIndex = 1
        Me.txtPrincipalAmount.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtInterestRate
        '
        Me.txtInterestRate.Location = New System.Drawing.Point(174, 134)
        Me.txtInterestRate.Name = "txtInterestRate"
        Me.txtInterestRate.Size = New System.Drawing.Size(100, 20)
        Me.txtInterestRate.TabIndex = 2
        Me.txtInterestRate.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'txtTerm
        '
        Me.txtTerm.Location = New System.Drawing.Point(174, 172)
        Me.txtTerm.Name = "txtTerm"
        Me.txtTerm.Size = New System.Drawing.Size(100, 20)
        Me.txtTerm.TabIndex = 3
        Me.txtTerm.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 18.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(92, 9)
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
        Me.Label2.Location = New System.Drawing.Point(115, 38)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(202, 16)
        Me.Label2.TabIndex = 14
        Me.Label2.Text = "Written by David C. Gibbons"
        '
        'closeButton
        '
        Me.closeButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.closeButton.Location = New System.Drawing.Point(345, 258)
        Me.closeButton.Name = "closeButton"
        Me.closeButton.Size = New System.Drawing.Size(75, 23)
        Me.closeButton.TabIndex = 15
        Me.closeButton.Text = "C&lose"
        Me.closeButton.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(131, 61)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(170, 13)
        Me.Label3.TabIndex = 16
        Me.Label3.Text = "POS/405 - Advanced Visual Basic"
        '
        'MortgageCalculator
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(432, 293)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.closeButton)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.txtTerm)
        Me.Controls.Add(Me.txtInterestRate)
        Me.Controls.Add(Me.txtPayment)
        Me.Controls.Add(Me.txtPrincipalAmount)
        Me.Controls.Add(Me.lblPayment)
        Me.Controls.Add(Me.lblPrincipalAmount)
        Me.Controls.Add(Me.lblInterestRate)
        Me.Controls.Add(Me.lblTerm)
        Me.Name = "MortgageCalculator"
        Me.Text = "Mortgage Calculator"
        CType(Me.ErrorProvider, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents txtPayment As System.Windows.Forms.TextBox
    Friend WithEvents lblPayment As System.Windows.Forms.Label
    Friend WithEvents ErrorProvider As System.Windows.Forms.ErrorProvider
    Friend WithEvents txtTerm As System.Windows.Forms.TextBox
    Friend WithEvents txtInterestRate As System.Windows.Forms.TextBox
    Friend WithEvents txtPrincipalAmount As System.Windows.Forms.TextBox
    Friend WithEvents lblPrincipalAmount As System.Windows.Forms.Label
    Friend WithEvents lblInterestRate As System.Windows.Forms.Label
    Friend WithEvents lblTerm As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ToolTip As System.Windows.Forms.ToolTip
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents closeButton As System.Windows.Forms.Button
    Friend WithEvents Label3 As System.Windows.Forms.Label

End Class
