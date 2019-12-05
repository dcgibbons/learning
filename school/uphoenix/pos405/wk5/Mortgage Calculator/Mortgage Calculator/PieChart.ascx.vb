' PieChart.aspx.vb
' Week 5 Assignment
' David C. Gibbons
' POS/405 - Advanced Visual Basic
' Don McPherson
' April 30, 2007
'
' Version | Date       | Description
' --------+------------+-----------------------------------------------
' | 1.00  | 2007-04-30 | Created to satisfy change request # 23. 
' --------+------------+-----------------------------------------------
'
' Much of the design of this custom control was derived from the article
' titled "Create Snazzy Web Charts and Graphics On the Fly with the .NET 
' Framework" by Scott Mitchell, available at 
' http://msdn.microsoft.com/msdnmag/issues/02/02/aspdraw/
'

Option Explicit On      ' Require all variables to be declared
Option Strict On        ' Require strict data type declarations and conversions

'
' Imports
'
Imports System
Imports System.ComponentModel
Imports System.Drawing
Imports System.Security
Imports System.Security.Permissions
Imports System.Web
Imports System.Web.UI
Imports System.Web.UI.WebControls

'
' Custom PieChart control
'
Partial Class PieChart
    Inherits UserControl

    Const DEFAULT_WIDTH As Integer = 240

    Private imageFilename As String
    Private title As String
    Private values(-1) As Decimal
    Private labels(-1) As String
    Private valueFormat As String
    Private width As Integer = DEFAULT_WIDTH

    '
    ' Clear method resets all of the data points and configuration
    '
    Public Sub Clear()
        title = ""
        valueFormat = ""
        width = DEFAULT_WIDTH
        ReDim values(-1)
        ReDim labels(-1)
    End Sub

    '
    ' Adds a new data point with the specific label to the chart
    '
    Public Sub AddData(ByVal label As String, ByVal value As Decimal)
        Dim n As Integer = values.Length

        ReDim Preserve labels(n)
        labels(n) = label

        ReDim Preserve values(n)
        values(n) = value
    End Sub

    '
    ' Property that defines the title of the chart
    '
    Public Property ChartTitle() As String
        Get
            ChartTitle = Me.title
        End Get

        Set(ByVal value As String)
            Me.title = value
        End Set
    End Property

    '
    ' Property that defines the string formating used for values
    '
    Public Property ValueFormatText() As String
        Get
            ValueFormatText = Me.valueFormat
        End Get
        Set(ByVal value As String)
            Me.valueFormat = value
        End Set
    End Property

    '
    ' Property that defines the width of the chart (height is computed)
    '
    Public Property ChartWidth() As Integer
        Get
            ChartWidth = Me.width
        End Get
        Set(ByVal value As Integer)
            Me.width = value
        End Set
    End Property

    '
    ' Export the chart to disk before we actually render the image in HTML
    '
    Protected Overrides Sub OnPreRender(ByVal e As System.EventArgs)
        MyBase.OnPreRender(e)

        Dim serverPath As String = ""
        Dim mappedPath As String = Server.MapPath("~")
        imageFilename = ExportChart(serverPath, mappedPath)
    End Sub

    '
    ' Renders the HTML used to visualize this control
    '
    Protected Overrides Sub Render(ByVal writer As System.Web.UI.HtmlTextWriter)
        writer.AddAttribute(HtmlTextWriterAttribute.Src, imageFilename)
        writer.AddAttribute(HtmlTextWriterAttribute.Alt, "Pie Chart: " & title)
        writer.RenderBeginTag(HtmlTextWriterTag.Img)
        writer.RenderEndTag()
    End Sub

    '
    ' Function that creates the charts and exports it to disk
    '
    Private Function ExportChart(ByVal relPath As String, ByVal absPath As String) As String
        'Get a random filename
        Dim strFileName As String
        Randomize()
        strFileName = Timer & Rnd() & ".gif"

        ' save chart bitmap to file
        Dim chart As Bitmap = DrawChart()
        chart.Save(absPath & IO.Path.DirectorySeparatorChar & strFileName, Imaging.ImageFormat.Gif)

        'Return the path to the GIF image of the graph
        ExportChart = strFileName
    End Function

    '
    ' Function to actually draw the chart bitmap 
    '
    Private Function DrawChart() As Bitmap
        Dim nValues As Integer = values.Length - 1

        ' determine the total of all the values provided
        Dim total As Decimal = 0D
        For i As Integer = 0 To nValues
            total = total + values(i)
        Next

        ' we need to create fonts for our legend and title
        Dim fontLegend As Font = New Font("Verdana", 10)
        Dim fontTitle As Font = New Font("Verdana", 14, FontStyle.Bold)

        ' We need to create a legend and title, how big do these need to be?
        ' Also, we need to resize the height for the pie chart, respective
        ' to the height of the legend and title
        Const bufferSpace As Integer = 15
        Dim legendHeight As Integer = fontLegend.Height * (values.Length + 1) + bufferSpace
        Dim titleHeight As Integer = fontTitle.Height + bufferSpace
        Dim height As Integer = width + legendHeight + titleHeight + bufferSpace
        Dim pieHeight As Integer = width ' maintain a one-to-one ratio

        ' Create a rectange for drawing our pie chart underneath the title area
        Dim pieRect As Rectangle = New Rectangle(0, titleHeight, width, pieHeight)

        ' create a random selection of colors, one for for each data value
        Dim rnd As Random = New Random
        Dim colors(values.Length) As Brush
        For i As Integer = 0 To nValues
            colors(i) = New SolidBrush(Color.FromArgb(rnd.Next(255), rnd.Next(255), rnd.Next(255)))
        Next

        ' define the fixed color brushes used for text and background drawing
        Dim backgroundBrush As Brush = Brushes.White
        Dim textBrush As Brush = Brushes.Black

        ' create a bitmap instance so we can draw into it
        Dim ret As Bitmap = New Drawing.Bitmap(width, height)
        Dim g As Graphics = Graphics.FromImage(ret)
        g.InterpolationMode = Drawing2D.InterpolationMode.HighQualityBicubic

        ' put a white background in
        g.FillRectangle(backgroundBrush, 0, 0, width, height)

        ' draw the pie regions
        Dim currentDegree As Single = 0.0
        For i As Integer = 0 To nValues
            ' each slice is a percentage of the total applied to a full circle
            Dim sweepAngle As Single = CSng(values(i) / total * 360)
            g.FillPie(colors(i), pieRect, currentDegree, sweepAngle)
            currentDegree = currentDegree + sweepAngle
        Next

        ' create the legend
        Dim stringFormat As StringFormat = New StringFormat
        stringFormat.Alignment = StringAlignment.Center
        stringFormat.LineAlignment = StringAlignment.Center
        g.DrawString(title, fontTitle, textBrush, New Rectangle(0, 0, width, titleHeight))

        For i As Integer = 0 To nValues
            ' create a solid rectangle to represent with this legend item's color
            Dim rectX As Integer = 5
            Dim rectY As Integer = height - legendHeight + fontLegend.Height * i + 5
            Dim rectWidth As Integer = 10
            Dim rectHeight As Integer = 10
            g.FillRectangle(colors(i), rectX, rectY, rectWidth, rectHeight)

            ' display the legend text next to the legend rectangle
            Dim legend As String = labels(i) & " - " & String.Format(valueFormat, values(i))
            Dim textX As Integer = 20
            Dim textY As Integer = height - legendHeight + fontLegend.Height * i + 1
            g.DrawString(legend, fontLegend, textBrush, textX, textY)
        Next

        ' add the total to the legend
        g.DrawString("Total: " & String.Format(valueFormat, total), fontLegend, textBrush, 5, height - fontLegend.Height - 5)

        DrawChart = ret
    End Function

End Class
