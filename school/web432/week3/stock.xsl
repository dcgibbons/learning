<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" version="4.0"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>Stock Information</title>
        <link href="stock.css" rel="stylesheet" type="text/css"/>
      </head>
      <body>
        <div id="datetime">
          <strong>Last Updated: </strong>
          <xsl:value-of select="portfolio/date" /> at <xsl:value-of select="portfolio/time" />
        </div>
        <h1 class="title">Hardin Financial</h1>
        <h2 class="title">Stock Information</h2>

        <h2 class="category">Industrials</h2>
        <xsl:apply-templates select="portfolio/stock[category='Industrials']">
          <xsl:sort select="name"/>
        </xsl:apply-templates>

        <h2 class="category">Utilities</h2>
        <xsl:apply-templates select="portfolio/stock[category='Utilities']">
          <xsl:sort select="name"/>
        </xsl:apply-templates>

        <h2 class="category">Transportation</h2>
        <xsl:apply-templates select="portfolio/stock[category='Transportation']">
          <xsl:sort select="name"/>
        </xsl:apply-templates>

      </body>
    </html>
  </xsl:template>

  <xsl:template match="stock">
    <table class="summary" border="1">
      <tr>
        <th colspan="2" class="summtitle">Summary Information</th>
      </tr>
      <tr>
        <th class="summary">Year High</th>
        <td class="number"><xsl:value-of select="year_high"/></td>
      </tr>
      <tr>
        <th class="summary">Year Low</th>
        <td class="number"><xsl:value-of select="year_low"/></td>
      </tr>
      <tr>
        <th class="summary">Price/Earnings Ratio</th>
        <td class="number"><xsl:value-of select="pe_ratio"/></td>
      </tr>
      <tr>
        <th class="summary">Yield</th>
        <td class="number"><xsl:value-of select="yield"/></td>
      </tr>
    </table>
    <div class="stock_info">
      <xsl:apply-templates select="name"/>
      <xsl:apply-templates select="today"/>
      <p><xsl:value-of select="description"/></p>
      <xsl:apply-templates select="five_day"/>
    </div>
  </xsl:template>

  <xsl:template match="name">
    <xsl:element name="a">
      <xsl:attribute name="href">
        <xsl:value-of select="../link"/>
      </xsl:attribute>
      <h3 class="name">
        <xsl:value-of select="."/>
        (<xsl:value-of select="@symbol"/>)
      </h3>
    </xsl:element>
  </xsl:template>

  <xsl:template match="today">
    <table class="today">
      <tr>
        <th class="today">Current</th>
        <th class="today">Open</th>
        <th class="today">High</th>
        <th class="today">Low</th>
        <th class="today">Volume</th>
      </tr>
      <tr>
        <td class="number">
          <xsl:value-of select="@current"/>
          <xsl:choose>
            <xsl:when test="@current &lt; @open">
              <img src="down.gif"/>
            </xsl:when>
            <xsl:when test="@current &gt; @open">
              <img src="up.gif"/>
            </xsl:when>
            <xsl:otherwise>
              <img src="same.gif"/>
            </xsl:otherwise>
          </xsl:choose>
        </td>
        <td class="number"><xsl:value-of select="@open"/></td>
        <td class="number"><xsl:value-of select="@high"/></td>
        <td class="number"><xsl:value-of select="@low"/></td>
        <td class="number"><xsl:value-of select="@vol"/></td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="five_day">
    <table border="1" width="620" class="history">
      <tr>
        <th class="histtitle" colspan="6">Recent History</th>
      </tr>
      <tr>
        <th class="history">Day</th>
        <th class="history">Open</th>
        <th class="history">High</th>
        <th class="history">Low</th>
        <th class="history">Close</th>
        <th class="history">Volume</th>
      </tr>
      <xsl:apply-templates select="day">
        <xsl:sort data-type="number" order="descending"/>
      </xsl:apply-templates>
    </table>
  </xsl:template>

  <xsl:template match="day">
    <tr>
      <td class="number">
        <xsl:value-of select="@date"/>
        <xsl:choose>
          <xsl:when test="@close &lt; @open">
            <img src="down.gif"/>
          </xsl:when>
          <xsl:when test="@close &gt; @open">
            <img src="up.gif"/>
          </xsl:when>
          <xsl:otherwise>
            <img src="same.gif"/>
          </xsl:otherwise>
        </xsl:choose>
      </td>
      <td class="number"><xsl:value-of select="@open"/></td>
      <td class="number"><xsl:value-of select="@high"/></td>
      <td class="number"><xsl:value-of select="@low"/></td>
      <td class="number"><xsl:value-of select="@close"/></td>
      <td class="number"><xsl:value-of select="@vol"/></td>
    </tr>
  </xsl:template>
</xsl:stylesheet>
