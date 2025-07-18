<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:frmwrk="Corel Framework Data">
	<xsl:template match="node()|@*">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
		</xsl:copy>
	</xsl:template>
	<xsl:template match="uiConfig/items">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
			<itemData guid="2GIS4CDR" dynamicCommand="2GIS4CDR" dynamicCategory="ab489730-8791-45d2-a825-b78bbe0d6a5d" icon=""/>
		</xsl:copy>
	</xsl:template>
	<xsl:template match="uiConfig/commandBars/commandBarData[@guid='2cfa7035-cbef-4276-b686-adb4f77c80d6']/menu/item[@guidRef='785c273f-c462-4f02-9ee4-c354018e0878']">
		<xsl:copy-of select="."/>
		<item guidRef="2GIS4CDR"/>
	</xsl:template>
</xsl:stylesheet>