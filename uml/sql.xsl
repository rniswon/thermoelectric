<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:xmi="http://www.omg.org/spec/XMI/20131001"
		xmlns:ecore="http://www.eclipse.org/emf/2002/Ecore"
		xmlns:uml="http://www.eclipse.org/uml2/5.0.0/UML"
		xmlns:str="http://exslt.org/strings"
                extension-element-prefixes="str">
  <xsl:output method="text"/>
  <!-- The upper-case() function is not available in XSLT 1.0, thus
       this infrastructure. See
       https://stackoverflow.com/questions/586231/how-can-i-convert-a-string-to-upper-or-lower-case-with-xslt
       for more. -->
  <xsl:variable name="lowercase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />
  
  <!-- An XSLT stylesheet to transform Papyrus UML (XMI) to SQLite
       SQL. -->
  
  <xsl:template match="/uml:Model">
    <xsl:apply-templates select="packagedElement[@xmi:type = 'uml:Class']"/>
  </xsl:template>
  
  <xsl:template match="packagedElement[@xmi:type = 'uml:Class']">
    <xsl:text>CREATE TABLE </xsl:text>
    <!-- TODO: convert CamelCase to all-uppercase snake case -->
    <xsl:value-of select="translate(@name, $lowercase, $uppercase)"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>
