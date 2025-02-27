//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.print;

import cz.stormware.schema.version_2.type.Boolean;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for isdocType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="isdocType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="includeToPdf" type="{http://www.stormware.cz/schema/version_2/type.xsd}boolean"/&gt;
 *         &lt;element name="graphicNote"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;enumeration value="topRight"/&gt;
 *               &lt;enumeration value="topLeft"/&gt;
 *               &lt;enumeration value="bottomRight"/&gt;
 *               &lt;enumeration value="bottomLeft"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "isdocType", propOrder = {

})
public class IsdocType {

    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected Boolean includeToPdf;
    @XmlElement(required = true)
    protected String graphicNote;

    /**
     * Gets the value of the includeToPdf property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean getIncludeToPdf() {
        return includeToPdf;
    }

    /**
     * Sets the value of the includeToPdf property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIncludeToPdf(Boolean value) {
        this.includeToPdf = value;
    }

    /**
     * Gets the value of the graphicNote property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGraphicNote() {
        return graphicNote;
    }

    /**
     * Sets the value of the graphicNote property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGraphicNote(String value) {
        this.graphicNote = value;
    }

}
