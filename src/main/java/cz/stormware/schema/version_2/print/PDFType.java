//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.print;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for PDFType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PDFType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="fileName" type="{http://www.stormware.cz/schema/version_2/type.xsd}nonEmptyString"/&gt;
 *         &lt;element name="isdoc" type="{http://www.stormware.cz/schema/version_2/print.xsd}isdocType" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PDFType", propOrder = {

})
public class PDFType {

    @XmlElement(required = true)
    protected String fileName;
    protected IsdocType isdoc;

    /**
     * Gets the value of the fileName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * Sets the value of the fileName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFileName(String value) {
        this.fileName = value;
    }

    /**
     * Gets the value of the isdoc property.
     * 
     * @return
     *     possible object is
     *     {@link IsdocType }
     *     
     */
    public IsdocType getIsdoc() {
        return isdoc;
    }

    /**
     * Sets the value of the isdoc property.
     * 
     * @param value
     *     allowed object is
     *     {@link IsdocType }
     *     
     */
    public void setIsdoc(IsdocType value) {
        this.isdoc = value;
    }

}
