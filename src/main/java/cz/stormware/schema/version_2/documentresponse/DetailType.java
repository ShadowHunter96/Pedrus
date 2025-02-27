//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.documentresponse;

import java.math.BigInteger;
import cz.stormware.schema.version_2.type.StavType;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for detailType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="detailType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="state" type="{http://www.stormware.cz/schema/version_2/type.xsd}stavType"/&gt;
 *         &lt;element name="errno" type="{http://www.w3.org/2001/XMLSchema}integer" minOccurs="0"/&gt;
 *         &lt;element name="note" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;group ref="{http://www.stormware.cz/schema/version_2/documentresponse.xsd}myGroupOfValue" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "detailType", propOrder = {
    "state",
    "errno",
    "note",
    "xPath",
    "valueRequested",
    "valueProduced"
})
public class DetailType {

    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected StavType state;
    protected BigInteger errno;
    protected String note;
    @XmlElement(name = "XPath")
    protected String xPath;
    protected String valueRequested;
    protected String valueProduced;

    /**
     * Gets the value of the state property.
     * 
     * @return
     *     possible object is
     *     {@link StavType }
     *     
     */
    public StavType getState() {
        return state;
    }

    /**
     * Sets the value of the state property.
     * 
     * @param value
     *     allowed object is
     *     {@link StavType }
     *     
     */
    public void setState(StavType value) {
        this.state = value;
    }

    /**
     * Gets the value of the errno property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getErrno() {
        return errno;
    }

    /**
     * Sets the value of the errno property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setErrno(BigInteger value) {
        this.errno = value;
    }

    /**
     * Gets the value of the note property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNote() {
        return note;
    }

    /**
     * Sets the value of the note property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNote(String value) {
        this.note = value;
    }

    /**
     * Gets the value of the xPath property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getXPath() {
        return xPath;
    }

    /**
     * Sets the value of the xPath property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setXPath(String value) {
        this.xPath = value;
    }

    /**
     * Gets the value of the valueRequested property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getValueRequested() {
        return valueRequested;
    }

    /**
     * Sets the value of the valueRequested property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setValueRequested(String value) {
        this.valueRequested = value;
    }

    /**
     * Gets the value of the valueProduced property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getValueProduced() {
        return valueProduced;
    }

    /**
     * Sets the value of the valueProduced property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setValueProduced(String value) {
        this.valueProduced = value;
    }

}
