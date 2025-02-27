//
// This file was generated by the Eclipse Implementation of JAXB, v3.0.2 
// See https://eclipse-ee4j.github.io/jaxb-ri 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2022.09.09 at 03:15:43 PM CEST 
//


package cz.stormware.schema.version_2.invoice;

import javax.xml.datatype.XMLGregorianCalendar;
import cz.stormware.schema.version_2.type.LiquidationType;
import cz.stormware.schema.version_2.type.RefType;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlType;


/**
 * <p>Java class for invoiceRetentionType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="invoiceRetentionType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;all&gt;
 *         &lt;element name="date" type="{http://www.w3.org/2001/XMLSchema}date"/&gt;
 *         &lt;element name="dateDue" type="{http://www.w3.org/2001/XMLSchema}date"/&gt;
 *         &lt;element name="homeCurrency" type="{http://www.stormware.cz/schema/version_2/type.xsd}currencyType"/&gt;
 *         &lt;element name="liquidation" type="{http://www.stormware.cz/schema/version_2/type.xsd}liquidationType" minOccurs="0"/&gt;
 *         &lt;element name="text" type="{http://www.stormware.cz/schema/version_2/type.xsd}string64" minOccurs="0"/&gt;
 *         &lt;element name="accounting" type="{http://www.stormware.cz/schema/version_2/type.xsd}refType" minOccurs="0"/&gt;
 *       &lt;/all&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "invoiceRetentionType", propOrder = {

})
public class InvoiceRetentionType {

    @XmlElement(required = true)
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar date;
    @XmlElement(required = true)
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar dateDue;
    protected double homeCurrency;
    protected LiquidationType liquidation;
    protected String text;
    protected RefType accounting;

    /**
     * Gets the value of the date property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDate() {
        return date;
    }

    /**
     * Sets the value of the date property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDate(XMLGregorianCalendar value) {
        this.date = value;
    }

    /**
     * Gets the value of the dateDue property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDateDue() {
        return dateDue;
    }

    /**
     * Sets the value of the dateDue property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDateDue(XMLGregorianCalendar value) {
        this.dateDue = value;
    }

    /**
     * Gets the value of the homeCurrency property.
     * 
     */
    public double getHomeCurrency() {
        return homeCurrency;
    }

    /**
     * Sets the value of the homeCurrency property.
     * 
     */
    public void setHomeCurrency(double value) {
        this.homeCurrency = value;
    }

    /**
     * Gets the value of the liquidation property.
     * 
     * @return
     *     possible object is
     *     {@link LiquidationType }
     *     
     */
    public LiquidationType getLiquidation() {
        return liquidation;
    }

    /**
     * Sets the value of the liquidation property.
     * 
     * @param value
     *     allowed object is
     *     {@link LiquidationType }
     *     
     */
    public void setLiquidation(LiquidationType value) {
        this.liquidation = value;
    }

    /**
     * Gets the value of the text property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getText() {
        return text;
    }

    /**
     * Sets the value of the text property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setText(String value) {
        this.text = value;
    }

    /**
     * Gets the value of the accounting property.
     * 
     * @return
     *     possible object is
     *     {@link RefType }
     *     
     */
    public RefType getAccounting() {
        return accounting;
    }

    /**
     * Sets the value of the accounting property.
     * 
     * @param value
     *     allowed object is
     *     {@link RefType }
     *     
     */
    public void setAccounting(RefType value) {
        this.accounting = value;
    }

}
